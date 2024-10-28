#' estimate inverse probability weights based on Beesley & Mukhherjee
#' @param stacked_data data.table of stacked data
#' @param weight_outcome_var variable name of sampling weights in external dataset
#' @param samp_var variable name of sampling weights in internal dataset
#' @param external_dataset name of external dataset
#' @param dataset_name name of internal dataset
#' @param id_var variable name of id
#' @param cancer_factor logical, whether to include cancer factor
#' @param cancer_factor_var variable name of cancer factor
#' @param covs vector of covariates to include in model
#' @param chop logical, whether to chop weights
#' @importFrom data.table as.data.table
#' @importFrom data.table is.data.table
#' @importFrom data.table data.table
#' @importFrom simplexreg simplexreg
#' @return return table of id and inverse probability weights
#' @export

ipw <- function(
    stacked_data,
    weight_outcome_var = "WTFA_A",
    samp_var           = "samp_WTFA_A",
    external_dataset   = "NHIS",
    dataset_name       = "MGI",
    id_var             = "id",
    cancer_factor      = FALSE,
    cancer_factor_var  = "cancer",
    covs = c(
      "age_50", "female", "nhw", "hypertension",
      "diabetes", "cancer", "anxiety", "depression",
      "bmi_cat"
    ),
    chop = TRUE
) {
  if (!data.table::is.data.table(stacked_data)) {
    stacked_data <- data.table::as.data.table(stacked_data)
  }
  stacked_data[dataset == external_dataset, (weight_outcome_var) := .N * get(weight_outcome_var) /
    sum(get(weight_outcome_var), na.rm = TRUE)]

  if (cancer_factor) {
    if (cancer_factor_var %in% covs) {
      cancer_TF <- TRUE
      covs      <- covs[covs != cancer_factor_var]
    } else {
      cancer_TF <- FALSE
    }
  }
  select_mod_covs <- paste0(covs, collapse = " + ")

  # modeling external sampling weights
  external_select_mod <- simplexreg::simplexreg(
    as.formula(paste0(
      samp_var, " ~ ",
      select_mod_covs
    )),
    data = stacked_data[dataset == external_dataset, ]
  )


  # selection model into internal data
  internal_select_mod <- glm(
    paste0(
      "as.numeric(dataset == '",
      dataset_name, "') ~ ",
      select_mod_covs
    ),
    data = stacked_data, family = quasibinomial()
  )

  # obtain fitted values from external and internal models
  p_external <- predict(external_select_mod,
    newdata = stacked_data[dataset == dataset_name, ],
    type = "response"
  )[, 1]
  p_internal <- predict(internal_select_mod,
    newdata = stacked_data[dataset == dataset_name, ],
    type = "response"
  )

  ###
  temp <- rep(0, times = length(p_internal))
  temp[which(rownames(data.frame(p_external)) %in%
    rownames(data.frame(p_internal)) == T)] <- p_external
  temp[which(rownames(data.frame(p_external)) %in%
    rownames(data.frame(p_internal)) == F)] <- NA
  p_external <- temp
  p_external[which(p_external == 0)] <- min(p_external[which(p_external > 0)], na.rm = TRUE)
  external_selection <- p_external * (p_internal / (1 - p_internal))
  ###

  if (chop) external_selection <- ms::chopr(external_selection)
  external_weight <- 1 / external_selection
  external_weight <- stacked_data[dataset == dataset_name, .N] *
    external_weight /
    sum(external_weight, na.rm = TRUE)

  ## With Cancer
  if (cancer_factor) {
    if (cancer_TF == TRUE) {
      external_cancer_mod <- glm(as.formula(paste0(cancer_factor_var, " ~ ", select_mod_covs)),
        data = stacked_data[dataset == external_dataset, ],
        weights = get(weight_outcome_var), family = quasibinomial()
      )

      external_cancer <- predict(external_cancer_mod,
        type = "response",
        newdata = stacked_data
      )

      internal_cancer_mod <- glm(as.formula(paste0(cancer_factor_var, " ~ ", select_mod_covs)),
        data = stacked_data[dataset == dataset_name, ],
        family = quasibinomial()
      )

      internal_cancer <- predict(internal_cancer_mod,
        type    = "response",
        newdata = stacked_data
      )
      denom <- ifelse(stacked_data[[cancer_factor_var]] == 1, internal_cancer,
        1 - internal_cancer
      )
      num <- ifelse(stacked_data[[cancer_factor_var]] == 1, external_cancer,
        1 - external_cancer
      )
      cancer_factor <- (num[stacked_data[, dataset] == dataset_name] /
        denom[stacked_data[, dataset] == dataset_name])
      if (chop) {
        external_weight <- ms::chopr(cancer_factor * external_weight)
      } else {
        external_weight <- cancer_factor * external_weight
      }
      external_weight <- stacked_data[dataset == dataset_name, .N] *
        external_weight /
        sum(external_weight, na.rm = TRUE)
    }
  }

  data.table::data.table(
    "id"        = stacked_data[dataset == dataset_name, ][[id_var]],
    "ip_weight" = external_weight
  )
}

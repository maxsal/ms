#' estimate poststratification weights based on Beesley & Mukhherjee
#' @param int_data internal data to use for estimating weights
#' @param ext_data external data to use for estimating weights
#' @param id_var variable name for id
#' @param age_var variable name for age
#' @param age_bin logical, whether to bin age
#' @param age_bin_breaks breaks for age binning
#' @param covs covariates to use for estimating weights
#' @param not_num_vars variables to exclude from numeric covariates
#' @param psu_var variable name for psu
#' @param strata_var variable name for strata
#' @param weight_var variable name for weight
#' @param chop logical, whether to truncate weights
#' @importFrom cli cli_alert_info
#' @importFrom cli cli_alert
#' @importFrom data.table copy
#' @importFrom data.table setnames
#' @importFrom data.table merge.data.table
#' @return return data.table with id and poststratification weights
#' @export

psw <- function(
    int_data,
    ext_data       = NULL,
    id_var         = "id",
    age_var        = "AgeLastEntry",
    age_bin        = FALSE,
    age_bin_breaks = c(0, 18, 35, 65, 80, 150),
    covs           = c("age_bin", "cad", "smoker", "diabetes", "female"),
    not_num_vars   = NULL,
    psu_var        = "psu_nhanes",
    strata_var     = "strata_nhanes",
    weight_var     = "weight_nhanes",
    chop           = TRUE
) {
  if (age_bin) {
    cli::cli_alert("constructing categorical age variable using breaks: {age_bin_breaks}")
    covs <- unique(c(covs, "age_bin"))
  }
  cli::cli_alert_info("estimating poststratification weights for covariates: {.field {covs}}")
  if (chop) cli::cli_alert_info("truncating weights to 2.5 and 97.5 percentiles")

  # 1. prep external data ----------------------------------------------
  external_data <- data.table::copy(ext_data)

  if (age_bin == TRUE) {
    external_data[, age_bin := cut(age, age_bin_breaks, right = FALSE)]
  }

  external_design <- survey::svydesign(
    id      = ~ get(psu_var),
    strata  = ~ get(strata_var),
    weights = ~ get(weight_var),
    nest    = TRUE,
    data    = external_data
  )

  # 2. get external proportions -------------------------------------------------
  population_proportions <- survey::svytable(
    formula = as.formula(paste0("~", paste(covs, collapse = " + "))),
    design = external_design
  ) |>
    prop.table() |>
    data.table::as.data.table()
  data.table::setnames(population_proportions, "N", "pop_prob")

  if (!is.null(not_num_vars)) {
    num_vars <- setdiff(covs, c(not_num_vars, "age_bin"))
  } else {
    num_vars <- setdiff(covs, "age_bin")
  }

  population_proportions[, (num_vars) := lapply(.SD, as.numeric), .SDcols = num_vars]

  # 3. get internal proportions -----------------------------------------------
  internal_data <- data.table::copy(int_data)
  if (age_bin == TRUE) {
    internal_data[, age_bin := cut(get(age_var), age_bin_breaks, right = FALSE)]
  }

  internal_probabilities <- internal_data[, ..covs] |>
    table() |>
    prop.table() |>
    data.table::as.data.table()
  data.table::setnames(internal_probabilities, "N", "int_prob")
  internal_probabilities[, (num_vars) := lapply(.SD, as.numeric), .SDcols = num_vars]

  # 4. merge probabilities into internal data ---------------------------------
  sub_vars <- c(id_var, covs)

  min_pop_prob <- min(population_proportions[, pop_prob][which(population_proportions[, pop_prob] > 0)])
  population_proportions[pop_prob == 0, pop_prob := min_pop_prob]

  merged <- internal_data[, ..sub_vars] |>
    data.table::merge.data.table(population_proportions, by = covs) |>
    data.table::merge.data.table(internal_probabilities, by = covs)
  merged[, ps_weight := pop_prob / int_prob]

  # 5. process ----------------------------------------------------------------
  if (chop == TRUE) merged[, ps_weight := chopr(ps_weight)]

  merged[, ps_weight := (.N * ps_weight) / sum(ps_weight, na.rm = TRUE)]

  return_vars <- c(id_var, "ps_weight")
  return(
    merged[, ..return_vars]
  )
}

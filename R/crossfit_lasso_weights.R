#' estimate crossfit lasso-based weights
#' @param internal_data data.table of internal data
#' @param external_data data.table of external data
#' @param select_vars vector of variables to select
#' @param weight_outcome_var variable name for weight outcome
#' @param samp_var variable name for sampling indicator
#' @param external_dataset name of external dataset
#' @param dataset_name name of internal dataset
#' @param id_var name of id variable
#' @param internal_var name of internal dataset indicator variable
#' @param cancer_factor whether to use a cancer factor
#' @param ncores number of cores to use
#' @param folds number of folds to use
#' @importFrom data.table rbindlist
#' @importFrom data.table copy
#' @importFrom parallelly availableCores
#' @importFrom glmnet cv.glmnet
#' @importFrom glmnet glmnet
#' @importFrom doParallel registerDoParallel
#' @return return table of results from model for the exposures
#' @export

crossfit_lasso_weights <- function(
    internal_data,
    external_data,
    select_vars  = c(
        "id", "internal", "age_50", "female", "nhw",
        "hypertension", "diabetes", "cancer", "anxiety",
        "depression", "bmi_obese", "bmi_overweight", "bmi_underweight", "bmi_unknown"),
    weight_outcome_var = "WTFA_A",
    samp_var           = "samp_WTFA_A",
    external_dataset   = "NHIS",
    dataset_name       = "MGI",
    id_var             = "id",
    internal_var       = "internal",
    cancer_factor      = FALSE,
    ncores             = parallelly::availableCores() / 4,
    folds              = 5
) {

    stack <- data.table::rbindlist(list(
        internal_data,
        external_data
    ), use.names = TRUE, fill = TRUE)

    stack_cc <- stack[complete.cases(stack[, ..select_vars]), ]

    # split into folds
    split_index <- sample(folds, nrow(stack_cc), replace = TRUE)

    # create empty list to store results
    results <- list()

    for (i in seq_len(folds)) {
        cli::cli_progress_step(paste0("Fold ", i, " of ", folds), spinner = TRUE)

        # split into train and test
        train <- stack_cc[split_index == i, ..select_vars]
        test  <- stack_cc[split_index != i, ]

        data <- data.table::copy(train)
        data[[id_var]] <- NULL

        # first step: using .*. for all interactions
        f <- as.formula(paste0(internal_var, " ~ .*."))
        y <- data[[internal_var]]

        # create all interactions
        x <- model.matrix(f, data)[, -1]

        # perform cross-validation to select lambda
        cl <- parallel::makeCluster(ncores, type = "PSOCK")
        doParallel::registerDoParallel(cl)
        cv.lambda.lasso <- glmnet::cv.glmnet(
            x = x, y = y,
            alpha = 1, family = "binomial",
            parallel = TRUE
        )
        parallel::stopCluster(cl)

        # use lambda.1se to select model with parsimony
        l.lasso.1se <- cv.lambda.lasso$lambda.1se
        lasso.model <- glmnet::glmnet(
            x = x, y = y,
            alpha = 1, family = "binomial",
            lambda = l.lasso.1se
        )

        reg <- coef(lasso.model)
        use_these_terms <- rownames(reg)[reg[, 1] != 0]
        if ("(Intercept)" %in% use_these_terms) {
            use_these_terms <- use_these_terms[use_these_terms != "(Intercept)"]
        }
        pretty_terms <- gsub(":", "*", use_these_terms)

        # get weights
        results[[i]] <- ms::ipw(
            stacked_data       = stack_cc[split_index != i, ],
            weight_outcome_var = weight_outcome_var,
            samp_var           = samp_var,
            external_dataset   = external_dataset,
            dataset_name       = dataset_name,
            cancer_factor      = cancer_factor,
            covs               = pretty_terms
        )

    }
    cli::cli_progress_done()

    # combine results
    results <- data.table::rbindlist(results, use.names = TRUE, fill = TRUE)

    # average over predictions
    results[, .(ip_weight = mean(ip_weight)), by = "id"]

}

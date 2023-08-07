#' Helper: perform rapid unweighted and weighted cooccurrence analysis
#' @param data dataset
#' @param outcome outcome variable
#' @param covariates vector of covariates
#' @param exposure exposure variable
#' @param weight_dsn survey design object
#' @param evalue logical; calculate evalues (default = TRUE)
#' @importFrom survey svyglm
#' @importFrom stats glm
#' @importFrom stats as.formula
#' @importFrom stats quasibinomial
#' @importFrom stats coef
#' @importFrom stats qnorm
#' @importFrom EValue evalues.OR
#' @return data.table with results from cooccurrence analysis
.quick_cooccur_mod <- function(
    data,
    covariates = c("age_at_threshold", "female", "length_followup"),
    outcome    = "case",
    exposure   = "X157",
    weight_dsn = NULL,
    evalue     = TRUE
) {

    if (!is.null(weight_dsn)) {
        mod <- survey::svyglm(
            stats::as.formula(paste0(outcome, " ~ ", exposure, " + ", paste0(covariates, collapse = " + "))),
            design = weight_dsn,
            family = stats::quasibinomial()
        )
    } else {
        mod <- glm(paste0(outcome, " ~ ", exposure, " + ", paste0(covariates, collapse = " + ")), data = data, family = quasibinomial())
    }
    est <- summary(mod)$coef[exposure, c(1, 2)]
    out <- data.table(
        exposure = exposure,
        beta     = stats::coef(mod)[[exposure]],
        se_beta  = stats::coef(summary(mod))[exposure, 2],
        p_value  = stats::coef(summary(mod))[exposure, 4],
        log10p   = log10(stats::coef(summary(mod))[exposure, 4])
    )
    if (evalue == TRUE) {
        rare <- sum(mod$data[[exposure]], na.rm = TRUE) / nrow(mod$data)
        eval <- suppressMessages(evalues.OR(est = exp(est[[1]]), lo = exp(est[[1]] - stats::qnorm(0.975) * est[[2]]), hi = exp(est[[1]] + stats::qnorm(0.975) * est[[2]]), rare = ifelse(rare >= 0.15, FALSE, TRUE)))
        out[, `:=`(
            or         = exp(est[[1]]),
            evalue_est = eval[2, 1],
            evalue_lo  = eval[2, 2],
            evalue_hi  = eval[2, 3]
        )]
    }
    out
}


#' Perform rapid unweighted or weighted cooccurrence analysis
#' @param data dataset containing outcome, exposures, covariates, and, if weighted, weight variable
#' @param covariates vector of covariates names
#' @param possible_exposures vector of exposure names
#' @param weight_var weight variable name
#' @param n_cores number of cores over which to parallelize (default = 1)
#' @param parallel logical; parallelize analyses (default = FALSE)
#' @param min_case_count minimum number of cases to consider an exposure (default = 10)
#' @param evalue logical; calculate evalues (default = TRUE)
#' @return data.table of cooccurrence analysis results
#' @importFrom data.table melt
#' @importFrom data.table as.data.table
#' @importFrom data.table copy
#' @importFrom data.table rbindlist
#' @importFrom survey svydesign
#' @importFrom foreach foreach
#' @importFrom doMC registerDoMC
#' @importFrom cli cli_progress_bar
#' @importFrom cli cli_progress_update
#' @importFrom cli cli_progress_done
#' @importFrom glue glue
#' @export
#' @examples
#' \dontrun{
#' # TBD
#' }

cooccurrence_analysis <- function(
    data,
    covariates,
    possible_exposures = paste0("X", ms::pheinfo[, phecode]),
    weight_var         = NULL,
    n_cores            = 1,
    parallel           = FALSE,
    min_case_count     = 10,
    evalue             = TRUE
) {

    data2 <- data |>
        data.table::as.data.table() |>
        data.table::copy()

    # 1. identify analytic phecodes
    possible_exposures <- names(data2)[names(data2) %in% possible_exposures]
    exposures_to_consider <- data.table::melt(
        data2[, ..possible_exposures][, lapply(.SD, \(x) sum(x, na.rm = TRUE))],
        variable.name = "exposure",
        value.name = "n",
        id.vars = character()
    )[n >= min_case_count, as.character(exposure)]

    # 2. make design if weighted
    if (!is.null(weight_var)) {
        weight_design <- survey::svydesign(
            ids     = ~1,
            weights = ~ get(weight_var),
            data    = data2[!is.na(get(weight_var)), ]
        )
    } else {
        weight_design <- NULL
    }

    # 3. run analyses
    if (parallel == FALSE) {
        out <- list()
        cli::cli_progress_bar(
            name = glue::glue("t = {t_thresh} threshold"),
            total = length(exposures_to_consider)
        )
        for (i in seq_along(exposures_to_consider)) {
            out[[i]] <- .quick_cooccur_mod(
                data       = data2,
                covariates = covariates,
                exposure   = exposures_to_consider[i],
                weight_dsn = weight_design,
                evalue     = evalue
            )
            cli::cli_progress_update()
        }
        cli::cli_progress_done()
        out <- data.table::rbindlist(out)
    }

    if (parallel == TRUE) {
        doMC::registerDoMC(cores = n_cores)
        columns <- exposures_to_consider
        cols    <- seq_along(exposures_to_consider)
        output <- foreach::foreach(i = cols) %dopar% {
            .quick_cooccur_mod(
                data       = data2,
                covariates = covariates,
                exposure   = exposures_to_consider[i],
                weight_dsn = weight_design,
                evalue     = evalue
            )
        }
        out <- data.table::rbindlist(output, use.names = TRUE, fill = TRUE)
    }

    return(out)
}

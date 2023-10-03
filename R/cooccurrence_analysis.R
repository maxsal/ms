#' Helper: perform rapid unweighted and weighted cooccurrence analysis
#' @param data dataset
#' @param outcome outcome variable
#' @param covariates vector of covariates
#' @param exposure exposure variable
#' @param weight_var weight variable name
#' @param weight_dsn survey design object
#' @param evalue logical; calculate evalues (default = TRUE)
#' @param check_separation logical; check for separation (default = TRUE)
#' @importFrom survey svyglm
#' @importFrom stats glm
#' @importFrom stats as.formula
#' @importFrom stats binomial
#' @importFrom stats quasibinomial
#' @importFrom stats coef
#' @importFrom stats vcov
#' @importFrom stats coefficients
#' @importFrom stats qnorm
#' @importFrom logistf logistf
#' @importFrom detectseparation detect_separation
#' @importFrom EValue evalues.OR
#' @return data.table with results from cooccurrence analysis
.quick_cooccur_mod <- function(
    data,
    covariates       = c("age_at_threshold", "female", "length_followup"),
    outcome          = "case",
    exposure         = "X157",
    weight_var       = NULL,
    weight_dsn       = NULL,
    evalue           = TRUE,
    check_separation = TRUE
) {

    if (check_separation) {
        sep_check <- glm(paste0(outcome, " ~ ", exposure, " + ", paste0(covariates, collapse = " + ")), data = data, family = stats::binomial(), method = "detect_separation")
        sep_check <- is.infinite(sep_check$coefficients[exposure])
    } else {
        sep_check <- FALSE
    }

    if (sep_check) {
        if (is.null(weight_var)) {
            mod <- logistf::logistf(
                stats::as.formula(paste0(outcome, " ~ ", exposure, " + ", paste0(covariates, collapse = " + "))),
                data = data
            )
        } else {
            wgt_ind <- which(!is.na(data[[weight_var]]))
            mod <- logistf::logistf(
                stats::as.formula(paste0(outcome, " ~ ", exposure, " + ", paste0(covariates, collapse = " + "))),
                data = data[wgt_ind, ],
                weights = data[wgt_ind, ][[weight_var]]
            )
        }
        logor <- stats::coefficients(mod)[[exposure]]
        se    <- sqrt(diag(stats::vcov(mod)))[which(mod$terms == exposure)]
        p_val <- mod$prob[which(mod$terms == exposure)]
        est   <- c(logor, se)
        out <- data.table(
            exposure = exposure,
            beta     = logor,
            se_beta  = se,
            p_value  = p_val,
            log10p   = log10(p_val),
            sep      = sep_check
        )
    } else {
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
            log10p   = log10(stats::coef(summary(mod))[exposure, 4]),
            sep      = sep_check
        )
    }

    if (evalue == TRUE) {
        if (class(mod) == "logistf") {
            rare <- sum(as.data.table(mod$model)[[exposure]], na.rm = TRUE) / nrow(mod$model)
        } else {
            rare <- sum(mod$data[[exposure]], na.rm = TRUE) / nrow(mod$data)
        }
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
#' @param min_case_count minimum number of cases to consider an exposure (default = 20)
#' @param min_overlap_count minimum number of cases with exposure to consider an exposure (default = 5)
#' @param evalue logical; calculate evalues (default = TRUE)
#' @param verbose logical; print notes (default = FALSE)
#' @return data.table of cooccurrence analysis results
#' @importFrom data.table melt
#' @importFrom data.table data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table copy
#' @importFrom data.table rbindlist
#' @importFrom survey svydesign
#' @importFrom foreach foreach
#' @importFrom parallel makeCluster
#' @importFrom parallel stopCluster
#' @importFrom doParallel registerDoParallel
#' @importFrom cli cli_alert
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
    min_case_count     = 20,
    min_overlap_count  = 5,
    evalue             = TRUE,
    verbose            = FALSE
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

    # 1b. if min_overlap_count is specified, separate exposures with insufficient overlap
    if (!is.null(min_overlap_count)) {
        overlap <- list() 
        for (i in seq_along(exposures_to_consider)) {
            overlap[[i]] <- data.table::data.table(
                exposure = exposures_to_consider[i],
                overlap  = ifelse(
                    identical(data2[, .N, c(exposures_to_consider[i], "case")][get(exposures_to_consider[i]) == 1 & case == 1, N], integer(0)),
                    0,
                    data2[, .N, c(exposures_to_consider[i], "case")][get(exposures_to_consider[i]) == 1 & case == 1, N]
                )
            )
        }
        overlap <- data.table::rbindlist(overlap)
        if (overlap[overlap >= min_overlap_count, .N] == 0) {
            stop("No exposures with sufficient overlap with cases.")
        }
        if ((overlap[overlap >= min_overlap_count, .N] != nrow(overlap)) & verbose == TRUE) {
            cli::cli_alert(paste0(overlap[overlap < min_overlap_count, .N], " exposures to be estimated using logistf."))
        }
        exposures_to_consider <- overlap[overlap >= min_overlap_count, exposure]
        exposures_overlap     <- overlap[overlap < min_overlap_count, exposure]
    }

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
            name = "cooccurrence_analysis",
            total = length(exposures_to_consider)
        )
        for (i in seq_along(exposures_to_consider)) {
            out[[i]] <- .quick_cooccur_mod(
                data       = data2,
                covariates = covariates,
                exposure   = exposures_to_consider[i],
                weight_var = weight_var,
                weight_dsn = weight_design,
                evalue     = evalue
            )
            cli::cli_progress_update()
        }
        cli::cli_progress_done()
        if (!is.null(exposures_overlap)) {
            out2 <- list()
            cli::cli_progress_bar(
                name = "checking for separation",
                total = length(exposures_overlap)
            )
            for (i in seq_along(exposures_overlap)) {
                out2[[i]] <- .quick_cooccur_mod(
                    data             = data2,
                    covariates       = covariates,
                    exposure         = exposures_overlap[i],
                    weight_var       = weight_var,
                    evalue           = evalue,
                    check_separation = TRUE
                )
                cli::cli_progress_update()
            }
        }
        cli::cli_progress_done()
        if (!data.table::is.data.table(out)) out <- data.table::as.data.table(out)
        if (!is.null(exposures_overlap)) {
            if (!data.table::is.data.table(out2)) out2 <- data.table::as.data.table(out2)
            out <- data.table::rbindlist(list(out, out2), use.names = TRUE, fill = TRUE)
        }
    }

    if (parallel == TRUE) {
        cl <- parallel::makeCluster(n_cores, type = "PSOCK")
        doParallel::registerDoParallel(cl)
        columns <- exposures_to_consider
        cols    <- seq_along(exposures_to_consider)
        output <- foreach::foreach(i = cols, .combine = rbind) %dopar% {
            .quick_cooccur_mod(
                data             = data2,
                covariates       = covariates,
                exposure         = exposures_to_consider[i],
                weight_dsn       = weight_design,
                evalue           = evalue,
                check_separation = FALSE
            )
        }
        if (!is.null(exposures_overlap)) {
            output2 <- foreach::foreach(i = seq_along(exposures_overlap), .combine = rbind) %dopar% {
                .quick_cooccur_mod(
                    data             = data2,
                    covariates       = covariates,
                    exposure         = exposures_overlap[i],
                    weight_var       = weight_var,
                    evalue           = evalue,
                    check_separation = TRUE
                )
            }
        }
        parallel::stopCluster(cl)
        if (!data.table::is.data.table(output))  output  <- data.table::as.data.table(output)
        if (!is.null(exposures_overlap)) {
            if (!data.table::is.data.table(output2)) output2 <- data.table::as.data.table(output2)
            out <- data.table::rbindlist(list(output, output2), use.names = TRUE, fill = TRUE)
        }
    }

    return(out)
}

#' Helper: perform rapid unweighted and weighted correlation
#' @param x matrix of variables over which to calculate pairwise correlations
#' @param w vector of weights
#' @importFrom collapse pwcor
#' @importFrom stats na.omit
#' @importFrom data.table as.data.table
#' @return data.table with pairwise correlations

.correlation <- function(x, w = NULL) {
  if (!is.null(w)) {
    tmp <- cor(x)
  } else {
    tmp <- collapse::pwcor(x, w = w)
  }
  tmp[lower.tri(tmp, diag = TRUE)] <- NA
  tmp <- data.table::as.data.table(as.table(tmp))
  tmp <- stats::na.omit(tmp)
  tmp
}

#' Helper: perform rapid (parallel) partial correlation calculations on a matrix
#' @param x matrix of variables over which to calculate pairwise correlations
#' @param covs matrix of variables that serve as covariates
#' @param covs_alt alternate matrix of variables that serve as covariates when pairwise correlation is not complete
#' @param ncores number of cores over which to parallelize
#' @importFrom stats cor
#' @importFrom doMC registerDoMC
#' @importFrom foreach foreach
#' @importFrom foreach `%dopar%`
#' @importFrom data.table data.table
#' @importFrom data.table rbindlist
#' @importFrom collapse flm
#' @return data.table with unweighted pairwise partial correlations

.partial_correlation <- function(x, covs, covs_alt = NULL, ncores = 1) {
  doMC::registerDoMC(cores = ncores)
  columns <- colnames(x)
  cols    <- 1:ncol(x)
  output <- foreach::foreach(i = cols) %dopar% {
    out <- list()
    for (j in cols) {
      if (j >= i) next
      cca <- stats::complete.cases(x[, .SD, .SDcols = c(columns[c(i, j)])])
      if (sum(cca) == nrow(x)) {
        tmp <- stats::cor(
          collapse::flm(y = as.matrix(x[, ..i]), X = as.matrix(covs), return.raw = TRUE)$residuals,
          collapse::flm(y = as.matrix(x[, ..j]), X = as.matrix(covs), return.raw = TRUE)$residuals
        )
      } else if (sum(cca) < nrow(x) && sum(cca != 0)) {
        tmp <- stats::cor(
          collapse::flm(y = as.matrix(x[cca, ..i]), X = as.matrix(covs_alt[cca, ]), return.raw = TRUE)$residuals,
          collapse::flm(y = as.matrix(x[cca, ..j]), X = as.matrix(covs_alt[cca, ]), return.raw = TRUE)$residuals
        )
      }

      if (exists("tmp")) {
        out[[j]] <- data.table::data.table(
          "var1" = columns[i],
          "var2" = columns[j],
          "cor"  = tmp[1]
        )
      } else {
        out[[j]] <- data.table::data.table(
          "var1" = columns[i],
          "var2" = columns[j]
        )
      }
    }
    data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
  }
  output
}

#' Helper: perform rapid (parallel) weighted partial correlation calculations on a matrix
#' @param x matrix of variables over which to calculate pairwise correlations
#' @param covs matrix of variables that serve as covariates
#' @param covs_alt alternate matrix of variables that serve as covariates when pairwise correlation is not complete
#' @param ncores number of cores over which to parallelize
#' @param w vector of weights
#' @importFrom stats complete.cases
#' @importFrom doMC registerDoMC
#' @importFrom foreach foreach
#' @importFrom foreach `%dopar%`
#' @importFrom data.table copy
#' @importFrom data.table is.data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table data.table
#' @importFrom data.table rbindlist
#' @importFrom collapse flm
#' @importFrom wCorr weightedCorr
#' @return data.table with weighted pairwise partial correlations

.weighted_partial_correlation <- function(x, covs, covs_alt = NULL, w = NULL, ncores = 1) {
  doMC::registerDoMC(cores = ncores)
  x2      <- data.table::copy(data.table::as.data.table(x))
  columns <- colnames(x2)
  cols    <- 1:ncol(x2)

  # get residuals up front
  vars_with_missing <- names(which(sapply(x2, \(x) sum(is.na(x)) > 0)))
  vars_no_missing <- setdiff(names(x2), vars_with_missing)

  x2[, (vars_no_missing) := lapply(.SD, \(x) collapse::flm(y = as.matrix(x), X = as.matrix(covs), w = w, return.raw = TRUE)$residuals), .SDcols = vars_no_missing]

  if (is.null(covs_alt)) covs_alt <- covs

  for (i in seq_along(vars_with_missing)) {
    indx <- which(!is.na(x2[[vars_with_missing[i]]]))
    x2[indx, ][[vars_with_missing[i]]] <- collapse::flm(y = as.matrix(x2[indx, ][[vars_with_missing[i]]]), X = as.matrix(covs_alt[indx, ]), w = w[indx], return.raw = TRUE)$residuals
  }

  output <- foreach::foreach(i = cols) %dopar% {
    out <- list()
    for (j in cols) {
      if (j >= i) next
      cca <- stats::complete.cases(x2[, .SD, .SDcols = c(columns[c(i, j)])])
      if (sum(cca) == nrow(x2)) {
        tmp <- wCorr::weightedCorr(x = x2[[i]], y = x2[[j]], weights = w, method = "Pearson")
      } else if (sum(cca) < nrow(x2) && sum(cca != 0)) {
        tmp <- wCorr::weightedCorr(x = x2[cca, ][[i]], y = x2[cca, ][[j]], weights = w[cca], method = "Pearson")
      }

      if (exists("tmp")) {
        out[[j]] <- data.table::data.table(
          "var1" = columns[i],
          "var2" = columns[j],
          "cor"  = tmp[1]
        )
      } else {
        out[[j]] <- data.table::data.table(
          "var1" = columns[i],
          "var2" = columns[j]
        )
      }
    }
    data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
  }
  if (!data.table::is.data.table(output)) {
    data.table::rbindlist(output)
  } else {
    output
  }
}

#' Export foreach infix operator for use within package
#' @param obj left side of operator
#' @param ex right side of operator
#' @importFrom foreach `%dopar%`

`%dopar%` <- foreach::`%dopar%`

#' Export data.table infix operator for use within package
#' @param ... arguments defined for use in j of DT[i, j, k]
#' @importFrom data.table `:=`

`:=` <- data.table::`:=`

#' Perform rapid (parallel) unweighted and weighted (partial) correlations on a matrix
#' @param x matrix of variables over which to calculate pairwise correlations
#' @param covs matrix of variables that serve as covariates
#' @param covs_alt alternate matrix of variables that serve as covariates when pairwise correlation is not complete
#' @param w vector of weights
#' @param n_cores number of cores over which to parallelize
#' @param verbose logical; report what correlations are being calculate
#' @return data.table of pairwise correlations
#' @importFrom cli cli_alert
#' @importFrom data.table as.data.table
#' @importFrom data.table rbindlist
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(matrix(rnorm(100 * 50), ncol = 50))
#' covs <- data.frame(matrix(rnorm(100 * 2), ncol = 2))
#' wgt <- 1/runif(100)
#'
#' fcor(x = data, covs = covs, w = wgt, n_cores = 1)
#' }

fcor <- function(x, covs = NULL, covs_alt = NULL, w = NULL, n_cores = 1, verbose = FALSE) {
  if (is.null(covs) & is.null(w)){
    if (verbose) cli::cli_alert("unweighted correlation")
    tmp <- .correlation(x = x, w = NULL)
  }
  if (is.null(covs) & !is.null(w)){
    if (verbose) cli::cli_alert("weighted correlation")
    tmp <- .correlation(x = x, w = w)
  }
  if (!is.null(covs) & is.null(w)){
    if (verbose) cli::cli_alert("unweighted partial correlation")
    if (is.null(covs_alt)) {
        tmp <- .partial_correlation(x = x, covs = covs, ncores = n_cores)
    } else {
        tmp <- .partial_correlation(x = x, covs = covs, covs_alt = covs_alt, ncores = n_cores)
    }
    if (!data.table::is.data.table(tmp)) tmp <- data.table::rbindlist(tmp, use.names = TRUE, fill = TRUE)
  }
  if (!is.null(covs) & !is.null(w)){
    if (verbose) cli::cli_alert("weighted partial correlation")
    if (is.null(covs_alt)) {
        tmp <- .weighted_partial_correlation(x = x, covs = covs, w = w, ncores = n_cores)
    } else {
        tmp <- .weighted_partial_correlation(x = x, covs = covs, covs_alt = covs_alt, w = w, ncores = n_cores)
    }
    if (!data.table::is.data.table(tmp)) tmp <- data.table::rbindlist(tmp, use.names = TRUE, fill = TRUE)
  }
  return(tmp)
}


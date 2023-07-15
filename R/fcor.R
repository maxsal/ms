#' Helper: perform rapid unweighted and weighted correlation
#' @param x matrix of variables over which to calculate pairwise correlations
#' @param w vector of weights
#' @importFrom collapse pwcor
#' @importFrom stats na.omit
#' @importFrom data.table as.data.table
#' @return data.table with pairwise correlations

.wcor_helper <- function(x, w = NULL) {
  tmp <- collapse::pwcor(x, w = w)
  tmp[lower.tri(tmp, diag = TRUE)] <- NA
  tmp <- data.table::as.data.table(as.table(tmp))
  tmp <- stats::na.omit(tmp)
  tmp
}

#' Helper: perform rapid (parallel) partial correlation calculations on a matrix
#' @param x matrix of variables over which to calculate pairwise correlations
#' @param covs matrix of variables that serve as covariates
#' @param ncores number of cores over which to parallelize
#' @importFrom stats cor
#' @importFrom doMC registerDoMC
#' @importFrom foreach foreach
#' @importFrom foreach `%dopar%`
#' @importFrom data.table data.table
#' @importFrom data.table rbindlist
#' @importFrom collapse flm
#' @return data.table with unweighted pairwise partial correlations

.pcor_helper <- function(x, covs, ncores) {
  doMC::registerDoMC(cores = ncores)
  columns <- colnames(x)
  if (!is.matrix(x)) x <- as.matrix(x)
  cols    <- 1:ncol(x)
  output <- foreach::foreach(i = cols) %dopar% {
    out <- list()
    for (j in cols) {
      if (j >= i) next
      tmp <- stats::cor(
        collapse::flm(y = as.matrix(x[, i]), X = as.matrix(covs), return.raw = TRUE)$residuals,
        collapse::flm(y = as.matrix(x[, j]), X = as.matrix(covs), return.raw = TRUE)$residuals
      )
      out[[j]] <- data.table::data.table(
        "var1" = columns[i],
        "var2" = columns[j],
        "cor"  = tmp[1]
      )
    }
    data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
  }
  output
}

#' Helper: perform rapid (parallel) weighted partial correlation calculations on a matrix
#' @param x matrix of variables over which to calculate pairwise correlations
#' @param covs matrix of variables that serve as covariates
#' @param ncores number of cores over which to parallelize
#' @param w vector of weights
#' @importFrom stats cor
#' @importFrom doMC registerDoMC
#' @importFrom foreach foreach
#' @importFrom foreach `%dopar%`
#' @importFrom data.table data.table
#' @importFrom data.table rbindlist
#' @importFrom collapse flm
#' @return data.table with weighted pairwise partial correlations

.pwcor_helper <- function(x, covs, w = NULL, ncores) {
  doMC::registerDoMC(cores = ncores)
  columns <- colnames(x)
  if (!is.matrix(x)) x <- as.matrix(x)
  cols    <- 1:ncol(x)
  output <- foreach::foreach(i = cols) %dopar% {
    out <- list()
    for (j in cols) {
      if (j >= i) next
      tmp <- stats::cor(
        collapse::flm(y = as.matrix(x[, i]), X = as.matrix(covs), w = w, return.raw = TRUE)$residuals,
        collapse::flm(y = as.matrix(x[, j]), X = as.matrix(covs), w = w, return.raw = TRUE)$residuals
      )
      out[[j]] <- data.table::data.table(
        "var1" = columns[i],
        "var2" = columns[j],
        "cor"  = tmp
      )
    }
    data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
  }
  output
}

#' Export foreach infix operator for use within package
#' @param obj left side of operator
#' @param ex right side of operator
#' @importFrom foreach `%dopar%`

`%dopar%` <- foreach::`%dopar%`

#' Perform rapid (parallel) unweighted and weighted (partial) correlations on a matrix
#' @param x matrix of variables over which to calculate pairwise correlations
#' @param covs matrix of variables that serve as covariates
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

fcor <- function(x, covs = NULL, w = NULL, n_cores = 1, verbose = FALSE) {
  if (is.null(covs) & is.null(w)){
    if (verbose) cli::cli_alert("unweighted correlation")
    tmp <- .wcor_helper(x = x, w = NULL)
  }
  if (is.null(covs) & !is.null(w)){
    if (verbose) cli::cli_alert("weighted correlation")
    tmp <- .wcor_helper(x = x, w = w)
  }
  if (!is.null(covs) & is.null(w)){
    if (verbose) cli::cli_alert("unweighted partial correlation")
    tmp <- .pcor_helper(x = x, covs = covs, ncores = n_cores)
    if (!data.table::is.data.table(tmp)) tmp <- data.table::rbindlist(tmp)
  }
  if (!is.null(covs) & !is.null(w)){
    if (verbose) cli::cli_alert("weighted partial correlation")
    tmp <- .pwcor_helper(x = x, covs = covs, w = w, ncores = n_cores)
    if (!data.table::is.data.table(tmp)) tmp <- data.table::rbindlist(tmp)
  }
  return(tmp)
}


#' Helper for truncating probabilities
#' @param x numeric vector
#' @param probs vector of probabilities
#' @importFrom stats quantile
#' @return numeric vector
#' @export

chopr <- function(x, probs = c(0.025, 0.975)) {
  quant <- stats::quantile(x, probs = probs, na.rm = TRUE)
  x[x < quant[1L]] <- quant[1L]
  x[x > quant[2L]] <- quant[2L]
  x
}
#' Helper: mean standardize a vector
#' @param x vector of values to standardize
#' @importFrom stats sd
#' @return vector of mean standardized values

.scale_manual <- function(x) {
  (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE)
}

#' Rapidly standarize variables in a datasset
#' @param data data.table or data.frame of data
#' @param cols vector of column names or indices to standardize
#' @param all_numeric logical; standardize all numeric variables; will override `cols`
#' @return data.table of data with standardized variables
#' @importFrom data.table copy
#' @importFrom data.table set
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(
#' a = rnorm(10000, mean = 10, sd = 2),
#' b = rnorm(10000, mean = 10, sd = 2),
#' c = as.character(rnorm(10000, mean = 10, sd = 2)),
#' d = rnorm(10000, mean = 10, sd = 2)
#' )
#'
#' standardize_variables(data = data, all_numeric = TRUE)
#' }

standardize_variables <- function(data, cols = NULL, all_numeric = FALSE) {

  out <- data.table::copy(data)

  # Get all numeric variables
  if (all_numeric) {
    if (!is.null(cols)) {
      warning("You provided `all_numeric = TRUE` and `cols`. `cols` will be ignored.")
    }
    cols <- which(sapply(out, is.numeric))
  }

  # Check if cols is provided
  if (is.null(cols)) {
    stop("You must provide column names or indices, or set `all_numeric = TRUE`.")
  }

  # If cols is numeric, treat it as indices
  if (is.numeric(cols)) {
    columns <- names(out)[cols]
  } else {
    columns <- cols
  }

  # Check if columns exist in dt
  if (any(!columns %in% names(out))) {
    stop("Some columns were not found in the data.")
  }

  # Apply mean standardization to each column
  for (i in columns) {
    if (!is.numeric(out[[i]])) {
      message(paste0(i, " is not numeric. skipping for standardization."))
      next
    }
    data.table::set(x = out, j = i, value = .scale_manual(out[[i]]))
  }

  return(out[])

}

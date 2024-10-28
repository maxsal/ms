#' Quickly attanch phecode descriptions to a data.table
#' @param x a data.table with phecodes to merge in descriptions and other metadata
#' @param phe_col name of column in `x` that contains phecodes
#' @param simplify if TRUE, only return `phe_col` and `description` columns
#' @param group if TRUE, return `group` column
#' @param keep_old if not NULL, keep additional columns from `x`
#' @return data.table with phecode descriptions
#' @importFrom data.table is.data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table merge.data.table
#' @importFrom data.table setcolorder
#' @export
#' @examples
#' \dontrun{
#' phecode_sub <- ms::pheinfo[1:10, .(phecode)]
#' phe_desc(phecode_sub)
#' }

phe_desc <- function(x, phe_col = "phecode", simplify = FALSE, group = TRUE, keep_old = NULL) {
  # Convert to data.table if not already
  if (!data.table::is.data.table(x)) x <- data.table::as.data.table(x)

  # Select required columns
  vars <- c("phecode", "description")
  if (group) vars <- c(vars, "group")

  # Check the phe_col type and adjust phecode format if needed
  pheinfo_tmp <- ms::pheinfo[, ..vars]

  if (all(startsWith(x[[phe_col]], "X"))) {
    pheinfo_tmp[, phecode := paste0("X", phecode)]
  } else if (is.numeric(x[[phe_col]])) {
    stop("`phe_col` must be a character column")
  }

  # Merge with pheinfo data by phecode
  out <- data.table::merge.data.table(
    x,
    pheinfo_tmp,
    by.x = phe_col,
    by.y = "phecode",
    all.x = TRUE
  )

  # Conditionally simplify the output
  if (simplify) {
    keep <- c(phe_col, "description")
    if (group) keep <- c(keep, "group")
    if (!is.null(keep_old)) keep <- c(keep, keep_old)
    out <- out[, ..keep]
  }

  # Set final column order
  data.table::setcolorder(out, c(phe_col, "description"))

  return(out)
}

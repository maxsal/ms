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
    if (!data.table::is.data.table(x)) x <- data.table::as.data.table(x)
    if (group) {
        vars <- c("phecode", "description", "group")
    } else {
        vars <- c("phecode", "description")
    }
    pheinfo_tmp <- copy(ms::pheinfo)
    if (startsWith(x[[phe_col]][1], "X")) {
        pheinfo_tmp[, phecode := paste0("X", phecode)]
    } else if (is.numeric(x[[phe_col]])) {
        stop("`phe_col` must be a character column")
    }
    out <- data.table::merge.data.table(
        x,
        pheinfo_tmp[, ..vars],
        by.x = phe_col,
        by.y = "phecode"
    )
    if (simplify) {
        keep <- c(phe_col, setdiff(vars, "phecode"))
        if (!is.null(keep_old)) keep <- c(keep, keep_old)
        out <- out[, ..keep]
    }
    data.table::setcolorder(out, c(phe_col, "description"))
    return(out)
}

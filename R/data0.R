#' Prep data for obtaining predictions
#' @param data data.frame or data.table
#' @param vars character vector of variables to keep
#' @return Checks to see whether packages are already installed or loaded. If not installed, will install. If already loaded, will skip loading. Otherwise, will load libraries and provide a quick summary.
#' @importFrom data.table copy as.data.table
#' @export
#' @examples
#' \dontrun{
#' data0(mtcars, c("mpg", "cyl", "disp", "make", "model"))
#' }

data0 <- function(data, vars) {
    dt <- data.table::copy(data.table::as.data.table(data)) # make copy to avoid pass-by-reference
    # Identify the columns that are in the data.table but not in vars
    colsToRemove <- setdiff(names(dt), vars)

    # If any columns need to be removed do it
    if (length(colsToRemove) > 0) dt[, (colsToRemove) := NULL]

    # Identify the columns that are in vars but not in the data.table
    colsToAdd <- setdiff(vars, names(dt))

    # If any columns need to be added do it
    if (length(colsToAdd) > 0) {
        for (col in colsToAdd) {
            dt[, (col) := 0]
        }
    }
    return(dt[])
}

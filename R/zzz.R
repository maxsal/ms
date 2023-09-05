#' Print data.table with nice features
#'
#' @param libname Currently unused
#' @param pkgname Currently unused
#'
#' @return Sets print.data.table options
#' @export
#'

.onAttach <- function(libname, pkgname) {
  options(
    datatable.print.topn       = 5,
    datatable.print.class      = TRUE,
    datatable.print.trunc.cols = TRUE
    )
  attached <- ms_attach()
}

same_library <- function(pkg) {
  loc <- if (pkg %in% loadedNamespaces()) dirname(getNamespaceInfo(pkg, "path"))
  library(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE)
}

ms_attach <- function() {
  to_load <- "detectseparation"

  suppressPackageStartupMessages(
    lapply(to_load, same_library)
  )

  invisible(to_load)
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

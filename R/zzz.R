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
}

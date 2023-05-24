#' Print data.table as tibbles for viewing
#'
#' @param libname Currently unused
#' @param pkgname Currently unused
#'
#' @return Rewrites print.data.table in background
#' @export
#'

.onAttach <- function(libname, pkgname) {
  print_as_tibble <- function(dt) print(tibble::as_tibble(dt))
  utils::assignInNamespace("print.data.table", print_as_tibble , ns="data.table")
}

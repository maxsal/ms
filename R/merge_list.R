#' Quickly merge across a list using dplyr-based merge functions
#' @param ... a list object 
#' @param join_fn the function on which to join (default = dplyr::left_join)
#' @param by_var the name of the variable(s) on which to join
#' @param verbose logical; report merging variable and merge function
#' @return a merged table across objects in list ...
#' @importFrom cli cli_alert_warning
#' @importFrom cli cli_text
#' @importFrom dplyr full_join
#' @export
#' @examples
#' \dontrun{
#' data <- data.frame(
#'   id = letters[1:10],
#'   a  = 1:10,
#'   b  = 11:20,
#'   c  = 21:30
#' )
#' data_list <- list()
#' for (i in 2:ncol(data)) {
#'   data_list[[i - 1]] <- data[, c(1, i)]
#' }
#' merged <- merge_list(data_list, verbose = TRUE)
#' merged
#' }

merge_list <- function(..., join_fn = NULL, by_var = "id", verbose = FALSE) {
    if (!by_var %in% names(...[[1]])) {
        cli::cli_alert_warning("{.field {by_var}} not in names of dataset")
        stop("change 'by_var' argument")
    }
    if (is.null(join_fn)) {
        join_fn <- dplyr::left_join
        cli::cli_text("using {.fun dplyr::left_join} as default join function on {.field {by_var}}. specify {.field join_fn} to change")
    } else if (verbose) {
        cli::cli_text("merging on {.field {by_var}} using {.fun {deparse(substitute(join_fn))}}")
    }
    Reduce(\(x, y) join_fn(x, y, by = by_var), ...)
}

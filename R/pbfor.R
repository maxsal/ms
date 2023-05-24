#' For loop function with lapply syntax and pretty progress bar
#'
#' @param X Object to iterate over
#' @param FUN Function to apply to object X
#' @param pbname Name of progress bar (optional)
#' @param simplify Try simplifying output (TRUE/FALSE)
#' @param ... Additional arguments to pass to FUN
#'
#' @return Returns output from iteration
#' @export

pbfor <- function(X, FUN, pbname = NULL, simplify = FALSE, ...) {

  cli::cli_progress_bar(name = pbname, total = length(X))

  out <- list()
  for (i in seq_along(X)) {
    out[[i]] <- FUN(X[[i]], ...)
    cli::cli_progress_update()
  }

  cli::cli_progress_done()

  if (simplify) {
    tryCatch(
      {
        do.call(rbind, out)
      },
      error = \(mes) {
        cli::cli_alert_danger("Unable to simplify (error)")
        cli::cli_alert_info(mes)
        return(out)
      },
      warning = \(mes) {
        cli::cli_alert_warning("Unable to simplify (warning)")
        cli::cli_alert_info(mes)
        return(out)
      }
    )

  } else {
    out
  }

}

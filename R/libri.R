#' Helper: Check for packages needed to use libri()
#' @return Logical. TRUE if `cli` and `remotes` packages exist in installed packages, FALSE otherwise.

.check_for_deps <- function() {

  nzchar(system.file(package = "cli")) & nzchar(system.file(package = "remotes"))

}

#' Helper: Check to see if packages are installed
#' @param x Package name as string
#' @return Logical. TRUE if the package exists, FALSE it not, indicating need for installation.

.check_package_installed <- function(x) {

  nzchar(system.file(package = x))

}

#' Helper: Check to see if packages are loaded
#' @param x Package name as string
#' @return Logical. TRUE if the package exists, FALSE it not, indicating need for installation.

.check_library_loaded <- function(x) {

  x %in% (.packages())

}

#' Check for, install, and load CRAN and GitHub R packages
#' @param ... A list of unquoted R packages to install/load
#' @param verbose return summary of install, load, and preloaded
#' @return Checks to see whether packages are already installed or loaded. If not installed, will install. If already loaded, will skip loading. Otherwise, will load libraries and provide a quick summary.
#' @importFrom pak pkg_install
#' @importFrom cli cli_h1
#' @importFrom cli cli_alert_info
#' @importFrom cli cli_alert_success
#' @export
#' @examples
#' \dontrun{
#' libri(tidyverse, janitor, glue, maxsal/covid19india)
#' }

libri <- function(..., verbose = TRUE) {

  if (.check_for_deps() == FALSE) {
    stop("`remotes` and `cli` packages are required: `install.packages(c('remotes', 'cli'))`")
  }

  # list of libraries ----------
  libs      <- as.character(eval(substitute(alist(...))))
  preloaded <- 0
  installed <- 0
  newloads  <- 0

  preloaded_libs <- NULL
  installed_libs <- NULL
  newload_libs   <- NULL

  for (i in seq_along(libs)) {

    tmp_lib <- sub(".*/", "", libs[i])

    # check install -----------
    if (.check_package_installed(tmp_lib) == FALSE) {

      if (grepl("/", libs[i]) == TRUE) {
        pak::pkg_install(libs[i])
      } else {
        pak::pkg_install(libs[i])
      }
      installed <- installed + 1

      if (is.null(installed_libs)) {
        installed_libs <- libs[i]
      } else {
        installed_libs <- c(installed_libs, libs[i])
      }

    }

    # check loaded ----------

    if (.check_library_loaded(tmp_lib) == TRUE) {

      preloaded      <- preloaded + 1
      if (is.null(preloaded_libs)) {
        preloaded_libs <- libs[i]
      } else {
        preloaded_libs <- c(preloaded_libs, libs[i])
      }
    }

    # load unloaded libraries ----------

    if (.check_library_loaded(tmp_lib) == FALSE) {

      base::suppressPackageStartupMessages(base::library(tmp_lib, character.only = T))
      newloads     <- newloads + 1
      if (is.null(newload_libs)) {
        newload_libs <- tmp_lib
      } else {
        newload_libs <- c(newload_libs, tmp_lib)
      }

    }

  }

  # output summary ----------
  if (verbose) {
    cli::cli_h1("Summary")
    if (preloaded > 0) cli::cli_alert_info("{preloaded} librar{?y/ies} already loaded: {paste(preloaded_libs, collapse = ', ')}")
    if (installed > 0) cli::cli_alert_info("{installed} librar{?y/ies} installed: {paste(installed_libs, collapse = ', ')}")
    if (newloads > 0) cli::cli_alert_success("{newloads} librar{?y/ies} loaded: {paste(newload_libs, collapse = ', ')}")
  }

}

#' PheWAS catalog information
#'
#' A table listing PheWAS X Codes (or phecodes) and their corresponding
#' description, category, and other information.
#' Last manually curated on June 1, 2023.
#'
#' @format ## `pheinfox`
#' A data frame with 3,612 rows and 8 columns:
#' \describe{
#'   \item{phecode}{PheWAS Code (aka phecode)}
#'   \item{description}{Phecode description}
#'   \item{group}{Phecode disease category or group}
#'   \item{groupnum}{A numeric value corresponding to `phecode` `group`}
#'   \item{order}{A variable ordering by `groupnum` and then numeric `phecode` value, useful for plotting}
#'   \item{color}{Phecode group color inspired by Okabe-Ito colorblind-friendly palette}
#'   \item{color_original}{Original `phecode` `group` color scheme}
#'   \item{sex}{Defines `male` or `female` specific phecodes, otherwise `both`}
#'   ...
#' }
#' @source Curated version of files available via the phecodeX github page (https://github.com/PheWAS/PhecodeX). 6 September 2023
"pheinfox"

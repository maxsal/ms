#' PhenomeX phenome structure
#'
#' An unedited table of PheWAS X Codes (or phecodes) and their "roll up" structure.
#' Downloaded from github.com/PheWAS/phecodex/ on 2023-09-21.
#'
#' @format ## `phecodex_rollup`
#' A data frame with 7,956 rows and 2 columns:
#' \describe{
#'   \item{code}{Primary phecode label}
#'   \item{phecode_unrolled}{A phecode that is implied by the primary phecode label}
#'   ...
#' }
#' @source Unedited, raw version of phecodeX_R_rollup_map.csv available via the phecodeX github page (https://github.com/PheWAS/PhecodeX). 2023-09-21.
"phecodex_rollup"

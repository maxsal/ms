#' PhenomeX phecode-sex mappings
#'
#' An unedited table of PheWAS X Codes (or phecodes) and whether they are sex-specific.
#' Downloaded from github.com/PheWAS/phecodex/ on 2023-09-21.
#'
#' @format ## `phecodex_sex`
#' A data frame with 3,612 rows and 3 columns:
#' \describe{
#'   \item{phecode}{The phecode label}
#'   \item{male_only}{A true/false indicator of whether the specific code is used more than 90\% of the time with EHR-reported male sex}
#'   \item{female_only}{A true/false indicator of whether the specific code is used more than 90\% of the time with EHR-reported female sex}
#'  ...
#' }
#' @source Unedited, raw version of phecodeX_R_sex.csv available via the phecodeX github page (https://github.com/PheWAS/PhecodeX). 2023-09-21.
"phecodex_sex"

#' PhenomeX ICD9/10CM mapping
#'
#' An unedited table of PheWAS X Codes (or phecodes) and their ICD9/10CM mappings.
#' Downloaded from github.com/PheWAS/phecodex/ on 2023-09-21.
#'
#' @format ## `phecodex_icdcm_map`
#' A data frame with 79,583 rows and 3 columns:
#' \describe{
#'   \item{code}{The code included in the phecode grouping (current supported code types are ICD-9-CM and ICD-10-CM)}
#'   \item{vocabulary_id}{A string indicating the code type (ICD9CM or ICD10CM)}
#'   \item{phecode}{The phecode label}
#'   ...
#' }
#' @source Unedited, raw version of phecodeX_R_map.csv available via the phecodeX github page (https://github.com/PheWAS/PhecodeX). 2023-09-21.
"phecodex_icdcm_map"

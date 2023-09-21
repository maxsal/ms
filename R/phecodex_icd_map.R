#' PhenomeX ICD10 (WHO) mapping
#'
#' An unedited table of PheWAS X Codes (or phecodes) and their ICD10 mappings.
#' Downloaded from https://phewascatalog.org/phecode_x on 2023-09-21.
#'
#' @format ## `phecodex_icd_map`
#' A data frame with 11,403 rows and 7 columns:
#' \describe{
#'   \item{icd}{The code included in the phecode grouping (current supported code types are ICD-10)}
#'   \item{vocabulary_id}{A string indicating the code type (ICD10)}
#'   \item{ICD_string}{A descriptive label for ICD code}
#'   \item{phecode}{The phecode label}
#'   \item{phecode_string}{A descriptive label for phecode}
#'   \item{category_num}{A numeric value corresponding to the phecode category}
#'   \item{category}{A string indicating the phecode category}
#'   ...
#' }
#' @source Unedited, raw version of phecodeX_ICD_WHO_map_flat.csv available via the PheWAS Catalog (https://phewascatalog.org/phecode_x). 2023-09-21.
"phecodex_icd_map"

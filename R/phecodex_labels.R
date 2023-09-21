#' PhenomeX label information
#'
#' An unedited table of PheWAS X Codes (or phecodes) and their corresponding
#' description, category, and other information.
#' Downloaded from github.com/PheWAS/phecodex/ on 2023-09-21.
#'
#' @format ## `phecodex_labels`
#' A data frame with 3,612 rows and 6 columns:
#' \describe{
#'   \item{phenotype}{The phecode label (two letters, “_”, and numeric phecode)}
#'   \item{description}{A descriptive label for phecode}
#'   \item{icd10_only}{A Boolean value: 1 if the phecode is defined only by ICD-10 codes; 0 if the phecode is defined by both ICD-9 and -10 codes}
#'   \item{groupnum}{A numeric value corresponding to the phecode category}
#'   \item{group}{A string indicating the phecode category}
#'   \item{color}{A string value indicating the color to use in plots for each group}
#'   ...
#' }
#' @source Unedited, raw version of phecodeX_R_labels.csv available via the phecodeX github page (https://github.com/PheWAS/PhecodeX). 2023-09-21.
"phecodex_labels"

#' ICD10 code description
#'
#' A table listing ICD-10 codes and their corresponding descriptions.
#' Last manually curated on August 18, 2023.
#'
#' @format ## `icd10`
#' A data frame with 16,168 rows and 3 columns:
#' \describe{
#'   \item{code}{ICD-10code}
#'   \item{description}{Description of code}
#'   \item{vocabulary_id}{Incidates that the code is from the ICD-10 vocabulary}
#'   ...
#' }
#' @source ICD-10 from Athena ODHSI downloaded from https://athena.ohdsi.org/ on August 9, 2023.
"icd10"
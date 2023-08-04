#' ICD10CM code description
#'
#' A table listing ICD-10-CM codes and their corresponding descriptions.
#' Last manually curated on August 4, 2023.
#'
#' @format ## `icd10cm`
#' A data frame with 97,296 rows and 6 columns:
#' \describe{
#'   \item{order}{Order number, right justified, zero filled.}
#'   \item{code}{ICD-10-CM or ICD-10-PCS code. Dots added}
#'   \item{valid}{Indicator for whether code is valid for submission for HIPAA-covered transactions.}
#'   \item{short_description}{Short description of code.}
#'   \item{long_description}{Long description of code.}
#'   \item{vocabulary_id}{Incidates that the code is from the ICD-10-CM vocabulary. Does not distinguish between ICD-10-CM and ICD-10-PCS.}
#'   ...
#' }
#' @source October 1, 2023 release of ICD-10-CM downloaded from https://www.cdc.gov/nchs/icd/Comprehensive-Listing-of-ICD-10-CM-Files.htm on August 4, 2023.
"icd10cm"
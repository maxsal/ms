#' ICD9CM code description
#'
#' A table listing ICD-9-CM diagnosis and procedure codes and their corresponding descriptions.
#' Last manually curated on August 4, 2023.
#'
#' @format ## `icd9cm`
#' A data frame with 18,449 rows and 4 columns:
#' \describe{
#'   \item{code}{ICD-9-CM diagnosis or procedure code. Dots added}
#'   \item{description}{Short description of code.}
#'   \item{vocabulary_id}{Incidates that the code is from the ICD-9-CM vocabulary. Does not distinguish between ICD-9-CM diagnosis or procedure codes.}
#'   \item{diagnosis}{Indicates whether the code is a diagnosis code (1) or a procedure code (0).}
#'   ...
#' }
#' @source Version 32 ICD-9-CM effective October 14, 2014 downloaded from https://www.cms.gov/medicare/coding/icd9providerdiagnosticcodes/codes on August 4, 2023.
"icd9cm"
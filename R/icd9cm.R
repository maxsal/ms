#' ICD9CM code description
#'
#' A table listing ICD-9-CM diagnosis and procedure codes and their corresponding descriptions.
#' Last manually curated on August 18, 2023.
#'
#' @format ## `icd9cm`
#' A data frame with 17,564 rows and 4 columns:
#' \describe{
#'   \item{code}{ICD-9-CM diagnosis code}
#'   \item{description}{Description of code}
#'   \item{vocabulary_id}{Incidates that the code is from the ICD-9-CM vocabulary}
#'   ...
#' }
#' @source ICD-9-CM from Athena ODHSI downloaded from https://athena.ohdsi.org/ on August 9, 2023.
"icd9cm"
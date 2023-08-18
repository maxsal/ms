#' Quickly attach ICD-10 and ICD-9/ICD10-CM codes descriptions to a data.table
#' @param x a data.table with phecodes to merge in descriptions and other metadata
#' @param code_col name of column in `x` that contains ICD codes
#' @param vocab_col name of column in `x` that contains ICD vocabulary (ICD9CM, ICD10, or ICD10CM)
#' @param simplify if TRUE, only return `code_col`, `vocabulary_id`, and `description` columns
#' @param keep_old if not NULL, keep additional columns from `x`
#' @return data.table with ICD code descriptions
#' @importFrom data.table is.data.table
#' @importFrom data.table as.data.table
#' @importFrom data.table merge.data.table
#' @importFrom data.table setcolorder
#' @export
#' @examples
#' \dontrun{
#' icd <- data.frame(
#'   vocabulary_id = c("ICD9CM", "ICD10CM"),
#'   code = c("185", "F32.A")
#' )
#' icd_desc(icd)
#' }

icd_desc <- function(x, code_col = "code", vocab_col = "vocabulary_id", simplify = FALSE, keep_old = NULL) {
    if (!data.table::is.data.table(x)) x <- data.table::as.data.table(x)
    if (!any(c("ICD9CM", "ICD10CM") %in% x[[vocab_col]])) {
        stop("`vocabulary_id` must contain ICD9CM, ICD10, and/or ICD10CM")
    }
    out <- data.table::copy(x)

    if ("ICD9CM" %in% unique(x[[vocab_col]])) {
        tmp9cm <- ms::icd9cm[code %in% out[get(vocab_col) == "ICD9CM", get(code_col)], .(code, vocabulary_id, description)]
    }

    if ("ICD10CM" %in% unique(x[[vocab_col]])) {
        tmp10 <- ms::icd10[code %in% out[get(vocab_col) == "ICD10", get(code_col)], .(code, vocabulary_id, description)]
    }

    if ("ICD10CM" %in% unique(x[[vocab_col]])) {
        tmp10cm <- ms::icd10cm[code %in% out[get(vocab_col) == "ICD10CM", get(code_col)], .(code, vocabulary_id, description)]
    }

    for (i in c("tmp9cm", "tmp10", "tmp10cm")) {
        if (!exists(i)) next
        if (!exists("tmp")) {
            tmp <- get(i)
        } else {
            tmp <- rbind(tmp, get(i))
        }
    }

    out <- merge.data.table(
        out,
        tmp,
        by.x = c(code_col, vocab_col),
        by.y = c("code", "vocabulary_id"),
        all.x = TRUE
    )

    if (simplify) {
        keep <- c(code_col, vocab_col, "description")
        if (!is.null(keep_old)) keep <- c(keep, keep_old)
        out <- out[, ..keep]
    }
    data.table::setcolorder(out, c(code_col, vocab_col, "description"))
    return(out)
}

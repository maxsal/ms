% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cooccurrence_analysis.R
\name{cooccurrence_analysis}
\alias{cooccurrence_analysis}
\title{Perform rapid unweighted or weighted cooccurrence analysis}
\usage{
cooccurrence_analysis(
  data,
  covariates,
  possible_exposures = ms::pheinfox[, phecode],
  weight_var = NULL,
  n_cores = 1,
  parallel = FALSE,
  min_case_count = 20,
  min_overlap_count = 5,
  evalue = TRUE,
  verbose = FALSE,
  logistf_pl = TRUE,
  detect_separation = TRUE
)
}
\arguments{
\item{data}{dataset containing outcome, exposures, covariates, and, if weighted, weight variable}

\item{covariates}{vector of covariates names}

\item{possible_exposures}{vector of exposure names}

\item{weight_var}{weight variable name}

\item{n_cores}{number of cores over which to parallelize (default = 1)}

\item{parallel}{logical; parallelize analyses (default = FALSE)}

\item{min_case_count}{minimum number of cases to consider an exposure (default = 20)}

\item{min_overlap_count}{minimum number of cases with exposure to consider an exposure (default = 5)}

\item{evalue}{logical; calculate evalues (default = TRUE)}

\item{verbose}{logical; print notes (default = FALSE)}

\item{logistf_pl}{logical; use profile likelihood (TRUE) for confidence intervals and tests or Wald (FALSE) in logistf; (default = TRUE)}

\item{detect_separation}{logical; use detect_separation (TRUE) or skip and use logistf regardless (FALSE); (default = TRUE)}
}
\value{
data.table of cooccurrence analysis results
}
\description{
Perform rapid unweighted or weighted cooccurrence analysis
}
\examples{
\dontrun{
# TBD
}
}

#' Helper: create formula for analysis
#' @param exposure name of exposure variable
#' @param outcome name of outcome variable
#' @param covariates vector of covariate names
#' @return formula for analysis
create_formula <- function(outcome, exposure, covariates = NULL) {
  if (is.null(covariates) || length(covariates) == 0) {
    formula_text <- paste(outcome, "~", exposure)
  } else {
    covariate_text <- paste(covariates, collapse = " + ")
    formula_text   <- paste(outcome, "~", exposure, "+", covariate_text)
  }
  as.formula(formula_text)
}


#' Helper: function to run a binary analysis using glm
#' @param data data frame
#' @param outcome name of outcome variable
#' @param exposure name of exposure variable
#' @param covariates vector of covariate names
#' @param weight_var name of weight variable
#' @importFrom dplyr pull
#' @importFrom dplyr slice
#' @return list of results from model for the exposure
glm_analysis <- function(data, outcome, exposure, covariates = NULL, weight_var = NULL) {
  formula <- create_formula(outcome, exposure, covariates)
  if (!is.null(weight_var)) {
    weight_index <- data |> dplyr::pull(!!weight_var) |> (\(x) !is.na(x))() |> which()
    fit          <- glm(formula,
                        data = data |> dplyr::slice(weight_index),
                        weights = data |> dplyr::slice(weight_index) |> dplyr::pull(!!weight_var),
                        family = binomial())
    method_out <- "wglm"
  } else {
    fit <- glm(formula, data = data, family = quasibinomial())
    method_out <- "glm"
  }
  beta    <- coef(fit)[exposure]
  se      <- summary(fit)$coefficients[exposure, "Std. Error"]
  p_value <- summary(fit)$coefficients[exposure, 4]
  return(list(phecode = exposure, beta = beta, se = se, p_value = p_value, log10p = log10(p_value), method = method_out))
}


#' Helper: function to run a binary analysis using brglm2
#' @param data data frame
#' @param outcome name of outcome variable
#' @param exposure name of exposure variable
#' @param covariates vector of covariate names
#' @param weight_var name of weight variable
#' @importFrom dplyr pull
#' @importFrom dplyr slice
#' @importFrom brglm2 brglm_fit
#' @importFrom brglm2 brglm_control
#' @return list of results from model for the exposure
brglm2_analysis <- function(data, outcome, exposure, covariates = NULL, weight_var = NULL) {
  formula <- create_formula(outcome, exposure, covariates)
  if (!is.null(weight_var)) {
    weight_index <- data |> dplyr::pull(!!weight_var) |> (\(x) !is.na(x))() |> which()
    fit <- glm(formula,
               data = data |> dplyr::slice(weight_index),
               weights = data |> dplyr::slice(weight_index) |> dplyr::pull(!!weight_var),
               family = binomial(), method = brglm2::brglm_fit, type = 'AS_mean',
               control = brglm2::brglm_control(maxit = 1000, max_step_factor = 0.5))
    method_out <- "wbrglm2"
  } else {
    fit <- glm(formula, data = data, family = binomial(), method =  brglm2::brglm_fit, type = "AS_mean",
               control = brglm2::brglm_control(maxit = 1000, max_step_factor = 0.5))
    method_out <- "brglm2"
  }
  beta    <- coef(fit)[exposure]
  se      <- summary(fit)$coefficients[exposure, "Std. Error"]
  p_value <- summary(fit)$coefficients[exposure, "Pr(>|z|)"]
  return(list(phecode = exposure, beta = beta, se = se, p_value = p_value, log10p = log10(p_value), method = method_out))
}


#' Helper: function to run a binary analysis using logistf
#' @param data data frame
#' @param outcome name of outcome variable
#' @param exposure name of exposure variable
#' @param covariates vector of covariate names
#' @param weight_var name of weight variable
#' @param logistf_pl whether to use profile likelihood in logistf
#' @importFrom dplyr pull
#' @importFrom dplyr filter
#' @importFrom logistf logistf
#' @importFrom logistf logistf.control
#' @return list of results from model for the exposure
logistf_analysis <- function(data, outcome, exposure, covariates = NULL, logistf_pl = FALSE, weight_var = NULL) {
  formula <- create_formula(outcome, exposure, covariates)
  if (!is.null(weight_var)) {
    fit     <- do.call(
      logistf::logistf,
      list(
        formula = formula,
        data    = data[!is.na(data[[weight_var]]), ],
        pl      = logistf_pl,
        weights = data[!is.na(data[[weight_var]]), ][[weight_var]],
        control = logistf::logistf.control(maxit = 1000, maxstep = 0.5)
      )
    )
    method_out <- "wlogistf"
  } else {
    fit     <- logistf::logistf(formula, data = data, pl = logistf_pl,
                       control = logistf::logistf.control(maxit = 1000, maxstep = 0.5))
    method_out <- "logistf"
  }
  beta    <- coef(fit)[exposure]
  se      <- sqrt(diag(vcov(fit)))[2]
  p_value <- fit$prob[exposure]
  return(list(phecode = exposure, beta = beta, se = se, p_value = p_value, log10p = log10(p_value), method = method_out))
}

#' Helper: function to run a binary analysis using survey::svyglm
#' @param design survey design object
#' @param outcome name of outcome variable
#' @param exposure name of exposure variable
#' @param covariates vector of covariate names
#' @importFrom survey svyglm
#' @return list of results from model for the exposure
svyglm_analysis <- function(design, outcome, exposure, covariates = NULL) {
  formula <- create_formula(outcome, exposure, covariates)
  fit     <- survey::svyglm(formula, design = design, family = "quasibinomial")
  beta    <- coef(fit)[exposure]
  se      <- summary(fit)$coefficients[exposure, "Std. Error"]
  p_value <- summary(fit)$coefficients[exposure, "Pr(>|t|)"]
  return(list(phecode = exposure, beta = beta, se = se, p_value = p_value, log10p = log10(p_value), method = "svyglm"))
}

#' Helper: function to run a binary analysis using SPAtest
#' @param data data frame
#' @param outcome name of outcome variable
#' @param exposure name of exposure variable
#' @param covariates vector of covariate names
#' @importFrom SPAtest ScoreTest_SPA
#' @return list of results from model for the exposure
SPAtest_analysis <- function(data, outcome, exposure, covariates = NULL) {
  if (!is.null(covariates)) {
    fit <- SPAtest::ScoreTest_SPA(
      genos = data[[exposure]],
      pheno = data[[outcome]],
      cov   = data[, ..covariates],
      beta.out = TRUE,
      beta.Cutoff = 1
    )
  } else {
    fit <- SPAtest::ScoreTest_SPA(
      genos       = data[[exposure]],
      pheno       = data[[outcome]],
      beta.out    = TRUE,
      beta.Cutoff = 1
    )
  }
  return(list(phecode = exposure, beta = fit$beta, se = fit$SEbeta, p_value = fit$p.value, log10p = log10(fit$p.value), method = "ScoreTest_SPA"))
}

#' Helper: function to run a weighted binary analysis using svyglm unless there's a separation concern, then run weighted logsitf
#' @param data data frame
#' @param design survey design object
#' @param outcome name of outcome variable
#' @param exposure name of exposure variable
#' @param covariates vector of covariate names
#' @param method method to use for analysis
#' @param logistf_pl whether to use profile likelihood in logistf
#' @param weight_var name of weight variable
#' @importFrom SPAtest ScoreTest_SPA
#' @return list of results from model for the exposure
weighted_analysis <- function(data, design, outcome, exposure, covariates, method = "svyglm", logistf_pl = FALSE, weight_var = NULL) {
  if (sum(data[[exposure]], na.rm = TRUE) < 20 | sum(data[[outcome]] == 1 & data[[exposure]] == 1, na.rm = TRUE) < 10) {
    method <- "logistf"
  }
  switch(method,
         "svyglm"  = svyglm_analysis(design, outcome, exposure, covariates),
         "logistf" = logistf_analysis(data, outcome, exposure, covariates, logistf_pl = logistf_pl, weight_var = weight_var))
}

#' function to run a binary analysis using a method specified by the method argument
#' @param data data frame
#' @param design survey design object
#' @param outcome name of outcome variable
#' @param exposure name of exposure variable
#' @param covariates vector of covariate names
#' @param method method to use for analysis
#' @param logistf_pl whether to use profile likelihood in logistf
#' @param .weight_var name of weight variable
#' @param verbose whether to print offending exposure
#' @param show_error whether to show error messages
#' @return list of results from model for the exposure
#' @export
phewas_analysis <- function(data = NULL, design = NULL, outcome, exposure, covariates, method, logistf_pl = FALSE, .weight_var = NULL, verbose = TRUE, show_error = FALSE) {
  tryCatch({
    switch(
      method,
        "glm"      = glm_analysis(data, outcome, exposure, covariates, weight_var = .weight_var),
        "brglm2"   = brglm2_analysis(data, outcome, exposure, covariates, weight_var = .weight_var),
        "logistf"  = logistf_analysis(data, outcome, exposure, covariates, logistf_pl = logistf_pl, weight_var = .weight_var),
        "SPAtest"  = SPAtest_analysis(data, outcome, exposure, covariates),
        "svyglm"   = svyglm_analysis(design, outcome, exposure, covariates),
        "weighted" = weighted_analysis(
          data = data, design = design, outcome = outcome, exposure = exposure,
          covariates = covariates, logistf_pl = logistf_pl, weight_var = .weight_var
        )
    )
  }, error = function(err_msg) {
      if (verbose) print(paste0("skipping: ", exposure, " [", ms::pheinfox[phecode == exposure, description], "]."))
      if (show_error) print(err_msg)
        list(phecode = exposure)
      })
}

#' function to map a binary analysis by vector of exposures using a method specified by the method argument
#' @param data data frame
#' @param design survey design object
#' @param outcome name of outcome variable
#' @param exposures vector of exposure variables in data
#' @param covariates vector of covariate names
#' @param method method to use for analysis
#' @param logistf_pl whether to use profile likelihood in logistf
#' @param .weight_var name of weight variable
#' @param workers number of workers to use for parallelization
#' @param plan_strategy strategy for parallelization
#' @param pwide_sig whether to add a column for significant results after p-value correction
#' @importFrom furrr future_map
#' @importFrom dplyr bind_rows
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom utils object.size
#' @return return table of results from model for the exposures
#' @export
map_phewas <- function(
  data = NULL,
  design = NULL,
  outcome,
  exposures,
  covariates,
  method,
  logistf_pl    = FALSE,
  .weight_var   = NULL,
  workers       = 1,
  plan_strategy = future::multicore,
  pwide_sig     = TRUE
) {
  if (is.null(design)) {
    objects_size <- object.size(data)
  } else {
    objects_size <- object.size(data) + object.size(design)
  }
  
  if (is.null(options("future.globals.maxSize")[[1]])) {
    if (objects_size > (450*1024^2)) {
        options(future.globals.maxSize = objects_size + 50 * 1024^2)
    }
  } else if (options("future.globals.maxSize")[[1]] < objects_size + 50 * 1024^2) {
    options(future.globals.maxSize = objects_size + 50 * 1024^2)
  }

  future::plan(plan_strategy, workers = workers)
  exposures |>
    furrr::future_map_dfr(
      \(x) {
        phewas_analysis(
          data        = data,
          design      = design,
          outcome     = outcome,
          exposure    = x,
          covariates  = covariates,
          method      = method,
          logistf_pl  = logistf_pl,
          .weight_var = .weight_var
        )
      },
      .progress = TRUE
    ) |>
    dplyr::arrange(p_value) |>
    (\(x) if (pwide_sig) {
      x |>
      dplyr::mutate(
        pwide_sig = dplyr::case_when(
          p_value < 0.05 / n() ~ 1,
          TRUE                 ~ 0
        )
      )
    } else {
      x
    })()
}

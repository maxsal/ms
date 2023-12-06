#' Conduct a tidy random forest analysis with hyperparameter tuning via tidymodels framework
#' @param data A data frame
#' @param outcome_var A string representing outcome variable
#' @param drop_vars A vector of strings representing variables to drop
#' @param split_prop A number representing proportion of data to use for training
#' @param num_threads A number representing number of threads to use
#' @param importance A string representing importance method
#' @param mode A string representing mode
#' @param num_trees A number representing number of trees
#' @param levels A number representing number of levels
#' @param best_metric A string representing best metric
#' @param ... Additional arguments to pass to parsnip::rand_forest()
#' @return A list of objects
#' @importFrom rsample initial_split training testing vfold_cv
#' @importFrom dplyr select mutate
#' @importFrom tidyr one_of
#' @importFrom parsnip rand_forest set_engine set_mode
#' @importFrom dials grid_regular finalize mtry min_n
#' @importFrom workflows workflow add_model add_formula
#' @importFrom tune tune_grid collect_metrics select_best finalize_workflow last_fit collect_predictions extract_workflow extract_fit_parsnip extract_fit_engine
#' @importFrom yardstick roc_curve
#' @importFrom vip vip
#' @importFrom cli cli_progress_step cli_progress_done
#' @importFrom ranger ranger
#' @importFrom ggplot2 ggplot geom_line geom_point facet_wrap scale_x_log10 scale_color_viridis_d autoplot
#' @importFrom scales label_number
#' @importFrom hardhat tune
#' @export

tidy_forest <- function(
    data,
    outcome_var,
    drop_vars   = NULL,
    split_prop  = 0.5,
    num_threads = 6,
    importance  = "permutation",
    mode        = "classification",
    num_trees   = 500,
    levels      = 5,
    best_metric = "accuracy",
    ...
) {
  
  data_split <- rsample::initial_split(
    data |> dplyr::select(-tidyr::one_of(drop_vars)),
    prop   = split_prop,
    strata = outcome_var
  )
  
  data_train <- rsample::training(data_split)
  data_test  <- rsample::testing(data_split)
  
  tune_spec <- parsnip::rand_forest(
      mtry  = hardhat::tune(),
      min_n = hardhat::tune()
    ) |>
      parsnip::set_engine(
        "ranger",
        num.threads = num_threads,
        importance  = importance,
        num.trees   = num_trees,
        ...
      ) |>
      parsnip::set_mode(mode = mode)
  
  forest_grid <- dials::grid_regular(
    dials::finalize(dials::mtry(), data_train |> dplyr::select(-tidyr::one_of(outcome_var))),
    dials::min_n(),
    levels = levels
  )
  
  data_folds <- rsample::vfold_cv(data_train)
  
  forest_wf <- workflows::workflow() |>
    workflows::add_model(tune_spec) |>
    workflows::add_formula(as.formula(paste0(outcome_var, " ~ .")))
  
  cli::cli_progress_step("selecting hyperparameters")
  forest_res <- forest_wf |>
    tune::tune_grid(
      resamples = data_folds,
      grid      = forest_grid
    )
  cli::cli_progress_done()
  
  diag_plot <- forest_res |>
    tune::collect_metrics() |>
    dplyr::mutate(min_n = factor(min_n)) |>
    ggplot2::ggplot(aes(mtry, mean, color = min_n)) +
    ggplot2::geom_line(linewidth = 1.5, alpha = 0.6) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_wrap(~ .metric, scales = "free", nrow = 2) +
    ggplot2::scale_x_log10(labels = scales::label_number()) +
    ggplot2::scale_color_viridis_d(option = "plasma", begin = .9, end = 0)
  
  best_forest <- forest_res |>
    tune::select_best(metric = best_metric)
  
  final_forest <- forest_wf |>
    tune::finalize_workflow(best_forest) 
  
  final_fit <- final_forest |>
    tune::last_fit(data_split)
  
  final_metrics <- final_fit |>
    tune::collect_metrics()
  
  ref_level <- paste0(".pred_", levels(dataset[[outcome_var]])[1])
  auc_roc_plot <- final_fit |>
      tune::collect_predictions() |>
      yardstick::roc_curve({{ outcome_var }}, {{ ref_level }}) |>
      ggplot2::autoplot()
  
  final_forest <- final_fit |>
    tune::extract_workflow()
  
  vip_plot <- final_fit |>
    tune::extract_fit_parsnip() |>
    vip::vip()
  
  return(
    list(
      forest_res   = forest_res,
      diag_plot    = diag_plot,
      best_forest  = best_forest,
      final_forest = final_forest,
      forest       = tune::extract_fit_engine(final_fit)$forest,
      auc_roc_plot = auc_roc_plot,
      vip_plot     = vip_plot
    )
  )
}

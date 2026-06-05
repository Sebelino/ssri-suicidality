# icf_config.R
# Shared iCF configuration — sourced by all pipeline scripts.
# Changing a value here updates both the monolithic and parallel pipelines.

config <- list(
  # Cross-validation folds
  K = 5,

  # Number of trees per forest
  n_trees = 200,

  # Number of iterations for CV
  n_iterations = 50,

  # Number of iterations for final model
  n_iterations_final = 1000,

  # Tree depths to evaluate
  depths = c(2, 3, 4, 5),

  # P-value threshold for heterogeneity test
  p_threshold = 0.1,

  # Bootstrap resamples for 95% CIs (0 = no CI)
  n_bootstrap = 1000,

  # Number of iterations for depth distribution diagnostics (Step 1)
  n_iterations_diagnostic = 50,

  # Number of seeds for variable-selection stability diagnostic (Step 1).
  # Costs ~n_vi_seeds * raw-CF time. Raw CF on this cohort takes ~5-15 min,
  # so n_vi_seeds=5 adds ~25-75 min to step 1 (well within the 12h budget).
  n_vi_seeds = 5L,

  # Variables used for confounding adjustment only (nuisance models)
  # but excluded from iCF splitting candidates
  adjust_only = c("year")
)

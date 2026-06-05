# 05_cate_comparison.R
# Compare three CATE estimation methods: Causal Forest, Doubly-Robust Learner, T-Learner
#
# All methods use the same data (icf_data.rds) and covariates for a fair comparison.
# Nuisance parameters (propensity scores, outcome regression) are shared across methods.
# All models use grf — no additional package dependencies.
#
# Usage: Rscript 05_cate_comparison.R
# Output: output/cate_comparison.rds, output/vi_*.csv

# Load required packages
required_packages <- c("dplyr", "grf", "here")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing package:", pkg, "\n")
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
}

here::i_am("suicidality/analysis-icf/05_cate_comparison.R")

`%||%` <- function(a, b) if (!is.null(a)) a else b

# =============================================================================
# LOAD DATA
# =============================================================================

cat("\n=== CATE Method Comparison ===\n")

data_path <- here("suicidality", "analysis-icf", "data", "icf_data.rds")
if (!file.exists(data_path)) {
  cat("Data not found. Running data preparation...\n")
  source(here("suicidality", "analysis-icf", "01_prepare_data.R"))
  if (!file.exists(data_path)) {
    stop("Data preparation completed but data file not found at: ", data_path)
  }
}

icf_data <- readRDS(data_path)
cat("Dataset dimensions:", dim(icf_data), "\n")

Y <- icf_data$Y
W <- icf_data$W
X <- as.matrix(icf_data[, !(names(icf_data) %in% c("Y", "W"))])

# Load config to respect adjust_only variables (e.g. calendar year).
# These are included in nuisance models but excluded from CATE estimation,
# matching the iCF pipeline behaviour.
source(here("suicidality", "analysis-icf", "icf_config.R"))
adjust_only <- config$adjust_only %||% character(0)
X_cate <- X[, !(colnames(X) %in% adjust_only), drop = FALSE]

cat("N =", length(Y), "\n")
cat("Covariates (nuisance):", ncol(X), "\n")
cat("Covariates (CATE):", ncol(X_cate),
    if (length(adjust_only) > 0) paste0("  (excluded: ", paste(adjust_only, collapse = ", "), ")") else "",
    "\n")
cat("Treatment:", sum(W), "treated,", sum(1 - W), "control\n")
cat("Outcome events:", sum(Y), "\n")

output_dir <- here("suicidality", "analysis-icf", "output")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Single seed for full reproducibility. The three methods run sequentially,
# so each consumes a different portion of the random stream.
set.seed(43)
start_time <- Sys.time()

# =============================================================================
# SHARED NUISANCE PARAMETERS
# =============================================================================

cat("\nEstimating nuisance parameters...\n")

# Propensity score: E[W | X]
ps_forest <- regression_forest(X, W, num.trees = 500, seed = 43L)
W.hat <- predict(ps_forest)$predictions
cat("  Propensity scores: range [", round(min(W.hat), 4), ",",
    round(max(W.hat), 4), "]\n")

# Outcome model: E[Y | X]
y_forest <- regression_forest(X, Y, num.trees = 500, seed = 43L)
Y.hat <- predict(y_forest)$predictions
cat("  Outcome predictions: range [", round(min(Y.hat), 4), ",",
    round(max(Y.hat), 4), "]\n")

# =============================================================================
# METHOD 1: CAUSAL FOREST
# =============================================================================

cat("\n--- Method 1: Causal Forest ---\n")

# tune.parameters = "none" + explicit seed -- see icf_algorithm.R cf_raw block
# for the rationale (auto-tuning produced seed-dependent VI on this cohort).
cf <- causal_forest(X_cate, Y, W, Y.hat = Y.hat, W.hat = W.hat,
                    num.trees = 2000, honesty = TRUE,
                    tune.parameters = "none", seed = 43L)
tau_cf <- predict(cf)$predictions
vi_cf <- variable_importance(cf)

cat("  ATE:", round(mean(tau_cf), 6), "\n")
cat("  CATE range: [", round(min(tau_cf), 6), ",", round(max(tau_cf), 6), "]\n")

# =============================================================================
# METHOD 2: DOUBLY-ROBUST (AIPW) LEARNER
# =============================================================================

cat("\n--- Method 2: Doubly-Robust Learner ---\n")

# Separate outcome models by treatment arm (use full X for nuisance)
y_forest_1 <- regression_forest(X[W == 1, ], Y[W == 1], num.trees = 500)
y_forest_0 <- regression_forest(X[W == 0, ], Y[W == 0], num.trees = 500)
mu1_hat <- predict(y_forest_1, X)$predictions
mu0_hat <- predict(y_forest_0, X)$predictions

# Clip propensity scores to avoid extreme weights
e_hat <- pmax(pmin(W.hat, 0.99), 0.01)

# DR pseudo-outcome (Kennedy 2023)
gamma <- (mu1_hat - mu0_hat) +
  W * (Y - mu1_hat) / e_hat -
  (1 - W) * (Y - mu0_hat) / (1 - e_hat)

# Regress pseudo-outcome on X_cate to get CATE(x)
dr_forest <- regression_forest(X_cate, gamma, num.trees = 2000, honesty = TRUE)
tau_dr <- predict(dr_forest)$predictions
vi_dr <- variable_importance(dr_forest)

cat("  ATE:", round(mean(tau_dr), 6), "\n")
cat("  CATE range: [", round(min(tau_dr), 6), ",", round(max(tau_dr), 6), "]\n")

# =============================================================================
# METHOD 3: T-LEARNER
# =============================================================================

cat("\n--- Method 3: T-Learner ---\n")

# Separate models per treatment arm (use X_cate so VI reflects CATE drivers only)
model_1 <- regression_forest(X_cate[W == 1, ], Y[W == 1], num.trees = 2000, honesty = TRUE)
model_0 <- regression_forest(X_cate[W == 0, ], Y[W == 0], num.trees = 2000, honesty = TRUE)

# CATE = E[Y|X, W=1] - E[Y|X, W=0]
mu1 <- predict(model_1, X_cate)$predictions
mu0 <- predict(model_0, X_cate)$predictions
tau_tl <- mu1 - mu0

# Variable importance: weighted average of the two arm-specific models
vi_1 <- variable_importance(model_1)
vi_0 <- variable_importance(model_0)
w1 <- sum(W) / length(W)
vi_tl <- w1 * vi_1 + (1 - w1) * vi_0

cat("  ATE:", round(mean(tau_tl), 6), "\n")
cat("  CATE range: [", round(min(tau_tl), 6), ",", round(max(tau_tl), 6), "]\n")

# =============================================================================
# SAVE RESULTS
# =============================================================================

cat("\nSaving results...\n")

results <- list(
  # Individual CATE predictions (vector per method)
  tau_cf = tau_cf,
  tau_dr = tau_dr,
  tau_tl = tau_tl,
  # Variable importance (named vector per method)
  vi_cf = setNames(as.vector(vi_cf), colnames(X_cate)),
  vi_dr = setNames(as.vector(vi_dr), colnames(X_cate)),
  vi_tl = setNames(as.vector(vi_tl), colnames(X_cate)),
  # ATE per method
  ate_cf = mean(tau_cf),
  ate_dr = mean(tau_dr),
  ate_tl = mean(tau_tl),
  # Nuisance parameters (shared)
  W.hat = W.hat,
  Y.hat = Y.hat,
  # Metadata
  n = length(Y),
  n_covariates = ncol(X_cate),
  covariate_names = colnames(X_cate)
)
saveRDS(results, file.path(output_dir, "cate_comparison.rds"))
cat("Saved: output/cate_comparison.rds\n")

# Save variable importance CSVs
save_vi_csv <- function(vi_vec, filename) {
  vi_df <- data.frame(
    variable = names(vi_vec),
    importance = as.numeric(vi_vec),
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(importance))
  write.csv(vi_df, file.path(output_dir, filename), row.names = FALSE)
  cat("Saved:", filename, "\n")
}

save_vi_csv(results$vi_cf, "vi_causal_forest.csv")
save_vi_csv(results$vi_dr, "vi_doubly_robust.csv")
save_vi_csv(results$vi_tl, "vi_t_learner.csv")

# =============================================================================
# SUMMARY
# =============================================================================

end_time <- Sys.time()
cat("\n==============================================\n")
cat("CATE COMPARISON COMPLETE\n")
cat("==============================================\n")
cat("Runtime:", round(difftime(end_time, start_time, units = "mins"), 1), "minutes\n")
cat("N:", results$n, "\n")
cat("Covariates:", results$n_covariates, "\n")
cat("\nATE estimates (risk difference):\n")
cat("  Causal Forest:     ", round(results$ate_cf, 6), "\n")
cat("  Doubly-Robust:     ", round(results$ate_dr, 6), "\n")
cat("  T-Learner:         ", round(results$ate_tl, 6), "\n")
cat("\nCorrelations between CATE estimates:\n")
cat("  CF vs DR:  ", round(cor(tau_cf, tau_dr), 4), "\n")
cat("  CF vs TL:  ", round(cor(tau_cf, tau_tl), 4), "\n")
cat("  DR vs TL:  ", round(cor(tau_dr, tau_tl), 4), "\n")

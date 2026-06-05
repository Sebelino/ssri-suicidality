# run_simple_cf.R
# Simplified Causal Forest Analysis
#
# This script provides a simplified causal forest analysis using just the grf
# package, without the full iCF algorithm. It can be used as:
# 1. A quick first pass to check for heterogeneity
# 2. A fallback if the full iCF package has compatibility issues
#
# The full iCF algorithm (02_run_icf.R) provides more robust subgroup
# identification through iterative voting and cross-validation.

library(tidyverse)
library(grf)
library(here)
here::i_am("suicidality/analysis-icf/run_simple_cf.R")

# =============================================================================
# LOAD DATA
# =============================================================================

cat("=== Simple Causal Forest Analysis ===\n\n")

# Check if prepared data exists, otherwise run preparation
data_path <- here("suicidality", "analysis-icf", "data", "icf_data.rds")
if (!file.exists(data_path)) {
  cat("Running data preparation...\n")
  source(here("suicidality", "analysis-icf", "01_prepare_data.R"))
}

Train <- readRDS(data_path)
cat("Dataset dimensions:", dim(Train), "\n")

# Extract components
Y <- Train$Y
W <- Train$W
X <- Train %>% select(-Y, -W) %>% as.matrix()

cat("Sample size:", length(Y), "\n")
cat("Treatment distribution:", sum(W), "treated,", sum(1-W), "control\n")
cat("Number of covariates:", ncol(X), "\n")

# =============================================================================
# STEP 1: ESTIMATE PROPENSITY SCORES AND EXPECTED OUTCOMES
# =============================================================================

cat("\n=== Step 1: Nuisance Parameter Estimation ===\n")

# Propensity score model
cat("Estimating propensity scores...\n")
ps_forest <- regression_forest(X, W, num.trees = 500)
W.hat <- predict(ps_forest)$predictions

# Expected outcome model
cat("Estimating expected outcomes...\n")
y_forest <- regression_forest(X, Y, num.trees = 500)
Y.hat <- predict(y_forest)$predictions

cat("Propensity score range:", round(min(W.hat), 4), "-", round(max(W.hat), 4), "\n")

# =============================================================================
# STEP 2: CAUSAL FOREST
# =============================================================================

cat("\n=== Step 2: Causal Forest ===\n")

# Grow causal forest
cat("Growing causal forest with 2000 trees...\n")
cf <- causal_forest(
  X = X,
  Y = Y,
  W = W,
  Y.hat = Y.hat,
  W.hat = W.hat,
  num.trees = 2000,
  honesty = TRUE,
  tune.parameters = "all"
)

# Test for heterogeneity
cat("\nTesting for treatment heterogeneity...\n")
calibration <- test_calibration(cf)
print(calibration)

het_p_value <- calibration[2, 4]  # P-value for differential forest prediction
cat("\nHeterogeneity test P-value:", round(het_p_value, 4), "\n")

if (het_p_value > 0.1) {
  cat("Note: Limited evidence of treatment heterogeneity (p > 0.1)\n")
  cat("Subgroup results should be interpreted with caution.\n")
}

# =============================================================================
# STEP 3: AVERAGE TREATMENT EFFECT
# =============================================================================

cat("\n=== Step 3: Average Treatment Effect ===\n")

ate <- average_treatment_effect(cf)
cat("Average Treatment Effect (ATE):\n")
cat("  Estimate:", round(ate[1], 5), "\n")
cat("  SE:", round(ate[2], 5), "\n")
cat("  95% CI: [", round(ate[1] - 1.96*ate[2], 5), ",",
    round(ate[1] + 1.96*ate[2], 5), "]\n")

# =============================================================================
# STEP 4: VARIABLE IMPORTANCE
# =============================================================================

cat("\n=== Step 4: Variable Importance ===\n")

var_imp <- variable_importance(cf)
var_imp_df <- data.frame(
  variable = colnames(X),
  importance = as.vector(var_imp)
) %>%
  arrange(desc(importance))

cat("\nTop 10 important variables for treatment effect heterogeneity:\n")
print(head(var_imp_df, 10))

# Select important variables (above mean importance)
selected_vars <- var_imp_df %>%
  filter(importance > mean(importance)) %>%
  pull(variable)

cat("\nSelected variables (importance > mean):", length(selected_vars), "\n")
cat(paste(selected_vars, collapse = ", "), "\n")

# =============================================================================
# STEP 5: CONDITIONAL AVERAGE TREATMENT EFFECTS
# =============================================================================

cat("\n=== Step 5: Conditional Average Treatment Effects ===\n")

# Get individual treatment effect predictions
tau_hat <- predict(cf)$predictions

cat("CATE distribution:\n")
cat("  Mean:", round(mean(tau_hat), 5), "\n")
cat("  SD:", round(sd(tau_hat), 5), "\n")
cat("  Min:", round(min(tau_hat), 5), "\n")
cat("  Max:", round(max(tau_hat), 5), "\n")

# =============================================================================
# STEP 6: SUBGROUP ANALYSIS BY KEY VARIABLES
# =============================================================================

cat("\n=== Step 6: Subgroup Analysis ===\n")

# Create data frame with predictions
results_df <- data.frame(
  tau_hat = tau_hat,
  Y = Y,
  W = W,
  X
)

# Function to calculate subgroup statistics
calc_subgroup_stats <- function(data, subgroup_name) {
  treat <- data %>% filter(W == 1)
  control <- data %>% filter(W == 0)

  n_treat <- nrow(treat)
  n_control <- nrow(control)
  events_treat <- sum(treat$Y)
  events_control <- sum(control$Y)

  rate_treat <- mean(treat$Y)
  rate_control <- mean(control$Y)
  risk_diff <- rate_treat - rate_control

  mean_tau <- mean(data$tau_hat)

  data.frame(
    subgroup = subgroup_name,
    n_total = n_treat + n_control,
    n_treat = n_treat,
    n_control = n_control,
    events_treat = events_treat,
    events_control = events_control,
    rate_treat = round(rate_treat * 100, 2),
    rate_control = round(rate_control * 100, 2),
    crude_RD = round(risk_diff * 100, 2),
    mean_CATE = round(mean_tau, 5)
  )
}

# Analyze by key binary variables
key_vars <- c("female", "diag_suicidal", "diag_phobic", "diag_anxiety_other",
              "hosp", "fh_suicidal", "diag_adhd", "diag_stress")

subgroup_results <- list()

# Overall
subgroup_results[[1]] <- calc_subgroup_stats(results_df, "Overall")

# By key variables
for (var in key_vars) {
  if (var %in% colnames(results_df)) {
    # Subgroup with variable = 1
    sub1 <- results_df %>% filter(.data[[var]] == 1)
    if (nrow(sub1) > 100) {
      subgroup_results[[length(subgroup_results) + 1]] <-
        calc_subgroup_stats(sub1, paste0(var, " = Yes"))
    }

    # Subgroup with variable = 0
    sub0 <- results_df %>% filter(.data[[var]] == 0)
    if (nrow(sub0) > 100) {
      subgroup_results[[length(subgroup_results) + 1]] <-
        calc_subgroup_stats(sub0, paste0(var, " = No"))
    }
  }
}

# Age groups (age_cat: 0=Children 6-11, 1=Adolescents 12-17, 2=Young adults 18-24)
if ("age_cat" %in% colnames(results_df)) {
  young <- results_df %>% filter(age_cat <= 1)
  adult <- results_df %>% filter(age_cat == 2)

  subgroup_results[[length(subgroup_results) + 1]] <-
    calc_subgroup_stats(young, "Age 6-17 (youth)")
  subgroup_results[[length(subgroup_results) + 1]] <-
    calc_subgroup_stats(adult, "Age 18-24 (young adult)")
}

# Combine results
subgroup_df <- bind_rows(subgroup_results)

cat("\nSubgroup Analysis Results:\n")
print(subgroup_df, row.names = FALSE)

# =============================================================================
# STEP 7: BEST LINEAR PROJECTION
# =============================================================================

cat("\n=== Step 7: Best Linear Projection of CATE ===\n")

# Identify which variables best explain heterogeneity
blp <- best_linear_projection(cf, X[, selected_vars[1:min(10, length(selected_vars))]])
cat("\nBest Linear Projection (selected variables):\n")
print(summary(blp))

# =============================================================================
# SAVE RESULTS
# =============================================================================

cat("\n=== Saving Results ===\n")

output_dir <- here("suicidality", "analysis-icf", "output")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save causal forest object
saveRDS(cf, file.path(output_dir, "simple_cf.rds"))
cat("Saved: simple_cf.rds\n")

# Save variable importance
write.csv(var_imp_df, file.path(output_dir, "simple_cf_var_importance.csv"),
          row.names = FALSE)
cat("Saved: simple_cf_var_importance.csv\n")

# Save subgroup results
write.csv(subgroup_df, file.path(output_dir, "simple_cf_subgroups.csv"),
          row.names = FALSE)
cat("Saved: simple_cf_subgroups.csv\n")

# Save individual predictions
predictions_df <- data.frame(
  tau_hat = tau_hat,
  W = W,
  Y = Y
)
saveRDS(predictions_df, file.path(output_dir, "simple_cf_predictions.rds"))
cat("Saved: simple_cf_predictions.rds\n")

cat("\n=== Analysis Complete ===\n")
cat("\nKey findings:\n")
cat("- ATE:", round(ate[1] * 100, 2), "percentage points\n")
cat("- Heterogeneity test p-value:", round(het_p_value, 4), "\n")
cat("- Top effect modifiers:", paste(head(var_imp_df$variable, 5), collapse = ", "), "\n")

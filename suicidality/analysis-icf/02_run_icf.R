# 02_run_icf.R
# Run iterative Causal Forest (iCF) analysis
#
# This script implements the iCF algorithm to identify subgroups with
# heterogeneous treatment effects (HTEs) for SSRI effects on suicidal behavior.
#
# Reference: Wang et al. (2024) Am J Epidemiol. 193(5):764-776

# Load required packages
required_packages <- c("dplyr", "grf", "here")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing package:", pkg, "\n")
    tryCatch({
      install.packages(pkg, repos = "https://cloud.r-project.org")
    }, error = function(e) {
      stop("Failed to install package '", pkg, "': ", e$message)
    })
    # Verify installation succeeded
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' installation appeared to succeed but package not found")
    }
  }
  library(pkg, character.only = TRUE)
}

here::i_am("suicidality/analysis-icf/02_run_icf.R")

# Source the iCF implementation
source(here("suicidality", "analysis-icf", "icf", "icf_algorithm.R"))

# =============================================================================
# CONFIGURATION
# =============================================================================

source(here("suicidality", "analysis-icf", "icf_config.R"))

# =============================================================================
# LOAD DATA
# =============================================================================

cat("\n=== Loading Data ===\n")

data_path <- here("suicidality", "analysis-icf", "data", "icf_data.rds")
if (!file.exists(data_path)) {
  cat("Data not found. Running data preparation...\n")
  tryCatch({
    source(here("suicidality", "analysis-icf", "01_prepare_data.R"))
  }, error = function(e) {
    stop("Data preparation failed: ", e$message)
  })
  if (!file.exists(data_path)) {
    stop("Data preparation completed but data file not found at: ", data_path)
  }
}

icf_data <- readRDS(data_path)
cat("Dataset dimensions:", dim(icf_data), "\n")
cat("Treatment distribution:", sum(icf_data$W), "treated,",
    sum(1 - icf_data$W), "control\n")
cat("Outcome events:", sum(icf_data$Y), "\n")

# =============================================================================
# RUN iCF
# =============================================================================

cat("\n=== Running iCF Analysis ===\n")
cat("Configuration:\n")
cat("  K-fold CV:", config$K, "\n")
cat("  Trees per forest:", config$n_trees, "\n")
cat("  Iterations per depth:", config$n_iterations, "\n")
cat("  Depths to evaluate:", paste(config$depths, collapse = ", "), "\n")

start_time <- Sys.time()

icf_results <- run_icf_cv(
  data = icf_data,
  K = config$K,
  n_trees = config$n_trees,
  n_iterations = config$n_iterations,
  n_iterations_final = config$n_iterations_final,
  depths = config$depths,
  p_threshold = config$p_threshold,
  n_bootstrap = config$n_bootstrap,
  adjust_only = config$adjust_only,
  verbose = TRUE
)

end_time <- Sys.time()
cat("\nTotal runtime:", round(difftime(end_time, start_time, units = "mins"), 1),
    "minutes\n")

# =============================================================================
# SAVE RESULTS
# =============================================================================

cat("\n=== Saving Results ===\n")

output_dir <- here("suicidality", "analysis-icf", "output")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Save main results (without large objects)
results_to_save <- icf_results
results_to_save$cate_individual <- predict(icf_results$cf_raw)$predictions
results_to_save$cf_raw <- NULL  # Remove large object
saveRDS(results_to_save, file.path(output_dir, "icf_results.rds"))
cat("Saved: icf_results.rds\n")

# Save CATE summary
write.csv(icf_results$cate, file.path(output_dir, "cate_summary.csv"),
          row.names = FALSE)
cat("Saved: cate_summary.csv\n")

# Save variable importance
var_imp_df <- data.frame(
  variable = names(icf_results$var_importance),
  importance = as.vector(icf_results$var_importance)
) %>%
  arrange(desc(importance))
write.csv(var_imp_df, file.path(output_dir, "variable_importance.csv"),
          row.names = FALSE)
cat("Saved: variable_importance.csv\n")

# Save configuration
saveRDS(config, file.path(output_dir, "config.rds"))
cat("Saved: config.rds\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n=== Summary ===\n")
cat("Heterogeneity test p-value:", round(icf_results$het_p_value, 4), "\n")
cat("Best depth:", icf_results$best_depth, "\n")
cat("Number of subgroups:", icf_results$n_subgroups, "\n")
cat("\nSubgroup labels:\n")
for (i in seq_along(icf_results$subgroup_labels)) {
  cat("  ", i, ":", icf_results$subgroup_labels[i], "\n")
}
cat("\niCF CATE by subgroup:\n")
print(icf_results$cate)

cat("\n=== Analysis Complete ===\n")
cat("Run 03_visualize_results.R to create visualizations.\n")

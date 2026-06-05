# 02c_icf_step3.R
# Parallel iCF pipeline — Step 3: Best depth selection + final model
#
# Loads Step 1 output and all Step 2 CV results, selects the best depth
# via cross-validation, fits the final iCF model on the full data,
# and saves results in the same format as 02_run_icf.R.
#
# Usage: Rscript 02c_icf_step3.R
# Input:  output/icf_step1.rds, output/icf_cv_fold*_depth*.rds
# Output: output/icf_results.rds, output/cate_summary.csv,
#         output/variable_importance.csv, output/config.rds

# Load required packages
required_packages <- c("dplyr", "grf", "here")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
}

here::i_am("suicidality/analysis-icf/02c_icf_step3.R")

# Source the iCF implementation
source(here("suicidality", "analysis-icf", "icf", "icf_algorithm.R"))

# =============================================================================
# LOAD STEP 1 OUTPUT
# =============================================================================

cat("\n=== Step 3: Best Depth Selection + Final Model ===\n")

output_dir <- here("suicidality", "analysis-icf", "output")
step1_path <- file.path(output_dir, "icf_step1.rds")

if (!file.exists(step1_path)) {
  stop("Step 1 output not found: ", step1_path)
}

step1 <- readRDS(step1_path)
config <- step1$config

start_time <- Sys.time()

# =============================================================================
# RECONSTRUCT CV MSE MATRIX
# =============================================================================

cat("\nLoading CV results...\n")

K      <- config$K
depths <- config$depths

cv_mse <- matrix(NA, nrow = K, ncol = length(depths))
colnames(cv_mse) <- paste0("D", depths)

n_missing <- 0

for (d_idx in seq_along(depths)) {
  depth <- depths[d_idx]
  for (fold in 1:K) {
    cv_file <- file.path(output_dir,
                         sprintf("icf_cv_fold%d_depth%d.rds", fold, depth))
    if (file.exists(cv_file)) {
      result <- readRDS(cv_file)
      cv_mse[fold, d_idx] <- result$cv_mse
    } else {
      warning(sprintf("Missing CV file: fold=%d, depth=%d. Setting MSE=Inf.", fold, depth))
      cv_mse[fold, d_idx] <- Inf
      n_missing <- n_missing + 1
    }
  }
}

if (n_missing > 0) {
  cat(sprintf("WARNING: %d of %d CV files missing (MSE set to Inf)\n",
              n_missing, K * length(depths)))
}

# =============================================================================
# SELECT BEST DEPTH + FINAL MODEL
# =============================================================================

n_bootstrap <- config$n_bootstrap %||% 0
n_iterations_final <- config$n_iterations_final %||% config$n_iterations
results <- icf_select_and_finalize(step1, cv_mse, depths,
                                   n_trees = config$n_trees,
                                   n_iterations = n_iterations_final,
                                   n_bootstrap = n_bootstrap,
                                   verbose = TRUE)

# =============================================================================
# SAVE
# =============================================================================

cat("\n=== Saving Results ===\n")

saveRDS(results, file.path(output_dir, "icf_results.rds"))
cat("Saved: icf_results.rds\n")

# Save CATE summary
write.csv(results$cate, file.path(output_dir, "cate_summary.csv"),
          row.names = FALSE)
cat("Saved: cate_summary.csv\n")

# Save variable importance
var_imp_df <- data.frame(
  variable = names(results$var_importance),
  importance = as.vector(results$var_importance)
) %>%
  arrange(desc(importance))
write.csv(var_imp_df, file.path(output_dir, "variable_importance.csv"),
          row.names = FALSE)
cat("Saved: variable_importance.csv\n")

# Save configuration
saveRDS(config, file.path(output_dir, "config.rds"))
cat("Saved: config.rds\n")

# =============================================================================
# CLEAN UP INTERMEDIATE FILES
# =============================================================================

cat("\nCleaning up intermediate files...\n")

# Remove step1 output
step1_file <- file.path(output_dir, "icf_step1.rds")
if (file.exists(step1_file)) {
  file.remove(step1_file)
  cat("  Removed: icf_step1.rds\n")
}

# Remove per-fold CV files
for (d_idx in seq_along(depths)) {
  depth <- depths[d_idx]
  for (fold in 1:K) {
    cv_file <- file.path(output_dir,
                         sprintf("icf_cv_fold%d_depth%d.rds", fold, depth))
    if (file.exists(cv_file)) {
      file.remove(cv_file)
    }
  }
}
cat("  Removed: icf_cv_fold*_depth*.rds\n")

# =============================================================================
# SUMMARY
# =============================================================================

end_time <- Sys.time()

cat("\n=== Summary ===\n")
cat("Heterogeneity test p-value:", round(results$het_p_value, 4), "\n")
cat("Best depth:", results$best_depth, "\n")
cat("Number of subgroups:", results$n_subgroups, "\n")
cat("\nSubgroup labels:\n")
for (i in seq_along(results$subgroup_labels)) {
  cat("  ", i, ":", results$subgroup_labels[i], "\n")
}
cat("\niCF CATE by subgroup:\n")
print(results$cate)

cat("\nStep 3 complete. Runtime:", round(difftime(end_time, start_time, units = "mins"), 1),
    "minutes\n")
cat("\n=== Analysis Complete ===\n")
cat("Run 03_visualize_results.R to create visualizations.\n")

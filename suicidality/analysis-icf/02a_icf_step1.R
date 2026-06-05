# 02a_icf_step1.R
# Parallel iCF pipeline — Step 1: Raw causal forest + variable selection
#
# Estimates nuisance parameters, runs a raw causal forest, tests for
# heterogeneity, performs variable selection, and generates CV fold IDs.
# Outputs are consumed by Step 2 (per-fold CV) and Step 3 (final model).
#
# Usage: Rscript 02a_icf_step1.R
# Output: output/icf_step1.rds

# Load required packages
required_packages <- c("dplyr", "grf", "here")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing package:", pkg, "\n")
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
}

here::i_am("suicidality/analysis-icf/02a_icf_step1.R")

source(here("suicidality", "analysis-icf", "paths.R"))

# Source the iCF implementation
source(here("suicidality", "analysis-icf", "icf", "icf_algorithm.R"))

# =============================================================================
# CONFIGURATION (shared across all pipeline steps)
# =============================================================================

source(here("suicidality", "analysis-icf", "icf_config.R"))

# =============================================================================
# LOAD DATA
# =============================================================================

cat(sprintf("\n=== Step 1: Raw Causal Forest + Variable Selection (variant: %s) ===\n",
            variant_label()))

data_path <- icf_data_path("analysis-icf")
if (!file.exists(data_path)) {
  cat("Data not found. Running data preparation...\n")
  source(here("suicidality", "analysis-icf", "01_prepare_data.R"))
  if (!file.exists(data_path)) {
    stop("Data preparation completed but data file not found at: ", data_path)
  }
}

icf_data <- readRDS(data_path)
cat("Dataset dimensions:", dim(icf_data), "\n")
cat("Treatment distribution:", sum(icf_data$W), "treated,",
    sum(1 - icf_data$W), "control\n")
cat("Outcome events:", sum(icf_data$Y), "\n")

start_time <- Sys.time()

# =============================================================================
# RUN VARIABLE SELECTION
# =============================================================================

step1 <- icf_variable_selection(icf_data, K = config$K,
                                p_threshold = config$p_threshold,
                                adjust_only = config$adjust_only, verbose = TRUE)

# =============================================================================
# DEPTH DISTRIBUTION DIAGNOSTICS (Wang et al. 2024 Step 2)
# =============================================================================

library(ggplot2)

depth_diag <- diagnose_depth_distribution(
  X = step1$X, Y = step1$Y, W = step1$W,
  Y.hat = step1$Y.hat, W.hat = step1$W.hat,
  selected_vars = step1$selected_vars,
  depths = config$depths,
  n_trees = config$n_trees,
  n_iterations = config$n_iterations_diagnostic %||% 50
)

output_dir <- here("suicidality", "analysis-icf", "output")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Plot bar charts
for (dname in names(depth_diag)) {
  dd <- depth_diag[[dname]]
  depth_df <- data.frame(depth = factor(dd$actual_depths))
  p <- ggplot(depth_df, aes(x = depth)) +
    geom_bar(fill = "steelblue", width = 0.7) +
    labs(
      title = sprintf("Depth Distribution for Target %s", dname),
      subtitle = sprintf("denominator = %d, min_leaf_size = %d, N = %d",
                         dd$denominator, dd$min_leaf_size, nrow(step1$X)),
      x = "Actual depth of best tree",
      y = "Count"
    ) +
    theme_bw(base_size = 13) +
    theme(plot.title = element_text(face = "bold"))
  fig_name <- sprintf("depth_distribution_%s%s.pdf", dname, variant_suffix())
  ggsave(file.path(output_dir, fig_name), p, width = 6, height = 4)
  cat(sprintf("Saved: %s\n", fig_name))
}

# =============================================================================
# VARIABLE-SELECTION STABILITY (diagnostic #7)
# =============================================================================

n_vi_seeds <- config$n_vi_seeds %||% 5L
cat(sprintf("\nVariable-selection stability across %d seeds (~%dx raw-CF time)...\n",
            n_vi_seeds, n_vi_seeds))
vi_stab <- vi_stability(
  X = step1$X, Y = step1$Y, W = step1$W,
  Y.hat = step1$Y.hat, W.hat = step1$W.hat,
  n_seeds = n_vi_seeds, num.trees = 2000,
  select_top_pct = NULL, verbose = TRUE
)
cat(sprintf("Mean off-diagonal Jaccard across selection sets: %.2f\n",
            vi_stab$mean_offdiag_jaccard))
cat("Variables selected in all", n_vi_seeds, "seeds:",
    paste(vi_stab$per_variable$variable[vi_stab$per_variable$selection_freq == 1],
          collapse = ", "), "\n")

# =============================================================================
# SAVE (exclude cf_raw to reduce file size)
# =============================================================================

# Save individual CATE predictions from raw CF before discarding it
step1$cate_individual <- predict(step1$cf_raw)$predictions
step1$cf_raw <- NULL
step1$config <- config
step1$depth_diagnostics <- depth_diag
step1$vi_stability <- vi_stab

step1_out <- icf_step1_path("analysis-icf")
saveRDS(step1, step1_out)

end_time <- Sys.time()
cat("\nStep 1 complete. Runtime:", round(difftime(end_time, start_time, units = "mins"), 1),
    "minutes\n")
cat("Saved:", step1_out, "\n")

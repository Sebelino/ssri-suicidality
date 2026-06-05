# 02c1_icf_step3_batch.R
# Parallel iCF pipeline — Step 3a: Batch forest growing
#
# SLURM array task. Each invocation grows a batch of forests at the
# best depth selected by cross-validation. Results are merged by
# 02c2_icf_step3_merge.R.
#
# Usage: Rscript 02c1_icf_step3_batch.R <BATCH_ID> <N_BATCHES>
# Input:  output/icf_step1.rds, output/icf_cv_fold*_depth*.rds
# Output: output/icf_batch_<BATCH_ID>.rds

# Load required packages
required_packages <- c("dplyr", "grf", "here")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
}

here::i_am("suicidality/analysis-icf/02c1_icf_step3_batch.R")

# Source the iCF implementation
source(here("suicidality", "analysis-icf", "icf", "icf_algorithm.R"))

# =============================================================================
# PARSE ARGUMENTS
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  stop("Usage: Rscript 02c1_icf_step3_batch.R <BATCH_ID> <N_BATCHES>")
}

batch_id  <- as.integer(args[1])
n_batches <- as.integer(args[2])

cat(sprintf("\n=== Step 3a: Batch %d of %d ===\n", batch_id, n_batches))

# =============================================================================
# LOAD STEP 1 OUTPUT
# =============================================================================

output_dir <- here("suicidality", "analysis-icf", "output")
step1_path <- file.path(output_dir, "icf_step1.rds")

if (!file.exists(step1_path)) {
  stop("Step 1 output not found: ", step1_path)
}

step1  <- readRDS(step1_path)
config <- step1$config

# Validate batch arguments
if (batch_id < 1 || batch_id > n_batches) {
  stop(sprintf("BATCH_ID must be in 1:%d, got %d", n_batches, batch_id))
}

start_time <- Sys.time()

# =============================================================================
# SELECT BEST DEPTH (same logic as 02c_icf_step3.R)
# =============================================================================

cat("\nSelecting best depth from CV results...\n")

K      <- config$K
depths <- config$depths

cv_mse <- matrix(NA, nrow = K, ncol = length(depths))
colnames(cv_mse) <- paste0("D", depths)

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
    }
  }
}

mean_cv_mse <- colMeans(cv_mse, na.rm = TRUE)

if (all(is.infinite(mean_cv_mse))) {
  warning("All depths have Inf MSE. Using smallest depth as fallback.")
  best_depth_idx <- 1
} else {
  # Select depth with minimum mean CV MSE (Wang et al. 2024)
  best_depth_idx <- which.min(mean_cv_mse)
}

best_depth <- depths[best_depth_idx]
cat("Selected depth:", best_depth, "\n")

# =============================================================================
# COMPUTE ITERATION RANGE
# =============================================================================

n_iterations_final <- config$n_iterations_final %||% config$n_iterations
batch_size <- ceiling(n_iterations_final / n_batches)
iter_start <- (batch_id - 1) * batch_size + 1
iter_end   <- min(batch_id * batch_size, n_iterations_final)
iterations <- iter_start:iter_end

cat(sprintf("Iterations %d-%d (batch size %d, total %d)\n",
            iter_start, iter_end, length(iterations), n_iterations_final))

# =============================================================================
# GROW FORESTS
# =============================================================================

cat("\nGrowing forests...\n")

iter_result <- run_icf_iterations(
  X = step1$X,
  Y = step1$Y,
  W = step1$W,
  Y.hat = step1$Y.hat,
  W.hat = step1$W.hat,
  selected_vars = step1$selected_vars,
  target_depth = best_depth,
  n_trees = config$n_trees,
  iterations = iterations,
  seed_offset = 0L
)

# =============================================================================
# SAVE
# =============================================================================

batch_result <- list(
  batch_id = batch_id,
  n_batches = n_batches,
  best_depth = best_depth,
  iterations = iterations,
  best_trees = iter_result$best_trees,
  tree_structures = iter_result$tree_structures,
  tree_depths = iter_result$tree_depths,
  var_names = iter_result$var_names
)

out_file <- file.path(output_dir, sprintf("icf_batch_%02d.rds", batch_id))
saveRDS(batch_result, out_file)

end_time <- Sys.time()
cat(sprintf("\nBatch %d complete. Runtime: %.1f minutes\n",
            batch_id, as.numeric(difftime(end_time, start_time, units = "mins"))))
cat("Saved:", basename(out_file), "\n")

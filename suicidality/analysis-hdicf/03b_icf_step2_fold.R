# 03b_icf_step2_fold.R
# hdiCF pipeline — Step 2: Single CV fold x depth
#
# Runs one iteration of the cross-validation: trains iCF at a given depth
# on training fold, evaluates test MSE on held-out fold.
#
# Usage: Rscript 03b_icf_step2_fold.R <fold> <depth>
# Input:  output/icf_step1.rds
# Output: output/icf_cv_fold{fold}_depth{depth}.rds

# Load required packages
required_packages <- c("dplyr", "grf", "here")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
}

here::i_am("suicidality/analysis-hdicf/03b_icf_step2_fold.R")

# Source the iCF implementation (shared with analysis-icf)
source(here("suicidality", "analysis-icf", "icf", "icf_algorithm.R"))

# =============================================================================
# PARSE ARGUMENTS
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 2) {
  stop("Usage: Rscript 03b_icf_step2_fold.R <fold> <depth>")
}

fold <- as.integer(args[1])
depth <- as.integer(args[2])

cat(sprintf("\n=== Step 2: CV fold=%d, depth=%d ===\n", fold, depth))

# =============================================================================
# LOAD STEP 1 OUTPUT
# =============================================================================

output_dir <- here("suicidality", "analysis-hdicf", "output")
step1_path <- file.path(output_dir, "icf_step1.rds")

if (!file.exists(step1_path)) {
  stop("Step 1 output not found: ", step1_path)
}

step1 <- readRDS(step1_path)
config <- step1$config

# Validate arguments
if (fold < 1 || fold > config$K) {
  stop(sprintf("fold must be in 1:%d, got %d", config$K, fold))
}
if (!(depth %in% config$depths)) {
  stop(sprintf("depth must be one of %s, got %d",
               paste(config$depths, collapse = ","), depth))
}

start_time <- Sys.time()

# =============================================================================
# RUN SINGLE CV FOLD
# =============================================================================

cv_mse <- icf_cv_fold(step1, fold, depth,
                      n_trees = config$n_trees,
                      n_iterations = config$n_iterations)

# =============================================================================
# SAVE
# =============================================================================

result <- list(
  fold = fold,
  depth = depth,
  cv_mse = cv_mse
)

out_file <- file.path(output_dir,
                      sprintf("icf_cv_fold%d_depth%d.rds", fold, depth))
saveRDS(result, out_file)

end_time <- Sys.time()
cat(sprintf("  MSE = %.6f\n", cv_mse))
cat("  Runtime:", round(difftime(end_time, start_time, units = "mins"), 1),
    "minutes\n")
cat("  Saved:", basename(out_file), "\n")

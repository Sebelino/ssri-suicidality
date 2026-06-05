# 03c2_icf_step3_merge.R
# hdiCF pipeline — Step 3b: Merge batches + finalize
#
# Loads all batch outputs from 03c1_icf_step3_batch.R, concatenates
# results, performs plurality voting, builds synthetic tree, and
# calculates subgroup CATEs.
#
# Usage: Rscript 03c2_icf_step3_merge.R
# Input:  output/icf_step1.rds, output/icf_batch_*.rds
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

here::i_am("suicidality/analysis-hdicf/03c2_icf_step3_merge.R")

# Source the iCF implementation (shared with analysis-icf)
source(here("suicidality", "analysis-icf", "icf", "icf_algorithm.R"))

# =============================================================================
# LOAD STEP 1 OUTPUT
# =============================================================================

cat("\n=== Step 3b: Merge Batches + Finalize ===\n")

output_dir <- here("suicidality", "analysis-hdicf", "output")
step1_path <- file.path(output_dir, "icf_step1.rds")

if (!file.exists(step1_path)) {
  stop("Step 1 output not found: ", step1_path)
}

step1  <- readRDS(step1_path)
config <- step1$config

start_time <- Sys.time()

# =============================================================================
# LOAD AND MERGE BATCH FILES
# =============================================================================

cat("\nLoading batch files...\n")

batch_files <- sort(list.files(output_dir, pattern = "^icf_batch_\\d+\\.rds$",
                               full.names = TRUE))

if (length(batch_files) == 0) {
  stop("No batch files found in: ", output_dir)
}

cat(sprintf("Found %d batch files\n", length(batch_files)))

# Load all batches
batches <- lapply(batch_files, readRDS)

# Verify all expected batches are present
expected_n <- batches[[1]]$n_batches
if (!is.null(expected_n)) {
  found_ids <- sort(sapply(batches, function(b) b$batch_id))
  missing_ids <- setdiff(seq_len(expected_n), found_ids)
  if (length(missing_ids) > 0) {
    stop(sprintf("Missing %d of %d expected batch files (IDs: %s). Check SLURM logs.",
                 length(missing_ids), expected_n,
                 paste(missing_ids, collapse = ", ")))
  }
  cat(sprintf("All %d expected batches present.\n", expected_n))
}

# Verify all batches selected the same best_depth
batch_depths <- sapply(batches, function(b) b$best_depth)
if (length(unique(batch_depths)) != 1) {
  stop(sprintf("Inconsistent best_depth across batches: %s",
               paste(unique(batch_depths), collapse = ", ")))
}
best_depth <- batch_depths[1]
cat("Best depth (all batches agree):", best_depth, "\n")

# Concatenate results
all_best_trees      <- do.call(c, lapply(batches, function(b) b$best_trees))
all_tree_structures <- do.call(c, lapply(batches, function(b) b$tree_structures))
all_tree_depths     <- do.call(c, lapply(batches, function(b) b$tree_depths))
var_names           <- batches[[1]]$var_names

n_total_iterations <- length(all_tree_structures)
cat(sprintf("Total iterations merged: %d\n", n_total_iterations))

# =============================================================================
# RECONSTRUCT CV MSE (for results object)
# =============================================================================

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
      cv_mse[fold, d_idx] <- Inf
    }
  }
}

mean_cv_mse <- colMeans(cv_mse, na.rm = TRUE)

# =============================================================================
# FINALIZE: PLURALITY VOTE + SUBGROUP ASSIGNMENT
# =============================================================================

cat("\nFinalizing: plurality vote + subgroup assignment...\n")

X_selected <- step1$X[, step1$selected_vars, drop = FALSE]

final_result <- finalize_icf_voted_tree(
  best_trees = all_best_trees,
  tree_structures = all_tree_structures,
  tree_depths = all_tree_depths,
  X_selected = X_selected,
  var_names = var_names,
  target_depth = best_depth
)

if (!final_result$success) {
  stop("finalize_icf_voted_tree() failed: ", final_result$message)
}

# =============================================================================
# CALCULATE SUBGROUP CATES
# =============================================================================

cat("\nCalculating subgroup CATEs...\n")

n_bootstrap <- config$n_bootstrap %||% 0

cate_results <- calculate_subgroup_cate(
  Y = step1$Y,
  W = step1$W,
  X = step1$X,
  subgroup_id = final_result$subgroup_id,
  subgroup_labels = final_result$subgroup_labels,
  W.hat = step1$W.hat,
  n_bootstrap = n_bootstrap
)

# Diagnostic #4: assert label/partition alignment (catches the 2026-05-10 bug)
label_check <- verify_subgroup_labels(
  X = step1$X[, step1$selected_vars, drop = FALSE],
  subgroup_id = final_result$subgroup_id,
  subgroup_labels = final_result$subgroup_labels,
  var_names = colnames(step1$X)[step1$selected_vars]
)
if (!isTRUE(label_check$overall_ok)) {
  cat("\nLabel-partition mismatch detected:\n")
  for (r in label_check$per_leaf) {
    cat(sprintf("  %s : expected n=%d, predicate n=%s [%s]\n",
                r$label, r$n_expected,
                ifelse(is.na(r$n_predicate), "NA", as.character(r$n_predicate)),
                r$note))
  }
  stop("verify_subgroup_labels() failed: subgroup_labels do not match the partition implied by subgroup_id. ",
       "This indicates a regression of the 2026-05-10 fix in calculate_subgroup_cate / assign_subgroups.")
} else {
  cat("Label-partition invariant check: PASS (", length(label_check$per_leaf), "leaves)\n")
}

# =============================================================================
# DECISION TREES AT ALL CANDIDATE DEPTHS
# =============================================================================

cat("\nGenerating decision trees for all candidate depths...\n")
all_depth_results <- list()

for (depth in config$depths) {
  dname <- paste0("D", depth)
  if (depth == best_depth) {
    cat(sprintf("  %s (best depth): already computed.\n", dname))
    all_depth_results[[dname]] <- list(
      final_result = final_result,
      cate = cate_results
    )
    next
  }
  cat(sprintf("  %s: growing %d forests...\n", dname, config$n_iterations))
  depth_result <- run_icf_at_depth(
    X = step1$X, Y = step1$Y, W = step1$W,
    Y.hat = step1$Y.hat, W.hat = step1$W.hat,
    selected_vars = step1$selected_vars,
    target_depth = depth,
    n_trees = config$n_trees,
    n_iterations = config$n_iterations,
    seed_offset = depth * 100000L + 50000L
  )
  if (depth_result$success) {
    depth_cate <- calculate_subgroup_cate(
      Y = step1$Y, W = step1$W, X = step1$X,
      subgroup_id = depth_result$subgroup_id,
      subgroup_labels = depth_result$subgroup_labels,
      W.hat = step1$W.hat,
      n_bootstrap = n_bootstrap
    )
    all_depth_results[[dname]] <- list(
      final_result = depth_result,
      cate = depth_cate
    )
    cat(sprintf("  %s: %d subgroups identified.\n", dname, depth_result$n_subgroups))
  } else {
    cat(sprintf("  %s: finalization failed.\n", dname))
    all_depth_results[[dname]] <- NULL
  }
}

# =============================================================================
# COMPILE RESULTS (same structure as icf_select_and_finalize)
# =============================================================================

results <- list(
  het_p_value = step1$het_p,
  selected_vars = step1$selected_vars,
  var_names = colnames(step1$X)[step1$selected_vars],
  var_importance = step1$var_imp,
  cv_mse = cv_mse,
  mean_cv_mse = mean_cv_mse,
  best_depth = best_depth,
  final_result = final_result,
  subgroup_id = final_result$subgroup_id,
  subgroup_labels = final_result$subgroup_labels,
  n_subgroups = final_result$n_subgroups,
  cate = cate_results,
  cf_raw = NULL,
  cate_individual = step1$cate_individual,
  W.hat = step1$W.hat,
  Y.hat = step1$Y.hat,
  all_depth_results = all_depth_results,
  # Preserve step1 fields needed for re-analysis without re-running step 1
  # after intermediate-file cleanup. cf_raw is intentionally
  # excluded (it's already null and was the largest object in step1).
  X = step1$X,
  Y = step1$Y,
  W = step1$W,
  depth_diagnostics = step1$depth_diagnostics
)

# =============================================================================
# SAVE
# =============================================================================

cat("\n=== Saving Results ===\n")

saveRDS(results, file.path(output_dir, "icf_results.rds"))
cat("Saved: icf_results.rds\n")

write.csv(results$cate, file.path(output_dir, "cate_summary.csv"),
          row.names = FALSE)
cat("Saved: cate_summary.csv\n")

var_imp_df <- data.frame(
  variable = names(results$var_importance),
  importance = as.vector(results$var_importance)
) %>%
  arrange(desc(importance))
write.csv(var_imp_df, file.path(output_dir, "variable_importance.csv"),
          row.names = FALSE)
cat("Saved: variable_importance.csv\n")

saveRDS(config, file.path(output_dir, "config.rds"))
cat("Saved: config.rds\n")

# Verify the main results file is readable before deleting intermediates
results_path <- file.path(output_dir, "icf_results.rds")
tryCatch({
  check <- readRDS(results_path)
  stopifnot(is.list(check), !is.null(check$cate))
  cat("Verified: icf_results.rds is readable\n")
}, error = function(e) {
  stop("icf_results.rds failed verification — keeping intermediate files. Error: ", e$message)
})

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

# Remove batch files
for (bf in batch_files) {
  file.remove(bf)
}
cat(sprintf("  Removed: %d batch files\n", length(batch_files)))

# =============================================================================
# SUMMARY
# =============================================================================

end_time <- Sys.time()

cat("\n=== Summary ===\n")
cat("Heterogeneity test p-value:", round(results$het_p_value, 4), "\n")
cat("Best depth:", results$best_depth, "\n")
cat("Number of subgroups:", results$n_subgroups, "\n")
cat("Total iterations:", n_total_iterations, "\n")
cat("\nSubgroup labels:\n")
for (i in seq_along(results$subgroup_labels)) {
  cat("  ", i, ":", results$subgroup_labels[i], "\n")
}
cat("\nhdiCF CATE by subgroup:\n")
print(results$cate)

# ---- Diagnostic #2: CV-MSE flatness ----
mse_range <- max(mean_cv_mse) - min(mean_cv_mse)
mse_relrange <- mse_range / mean(mean_cv_mse)
fold_sd <- apply(cv_mse, 2, sd, na.rm = TRUE)
cat("\n=== CV-MSE flatness diagnostic (#2) ===\n")
cat(sprintf("Mean CV-MSE per depth: %s\n",
            paste0(names(mean_cv_mse), "=", sprintf("%.6f", mean_cv_mse), collapse = "  ")))
cat(sprintf("Per-fold SD per depth: %s\n",
            paste0(names(fold_sd), "=", sprintf("%.6f", fold_sd), collapse = "  ")))
cat(sprintf("Range across depths : %.2e (relative: %.2e)\n", mse_range, mse_relrange))
cat(sprintf("Mean fold-SD / range: %.1fx (>1 means within-fold noise dwarfs across-depth differences)\n",
            mean(fold_sd) / max(mse_range, .Machine$double.eps)))
if (mse_relrange < 1e-3) {
  cat("FLAG: depth selection is effectively a coin flip — CV-MSE landscape is flat at <0.1% relative.\n")
}

# ---- 1-SE rule (Breiman 1984) for headline depth selection ----
one_se <- select_depth_1se(cv_mse, depths = config$depths)
cat(sprintf("\nCV-argmin depth: D%d (Wang 2024 default)\n", one_se$cv_min_depth))
cat(sprintf("1-SE rule:       SE of CV-min = %.2e; threshold = min + SE = %.6f\n",
            one_se$se, one_se$threshold))
cat(sprintf("                 1-SE plateau = {%s}\n",
            paste0("D", one_se$plateau, collapse = ", ")))
if (one_se$is_flat) {
  cat(sprintf("                 1-SE rule fires (|plateau| > 1). Reported headline depth: D%d (shallowest in plateau).\n",
              one_se$one_se_depth))
} else {
  cat(sprintf("                 CV argmin is uniquely identified at 1 SE. Reported headline depth: D%d.\n",
              one_se$one_se_depth))
}

# ---- Diagnostic #1: per-depth voted-tree stability ----
cat("\n=== Stability diagnostic (#1) ===\n")
for (depth in config$depths) {
  dname <- paste0("D", depth)
  ar <- all_depth_results[[dname]]
  if (is.null(ar) || is.null(ar$final_result)) next
  fr <- ar$final_result
  cat(sprintf("\n%s (target depth %d, mean CV-MSE %.6f, fold SD %.6f):\n",
              dname, depth, mean_cv_mse[dname], fold_sd[dname]))
  cat(sprintf("  voted structure: %s   (vote freq = %.3f)\n",
              fr$voted_structure, fr$vote_frequency))
  if (!is.null(fr$vote_distribution) && nrow(fr$vote_distribution) > 0) {
    top <- head(fr$vote_distribution, 3)
    cat("  top 3 voted structures:\n")
    for (k in seq_len(nrow(top))) {
      cat(sprintf("    [%d] %.3f  %s\n", top$count[k], top$frequency[k], top$structure[k]))
    }
  }
  if (!is.null(fr$root_split_distribution) && nrow(fr$root_split_distribution) > 0) {
    rsd <- head(fr$root_split_distribution, 5)
    cat("  root-split variable distribution: ",
        paste0(rsd$variable, "=", sprintf("%.2f", rsd$frequency), collapse = ", "),
        "\n", sep = "")
  }
}

# Persist diagnostic outputs in the results object so downstream scripts and
# DESCRIPTION.md can reference them without rerunning the pipeline.
results$diagnostics <- list(
  cv_mse_range = mse_range,
  cv_mse_relrange = mse_relrange,
  cv_mse_fold_sd = fold_sd,
  vi_stability = step1$vi_stability,
  label_check = label_check,
  one_se = one_se
)
# Headline reporting uses the 1-SE rule result when CV-MSE is flat.
# The CV-argmin remains in $best_depth for transparency / Wang 2024 fidelity.
results$reported_depth <- one_se$one_se_depth
saveRDS(results, results_path)
cat("\nUpdated icf_results.rds with $diagnostics block and $reported_depth.\n")

# ---- Diagnostic #7: variable-selection stability (already computed in step1) ----
if (!is.null(step1$vi_stability)) {
  vs <- step1$vi_stability
  cat("\n=== Variable-selection stability diagnostic (#7) ===\n")
  cat(sprintf("Across %d seeds, mean off-diagonal Jaccard of selection sets: %.2f\n",
              vs$n_seeds, vs$mean_offdiag_jaccard))
  always <- vs$per_variable$variable[vs$per_variable$selection_freq == 1]
  ever   <- vs$per_variable$variable[vs$per_variable$selection_freq > 0]
  cat(sprintf("Variables selected in all %d seeds (n=%d): %s\n",
              vs$n_seeds, length(always), paste(always, collapse = ", ")))
  cat(sprintf("Variables selected in some seeds (n=%d, churn): %s\n",
              length(ever) - length(always),
              paste(setdiff(ever, always), collapse = ", ")))
  cat("Top-10 by selection freq:\n")
  print(head(vs$per_variable, 10), row.names = FALSE)
}

cat("\nStep 3b complete. Runtime:", round(difftime(end_time, start_time, units = "mins"), 1),
    "minutes\n")
cat("\n=== Analysis Complete ===\n")
cat("Run 04_visualize_results.R to create visualizations.\n")

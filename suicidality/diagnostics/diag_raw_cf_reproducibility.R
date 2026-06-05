#!/usr/bin/env Rscript
# diag_raw_cf_reproducibility.R
#
# Reproduce run-to-run variation in the raw causal forest variable importance.
# Fits the raw CF twice with the current (unseeded) code path, then reports:
#   - per-variable VI for each run (sorted by Run 1 VI)
#   - Spearman rank correlation across runs
#   - top-N overlap (Jaccard)
#   - selection-set overlap (above-mean VI per Wang 2024 §2.2)

library(dplyr)
library(grf)
library(here)
here::i_am("suicidality/diagnostics/diag_raw_cf_reproducibility.R")

icf_data <- readRDS(here("suicidality", "analysis-icf", "data", "icf_data.rds"))
Y <- icf_data$Y
W <- icf_data$W
X_all <- as.matrix(icf_data[, !(names(icf_data) %in% c("Y", "W"))])
var_names <- colnames(X_all)
cat(sprintf("N = %d, vars = %d\n", nrow(X_all), ncol(X_all)))

# Replicate the (unseeded) sequence at icf_algorithm.R:579-615 verbatim.
fit_raw_cf <- function(label) {
  cat(sprintf("\n--- %s ---\n", label))
  set.seed(42)  # only seeds R's RNG -- grf uses its own C++ RNG, controlled
                # only by the `seed` arg, which is NOT passed below.

  t0 <- Sys.time()
  ps_forest <- regression_forest(X_all, W, num.trees = 500)
  W.hat <- predict(ps_forest)$predictions

  y_forest <- regression_forest(X_all, Y, num.trees = 500)
  Y.hat <- predict(y_forest)$predictions

  cf <- causal_forest(X_all, Y, W,
                      Y.hat = Y.hat, W.hat = W.hat,
                      num.trees = 2000, tune.parameters = "all")
  cat(sprintf("  elapsed: %.1f min\n", as.numeric(Sys.time() - t0, units = "mins")))

  vi <- variable_importance(cf)[, 1]
  names(vi) <- var_names
  vi
}

vi1 <- fit_raw_cf("Run 1")
vi2 <- fit_raw_cf("Run 2")

# Build comparison table
cmp <- data.frame(
  variable     = var_names,
  vi_run1      = vi1,
  vi_run2      = vi2,
  rank_run1    = rank(-vi1, ties.method = "min"),
  rank_run2    = rank(-vi2, ties.method = "min")
) %>%
  mutate(rank_diff = rank_run2 - rank_run1) %>%
  arrange(rank_run1)

cat("\n=== Top 15 by Run 1 ===\n")
cmp |> head(15) |> print(row.names = FALSE)

cat(sprintf(
  "\nSpearman rank correlation across all %d variables: %.4f\n",
  nrow(cmp), cor(vi1, vi2, method = "spearman")))

for (k in c(5, 10, 15)) {
  top1 <- cmp$variable[order(-vi1)][1:k]
  top2 <- cmp$variable[order(-vi2)][1:k]
  cat(sprintf("Top-%d Jaccard: %.2f  (intersection size %d / %d)\n",
              k, length(intersect(top1, top2)) / length(union(top1, top2)),
              length(intersect(top1, top2)), k))
}

# Wang-2024 §2.2 above-mean VI selection
sel1 <- var_names[vi1 > mean(vi1)]
sel2 <- var_names[vi2 > mean(vi2)]
cat(sprintf("Above-mean selection set: %d vs %d vars; Jaccard %.2f\n",
            length(sel1), length(sel2),
            length(intersect(sel1, sel2)) / length(union(sel1, sel2))))
cat("  Only in Run 1:", paste(setdiff(sel1, sel2), collapse = ", "), "\n")
cat("  Only in Run 2:", paste(setdiff(sel2, sel1), collapse = ", "), "\n")

saveRDS(list(vi1 = vi1, vi2 = vi2, cmp = cmp),
        here("suicidality", "diagnostics",
             "diag_raw_cf_reproducibility.rds"))
cat("\nSaved diag_raw_cf_reproducibility.rds\n")

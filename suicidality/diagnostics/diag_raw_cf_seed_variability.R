#!/usr/bin/env Rscript
# diag_raw_cf_seed_variability.R
#
# How much does the raw CF variable importance shift when only the RNG seed
# changes? Fits the raw CF (same data, same code path as icf_algorithm.R but
# with parameterized seed) 5 times with seeds {42, 43, 44, 45, 46}, then
# reports Spearman rank corr, top-N Jaccards, and above-mean selection-set
# agreement across the 5 runs.

library(dplyr)
library(grf)
library(here)
here::i_am("suicidality/diagnostics/diag_raw_cf_seed_variability.R")

icf_data <- readRDS(here("suicidality", "analysis-icf", "data", "icf_data.rds"))
Y <- icf_data$Y
W <- icf_data$W
X_all <- as.matrix(icf_data[, !(names(icf_data) %in% c("Y", "W"))])
var_names <- colnames(X_all)
cat(sprintf("N = %d, vars = %d\n", nrow(X_all), ncol(X_all)))

fit_raw_cf <- function(seed) {
  cat(sprintf("\n--- seed = %d ---\n", seed))
  set.seed(seed)
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
  list(vi = vi, tuned = cf$tunable.params)
}

seeds <- c(42L, 43L, 44L, 45L, 46L)
runs <- lapply(seeds, fit_raw_cf)

# Build VI matrix
M <- do.call(cbind, lapply(runs, `[[`, "vi"))
colnames(M) <- paste0("seed", seeds)

cat("\n=== Top 15 by seed 42 ===\n")
ord <- order(-M[, 1])
df <- data.frame(variable = var_names[ord], M[ord, ])
print(df |> head(15), row.names = FALSE)

cat("\n=== Tuned hyperparameters per seed ===\n")
tuned_df <- do.call(rbind, lapply(runs, \(r) data.frame(t(unlist(r$tuned)))))
tuned_df$seed <- seeds
print(tuned_df, row.names = FALSE)

# Pairwise Spearman across the 5 runs (full var set)
cat("\n=== Pairwise Spearman rank correlation (full var set) ===\n")
print(round(cor(M, method = "spearman"), 4))

# Top-N Jaccard across the 5 runs (each pair)
top_jaccard <- function(M, k) {
  s <- lapply(seq_len(ncol(M)), \(j) var_names[order(-M[, j])][seq_len(k)])
  J <- matrix(NA_real_, ncol(M), ncol(M),
              dimnames = list(seeds, seeds))
  for (i in seq_along(s)) for (j in seq_along(s)) {
    J[i, j] <- length(intersect(s[[i]], s[[j]])) /
               length(union(s[[i]], s[[j]]))
  }
  round(J, 3)
}
cat("\n=== Top-10 Jaccard ===\n");  print(top_jaccard(M, 10))
cat("\n=== Top-5  Jaccard ===\n");  print(top_jaccard(M, 5))

# Above-mean selection set per seed
cat("\n=== Above-mean selection set per seed ===\n")
sel_per <- lapply(seq_len(ncol(M)), \(j) var_names[M[, j] > mean(M[, j])])
for (i in seq_along(sel_per))
  cat(sprintf("seed %d (n=%d): %s\n",
              seeds[i], length(sel_per[[i]]),
              paste(sel_per[[i]], collapse = ", ")))

selJ <- matrix(NA_real_, length(sel_per), length(sel_per),
               dimnames = list(seeds, seeds))
for (i in seq_along(sel_per)) for (j in seq_along(sel_per))
  selJ[i, j] <- length(intersect(sel_per[[i]], sel_per[[j]])) /
                length(union(sel_per[[i]], sel_per[[j]]))
cat("\nAbove-mean selection-set Jaccard:\n"); print(round(selJ, 3))

saveRDS(list(seeds = seeds, M = M, tuned = tuned_df, sel_per = sel_per),
        here("suicidality", "diagnostics",
             "diag_raw_cf_seed_variability.rds"))
cat("\nSaved diag_raw_cf_seed_variability.rds\n")

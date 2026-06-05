# icf/icf_algorithm.R
# Main iterative Causal Forest algorithm
#
# Re-implementation based on Wang et al. (2024) Am J Epidemiol. 193(5):764-776

library(grf)
library(dplyr)

# Null-coalescing operator (returns b if a is NULL)
`%||%` <- function(a, b) if (!is.null(a)) a else b

# Source core functions
source(here::here("suicidality", "analysis-icf", "icf", "core.R"))

#' Run the forest-growing phase of iCF
#'
#' Grows multiple causal forests, extracts the best tree from each, and
#' records tree structures and depths. This is the computationally expensive
#' phase that can be parallelized across batches.
#'
#' @param X Covariate matrix
#' @param Y Outcome vector
#' @param W Treatment indicator
#' @param Y.hat Predicted outcomes (from regression forest)
#' @param W.hat Propensity scores (from regression forest)
#' @param selected_vars Indices of selected variables
#' @param target_depth Target tree depth (2, 3, 4, or 5)
#' @param n_trees Number of trees per forest
#' @param iterations Vector of iteration indices (e.g., 1:50 or 51:100).
#'   Seeds use seed_offset + k where k is the iteration index, so results
#'   are identical regardless of batching.
#' @param min_leaf_size Minimum observations per leaf
#' @param seed_offset Base seed for deterministic forest seeds
#' @return List with best_trees, tree_structures, tree_depths, var_names
run_icf_iterations <- function(X, Y, W, Y.hat, W.hat, selected_vars,
                                target_depth, n_trees = 200,
                                iterations = 1:50, min_leaf_size = NULL,
                                seed_offset = 0L) {
  var_names <- colnames(X)[selected_vars]
  X_selected <- X[, selected_vars, drop = FALSE]

  # Calculate default min_leaf_size if not provided
  if (is.null(min_leaf_size)) {
    # Heuristic: smaller leaf size for deeper trees
    denominators <- c(D2 = 25, D3 = 45, D4 = 65, D5 = 85)
    denom <- denominators[paste0("D", target_depth)]
    min_leaf_size <- max(5, round(nrow(X) / denom))
  }

  best_trees <- list()
  tree_structures <- character(length(iterations))
  tree_depths <- numeric(length(iterations))

  cat("  Running", length(iterations), "iterations for depth", target_depth,
      " (indices", min(iterations), "-", max(iterations), ")...\n")

  for (i in seq_along(iterations)) {
    k <- iterations[i]
    # Grow causal forest with deterministic seed for reproducibility.
    # Each iteration gets a unique seed: seed_offset + k.
    # Different callers pass different seed_offsets to avoid correlated forests.
    # Note: honesty.fraction = 0.5 splits data equally between tree-building
    # and estimation halves, which is standard for honest causal forests (#39)
    cf <- causal_forest(
      X = X_selected,
      Y = Y,
      W = W,
      Y.hat = Y.hat,
      W.hat = W.hat,
      num.trees = n_trees,
      min.node.size = min_leaf_size,
      honesty = TRUE,
      honesty.fraction = 0.5,  # Standard: equal split for honest estimation
      tune.parameters = "none",
      seed = seed_offset + k
    )

    # Find best tree
    bt_result <- find_best_tree(cf)
    best_trees[[i]] <- bt_result$tree

    # Extract structure
    tree_structures[i] <- extract_tree_structure(bt_result$tree, var_names)
    tree_depths[i] <- get_tree_depth(bt_result$tree)
  }

  list(best_trees = best_trees,
       tree_structures = tree_structures,
       tree_depths = tree_depths,
       var_names = var_names)
}

#' Diagnose depth distribution for minimum leaf size tuning
#'
#' For each candidate depth, grows a set of causal forests and records the
#' actual depth of the best tree from each. This produces the bar charts
#' shown in Wang et al. (2024) Step 2, which verify that the min_leaf_size
#' heuristic produces trees at the intended depth.
#'
#' @param X Full covariate matrix
#' @param Y Outcome vector
#' @param W Treatment indicator
#' @param Y.hat Predicted outcomes
#' @param W.hat Propensity scores
#' @param selected_vars Indices of selected variables
#' @param depths Vector of target depths (default c(2,3,4,5))
#' @param n_trees Trees per forest
#' @param n_iterations Number of forests to grow per depth
#' @param verbose Print progress
#' @return Named list (D2, D3, ...) each with target_depth, actual_depths,
#'   denominator, min_leaf_size
diagnose_depth_distribution <- function(X, Y, W, Y.hat, W.hat, selected_vars,
                                         depths = c(2, 3, 4, 5), n_trees = 200,
                                         n_iterations = 50, verbose = TRUE) {
  if (verbose) cat("\n--- Depth Distribution Diagnostics ---\n")

  results <- list()
  denominators <- c(D2 = 25, D3 = 45, D4 = 65, D5 = 85)

  for (depth in depths) {
    denom <- denominators[paste0("D", depth)]
    min_leaf_size <- max(5, round(nrow(X) / denom))

    if (verbose) {
      cat(sprintf("  D%d: denominator=%d, min_leaf_size=%d, growing %d forests...\n",
                  depth, denom, min_leaf_size, n_iterations))
    }

    iter_result <- run_icf_iterations(
      X = X, Y = Y, W = W, Y.hat = Y.hat, W.hat = W.hat,
      selected_vars = selected_vars,
      target_depth = depth,
      n_trees = n_trees,
      iterations = seq_len(n_iterations),
      min_leaf_size = min_leaf_size,
      seed_offset = 999000L + depth * 1000L
    )

    if (verbose) {
      depth_tab <- table(factor(iter_result$tree_depths,
                                levels = seq_len(max(depths + 1))))
      cat(sprintf("    Depth distribution: %s\n",
                  paste(names(depth_tab), depth_tab, sep = ":", collapse = "  ")))
      cat(sprintf("    Mean depth: %.2f\n", mean(iter_result$tree_depths)))
    }

    results[[paste0("D", depth)]] <- list(
      target_depth = depth,
      actual_depths = iter_result$tree_depths,
      denominator = denom,
      min_leaf_size = min_leaf_size
    )
  }

  results
}

#' Finalize iCF by plurality voting and subgroup assignment
#'
#' Takes the output of one or more run_icf_iterations() calls (after
#' concatenation), performs plurality voting, builds a synthetic tree,
#' and assigns observations to subgroups.
#'
#' @param best_trees List of best tree objects
#' @param tree_structures Character vector of tree structure strings
#' @param tree_depths Numeric vector of tree depths
#' @param X_selected Covariate matrix (already subsetted to selected vars)
#' @param var_names Variable names for selected variables
#' @param target_depth Target tree depth
#' @return List with voted tree structure and subgroup decision
finalize_icf_voted_tree <- function(best_trees, tree_structures, tree_depths,
                                     X_selected, var_names, target_depth) {
  n_iterations <- length(tree_structures)

  # Plurality vote for most common structure
  vote_result <- plurality_vote_structure(tree_structures)

  # Report tree degeneration (#29)
  n_no_split <- sum(tree_structures == "NO_SPLIT", na.rm = TRUE)
  if (n_no_split > 0) {
    warning(paste0(n_no_split, " of ", n_iterations,
                   " iterations produced degenerate trees (NO_SPLIT). ",
                   "Consider reducing min_leaf_size or checking data."))
  }

  if (is.null(vote_result)) {
    return(list(
      success = FALSE,
      message = paste0("No valid tree structures found. ", n_no_split,
                       " degenerate trees. Check min_leaf_size or data quality.")
    ))
  }

  cat("  Most common structure (", vote_result$count, "/", vote_result$total,
      " = ", round(vote_result$frequency * 100, 1), "%):\n", sep = "")
  cat("   ", vote_result$structure, "\n")

  # Warn about low vote frequency (#25)
  if (vote_result$frequency < 0.3) {
    warning(paste0("Low vote frequency (", round(vote_result$frequency * 100, 1),
                   "%) indicates unstable tree structure. ",
                   "Results may not be reproducible."))
  }

  # Get trees matching the voted structure
  matching_idx <- which(tree_structures == vote_result$structure)
  matching_trees <- best_trees[matching_idx]

  # Check for empty matching trees (#28)
  if (length(matching_trees) == 0) {
    return(list(
      success = FALSE,
      message = "No trees match voted structure (internal error)"
    ))
  }

  # Build synthetic tree with mean split values
  synthetic_splits <- build_synthetic_tree(matching_trees, var_names)

  # Check synthetic tree was built successfully
  if (is.null(synthetic_splits)) {
    return(list(
      success = FALSE,
      message = "Failed to build synthetic tree from matching trees"
    ))
  }

  # Assign observations to subgroups
  subgroup_result <- assign_subgroups(X_selected, synthetic_splits, var_names)

  # Calculate mean_depth with validation (#47)
  valid_depths <- tree_depths[tree_depths > 0 & !is.na(tree_depths)]
  if (length(valid_depths) == 0) {
    warning("No valid tree depths collected (all trees may be degenerate)")
    computed_mean_depth <- NA_real_
  } else {
    computed_mean_depth <- mean(valid_depths)
  }

  return(list(
    success = TRUE,
    target_depth = target_depth,
    voted_structure = vote_result$structure,
    vote_frequency = vote_result$frequency,
    vote_distribution = vote_result$vote_distribution,
    root_split_distribution = vote_result$root_split_distribution,
    synthetic_splits = synthetic_splits,
    subgroup_id = subgroup_result$subgroup_id,
    subgroup_labels = subgroup_result$subgroup_labels,
    n_subgroups = subgroup_result$n_subgroups,
    mean_depth = computed_mean_depth
  ))
}

#' Apply Breiman et al. (1984) 1-SE rule to depth selection
#'
#' When the cross-validated MSE landscape across candidate depths is flat
#' relative to within-fold noise, the CV argmin is governed by simulation
#' variance rather than by genuine out-of-sample fit. The 1-SE rule
#' (Breiman, Friedman, Olshen, Stone 1984, "Classification and Regression
#' Trees", §3.4.3 — Wang 2024 reference 14) declares two candidate models
#' to be statistically equivalent when their CV-MSEs differ by less than
#' one standard error of the minimum, and selects the simplest model among
#' those that are statistically equivalent to the minimum.
#'
#' Wang 2024 itself uses pure CV-argmin (no 1-SE rule). We add the 1-SE
#' rule as an explicit, citable fallback that fires when CV-MSE is flat.
#'
#' @param cv_mse K-by-length(depths) matrix of per-fold CV-MSEs (rows = folds)
#' @param depths integer vector of candidate depths
#' @return List with:
#'   - cv_min_depth: depth at the CV argmin (Wang 2024 behavior)
#'   - one_se_depth: shallowest depth in the 1-SE plateau (this is the
#'     reported headline depth when is_flat is TRUE)
#'   - plateau: depths within 1*SE of the CV minimum
#'   - se: standard error of the CV minimum (= per-depth SD across folds
#'     at the argmin, divided by sqrt(K))
#'   - threshold: min_cv_mse + se
#'   - is_flat: TRUE iff |plateau| > 1 (1-SE rule fires)
select_depth_1se <- function(cv_mse, depths) {
  mean_mse <- colMeans(cv_mse, na.rm = TRUE)
  K <- nrow(cv_mse)
  fold_sd <- apply(cv_mse, 2, sd, na.rm = TRUE)
  se <- fold_sd / sqrt(K)
  argmin <- which.min(mean_mse)
  se_min <- se[argmin]
  threshold <- mean_mse[argmin] + se_min
  in_plateau <- mean_mse <= threshold
  selected_idx <- min(which(in_plateau))
  list(
    cv_min_depth = depths[argmin],
    one_se_depth = depths[selected_idx],
    plateau = depths[in_plateau],
    se = se_min,
    threshold = threshold,
    is_flat = sum(in_plateau) > 1
  )
}

#' Variable-selection stability across seeds
#'
#' Refits the raw causal forest n_seeds times with different RNG seeds, records
#' the variable-importance ranking and the "above-mean" selection set for each
#' seed, and returns per-variable selection frequencies plus the pairwise
#' Jaccard index across selection sets. Costs roughly n_seeds * (raw CF time);
#' use a small n_seeds (e.g., 5).
#'
#' @param X Candidate-splitting covariate matrix (X_can in icf_variable_selection)
#' @param Y Outcome
#' @param W Treatment indicator
#' @param Y.hat Outcome forest predictions (re-used; cheap)
#' @param W.hat Propensity-forest predictions (re-used; cheap)
#' @param n_seeds Number of seeds (default 5)
#' @param num.trees Trees per raw CF (default 2000, matching icf_variable_selection)
#' @param select_top_pct If non-NULL, top-S% selection rule (hdiCF mode);
#'        otherwise uses VI > mean (standard iCF rule).
#' @param verbose Print progress
#' @return List with per_variable (data.frame: variable, selection_freq,
#'         median_rank, rank_iqr), jaccard_pairwise (matrix), and
#'         selected_per_seed (list of character vectors).
vi_stability <- function(X, Y, W, Y.hat, W.hat,
                         n_seeds = 5, num.trees = 2000,
                         select_top_pct = NULL, verbose = TRUE) {
  var_names <- colnames(X)
  selected_per_seed <- vector("list", n_seeds)
  rank_per_seed <- matrix(NA_integer_, nrow = length(var_names), ncol = n_seeds,
                          dimnames = list(var_names, paste0("seed", seq_len(n_seeds))))

  for (s in seq_len(n_seeds)) {
    if (verbose) cat(sprintf("  VI stability seed %d/%d ...\n", s, n_seeds))
    forest_seed <- 20260510L + s
    set.seed(forest_seed)
    # Match the raw-CF call in icf_variable_selection (tune.parameters = "none";
    # tuning is unstable on this cohort and was dropped in the main pipeline).
    cf_s <- causal_forest(X, Y, W, Y.hat = Y.hat, W.hat = W.hat,
                          num.trees = num.trees, tune.parameters = "none",
                          seed = forest_seed)
    vi_s <- variable_importance(cf_s)
    if (length(vi_s) == length(var_names)) names(vi_s) <- var_names

    if (!is.null(select_top_pct)) {
      top_n <- max(2, round(length(vi_s) * select_top_pct))
      sel <- names(sort(vi_s, decreasing = TRUE))[seq_len(top_n)]
    } else {
      sel <- names(vi_s)[vi_s > mean(vi_s)]
      if (length(sel) == 0) sel <- names(sort(vi_s, decreasing = TRUE))[seq_len(min(3, length(vi_s)))]
    }
    selected_per_seed[[s]] <- sel
    rank_per_seed[, s] <- rank(-vi_s, ties.method = "min")[match(var_names, names(vi_s))]

    # Drop the forest before the next iteration to release grf's C++ backing
    # store. Without this, the five sequential forests accumulate in memory
    # and OOM the SLURM allocation (job 710734 was killed at seed 2/5 with
    # only 8 GB; even after the 8 -> 32 GB bump it's wasteful to keep five
    # large forest objects alive when we only need the VI vector).
    rm(cf_s, vi_s)
    gc(verbose = FALSE)
  }

  # Per-variable selection frequency and rank summary
  all_vars <- var_names
  selection_freq <- sapply(all_vars, function(v) mean(sapply(selected_per_seed, function(s) v %in% s)))
  median_rank <- apply(rank_per_seed, 1, median, na.rm = TRUE)
  rank_iqr <- apply(rank_per_seed, 1, function(r) diff(quantile(r, c(0.25, 0.75), na.rm = TRUE)))
  per_variable <- data.frame(
    variable = all_vars,
    selection_freq = as.numeric(selection_freq),
    median_rank = as.numeric(median_rank),
    rank_iqr = as.numeric(rank_iqr),
    stringsAsFactors = FALSE
  )
  per_variable <- per_variable[order(-per_variable$selection_freq, per_variable$median_rank), ]
  rownames(per_variable) <- NULL

  # Pairwise Jaccard across selection sets
  jacc <- matrix(NA_real_, n_seeds, n_seeds)
  for (a in seq_len(n_seeds)) for (b in seq_len(n_seeds)) {
    A <- selected_per_seed[[a]]; B <- selected_per_seed[[b]]
    jacc[a, b] <- length(intersect(A, B)) / max(1L, length(union(A, B)))
  }

  list(
    n_seeds = n_seeds,
    per_variable = per_variable,
    jaccard_pairwise = jacc,
    mean_offdiag_jaccard = mean(jacc[lower.tri(jacc)]),
    selected_per_seed = selected_per_seed,
    rank_per_seed = rank_per_seed
  )
}

#' Run iterative Causal Forest at a specific depth
#'
#' Grows multiple causal forests, extracts best trees, and performs
#' plurality voting to find the most stable tree structure.
#' Thin wrapper around run_icf_iterations() + finalize_icf_voted_tree().
#'
#' @param X Covariate matrix
#' @param Y Outcome vector
#' @param W Treatment indicator
#' @param Y.hat Predicted outcomes (from regression forest)
#' @param W.hat Propensity scores (from regression forest)
#' @param selected_vars Indices of selected variables
#' @param target_depth Target tree depth (2, 3, 4, or 5)
#' @param n_trees Number of trees per forest
#' @param n_iterations Number of forest iterations
#' @param min_leaf_size Minimum observations per leaf
#' @param seed_offset Base seed for deterministic forest seeds. Iteration k
#'   uses seed = seed_offset + k. Different callers should use different
#'   offsets (e.g., fold * 100000 + depth * 1000) to avoid correlated forests.
#' @return List with voted tree structure and subgroup decision
run_icf_at_depth <- function(X, Y, W, Y.hat, W.hat, selected_vars,
                             target_depth, n_trees = 200, n_iterations = 50,
                             min_leaf_size = NULL, seed_offset = 0L) {
  iter_result <- run_icf_iterations(
    X = X, Y = Y, W = W, Y.hat = Y.hat, W.hat = W.hat,
    selected_vars = selected_vars,
    target_depth = target_depth,
    n_trees = n_trees,
    iterations = 1:n_iterations,
    min_leaf_size = min_leaf_size,
    seed_offset = seed_offset
  )

  var_names <- colnames(X)[selected_vars]
  X_selected <- X[, selected_vars, drop = FALSE]

  finalize_icf_voted_tree(
    best_trees = iter_result$best_trees,
    tree_structures = iter_result$tree_structures,
    tree_depths = iter_result$tree_depths,
    X_selected = X_selected,
    var_names = var_names,
    target_depth = target_depth
  )
}

#' Fit transformed outcome model for subgroup evaluation
#'
#' Fits a linear model to predict the transformed outcome using
#' subgroup indicators and their interactions with treatment.
#'
#' @param Y_star Transformed outcome
#' @param W Treatment indicator
#' @param X Covariates
#' @param subgroup_id Subgroup assignments
#' @return Model object and MSE
fit_subgroup_model <- function(Y_star, W, X, subgroup_id) {
  # Create model data
  model_data <- data.frame(
    Y_star = Y_star,
    W = W,
    G = factor(subgroup_id)
  )

  # Add covariates with prefix to avoid name conflicts (#41)
  X_df <- as.data.frame(X)
  reserved_names <- c("Y_star", "W", "G")
  conflicting <- intersect(colnames(X_df), reserved_names)
  if (length(conflicting) > 0) {
    warning(paste0("Covariate names conflict with model terms: ",
                   paste(conflicting, collapse = ", "), ". Adding 'X_' prefix."))
    colnames(X_df) <- paste0("X_", colnames(X_df))
  }
  model_data <- cbind(model_data, X_df)

  # Fit model: Y* ~ covariates + W + G + W:G
  # The G and W:G terms capture heterogeneity
  covar_names <- colnames(X_df)
  formula_str <- paste0("Y_star ~ ", paste(covar_names, collapse = " + "),
                        " + W + G + W:G")

  # Validate formula can be parsed (#48)
  formula_obj <- tryCatch({
    as.formula(formula_str)
  }, error = function(e) {
    NULL
  })

  if (is.null(formula_obj)) {
    return(list(
      model = NULL,
      mse = Inf,
      predictions = NULL,
      error = paste0("Invalid formula (check covariate names): ", formula_str)
    ))
  }

  tryCatch({
    model <- lm(formula_obj, data = model_data)
    predictions <- predict(model)
    mse <- mean((Y_star - predictions)^2, na.rm = TRUE)

    return(list(
      model = model,
      mse = mse,
      predictions = predictions
    ))
  }, error = function(e) {
    return(list(
      model = NULL,
      mse = Inf,
      predictions = NULL,
      error = e$message
    ))
  })
}

#' iCF Step 1: Variable selection and setup
#'
#' Estimates nuisance parameters, runs a raw causal forest, tests for
#' heterogeneity, performs variable selection, and generates CV fold IDs.
#'
#' @param data Data frame with Y, W, and covariates
#' @param K Number of CV folds
#' @param p_threshold P-value threshold for heterogeneity test
#' @param adjust_only Character vector of variable names to include in nuisance
#'   models (propensity score and outcome regression) but exclude from the raw
#'   causal forest, variable importance, and iCF splitting candidates. Use this
#'   for variables needed for confounding adjustment but not meaningful as effect
#'   modifiers (e.g., calendar year). Default NULL = all variables used everywhere.
#' @param skip_het_test If TRUE, skip the heterogeneity test and always proceed.
#'   Recommended for hdiCF where sparse HD variables can dilute the signal
#'   (Wang et al. 2025).
#' @param select_top_pct If non-NULL, select the top S% of variables by VI
#'   instead of variables with importance > mean. E.g., 0.05 for top 5%.
#'   Recommended for hdiCF (Wang et al. 2025).
#' @param verbose Print progress
#' @return List with X, Y, W, var_names, W.hat, Y.hat, var_imp, selected_vars,
#'   het_p, fold_ids, cf_raw
icf_variable_selection <- function(data, K = 5, p_threshold = 0.1,
                                    adjust_only = NULL, skip_het_test = FALSE,
                                    select_top_pct = NULL, verbose = TRUE) {

  # Extract components
  Y <- data$Y
  W <- data$W
  X_all <- as.matrix(data[, !(names(data) %in% c("Y", "W"))])
  all_var_names <- colnames(X_all)
  n <- nrow(X_all)

  # Separate adjustment-only variables from candidate splitting variables
  if (!is.null(adjust_only)) {
    missing <- setdiff(adjust_only, all_var_names)
    if (length(missing) > 0) {
      warning("adjust_only variables not found in data: ",
              paste(missing, collapse = ", "))
      adjust_only <- intersect(adjust_only, all_var_names)
    }
    candidate_idx <- !(all_var_names %in% adjust_only)
    X <- X_all[, candidate_idx, drop = FALSE]
    var_names <- colnames(X)
    if (verbose) {
      cat("Adjustment-only variables (nuisance models only):",
          paste(adjust_only, collapse = ", "), "\n")
      cat("Candidate splitting variables:", length(var_names), "\n")
    }
  } else {
    X <- X_all
    var_names <- all_var_names
  }

  # Validate binary outcome and treatment
  if (!all(Y %in% c(0, 1, NA))) {
    unique_y <- unique(Y[!is.na(Y)])
    if (length(unique_y) <= 10) {
      warning(paste0("Outcome Y is not binary (0/1). Found values: ",
                     paste(sort(unique_y), collapse = ", "),
                     ". CATE interpretation as risk difference may be invalid."))
    } else {
      warning(paste0("Outcome Y is not binary (0/1). Found ", length(unique_y),
                     " unique values. CATE interpretation as risk difference may be invalid."))
    }
  }

  if (!all(W %in% c(0, 1, NA))) {
    stop("Treatment W must be binary (0/1). Found non-binary values.")
  }

  if (verbose) cat("\n=== Iterative Causal Forest Analysis ===\n")
  if (verbose) cat("Sample size:", n, "\n")
  if (verbose) cat("Number of covariates (total):", ncol(X_all), "\n")
  if (verbose && !is.null(adjust_only)) {
    cat("Number of covariates (splitting candidates):", ncol(X), "\n")
  }

  # Step 1: Raw causal forest for variable selection and heterogeneity test
  if (verbose) cat("\n--- Step 1: Raw Causal Forest ---\n")

  # Seed for reproducibility of nuisance models and raw causal forest.
  # Bumped from 42 to 43 on 2026-05-14 to test seed-robustness of the
  # variable-selection set, voted-tree structures, and CATE estimates.
  set.seed(43)

  # Estimate nuisance parameters using ALL covariates (including adjust_only).
  # Explicit seeds make the nuisance forests reproducible across sessions
  # (set.seed() above is belt-and-suspenders; grf consumes its own seed via
  # runif when none is provided, but passing it explicitly is more robust).
  if (verbose) cat("Estimating propensity scores...\n")
  ps_forest <- regression_forest(X_all, W, num.trees = 500, seed = 43L)
  W.hat <- predict(ps_forest)$predictions

  # Propensity score diagnostics
  ps_min <- min(W.hat, na.rm = TRUE)
  ps_max <- max(W.hat, na.rm = TRUE)
  # Explicitly handle NAs in extreme check (#51)
  ps_extreme <- sum((W.hat < 0.01 | W.hat > 0.99) & !is.na(W.hat))
  ps_missing <- sum(is.na(W.hat))

  if (verbose) {
    cat(sprintf("  PS range: [%.3f, %.3f]\n", ps_min, ps_max))
    if (ps_missing > 0) {
      cat(sprintf("  WARNING: %d observations have missing PS\n", ps_missing))
    }
    if (ps_extreme > 0) {
      cat(sprintf("  WARNING: %d observations (%.1f%%) have extreme PS (<0.01 or >0.99)\n",
                  ps_extreme, 100 * ps_extreme / sum(!is.na(W.hat))))
    }
  }

  if (ps_min < 0.001 || ps_max > 0.999) {
    warning("Propensity scores have extreme values. Consider checking covariate overlap.")
  }

  if (verbose) cat("Estimating expected outcomes...\n")
  y_forest <- regression_forest(X_all, Y, num.trees = 500, seed = 43L)
  Y.hat <- predict(y_forest)$predictions

  # Raw causal forest — uses only candidate splitting variables (excludes adjust_only).
  # tune.parameters = "none" (grf defaults): the auto-tuned variant produced highly
  # seed-dependent hyperparameters on this cohort (sample.fraction 0.06-0.46,
  # min.node.size 38-561 across 5 seeds), causing the above-mean VI filter to
  # select between 4 and 14 variables seed-to-seed. The CV-MSE landscape used by
  # the tuner is flat (relative range ~10^-4), so the tuner picks essentially at
  # random among configurations with equivalent CV fit; dropping tuning gives a
  # reproducible forest. Explicit seed (43; bumped from 42 on 2026-05-14
  # to test seed-robustness) for cross-session determinism.
  if (verbose) cat("Growing raw causal forest...\n")
  cf_raw <- causal_forest(X, Y, W, Y.hat = Y.hat, W.hat = W.hat,
                          num.trees = 2000, tune.parameters = "none", seed = 43L)

  # Test for heterogeneity
  calibration <- test_calibration(cf_raw)

  # Extract heterogeneity p-value safely by name
  # Row: "differential.forest.prediction" tests for heterogeneity
  # Column: "Pr(>t)" contains the p-value
  het_row <- "differential.forest.prediction"
  het_col <- "Pr(>t)"

  if (het_row %in% rownames(calibration) && het_col %in% colnames(calibration)) {
    het_p <- calibration[het_row, het_col]
  } else {
    # Fallback to index-based access with warning
    warning("test_calibration() matrix structure unexpected, using index [2,4]")
    het_p <- calibration[2, 4]
  }

  if (verbose) {
    if (skip_het_test) {
      cat("Heterogeneity test skipped (hdiCF mode)\n")
      cat("  (p-value was:", round(het_p, 4), ")\n")
    } else {
      cat("Heterogeneity test p-value:", round(het_p, 4), "\n")
      if (het_p > p_threshold) {
        cat("WARNING: Limited evidence of heterogeneity (p >", p_threshold, ")\n")
      }
    }
  }

  # Variable importance and selection
  var_imp <- variable_importance(cf_raw)

  # Safely assign names (#32)
  if (length(var_imp) == length(var_names)) {
    names(var_imp) <- var_names
  } else {
    warning(paste0("Variable importance length (", length(var_imp),
                   ") doesn't match variable names (", length(var_names),
                   "). Using numeric indices."))
    names(var_imp) <- paste0("V", seq_along(var_imp))
  }

  if (!is.null(select_top_pct)) {
    # Select top S% of variables by VI (hdiCF mode)
    top_n <- max(2, round(length(var_imp) * select_top_pct))
    selected_vars <- order(var_imp, decreasing = TRUE)[1:top_n]
    if (verbose) {
      cat(sprintf("Selected top %g%% = %d variables by VI\n",
                  select_top_pct * 100, length(selected_vars)))
    }
  } else {
    # Select variables with importance > mean (standard iCF)
    mean_imp <- mean(var_imp)
    selected_vars <- which(var_imp > mean_imp)

    # Check for empty selection
    if (length(selected_vars) == 0) {
      warning("No variables above mean importance. Using top 3 variables instead.")
      top_n <- min(3, length(var_imp))
      selected_vars <- order(var_imp, decreasing = TRUE)[1:top_n]
    }

    if (verbose) {
      cat("Selected", length(selected_vars), "variables with importance > mean\n")
    }
  }

  if (verbose) {
    cat("Variables:", paste(var_names[selected_vars], collapse = ", "), "\n")
  }

  # Generate CV fold IDs
  set.seed(1234)
  fold_ids <- sample(rep(1:K, length.out = n))

  # Validate all K folds have observations (#50)
  actual_folds <- length(unique(fold_ids))
  if (actual_folds < K) {
    warning(paste0("Only ", actual_folds, " of ", K,
                   " folds have observations. Consider reducing K."))
  }

  return(list(
    X = X,
    Y = Y,
    W = W,
    var_names = var_names,
    W.hat = W.hat,
    Y.hat = Y.hat,
    var_imp = var_imp,
    selected_vars = selected_vars,
    het_p = het_p,
    fold_ids = fold_ids,
    cf_raw = cf_raw
  ))
}

#' iCF Step 2: Single CV fold evaluation
#'
#' Runs one iteration of the cross-validation: trains iCF at a given depth
#' on the training fold, evaluates test MSE on the held-out fold.
#'
#' @param step1 Result list from icf_variable_selection()
#' @param fold Fold index to hold out
#' @param depth Target tree depth
#' @param n_trees Number of trees per forest
#' @param n_iterations Number of iterations per depth
#' @return Scalar CV MSE value
icf_cv_fold <- function(step1, fold, depth, n_trees = 200, n_iterations = 50) {
  X             <- step1$X
  Y             <- step1$Y
  W             <- step1$W
  var_names     <- step1$var_names
  W.hat         <- step1$W.hat
  Y.hat         <- step1$Y.hat
  selected_vars <- step1$selected_vars
  fold_ids      <- step1$fold_ids

  train_idx <- fold_ids != fold
  test_idx  <- fold_ids == fold

  # Training data
  X_train     <- X[train_idx, , drop = FALSE]
  Y_train     <- Y[train_idx]
  W_train     <- W[train_idx]
  Y.hat_train <- Y.hat[train_idx]
  W.hat_train <- W.hat[train_idx]

  # Test data
  X_test      <- X[test_idx, , drop = FALSE]
  Y_test      <- Y[test_idx]
  W_test      <- W[test_idx]
  W.hat_test  <- W.hat[test_idx]

  # Run iCF at this depth on training data
  # Deterministic seed: unique per fold/depth combination
  icf_result <- run_icf_at_depth(
    X = X_train, Y = Y_train, W = W_train,
    Y.hat = Y.hat_train, W.hat = W.hat_train,
    selected_vars = selected_vars,
    target_depth = depth,
    n_trees = n_trees,
    n_iterations = n_iterations,
    seed_offset = fold * 100000L + depth * 1000L
  )

  if (!icf_result$success) {
    warning(paste0("CV fold ", fold, " depth ", depth, " failed: ",
                   icf_result$message %||% "unknown error"))
    return(Inf)
  }

  # Calculate transformed outcome for training
  Y_star_train <- calculate_transformed_outcome(Y_train, W_train, W.hat_train)

  # Fit model on training data
  model_result <- fit_subgroup_model(
    Y_star = Y_star_train,
    W = W_train,
    X = X_train[, selected_vars, drop = FALSE],
    subgroup_id = icf_result$subgroup_id
  )

  if (is.null(model_result$model)) {
    warning(paste0("CV fold ", fold, " depth ", depth,
                   " failed: subgroup model fitting error - ",
                   model_result$error %||% "unknown"))
    return(Inf)
  }

  # Assign test observations to subgroups
  test_subgroups <- assign_subgroups(
    X_test[, selected_vars, drop = FALSE],
    icf_result$synthetic_splits,
    var_names[selected_vars]
  )

  # Calculate transformed outcome for test
  Y_star_test <- calculate_transformed_outcome(Y_test, W_test, W.hat_test)

  # Handle factor levels across train/test
  # Use union of train and test subgroup levels to avoid factor mismatch
  train_levels <- unique(icf_result$subgroup_id)
  test_levels  <- unique(test_subgroups$subgroup_id)
  unseen_levels <- setdiff(test_levels, train_levels)

  if (length(unseen_levels) > 0) {
    warning(paste0("CV fold ", fold, " depth ", depth,
                   ": test data has ", length(unseen_levels),
                   " subgroup(s) not seen in training"))
  }

  all_levels <- sort(unique(c(train_levels, test_levels)))
  test_data <- data.frame(
    Y_star = Y_star_test,
    W = W_test,
    G = factor(test_subgroups$subgroup_id, levels = all_levels)
  )
  test_data <- cbind(test_data, as.data.frame(X_test[, selected_vars, drop = FALSE]))

  tryCatch({
    pred_test <- predict(model_result$model, newdata = test_data)
    mean((Y_star_test - pred_test)^2, na.rm = TRUE)
  }, error = function(e) {
    warning(paste0("CV fold ", fold, " depth ", depth,
                   " failed: test prediction error - ", e$message))
    Inf
  })
}

#' iCF Step 3: Select best depth and fit final model
#'
#' Selects the best tree depth from cross-validation MSE, fits the final iCF
#' model on the full data, and calculates subgroup CATEs.
#'
#' @param step1 Result list from icf_variable_selection()
#' @param cv_mse Matrix of CV MSE values (K x length(depths))
#' @param depths Vector of tree depths tried
#' @param n_trees Number of trees per forest
#' @param n_iterations Number of iterations per depth
#' @param n_bootstrap Number of bootstrap resamples for 95% CI (0 = no CI)
#' @param verbose Print progress
#' @return List with final subgroup decision and CATE estimates (same structure
#'   as run_icf_cv())
icf_select_and_finalize <- function(step1, cv_mse, depths, n_trees = 200,
                                     n_iterations = 50, n_bootstrap = 0,
                                     verbose = TRUE) {
  X             <- step1$X
  Y             <- step1$Y
  W             <- step1$W
  var_names     <- step1$var_names
  W.hat         <- step1$W.hat
  Y.hat         <- step1$Y.hat
  var_imp       <- step1$var_imp
  selected_vars <- step1$selected_vars
  het_p         <- step1$het_p

  # Select best depth
  if (verbose) cat("\n--- Step 3: Select Best Depth ---\n")

  mean_cv_mse <- colMeans(cv_mse, na.rm = TRUE)

  # Check if all depths failed
  if (all(is.infinite(mean_cv_mse))) {
    warning("All depths have Inf MSE - all CV folds failed. Using smallest depth as fallback.")
    best_depth_idx <- 1
  } else {
    # Select depth with minimum mean CV MSE (Wang et al. 2024)
    best_depth_idx <- which.min(mean_cv_mse)
  }

  best_depth <- depths[best_depth_idx]

  if (verbose) {
    cat("Cross-validated MSE by depth:\n")
    for (d_idx in seq_along(depths)) {
      cat("  D", depths[d_idx], ":", round(mean_cv_mse[d_idx], 4), "\n", sep = "")
    }
    cat("Selected depth:", best_depth, "\n")
  }

  # Final model on full data
  if (verbose) cat("\n--- Step 4: Final Model ---\n")

  # Seed offset 0 for the final model (distinct from CV folds which use
  # fold * 100000 + depth * 1000)
  final_result <- run_icf_at_depth(
    X = X, Y = Y, W = W,
    Y.hat = Y.hat, W.hat = W.hat,
    selected_vars = selected_vars,
    target_depth = best_depth,
    n_trees = n_trees,
    n_iterations = n_iterations,
    seed_offset = 0L
  )

  # Calculate CATE for each subgroup
  if (verbose) cat("\nCalculating subgroup CATEs...\n")

  cate_results <- calculate_subgroup_cate(
    Y = Y, W = W, X = X,
    subgroup_id = final_result$subgroup_id,
    subgroup_labels = final_result$subgroup_labels,
    W.hat = W.hat,
    n_bootstrap = n_bootstrap
  )

  # Compile final results
  results <- list(
    # Heterogeneity test
    het_p_value = het_p,

    # Variable selection
    selected_vars = selected_vars,
    var_names = var_names[selected_vars],
    var_importance = var_imp,

    # CV results
    cv_mse = cv_mse,
    mean_cv_mse = mean_cv_mse,
    best_depth = best_depth,

    # Final subgroup decision
    final_result = final_result,
    subgroup_id = final_result$subgroup_id,
    subgroup_labels = final_result$subgroup_labels,
    n_subgroups = final_result$n_subgroups,

    # CATE estimates
    cate = cate_results,

    # Raw objects for further analysis
    cf_raw = step1$cf_raw,
    W.hat = W.hat,
    Y.hat = Y.hat
  )

  # Warn if only one subgroup found (#33)
  if (final_result$n_subgroups == 1) {
    warning("Only 1 subgroup identified - no heterogeneous treatment effects found. ",
            "All observations assigned to a single group.")
  }

  if (verbose) {
    cat("\n=== iCF Complete ===\n")
    cat("Final subgroups:", final_result$n_subgroups, "\n")
    cat("\nSubgroup CATEs:\n")
    print(cate_results)
  }

  return(results)
}

#' Main iCF with cross-validation
#'
#' Implements the full iCF algorithm with K-fold cross-validation
#' to select the optimal subgroup decision. Delegates to
#' icf_variable_selection(), icf_cv_fold(), and icf_select_and_finalize().
#'
#' @note Reproducibility: CV fold assignment uses seed 1234. Individual causal
#'   forests use deterministic seeds derived from fold/depth/iteration indices,
#'   so results are fully reproducible across runs.
#'
#' @param data Data frame with Y, W, and covariates
#' @param K Number of CV folds
#' @param n_trees Number of trees per forest
#' @param n_iterations Number of iterations per depth
#' @param depths Vector of tree depths to try
#' @param p_threshold P-value threshold for heterogeneity test
#' @param n_bootstrap Number of bootstrap resamples for 95% CI (0 = no CI)
#' @param verbose Print progress
#' @return List with final subgroup decision and CATE estimates
run_icf_cv <- function(data, K = 5, n_trees = 200, n_iterations = 50,
                       n_iterations_final = NULL,
                       depths = c(2, 3, 4, 5), p_threshold = 0.1,
                       n_bootstrap = 0, adjust_only = NULL, verbose = TRUE) {

  if (is.null(n_iterations_final)) n_iterations_final <- n_iterations

  step1 <- icf_variable_selection(data, K, p_threshold, adjust_only, verbose)

  # Step 2: Cross-validation
  if (verbose) cat("\n--- Step 2: K-fold Cross-Validation ---\n")

  cv_mse <- matrix(NA, nrow = K, ncol = length(depths))
  colnames(cv_mse) <- paste0("D", depths)

  for (d_idx in seq_along(depths)) {
    depth <- depths[d_idx]
    if (verbose) cat("\nProcessing depth", depth, "...\n")

    for (fold in 1:K) {
      cv_mse[fold, d_idx] <- icf_cv_fold(step1, fold, depth, n_trees, n_iterations)
    }
  }

  # Step 3: Select best depth and finalize
  icf_select_and_finalize(step1, cv_mse, depths, n_trees, n_iterations_final,
                          n_bootstrap, verbose)
}

#' Compute IPTW-weighted CATE for a single subgroup sample
#'
#' Helper that encapsulates the IPTW weighting logic so it can be reused
#' by both the main CATE calculation and the bootstrap loop.
#'
#' @param Y_g Outcome vector for the subgroup sample
#' @param W_g Treatment vector for the subgroup sample
#' @param ps_g Propensity scores for the subgroup sample
#' @param ps_bounds Propensity score truncation bounds
#' @return Scalar IPTW-weighted risk difference (on the original 0-1 scale)
compute_iptw_cate_single <- function(Y_g, W_g, ps_g, ps_bounds = c(0.01, 0.99)) {
  n_treated <- sum(W_g == 1)
  n_control <- sum(W_g == 0)

  if (n_treated == 0 || n_control == 0) return(NA_real_)

  # Truncate propensity scores
  ps_g <- pmax(pmin(ps_g, ps_bounds[2]), ps_bounds[1])

  # Stabilized weights
  p_treat <- mean(W_g)
  p_treat <- pmax(pmin(p_treat, 0.999), 0.001)
  weights <- ifelse(W_g == 1, p_treat / ps_g, (1 - p_treat) / (1 - ps_g))

  # Weighted means
  sum_wt <- sum(weights[W_g == 1], na.rm = TRUE)
  sum_wc <- sum(weights[W_g == 0], na.rm = TRUE)

  wrt <- if (sum_wt > 0) {
    sum(Y_g[W_g == 1] * weights[W_g == 1], na.rm = TRUE) / sum_wt
  } else {
    mean(Y_g[W_g == 1], na.rm = TRUE)
  }

  wrc <- if (sum_wc > 0) {
    sum(Y_g[W_g == 0] * weights[W_g == 0], na.rm = TRUE) / sum_wc
  } else {
    mean(Y_g[W_g == 0], na.rm = TRUE)
  }

  wrt - wrc
}

#' Calculate CATE for each subgroup using IPW
#'
#' @param Y Outcome
#' @param W Treatment
#' @param X Covariates
#' @param subgroup_id Subgroup assignments
#' @param subgroup_labels Subgroup labels
#' @param W.hat Propensity scores
#' @param ps_bounds Propensity score truncation bounds (default c(0.01, 0.99))
#' @param n_bootstrap Number of bootstrap resamples for 95% CI (0 = no CI)
#' @return Data frame with CATE estimates (and ci_lower, ci_upper if n_bootstrap > 0)
calculate_subgroup_cate <- function(Y, W, X, subgroup_id, subgroup_labels, W.hat,
                                    ps_bounds = c(0.01, 0.99), n_bootstrap = 0) {

  results <- data.frame(
    subgroup_id = integer(),
    label = character(),
    n_total = integer(),
    n_treated = integer(),
    n_control = integer(),
    events_treated = integer(),
    events_control = integer(),
    rate_treated = numeric(),
    rate_control = numeric(),
    crude_rd = numeric(),
    iptw_cate = numeric(),
    ci_lower = numeric(),
    ci_upper = numeric(),
    stringsAsFactors = FALSE
  )

  # Iterate over subgroup IDs in numeric order so that row k corresponds to
  # subgroup_id k. Using `unique(subgroup_id)` here yields IDs in order of
  # first appearance in the patient vector, which is data-dependent and led
  # to label-row misalignment in `cate$label` (e.g. the female group printed
  # with a male-edufam label) when patient row 1 was not in subgroup 1.
  unique_ids <- sort(unique(subgroup_id))

  # Validate label-ID correspondence (#24)
  if (length(unique_ids) != length(subgroup_labels)) {
    warning(paste0("Subgroup ID count (", length(unique_ids),
                   ") doesn't match label count (", length(subgroup_labels),
                   "). Results may have mismatched labels."))
  }

  for (i in seq_along(unique_ids)) {
    g <- unique_ids[i]
    idx <- subgroup_id == g
    Y_g <- Y[idx]
    W_g <- W[idx]
    ps_g <- W.hat[idx]

    n_total <- sum(idx)
    n_treated <- sum(W_g == 1)
    n_control <- sum(W_g == 0)

    # Warn if subgroup has zero treated or control observations
    if (n_treated == 0) {
      warning(paste("Subgroup", g, "has zero treated observations - CATE will be NA"))
    }
    if (n_control == 0) {
      warning(paste("Subgroup", g, "has zero control observations - CATE will be NA"))
    }

    events_treated <- sum(Y_g[W_g == 1])
    events_control <- sum(Y_g[W_g == 0])

    rate_treated <- if (n_treated > 0) mean(Y_g[W_g == 1]) else NA
    rate_control <- if (n_control > 0) mean(Y_g[W_g == 0]) else NA

    crude_rd <- rate_treated - rate_control

    # IPTW point estimate using helper
    iptw_cate <- compute_iptw_cate_single(Y_g, W_g, ps_g, ps_bounds)

    # Warn about extreme weights (diagnostic, using full sample)
    ps_trunc <- pmax(pmin(ps_g, ps_bounds[2]), ps_bounds[1])
    p_treat <- pmax(pmin(mean(W_g), 0.999), 0.001)
    weights <- ifelse(W_g == 1, p_treat / ps_trunc, (1 - p_treat) / (1 - ps_trunc))
    max_weight <- max(weights, na.rm = TRUE)
    min_weight <- min(weights, na.rm = TRUE)
    if (max_weight > 20) {
      warning(paste0("Subgroup ", g, " has extreme IPTW weights (max=",
                     round(max_weight, 1), "). CATE estimate may be unstable."))
    }
    if (min_weight < 0.05) {
      warning(paste0("Subgroup ", g, " has very small IPTW weights (min=",
                     round(min_weight, 3), "). Some observations effectively ignored."))
    }

    # Bootstrap CI
    ci_lower <- NA_real_
    ci_upper <- NA_real_
    if (n_bootstrap > 0 && n_treated > 0 && n_control > 0) {
      sg_indices <- which(idx)
      boot_cates <- replicate(n_bootstrap, {
        boot_idx <- sample(sg_indices, length(sg_indices), replace = TRUE)
        compute_iptw_cate_single(Y[boot_idx], W[boot_idx], W.hat[boot_idx], ps_bounds)
      })
      boot_cates <- boot_cates[!is.na(boot_cates)]
      if (length(boot_cates) >= 10) {
        ci_lower <- round(quantile(boot_cates, 0.025) * 100, 2)
        ci_upper <- round(quantile(boot_cates, 0.975) * 100, 2)
      }
    }

    # Index labels by g (subgroup_id), not by i. With unique_ids sorted
    # ascending these coincide, but indexing by g is robust to any future
    # change in iteration order.
    results <- rbind(results, data.frame(
      subgroup_id = g,
      label = subgroup_labels[g],
      n_total = n_total,
      n_treated = n_treated,
      n_control = n_control,
      events_treated = events_treated,
      events_control = events_control,
      rate_treated = round(rate_treated * 100, 2),
      rate_control = round(rate_control * 100, 2),
      crude_rd = round(crude_rd * 100, 2),
      iptw_cate = round(iptw_cate * 100, 4),
      ci_lower = ci_lower,
      ci_upper = ci_upper,
      stringsAsFactors = FALSE
    ))
  }

  return(results)
}

# icf/core.R
# Core functions for iterative Causal Forest (iCF)
#
# Re-implementation based on:
# Wang et al. (2024) "Iterative Causal Forest: A Novel Algorithm for
# Subgroup Identification" Am J Epidemiol. 193(5):764-776
#
# This is an independent implementation using the grf package.

library(grf)

#' Find the best tree in a causal forest
#'
#' Selects the tree with the smallest root-node impurity (R-loss).
#' The R-loss measures how well a tree captures treatment effect heterogeneity.
#'
#' @param cf A causal_forest object from grf
#' @return List with best_tree index and the tree object
find_best_tree <- function(cf) {
  n_trees <- cf$`_num_trees`

  # Get out-of-bag predictions for each tree
  # We want the tree whose predictions best match the overall forest predictions
  forest_pred <- predict(cf)$predictions

  best_tree_idx <- 1
  best_loss <- Inf

  # Track failures for diagnostics
  n_degenerate <- 0
  n_pred_failed <- 0

  for (i in 1:n_trees) {
    tree <- get_tree(cf, i)

    # Skip degenerate trees (no splits)
    if (is.null(tree$nodes) || length(tree$nodes) < 2) {
      n_degenerate <- n_degenerate + 1
      next
    }

    # Calculate R-loss for this tree
    # R-loss = sum of squared differences between tree predictions and target
    tree_pred <- predict_single_tree(cf, i)

    if (!is.null(tree_pred)) {
      # Check for high NA rate in predictions
      na_rate <- mean(is.na(tree_pred))
      if (na_rate > 0.05) {
        warning(paste0("Tree ", i, " has ", round(na_rate * 100, 1),
                       "% NA predictions - loss may be unreliable"))
      }

      loss <- mean((tree_pred - forest_pred)^2, na.rm = TRUE)

      if (loss < best_loss) {
        best_loss <- loss
        best_tree_idx <- i
      }
    } else {
      n_pred_failed <- n_pred_failed + 1
    }
  }

  # Report failures
  n_valid <- n_trees - n_degenerate - n_pred_failed
  if (n_degenerate > 0 || n_pred_failed > 0) {
    pct_failed <- round(100 * (n_degenerate + n_pred_failed) / n_trees, 1)
    if (pct_failed > 50) {
      warning(paste0("High tree failure rate: ", n_degenerate, " degenerate, ",
                     n_pred_failed, " prediction failures out of ", n_trees,
                     " trees (", pct_failed, "%)"))
    }
  }

  # Check if we found any valid tree
  if (is.infinite(best_loss)) {
    warning("All trees failed - returning first tree which may be degenerate")
  }

  best_tree <- get_tree(cf, best_tree_idx)

  return(list(
    best_tree = best_tree_idx,
    tree = best_tree,
    loss = best_loss,
    n_valid = n_valid,
    n_degenerate = n_degenerate,
    n_pred_failed = n_pred_failed
  ))
}

#' Get predictions from a single tree
#'
#' @param cf A causal_forest object
#' @param tree_idx Index of the tree
#' @return Vector of predictions
predict_single_tree <- function(cf, tree_idx) {
  tree <- get_tree(cf, tree_idx)

  if (is.null(tree$nodes)) {
    return(NULL)
  }

  # Validate X.orig exists (#44)
  if (is.null(cf$X.orig)) {
    warning("causal_forest object missing X.orig - cannot make predictions")
    return(NULL)
  }

  # Get the leaf assignments for each observation
  # This uses the tree structure to assign observations to leaves
  n_obs <- nrow(cf$X.orig)
  predictions <- rep(NA, n_obs)

  # Traverse tree for each observation
  # Maximum iterations to prevent infinite loops from malformed trees
  # 100 is sufficient for trees up to depth ~7 (2^7 = 128 nodes max path)
  # which exceeds typical iCF depths of 2-5
  max_iterations <- 100

  for (i in 1:n_obs) {
    x <- cf$X.orig[i, ]
    node_idx <- 1  # Start at root
    iterations <- 0

    while (iterations < max_iterations) {
      iterations <- iterations + 1

      # Bounds check: node_idx must be valid
      if (node_idx < 1 || node_idx > length(tree$nodes)) {
        predictions[i] <- NA
        break
      }

      node <- tree$nodes[[node_idx]]

      if (node$is_leaf) {
        # Return the treatment effect estimate at this leaf
        predictions[i] <- node$leaf_stats[1]  # CATE estimate
        break
      }

      # Get split info
      split_var <- node$split_variable
      split_val <- node$split_value

      # Validate split_var is a valid column index
      if (!is.numeric(split_var) || split_var < 1 || split_var > length(x)) {
        predictions[i] <- NA
        break
      }

      # Go left or right
      if (x[split_var] <= split_val) {
        node_idx <- node$left_child
      } else {
        node_idx <- node$right_child
      }
    }

    # If we hit max iterations, something is wrong with the tree
    if (iterations >= max_iterations) {
      predictions[i] <- NA
      warning(paste("Max iterations reached for observation", i, "- possible malformed tree"))
    }
  }

  return(predictions)
}

#' Extract tree structure (splitting variables and pattern)
#'
#' Extracts a simplified representation of the tree structure for comparison.
#' Ignores splitting values, focusing only on which variables are used at each level.
#'
#' @param tree A tree object from get_tree()
#' @param var_names Names of variables
#' @return A character string representing the tree structure
extract_tree_structure <- function(tree, var_names) {
  if (is.null(tree$nodes) || length(tree$nodes) < 2) {
    return("NO_SPLIT")
  }

  # Build structure string by traversing tree
  structure_parts <- c()

  traverse_node <- function(node_idx, depth) {
    if (node_idx > length(tree$nodes)) return()

    node <- tree$nodes[[node_idx]]

    if (!node$is_leaf) {
      var_idx <- node$split_variable
      var_name <- if (var_idx <= length(var_names)) var_names[var_idx] else paste0("V", var_idx)
      structure_parts <<- c(structure_parts, paste0("D", depth, ":", var_name))

      traverse_node(node$left_child, depth + 1)
      traverse_node(node$right_child, depth + 1)
    }
  }

  traverse_node(1, 1)

  if (length(structure_parts) == 0) {
    return("NO_SPLIT")
  }

  return(paste(sort(structure_parts), collapse = "|"))
}

#' Extract detailed tree splits for subgroup definition
#'
#' @param tree A tree object from get_tree()
#' @param var_names Names of variables
#' @return Data frame with split information
extract_tree_splits <- function(tree, var_names) {
  if (is.null(tree$nodes) || length(tree$nodes) < 2) {
    return(data.frame(
      node_id = integer(),
      depth = integer(),
      variable = character(),
      split_value = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  splits <- list()

  traverse_node <- function(node_idx, depth) {
    if (node_idx > length(tree$nodes)) return()

    node <- tree$nodes[[node_idx]]

    if (!node$is_leaf) {
      var_idx <- node$split_variable
      var_name <- if (var_idx <= length(var_names)) var_names[var_idx] else paste0("V", var_idx)

      splits[[length(splits) + 1]] <<- data.frame(
        node_id = node_idx,
        depth = depth,
        variable = var_name,
        var_idx = var_idx,
        split_value = node$split_value,
        left_child = node$left_child,
        right_child = node$right_child,
        stringsAsFactors = FALSE
      )

      traverse_node(node$left_child, depth + 1)
      traverse_node(node$right_child, depth + 1)
    }
  }

  traverse_node(1, 1)

  if (length(splits) == 0) {
    return(data.frame(
      node_id = integer(),
      depth = integer(),
      variable = character(),
      split_value = numeric(),
      stringsAsFactors = FALSE
    ))
  }

  return(do.call(rbind, splits))
}

#' Get tree depth
#'
#' @param tree A tree object from get_tree()
#' @return Maximum depth of the tree
get_tree_depth <- function(tree) {
  if (is.null(tree$nodes) || length(tree$nodes) < 2) {
    return(0)
  }

  max_depth <- 0

  get_depth <- function(node_idx, depth) {
    if (node_idx > length(tree$nodes)) return()

    node <- tree$nodes[[node_idx]]

    if (node$is_leaf) {
      max_depth <<- max(max_depth, depth)
    } else {
      get_depth(node$left_child, depth + 1)
      get_depth(node$right_child, depth + 1)
    }
  }

  get_depth(1, 1)
  return(max_depth)
}

#' Plurality vote across tree structures
#'
#' Selects the most common tree structure from a collection of trees.
#'
#' @param structures Vector of tree structure strings
#' @return The most common structure, plus the full vote distribution and the
#'   root-split-variable distribution (useful for stability diagnostics).
plurality_vote_structure <- function(structures) {
  # Remove NA and empty structures
  structures <- structures[!is.na(structures) & structures != "NO_SPLIT"]

  if (length(structures) == 0) {
    return(NULL)
  }

  # Count occurrences
  counts <- sort(table(structures), decreasing = TRUE)
  most_common <- names(counts)[1]
  n_total <- length(structures)

  # Full ranked distribution as a data frame for diagnostics
  vote_distribution <- data.frame(
    structure = names(counts),
    count = as.integer(counts),
    frequency = as.numeric(counts) / n_total,
    stringsAsFactors = FALSE
  )

  # Root-split variable: the variable that appears in the "D1:" segment of
  # each structure string. Structures look like "D1:female|D2:edufam_cat";
  # an empty / single-leaf tree has no D1 segment.
  root_var <- sub("^D1:([^|]+).*$", "\\1", structures)
  root_var[!grepl("^D1:", structures)] <- NA_character_
  root_var <- root_var[!is.na(root_var)]
  if (length(root_var) > 0) {
    rc <- sort(table(root_var), decreasing = TRUE)
    root_split_distribution <- data.frame(
      variable = names(rc),
      count = as.integer(rc),
      frequency = as.numeric(rc) / length(root_var),
      stringsAsFactors = FALSE
    )
  } else {
    root_split_distribution <- data.frame(
      variable = character(0), count = integer(0), frequency = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  return(list(
    structure = most_common,
    count = as.integer(counts[1]),
    total = n_total,
    frequency = as.numeric(counts[1]) / n_total,
    vote_distribution = vote_distribution,
    root_split_distribution = root_split_distribution
  ))
}

#' Verify that subgroup labels match the actual partition
#'
#' For each leaf with id g, parses the path string in subgroup_labels[g] back
#' into a sequence of "var <= val" / "var > val" predicates and verifies that
#' the set of patients matching all those predicates equals the set with
#' subgroup_id == g. Returns a list with overall_ok and a per-leaf breakdown
#' suitable for printing. Cheap (O(n_leaves * n_obs)).
#'
#' Use after assign_subgroups() / calculate_subgroup_cate() to catch any
#' regression of the label/row-alignment bug fixed on 2026-05-10.
verify_subgroup_labels <- function(X, subgroup_id, subgroup_labels, var_names) {
  X <- as.data.frame(X)
  n <- nrow(X)
  results <- vector("list", length(subgroup_labels))

  # Variable names can themselves contain underscores (edufam_cat, diag_sud, …),
  # so we can't split the label on "_". Instead, anchor on var_names sorted by
  # length descending and parse predicates with a single regex.
  vn_sorted <- var_names[order(nchar(var_names), decreasing = TRUE)]
  # Variable names in this codebase only use letters, digits, "_", and ".".
  # Escape "." so it matches a literal dot rather than any character.
  vn_escaped <- vapply(vn_sorted, function(v) gsub(".", "\\.", v, fixed = TRUE), character(1))
  vn_alt <- paste(vn_escaped, collapse = "|")
  predicate_re <- paste0("(", vn_alt, ")(<=|>)(-?[0-9]+(?:\\.[0-9]+)?)")

  for (g in seq_along(subgroup_labels)) {
    label <- subgroup_labels[g]
    expected_idx <- which(subgroup_id == g)

    body <- sub("^SG_?", "", label)
    if (body == "" || body == "all") {
      predicate_idx <- seq_len(n)
    } else {
      m_list <- gregexpr(predicate_re, body, perl = TRUE)
      starts <- as.integer(m_list[[1]])
      lengths <- attr(m_list[[1]], "match.length")
      if (length(starts) == 1L && starts == -1L) {
        predicate_idx <- NULL
      } else {
        # Verify the regex matches cover the body, separated only by single "_"
        # characters: length(body) == sum(match.length) + (n_matches - 1)
        if (sum(lengths) + (length(starts) - 1L) == nchar(body)) {
          tokens <- regmatches(body, m_list)[[1]]
          parsed <- regmatches(tokens, regexec(predicate_re, tokens, perl = TRUE))
          mask <- rep(TRUE, n)
          for (p in parsed) {
            if (length(p) != 4 || !(p[2] %in% var_names)) { mask <- NULL; break }
            v <- X[[p[2]]]
            val <- as.numeric(p[4])
            mask <- mask & if (p[3] == "<=") v <= val else v > val
          }
          predicate_idx <- if (is.null(mask)) NULL else which(mask)
        } else {
          predicate_idx <- NULL
        }
      }
    }

    if (is.null(predicate_idx)) {
      results[[g]] <- list(ok = NA, label = label, n_expected = length(expected_idx),
                           n_predicate = NA_integer_, note = "could not parse label")
    } else {
      ok <- length(predicate_idx) == length(expected_idx) &&
            all(sort(predicate_idx) == sort(expected_idx))
      results[[g]] <- list(ok = ok, label = label,
                           n_expected = length(expected_idx),
                           n_predicate = length(predicate_idx),
                           note = if (ok) "match" else "MISMATCH")
    }
  }

  list(
    overall_ok = all(sapply(results, function(r) isTRUE(r$ok))),
    per_leaf = results
  )
}

#' Build synthetic tree with mean split values
#'
#' Given trees with the same structure, compute mean split values.
#'
#' @param trees List of tree objects with same structure
#' @param var_names Variable names
#' @return Data frame defining the synthetic tree splits
build_synthetic_tree <- function(trees, var_names) {
  # Extract splits from all trees
  all_splits <- lapply(trees, function(t) extract_tree_splits(t, var_names))

  # Filter to non-empty
  all_splits <- all_splits[sapply(all_splits, nrow) > 0]

  if (length(all_splits) == 0) {
    return(NULL)
  }

  # Get reference structure from first tree
  ref_splits <- all_splits[[1]]

  if (nrow(ref_splits) == 0) {
    return(NULL)
  }

  # For each split position, compute mean split value
  synthetic_splits <- ref_splits

  for (i in 1:nrow(synthetic_splits)) {
    nid_i <- synthetic_splits$node_id[i]

    # Collect split values from all trees at this position, matched by node_id
    # to avoid ambiguity when the same variable appears at the same depth on
    # different branches (e.g., both children split on the same variable)
    split_vals <- sapply(all_splits, function(s) {
      match_row <- s[s$node_id == nid_i, ]
      if (nrow(match_row) > 0) match_row$split_value[1] else NA
    })

    # Check for floating point precision issues (#49)
    valid_vals <- split_vals[!is.na(split_vals)]
    if (length(valid_vals) > 1) {
      val_range <- diff(range(valid_vals))
      val_mean <- mean(valid_vals)
      # Warn if relative range is large (suggests different splits, not precision)
      if (val_mean != 0 && val_range / abs(val_mean) > 0.1) {
        warning(paste0("Split values for node ", nid_i,
                       " vary significantly (range=", round(val_range, 4),
                       "). Trees may have different structures."))
      }
    }

    # For ordinal/categorical splits (few distinct threshold values), use mode
    # instead of mean to avoid fractional thresholds between categories (#5).
    n_unique <- length(unique(round(valid_vals, 6)))
    if (length(valid_vals) > 0 && n_unique <= 5) {
      # Modal threshold: most common value
      val_table <- table(round(valid_vals, 6))
      synthetic_splits$split_value[i] <- as.numeric(names(which.max(val_table)))
    } else {
      synthetic_splits$split_value[i] <- mean(split_vals, na.rm = TRUE)
    }
  }

  return(synthetic_splits)
}

#' Assign observations to subgroups based on tree splits
#'
#' @param X Covariate matrix
#' @param splits Data frame of tree splits (from build_synthetic_tree)
#' @param var_names Variable names
#' @return Vector of subgroup assignments
assign_subgroups <- function(X, splits, var_names) {
  if (is.null(splits) || nrow(splits) == 0) {
    # Return a list with consistent structure (all observations in one subgroup)
    return(list(
      subgroup_id = rep(1L, nrow(X)),
      subgroup_labels = "SG_all",
      n_subgroups = 1L
    ))
  }

  n_obs <- nrow(X)
  subgroup <- rep("root", n_obs)
  # Track leaf labels in tree-traversal order (left-first DFS) so that the
  # subgroup_id assigned downstream is 1 = leftmost leaf, 2 = next, etc.
  # Using unique(subgroup) instead would order by first-occurrence in the
  # patient vector, which depends on patient row ordering in X rather than
  # on tree position and produces counter-intuitive leaf numbering in
  # rendered decision trees.
  traversal_order <- character(0)

  # Build assignment recursively
  assign_node <- function(obs_idx, node_id, path) {
    # Find split for this node
    node_split <- splits[splits$node_id == node_id, ]

    if (nrow(node_split) == 0) {
      # This is a leaf - assign the path as subgroup
      subgroup[obs_idx] <<- path
      traversal_order <<- c(traversal_order, path)
      return()
    }

    var_name <- node_split$variable[1]
    split_val <- node_split$split_value[1]
    var_idx <- which(var_names == var_name)

    # Validate var_idx: must be exactly one match
    if (length(var_idx) == 0) {
      warning(paste("Variable", var_name, "not found in var_names"))
      subgroup[obs_idx] <<- path
      traversal_order <<- c(traversal_order, path)
      return()
    }
    if (length(var_idx) > 1) {
      warning(paste("Variable", var_name, "has multiple matches, using first"))
      var_idx <- var_idx[1]
    }

    # Split observations
    x_vals <- X[obs_idx, var_idx, drop = TRUE]
    left_obs <- obs_idx[x_vals <= split_val]
    right_obs <- obs_idx[x_vals > split_val]

    # Recurse
    if (length(left_obs) > 0) {
      left_path <- paste0(path, "_", var_name, "<=", round(split_val, 2))
      left_child <- node_split$left_child[1]
      assign_node(left_obs, left_child, left_path)
    }

    if (length(right_obs) > 0) {
      right_path <- paste0(path, "_", var_name, ">", round(split_val, 2))
      right_child <- node_split$right_child[1]
      assign_node(right_obs, right_child, right_path)
    }
  }

  assign_node(1:n_obs, 1, "SG")

  # Convert to numeric IDs in tree-traversal (left-to-right) order rather
  # than first-occurrence-in-patient-vector order.
  subgroup_id <- match(subgroup, traversal_order)

  return(list(
    subgroup_id = subgroup_id,
    subgroup_labels = traversal_order,
    n_subgroups = length(traversal_order)
  ))
}

#' Calculate transformed outcome
#'
#' Y* = Y/PS if W=1, -Y/(1-PS) if W=0
#' This is an unbiased estimator of the CATE.
#'
#' @param Y Outcome vector
#' @param W Treatment indicator
#' @param ps Propensity scores
#' @param truncate_ps Whether to truncate extreme propensity scores
#' @param truncate_quantile Quantile for truncation
#' @param min_ps Minimum propensity score (hard floor to prevent division by zero)
#' @return Vector of transformed outcomes
calculate_transformed_outcome <- function(Y, W, ps, truncate_ps = TRUE,
                                          truncate_quantile = 0.01,
                                          min_ps = 0.001) {
  # Truncate propensity scores to avoid extreme weights
  if (truncate_ps) {
    lower <- quantile(ps, truncate_quantile, na.rm = TRUE)
    upper <- quantile(ps, 1 - truncate_quantile, na.rm = TRUE)
    # Report truncation (#46)
    n_truncated <- sum(ps < lower | ps > upper, na.rm = TRUE)
    if (n_truncated > 0) {
      pct_truncated <- round(100 * n_truncated / sum(!is.na(ps)), 1)
      if (pct_truncated > 5) {
        warning(paste0("Truncated ", n_truncated, " propensity scores (",
                       pct_truncated, "%) at quantile bounds [",
                       round(lower, 4), ", ", round(upper, 4), "]"))
      }
    }
    ps <- pmax(pmin(ps, upper), lower)
  }

  # Apply hard floor/ceiling to prevent division by zero
  # Even after quantile truncation, bounds could be at 0 or 1
  ps <- pmax(pmin(ps, 1 - min_ps), min_ps)

  # Calculate transformed outcome
  Y_star <- ifelse(W == 1, Y / ps, -Y / (1 - ps))

  # Check for problematic values and warn

  n_inf <- sum(is.infinite(Y_star), na.rm = TRUE)
  n_nan <- sum(is.nan(Y_star), na.rm = TRUE)
  n_na <- sum(is.na(Y_star)) - n_nan

  if (n_inf > 0 || n_nan > 0) {
    warning(paste0("Transformed outcome has ", n_inf, " Inf and ", n_nan,
                   " NaN values. This may affect model fitting."))
  }
  if (n_na > 0) {
    warning(paste0("Transformed outcome has ", n_na,
                   " NA values from missing propensity scores."))
  }

  return(Y_star)
}

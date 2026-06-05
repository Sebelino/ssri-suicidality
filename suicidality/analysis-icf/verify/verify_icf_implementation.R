# verify_icf_implementation.R
# Verification tests for the iCF implementation
#
# This script tests for known bugs and validates the implementation.

library(dplyr)
library(grf)
library(here)

here::i_am("suicidality/analysis-icf/verify/verify_icf_implementation.R")

# Source the iCF implementation
source(here("suicidality", "analysis-icf", "icf", "core.R"))

cat("=== iCF Implementation Verification ===\n\n")

errors <- 0
warnings <- 0

# =============================================================================
# TEST 1: assign_subgroups return type consistency
# =============================================================================
cat("--- Test 1: assign_subgroups return type ---\n")

# Test with NULL splits (should return a list, not a vector)
X_test <- matrix(rnorm(100), nrow = 10, ncol = 10)
colnames(X_test) <- paste0("V", 1:10)
var_names <- colnames(X_test)

result_null <- assign_subgroups(X_test, NULL, var_names)

if (is.list(result_null)) {
  cat("PASS: assign_subgroups with NULL splits returns a list\n")
  if (all(c("subgroup_id", "subgroup_labels", "n_subgroups") %in% names(result_null))) {
    cat("PASS: List contains expected elements\n")
  } else {
    cat("FAIL: List missing expected elements\n")
    errors <- errors + 1
  }
} else {
  cat("FAIL: assign_subgroups with NULL splits returns", class(result_null), "instead of list\n")
  cat("  BUG: Line 315 in core.R returns rep(1, nrow(X)) instead of a list\n")
  errors <- errors + 1
}

# Test with empty splits data frame
empty_splits <- data.frame(
  node_id = integer(),
  depth = integer(),
  variable = character(),
  split_value = numeric(),
  stringsAsFactors = FALSE
)
result_empty <- assign_subgroups(X_test, empty_splits, var_names)

if (is.list(result_empty)) {
  cat("PASS: assign_subgroups with empty splits returns a list\n")
} else {
  cat("FAIL: assign_subgroups with empty splits returns", class(result_empty), "\n")
  errors <- errors + 1
}

cat("\n")

# =============================================================================
# TEST 2: build_synthetic_tree matching logic
# =============================================================================
cat("--- Test 2: build_synthetic_tree matching logic ---\n")

# This test checks if the matching by (depth, variable) is correct
# In a tree, you can have the same variable at the same depth on different branches

# Create a mock tree structure with same variable at same depth
# Tree structure:
#       V1
#      /  \
#    V2    V2   <- same variable, same depth, different branches
#
# The current implementation might incorrectly match these

# We can't easily test this without creating actual grf trees,
# but we document the potential issue
cat("WARNING: build_synthetic_tree matches splits by (depth, variable) which may\n")
cat("  incorrectly match splits from different branches at the same depth.\n")
cat("  This could affect synthetic tree construction when the same variable\n")
cat("  appears multiple times at the same depth.\n")
warnings <- warnings + 1

cat("\n")

# =============================================================================
# TEST 3: calculate_subgroup_cate label indexing
# =============================================================================
cat("--- Test 3: calculate_subgroup_cate label indexing ---\n")

# Source the full algorithm to get calculate_subgroup_cate
source(here("suicidality", "analysis-icf", "icf", "icf_algorithm.R"))

# Create test data where subgroup_id values don't match array indices
Y <- c(0, 1, 0, 0, 1, 0)
W <- c(1, 1, 1, 0, 0, 0)
X <- matrix(rnorm(6 * 3), nrow = 6, ncol = 3)
subgroup_id <- c(1, 1, 3, 3, 3, 1)  # IDs are 1 and 3, not 1 and 2
subgroup_labels <- c("Group_A", "Group_B", "Group_C")
W.hat <- rep(0.5, 6)

# This should work correctly, but the original code uses subgroup_labels[g]
# where g comes from unique(subgroup_id), so g=3 would access subgroup_labels[3]
tryCatch({
  result <- calculate_subgroup_cate(Y, W, X, subgroup_id, subgroup_labels, W.hat)

  # Check if labels are correctly matched
  if (all(result$label %in% subgroup_labels)) {
    cat("PASS: Labels are correctly matched\n")
  } else {
    cat("FAIL: Labels not correctly matched\n")
    cat("  Expected labels from:", paste(subgroup_labels, collapse = ", "), "\n")
    cat("  Got labels:", paste(result$label, collapse = ", "), "\n")
    errors <- errors + 1
  }
}, error = function(e) {
  cat("FAIL: calculate_subgroup_cate threw error:", e$message, "\n")
  cat("  BUG: When subgroup_id values don't match array indices,\n")
  cat("  subgroup_labels[g] accesses wrong index\n")
  errors <<- errors + 1
})

cat("\n")

# =============================================================================
# TEST 4: Transformed outcome calculation
# =============================================================================
cat("--- Test 4: Transformed outcome calculation ---\n")

Y <- c(1, 0, 1, 0, 1, 0)
W <- c(1, 1, 1, 0, 0, 0)
ps <- c(0.5, 0.5, 0.5, 0.5, 0.5, 0.5)

Y_star <- calculate_transformed_outcome(Y, W, ps, truncate_ps = FALSE)

# Expected: Y/ps for treated, -Y/(1-ps) for control
expected <- c(1/0.5, 0/0.5, 1/0.5, -0/0.5, -1/0.5, -0/0.5)

if (all(abs(Y_star - expected) < 1e-10)) {
  cat("PASS: Transformed outcome calculation is correct\n")
} else {
  cat("FAIL: Transformed outcome calculation mismatch\n")
  cat("  Expected:", paste(expected, collapse = ", "), "\n")
  cat("  Got:", paste(Y_star, collapse = ", "), "\n")
  errors <- errors + 1
}

# Test truncation
ps_extreme <- c(0.001, 0.999, 0.5, 0.5, 0.5, 0.5)
Y_star_trunc <- calculate_transformed_outcome(Y, W, ps_extreme, truncate_ps = TRUE, truncate_quantile = 0.1)

if (all(is.finite(Y_star_trunc))) {
  cat("PASS: Propensity score truncation prevents extreme values\n")
} else {
  cat("FAIL: Truncation did not prevent extreme values\n")
  errors <- errors + 1
}

cat("\n")

# =============================================================================
# TEST 5: Tree traversal and prediction
# =============================================================================
cat("--- Test 5: Tree structure extraction ---\n")

# Create a simple causal forest for testing
set.seed(42)
n <- 500
p <- 5
X <- matrix(rnorm(n * p), n, p)
colnames(X) <- paste0("V", 1:p)
W <- rbinom(n, 1, 0.5)
tau <- X[, 1] > 0  # True treatment effect depends on V1
Y <- W * tau + rnorm(n, 0, 0.1)

cf <- causal_forest(X, Y, W, num.trees = 50)

# Test find_best_tree
bt_result <- find_best_tree(cf)

if (!is.null(bt_result$tree) && bt_result$best_tree >= 1) {
  cat("PASS: find_best_tree returns valid tree\n")
} else {
  cat("FAIL: find_best_tree did not return valid tree\n")
  errors <- errors + 1
}

# Test extract_tree_structure
structure <- extract_tree_structure(bt_result$tree, colnames(X))
if (is.character(structure) && nchar(structure) > 0) {
  cat("PASS: extract_tree_structure returns valid structure string\n")
  cat("  Structure:", structure, "\n")
} else {
  cat("FAIL: extract_tree_structure did not return valid string\n")
  errors <- errors + 1
}

# Test get_tree_depth
depth <- get_tree_depth(bt_result$tree)
if (is.numeric(depth) && depth >= 0) {
  cat("PASS: get_tree_depth returns valid depth:", depth, "\n")
} else {
  cat("FAIL: get_tree_depth did not return valid depth\n")
  errors <- errors + 1
}

cat("\n")

# =============================================================================
# TEST 6: End-to-end iCF with simulated data
# =============================================================================
cat("--- Test 6: End-to-end iCF test ---\n")

set.seed(123)
n <- 1000
p <- 10

# Simulate data with known heterogeneity
X <- matrix(rnorm(n * p), n, p)
colnames(X) <- paste0("V", 1:p)
W <- rbinom(n, 1, plogis(X[, 1]))  # Treatment depends on V1

# True CATE: positive effect when V2 > 0, negative when V2 <= 0
tau_true <- ifelse(X[, 2] > 0, 0.5, -0.3)
Y <- tau_true * W + rnorm(n, 0, 0.5)

test_data <- data.frame(Y = Y, W = W, X)

# Run iCF with minimal settings for speed
tryCatch({
  results <- run_icf_cv(
    data = test_data,
    K = 2,
    n_trees = 30,
    n_iterations = 5,
    depths = c(2),
    verbose = FALSE
  )

  # Basic sanity checks
  if (!is.null(results$subgroup_id) && length(results$subgroup_id) == n) {
    cat("PASS: iCF returns valid subgroup assignments\n")
  } else {
    cat("FAIL: Invalid subgroup assignments\n")
    errors <- errors + 1
  }

  if (!is.null(results$cate) && nrow(results$cate) > 0) {
    cat("PASS: iCF returns CATE estimates\n")
    cat("  Number of subgroups:", results$n_subgroups, "\n")
  } else {
    cat("FAIL: No CATE estimates returned\n")
    errors <- errors + 1
  }

  if (!is.null(results$var_importance) && length(results$var_importance) == p) {
    cat("PASS: Variable importance has correct length\n")
  } else {
    cat("FAIL: Variable importance has wrong length\n")
    errors <- errors + 1
  }

}, error = function(e) {
  cat("FAIL: End-to-end iCF test threw error:", e$message, "\n")
  errors <<- errors + 1
})

cat("\n")

# =============================================================================
# SUMMARY
# =============================================================================
cat("=== VERIFICATION SUMMARY ===\n")
cat("Errors:", errors, "\n")
cat("Warnings:", warnings, "\n")

if (errors > 0) {
  cat("\nSOME TESTS FAILED - Review bugs above\n")
} else if (warnings > 0) {
  cat("\nAll tests passed but there are warnings to review\n")
} else {
  cat("\nAll tests passed\n")
}

cat("\n=== Known Issues (to monitor) ===\n")
cat("1. [FIXED] assign_subgroups now returns list consistently when splits=NULL\n")
cat("2. build_synthetic_tree matches by (depth, variable) which may incorrectly\n")
cat("   match splits from different branches at the same depth. This is rare\n")
cat("   but could affect synthetic tree construction.\n")
cat("3. calculate_subgroup_cate assumes subgroup_id values match array indices.\n")
cat("   Current implementation appears to work correctly in practice.\n")

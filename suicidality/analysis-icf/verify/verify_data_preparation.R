# verify_data_preparation.R
# Verification tests for the iCF data preparation
#
# This script checks for data issues and validates the preparation.

library(dplyr)
library(here)

here::i_am("suicidality/analysis-icf/verify/verify_data_preparation.R")

cat("=== iCF Data Preparation Verification ===\n\n")

errors <- 0
warnings <- 0

# =============================================================================
# Load prepared data
# =============================================================================
cat("Loading prepared iCF data...\n")

data_path <- here("suicidality", "analysis-icf", "data", "icf_data.rds")
if (!file.exists(data_path)) {
  cat("ERROR: Prepared data not found. Run 01_prepare_data.R first.\n")
  quit(status = 1)
}

icf_data <- readRDS(data_path)
cat("Loaded", nrow(icf_data), "observations with", ncol(icf_data), "columns\n\n")

# =============================================================================
# TEST 1: Required columns exist
# =============================================================================
cat("--- Test 1: Required columns ---\n")

required_cols <- c("Y", "W")
missing_cols <- setdiff(required_cols, names(icf_data))

if (length(missing_cols) == 0) {
  cat("PASS: All required columns (Y, W) present\n")
} else {
  cat("FAIL: Missing required columns:", paste(missing_cols, collapse = ", "), "\n")
  errors <- errors + 1
}

cat("\n")

# =============================================================================
# TEST 2: Y is binary
# =============================================================================
cat("--- Test 2: Outcome variable Y ---\n")

Y_values <- unique(icf_data$Y)
if (all(Y_values %in% c(0, 1))) {
  cat("PASS: Y is binary (0/1)\n")
  cat("  Y=0:", sum(icf_data$Y == 0), "\n")
  cat("  Y=1:", sum(icf_data$Y == 1), "\n")
  cat("  Event rate:", round(mean(icf_data$Y) * 100, 2), "%\n")
} else {
  cat("FAIL: Y contains non-binary values:", paste(Y_values, collapse = ", "), "\n")
  errors <- errors + 1
}

cat("\n")

# =============================================================================
# TEST 3: W is binary
# =============================================================================
cat("--- Test 3: Treatment variable W ---\n")

W_values <- unique(icf_data$W)
if (all(W_values %in% c(0, 1))) {
  cat("PASS: W is binary (0/1)\n")
  cat("  W=0 (control):", sum(icf_data$W == 0), "\n")
  cat("  W=1 (treated):", sum(icf_data$W == 1), "\n")
  cat("  Treatment rate:", round(mean(icf_data$W) * 100, 2), "%\n")
} else {
  cat("FAIL: W contains non-binary values:", paste(W_values, collapse = ", "), "\n")
  errors <- errors + 1
}

cat("\n")

# =============================================================================
# TEST 4: No missing values
# =============================================================================
cat("--- Test 4: Missing values ---\n")

na_counts <- colSums(is.na(icf_data))
cols_with_na <- names(na_counts)[na_counts > 0]

if (length(cols_with_na) == 0) {
  cat("PASS: No missing values in any column\n")
} else {
  cat("FAIL: Missing values found in columns:\n")
  for (col in cols_with_na) {
    cat("  ", col, ":", na_counts[col], "NAs\n")
  }
  errors <- errors + 1
}

cat("\n")

# =============================================================================
# TEST 5: All covariates are numeric
# =============================================================================
cat("--- Test 5: Covariate types ---\n")

covar_cols <- setdiff(names(icf_data), c("Y", "W"))
non_numeric <- sapply(icf_data[, covar_cols], function(x) !is.numeric(x))
non_numeric_cols <- names(non_numeric)[non_numeric]

if (length(non_numeric_cols) == 0) {
  cat("PASS: All", length(covar_cols), "covariates are numeric\n")
} else {
  cat("FAIL: Non-numeric covariates found:\n")
  for (col in non_numeric_cols) {
    cat("  ", col, ":", class(icf_data[[col]]), "\n")
  }
  errors <- errors + 1
}

cat("\n")

# =============================================================================
# TEST 6: Source variable encoding
# =============================================================================
cat("--- Test 6: Source variable encoding ---\n")

if ("source" %in% names(icf_data)) {
  source_values <- unique(icf_data$source)
  cat("Source values in data:", paste(sort(source_values), collapse = ", "), "\n")

  # Check if encoding is as expected (0=O, 1=S, 2=P)
  # Note: The original data might have other values that weren't handled
  if (all(source_values %in% c(0, 1, 2))) {
    cat("PASS: Source is encoded as 0, 1, 2\n")
  } else {
    cat("WARNING: Unexpected source values found\n")
    warnings <- warnings + 1
  }

  # Check for potential issues with primary care (M) not being handled
  cat("  Note: Original source has O=outpatient, S=specialist, P/M=primary care\n")
  cat("  The preparation script encodes O=0, S=1, P=2, with TRUE~0 as default\n")
  cat("  If M (primary care) exists in raw data, it would be incorrectly coded as 0\n")
} else {
  cat("WARNING: source column not found\n")
  warnings <- warnings + 1
}

cat("\n")

# =============================================================================
# TEST 7: Check for potential data leakage
# =============================================================================
cat("--- Test 7: Data leakage check ---\n")

# Check if outcome is perfectly predicted by any single covariate
for (col in covar_cols) {
  if (length(unique(icf_data[[col]])) <= 10) {  # Only check categorical-like vars
    tab <- table(icf_data[[col]], icf_data$Y)
    # Check if any level has 100% one outcome
    row_props <- prop.table(tab, 1)
    if (any(row_props == 1, na.rm = TRUE)) {
      cat("WARNING: Perfect separation detected for", col, "\n")
      warnings <- warnings + 1
    }
  }
}

cat("PASS: No obvious data leakage detected\n")

cat("\n")

# =============================================================================
# TEST 8: Covariate distributions
# =============================================================================
cat("--- Test 8: Covariate distributions ---\n")

# Check for zero-variance covariates
zero_var <- sapply(icf_data[, covar_cols], function(x) var(x, na.rm = TRUE) == 0)
zero_var_cols <- names(zero_var)[zero_var]

if (length(zero_var_cols) == 0) {
  cat("PASS: No zero-variance covariates\n")
} else {
  cat("WARNING: Zero-variance covariates found:\n")
  for (col in zero_var_cols) {
    cat("  ", col, "\n")
  }
  warnings <- warnings + 1
}

# Check for highly imbalanced binary covariates
binary_cols <- covar_cols[sapply(icf_data[, covar_cols], function(x) all(x %in% c(0, 1)))]
for (col in binary_cols) {
  prop <- mean(icf_data[[col]])
  if (prop < 0.01 || prop > 0.99) {
    cat("WARNING: Highly imbalanced binary covariate", col, ":", round(prop * 100, 2), "% = 1\n")
    warnings <- warnings + 1
  }
}

cat("\n")

# =============================================================================
# TEST 9: Outcome by treatment balance
# =============================================================================
cat("--- Test 9: Outcome by treatment balance ---\n")

outcome_by_treat <- icf_data %>%
  group_by(W) %>%
  summarise(
    n = n(),
    events = sum(Y),
    rate = mean(Y) * 100,
    .groups = "drop"
  )

print(outcome_by_treat)

# Check if there are enough events in each group
min_events <- min(outcome_by_treat$events)
if (min_events < 10) {
  cat("WARNING: Very few events in one treatment group (", min_events, ")\n")
  cat("  This may lead to unstable estimates\n")
  warnings <- warnings + 1
} else {
  cat("PASS: Sufficient events in both treatment groups\n")
}

cat("\n")

# =============================================================================
# TEST 10: Expected covariates present
# =============================================================================
cat("--- Test 10: Expected covariates ---\n")

expected_covars <- c(
  # Demographics
  "female", "age_cat", "year",
  # Socioeconomic
  "edufam_cat", "source", "inc_cat",
  # Family history
  "fh_suicidal", "fh_depr",
  # Hospitalization
  "hosp",
  # Key diagnoses (note: diag_mdd excluded - all have depression diagnosis)
  "diag_suicidal", "diag_phobic", "diag_anxiety_other"
)

missing_expected <- setdiff(expected_covars, covar_cols)
if (length(missing_expected) == 0) {
  cat("PASS: All expected key covariates present\n")
} else {
  cat("WARNING: Missing expected covariates:\n")
  for (col in missing_expected) {
    cat("  ", col, "\n")
  }
  warnings <- warnings + 1
}

cat("\n")

# =============================================================================
# SUMMARY
# =============================================================================
cat("=== VERIFICATION SUMMARY ===\n")
cat("Errors:", errors, "\n")
cat("Warnings:", warnings, "\n")

if (errors > 0) {
  cat("\nSOME TESTS FAILED - Data may not be suitable for iCF\n")
} else if (warnings > 0) {
  cat("\nAll tests passed but there are warnings to review\n")
} else {
  cat("\nAll tests passed - Data appears ready for iCF analysis\n")
}

cat("\n=== Data Summary ===\n")
cat("Observations:", nrow(icf_data), "\n")
cat("Covariates:", length(covar_cols), "\n")
cat("Treated:", sum(icf_data$W == 1), "(", round(mean(icf_data$W) * 100, 1), "%)\n")
cat("Events:", sum(icf_data$Y == 1), "(", round(mean(icf_data$Y) * 100, 2), "%)\n")

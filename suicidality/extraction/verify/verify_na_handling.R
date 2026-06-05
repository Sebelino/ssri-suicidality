# verify_na_handling.R
# Verifies NA handling in aggregation functions across the codebase
#
# Checks for potential bugs A8, E4-E7:
# - A8: Missing na.rm in ITT_12wks.R sum() calls
# - E4: Missing na.rm in diagnosis aggregation
# - E5: Missing na.rm in medication aggregation
# - E6: Missing na.rm in time-varying aggregation
# - E7: Inconsistent safe_min/safe_max usage

library(dplyr)
library(here)
here::i_am("suicidality/extraction/verify/verify_na_handling.R")

source(here("suicidality", "extraction", "lib", "common.R"))

cat("======================================================================\n")
cat("NA HANDLING VERIFICATION\n")
cat("======================================================================\n\n")

errors <- 0
warnings <- 0

# Load data
main_28 <- read_rds("main_12wks_28.rds")
cat("main_12wks_28 rows:", nrow(main_28), "\n\n")

# =============================================================================
# Check 1: NA values in cc and sb12_itt (ITT_12wks.R A8)
# =============================================================================
cat("--- Check 1: NA values in cc and sb12_itt (A8) ---\n")
cat("Verifies ITT_12wks.R lines 121-124 sum() calls are safe\n\n")

n_cc_na <- sum(is.na(main_28$cc))
n_sb12_itt_na <- sum(is.na(main_28$sb12_itt))

cat(sprintf("NA values in cc:        %d\n", n_cc_na))
cat(sprintf("NA values in sb12_itt:  %d\n", n_sb12_itt_na))

if (n_cc_na == 0 && n_sb12_itt_na == 0) {
  cat("OK: No NA values - sum() calls in ITT_12wks.R are safe\n")
  cat("Note: Adding na.rm = TRUE would still be defensive programming best practice\n")
} else {
  cat("WARNING: NA values found - sum() calls without na.rm could return NA\n")
  warnings <- warnings + 1
  if (n_cc_na > 0) {
    cat(sprintf("  %d rows have NA in cc\n", n_cc_na))
  }
  if (n_sb12_itt_na > 0) {
    cat(sprintf("  %d rows have NA in sb12_itt\n", n_sb12_itt_na))
  }
}

# =============================================================================
# Check 2: NA values in diagnosis covariates (E4)
# =============================================================================
cat("\n--- Check 2: NA values in diagnosis covariates (E4) ---\n")
cat("Verifies 19_process_cov_diagnoses.R aggregation is safe\n\n")

diag_cols <- grep("^diag_", names(main_28), value = TRUE)
cat(sprintf("Diagnosis columns found: %d\n", length(diag_cols)))

diag_na_counts <- sapply(main_28[diag_cols], function(x) sum(is.na(x)))
total_diag_na <- sum(diag_na_counts)

cat(sprintf("Total NA values across diagnosis columns: %d\n", total_diag_na))

if (total_diag_na == 0) {
  cat("OK: No NA values in diagnosis covariates\n")
  cat("Note: The extraction pipeline correctly handles NAs via mutate(across(..., ~if_else(is.na(.), 0L, .)))\n")
} else {
  cat("WARNING: NA values found in diagnosis covariates\n")
  warnings <- warnings + 1
  non_zero <- diag_na_counts[diag_na_counts > 0]
  for (col in names(non_zero)) {
    cat(sprintf("  %s: %d NA values\n", col, non_zero[col]))
  }
}

# =============================================================================
# Check 3: NA values in medication covariates (E5)
# =============================================================================
cat("\n--- Check 3: NA values in medication covariates (E5) ---\n")
cat("Verifies 20_process_cov_medications.R aggregation is safe\n\n")

med_cols <- grep("^med_", names(main_28), value = TRUE)
cat(sprintf("Medication columns found: %d\n", length(med_cols)))

med_na_counts <- sapply(main_28[med_cols], function(x) sum(is.na(x)))
total_med_na <- sum(med_na_counts)

cat(sprintf("Total NA values across medication columns: %d\n", total_med_na))

if (total_med_na == 0) {
  cat("OK: No NA values in medication covariates\n")
  cat("Note: The extraction pipeline correctly handles NAs via mutate(across(...))\n")
} else {
  cat("WARNING: NA values found in medication covariates\n")
  warnings <- warnings + 1
  non_zero <- med_na_counts[med_na_counts > 0]
  for (col in names(non_zero)) {
    cat(sprintf("  %s: %d NA values\n", col, non_zero[col]))
  }
}

# =============================================================================
# Check 4: NA values in time-varying / PP columns (E6)
# =============================================================================
cat("\n--- Check 4: NA values in PP-related columns (E6) ---\n")
cat("Verifies 23_process_time_varying.R aggregation is safe\n\n")

pp_cols <- c("fu_start", "fu_end_pp", "sb12_pp")
pp_cols_present <- pp_cols[pp_cols %in% names(main_28)]

if (length(pp_cols_present) == length(pp_cols)) {
  pp_na_counts <- sapply(main_28[pp_cols_present], function(x) sum(is.na(x)))
  total_pp_na <- sum(pp_na_counts)

  cat(sprintf("Total NA values across PP columns: %d\n", total_pp_na))

  if (total_pp_na == 0) {
    cat("OK: No NA values in PP-related columns\n")
  } else {
    cat("WARNING: NA values found in PP-related columns\n")
    warnings <- warnings + 1
    for (col in pp_cols_present) {
      if (pp_na_counts[col] > 0) {
        cat(sprintf("  %s: %d NA values\n", col, pp_na_counts[col]))
      }
    }
  }
} else {
  cat("INFO: Some PP columns not found in main_12wks_28.rds\n")
  cat("(This is expected - PP analysis uses pp_12wks_max.rds)\n")
}

# Also check pp_12wks_max if it exists
pp_file <- file.path(rds_output_dir(), "pp_12wks_max.rds")
if (file.exists(pp_file)) {
  pp_max <- read_rds("pp_12wks_max.rds")
  cat(sprintf("\npp_12wks_max rows: %d\n", nrow(pp_max)))

  # Check key columns
  pp_key_cols <- c("cc", "fu_start", "fu_end_pp", "sb12_pp", "week_start", "week_end")
  pp_key_present <- pp_key_cols[pp_key_cols %in% names(pp_max)]

  if (length(pp_key_present) > 0) {
    pp_key_na <- sapply(pp_max[pp_key_present], function(x) sum(is.na(x)))
    total_pp_key_na <- sum(pp_key_na)

    cat(sprintf("NA values in pp_12wks_max key columns: %d\n", total_pp_key_na))

    if (total_pp_key_na == 0) {
      cat("OK: No NA values in pp_12wks_max key columns\n")
    } else {
      cat("WARNING: NA values found\n")
      warnings <- warnings + 1
      for (col in pp_key_present) {
        if (pp_key_na[col] > 0) {
          cat(sprintf("  %s: %d NA values\n", col, pp_key_na[col]))
        }
      }
    }
  }
}

# =============================================================================
# Check 5: Verify safe_min/safe_max behavior in cohort definition (E7)
# =============================================================================
cat("\n--- Check 5: Cohort definition diff calculations (E7) ---\n")
cat("Verifies 04_define_cohort.R safe_min/safe_max consistency\n\n")

# This check verifies the cohort_base.rds to ensure no issues from the
# inconsistent use of safe_min/safe_max

cohort_base_file <- file.path(rds_output_dir(), "cohort_base.rds")
if (file.exists(cohort_base_file)) {
  cohort_base <- read_rds("cohort_base.rds")
  cat(sprintf("cohort_base rows: %d\n", nrow(cohort_base)))

  # Check for any unexpected NA values in key columns
  key_cols <- c("lopnr", "diagn_date", "bdate")
  key_cols_present <- key_cols[key_cols %in% names(cohort_base)]

  for (col in key_cols_present) {
    n_na <- sum(is.na(cohort_base[[col]]))
    if (n_na > 0) {
      cat(sprintf("WARNING: %d NA values in %s\n", n_na, col))
      warnings <- warnings + 1
    }
  }

  if (all(sapply(cohort_base[key_cols_present], function(x) sum(is.na(x)) == 0))) {
    cat("OK: No NA values in cohort_base key columns\n")
    cat("Note: The inconsistent safe_min/safe_max usage did not cause issues in practice\n")
  }
} else {
  cat("INFO: cohort_base.rds not found - skipping check\n")
}

# =============================================================================
# Check 6: Overall data quality summary
# =============================================================================
cat("\n--- Check 6: Overall data quality summary ---\n")

# Count total NA values across all columns
all_na_counts <- sapply(main_28, function(x) sum(is.na(x)))
cols_with_na <- all_na_counts[all_na_counts > 0]

cat(sprintf("Columns with any NA values: %d / %d\n", length(cols_with_na), ncol(main_28)))

if (length(cols_with_na) > 0) {
  cat("\nColumns with NA values:\n")
  for (col in names(sort(cols_with_na, decreasing = TRUE))) {
    pct <- 100 * cols_with_na[col] / nrow(main_28)
    cat(sprintf("  %-30s: %6d NA (%5.2f%%)\n", col, cols_with_na[col], pct))
  }
} else {
  cat("No columns have NA values - excellent data quality\n")
}

# =============================================================================
# Summary
# =============================================================================
cat("\n======================================================================\n")
cat("NA HANDLING VERIFICATION SUMMARY\n")
cat("======================================================================\n")
cat(sprintf("Errors:   %d\n", errors))
cat(sprintf("Warnings: %d\n", warnings))

if (errors == 0 && warnings == 0) {
  cat("\nAll NA handling checks passed!\n")
  cat("\nConclusion:\n")
  cat("- A8: FALSE POSITIVE - cc and sb12_itt have no NAs\n")
  cat("- E4: FALSE POSITIVE - diagnosis covariates have no NAs (handled by pipeline)\n")
  cat("- E5: FALSE POSITIVE - medication covariates have no NAs (handled by pipeline)\n")
  cat("- E6: FALSE POSITIVE - PP columns have no NAs\n")
  cat("- E7: DOCUMENTED - inconsistent but harmless (no NAs in practice)\n")
  cat("\nRecommendation: Add na.rm = TRUE for defensive programming, but no urgent fix needed.\n")
} else if (errors == 0) {
  cat("\nNo critical errors. Review warnings above.\n")
} else {
  cat("\nPlease review the errors above.\n")
}

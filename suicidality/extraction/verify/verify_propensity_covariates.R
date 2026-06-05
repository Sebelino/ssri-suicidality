# verify_propensity_covariates.R
# Verifies propensity score model covariates are present and have expected values

library(dplyr)
library(here)
here::i_am("suicidality/extraction/verify/verify_propensity_covariates.R")

source(here("suicidality", "extraction", "lib", "common.R"))

cat("======================================================================\n")
cat("PROPENSITY SCORE COVARIATE VERIFICATION\n")
cat("======================================================================\n\n")

errors <- 0
warnings <- 0

# Load data
main_28 <- read_rds("main_12wks_28.rds")
cat("main_12wks_28 rows:", nrow(main_28), "\n\n")

# Expected covariates based on Lagerberg et al. 2023 and ITT_12wks.R
# From the propensity score formula in ITT_12wks.R:
# cc ~ female + age + year + edufam_cat + source + inc_cat +
#      diag_bipolar + diag_psychotic + diag_sud + diag_autism + diag_adhd + diag_suicidal +
#      diag_stress + diag_phobic + diag_anxiety_other +
#      med_antipsychotic + med_hypnotic + med_benzodiazepine + med_antiepileptic + med_stimulant

expected_covariates <- c(
  # Demographics
  "female", "age", "year",
  # Socioeconomic
  "edufam_cat", "inc_cat",
  # Source
  "source",
  # Diagnoses
  "diag_bipolar", "diag_psychotic", "diag_sud", "diag_autism", "diag_adhd",
  "diag_suicidal", "diag_stress", "diag_phobic", "diag_anxiety_other",
  # Medications
  "med_antipsychotic", "med_hypnotic", "med_benzodiazepine", "med_antiepileptic", "med_stimulant"
)

# Check 1: All expected covariates present
cat("--- Check 1: Expected covariates present ---\n")
missing_covs <- setdiff(expected_covariates, names(main_28))
if (length(missing_covs) == 0) {
  cat("OK: All expected propensity score covariates present\n")
} else {
  cat(sprintf("ERROR: Missing covariates: %s\n", paste(missing_covs, collapse = ", ")))
  errors <- errors + 1
}

# Check 2: No missing values in required covariates
cat("\n--- Check 2: Missing values in covariates ---\n")
for (cov in expected_covariates) {
  if (cov %in% names(main_28)) {
    n_missing <- sum(is.na(main_28[[cov]]))
    if (n_missing == 0) {
      # OK, silent
    } else {
      cat(sprintf("WARNING: %s has %d missing values (%.2f%%)\n",
                  cov, n_missing, 100 * n_missing / nrow(main_28)))
      warnings <- warnings + 1
    }
  }
}
cat("OK: Checked all covariates for missing values\n")

# Check 3: Categorical variables have expected levels
cat("\n--- Check 3: Categorical variable levels ---\n")

# edufam_cat should be 0, 1, 2, 99
edufam_vals <- unique(main_28$edufam_cat)
expected_edufam <- c(0, 1, 2, 99)
if (all(edufam_vals %in% expected_edufam)) {
  cat(sprintf("OK: edufam_cat has expected values: %s\n", paste(sort(edufam_vals), collapse = ", ")))
} else {
  cat(sprintf("ERROR: edufam_cat has unexpected values: %s\n",
              paste(setdiff(edufam_vals, expected_edufam), collapse = ", ")))
  errors <- errors + 1
}

# inc_cat should be 1-5 or 99
inc_vals <- unique(main_28$inc_cat)
expected_inc <- c(1, 2, 3, 4, 5, 99)
if (all(inc_vals %in% expected_inc)) {
  cat(sprintf("OK: inc_cat has expected values: %s\n", paste(sort(inc_vals), collapse = ", ")))
} else {
  cat(sprintf("ERROR: inc_cat has unexpected values: %s\n",
              paste(setdiff(inc_vals, expected_inc), collapse = ", ")))
  errors <- errors + 1
}

# source should be O, S, T (M was filtered)
source_vals <- unique(main_28$source)
expected_source <- c("O", "S", "T")
if (all(source_vals %in% expected_source)) {
  cat(sprintf("OK: source has expected values: %s\n", paste(sort(source_vals), collapse = ", ")))
} else {
  cat(sprintf("WARNING: source has additional values: %s\n",
              paste(setdiff(source_vals, expected_source), collapse = ", ")))
  warnings <- warnings + 1
}

# Check 4: Binary covariates are 0/1
cat("\n--- Check 4: Binary covariates are 0/1 ---\n")
binary_covs <- c("female", "diag_bipolar", "diag_psychotic", "diag_sud", "diag_autism",
                 "diag_adhd", "diag_suicidal", "diag_stress", "diag_phobic", "diag_anxiety_other",
                 "med_antipsychotic", "med_hypnotic", "med_benzodiazepine", "med_antiepileptic",
                 "med_stimulant")

for (cov in binary_covs) {
  if (cov %in% names(main_28)) {
    vals <- unique(main_28[[cov]])
    vals <- vals[!is.na(vals)]
    if (all(vals %in% c(0, 1))) {
      # OK, silent
    } else {
      cat(sprintf("ERROR: %s has non-binary values: %s\n", cov, paste(vals, collapse = ", ")))
      errors <- errors + 1
    }
  }
}
cat("OK: All binary covariates are 0/1\n")

# Check 5: Numeric covariates have reasonable ranges
cat("\n--- Check 5: Numeric covariate ranges ---\n")

age_range <- range(main_28$age)
cat(sprintf("age range: %d - %d\n", age_range[1], age_range[2]))
if (age_range[1] >= 6 && age_range[2] <= 24) {
  cat("OK: Age range is 6-24 as expected\n")
} else {
  cat("ERROR: Age range outside 6-24\n")
  errors <- errors + 1
}

year_range <- range(main_28$year)
cat(sprintf("year range: %d - %d\n", year_range[1], year_range[2]))
if (year_range[1] >= 2006 && year_range[2] <= 2020) {
  cat("OK: Year range is within study period\n")
} else {
  cat("WARNING: Year range outside expected study period\n")
  warnings <- warnings + 1
}

# Check 6: Covariate balance between treatment groups
cat("\n--- Check 6: Covariate balance (unadjusted) ---\n")
cat("Treatment group distribution:\n")
cat(sprintf("  cc=0 (control):  %d (%.1f%%)\n",
            sum(main_28$cc == 0), 100 * mean(main_28$cc == 0)))
cat(sprintf("  cc=1 (treated):  %d (%.1f%%)\n",
            sum(main_28$cc == 1), 100 * mean(main_28$cc == 1)))

# Calculate standardized differences for key covariates
calc_smd <- function(data, var, treatment_var = "cc") {
  treated <- data[[var]][data[[treatment_var]] == 1]
  control <- data[[var]][data[[treatment_var]] == 0]

  mean_diff <- mean(treated, na.rm = TRUE) - mean(control, na.rm = TRUE)
  pooled_sd <- sqrt((var(treated, na.rm = TRUE) + var(control, na.rm = TRUE)) / 2)

  if (pooled_sd == 0) return(0)
  return(mean_diff / pooled_sd)
}

cat("\nStandardized mean differences (SMD) for key covariates:\n")
cat("(SMD > 0.1 suggests imbalance)\n\n")

smd_vars <- c("female", "age", "diag_suicidal", "diag_adhd", "med_hypnotic", "med_stimulant")
for (var in smd_vars) {
  smd <- calc_smd(main_28, var)
  flag <- if (abs(smd) > 0.1) " *" else ""
  cat(sprintf("  %-20s SMD = %6.3f%s\n", var, smd, flag))
}

cat("\n* indicates SMD > 0.1 (imbalance expected before IPW adjustment)\n")

# Check 7: Covariates NOT in propensity model but present in data
cat("\n--- Check 7: Additional covariates in data (not in PS model) ---\n")
additional_covs <- c("diag_mdd", "diag_ocd", "diag_anorexia", "diag_bulimia", "diag_sleep",
                     "diag_personality_cluster_b", "diag_intellectual_disability", "diag_conduct",
                     "diag_overdose", "diag_organic",
                     "med_opioid", "med_addiction", "med_mood_stabilizer",
                     "fh_suicidal", "fh_depr", "hosp")

present_additional <- intersect(additional_covs, names(main_28))
cat(sprintf("Additional covariates in data: %d\n", length(present_additional)))
cat(sprintf("  %s\n", paste(present_additional, collapse = ", ")))
cat("\nNote: These covariates are available but NOT used in the propensity score model.\n")
cat("      Consider if any should be added based on Lagerberg et al. 2023.\n")

# Summary
cat("\n======================================================================\n")
cat("PROPENSITY COVARIATE VERIFICATION SUMMARY\n")
cat("======================================================================\n")
cat(sprintf("Errors:   %d\n", errors))
cat(sprintf("Warnings: %d\n", warnings))

if (errors == 0 && warnings == 0) {
  cat("\nAll propensity score covariate checks passed!\n")
} else if (errors == 0) {
  cat("\nNo errors, but review the warnings above.\n")
} else {
  cat("\nPlease review the errors above.\n")
}

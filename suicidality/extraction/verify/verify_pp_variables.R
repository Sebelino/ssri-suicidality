# verify_pp_variables.R
# Verifies that all variables used in PP_12wks.R exist in the data

library(dplyr)
library(here)
here::i_am("suicidality/extraction/verify/verify_pp_variables.R")

source(here("suicidality", "extraction", "lib", "common.R"))

cat("======================================================================\n")
cat("PP ANALYSIS VARIABLE VERIFICATION\n")
cat("======================================================================\n\n")

errors <- 0
warnings <- 0

# Load PP data
pp_max <- read_rds("pp_12wks_max.rds")
cat("pp_12wks_max rows:", nrow(pp_max), "\n\n")

# Variables used in PP_12wks.R denominator model (lines 55-63)
denom_vars <- c(
  "time", "timesq", "timefromdiag",  # Created in script
  "female", "age", "year", "edufam_cat", "hosp", "source", "inc_cat",
  "fh_suicidal", "fh_depr",
  "diag_stress", "diag_phobic", "diag_anxiety_other", "diag_anorexia", "diag_bulimia", "diag_sud",
  "diag_personality_cluster_b", "diag_adhd", "diag_intellectual_disability", "diag_autism",
  "diag_conduct", "diag_overdose", "diag_suicidal",
  "med_antipsychotic", "med_hypnotic", "med_benzodiazepine", "med_antiepileptic", "med_opioid", "med_stimulant",
  "anypsych_tv"
)

# Variables used in PP_12wks.R numerator model (lines 70-78)
# Updated to match fixed variable names
num_vars <- c(
  "time", "timesq", "timefromdiag",  # Created in script
  "female", "age", "year", "edufam_cat", "hosp", "source", "inc_cat",
  "fh_suicidal", "fh_depr",
  "diag_stress", "diag_phobic", "diag_anxiety_other", "diag_anorexia", "diag_bulimia", "diag_sud",
  "diag_personality_cluster_b", "diag_adhd", "diag_intellectual_disability", "diag_autism",
  "diag_conduct", "diag_overdose", "diag_suicidal",
  "med_antipsychotic", "med_hypnotic", "med_benzodiazepine", "med_antiepileptic", "med_opioid", "med_stimulant"
)

# Check denominator variables
cat("--- Denominator model variables ---\n")
for (var in denom_vars) {
  if (var %in% c("time", "timesq", "timefromdiag")) {
    cat(sprintf("OK: %s (created in analysis script)\n", var))
  } else if (var %in% names(pp_max)) {
    cat(sprintf("OK: %s present\n", var))
  } else {
    cat(sprintf("ERROR: %s MISSING from PP data\n", var))
    errors <- errors + 1
  }
}

# Check numerator variables
cat("\n--- Numerator model variables ---\n")
for (var in num_vars) {
  if (var %in% c("time", "timesq", "timefromdiag")) {
    cat(sprintf("OK: %s (created in analysis script)\n", var))
  } else if (var %in% names(pp_max)) {
    cat(sprintf("OK: %s present\n", var))
  } else {
    cat(sprintf("ERROR: %s MISSING from PP data\n", var))
    errors <- errors + 1
  }
}

# Check anypsych_tv computation
cat("\n--- anypsych_tv verification ---\n")
if ("anypsych_tv" %in% names(pp_max)) {
  # anypsych_tv should be 1 if any of: med_antipsychotic, med_benzodiazepine, opsych
  anypsych_check <- pp_max %>%
    mutate(
      expected_anypsych = if_else(med_antipsychotic == 1 | med_benzodiazepine == 1 | opsych == 1, 1L, 0L)
    ) %>%
    filter(anypsych_tv != expected_anypsych)

  n_mismatch <- nrow(anypsych_check)
  if (n_mismatch == 0) {
    cat("OK: anypsych_tv correctly computed from components\n")
  } else {
    cat(sprintf("ERROR: %d rows with incorrect anypsych_tv\n", n_mismatch))
    errors <- errors + 1
  }
} else {
  cat("ERROR: anypsych_tv not present\n")
  errors <- errors + 1
}

# Summary
cat("\n======================================================================\n")
cat("PP VARIABLE VERIFICATION SUMMARY\n")
cat("======================================================================\n")
cat(sprintf("Errors:   %d\n", errors))
cat(sprintf("Warnings: %d\n", warnings))

if (errors == 0) {
  cat("\nAll PP analysis variables are present!\n")
} else {
  cat("\nERRORS FOUND! Some required variables are missing from pp_12wks_max.rds.\n")
  cat("Please check the extraction pipeline to ensure all variables are included.\n")
}

# verify_propensity_models.R
# Verifies propensity score model consistency across analysis scripts
# Related to bug A4: Propensity Score Model Inconsistency Across Scripts

library(dplyr)
library(here)
here::i_am("suicidality/extraction/verify/verify_propensity_models.R")

cat("======================================================================\n")
cat("PROPENSITY SCORE MODEL CONSISTENCY VERIFICATION\n")
cat("======================================================================\n\n")

errors <- 0
warnings <- 0

# Define expected covariates based on Lagerberg et al. 2023 Table 1
# These are the covariates that SHOULD be in all propensity score models

expected_demographics <- c("female", "age", "year")
expected_socioeconomic <- c("edufam_cat", "source", "inc_cat")
expected_family_history <- c("fh_suicidal", "fh_depr")
expected_hospitalization <- c("hosp")

# Diagnosis covariates from Table S3 (Lagerberg 2023)
expected_diagnoses <- c(
  "diag_mdd",           # F32-F33: Major depressive disorder
  "diag_adhd",          # F90: ADHD
  "diag_stress",        # F43: Stress/adjustment disorders
  "diag_sud",           # F11-F19 excl F17: Substance use disorder (excl alcohol)
  "diag_alcohol",       # F10: Alcohol use disorder (separate from SUD)
  "diag_suicidal",      # X60-X84, Y10-Y34: Suicidal behavior
  "diag_overdose",      # T36-T51, X40-X49: Overdose/poisoning
  "diag_autism",        # F84: Autism spectrum
  "diag_anxiety_other", # F41.0-F41.1: Panic/GAD (or combined diag_anxiety)
  "diag_phobic",        # F40: Phobic anxiety (or combined diag_anxiety)
  "diag_sleep",         # F51: Sleep disorders
  "diag_organic",       # F00-F09: Organic mental disorders
  "diag_anorexia",      # F50.0-F50.1: Anorexia
  "diag_ocd",           # F42: OCD
  "diag_conduct",       # F91: Conduct disorders
  "diag_psychotic",     # F20-F29: Psychotic disorders
  "diag_intellectual_disability", # F70-F79
  "diag_bipolar",       # F30-F31: Bipolar
  "diag_personality_cluster_b",   # F60.2-F60.3
  "diag_bulimia"        # F50.2-F50.3: Bulimia
)

# Medication covariates from Table 1
expected_medications <- c(
  "med_hypnotic",       # N05C: Hypnotics/sedatives
  "med_stimulant",      # N06B: Psychostimulants
  "med_antipsychotic",  # N05A excl lithium
  "med_benzodiazepine", # N05BA
  "med_opioid",         # N02A
  "med_antiepileptic",  # N03A excl mood stabilizers
  "med_mood_stabilizer", # Lithium, valproate, etc.
  "med_addiction"       # N07B
)

cat("Expected covariates based on Lagerberg 2023:\n")
cat("  Demographics:", length(expected_demographics), "\n")
cat("  Socioeconomic:", length(expected_socioeconomic), "\n")
cat("  Family history:", length(expected_family_history), "\n")
cat("  Hospitalization:", length(expected_hospitalization), "\n")
cat("  Diagnoses:", length(expected_diagnoses), "\n")
cat("  Medications:", length(expected_medications), "\n")
cat("  TOTAL:", length(c(expected_demographics, expected_socioeconomic,
                         expected_family_history, expected_hospitalization,
                         expected_diagnoses, expected_medications)), "\n\n")

# Define what each script actually uses (extracted from code review)
# ITT_12wks.R baseline treatment model
itt_covariates <- c(
  # Demographics
  "female", "age", "year",
  # Socioeconomic
  "edufam_cat", "source", "inc_cat",
  # Family history
  "fh_suicidal", "fh_depr",
  # Hospitalization
  "hosp",
  # Diagnoses (uses diag_anxiety = combined phobic + anxiety_other)
  "diag_mdd", "diag_bipolar", "diag_psychotic", "diag_alcohol", "diag_sud",
  "diag_autism", "diag_adhd", "diag_suicidal", "diag_overdose",
  "diag_stress", "diag_anxiety",  # combined variable
  "diag_sleep", "diag_organic", "diag_anorexia", "diag_bulimia",
  "diag_ocd", "diag_conduct", "diag_intellectual_disability", "diag_personality_cluster_b",
  # Medications
  "med_antipsychotic", "med_hypnotic", "med_benzodiazepine", "med_antiepileptic",
  "med_stimulant", "med_opioid", "med_mood_stabilizer", "med_addiction"
)

# PP_12wks.R baseline treatment model (lines 171-179) - FIXED to match bootstrap
pp_baseline_covariates <- c(
  # Demographics
  "female", "age", "year",
  # Socioeconomic
  "edufam_cat", "source", "inc_cat",
  # Family history
  "fh_suicidal", "fh_depr",
  # Hospitalization
  "hosp",
  # Diagnoses
  "diag_stress", "diag_phobic", "diag_anxiety_other", "diag_anorexia", "diag_bulimia",
  "diag_alcohol", "diag_sud", "diag_personality_cluster_b", "diag_adhd",
  "diag_intellectual_disability", "diag_autism", "diag_conduct", "diag_overdose", "diag_suicidal",
  # Medications
  "med_antipsychotic", "med_hypnotic", "med_benzodiazepine", "med_antiepileptic",
  "med_opioid", "med_stimulant"
)

# PP_12wks.R bootstrap model (lines 359-366)
pp_bootstrap_covariates <- c(
  # Demographics
  "female", "age", "year",
  # Socioeconomic
  "edufam_cat", "source", "inc_cat",
  # Family history
  "fh_suicidal", "fh_depr",
  # Hospitalization
  "hosp",
  # Diagnoses
  "diag_stress", "diag_phobic", "diag_anxiety_other", "diag_anorexia", "diag_bulimia",
  "diag_alcohol", "diag_sud", "diag_personality_cluster_b", "diag_adhd",
  "diag_intellectual_disability", "diag_autism", "diag_conduct", "diag_overdose", "diag_suicidal",
  # Medications
  "med_antipsychotic", "med_hypnotic", "med_benzodiazepine", "med_antiepileptic",
  "med_opioid", "med_stimulant"
)

# validate_paper_results.R model
validate_covariates <- c(
  # Demographics
  "female", "age", "year",
  # Socioeconomic
  "edufam_cat", "source", "inc_cat",
  # Family history
  "fh_suicidal", "fh_depr",
  # Hospitalization
  "hosp",
  # Diagnoses
  "diag_organic", "diag_alcohol", "diag_sud", "diag_psychotic", "diag_bipolar", "diag_mdd",
  "diag_phobic", "diag_anxiety_other", "diag_ocd", "diag_stress", "diag_anorexia", "diag_bulimia",
  "diag_sleep", "diag_personality_cluster_b", "diag_intellectual_disability", "diag_autism",
  "diag_adhd", "diag_conduct", "diag_overdose", "diag_suicidal",
  # Medications
  "med_antipsychotic", "med_hypnotic", "med_benzodiazepine", "med_antiepileptic",
  "med_stimulant", "med_addiction", "med_opioid", "med_mood_stabilizer"
)

# Function to compare covariate sets
compare_covariates <- function(name1, cov1, name2, cov2) {
  in_1_not_2 <- setdiff(cov1, cov2)
  in_2_not_1 <- setdiff(cov2, cov1)

  cat(sprintf("\n--- %s vs %s ---\n", name1, name2))
  cat(sprintf("%s has %d covariates\n", name1, length(cov1)))
  cat(sprintf("%s has %d covariates\n", name2, length(cov2)))

  if (length(in_1_not_2) > 0) {
    cat(sprintf("In %s but not %s: %s\n", name1, name2, paste(in_1_not_2, collapse = ", ")))
  }
  if (length(in_2_not_1) > 0) {
    cat(sprintf("In %s but not %s: %s\n", name2, name1, paste(in_2_not_1, collapse = ", ")))
  }

  if (length(in_1_not_2) == 0 && length(in_2_not_1) == 0) {
    cat("OK: Models are consistent\n")
    return(0)
  } else {
    return(1)
  }
}

cat("=== Model Comparisons ===\n")

# Compare ITT vs PP baseline
diff1 <- compare_covariates("ITT_12wks", itt_covariates, "PP_baseline", pp_baseline_covariates)
if (diff1 > 0) warnings <- warnings + 1

# Compare ITT vs PP bootstrap
diff2 <- compare_covariates("ITT_12wks", itt_covariates, "PP_bootstrap", pp_bootstrap_covariates)
if (diff2 > 0) warnings <- warnings + 1

# Compare PP baseline vs PP bootstrap
diff3 <- compare_covariates("PP_baseline", pp_baseline_covariates, "PP_bootstrap", pp_bootstrap_covariates)
if (diff3 > 0) {
  cat("ERROR: PP baseline and bootstrap models are INCONSISTENT within same script!\n")
  errors <- errors + 1
}

# Compare ITT vs validate_paper_results
diff4 <- compare_covariates("ITT_12wks", itt_covariates, "validate_paper_results", validate_covariates)
if (diff4 > 0) warnings <- warnings + 1

# Summary
cat("\n======================================================================\n")
cat("PROPENSITY MODEL VERIFICATION SUMMARY\n")
cat("======================================================================\n")
cat(sprintf("Errors:   %d\n", errors))
cat(sprintf("Warnings: %d\n", warnings))

if (errors == 0 && warnings == 0) {
  cat("\nAll propensity score models are consistent!\n")
} else if (errors > 0) {
  cat("\nCRITICAL: PP_12wks.R uses different models for baseline and bootstrap!\n")
  cat("This means bootstrap CIs are computed with a different model than point estimates.\n")
} else {
  cat("\nNote: Some model differences exist across scripts.\n")
  cat("ITT and PP analyses may use slightly different covariate sets.\n")
}

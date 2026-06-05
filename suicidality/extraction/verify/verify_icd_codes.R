# verify_icd_codes.R
# Verifies ICD-10 code definitions for diagnosis covariates

library(dplyr)
library(here)
here::i_am("suicidality/extraction/verify/verify_icd_codes.R")

source(here("suicidality", "extraction", "lib", "common.R"))

cat("======================================================================\n")
cat("ICD CODE VERIFICATION\n")
cat("======================================================================\n\n")

errors <- 0
warnings <- 0

# Load data
main_28 <- read_rds("main_12wks_28.rds")
raw_diagnoses <- read_rds("raw_diagnoses_cohort.rds")
cov_diagnoses <- read_rds("cov_diagnoses.rds")

# Get fu_start for filtering
fu_start_data <- main_28 %>%
  select(lopnr, fu_start)

# Filter diagnoses to before/on fu_start
# Note: Uses <= fu_start to include same-day diagnoses (bug #13 fix)
diag_before_fu <- raw_diagnoses %>%
  inner_join(fu_start_data, by = "lopnr") %>%
  filter(diagn_date <= fu_start) %>%
  mutate(
    dia3 = substr(dia, 1, 3),
    dia4 = substr(dia, 1, 4)
  )

cat("Diagnoses before fu_start:", nrow(diag_before_fu), "\n\n")

# Define expected ICD code mappings (from 19_process_cov_diagnoses.R)
# Table S3 from Lagerberg et al. 2023

verify_diagnosis <- function(data, cov_data, cov_name, filter_expr, description) {
  expected <- data %>%
    filter(!!rlang::parse_expr(filter_expr)) %>%
    distinct(lopnr) %>%
    nrow()

  actual <- sum(cov_data[[cov_name]] == 1)

  cat(sprintf("--- %s ---\n", cov_name))
  cat(sprintf("Description: %s\n", description))
  cat(sprintf("Filter: %s\n", filter_expr))
  cat(sprintf("Expected: %d, Actual: %d\n", expected, actual))

  if (expected == actual) {
    cat("OK: Count matches\n\n")
    return(0)
  } else {
    cat(sprintf("ERROR: Count mismatch (diff: %d)\n\n", actual - expected))
    return(1)
  }
}

# Verify each diagnosis covariate
cat("======================================================================\n")
cat("VERIFYING DIAGNOSIS COVARIATES\n")
cat("======================================================================\n\n")

errors <- errors + verify_diagnosis(
  diag_before_fu, cov_diagnoses, "diag_organic",
  "dia3 >= 'F00' & dia3 <= 'F09'",
  "F00-F09: Organic mental disorders"
)

# Bug #9 fix: diag_alcohol (F10) is now separate from diag_sud (F11-F19 excl F17)
errors <- errors + verify_diagnosis(
  diag_before_fu, cov_diagnoses, "diag_alcohol",
  "dia3 == 'F10'",
  "F10: Alcohol use disorder"
)

errors <- errors + verify_diagnosis(
  diag_before_fu, cov_diagnoses, "diag_sud",
  "dia3 >= 'F11' & dia3 <= 'F19' & dia3 != 'F17'",
  "F11-F19 excl F17: Substance use disorder (excl alcohol, tobacco)"
)

errors <- errors + verify_diagnosis(
  diag_before_fu, cov_diagnoses, "diag_psychotic",
  "dia3 >= 'F20' & dia3 <= 'F29'",
  "F20-F29: Schizophrenia and psychotic disorders"
)

errors <- errors + verify_diagnosis(
  diag_before_fu, cov_diagnoses, "diag_bipolar",
  "dia3 %in% c('F30', 'F31')",
  "F30-F31: Bipolar/manic disorders"
)

errors <- errors + verify_diagnosis(
  diag_before_fu, cov_diagnoses, "diag_mdd",
  "dia3 %in% c('F32', 'F33')",
  "F32-F33: Major depressive disorder"
)

errors <- errors + verify_diagnosis(
  diag_before_fu, cov_diagnoses, "diag_phobic",
  "dia4 %in% c('F400', 'F401', 'F402')",
  "F40.0-F40.2: Phobic anxiety disorders"
)

errors <- errors + verify_diagnosis(
  diag_before_fu, cov_diagnoses, "diag_anxiety_other",
  "dia4 %in% c('F410', 'F411')",
  "F41.0-F41.1: Panic/generalized anxiety"
)

errors <- errors + verify_diagnosis(
  diag_before_fu, cov_diagnoses, "diag_ocd",
  "dia3 == 'F42'",
  "F42: OCD"
)

errors <- errors + verify_diagnosis(
  diag_before_fu, cov_diagnoses, "diag_stress",
  "dia3 == 'F43'",
  "F43: Stress/adjustment disorders"
)

errors <- errors + verify_diagnosis(
  diag_before_fu, cov_diagnoses, "diag_anorexia",
  "dia4 %in% c('F500', 'F501')",
  "F50.0-F50.1: Anorexia nervosa"
)

errors <- errors + verify_diagnosis(
  diag_before_fu, cov_diagnoses, "diag_bulimia",
  "dia4 %in% c('F502', 'F503')",
  "F50.2-F50.3: Bulimia nervosa"
)

errors <- errors + verify_diagnosis(
  diag_before_fu, cov_diagnoses, "diag_sleep",
  "dia3 == 'F51'",
  "F51: Non-organic sleep disorders"
)

errors <- errors + verify_diagnosis(
  diag_before_fu, cov_diagnoses, "diag_personality_cluster_b",
  "dia4 %in% c('F602', 'F603')",
  "F60.2-F60.3: Cluster B personality disorders"
)

errors <- errors + verify_diagnosis(
  diag_before_fu, cov_diagnoses, "diag_intellectual_disability",
  "dia3 >= 'F70' & dia3 <= 'F79'",
  "F70-F79: Intellectual disability"
)

errors <- errors + verify_diagnosis(
  diag_before_fu, cov_diagnoses, "diag_autism",
  "dia4 %in% c('F840', 'F841', 'F845', 'F848', 'F849')",
  "F84.0/1/5/8/9: Autism spectrum disorders"
)

errors <- errors + verify_diagnosis(
  diag_before_fu, cov_diagnoses, "diag_adhd",
  "dia3 == 'F90'",
  "F90: ADHD/Hyperkinetic disorder"
)

errors <- errors + verify_diagnosis(
  diag_before_fu, cov_diagnoses, "diag_conduct",
  "dia3 == 'F91'",
  "F91: Conduct disorders"
)

errors <- errors + verify_diagnosis(
  diag_before_fu, cov_diagnoses, "diag_overdose",
  "(dia3 >= 'T36' & dia3 <= 'T51') | (dia3 >= 'X40' & dia3 <= 'X49')",
  "T36-T51, X40-X49: Overdose/poisoning"
)

errors <- errors + verify_diagnosis(
  diag_before_fu, cov_diagnoses, "diag_suicidal",
  "(dia3 >= 'X60' & dia3 <= 'X84') | (dia3 >= 'Y10' & dia3 <= 'Y34')",
  "X60-X84, Y10-Y34: Suicidal behaviour"
)

# Check for F334 inclusion (remission codes)
cat("======================================================================\n")
cat("CHECKING F334 (DEPRESSION IN REMISSION)\n")
cat("======================================================================\n\n")

n_f334 <- diag_before_fu %>%
  filter(dia4 == "F334") %>%
  distinct(lopnr) %>%
  nrow()

cat(sprintf("Individuals with F334 (depression in remission): %d\n", n_f334))
if (n_f334 > 0) {
  cat("INFO: F334 is INCLUDED in diag_mdd covariate\n")
  cat("      F334 was EXCLUDED from index diagnosis (script 01)\n")
  cat("      This asymmetry may be intentional (prior history includes remission)\n")
  warnings <- warnings + 1
}

# Summary
cat("\n======================================================================\n")
cat("ICD CODE VERIFICATION SUMMARY\n")
cat("======================================================================\n")
cat(sprintf("Errors:   %d\n", errors))
cat(sprintf("Warnings: %d\n", warnings))

if (errors == 0 && warnings == 0) {
  cat("\nAll ICD code definitions verified!\n")
} else if (errors == 0) {
  cat("\nNo errors, but review the informational notes above.\n")
} else {
  cat("\nPlease review the errors above.\n")
}

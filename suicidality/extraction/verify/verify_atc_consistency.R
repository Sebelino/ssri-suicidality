# verify_atc_consistency.R
# Verifies ATC code definitions are consistent across extraction scripts

library(dplyr)
library(here)
here::i_am("suicidality/extraction/verify/verify_atc_consistency.R")

source(here("suicidality", "extraction", "lib", "common.R"))

cat("======================================================================\n")
cat("ATC CODE CONSISTENCY VERIFICATION\n")
cat("======================================================================\n\n")

errors <- 0
warnings <- 0

# Load raw prescriptions for verification
raw_prescriptions <- read_rds("raw_prescriptions_cohort.rds")
main_28 <- read_rds("main_12wks_28.rds")
cov_medications <- read_rds("cov_medications.rds")

fu_start_data <- main_28 %>%
  select(lopnr, fu_start)

# Get medications in 90-day window (3 months per paper)
# Note: Uses <= fu_start to include same-day prescriptions (bug #13 fix)
meds_90day <- raw_prescriptions %>%
  inner_join(fu_start_data, by = "lopnr") %>%
  filter(edatum >= fu_start - 90 & edatum <= fu_start) %>%
  select(lopnr, atc)

cat("Medications in 90-day window:", nrow(meds_90day), "\n\n")

# Define expected ATC patterns (from 20_process_cov_medications.R)
mood_stabilizer_codes <- c("N03AG01", "N03AX09", "N03AF01")

# Test each medication category

cat("--- Verifying med_antipsychotic (N05A excl N05AN) ---\n")
expected_antipsychotic <- meds_90day %>%
  filter(substr(atc, 1, 4) == "N05A" & !grepl("^N05AN", atc)) %>%
  distinct(lopnr) %>%
  nrow()
actual_antipsychotic <- sum(cov_medications$med_antipsychotic == 1)
cat(sprintf("Expected: %d, Actual: %d\n", expected_antipsychotic, actual_antipsychotic))
if (expected_antipsychotic == actual_antipsychotic) {
  cat("OK: med_antipsychotic count matches\n")
} else {
  cat("ERROR: med_antipsychotic count mismatch\n")
  errors <- errors + 1
}

cat("\n--- Verifying med_hypnotic (N05C) ---\n")
expected_hypnotic <- meds_90day %>%
  filter(substr(atc, 1, 4) == "N05C") %>%
  distinct(lopnr) %>%
  nrow()
actual_hypnotic <- sum(cov_medications$med_hypnotic == 1)
cat(sprintf("Expected: %d, Actual: %d\n", expected_hypnotic, actual_hypnotic))
if (expected_hypnotic == actual_hypnotic) {
  cat("OK: med_hypnotic count matches\n")
} else {
  cat("ERROR: med_hypnotic count mismatch\n")
  errors <- errors + 1
}

cat("\n--- Verifying med_benzodiazepine (N05BA) ---\n")
expected_benzo <- meds_90day %>%
  filter(substr(atc, 1, 5) == "N05BA") %>%
  distinct(lopnr) %>%
  nrow()
actual_benzo <- sum(cov_medications$med_benzodiazepine == 1)
cat(sprintf("Expected: %d, Actual: %d\n", expected_benzo, actual_benzo))
if (expected_benzo == actual_benzo) {
  cat("OK: med_benzodiazepine count matches\n")
} else {
  cat("ERROR: med_benzodiazepine count mismatch\n")
  errors <- errors + 1
}

cat("\n--- Verifying med_antiepileptic (N03A excl mood stabilizers) ---\n")
expected_antiepileptic <- meds_90day %>%
  filter(substr(atc, 1, 4) == "N03A" & !(atc %in% mood_stabilizer_codes)) %>%
  distinct(lopnr) %>%
  nrow()
actual_antiepileptic <- sum(cov_medications$med_antiepileptic == 1)
cat(sprintf("Expected: %d, Actual: %d\n", expected_antiepileptic, actual_antiepileptic))
if (expected_antiepileptic == actual_antiepileptic) {
  cat("OK: med_antiepileptic count matches\n")
} else {
  cat("ERROR: med_antiepileptic count mismatch\n")
  errors <- errors + 1
}

cat("\n--- Verifying med_stimulant (N06B) ---\n")
expected_stimulant <- meds_90day %>%
  filter(substr(atc, 1, 4) == "N06B") %>%
  distinct(lopnr) %>%
  nrow()
actual_stimulant <- sum(cov_medications$med_stimulant == 1)
cat(sprintf("Expected: %d, Actual: %d\n", expected_stimulant, actual_stimulant))
if (expected_stimulant == actual_stimulant) {
  cat("OK: med_stimulant count matches\n")
} else {
  cat("ERROR: med_stimulant count mismatch\n")
  errors <- errors + 1
}

cat("\n--- Verifying med_addiction (N07B) ---\n")
expected_addiction <- meds_90day %>%
  filter(substr(atc, 1, 4) == "N07B") %>%
  distinct(lopnr) %>%
  nrow()
actual_addiction <- sum(cov_medications$med_addiction == 1)
cat(sprintf("Expected: %d, Actual: %d\n", expected_addiction, actual_addiction))
if (expected_addiction == actual_addiction) {
  cat("OK: med_addiction count matches\n")
} else {
  cat("ERROR: med_addiction count mismatch\n")
  errors <- errors + 1
}

cat("\n--- Verifying med_opioid (N02A) ---\n")
expected_opioid <- meds_90day %>%
  filter(substr(atc, 1, 4) == "N02A") %>%
  distinct(lopnr) %>%
  nrow()
actual_opioid <- sum(cov_medications$med_opioid == 1)
cat(sprintf("Expected: %d, Actual: %d\n", expected_opioid, actual_opioid))
if (expected_opioid == actual_opioid) {
  cat("OK: med_opioid count matches\n")
} else {
  cat("ERROR: med_opioid count mismatch\n")
  errors <- errors + 1
}

cat("\n--- Verifying med_mood_stabilizer (N05AN + mood stabilizer codes) ---\n")
expected_mood_stabilizer <- meds_90day %>%
  filter(grepl("^N05AN", atc) | atc %in% mood_stabilizer_codes) %>%
  distinct(lopnr) %>%
  nrow()
actual_mood_stabilizer <- sum(cov_medications$med_mood_stabilizer == 1)
cat(sprintf("Expected: %d, Actual: %d\n", expected_mood_stabilizer, actual_mood_stabilizer))
if (expected_mood_stabilizer == actual_mood_stabilizer) {
  cat("OK: med_mood_stabilizer count matches\n")
} else {
  cat("ERROR: med_mood_stabilizer count mismatch\n")
  errors <- errors + 1
}

# Check for overlap issues
cat("\n--- Checking medication overlaps ---\n")

# Antipsychotic should not overlap with lithium (N05AN)
antipsych_lopnrs <- meds_90day %>%
  filter(substr(atc, 1, 4) == "N05A" & !grepl("^N05AN", atc)) %>%
  distinct(lopnr) %>%
  pull(lopnr)

lithium_lopnrs <- meds_90day %>%
  filter(grepl("^N05AN", atc)) %>%
  distinct(lopnr) %>%
  pull(lopnr)

# Check if any lithium users are wrongly counted as antipsychotic users
lithium_in_antipsych <- cov_medications %>%
  filter(lopnr %in% lithium_lopnrs & med_antipsychotic == 1 & !(lopnr %in% antipsych_lopnrs))

if (nrow(lithium_in_antipsych) == 0) {
  cat("OK: No lithium-only users wrongly classified as antipsychotic users\n")
} else {
  cat(sprintf("ERROR: %d lithium-only users wrongly classified as antipsychotic\n",
              nrow(lithium_in_antipsych)))
  errors <- errors + 1
}

# Antiepileptic should not include mood stabilizers
antiepi_lopnrs <- meds_90day %>%
  filter(substr(atc, 1, 4) == "N03A" & !(atc %in% mood_stabilizer_codes)) %>%
  distinct(lopnr) %>%
  pull(lopnr)

mood_only_lopnrs <- meds_90day %>%
  filter(atc %in% mood_stabilizer_codes) %>%
  filter(!(lopnr %in% antiepi_lopnrs)) %>%
  distinct(lopnr) %>%
  pull(lopnr)

mood_only_in_antiepi <- cov_medications %>%
  filter(lopnr %in% mood_only_lopnrs & med_antiepileptic == 1)

if (nrow(mood_only_in_antiepi) == 0) {
  cat("OK: No mood-stabilizer-only users wrongly classified as antiepileptic users\n")
} else {
  cat(sprintf("ERROR: %d mood-stabilizer-only users wrongly classified as antiepileptic\n",
              nrow(mood_only_in_antiepi)))
  errors <- errors + 1
}

# Summary
cat("\n======================================================================\n")
cat("ATC CONSISTENCY VERIFICATION SUMMARY\n")
cat("======================================================================\n")
cat(sprintf("Errors:   %d\n", errors))
cat(sprintf("Warnings: %d\n", warnings))

if (errors == 0) {
  cat("\nAll ATC code definitions are consistent!\n")
} else {
  cat("\nPlease review the errors above.\n")
}

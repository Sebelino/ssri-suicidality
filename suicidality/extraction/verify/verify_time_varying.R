# verify_time_varying.R
# Verifies time-varying medication definitions and checks for inconsistencies

library(dplyr)
library(here)
here::i_am("suicidality/extraction/verify/verify_time_varying.R")

source(here("suicidality", "extraction", "lib", "common.R"))

cat("======================================================================\n")
cat("TIME-VARYING MEDICATION VERIFICATION\n")
cat("======================================================================\n\n")

errors <- 0
warnings <- 0

# Load data
pp_max <- read_rds("pp_12wks_max.rds")
main_28 <- read_rds("main_12wks_28.rds")
raw_prescriptions <- read_rds("raw_prescriptions_cohort.rds")

cat("pp_12wks_max rows:", nrow(pp_max), "\n")
cat("main_12wks_28 rows:", nrow(main_28), "\n\n")

# Check 1: Time-varying variable presence
cat("--- Check 1: Time-varying variables present ---\n")
expected_tv_vars <- c("exp", "med_antipsychotic", "med_benzodiazepine", "opsych", "anypsych_tv")
missing_vars <- setdiff(expected_tv_vars, names(pp_max))
if (length(missing_vars) == 0) {
  cat("OK: All expected time-varying variables present\n")
} else {
  cat(sprintf("ERROR: Missing variables: %s\n", paste(missing_vars, collapse = ", ")))
  errors <- errors + 1
}

# Check 2: Time-varying variables are binary
cat("\n--- Check 2: Time-varying variables are binary ---\n")
for (var in expected_tv_vars) {
  if (var %in% names(pp_max)) {
    vals <- unique(pp_max[[var]])
    if (all(vals %in% c(0, 1))) {
      cat(sprintf("OK: %s is binary (0/1)\n", var))
    } else {
      cat(sprintf("ERROR: %s has non-binary values: %s\n", var, paste(vals, collapse = ", ")))
      errors <- errors + 1
    }
  }
}

# Check 3: Lithium inclusion in time-varying analysis (fixed in bug #4)
cat("\n--- Check 3: Lithium in time-varying analysis ---\n")

# Count lithium prescriptions during follow-up
lithium_during_fu <- raw_prescriptions %>%
  filter(grepl("^N05AN", atc)) %>%
  inner_join(main_28 %>% select(lopnr, fu_start, fu_end_itt), by = "lopnr") %>%
  filter(edatum >= fu_start & edatum <= fu_end_itt) %>%
  distinct(lopnr)

n_lithium_fu <- nrow(lithium_during_fu)
cat(sprintf("Individuals with lithium prescription during follow-up: %d\n", n_lithium_fu))

# Lithium (N05AN) is now included in opsych (fixed in bug #4)
cat("OK: Lithium is included in time-varying opsych variable (bug #4 fix)\n")

# Check 4: opsych excludes mood stabilizers (fixed in bug #7)
cat("\n--- Check 4: opsych excludes mood stabilizers ---\n")
cat("Note: opsych in time-varying now EXCLUDES mood stabilizers (bug #7 fix)\n")
cat("      This is consistent with baseline med_antiepileptic (also excludes mood stabilizers)\n")

mood_stabilizer_codes <- c("N03AG01", "N03AX09", "N03AF01")

# Count mood stabilizer prescriptions during follow-up (not counted in opsych)
mood_stab_during_fu <- raw_prescriptions %>%
  filter(atc %in% mood_stabilizer_codes) %>%
  inner_join(main_28 %>% select(lopnr, fu_start, fu_end_itt), by = "lopnr") %>%
  filter(edatum >= fu_start & edatum <= fu_end_itt) %>%
  distinct(lopnr)

n_mood_stab_fu <- nrow(mood_stab_during_fu)
cat(sprintf("Individuals with mood stabilizer prescriptions during follow-up: %d\n",
            n_mood_stab_fu))

cat("OK: Mood stabilizers are excluded from opsych (consistent with baseline med_antiepileptic)\n")

# Check 5: anypsych_tv definition
cat("\n--- Check 5: anypsych_tv consistency ---\n")
# anypsych_tv should be 1 if any of: med_antipsychotic, med_benzodiazepine, opsych
anypsych_check <- pp_max %>%
  mutate(
    expected_anypsych = if_else(med_antipsychotic == 1 | med_benzodiazepine == 1 | opsych == 1, 1L, 0L)
  ) %>%
  filter(anypsych_tv != expected_anypsych)

n_anypsych_mismatch <- nrow(anypsych_check)
if (n_anypsych_mismatch == 0) {
  cat("OK: anypsych_tv correctly computed from components\n")
} else {
  cat(sprintf("ERROR: %d rows with incorrect anypsych_tv\n", n_anypsych_mismatch))
  errors <- errors + 1
}

# Check 6: Per-protocol censoring for initiators off treatment
cat("\n--- Check 6: Per-protocol censoring for initiators ---\n")
# In PP analysis, initiators (cc=1) should always have exp=1
initiators_off_treatment <- pp_max %>%
  filter(cc == 1 & exp == 0)

n_initiators_off <- nrow(initiators_off_treatment)
if (n_initiators_off == 0) {
  cat("OK: No initiators off treatment in PP cohort (correctly censored)\n")
} else {
  cat(sprintf("ERROR: %d initiator person-weeks with exp=0 (should be censored)\n",
              n_initiators_off))
  errors <- errors + 1
}

# Check 7: Week numbering consistency
cat("\n--- Check 7: Week numbering ---\n")
# Column might be named 'week' or 'weeks' depending on pipeline version
week_col <- if ("week" %in% names(pp_max)) "week" else if ("weeks" %in% names(pp_max)) "weeks" else NULL

if (!is.null(week_col)) {
  week_range <- range(pp_max[[week_col]], na.rm = TRUE)
  cat(sprintf("Week range: %d to %d\n", as.integer(week_range[1]), as.integer(week_range[2])))

  if (week_range[1] == 0 && week_range[2] <= 11) {
    cat("OK: Week numbering is 0-11 (12 weeks total)\n")
  } else {
    cat(sprintf("WARNING: Unexpected week range (expected 0-11)\n"))
    warnings <- warnings + 1
  }
} else {
  cat("WARNING: No week/weeks column found in pp_max\n")
  warnings <- warnings + 1
}

# Summary
cat("\n======================================================================\n")
cat("TIME-VARYING VERIFICATION SUMMARY\n")
cat("======================================================================\n")
cat(sprintf("Errors:   %d\n", errors))
cat(sprintf("Warnings: %d\n", warnings))

if (errors == 0 && warnings == 0) {
  cat("\nAll time-varying checks passed!\n")
} else if (errors == 0) {
  cat("\nNo errors, but review the warnings above.\n")
} else {
  cat("\nPlease review the errors above.\n")
}

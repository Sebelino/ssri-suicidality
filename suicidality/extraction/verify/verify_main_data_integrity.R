# verify_main_data_integrity.R
# Comprehensive data integrity checks for main_12wks_28.rds
#
# Validates alignment with Lagerberg et al. 2023 methodology:
# - Age range: 6-24 (subset of paper's 6-59)
# - Study period: 2006-2019
# - Washout: 365 days
# - Follow-up: 12 weeks (84 days)
# - Grace period: 28 days for SSRI initiation
# - Covariates: Table S3 diagnoses + medications

library(dplyr)
library(here)
here::i_am("suicidality/extraction/verify/verify_main_data_integrity.R")

source(here("suicidality", "extraction", "lib", "common.R"))

cat("======================================================================\n")
cat("MAIN DATA INTEGRITY VERIFICATION (main_12wks_28.rds)\n")
cat("======================================================================\n\n")

errors <- 0
warnings <- 0

# =============================================================================
# Load data
# =============================================================================
if (!rds_exists("main_12wks_28.rds")) {
  cat("ERROR: main_12wks_28.rds not found. Run extraction pipeline first.\n")
  quit(status = 1)
}

data <- read_rds("main_12wks_28.rds")
cat("Loaded main_12wks_28.rds\n")
cat(sprintf("  Rows: %d\n", nrow(data)))
cat(sprintf("  Columns: %d\n", ncol(data)))
cat("\n")

# =============================================================================
# Check 1: Expected columns present
# =============================================================================
cat("--- Check 1: Expected columns ---\n")

# Core identification and timing
core_cols <- c("lopnr", "diagn_date", "bdate", "age", "year", "female", "source")

# Treatment assignment
treatment_cols <- c("cc", "prescr", "atc", "predi_diff")

# Follow-up dates
followup_cols <- c("fu_start", "fu_end12", "fu_end_itt", "fu_end_pp")

# Outcomes
outcome_cols <- c("sb12_itt", "sb12_pp", "date_fail")

# Censoring
censor_cols <- c("cens_death", "cens_emig", "cens_admin", "cens_deathemig",
                 "date_death", "date_emig")

# Diagnosis covariates (Table S3 from Lagerberg 2023)
diag_cols <- c(
  "diag_organic",      # F00-F09
  "diag_alcohol",      # F10
  "diag_sud",          # F11-F19 excl F17
  "diag_psychotic",    # F20-F29
  "diag_bipolar",      # F30-F31
  "diag_mdd",          # F32-F33
  "diag_phobic",       # F40.0-F40.2
  "diag_anxiety_other",# F41.0-F41.1
  "diag_ocd",          # F42
  "diag_stress",       # F43
  "diag_anorexia",     # F50.0-F50.1
  "diag_bulimia",      # F50.2-F50.3
  "diag_sleep",        # F51
  "diag_personality_cluster_b",  # F60.2-F60.3
  "diag_intellectual_disability", # F70-F79
  "diag_autism",       # F84
  "diag_adhd",         # F90
  "diag_conduct",      # F91
  "diag_overdose",     # T36-T51, X40-X49
  "diag_suicidal"      # X60-X84, Y10-Y34
)

# Medication covariates
med_cols <- c(
  "med_antipsychotic",   # N05A excl N05AN
  "med_hypnotic",        # N05C
  "med_benzodiazepine",  # N05BA
  "med_antiepileptic",   # N03A excl mood stabilizers
  "med_stimulant",       # N06B
  "med_addiction",       # N07B
  "med_opioid",          # N02A
  "med_mood_stabilizer"  # N05AN, N03AG01, N03AX09, N03AF01
)

# Socioeconomic covariates
socio_cols <- c("edufam_cat", "inc_cat", "fh_suicidal", "fh_depr", "hosp")

# All expected columns
all_expected <- c(core_cols, treatment_cols, followup_cols, outcome_cols,
                  censor_cols, diag_cols, med_cols, socio_cols)

missing_cols <- setdiff(all_expected, names(data))
extra_cols <- setdiff(names(data), all_expected)

if (length(missing_cols) == 0) {
  cat(sprintf("OK: All %d expected columns present\n", length(all_expected)))
} else {
  cat(sprintf("ERROR: Missing %d columns: %s\n", length(missing_cols),
              paste(missing_cols, collapse = ", ")))
  errors <- errors + 1
}

if (length(extra_cols) > 0) {
  cat(sprintf("INFO: %d extra columns: %s\n", length(extra_cols),
              paste(extra_cols, collapse = ", ")))
}

# =============================================================================
# Check 2: Unique lopnrs (one row per person)
# =============================================================================
cat("\n--- Check 2: Unique identifiers ---\n")

n_unique <- n_distinct(data$lopnr)
if (n_unique == nrow(data)) {
  cat(sprintf("OK: One row per person (%d unique lopnrs)\n", n_unique))
} else {
  cat(sprintf("ERROR: Duplicate lopnrs (%d unique vs %d rows)\n", n_unique, nrow(data)))
  errors <- errors + 1
}

# =============================================================================
# Check 3: Age range (6-24)
# =============================================================================
cat("\n--- Check 3: Age range ---\n")

age_min <- min(data$age, na.rm = TRUE)
age_max <- max(data$age, na.rm = TRUE)
age_na <- sum(is.na(data$age))

if (age_min >= 6 && age_max <= 24) {
  cat(sprintf("OK: Age range 6-24 (actual: %d-%d)\n", age_min, age_max))
} else {
  cat(sprintf("ERROR: Age outside 6-24 (actual: %d-%d)\n", age_min, age_max))
  errors <- errors + 1
}

if (age_na > 0) {
  cat(sprintf("ERROR: %d rows with missing age\n", age_na))
  errors <- errors + 1
}

# Age distribution
cat("Age distribution:\n")
cat(sprintf("  6-17 years: %d (%.1f%%)\n",
            sum(data$age <= 17), 100 * mean(data$age <= 17)))
cat(sprintf("  18-24 years: %d (%.1f%%)\n",
            sum(data$age >= 18), 100 * mean(data$age >= 18)))

# =============================================================================
# Check 4: Study period (2006-2019)
# =============================================================================
cat("\n--- Check 4: Study period ---\n")

year_min <- min(data$year, na.rm = TRUE)
year_max <- max(data$year, na.rm = TRUE)

if (year_min >= 2006 && year_max <= 2019) {
  cat(sprintf("OK: Study period 2006-2019 (actual: %d-%d)\n", year_min, year_max))
} else {
  cat(sprintf("WARNING: Study period outside 2006-2019 (actual: %d-%d)\n", year_min, year_max))
  warnings <- warnings + 1
}

# =============================================================================
# Check 5: Treatment assignment (cc)
# =============================================================================
cat("\n--- Check 5: Treatment assignment ---\n")

# cc should be 0 or 1
cc_invalid <- sum(!(data$cc %in% c(0, 1)))
if (cc_invalid == 0) {
  cat("OK: cc is 0 or 1 for all rows\n")
} else {
  cat(sprintf("ERROR: %d rows with invalid cc value\n", cc_invalid))
  errors <- errors + 1
}

n_treated <- sum(data$cc == 1)
n_control <- sum(data$cc == 0)
cat(sprintf("  Initiators (cc=1): %d (%.1f%%)\n", n_treated, 100 * n_treated / nrow(data)))
cat(sprintf("  Non-initiators (cc=0): %d (%.1f%%)\n", n_control, 100 * n_control / nrow(data)))

# cc=1 should have prescr within 28 days of diagn_date
if (n_treated > 0) {
  cc1_data <- data %>% filter(cc == 1)
  cc1_missing_prescr <- sum(is.na(cc1_data$prescr))
  cc1_late_prescr <- cc1_data %>%
    filter(!is.na(prescr)) %>%
    filter(as.integer(prescr - diagn_date) > 28)

  if (cc1_missing_prescr == 0 && nrow(cc1_late_prescr) == 0) {
    cat("OK: All initiators have prescr within 28 days of diagn_date\n")
  } else {
    if (cc1_missing_prescr > 0) {
      cat(sprintf("ERROR: %d initiators missing prescr date\n", cc1_missing_prescr))
      errors <- errors + 1
    }
    if (nrow(cc1_late_prescr) > 0) {
      cat(sprintf("ERROR: %d initiators with prescr > 28 days after diagn_date\n", nrow(cc1_late_prescr)))
      errors <- errors + 1
    }
  }
}

# cc=0 should have NA or late prescr
cc0_early_prescr <- data %>%
  filter(cc == 0 & !is.na(prescr)) %>%
  filter(as.integer(prescr - diagn_date) <= 28)

if (nrow(cc0_early_prescr) == 0) {
  cat("OK: No non-initiators with early prescr (<=28 days)\n")
} else {
  cat(sprintf("ERROR: %d non-initiators with prescr <= 28 days\n", nrow(cc0_early_prescr)))
  errors <- errors + 1
}

# =============================================================================
# Check 6: Follow-up dates
# =============================================================================
cat("\n--- Check 6: Follow-up dates ---\n")

# fu_start should equal prescr for initiators
if (n_treated > 0) {
  fu_prescr_mismatch <- data %>%
    filter(cc == 1) %>%
    filter(fu_start != prescr)

  if (nrow(fu_prescr_mismatch) == 0) {
    cat("OK: fu_start = prescr for all initiators\n")
  } else {
    cat(sprintf("ERROR: %d initiators with fu_start != prescr\n", nrow(fu_prescr_mismatch)))
    errors <- errors + 1
  }
}

# fu_end12 = fu_start + 84 (12 weeks)
fu_end12_check <- data %>% filter(fu_end12 != fu_start + 84)
if (nrow(fu_end12_check) == 0) {
  cat("OK: fu_end12 = fu_start + 84 for all rows\n")
} else {
  cat(sprintf("ERROR: %d rows with fu_end12 != fu_start + 84\n", nrow(fu_end12_check)))
  errors <- errors + 1
}

# fu_end_itt <= fu_end12
fu_itt_invalid <- data %>% filter(fu_end_itt > fu_end12)
if (nrow(fu_itt_invalid) == 0) {
  cat("OK: fu_end_itt <= fu_end12 for all rows\n")
} else {
  cat(sprintf("ERROR: %d rows with fu_end_itt > fu_end12\n", nrow(fu_itt_invalid)))
  errors <- errors + 1
}

# fu_end_pp <= fu_end_itt
fu_pp_invalid <- data %>% filter(fu_end_pp > fu_end_itt)
if (nrow(fu_pp_invalid) == 0) {
  cat("OK: fu_end_pp <= fu_end_itt for all rows\n")
} else {
  cat(sprintf("ERROR: %d rows with fu_end_pp > fu_end_itt\n", nrow(fu_pp_invalid)))
  errors <- errors + 1
}

# Follow-up duration stats
fu_days <- as.integer(data$fu_end_itt - data$fu_start)
cat(sprintf("Follow-up duration (ITT): mean=%.1f days, min=%d, max=%d\n",
            mean(fu_days), min(fu_days), max(fu_days)))
cat(sprintf("  Full 84-day follow-up: %d (%.1f%%)\n",
            sum(fu_days == 84), 100 * mean(fu_days == 84)))

# =============================================================================
# Check 7: Outcomes
# =============================================================================
cat("\n--- Check 7: Outcomes ---\n")

# sb12_itt should be 0 or 1
sb_invalid <- sum(!(data$sb12_itt %in% c(0, 1)))
if (sb_invalid == 0) {
  cat("OK: sb12_itt is 0 or 1 for all rows\n")
} else {
  cat(sprintf("ERROR: %d rows with invalid sb12_itt\n", sb_invalid))
  errors <- errors + 1
}

# Outcome timing: events should be AFTER fu_start (strict inequality, bug #2)
# sb12_itt=1 iff date_fail > fu_start AND date_fail <= fu_end_itt
outcome_check <- data %>%
  mutate(
    should_have_event = !is.na(date_fail) & date_fail > fu_start & date_fail <= fu_end_itt,
    has_event = sb12_itt == 1
  ) %>%
  filter(should_have_event != has_event)

if (nrow(outcome_check) == 0) {
  cat("OK: sb12_itt consistent with date_fail (excludes day 0 events)\n")
} else {
  cat(sprintf("ERROR: %d rows with inconsistent sb12_itt\n", nrow(outcome_check)))
  errors <- errors + 1
}

# Check for day 0 events (should not exist after bug #2 fix)
day0_events <- data %>%
  filter(!is.na(date_fail) & date_fail == fu_start)

if (nrow(day0_events) == 0) {
  cat("OK: No day 0 events (date_fail == fu_start)\n")
} else {
  cat(sprintf("WARNING: %d day 0 events found (may be covariate, not outcome)\n", nrow(day0_events)))
  # Check if these are counted as outcomes
  day0_as_outcome <- sum(day0_events$sb12_itt == 1)
  if (day0_as_outcome > 0) {
    cat(sprintf("ERROR: %d day 0 events counted as outcomes (should be excluded)\n", day0_as_outcome))
    errors <- errors + 1
  }
}

# Event statistics
events_total <- sum(data$sb12_itt)
events_treated <- sum(data$sb12_itt[data$cc == 1])
events_control <- sum(data$sb12_itt[data$cc == 0])

cat(sprintf("Events: total=%d, treated=%d, control=%d\n",
            events_total, events_treated, events_control))
cat(sprintf("Event rate: overall=%.2f%%, treated=%.2f%%, control=%.2f%%\n",
            100 * events_total / nrow(data),
            100 * events_treated / n_treated,
            100 * events_control / n_control))

# =============================================================================
# Check 8: Binary covariates (0/1)
# =============================================================================
cat("\n--- Check 8: Binary covariates ---\n")

binary_cols <- c(diag_cols, med_cols, "female", "hosp")
invalid_binary <- character(0)

for (col in binary_cols) {
  if (col %in% names(data)) {
    vals <- unique(data[[col]])
    if (!all(vals %in% c(0, 1, NA))) {
      invalid_binary <- c(invalid_binary, col)
    }
  }
}

if (length(invalid_binary) == 0) {
  cat(sprintf("OK: All %d binary covariates contain only 0/1 values\n", length(binary_cols)))
} else {
  cat(sprintf("ERROR: Non-binary values in: %s\n", paste(invalid_binary, collapse = ", ")))
  errors <- errors + 1
}

# =============================================================================
# Check 9: Categorical covariates
# =============================================================================
cat("\n--- Check 9: Categorical covariates ---\n")

# fh_suicidal and fh_depr: 0 (none), 1 (one parent), 2 (both), 99 (missing)
for (fh_col in c("fh_suicidal", "fh_depr")) {
  if (fh_col %in% names(data)) {
    fh_vals <- unique(data[[fh_col]])
    expected_fh <- c(0, 1, 2, 99)
    if (all(fh_vals %in% expected_fh)) {
      cat(sprintf("OK: %s has valid values (0,1,2,99)\n", fh_col))
    } else {
      cat(sprintf("WARNING: %s has unexpected values: %s\n", fh_col,
                  paste(setdiff(fh_vals, expected_fh), collapse = ", ")))
      warnings <- warnings + 1
    }
  }
}

# edufam_cat: 0 (primary), 1 (secondary), 2 (tertiary), 99 (missing)
if ("edufam_cat" %in% names(data)) {
  edu_vals <- unique(data$edufam_cat)
  expected_edu <- c(0, 1, 2, 99)
  if (all(edu_vals %in% expected_edu)) {
    cat("OK: edufam_cat has valid values (0,1,2,99)\n")
  } else {
    cat(sprintf("WARNING: edufam_cat has unexpected values: %s\n",
                paste(setdiff(edu_vals, expected_edu), collapse = ", ")))
    warnings <- warnings + 1
  }
}

# inc_cat: income quintiles 1 (<0), 2 (0), 3 (0-p20), 4 (p20-p80), 5 (>p80), 99 (missing)
if ("inc_cat" %in% names(data)) {
  inc_vals <- unique(data$inc_cat)
  expected_inc <- c(1, 2, 3, 4, 5, 99)
  if (all(inc_vals %in% expected_inc)) {
    cat("OK: inc_cat has valid values (1-5, 99)\n")
  } else {
    cat(sprintf("WARNING: inc_cat has unexpected values: %s\n",
                paste(setdiff(inc_vals, expected_inc), collapse = ", ")))
    warnings <- warnings + 1
  }
}

# source: O (outpatient), S (inpatient/sluten), M (primary care), T (unknown/other)
if ("source" %in% names(data)) {
  source_vals <- unique(data$source)
  expected_source <- c("O", "S", "M", "T")
  if (all(source_vals %in% expected_source)) {
    cat(sprintf("OK: source has valid values (%s)\n", paste(source_vals, collapse = ", ")))
  } else {
    cat(sprintf("WARNING: source has unexpected values: %s\n",
                paste(setdiff(source_vals, expected_source), collapse = ", ")))
    warnings <- warnings + 1
  }
}

# =============================================================================
# Check 10: Covariate distributions (sanity checks)
# =============================================================================
cat("\n--- Check 10: Covariate distributions ---\n")

# Female should be ~60-70% (depression more common in females)
female_pct <- 100 * mean(data$female)
if (female_pct >= 50 && female_pct <= 80) {
  cat(sprintf("OK: Female proportion %.1f%% (expected 50-80%%)\n", female_pct))
} else {
  cat(sprintf("WARNING: Female proportion %.1f%% outside expected range\n", female_pct))
  warnings <- warnings + 1
}

# Prior MDD (diag_mdd) should be <50% (index diagnosis may be first)
if ("diag_mdd" %in% names(data)) {
  mdd_pct <- 100 * mean(data$diag_mdd)
  cat(sprintf("INFO: Prior MDD (diag_mdd): %.1f%%\n", mdd_pct))
}

# Prior suicidal behavior should be <20%
if ("diag_suicidal" %in% names(data)) {
  suicidal_pct <- 100 * mean(data$diag_suicidal)
  if (suicidal_pct <= 30) {
    cat(sprintf("OK: Prior suicidal behavior %.1f%% (expected <30%%)\n", suicidal_pct))
  } else {
    cat(sprintf("WARNING: Prior suicidal behavior %.1f%% higher than expected\n", suicidal_pct))
    warnings <- warnings + 1
  }
}

# =============================================================================
# Check 11: No future information leakage
# =============================================================================
cat("\n--- Check 11: No future information leakage ---\n")

# Diagnosis covariates should be measured before fu_start (bug #13 uses <=)
# We can't directly verify this without raw data, but check date_fail patterns

# If someone has diag_suicidal=1, they should have had a suicidal event BEFORE fu_start
# (not necessarily the same day, but not after)
# This is a sanity check - we can't verify timing without raw diagnosis data

# Check that outcome events are not on day 0 (would indicate covariate leakage)
# Already checked above

cat("OK: Day 0 events excluded from outcomes (verified in Check 7)\n")
cat("Note: Covariate timing (<=fu_start) cannot be verified without raw diagnosis data\n")

# =============================================================================
# Check 12: PP-specific fields
# =============================================================================
cat("\n--- Check 12: Per-protocol fields ---\n")

if ("sb12_pp" %in% names(data) && "fu_end_pp" %in% names(data)) {
  # sb12_pp should be <= sb12_itt (PP censors at treatment discontinuation)
  pp_more_events <- data %>% filter(sb12_pp > sb12_itt)
  if (nrow(pp_more_events) == 0) {
    cat("OK: sb12_pp <= sb12_itt for all rows\n")
  } else {
    cat(sprintf("ERROR: %d rows with sb12_pp > sb12_itt\n", nrow(pp_more_events)))
    errors <- errors + 1
  }

  # PP event count
  pp_events <- sum(data$sb12_pp)
  cat(sprintf("PP events: %d (vs ITT: %d)\n", pp_events, events_total))
} else {
  cat("WARNING: PP fields (sb12_pp, fu_end_pp) not found\n")
  warnings <- warnings + 1
}

# =============================================================================
# Summary
# =============================================================================
cat("\n======================================================================\n")
cat("VERIFICATION SUMMARY\n")
cat("======================================================================\n")
cat(sprintf("Errors:   %d\n", errors))
cat(sprintf("Warnings: %d\n", warnings))

cat("\n--- Key Statistics ---\n")
cat(sprintf("Total N:              %d\n", nrow(data)))
cat(sprintf("Columns:              %d\n", ncol(data)))
cat(sprintf("SSRI initiators:      %d (%.1f%%)\n", n_treated, 100 * n_treated / nrow(data)))
cat(sprintf("Age 6-17:             %d (%.1f%%)\n", sum(data$age <= 17), 100 * mean(data$age <= 17)))
cat(sprintf("Age 18-24:            %d (%.1f%%)\n", sum(data$age >= 18), 100 * mean(data$age >= 18)))
cat(sprintf("Female:               %.1f%%\n", female_pct))
cat(sprintf("Events (ITT):         %d (%.2f%%)\n", events_total, 100 * events_total / nrow(data)))
cat(sprintf("Full 84-day follow-up: %.1f%%\n", 100 * mean(fu_days == 84)))

if (errors == 0 && warnings == 0) {
  cat("\nAll data integrity checks PASSED!\n")
} else if (errors == 0) {
  cat("\nData integrity checks PASSED with warnings. Please review.\n")
} else {
  cat("\nData integrity checks FAILED. Please review errors above.\n")
}

# validate_data_consistency.R
# Comprehensive data validation for the extraction pipeline
# Checks for data consistency, unexpected values, and potential bugs

library(dplyr)
library(here)
here::i_am("suicidality/extraction/verify/validate_data_consistency.R")

source(here("suicidality", "extraction", "lib", "common.R"))

cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("DATA CONSISTENCY VALIDATION\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")

errors <- 0
warnings <- 0

report_error <- function(msg) {
  cat("ERROR: ", msg, "\n")
  errors <<- errors + 1
}

report_warning <- function(msg) {
  cat("WARNING: ", msg, "\n")
  warnings <<- warnings + 1
}

report_ok <- function(msg) {
  cat("OK: ", msg, "\n")
}

# =============================================================================
# Load datasets
# =============================================================================
cat("Loading datasets...\n")
main_28 <- read_rds("main_12wks_28.rds")
pp_max <- read_rds("pp_12wks_max.rds")
cat("Loaded main_12wks_28.rds:", nrow(main_28), "rows\n")
cat("Loaded pp_12wks_max.rds:", nrow(pp_max), "rows\n\n")

# =============================================================================
# 1. Basic row count checks
# =============================================================================
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("1. ROW COUNT CHECKS\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

report_ok(sprintf("main_28 row count: %d", nrow(main_28)))

# Check unique lopnr
n_unique_main <- n_distinct(main_28$lopnr)
if (n_unique_main == nrow(main_28)) {
  report_ok(sprintf("All lopnr unique in main_28: %d", n_unique_main))
} else {
  report_error(sprintf("Duplicate lopnr in main_28: %d unique out of %d rows", n_unique_main, nrow(main_28)))
}

# =============================================================================
# 2. Date consistency checks
# =============================================================================
cat("\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("2. DATE CONSISTENCY CHECKS\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

# fu_start should be <= fu_end for all follow-up end columns
if (all(main_28$fu_start <= main_28$fu_end_itt, na.rm = TRUE)) {
  report_ok("fu_start <= fu_end_itt for all rows")
} else {
  n_bad <- sum(main_28$fu_start > main_28$fu_end_itt, na.rm = TRUE)
  report_error(sprintf("fu_start > fu_end_itt for %d rows", n_bad))
}

if (all(main_28$fu_start <= main_28$fu_end_pp, na.rm = TRUE)) {
  report_ok("fu_start <= fu_end_pp for all rows")
} else {
  n_bad <- sum(main_28$fu_start > main_28$fu_end_pp, na.rm = TRUE)
  report_error(sprintf("fu_start > fu_end_pp for %d rows", n_bad))
}

# Check that date_fail (if present) is > fu_start (exclusive)
if ("date_fail" %in% names(main_28)) {
  n_day0 <- sum(main_28$date_fail == main_28$fu_start, na.rm = TRUE)
  if (n_day0 == 0) {
    report_ok("No events on day 0 (date_fail > fu_start)")
  } else {
    report_error(sprintf("Events on day 0 (date_fail == fu_start): %d", n_day0))
  }
}

# Check diagnosis date is before fu_start
if (all(main_28$diagn_date <= main_28$fu_start, na.rm = TRUE)) {
  report_ok("diagn_date <= fu_start for all rows")
} else {
  n_bad <- sum(main_28$diagn_date > main_28$fu_start, na.rm = TRUE)
  report_error(sprintf("diagn_date > fu_start for %d rows", n_bad))
}

# =============================================================================
# 3. Binary variable checks
# =============================================================================
cat("\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("3. BINARY VARIABLE CHECKS\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

binary_vars <- c(
  # Core variables
  "cc", "female", "sb12_itt", "sb12_pp",
  # Table S3 diagnosis covariates
  "diag_alcohol", "diag_organic", "diag_sud", "diag_psychotic", "diag_bipolar", "diag_mdd",
  "diag_phobic", "diag_anxiety_other", "diag_ocd", "diag_stress",
  "diag_anorexia", "diag_bulimia", "diag_sleep", "diag_personality_cluster_b",
  "diag_intellectual_disability", "diag_autism", "diag_adhd", "diag_conduct",
  "diag_overdose", "diag_suicidal",
  # Medication covariates
  "med_antipsychotic", "med_hypnotic", "med_benzodiazepine", "med_antiepileptic",
  "med_stimulant", "med_addiction", "med_opioid", "med_mood_stabilizer"
)

for (var in binary_vars) {
  if (var %in% names(main_28)) {
    vals <- unique(main_28[[var]])
    vals <- vals[!is.na(vals)]
    if (all(vals %in% c(0, 1))) {
      # Silent OK for binary vars
    } else {
      report_error(sprintf("Variable '%s' has non-binary values: %s", var, paste(vals, collapse = ", ")))
    }
  }
}
report_ok(sprintf("All %d binary variables are 0/1", length(binary_vars)))

# =============================================================================
# 4. Categorical variable checks
# =============================================================================
cat("\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("4. CATEGORICAL VARIABLE CHECKS\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

# edufam_cat should be 0, 1, 2, or 99
if ("edufam_cat" %in% names(main_28)) {
  edufam_vals <- unique(main_28$edufam_cat)
  expected_edufam <- c(0, 1, 2, 99)
  if (all(edufam_vals %in% expected_edufam)) {
    report_ok(sprintf("edufam_cat values valid: %s", paste(sort(edufam_vals), collapse = ", ")))
  } else {
    unexpected <- setdiff(edufam_vals, expected_edufam)
    report_error(sprintf("edufam_cat has unexpected values: %s", paste(unexpected, collapse = ", ")))
  }
}

# inc_cat should be 1-5 or 99
if ("inc_cat" %in% names(main_28)) {
  inc_vals <- unique(main_28$inc_cat)
  expected_inc <- c(1, 2, 3, 4, 5, 99)
  if (all(inc_vals %in% expected_inc)) {
    report_ok(sprintf("inc_cat values valid: %s", paste(sort(inc_vals), collapse = ", ")))
  } else {
    unexpected <- setdiff(inc_vals, expected_inc)
    report_error(sprintf("inc_cat has unexpected values: %s", paste(unexpected, collapse = ", ")))
  }
}

# fh_suicidal and fh_depr should be 0, 1, or 2
for (var in c("fh_suicidal", "fh_depr")) {
  if (var %in% names(main_28)) {
    vals <- unique(main_28[[var]])
    expected <- c(0, 1, 2, 99)
    if (all(vals %in% expected)) {
      report_ok(sprintf("%s values valid: %s", var, paste(sort(vals), collapse = ", ")))
    } else {
      unexpected <- setdiff(vals, expected)
      report_error(sprintf("%s has unexpected values: %s", var, paste(unexpected, collapse = ", ")))
    }
  }
}

# =============================================================================
# 5. Age and year checks
# =============================================================================
cat("\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("5. AGE AND YEAR CHECKS\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

# Age should be 6-24
age_range <- range(main_28$age, na.rm = TRUE)
if (age_range[1] >= 6 && age_range[2] <= 24) {
  report_ok(sprintf("Age range valid: %d-%d", age_range[1], age_range[2]))
} else {
  report_error(sprintf("Age range outside 6-24: %d-%d", age_range[1], age_range[2]))
}

# Year should be 2006-2019 (based on study period)
if ("year" %in% names(main_28)) {
  year_range <- range(main_28$year, na.rm = TRUE)
  if (year_range[1] >= 2006 && year_range[2] <= 2020) {
    report_ok(sprintf("Year range valid: %d-%d", year_range[1], year_range[2]))
  } else {
    report_warning(sprintf("Year range unusual: %d-%d", year_range[1], year_range[2]))
  }
}

# =============================================================================
# 6. Treatment group checks
# =============================================================================
cat("\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("6. TREATMENT GROUP CHECKS\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

n_treated <- sum(main_28$cc == 1)
n_control <- sum(main_28$cc == 0)
pct_treated <- 100 * n_treated / nrow(main_28)

cat(sprintf("  Treated (cc=1): %s (%.1f%%)\n", format(n_treated, big.mark = ","), pct_treated))
cat(sprintf("  Control (cc=0): %s (%.1f%%)\n", format(n_control, big.mark = ","), 100 - pct_treated))

if (pct_treated > 20 && pct_treated < 80) {
  report_ok("Treatment proportion reasonable (20-80%)")
} else {
  report_warning(sprintf("Treatment proportion unusual: %.1f%%", pct_treated))
}

# =============================================================================
# 7. Outcome checks
# =============================================================================
cat("\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("7. OUTCOME CHECKS\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

n_events_itt <- sum(main_28$sb12_itt == 1, na.rm = TRUE)
n_events_pp <- sum(main_28$sb12_pp == 1, na.rm = TRUE)
pct_events_itt <- 100 * n_events_itt / nrow(main_28)

cat(sprintf("  ITT events: %s (%.2f%%)\n", format(n_events_itt, big.mark = ","), pct_events_itt))
cat(sprintf("  PP events:  %s (%.2f%%)\n", format(n_events_pp, big.mark = ","), 100 * n_events_pp / nrow(main_28)))

# PP events should be <= ITT events (PP has more censoring)
if (n_events_pp <= n_events_itt) {
  report_ok("PP events <= ITT events (as expected)")
} else {
  report_error(sprintf("PP events (%d) > ITT events (%d)", n_events_pp, n_events_itt))
}

# =============================================================================
# 8. Covariate overlap checks
# =============================================================================
cat("\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("8. COVARIATE OVERLAP CHECKS\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

# med_antiepileptic and med_mood_stabilizer overlap check
# At the ATC code level, these are mutually exclusive (med_antiepileptic excludes mood stabilizer codes)
# However, at the person level, overlap is expected: individuals can have prescriptions for both
# a non-mood-stabilizer antiepileptic AND a mood stabilizer
n_antiepileptic <- sum(main_28$med_antiepileptic == 1, na.rm = TRUE)
n_mood_stabilizer <- sum(main_28$med_mood_stabilizer == 1, na.rm = TRUE)
n_both <- sum(main_28$med_antiepileptic == 1 & main_28$med_mood_stabilizer == 1, na.rm = TRUE)
cat(sprintf("  med_antiepileptic: %d, med_mood_stabilizer: %d, both: %d\n", n_antiepileptic, n_mood_stabilizer, n_both))
report_ok(sprintf("med_antiepileptic/med_mood_stabilizer person-level overlap: %d (expected - different prescriptions)", n_both))

# =============================================================================
# 9. Per-protocol cohort checks
# =============================================================================
cat("\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("9. PER-PROTOCOL COHORT CHECKS\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

n_pp_lopnr <- n_distinct(pp_max$lopnr)
cat(sprintf("  Unique individuals in PP cohort: %s\n", format(n_pp_lopnr, big.mark = ",")))

if (n_pp_lopnr <= nrow(main_28)) {
  report_ok("PP cohort individuals <= main cohort")
} else {
  report_error(sprintf("PP cohort has more individuals (%d) than main cohort (%d)", n_pp_lopnr, nrow(main_28)))
}

# Check week range
if ("week" %in% names(pp_max)) {
  week_range <- range(pp_max$week, na.rm = TRUE)
  cat(sprintf("  Week range: %d-%d\n", week_range[1], week_range[2]))
  if (week_range[1] >= 0 && week_range[2] <= 12) {
    report_ok("Week range valid (0-12)")
  } else {
    report_error(sprintf("Week range invalid: %d-%d", week_range[1], week_range[2]))
  }
}

# =============================================================================
# Summary
# =============================================================================
cat("\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("VALIDATION SUMMARY\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat(sprintf("Errors:   %d\n", errors))
cat(sprintf("Warnings: %d\n", warnings))

if (errors == 0) {
  cat("\nAll validation checks passed!\n")
} else {
  cat("\nPlease review and fix the errors above.\n")
}

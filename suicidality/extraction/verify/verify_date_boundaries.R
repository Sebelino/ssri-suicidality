# verify_date_boundaries.R
# Verifies date boundary consistency in outcome and covariate definitions

library(dplyr)
library(here)
here::i_am("suicidality/extraction/verify/verify_date_boundaries.R")

source(here("suicidality", "extraction", "lib", "common.R"))

cat("======================================================================\n")
cat("DATE BOUNDARY VERIFICATION\n")
cat("======================================================================\n\n")

errors <- 0
warnings <- 0

# Load data
main_28 <- read_rds("main_12wks_28.rds")
dia_all_28 <- read_rds("dia_all_28.rds")

cat("main_12wks_28 rows:", nrow(main_28), "\n")
cat("dia_all_28 rows:", nrow(dia_all_28), "\n\n")

# Check 1: Events on day 0 (date_fail == fu_start)
# Note: Day 0 events should NOT occur - outcomes require fu_start < date_fail (strict inequality)
# This ensures we count NEW suicidal behavior AFTER follow-up starts, not same-day events
cat("--- Check 1: Events on day 0 ---\n")

events_day0 <- main_28 %>%
  filter(!is.na(date_fail)) %>%
  filter(date_fail == fu_start)

n_day0 <- nrow(events_day0)
cat(sprintf("Events where date_fail == fu_start: %d\n", n_day0))

if (n_day0 == 0) {
  cat("OK: No day 0 events (outcomes correctly exclude same-day events)\n")
} else {
  cat("ERROR: Day 0 events found - outcomes should use strict inequality (fu_start < date_fail)\n")
  errors <- errors + 1
}

# Check 2: Events before fu_start
cat("\n--- Check 2: Events before fu_start ---\n")

events_before_start <- main_28 %>%
  filter(!is.na(date_fail)) %>%
  filter(date_fail < fu_start)

n_before_start <- nrow(events_before_start)
cat(sprintf("Events where date_fail < fu_start: %d\n", n_before_start))

if (n_before_start == 0) {
  cat("OK: No events before fu_start\n")
} else {
  cat("ERROR: Events before fu_start found\n")
  errors <- errors + 1
}

# Check 3: Events after fu_end_itt
cat("\n--- Check 3: Events after fu_end ---\n")

events_after_end_itt <- main_28 %>%
  filter(!is.na(date_fail)) %>%
  filter(date_fail > fu_end_itt)

n_after_end_itt <- nrow(events_after_end_itt)
cat(sprintf("Events where date_fail > fu_end_itt: %d\n", n_after_end_itt))

if (n_after_end_itt == 0) {
  cat("OK: No events after fu_end_itt\n")
} else {
  cat("ERROR: Events after fu_end_itt found\n")
  errors <- errors + 1
}

# Check 4: Outcome flags consistency with date_fail
cat("\n--- Check 4: Outcome flag consistency ---\n")

# sb12_itt should be 1 iff date_fail is within (fu_start, fu_end_itt] (exclusive start, inclusive end)
# This ensures day 0 events are NOT counted as outcomes
outcome_consistency <- main_28 %>%
  mutate(
    should_have_event = !is.na(date_fail) & date_fail > fu_start & date_fail <= fu_end_itt,
    has_event = sb12_itt == 1
  )

inconsistent_itt <- outcome_consistency %>%
  filter(should_have_event != has_event)

n_inconsistent_itt <- nrow(inconsistent_itt)
cat(sprintf("Inconsistent sb12_itt flags: %d\n", n_inconsistent_itt))

if (n_inconsistent_itt == 0) {
  cat("OK: sb12_itt flags are consistent with date_fail\n")
} else {
  cat("ERROR: sb12_itt flags inconsistent with date_fail\n")
  errors <- errors + 1
  # Show examples
  cat("Examples of inconsistencies:\n")
  inconsistent_itt %>%
    select(lopnr, fu_start, fu_end_itt, date_fail, sb12_itt, should_have_event) %>%
    head(5) %>%
    print()
}

# Check 5: PP outcome consistency
cat("\n--- Check 5: PP outcome flag consistency ---\n")

# PP outcome: date_fail within (fu_start, fu_end_pp] (exclusive start, inclusive end)
# This ensures day 0 events are NOT counted as outcomes
outcome_pp_consistency <- main_28 %>%
  mutate(
    should_have_event_pp = !is.na(date_fail) & date_fail > fu_start & date_fail <= fu_end_pp,
    has_event_pp = sb12_pp == 1
  )

inconsistent_pp <- outcome_pp_consistency %>%
  filter(should_have_event_pp != has_event_pp)

n_inconsistent_pp <- nrow(inconsistent_pp)
cat(sprintf("Inconsistent sb12_pp flags: %d\n", n_inconsistent_pp))

if (n_inconsistent_pp == 0) {
  cat("OK: sb12_pp flags are consistent with date_fail and fu_end_pp\n")
} else {
  cat("ERROR: sb12_pp flags inconsistent\n")
  errors <- errors + 1
}

# Check 6: Follow-up end dates are reasonable
cat("\n--- Check 6: Follow-up duration ---\n")

fu_stats <- main_28 %>%
  mutate(
    fu_days_itt = as.integer(fu_end_itt - fu_start),
    fu_days_pp = as.integer(fu_end_pp - fu_start)
  ) %>%
  summarise(
    min_itt = min(fu_days_itt),
    max_itt = max(fu_days_itt),
    mean_itt = mean(fu_days_itt),
    min_pp = min(fu_days_pp),
    max_pp = max(fu_days_pp),
    mean_pp = mean(fu_days_pp)
  )

cat(sprintf("ITT follow-up days: min=%d, max=%d, mean=%.1f\n",
            fu_stats$min_itt, fu_stats$max_itt, fu_stats$mean_itt))
cat(sprintf("PP follow-up days:  min=%d, max=%d, mean=%.1f\n",
            fu_stats$min_pp, fu_stats$max_pp, fu_stats$mean_pp))

# 12 weeks = 84 days
if (fu_stats$max_itt <= 84) {
  cat("OK: ITT follow-up <= 84 days (12 weeks)\n")
} else {
  cat(sprintf("WARNING: ITT follow-up exceeds 84 days (max=%d)\n", fu_stats$max_itt))
  warnings <- warnings + 1
}

if (fu_stats$min_itt >= 0) {
  cat("OK: ITT follow-up >= 0 days\n")
} else {
  cat(sprintf("ERROR: Negative ITT follow-up (min=%d)\n", fu_stats$min_itt))
  errors <- errors + 1
}

# Check 7: Same-day hospitalizations included in covariate
# Note: Unlike outcomes, covariates SHOULD include same-day events (bug #13/#19)
# Hospitalizations on fu_start should be counted in hosp covariate
cat("\n--- Check 7: Same-day hospitalizations in covariate ---\n")

raw_hosp <- read_rds("raw_hospitalization.rds")
cat("raw_hospitalization rows:", nrow(raw_hosp), "\n")

# Find individuals with inpatient hospitalization on fu_start
sameday_hosp <- raw_hosp %>%
  filter(source == "S") %>%
  inner_join(main_28 %>% select(lopnr, fu_start, hosp), by = "lopnr") %>%
  filter(diagn_date == fu_start) %>%
  distinct(lopnr, fu_start, hosp)

n_sameday_hosp <- nrow(sameday_hosp)
cat(sprintf("Individuals with hospitalization on fu_start: %d\n", n_sameday_hosp))

if (n_sameday_hosp > 0) {
  # Check if these are included in hosp covariate
  n_included <- sum(sameday_hosp$hosp == 1)
  n_excluded <- sum(sameday_hosp$hosp == 0)

  cat(sprintf("  Included in hosp covariate (hosp=1): %d\n", n_included))
  cat(sprintf("  Excluded from hosp covariate (hosp=0): %d\n", n_excluded))

  if (n_excluded == 0) {
    cat("OK: All same-day hospitalizations are included in hosp covariate\n")
  } else {
    cat("ERROR: Same-day hospitalizations excluded - should use <= not < in date filter\n")
    errors <- errors + 1
  }
} else {
  cat("OK: No same-day hospitalizations to check (or none in cohort)\n")
}

# Summary
cat("\n======================================================================\n")
cat("DATE BOUNDARY VERIFICATION SUMMARY\n")
cat("======================================================================\n")
cat(sprintf("Errors:   %d\n", errors))
cat(sprintf("Warnings: %d\n", warnings))

if (errors == 0) {
  cat("\nAll date boundary checks passed!\n")
} else {
  cat("\nPlease review the errors above.\n")
}

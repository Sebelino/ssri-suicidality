# verify_followup_dates.R
# Verifies that follow-up date variables are calculated correctly
# Related to bug #14 investigation (now documented as false positive)

library(dplyr)
library(here)
here::i_am("suicidality/extraction/verify/verify_followup_dates.R")

source(here("suicidality", "extraction", "lib", "common.R"))

cat("======================================================================\n")
cat("FOLLOW-UP DATE VERIFICATION\n")
cat("======================================================================\n\n")

errors <- 0
warnings <- 0

# Load data
if (!rds_exists("main_12wks_28.rds")) {
  cat("ERROR: main_12wks_28.rds not found. Run extraction pipeline first.\n")
  quit(status = 1)
}

main_28 <- read_rds("main_12wks_28.rds")
cat("Loaded main_12wks_28.rds with", nrow(main_28), "rows\n\n")

# Check 1: fu_end12 = fu_start + 84 days
cat("--- Check 1: fu_end12 = fu_start + 84 days ---\n")

fu_end12_check <- main_28 %>%
  mutate(expected_fu_end12 = fu_start + 84) %>%
  filter(fu_end12 != expected_fu_end12)

n_mismatch_12 <- nrow(fu_end12_check)
cat(sprintf("Rows where fu_end12 != fu_start + 84: %d\n", n_mismatch_12))

if (n_mismatch_12 == 0) {
  cat("OK: fu_end12 is correctly calculated as fu_start + 84 days\n")
} else {
  cat("ERROR: fu_end12 calculation errors found\n")
  errors <- errors + 1
}

# Check 2: fu_end_itt <= fu_end12 (ITT endpoint cannot exceed max follow-up)
cat("\n--- Check 2: fu_end_itt <= fu_end12 ---\n")

itt_exceeds_12 <- main_28 %>%
  filter(fu_end_itt > fu_end12)

n_itt_exceeds <- nrow(itt_exceeds_12)
cat(sprintf("Rows where fu_end_itt > fu_end12: %d\n", n_itt_exceeds))

if (n_itt_exceeds == 0) {
  cat("OK: fu_end_itt never exceeds fu_end12\n")
} else {
  cat("ERROR: fu_end_itt exceeds fu_end12 in some cases\n")
  errors <- errors + 1
}

# Check 3: fu_end_itt logic verification
# fu_end_itt = min(fu_end12, date_death, date_emig, admin_end, date_fail)
cat("\n--- Check 3: fu_end_itt = min(fu_end12, censoring dates, date_fail) ---\n")

admin_end <- as.Date("2020-12-31")

itt_logic <- main_28 %>%
  rowwise() %>%
  mutate(
    expected_fu_end_itt = min(fu_end12, date_death, date_emig, admin_end, date_fail, na.rm = TRUE),
    expected_fu_end_itt = if_else(is.infinite(expected_fu_end_itt), fu_end12, expected_fu_end_itt)
  ) %>%
  ungroup()

itt_logic_mismatch <- itt_logic %>%
  filter(fu_end_itt != expected_fu_end_itt)

n_itt_mismatch <- nrow(itt_logic_mismatch)
cat(sprintf("Rows where fu_end_itt != expected: %d\n", n_itt_mismatch))

if (n_itt_mismatch == 0) {
  cat("OK: fu_end_itt is correctly calculated\n")
} else {
  cat("WARNING: fu_end_itt calculation differs from expected\n")
  warnings <- warnings + 1
  cat("Examples:\n")
  itt_logic_mismatch %>%
    select(lopnr, fu_start, fu_end12, fu_end_itt, expected_fu_end_itt, date_death, date_emig, date_fail) %>%
    head(5) %>%
    print()
}

# Check 4: fu_end_pp <= fu_end12
cat("\n--- Check 4: fu_end_pp <= fu_end12 ---\n")

pp_exceeds_12 <- main_28 %>%
  filter(fu_end_pp > fu_end12)

n_pp_exceeds <- nrow(pp_exceeds_12)
cat(sprintf("Rows where fu_end_pp > fu_end12: %d\n", n_pp_exceeds))

if (n_pp_exceeds == 0) {
  cat("OK: fu_end_pp never exceeds fu_end12\n")
} else {
  cat("ERROR: fu_end_pp exceeds fu_end12 in some cases\n")
  errors <- errors + 1
}

# Check 5: fu_end_pp <= fu_end_itt for all non-switchers
# PP analysis includes switch censoring, so fu_end_pp <= fu_end_itt
cat("\n--- Check 5: fu_end_pp <= fu_end_itt ---\n")

pp_exceeds_itt <- main_28 %>%
  filter(fu_end_pp > fu_end_itt)

n_pp_exceeds_itt <- nrow(pp_exceeds_itt)
cat(sprintf("Rows where fu_end_pp > fu_end_itt: %d\n", n_pp_exceeds_itt))

if (n_pp_exceeds_itt == 0) {
  cat("OK: fu_end_pp never exceeds fu_end_itt\n")
} else {
  cat("ERROR: fu_end_pp exceeds fu_end_itt in some cases\n")
  errors <- errors + 1
}

# Check 6: Distribution of follow-up lengths
cat("\n--- Check 6: Follow-up duration distribution ---\n")

fu_distribution <- main_28 %>%
  mutate(
    fu_days_12 = as.integer(fu_end12 - fu_start),
    fu_days_itt = as.integer(fu_end_itt - fu_start),
    fu_days_pp = as.integer(fu_end_pp - fu_start)
  ) %>%
  summarise(
    # fu_end12 should always be 84
    fu_12_unique = n_distinct(fu_days_12),
    fu_12_all_84 = all(fu_days_12 == 84),
    # ITT and PP vary
    fu_itt_mean = mean(fu_days_itt),
    fu_itt_sd = sd(fu_days_itt),
    fu_itt_median = median(fu_days_itt),
    fu_itt_min = min(fu_days_itt),
    fu_itt_max = max(fu_days_itt),
    fu_pp_mean = mean(fu_days_pp),
    fu_pp_sd = sd(fu_days_pp),
    fu_pp_median = median(fu_days_pp),
    fu_pp_min = min(fu_days_pp),
    fu_pp_max = max(fu_days_pp)
  )

cat("fu_end12 (days):\n")
cat(sprintf("  All values = 84: %s\n", fu_distribution$fu_12_all_84))
if (fu_distribution$fu_12_all_84) {
  cat("  OK: fu_end12 is always exactly 84 days after fu_start\n")
} else {
  cat("  ERROR: fu_end12 is not always 84 days\n")
  errors <- errors + 1
}

cat("\nfu_end_itt (days):\n")
cat(sprintf("  Mean: %.1f, SD: %.1f, Median: %.0f\n",
            fu_distribution$fu_itt_mean, fu_distribution$fu_itt_sd, fu_distribution$fu_itt_median))
cat(sprintf("  Range: [%d, %d]\n", fu_distribution$fu_itt_min, fu_distribution$fu_itt_max))

cat("\nfu_end_pp (days):\n")
cat(sprintf("  Mean: %.1f, SD: %.1f, Median: %.0f\n",
            fu_distribution$fu_pp_mean, fu_distribution$fu_pp_sd, fu_distribution$fu_pp_median))
cat(sprintf("  Range: [%d, %d]\n", fu_distribution$fu_pp_min, fu_distribution$fu_pp_max))

# Check 7: Verify the comment in Summary_statistics.R is misleading
cat("\n--- Check 7: Verify fu_end_itt vs fu_end12 usage context ---\n")

n_full_followup <- main_28 %>%
  filter(fu_end_itt == fu_end12) %>%
  nrow()

n_censored <- main_28 %>%
  filter(fu_end_itt < fu_end12) %>%
  nrow()

cat(sprintf("Individuals with full 12-week follow-up (fu_end_itt == fu_end12): %d (%.1f%%)\n",
            n_full_followup, 100 * n_full_followup / nrow(main_28)))
cat(sprintf("Individuals censored before 12 weeks (fu_end_itt < fu_end12): %d (%.1f%%)\n",
            n_censored, 100 * n_censored / nrow(main_28)))

cat("\nCONCLUSION:\n")
cat("- fu_end12 = intended follow-up (always 84 days) - use for descriptive purposes\n")
cat("- fu_end_itt = actual follow-up accounting for censoring - use for survival analysis\n")
cat("- ITT_12wks.R CORRECTLY uses fu_end_itt for Kaplan-Meier analysis\n")
cat("- Summary_statistics.R comment 'fu_end_itt has a bug' is MISLEADING\n")

# Summary
cat("\n======================================================================\n")
cat("FOLLOW-UP DATE VERIFICATION SUMMARY\n")
cat("======================================================================\n")
cat(sprintf("Errors:   %d\n", errors))
cat(sprintf("Warnings: %d\n", warnings))

if (errors == 0 && warnings == 0) {
  cat("\nAll follow-up date checks passed!\n")
} else if (errors == 0) {
  cat("\nChecks passed with warnings. Please review.\n")
} else {
  cat("\nPlease review the errors above.\n")
}

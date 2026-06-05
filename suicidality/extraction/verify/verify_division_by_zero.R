# verify_division_by_zero.R
# Verifies that division by zero scenarios are unlikely in the analysis
#
# Checks for bugs A9 and A10:
# - A9: Division by zero in risk ratio calculations
# - A10: Division by zero in adherence weight calculations

library(dplyr)
library(here)
here::i_am("suicidality/extraction/verify/verify_division_by_zero.R")

source(here("suicidality", "extraction", "lib", "common.R"))

cat("======================================================================\n")
cat("DIVISION BY ZERO VERIFICATION\n")
cat("======================================================================\n\n")

errors <- 0
warnings <- 0

# Load data
main_28 <- read_rds("main_12wks_28.rds")
cat("main_12wks_28 rows:", nrow(main_28), "\n\n")

# =============================================================================
# Check 1: Risk ratio denominator (A9) - events in control group
# =============================================================================
cat("--- Check 1: Events in control group (A9 - risk ratio denominator) ---\n")

n_control <- sum(main_28$cc == 0)
events_control <- sum(main_28$cc == 0 & main_28$sb12_itt == 1)
risk_control <- events_control / n_control

cat(sprintf("Control group N: %d\n", n_control))
cat(sprintf("Events in control: %d\n", events_control))
cat(sprintf("Risk in control: %.4f%%\n", risk_control * 100))

if (events_control == 0) {
  cat("ERROR: Zero events in control group - risk ratio undefined!\n")
  errors <- errors + 1
} else if (events_control < 10) {
  cat("WARNING: Very few events in control group - risk ratio unstable\n")
  warnings <- warnings + 1
} else {
  cat("OK: Sufficient events in control group for stable risk ratio\n")
}

# =============================================================================
# Check 2: Risk ratio numerator (A9) - events in treated group
# =============================================================================
cat("\n--- Check 2: Events in treated group (A9 - log(risk) calculation) ---\n")

n_treated <- sum(main_28$cc == 1)
events_treated <- sum(main_28$cc == 1 & main_28$sb12_itt == 1)
risk_treated <- events_treated / n_treated

cat(sprintf("Treated group N: %d\n", n_treated))
cat(sprintf("Events in treated: %d\n", events_treated))
cat(sprintf("Risk in treated: %.4f%%\n", risk_treated * 100))

if (events_treated == 0) {
  cat("ERROR: Zero events in treated group - log(risk) undefined!\n")
  errors <- errors + 1
} else if (events_treated < 10) {
  cat("WARNING: Very few events in treated group - estimates unstable\n")
  warnings <- warnings + 1
} else {
  cat("OK: Sufficient events in treated group\n")
}

# =============================================================================
# Check 3: Subgroup analyses - age stratified
# =============================================================================
cat("\n--- Check 3: Age-stratified subgroups (A9 - subgroup risk ratios) ---\n")

subgroups <- list(
  "Age 6-17" = main_28$age >= 6 & main_28$age <= 17,
  "Age 18-24" = main_28$age >= 18 & main_28$age <= 24
)

for (name in names(subgroups)) {
  subset <- main_28[subgroups[[name]], ]
  n_sub_control <- sum(subset$cc == 0)
  n_sub_treated <- sum(subset$cc == 1)
  events_sub_control <- sum(subset$cc == 0 & subset$sb12_itt == 1)
  events_sub_treated <- sum(subset$cc == 1 & subset$sb12_itt == 1)

  cat(sprintf("\n%s:\n", name))
  cat(sprintf("  Control: N=%d, events=%d\n", n_sub_control, events_sub_control))
  cat(sprintf("  Treated: N=%d, events=%d\n", n_sub_treated, events_sub_treated))

  if (events_sub_control == 0 || events_sub_treated == 0) {
    cat("  WARNING: Zero events in one group - subgroup analysis at risk\n")
    warnings <- warnings + 1
  } else if (events_sub_control < 10 || events_sub_treated < 10) {
    cat("  WARNING: Few events - subgroup estimates may be unstable\n")
    warnings <- warnings + 1
  } else {
    cat("  OK: Sufficient events for subgroup analysis\n")
  }
}

# =============================================================================
# Check 4: Bootstrap sample simulation (A9)
# =============================================================================
cat("\n--- Check 4: Bootstrap zero-event probability (A9) ---\n")

# Simulate bootstrap sampling to estimate probability of zero events
set.seed(42)
n_bootstrap <- 1000
zero_control_count <- 0
zero_treated_count <- 0

for (i in 1:n_bootstrap) {
  # Resample with replacement
  boot_idx <- sample(nrow(main_28), replace = TRUE)
  boot_data <- main_28[boot_idx, ]

  boot_events_control <- sum(boot_data$cc == 0 & boot_data$sb12_itt == 1)
  boot_events_treated <- sum(boot_data$cc == 1 & boot_data$sb12_itt == 1)

  if (boot_events_control == 0) zero_control_count <- zero_control_count + 1
  if (boot_events_treated == 0) zero_treated_count <- zero_treated_count + 1
}

cat(sprintf("Simulated %d bootstrap samples\n", n_bootstrap))
cat(sprintf("Zero events in control: %d samples (%.2f%%)\n",
            zero_control_count, 100 * zero_control_count / n_bootstrap))
cat(sprintf("Zero events in treated: %d samples (%.2f%%)\n",
            zero_treated_count, 100 * zero_treated_count / n_bootstrap))

if (zero_control_count > 0 || zero_treated_count > 0) {
  cat("WARNING: Bootstrap samples can have zero events - division by zero possible\n")
  warnings <- warnings + 1
} else {
  cat("OK: No zero-event bootstrap samples in simulation\n")
}

# =============================================================================
# Check 5: PP analysis weights (A10)
# =============================================================================
cat("\n--- Check 5: PP analysis weight denominators (A10) ---\n")

pp_file <- file.path(rds_output_dir(), "pp_12wks_max.rds")
if (file.exists(pp_file)) {
  pp_data <- read_rds("pp_12wks_max.rds")
  cat(sprintf("pp_12wks_max rows: %d\n", nrow(pp_data)))

  # Check for extreme values that could lead to zero cumulative products
  # The adherence model uses time-varying covariates

  # Check if there are any extreme week values
  cat(sprintf("Week range: %d to %d\n", min(pp_data$weeks), max(pp_data$weeks)))

  # Check cc distribution
  cc_dist <- table(pp_data$cc)
  cat(sprintf("CC distribution: 0=%d, 1=%d\n", cc_dist["0"], cc_dist["1"]))

  cat("Note: Full verification of weight denominators requires running the PP analysis\n")
  cat("The ipweight() function should be checked for extreme probability values\n")
} else {
  cat("INFO: pp_12wks_max.rds not found - skipping PP weight check\n")
}

# =============================================================================
# Summary
# =============================================================================
cat("\n======================================================================\n")
cat("DIVISION BY ZERO VERIFICATION SUMMARY\n")
cat("======================================================================\n")
cat(sprintf("Errors:   %d\n", errors))
cat(sprintf("Warnings: %d\n", warnings))

if (errors == 0 && warnings == 0) {
  cat("\nAll division by zero checks passed!\n")
  cat("Risk of division by zero is low with current data.\n")
} else if (errors == 0) {
  cat("\nNo critical errors but warnings found.\n")
  cat("Consider adding defensive checks to handle edge cases.\n")
} else {
  cat("\nCritical errors found - division by zero will occur!\n")
}

cat("\nRecommendation:\n")
cat("Even if current data is safe, add defensive checks for:\n")
cat("- Future data with different event rates\n")
cat("- Subgroup analyses with smaller samples\n")
cat("- Bootstrap samples that may have zero events by chance\n")

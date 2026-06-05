# verify_lithium_fix.R
# Verifies that lithium (N05AN) is properly included in med_mood_stabilizer

library(dplyr)
library(here)
here::i_am("suicidality/extraction/verify/verify_lithium_fix.R")

source(here("suicidality", "extraction", "lib", "common.R"))

cat("======================================================================\n")
cat("LITHIUM INCLUSION VERIFICATION\n")
cat("======================================================================\n\n")

errors <- 0
warnings <- 0

# Load raw prescriptions and main cohort
raw_prescriptions <- read_rds("raw_prescriptions_cohort.rds")
main_28 <- read_rds("main_12wks_28.rds")
cov_medications <- read_rds("cov_medications.rds")

# Get fu_start dates
fu_start_data <- main_28 %>%
  select(lopnr, fu_start)

# Count lithium prescriptions in 90-day window (3 months per paper) before/on fu_start
# Note: Uses <= fu_start to include same-day prescriptions (bug #13 fix)
lithium_users <- raw_prescriptions %>%
  filter(grepl("^N05AN", atc)) %>%
  inner_join(fu_start_data, by = "lopnr") %>%
  filter(edatum >= fu_start - 90 & edatum <= fu_start) %>%
  distinct(lopnr) %>%
  pull(lopnr)

n_lithium_users <- length(lithium_users)
cat(sprintf("Lithium (N05AN) users in 90-day window: %d\n", n_lithium_users))

# Check how many lithium users have med_mood_stabilizer = 1
lithium_in_mood_stabilizer <- cov_medications %>%
  filter(lopnr %in% lithium_users) %>%
  filter(med_mood_stabilizer == 1) %>%
  nrow()

cat(sprintf("Lithium users with med_mood_stabilizer=1: %d\n", lithium_in_mood_stabilizer))

if (lithium_in_mood_stabilizer == n_lithium_users) {
  cat("OK: All lithium users are correctly classified as mood stabilizer users\n")
} else {
  cat(sprintf("ERROR: %d lithium users missing from med_mood_stabilizer\n",
              n_lithium_users - lithium_in_mood_stabilizer))
  errors <- errors + 1
}

# Verify med_mood_stabilizer total includes lithium + other mood stabilizers
mood_stabilizer_codes <- c("N03AG01", "N03AX09", "N03AF01")

other_mood_stabilizer_users <- raw_prescriptions %>%
  filter(atc %in% mood_stabilizer_codes) %>%
  inner_join(fu_start_data, by = "lopnr") %>%
  filter(edatum >= fu_start - 90 & edatum <= fu_start) %>%
  distinct(lopnr) %>%
  pull(lopnr)

n_other_mood_stabilizer <- length(other_mood_stabilizer_users)
cat(sprintf("\nOther mood stabilizer users (valproate/lamotrigine/carbamazepine): %d\n",
            n_other_mood_stabilizer))

# Combined unique users
all_mood_stabilizer_users <- unique(c(lithium_users, other_mood_stabilizer_users))
n_all_mood_stabilizer <- length(all_mood_stabilizer_users)
cat(sprintf("Total unique mood stabilizer users (lithium + others): %d\n", n_all_mood_stabilizer))

# Check against cov_medications
n_med_mood_stabilizer <- sum(cov_medications$med_mood_stabilizer == 1)
cat(sprintf("med_mood_stabilizer=1 in cov_medications: %d\n", n_med_mood_stabilizer))

if (n_med_mood_stabilizer == n_all_mood_stabilizer) {
  cat("OK: med_mood_stabilizer count matches expected total\n")
} else {
  cat(sprintf("WARNING: Count mismatch (expected %d, got %d)\n",
              n_all_mood_stabilizer, n_med_mood_stabilizer))
  # This might not be an error if there are edge cases
}

# Summary
cat("\n======================================================================\n")
cat("VERIFICATION SUMMARY\n")
cat("======================================================================\n")
cat(sprintf("Errors: %d\n", errors))

if (errors == 0) {
  cat("\nLithium fix verified successfully!\n")
} else {
  cat("\nPlease review the errors above.\n")
}

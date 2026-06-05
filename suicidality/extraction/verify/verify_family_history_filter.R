# verify_family_history_filter.R
# Verifies the family history filtering in sensitivity analysis

library(dplyr)
library(here)
here::i_am("suicidality/extraction/verify/verify_family_history_filter.R")

source(here("suicidality", "extraction", "lib", "common.R"))

cat("======================================================================\n")
cat("FAMILY HISTORY FILTER VERIFICATION\n")
cat("======================================================================\n\n")

warnings <- 0

# Load data
main_28 <- read_rds("main_12wks_28.rds")
cat("main_12wks_28 rows:", nrow(main_28), "\n\n")

# Check fh_suicidal distribution
cat("--- fh_suicidal distribution ---\n")
fh_suicidal_dist <- table(main_28$fh_suicidal, useNA = "ifany")
print(fh_suicidal_dist)

# Check fh_depr distribution
cat("\n--- fh_depr distribution ---\n")
fh_depr_dist <- table(main_28$fh_depr, useNA = "ifany")
print(fh_depr_dist)

# Verify: In ITT_12wks.R lines 420-421, fh_suicidal==2 and fh_depr==2 are filtered
# This comment says "exclude obs with missing info on covariates" but 2 is not missing
cat("\n--- Analysis of fh_suicidal==2 and fh_depr==2 exclusions ---\n")
n_fh_suicidal_2 <- sum(main_28$fh_suicidal == 2, na.rm = TRUE)
n_fh_depr_2 <- sum(main_28$fh_depr == 2, na.rm = TRUE)

cat(sprintf("Individuals with fh_suicidal==2 (both parents): %d\n", n_fh_suicidal_2))
cat(sprintf("Individuals with fh_depr==2 (both parents): %d\n", n_fh_depr_2))

if (n_fh_suicidal_2 > 0 || n_fh_depr_2 > 0) {
  cat("\nWARNING: ITT_12wks.R lines 420-421 filter out fh_suicidal==2 and fh_depr==2\n")
  cat("         The comment says 'exclude obs with missing info on covariates'\n")
  cat("         but 2 is not missing - it means BOTH parents have the condition.\n")
  cat("         This may be a bug or intentional for a specific sensitivity analysis.\n")
  warnings <- warnings + 1
}

# Check for overlap with other missing data exclusions
cat("\n--- Impact of combined exclusions in sensitivity analysis ---\n")
sens_filtered <- main_28 %>%
  filter(
    edufam_cat != "99" | is.na(edufam_cat),
    inc_cat != "NOINFO" | is.na(inc_cat),
    source != "M" | is.na(source),
    fh_suicidal != 2 | is.na(fh_suicidal),
    fh_depr != 2 | is.na(fh_depr)
  )

n_excluded <- nrow(main_28) - nrow(sens_filtered)
cat(sprintf("Total rows excluded by sensitivity analysis filters: %d (%.2f%%)\n",
            n_excluded, 100 * n_excluded / nrow(main_28)))

# Summary
cat("\n======================================================================\n")
cat("FAMILY HISTORY VERIFICATION SUMMARY\n")
cat("======================================================================\n")
cat(sprintf("Warnings: %d\n", warnings))

if (warnings == 0) {
  cat("\nNo issues found.\n")
} else {
  cat("\nReview the warnings above.\n")
}

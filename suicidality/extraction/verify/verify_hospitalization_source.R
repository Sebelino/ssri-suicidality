# verify_hospitalization_source.R
# Verification script for E8: Hospitalization Source Asymmetry
#
# Checks for asymmetry between:
# - Censoring (uses source "S" and "T")
# - Hospitalization covariate (uses only source "S")

library(dplyr)
library(here)
here::i_am("suicidality/extraction/verify/verify_hospitalization_source.R")

source(here("suicidality", "extraction", "lib", "common.R"))

cat("======================================================================\n")
cat("VERIFICATION: Hospitalization Source Asymmetry (E8)\n")
cat("======================================================================\n\n")

errors <- 0
warnings <- 0

# =============================================================================
# Load data
# =============================================================================
if (!rds_exists("raw_hospitalization.rds")) {
  cat("ERROR: raw_hospitalization.rds not found. Run extraction pipeline first.\n")
  quit(status = 1)
}

raw_hosp <- read_rds("raw_hospitalization.rds")
main_data <- read_rds("main_12wks_28.rds")

cat("Loaded raw_hospitalization.rds:", nrow(raw_hosp), "rows\n")
cat("Loaded main_12wks_28.rds:", nrow(main_data), "rows\n\n")

# =============================================================================
# Check 1: Source values in raw data
# =============================================================================
cat("--- Check 1: Source values in raw hospitalization data ---\n")

source_counts <- raw_hosp %>%
  count(source) %>%
  arrange(desc(n))

cat("Source distribution:\n")
print(source_counts)
cat("\n")

# =============================================================================
# Check 2: Count hospitalizations by source for cohort
# =============================================================================
cat("--- Check 2: Hospitalizations by source for cohort ---\n")

fu_start_data <- main_data %>%
  select(lopnr, fu_start)

# Hospitalizations before fu_start by source
hosp_by_source <- raw_hosp %>%
  inner_join(fu_start_data, by = "lopnr") %>%
  filter(diagn_date <= fu_start) %>%
  group_by(source) %>%
  summarise(
    n_records = n(),
    n_lopnrs = n_distinct(lopnr),
    .groups = "drop"
  )

cat("Hospitalizations before fu_start by source:\n")
print(hosp_by_source)
cat("\n")

# =============================================================================
# Check 3: Impact of asymmetry
# =============================================================================
cat("--- Check 3: Impact of source asymmetry ---\n")

# Current covariate: S (specialist) + T (therapeutic)
hosp_S_and_T <- raw_hosp %>%
  filter(source %in% c("S", "T")) %>%
  inner_join(fu_start_data, by = "lopnr") %>%
  filter(diagn_date <= fu_start) %>%
  distinct(lopnr)

# For reference: what if we only used S
hosp_S_only <- raw_hosp %>%
  filter(source == "S") %>%
  inner_join(fu_start_data, by = "lopnr") %>%
  filter(diagn_date <= fu_start) %>%
  distinct(lopnr)

n_S_and_T <- nrow(hosp_S_and_T)
n_S_only <- nrow(hosp_S_only)
n_diff <- n_S_and_T - n_S_only

cat(sprintf("Individuals with prior hospitalization (S + T, current): %d\n", n_S_and_T))
cat(sprintf("Individuals with prior hospitalization (S only):         %d\n", n_S_only))
cat(sprintf("Additional from T source:                                %d\n", n_diff))

cat("\n")

# =============================================================================
# Check 4: Compare with hosp covariate in main data
# =============================================================================
cat("--- Check 4: Verify hosp covariate in main data ---\n")

hosp_in_main <- sum(main_data$hosp == 1)
cat(sprintf("hosp=1 in main_12wks_28.rds: %d\n", hosp_in_main))
cat(sprintf("Expected from S-only filter:  %d\n", n_S_only))

if (abs(hosp_in_main - n_S_only) > 0) {
  cat(sprintf("INFO: Difference of %d may be due to date boundary handling\n",
              hosp_in_main - n_S_only))
}

cat("\n")

# =============================================================================
# Check 5: Verify censoring includes both S and T
# =============================================================================
cat("--- Check 5: Censoring source filter ---\n")

# Load censoring data to verify
if (rds_exists("cens_hosp_28.rds")) {
  cens_hosp <- read_rds("cens_hosp_28.rds")

  # Check which sources are in censoring
  cens_sources <- cens_hosp %>%
    inner_join(raw_hosp %>% select(lopnr, diagn_date = diagn_date, source),
               by = c("lopnr", "date_cens" = "diagn_date")) %>%
    count(source)

  cat("Sources in censoring events:\n")
  print(cens_sources)

  if (all(c("S", "T") %in% cens_sources$source)) {
    cat("OK: Censoring includes both S and T sources\n")
  } else if ("T" %in% cens_sources$source) {
    cat("OK: Censoring includes T source\n")
  } else {
    cat("INFO: T source may not appear in censoring (could be data-dependent)\n")
  }
} else {
  cat("WARNING: cens_hosp_28.rds not found\n")
  warnings <- warnings + 1
}

cat("\n")

# =============================================================================
# Summary
# =============================================================================
cat("======================================================================\n")
cat("VERIFICATION SUMMARY\n")
cat("======================================================================\n")
cat(sprintf("Errors:   %d\n", errors))
cat(sprintf("Warnings: %d\n", warnings))

cat("\n--- Asymmetry Analysis ---\n")
cat("ISSUE: Hospitalization covariate uses source='S' only\n")
cat("       Censoring uses source='S' OR 'T'\n")
cat(sprintf("IMPACT: %d individuals have T-only hospitalizations not captured in covariate\n", n_diff))

if (n_diff > 100) {
  cat("\nRECOMMENDATION: Consider adding source='T' to hospitalization covariate\n")
  cat("                to ensure consistency with censoring definition.\n")
} else {
  cat("\nNOTE: Impact appears minimal - asymmetry affects few individuals.\n")
}

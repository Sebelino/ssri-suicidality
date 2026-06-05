# process_base.R
# Phase 2: Create base cohort with death/emigration dates
#
# Inputs: cohort_base.rds, raw_migration.rds, raw_dor.rds
# Output: base_28.rds

library(dplyr)
library(here)
here::i_am("suicidality/extraction/12_process_base.R")

source(here("suicidality", "extraction", "lib", "common.R"))

process_base <- function(output_dir = rds_output_dir()) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat("=== process_base.R ===\n\n")

  # Load data
  cohort_base <- read_rds("cohort_base.rds")
  raw_migration <- read_rds("raw_migration.rds")
  raw_dor <- read_rds("raw_dor.rds")

  cat("cohort_base rows:", nrow(cohort_base), "\n")
  cat("raw_migration rows:", nrow(raw_migration), "\n")
  cat("raw_dor rows:", nrow(raw_dor), "\n")

  # Helper for safe min date

  safe_min_date <- function(x) if (all(is.na(x))) as.Date(NA) else min(x, na.rm = TRUE)

  # Get first emigration date on or after diagnosis for each person
  cat("\nProcessing emigration dates...\n")

  migration_after_diagn <- cohort_base %>%
    select(lopnr, diagn_date) %>%
    left_join(raw_migration, by = "lopnr", relationship = "many-to-many") %>%
    filter(is.na(date_emig) | date_emig >= diagn_date) %>%
    group_by(lopnr) %>%
    summarise(date_emig = safe_min_date(date_emig), .groups = "drop")

  # Get death dates
  cat("Processing death dates...\n")

  death_dates <- raw_dor %>%
    select(lopnr, date_death) %>%
    distinct()

  # Join with cohort_base
  base_28 <- cohort_base %>%
    left_join(death_dates, by = "lopnr") %>%
    left_join(migration_after_diagn, by = "lopnr")

  # Reorder columns to match expected output
  base_28 <- base_28 %>%
    select(lopnr, diagn_date, dia, bdate, atc, prescr, age, agecat,
           date_death, date_emig, lopnrmor, lopnrfar)

  cat("\nbase_28 rows:", nrow(base_28), "\n")
  cat("With death date:", sum(!is.na(base_28$date_death)), "\n")
  cat("With emigration date:", sum(!is.na(base_28$date_emig)), "\n")

  save_rds(base_28, "base_28.rds")
  cat("Saved base_28.rds\n")

  cat("\n=== process_base.R completed ===\n")
  invisible(base_28)
}

if (sys.nframe() == 0) {
  process_base()
}

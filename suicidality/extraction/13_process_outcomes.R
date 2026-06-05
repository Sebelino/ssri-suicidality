# process_outcomes.R
# Phase 2: Identify suicidal behavior outcomes
#
# Outcomes: Suicide attempts (hospital) + deaths from suicide
# - Known intent: X60-X84
# - Unknown intent: Y10-Y34
#
# Inputs: raw_diagnoses_cohort.rds, raw_dor.rds, cohort_lopnrs.rds
# Output: dia_all_28.rds

library(dplyr)
library(here)
here::i_am("suicidality/extraction/13_process_outcomes.R")

source(here("suicidality", "extraction", "lib", "common.R"))

process_outcomes <- function(output_dir = rds_output_dir()) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat("=== process_outcomes.R ===\n\n")

  # Load data
  raw_diagnoses_cohort <- read_rds("raw_diagnoses_cohort.rds")
  raw_dor <- read_rds("raw_dor.rds")
  cohort_lopnrs <- read_rds("cohort_lopnrs.rds")$lopnr

  cat("raw_diagnoses_cohort rows:", nrow(raw_diagnoses_cohort), "\n")
  cat("raw_dor rows:", nrow(raw_dor), "\n")

  # Suicidal behavior ICD codes
  known_intent <- paste0("X", 60:84)
  unknown_intent <- paste0("Y", 10:34)

  # Extract hospital suicidal behavior (known intent)
  cat("\nExtracting hospital suicidal behavior - known intent...\n")

  dia_known <- raw_diagnoses_cohort %>%
    filter(substr(dia, 1, 3) %in% known_intent) %>%
    filter(lopnr %in% cohort_lopnrs) %>%
    mutate(date_fail = diagn_date) %>%
    select(lopnr, dia, date_fail) %>%
    arrange(lopnr, date_fail) %>%
    group_by(lopnr, date_fail) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(source = "hospital", intent = "known")

  cat("Known intent events:", nrow(dia_known), "\n")

  # Extract hospital suicidal behavior (unknown intent)
  cat("Extracting hospital suicidal behavior - unknown intent...\n")

  dia_unknown <- raw_diagnoses_cohort %>%
    filter(substr(dia, 1, 3) %in% unknown_intent) %>%
    filter(lopnr %in% cohort_lopnrs) %>%
    mutate(date_fail = diagn_date) %>%
    select(lopnr, dia, date_fail) %>%
    arrange(lopnr, date_fail) %>%
    group_by(lopnr, date_fail) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(source = "hospital", intent = "unknown")

  cat("Unknown intent events:", nrow(dia_unknown), "\n")

  # Extract deaths from suicide (from raw_dor)
  cat("Extracting deaths from suicide...\n")

  all_suicidal_codes <- c(known_intent, unknown_intent)

  death_suicide <- raw_dor %>%
    filter(!is.na(cause) & substr(cause, 1, 3) %in% all_suicidal_codes) %>%
    filter(lopnr %in% cohort_lopnrs) %>%
    mutate(
      date_fail = date_death,
      dia = cause,
      source = "death",
      intent = if_else(substr(cause, 1, 1) == "X", "known", "unknown")
    ) %>%
    select(lopnr, dia, date_fail, source, intent) %>%
    arrange(lopnr, date_fail) %>%
    group_by(lopnr, date_fail) %>%
    slice(1) %>%
    ungroup()

  cat("Death events:", nrow(death_suicide), "\n")

  # Combine all events
  dia_all <- bind_rows(dia_known, dia_unknown, death_suicide)

  cat("\nCombined events before dedup:", nrow(dia_all), "\n")

  # Deduplicate (prioritize death events on same date)
  dia_all_28 <- dia_all %>%
    arrange(lopnr, date_fail, desc(source == "death")) %>%
    group_by(lopnr, date_fail) %>%
    slice(1) %>%
    ungroup()

  cat("dia_all_28 rows:", nrow(dia_all_28), "\n")
  cat("Unique lopnr:", n_distinct(dia_all_28$lopnr), "\n")
  cat("Hospital events:", sum(dia_all_28$source == "hospital"), "\n")
  cat("Death events:", sum(dia_all_28$source == "death"), "\n")

  save_rds(dia_all_28, "dia_all_28.rds")
  cat("\nSaved dia_all_28.rds\n")

  cat("\n=== process_outcomes.R completed ===\n")
  invisible(dia_all_28)
}

if (sys.nframe() == 0) {
  process_outcomes()
}

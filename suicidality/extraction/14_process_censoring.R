# process_censoring.R
# Phase 2: Identify hospitalization censoring events
#
# Censoring: Psychiatric hospitalizations >= 2 days
#
# Inputs: raw_hospitalization.rds, cohort_lopnrs.rds
# Output: cens_hosp_28.rds

library(dplyr)
library(here)
here::i_am("suicidality/extraction/14_process_censoring.R")

source(here("suicidality", "extraction", "lib", "common.R"))

process_censoring <- function(output_dir = rds_output_dir()) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat("=== process_censoring.R ===\n\n")

  # Load data
  raw_hospitalization <- read_rds("raw_hospitalization.rds")
  cohort_lopnrs <- read_rds("cohort_lopnrs.rds")$lopnr

  cat("raw_hospitalization rows:", nrow(raw_hospitalization), "\n")

  # Filter for censoring: inpatient (S or T), psychiatric/self-harm, >= 2 days
  # ICD codes: F* (psychiatric), X60-X84, Y10-Y34 (self-harm)
  known_intent <- paste0("X", 60:84)
  unknown_intent <- paste0("Y", 10:34)

  cens_hosp_28 <- raw_hospitalization %>%
    filter(lopnr %in% cohort_lopnrs) %>%
    filter(source %in% c("S", "T")) %>%
    filter(!is.na(stay_days) & stay_days >= 2) %>%
    filter(
      substr(dia, 1, 1) == "F" |
      substr(dia, 1, 3) %in% known_intent |
      substr(dia, 1, 3) %in% unknown_intent
    ) %>%
    mutate(date_cens = diagn_date) %>%
    select(lopnr, dia, date_cens, stay_days) %>%
    arrange(lopnr, date_cens) %>%
    group_by(lopnr, date_cens) %>%
    slice(1) %>%
    ungroup()

  cat("Censoring events:", nrow(cens_hosp_28), "\n")
  cat("Unique lopnr:", n_distinct(cens_hosp_28$lopnr), "\n")

  save_rds(cens_hosp_28, "cens_hosp_28.rds")
  cat("Saved cens_hosp_28.rds\n")

  cat("\n=== process_censoring.R completed ===\n")
  invisible(cens_hosp_28)
}

if (sys.nframe() == 0) {
  process_censoring()
}

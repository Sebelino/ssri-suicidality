# process_cov_hospitalizations.R
# Phase 2: Process hospitalizations to create hospitalization covariate
#
# Creates indicator for prior psychiatric hospitalizations before follow-up start
#
# Inputs: main_12wks_28_tmp.rds, raw_hospitalization.rds
# Output: cov_hospitalizations.rds

library(dplyr)
library(here)
here::i_am("suicidality/extraction/21_process_cov_hospitalizations.R")

source(here("suicidality", "extraction", "lib", "common.R"))

process_cov_hospitalizations <- function(output_dir = rds_output_dir(), grace_days = 28L) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat(sprintf("=== process_cov_hospitalizations.R (grace_days = %d) ===\n\n", grace_days))

  # Load data
  main_file <- sprintf("main_12wks_%d_tmp.rds", grace_days)
  main_tmp <- read_rds(main_file)
  raw_hospitalization <- read_rds("raw_hospitalization.rds")

  cat(main_file, "rows:", nrow(main_tmp), "\n")
  cat("raw_hospitalization rows:", nrow(raw_hospitalization), "\n")

  # Get fu_start dates
  fu_start_data <- main_tmp %>%
    select(lopnr, fu_start)

  max_date <- max(fu_start_data$fu_start, na.rm = TRUE)

  # Filter hospitalizations to before fu_start
  # Inpatient sources: S (specialist) and T (therapeutic)
  # Matches censoring definition in 14_process_censoring.R
  hosp_filtered <- raw_hospitalization %>%
    filter(source %in% c("S", "T")) %>%
    filter(diagn_date < max_date) %>%
    inner_join(fu_start_data, by = "lopnr") %>%
    filter(diagn_date <= fu_start) %>%
    select(lopnr, diagn_date)

  cat("Hospitalizations before fu_start:", nrow(hosp_filtered), "\n")

  # Count hospitalizations per person
  hosp_counts <- hosp_filtered %>%
    distinct(lopnr, diagn_date) %>%
    count(lopnr, name = "hosp_no")

  cat("Patients with hospitalizations:", nrow(hosp_counts), "\n")

  # Create covariate
  cov_hospitalizations <- fu_start_data %>%
    select(lopnr) %>%
    left_join(hosp_counts, by = "lopnr") %>%
    mutate(
      hosp_no = if_else(is.na(hosp_no), 0L, as.integer(hosp_no)),
      hosp = if_else(hosp_no > 0, 1L, 0L)
    )

  cat("\nHospitalization summary:\n")
  cat("hosp = 1:", sum(cov_hospitalizations$hosp == 1), "\n")
  cat("hosp = 0:", sum(cov_hospitalizations$hosp == 0), "\n")
  cat("Mean hosp_no (among those with any):",
      mean(cov_hospitalizations$hosp_no[cov_hospitalizations$hosp == 1]), "\n")

  out_file <- if (grace_days == 28L) "cov_hospitalizations.rds" else sprintf("cov_hospitalizations_%d.rds", grace_days)
  save_rds(cov_hospitalizations, out_file)
  cat("\nSaved", out_file, "\n")

  cat("\n=== process_cov_hospitalizations.R completed ===\n")
  invisible(cov_hospitalizations)
}

if (sys.nframe() == 0) {
  process_cov_hospitalizations()
}

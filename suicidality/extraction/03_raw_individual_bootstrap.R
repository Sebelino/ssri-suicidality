# raw_individual_bootstrap.R
# Phase 1a: Extract individual data (birth dates, sex) for potential cohort
#
# Depends on: raw_diagnoses_index.rds (to know which lopnrs)
#
# Output: raw_individual.rds
#   Columns: lopnr, bdate, sex

library(DBI)
library(odbc)
library(dplyr)
library(here)
here::i_am("suicidality/extraction/03_raw_individual_bootstrap.R")

source(here("suicidality", "extraction", "lib", "common.R"))

raw_individual_bootstrap <- function(output_dir = rds_output_dir()) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat("=== raw_individual_bootstrap.R ===\n\n")

  # Load diagnoses to get lopnrs
  raw_diagnoses_index <- read_rds("raw_diagnoses_index.rds")
  potential_lopnrs <- unique(raw_diagnoses_index$lopnr)
  cat("Potential cohort lopnrs:", length(potential_lopnrs), "\n")

  con <- db_connect()
  on.exit(dbDisconnect(con))

  # Extract birth dates and sex for potential cohort
  cat("Extracting individual data (batched)...\n")

  raw_individual <- batch_query(con, "
    SELECT lopnr, fodelsedatum, kon
    FROM dbo.v_individual
    WHERE lopnr IN (%s)
  ", potential_lopnrs) %>%
    mutate(
      # Parse birth date: YYYYMM format -> use 15th of month
      bdate = as.Date(paste0(fodelsedatum, "15"), format = "%Y%m%d"),
      # Sex: 1 = male, 2 = female
      sex = as.integer(kon)
    ) %>%
    select(lopnr, bdate, sex)

  cat("Rows extracted:", nrow(raw_individual), "\n")

  # Check for missing data
  missing_bdate <- sum(is.na(raw_individual$bdate))
  missing_sex <- sum(is.na(raw_individual$sex))
  if (missing_bdate > 0) cat("WARNING: Missing birth dates:", missing_bdate, "\n")
  if (missing_sex > 0) cat("WARNING: Missing sex:", missing_sex, "\n")

  save_rds(raw_individual, "raw_individual.rds")
  cat("Saved raw_individual.rds\n")

  cat("\n=== raw_individual_bootstrap.R completed ===\n")
  invisible(raw_individual)
}

if (sys.nframe() == 0) {
  raw_individual_bootstrap()
}

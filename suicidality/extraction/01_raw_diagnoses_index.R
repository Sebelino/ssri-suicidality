# raw_diagnoses_index.R
# Phase 1a: Extract depression diagnoses (F32/F33) for cohort identification
#
# This is the first step - extracts diagnoses that define potential cohort members.
# Minimal processing: only date conversion and basic filtering.
#
# Output: raw_diagnoses_index.rds
#   Columns: lopnr, dia, diagn_date, source

library(DBI)
library(odbc)
library(dplyr)
library(here)
here::i_am("suicidality/extraction/01_raw_diagnoses_index.R")

source(here("suicidality", "extraction", "lib", "common.R"))

raw_diagnoses_index <- function(output_dir = rds_output_dir()) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat("=== raw_diagnoses_index.R ===\n\n")

  con <- db_connect()
  on.exit(dbDisconnect(con))

  # Extract depression diagnoses (F32, F33) from 2006-2019.
  # Exclude remission (F334).
  # Exclude chapter-range placeholder codes ("F32-", "F33-") that the
  # Swedish NPR sometimes records when no specific subcode is given —
  # their 3-char prefix matches but they don't represent a real F32 / F33
  # diagnosis and would let ~118 placeholder-only patients into the cohort.
  cat("Extracting F32/F33 diagnoses...\n")

  raw_diagnoses_index <- dbGetQuery(con, "
    SELECT lopnr, dia, source, x_indatum
    FROM dbo.v_npr_dia
    WHERE icd = '10'
      AND LEFT(dia, 3) IN ('F32', 'F33')
      AND LEFT(dia, 4) != 'F334'
      AND dia NOT LIKE '%-%'
      AND x_indatum >= '20060701'
      AND x_indatum <= '20191231'
  ") %>%
    mutate(diagn_date = as.Date(x_indatum, format = "%Y%m%d")) %>%
    select(lopnr, dia, diagn_date, source)

  cat("Rows extracted:", nrow(raw_diagnoses_index), "\n")
  cat("Unique lopnr:", n_distinct(raw_diagnoses_index$lopnr), "\n")

  save_rds(raw_diagnoses_index, "raw_diagnoses_index.rds")
  cat("Saved raw_diagnoses_index.rds\n")

  cat("\n=== raw_diagnoses_index.R completed ===\n")
  invisible(raw_diagnoses_index)
}

if (sys.nframe() == 0) {
  raw_diagnoses_index()
}

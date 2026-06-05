# raw_prescriptions_all.R
# Phase 1a: Extract N06A prescriptions for cohort identification
#
# Only extracts N06A (antidepressants) filtered to lopnrs with F32/F33 diagnoses.
# This is needed for:
# - 365-day washout check
# - SSRI initiator identification
#
# Other prescriptions (N02A, N03A, N05*, N07B) are extracted in Phase 3
# after the cohort is defined.
#
# Prerequisite: raw_diagnoses_index.rds must exist
# Output: raw_prescriptions.rds
#   Columns: lopnr, atc, edatum, antal

library(DBI)
library(odbc)
library(dplyr)
library(here)
here::i_am("suicidality/extraction/02_raw_prescriptions_all.R")

source(here("suicidality", "extraction", "lib", "common.R"))

raw_prescriptions_all <- function(output_dir = rds_output_dir()) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat("=== raw_prescriptions_all.R ===\n\n")

  # Load lopnrs from diagnoses to filter prescriptions
  raw_diagnoses_index <- read_rds("raw_diagnoses_index.rds")
  lopnrs <- unique(raw_diagnoses_index$lopnr)
  cat("Unique lopnrs from diagnoses:", length(lopnrs), "\n\n")
  rm(raw_diagnoses_index)
  gc(verbose = FALSE)

  con <- db_connect()
  on.exit(dbDisconnect(con))

  # Extract N06A prescriptions in batches by lopnr
  cat("Extracting N06A prescriptions for diagnosed lopnrs...\n")

  query_template <- "
    SELECT lopnr, atc, edatum, antal
    FROM dbo.v_lmr
    WHERE antal > 0
      AND LEFT(atc, 4) = 'N06A'
      AND lopnr IN (%s)
  "

  raw_prescriptions <- batch_query(con, query_template, lopnrs, batch_size = 10000)

  # Convert date
  raw_prescriptions$edatum <- as.Date(raw_prescriptions$edatum, format = "%Y%m%d")

  cat("\nRows extracted:", nrow(raw_prescriptions), "\n")
  cat("Unique lopnr:", n_distinct(raw_prescriptions$lopnr), "\n")
  cat("Date range:", as.character(min(raw_prescriptions$edatum, na.rm = TRUE)), "to",
      as.character(max(raw_prescriptions$edatum, na.rm = TRUE)), "\n")

  # Summary by ATC subgroup
  cat("\nPrescription counts by ATC subgroup:\n")
  raw_prescriptions %>%
    mutate(atc_sub = substr(atc, 1, 5)) %>%
    count(atc_sub) %>%
    arrange(desc(n)) %>%
    print()

  save_rds(raw_prescriptions, "raw_prescriptions.rds")
  cat("\nSaved raw_prescriptions.rds\n")

  cat("\n=== raw_prescriptions_all.R completed ===\n")
  invisible(raw_prescriptions)
}

if (sys.nframe() == 0) {
  raw_prescriptions_all()
}

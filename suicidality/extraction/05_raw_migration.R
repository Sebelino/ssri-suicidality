# raw_migration.R
# Phase 1c: Extract migration (emigration) data for cohort
#
# Depends on: cohort_lopnrs.rds
#
# Output: raw_migration.rds
#   Columns: lopnr, date_emig

library(DBI)
library(odbc)
library(dplyr)
library(here)
here::i_am("suicidality/extraction/05_raw_migration.R")

source(here("suicidality", "extraction", "lib", "common.R"))

raw_migration <- function(output_dir = rds_output_dir()) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat("=== raw_migration.R ===\n\n")

  # Load cohort lopnrs
  cohort_lopnrs <- read_rds("cohort_lopnrs.rds")$lopnr
  cat("Cohort lopnrs:", length(cohort_lopnrs), "\n")

  con <- db_connect()
  on.exit(dbDisconnect(con))

  # Extract emigration data (POSTTYP = 'U')
  cat("Extracting emigration data (batched)...\n")

  raw_migration <- batch_query(con, "
    SELECT lopnr, INVUTVMANAD
    FROM dbo.v_migration
    WHERE POSTTYP = 'U' AND lopnr IN (%s)
  ", cohort_lopnrs) %>%
    filter(!is.na(INVUTVMANAD) & nchar(as.character(INVUTVMANAD)) >= 6) %>%
    mutate(
      year = as.integer(substr(as.character(INVUTVMANAD), 1, 4)),
      month = as.integer(substr(as.character(INVUTVMANAD), 5, 6))
    ) %>%
    filter(!is.na(year) & !is.na(month) & year > 1900 & month >= 1 & month <= 12) %>%
    mutate(date_emig = as.Date(ISOdate(year, month, 15))) %>%  # Day 15 minimizes avg error (data only has YYYYMM)
    filter(!is.na(date_emig)) %>%
    select(lopnr, date_emig)

  cat("Emigration records:", nrow(raw_migration), "\n")
  cat("Unique lopnr with emigration:", n_distinct(raw_migration$lopnr), "\n")

  save_rds(raw_migration, "raw_migration.rds")
  cat("Saved raw_migration.rds\n")

  cat("\n=== raw_migration.R completed ===\n")
  invisible(raw_migration)
}

if (sys.nframe() == 0) {
  raw_migration()
}

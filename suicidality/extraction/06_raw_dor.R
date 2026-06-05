# raw_dor.R
# Phase 1c: Extract death data (death dates and causes) for cohort
#
# Depends on: cohort_lopnrs.rds
#
# Output: raw_dor.rds
#   Columns: lopnr, date_death, cause (underlying cause of death)

library(DBI)
library(odbc)
library(dplyr)
library(here)
here::i_am("suicidality/extraction/06_raw_dor.R")

source(here("suicidality", "extraction", "lib", "common.R"))

raw_dor <- function(output_dir = rds_output_dir()) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat("=== raw_dor.R ===\n\n")

  # Load cohort lopnrs
  cohort_lopnrs <- read_rds("cohort_lopnrs.rds")$lopnr
  cat("Cohort lopnrs:", length(cohort_lopnrs), "\n")

  con <- db_connect()
  on.exit(dbDisconnect(con))

  # Extract death dates
  cat("Extracting death dates (batched)...\n")

  death_dates <- batch_query(con, "
    SELECT lopnr, x_dodsdat
    FROM dbo.v_dor_bas
    WHERE lopnr IN (%s)
  ", cohort_lopnrs) %>%
    mutate(date_death = as.Date(x_dodsdat, format = "%Y%m%d")) %>%
    select(lopnr, date_death)

  cat("Death date records:", nrow(death_dates), "\n")

  # Extract underlying cause of death (NR = 'U00')
  cat("Extracting causes of death (batched)...\n")

  death_causes <- batch_query(con, "
    SELECT lopnr, ORSAK as cause, x_dodsdat
    FROM dbo.v_dor_orsak
    WHERE NR = 'U00' AND lopnr IN (%s)
  ", cohort_lopnrs) %>%
    mutate(date_death = as.Date(x_dodsdat, format = "%Y%m%d")) %>%
    select(lopnr, date_death, cause)

  cat("Cause of death records:", nrow(death_causes), "\n")

  # Combine death dates with causes
  raw_dor <- death_dates %>%
    left_join(death_causes %>% select(lopnr, cause), by = "lopnr")

  cat("Combined death records:", nrow(raw_dor), "\n")

  save_rds(raw_dor, "raw_dor.rds")
  cat("Saved raw_dor.rds\n")

  cat("\n=== raw_dor.R completed ===\n")
  invisible(raw_dor)
}

if (sys.nframe() == 0) {
  raw_dor()
}

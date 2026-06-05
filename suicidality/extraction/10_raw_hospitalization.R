# raw_hospitalization.R
# Phase 1c: Extract inpatient hospitalization data for cohort
#
# Used for:
# - Censoring (psychiatric hospitalizations >= 2 days)
# - Covariate (prior hospitalization counts)
#
# Depends on: cohort_lopnrs.rds
#
# Output: raw_hospitalization.rds
#   Columns: lopnr, dia, diagn_date, source, stay_days

library(DBI)
library(odbc)
library(dplyr)
library(here)
here::i_am("suicidality/extraction/10_raw_hospitalization.R")

source(here("suicidality", "extraction", "lib", "common.R"))

raw_hospitalization <- function(output_dir = rds_output_dir()) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat("=== raw_hospitalization.R ===\n\n")

  # Load cohort lopnrs
  cohort_lopnrs <- read_rds("cohort_lopnrs.rds")$lopnr
  cat("Cohort lopnrs:", length(cohort_lopnrs), "\n")

  con <- db_connect()
  on.exit(dbDisconnect(con))

  # Extract inpatient hospitalizations (source = 'S' for sluten vard)
  # Include psychiatric (F*) and self-harm (X60-X84, Y10-Y34) diagnoses
  cat("Extracting inpatient hospitalizations (batched)...\n")

  raw_hospitalization <- batch_query(con, "
    SELECT lopnr, dia, x_indatum, x_utdatum, source
    FROM dbo.v_npr_dia
    WHERE source IN ('S', 'T')
      AND lopnr IN (%s)
      AND (
        LEFT(dia, 1) = 'F'
        OR LEFT(dia, 3) IN ('X60', 'X61', 'X62', 'X63', 'X64', 'X65', 'X66', 'X67', 'X68', 'X69',
                            'X70', 'X71', 'X72', 'X73', 'X74', 'X75', 'X76', 'X77', 'X78', 'X79',
                            'X80', 'X81', 'X82', 'X83', 'X84')
        OR LEFT(dia, 3) IN ('Y10', 'Y11', 'Y12', 'Y13', 'Y14', 'Y15', 'Y16', 'Y17', 'Y18', 'Y19',
                            'Y20', 'Y21', 'Y22', 'Y23', 'Y24', 'Y25', 'Y26', 'Y27', 'Y28', 'Y29',
                            'Y30', 'Y31', 'Y32', 'Y33', 'Y34')
      )
  ", cohort_lopnrs) %>%
    mutate(
      diagn_date = as.Date(x_indatum, format = "%Y%m%d"),
      utdatum = as.Date(x_utdatum, format = "%Y%m%d"),
      stay_days = as.integer(utdatum - diagn_date)
    ) %>%
    select(lopnr, dia, diagn_date, source, stay_days)

  cat("Hospitalization records:", nrow(raw_hospitalization), "\n")
  cat("Unique lopnr:", n_distinct(raw_hospitalization$lopnr), "\n")

  # Summary
  cat("\nBy source:\n")
  raw_hospitalization %>%
    count(source) %>%
    print()

  save_rds(raw_hospitalization, "raw_hospitalization.rds")
  cat("\nSaved raw_hospitalization.rds\n")

  cat("\n=== raw_hospitalization.R completed ===\n")
  invisible(raw_hospitalization)
}

if (sys.nframe() == 0) {
  raw_hospitalization()
}

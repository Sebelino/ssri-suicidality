# raw_prescriptions_cohort.R
# Phase 3: Extract all relevant prescriptions for the defined cohort
#
# Extracts prescriptions for:
# - N06A (antidepressants) - for time-varying analysis
# - N02A (opioids), N03A (antiepileptics), N05A (antipsychotics),
#   N05B (anxiolytics), N05C (hypnotics), N06B (stimulants), N07B (addiction meds)
#   - for covariates and time-varying analysis
#
# Prerequisite: cohort_lopnrs.rds must exist
# Output: raw_prescriptions_cohort.rds
#   Columns: lopnr, atc, edatum, antal

library(DBI)
library(odbc)
library(dplyr)
library(here)
here::i_am("suicidality/extraction/11_raw_prescriptions_cohort.R")

source(here("suicidality", "extraction", "lib", "common.R"))

raw_prescriptions_cohort <- function(output_dir = rds_output_dir()) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat("=== raw_prescriptions_cohort.R ===\n\n")

  # Load cohort lopnrs
  cohort_lopnrs <- read_rds("cohort_lopnrs.rds")$lopnr
  cat("Cohort lopnrs:", length(cohort_lopnrs), "\n\n")

  con <- db_connect()
  on.exit(dbDisconnect(con))

  # Extract all relevant prescriptions for cohort in batches
  atc_prefixes <- c("N02A", "N03A", "N05A", "N05B", "N05C", "N06A", "N06B", "N07B")
  atc_filter <- paste(sprintf("'%s'", atc_prefixes), collapse = ", ")

  cat("Extracting prescriptions (", paste(atc_prefixes, collapse = ", "), ") for cohort...\n")

  query_template <- sprintf("
    SELECT lopnr, atc, edatum, antal
    FROM dbo.v_lmr
    WHERE antal > 0
      AND LEFT(atc, 4) IN (%s)
      AND lopnr IN (%%s)
  ", atc_filter)

  raw_prescriptions <- batch_query(con, query_template, cohort_lopnrs, batch_size = 10000)

  # Convert date
  raw_prescriptions$edatum <- as.Date(raw_prescriptions$edatum, format = "%Y%m%d")

  cat("\nRows extracted:", nrow(raw_prescriptions), "\n")
  cat("Unique lopnr:", n_distinct(raw_prescriptions$lopnr), "\n")
  cat("Date range:", as.character(min(raw_prescriptions$edatum, na.rm = TRUE)), "to",
      as.character(max(raw_prescriptions$edatum, na.rm = TRUE)), "\n")

  # Summary by ATC prefix
  cat("\nPrescription counts by ATC prefix:\n")
  raw_prescriptions %>%
    mutate(atc_prefix = substr(atc, 1, 4)) %>%
    count(atc_prefix) %>%
    arrange(desc(n)) %>%
    print()

  save_rds(raw_prescriptions, "raw_prescriptions_cohort.rds")
  cat("\nSaved raw_prescriptions_cohort.rds\n")

  cat("\n=== raw_prescriptions_cohort.R completed ===\n")
  invisible(raw_prescriptions)
}

if (sys.nframe() == 0) {
  raw_prescriptions_cohort()
}

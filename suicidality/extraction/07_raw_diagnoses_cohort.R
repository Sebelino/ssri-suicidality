# raw_diagnoses_cohort.R
# Phase 1c: Extract all relevant diagnoses for cohort members
#
# Includes:
# - Psychiatric diagnoses (F00-F99) for covariates
# - Poisoning diagnoses (X40-X49, T36-T50) for covariates
# - Suicidal behavior (X60-X84, Y10-Y34) for outcomes
#
# Depends on: cohort_lopnrs.rds
#
# Output: raw_diagnoses_cohort.rds
#   Columns: lopnr, dia, diagn_date, source

library(DBI)
library(odbc)
library(dplyr)
library(here)
here::i_am("suicidality/extraction/07_raw_diagnoses_cohort.R")

source(here("suicidality", "extraction", "lib", "common.R"))

raw_diagnoses_cohort <- function(output_dir = rds_output_dir()) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat("=== raw_diagnoses_cohort.R ===\n\n")

  # Load cohort lopnrs
  cohort_lopnrs <- read_rds("cohort_lopnrs.rds")$lopnr
  cat("Cohort lopnrs:", length(cohort_lopnrs), "\n")

  con <- db_connect()
  on.exit(dbDisconnect(con))

  # Extract all relevant diagnoses for cohort
  # F00-F99: Mental disorders
  # X40-X49: Accidental poisoning
  # X60-X84: Intentional self-harm
  # Y10-Y34: Event of undetermined intent
  # T36-T50: Poisoning by drugs/substances
  cat("Extracting diagnoses (batched)...\n")

  raw_diagnoses_cohort <- batch_query(con, "
    SELECT lopnr, dia, x_indatum, source
    FROM dbo.v_npr_dia
    WHERE lopnr IN (%s)
      AND (
        LEFT(dia, 1) = 'F'
        OR LEFT(dia, 3) IN ('X40', 'X41', 'X42', 'X43', 'X44', 'X45', 'X46', 'X47', 'X48', 'X49')
        OR LEFT(dia, 3) IN ('X60', 'X61', 'X62', 'X63', 'X64', 'X65', 'X66', 'X67', 'X68', 'X69',
                            'X70', 'X71', 'X72', 'X73', 'X74', 'X75', 'X76', 'X77', 'X78', 'X79',
                            'X80', 'X81', 'X82', 'X83', 'X84')
        OR LEFT(dia, 3) IN ('Y10', 'Y11', 'Y12', 'Y13', 'Y14', 'Y15', 'Y16', 'Y17', 'Y18', 'Y19',
                            'Y20', 'Y21', 'Y22', 'Y23', 'Y24', 'Y25', 'Y26', 'Y27', 'Y28', 'Y29',
                            'Y30', 'Y31', 'Y32', 'Y33', 'Y34')
        OR LEFT(dia, 3) IN ('T36', 'T37', 'T38', 'T39', 'T40', 'T41', 'T42', 'T43', 'T44', 'T45',
                            'T46', 'T47', 'T48', 'T49', 'T50', 'T51')
      )
  ", cohort_lopnrs) %>%
    mutate(diagn_date = as.Date(x_indatum, format = "%Y%m%d")) %>%
    select(lopnr, dia, diagn_date, source)

  cat("Diagnosis records:", nrow(raw_diagnoses_cohort), "\n")
  cat("Unique lopnr:", n_distinct(raw_diagnoses_cohort$lopnr), "\n")

  # Summary by diagnosis category
  cat("\nDiagnosis counts by category:\n")
  raw_diagnoses_cohort %>%
    mutate(category = case_when(
      substr(dia, 1, 1) == "F" ~ "Psychiatric (F)",
      substr(dia, 1, 1) == "T" ~ "Poisoning (T)",
      substr(dia, 1, 3) %in% paste0("X", 40:49) ~ "Accidental poisoning (X40-X49)",
      substr(dia, 1, 3) %in% paste0("X", 60:84) ~ "Self-harm (X60-X84)",
      substr(dia, 1, 3) %in% paste0("Y", 10:34) ~ "Undetermined (Y10-Y34)",
      TRUE ~ "Other"
    )) %>%
    count(category) %>%
    print()

  save_rds(raw_diagnoses_cohort, "raw_diagnoses_cohort.rds")
  cat("\nSaved raw_diagnoses_cohort.rds\n")

  cat("\n=== raw_diagnoses_cohort.R completed ===\n")
  invisible(raw_diagnoses_cohort)
}

if (sys.nframe() == 0) {
  raw_diagnoses_cohort()
}

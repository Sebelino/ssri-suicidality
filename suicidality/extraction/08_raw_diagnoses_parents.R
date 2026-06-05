# raw_diagnoses_parents.R
# Phase 1c: Extract diagnoses for parents (family history)
#
# Extracts:
# - Suicidal behavior (X60-X84, Y10-Y34) for fh_suicidal
# - Depression (F32-F33) for fh_depr
#
# Depends on: parent_lopnrs.rds
#
# Output: raw_diagnoses_parents.rds
#   Columns: lopnr, dia, diagn_date

library(DBI)
library(odbc)
library(dplyr)
library(here)
here::i_am("suicidality/extraction/08_raw_diagnoses_parents.R")

source(here("suicidality", "extraction", "lib", "common.R"))

raw_diagnoses_parents <- function(output_dir = rds_output_dir()) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat("=== raw_diagnoses_parents.R ===\n\n")

  # Load parent lopnrs
  parent_lopnrs <- read_rds("parent_lopnrs.rds")$lopnr
  cat("Parent lopnrs:", length(parent_lopnrs), "\n")

  con <- db_connect()
  on.exit(dbDisconnect(con))

  # Extract diagnoses for family history
  # Suicidal: X60-X84, Y10-Y34
  # Depression: F32, F33
  cat("Extracting parent diagnoses (batched)...\n")

  raw_diagnoses_parents <- batch_query(con, "
    SELECT lopnr, dia, x_indatum
    FROM dbo.v_npr_dia
    WHERE lopnr IN (%s)
      AND (
        LEFT(dia, 3) IN ('F32', 'F33')
        OR LEFT(dia, 3) IN ('X60', 'X61', 'X62', 'X63', 'X64', 'X65', 'X66', 'X67', 'X68', 'X69',
                            'X70', 'X71', 'X72', 'X73', 'X74', 'X75', 'X76', 'X77', 'X78', 'X79',
                            'X80', 'X81', 'X82', 'X83', 'X84')
        OR LEFT(dia, 3) IN ('Y10', 'Y11', 'Y12', 'Y13', 'Y14', 'Y15', 'Y16', 'Y17', 'Y18', 'Y19',
                            'Y20', 'Y21', 'Y22', 'Y23', 'Y24', 'Y25', 'Y26', 'Y27', 'Y28', 'Y29',
                            'Y30', 'Y31', 'Y32', 'Y33', 'Y34')
      )
  ", parent_lopnrs) %>%
    mutate(diagn_date = as.Date(x_indatum, format = "%Y%m%d")) %>%
    select(lopnr, dia, diagn_date)

  cat("Parent diagnosis records:", nrow(raw_diagnoses_parents), "\n")
  cat("Unique parent lopnr:", n_distinct(raw_diagnoses_parents$lopnr), "\n")

  # Summary
  cat("\nDiagnosis counts:\n")
  raw_diagnoses_parents %>%
    mutate(category = case_when(
      substr(dia, 1, 3) %in% c("F32", "F33") ~ "Depression",
      TRUE ~ "Suicidal behavior"
    )) %>%
    count(category) %>%
    print()

  save_rds(raw_diagnoses_parents, "raw_diagnoses_parents.rds")
  cat("\nSaved raw_diagnoses_parents.rds\n")

  cat("\n=== raw_diagnoses_parents.R completed ===\n")
  invisible(raw_diagnoses_parents)
}

if (sys.nframe() == 0) {
  raw_diagnoses_parents()
}

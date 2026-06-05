# verify_raw_extraction.R
# Verification script for RDS files produced by scripts 01-10
# Checks data integrity, expected columns, value ranges, and cross-file consistency

library(dplyr)
library(here)
here::i_am("suicidality/extraction/verify/verify_raw_extraction.R")

source(here("suicidality", "extraction", "lib", "common.R"))

cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("RAW EXTRACTION VERIFICATION (Scripts 01-10)\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")

errors <- 0
warnings <- 0

report_error <- function(msg) {
  cat("ERROR: ", msg, "\n")
  errors <<- errors + 1
}

report_warning <- function(msg) {
  cat("WARNING: ", msg, "\n")
  warnings <<- warnings + 1
}

report_ok <- function(msg) {
  cat("OK: ", msg, "\n")
}

report_info <- function(msg) {
  cat("INFO: ", msg, "\n")
}

# =============================================================================
# 1. Script 01: raw_diagnoses_index.rds
# =============================================================================
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("1. SCRIPT 01: raw_diagnoses_index.rds\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

if (rds_exists("raw_diagnoses_index.rds")) {
  raw_diagnoses_index <- read_rds("raw_diagnoses_index.rds")
  report_info(sprintf("Rows: %s", format(nrow(raw_diagnoses_index), big.mark = ",")))

  # Check required columns
  required_cols <- c("lopnr", "dia", "diagn_date")
  missing_cols <- setdiff(required_cols, names(raw_diagnoses_index))
  if (length(missing_cols) == 0) {
    report_ok("All required columns present")
  } else {
    report_error(sprintf("Missing columns: %s", paste(missing_cols, collapse = ", ")))
  }

  # Check unique lopnr count
  n_unique <- n_distinct(raw_diagnoses_index$lopnr)
  report_info(sprintf("Unique lopnr: %s", format(n_unique, big.mark = ",")))

  # Check diagnosis codes are F32/F33 (depression)
  if ("dia" %in% names(raw_diagnoses_index)) {
    dia_prefixes <- substr(raw_diagnoses_index$dia, 1, 3) |> unique()
    if (all(dia_prefixes %in% c("F32", "F33"))) {
      report_ok("All diagnoses are F32/F33 (depression)")
    } else {
      unexpected <- setdiff(dia_prefixes, c("F32", "F33"))
      report_error(sprintf("Unexpected diagnosis prefixes: %s", paste(unexpected, collapse = ", ")))
    }
  }

  # Check date range
  if ("diagn_date" %in% names(raw_diagnoses_index)) {
    date_range <- range(raw_diagnoses_index$diagn_date, na.rm = TRUE)
    report_info(sprintf("Date range: %s to %s", date_range[1], date_range[2]))
    if (date_range[1] >= as.Date("2006-01-01") && date_range[2] <= as.Date("2020-12-31")) {
      report_ok("Date range within expected bounds (2006-2020)")
    } else {
      report_warning("Date range outside expected bounds")
    }
  }

  rm(raw_diagnoses_index)
} else {
  report_error("File not found: raw_diagnoses_index.rds")
}

# =============================================================================
# 2. Script 02: raw_prescriptions.rds
# =============================================================================
cat("\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("2. SCRIPT 02: raw_prescriptions.rds\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

if (rds_exists("raw_prescriptions.rds")) {
  raw_prescriptions <- read_rds("raw_prescriptions.rds")
  report_info(sprintf("Rows: %s", format(nrow(raw_prescriptions), big.mark = ",")))

  # Check required columns
  required_cols <- c("lopnr", "atc", "edatum")
  missing_cols <- setdiff(required_cols, names(raw_prescriptions))
  if (length(missing_cols) == 0) {
    report_ok("All required columns present")
  } else {
    report_error(sprintf("Missing columns: %s", paste(missing_cols, collapse = ", ")))
  }

  # Check unique lopnr count
  n_unique <- n_distinct(raw_prescriptions$lopnr)
  report_info(sprintf("Unique lopnr: %s", format(n_unique, big.mark = ",")))

  # Check ATC codes are N06A (antidepressants)
  if ("atc" %in% names(raw_prescriptions)) {
    atc_prefix <- substr(raw_prescriptions$atc, 1, 4) |> unique()
    if (all(atc_prefix == "N06A")) {
      report_ok("All prescriptions are N06A (antidepressants)")
    } else {
      unexpected <- setdiff(atc_prefix, "N06A")
      report_warning(sprintf("Some ATC codes not N06A: %s", paste(head(unexpected, 5), collapse = ", ")))
    }
  }

  # Check date range
  if ("edatum" %in% names(raw_prescriptions)) {
    date_range <- range(raw_prescriptions$edatum, na.rm = TRUE)
    report_info(sprintf("Date range: %s to %s", date_range[1], date_range[2]))
  }

  rm(raw_prescriptions)
} else {
  report_error("File not found: raw_prescriptions.rds")
}

# =============================================================================
# 3. Script 03: raw_individual.rds
# =============================================================================
cat("\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("3. SCRIPT 03: raw_individual.rds\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

if (rds_exists("raw_individual.rds")) {
  raw_individual <- read_rds("raw_individual.rds")
  report_info(sprintf("Rows: %s", format(nrow(raw_individual), big.mark = ",")))

  # Check required columns
  required_cols <- c("lopnr", "bdate", "sex")
  missing_cols <- setdiff(required_cols, names(raw_individual))
  if (length(missing_cols) == 0) {
    report_ok("All required columns present")
  } else {
    report_error(sprintf("Missing columns: %s", paste(missing_cols, collapse = ", ")))
  }

  # Check unique lopnr
  n_unique <- n_distinct(raw_individual$lopnr)
  if (n_unique == nrow(raw_individual)) {
    report_ok(sprintf("All %s lopnr are unique", format(n_unique, big.mark = ",")))
  } else {
    report_error(sprintf("Duplicate lopnr: %d unique out of %d rows", n_unique, nrow(raw_individual)))
  }

  # Check sex values (1=male, 2=female)
  if ("sex" %in% names(raw_individual)) {
    sex_vals <- unique(raw_individual$sex)
    if (all(sex_vals %in% c(1, 2))) {
      report_ok("sex values are 1 or 2")
    } else {
      report_error(sprintf("Unexpected sex values: %s", paste(sex_vals, collapse = ", ")))
    }
  }

  rm(raw_individual)
} else {
  report_error("File not found: raw_individual.rds")
}

# =============================================================================
# 4. Script 04: cohort files
# =============================================================================
cat("\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("4. SCRIPT 04: Cohort definition files\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

# 4a. cohort_lopnrs.rds
if (rds_exists("cohort_lopnrs.rds")) {
  cohort_lopnrs <- read_rds("cohort_lopnrs.rds")
  n_cohort <- nrow(cohort_lopnrs)
  report_info(sprintf("cohort_lopnrs.rds: %s rows", format(n_cohort, big.mark = ",")))

  if (n_distinct(cohort_lopnrs$lopnr) == n_cohort) {
    report_ok("All cohort lopnr are unique")
  } else {
    report_error("Duplicate lopnr in cohort_lopnrs")
  }
} else {
  report_error("File not found: cohort_lopnrs.rds")
}

# 4b. parent_lopnrs.rds
if (rds_exists("parent_lopnrs.rds")) {
  parent_lopnrs <- read_rds("parent_lopnrs.rds")
  n_parents <- nrow(parent_lopnrs)
  report_info(sprintf("parent_lopnrs.rds: %s rows", format(n_parents, big.mark = ",")))

  if (n_distinct(parent_lopnrs$lopnr) == n_parents) {
    report_ok("All parent lopnr are unique")
  } else {
    report_error("Duplicate lopnr in parent_lopnrs")
  }
} else {
  report_error("File not found: parent_lopnrs.rds")
}

# 4c. cohort_base.rds
if (rds_exists("cohort_base.rds")) {
  cohort_base <- read_rds("cohort_base.rds")
  report_info(sprintf("cohort_base.rds: %s rows", format(nrow(cohort_base), big.mark = ",")))

  # Check required columns (cc is added in later processing, not in raw cohort_base)
  required_cols <- c("lopnr", "diagn_date", "bdate")
  missing_cols <- setdiff(required_cols, names(cohort_base))
  if (length(missing_cols) == 0) {
    report_ok("cohort_base has all required columns")
  } else {
    report_error(sprintf("cohort_base missing columns: %s", paste(missing_cols, collapse = ", ")))
  }

  # Check for parent lopnr columns
  if (all(c("lopnrmor", "lopnrfar") %in% names(cohort_base))) {
    report_ok("Parent lopnr columns present (lopnrmor, lopnrfar)")
    n_with_mother <- sum(!is.na(cohort_base$lopnrmor))
    n_with_father <- sum(!is.na(cohort_base$lopnrfar))
    report_info(sprintf("Cohort members with mother: %s, with father: %s",
                        format(n_with_mother, big.mark = ","),
                        format(n_with_father, big.mark = ",")))
  }

  # Check cohort_base matches cohort_lopnrs
  if (exists("cohort_lopnrs") && exists("n_cohort")) {
    if (nrow(cohort_base) == n_cohort) {
      report_ok("cohort_base row count matches cohort_lopnrs")
    } else {
      report_warning(sprintf("cohort_base (%d) != cohort_lopnrs (%d)", nrow(cohort_base), n_cohort))
    }
  }

  rm(cohort_base)
} else {
  report_error("File not found: cohort_base.rds")
}

# 4d. cohort_flow_summary.rds
if (rds_exists("cohort_flow_summary.rds")) {
  cohort_flow <- read_rds("cohort_flow_summary.rds")
  report_ok("cohort_flow_summary.rds exists")
} else {
  report_warning("File not found: cohort_flow_summary.rds")
}

if (exists("cohort_lopnrs")) rm(cohort_lopnrs)
if (exists("parent_lopnrs")) rm(parent_lopnrs)

# =============================================================================
# 5. Script 05: raw_migration.rds
# =============================================================================
cat("\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("5. SCRIPT 05: raw_migration.rds\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

if (rds_exists("raw_migration.rds")) {
  raw_migration <- read_rds("raw_migration.rds")
  report_info(sprintf("Rows: %s", format(nrow(raw_migration), big.mark = ",")))

  # Check required columns
  required_cols <- c("lopnr", "date_emig")
  missing_cols <- setdiff(required_cols, names(raw_migration))
  if (length(missing_cols) == 0) {
    report_ok("All required columns present")
  } else {
    report_error(sprintf("Missing columns: %s", paste(missing_cols, collapse = ", ")))
  }

  # Check unique lopnr
  n_unique <- n_distinct(raw_migration$lopnr)
  report_info(sprintf("Unique lopnr: %s", format(n_unique, big.mark = ",")))

  # Check date range
  if ("date_emig" %in% names(raw_migration)) {
    date_range <- range(raw_migration$date_emig, na.rm = TRUE)
    report_info(sprintf("Emigration date range: %s to %s", date_range[1], date_range[2]))
  }

  rm(raw_migration)
} else {
  report_error("File not found: raw_migration.rds")
}

# =============================================================================
# 6. Script 06: raw_dor.rds
# =============================================================================
cat("\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("6. SCRIPT 06: raw_dor.rds (Death registry)\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

if (rds_exists("raw_dor.rds")) {
  raw_dor <- read_rds("raw_dor.rds")
  report_info(sprintf("Rows: %s", format(nrow(raw_dor), big.mark = ",")))

  # Check required columns
  required_cols <- c("lopnr", "date_death")
  missing_cols <- setdiff(required_cols, names(raw_dor))
  if (length(missing_cols) == 0) {
    report_ok("All required columns present")
  } else {
    report_error(sprintf("Missing columns: %s", paste(missing_cols, collapse = ", ")))
  }

  # Check unique lopnr (each person should die only once)
  n_unique <- n_distinct(raw_dor$lopnr)
  if (n_unique == nrow(raw_dor)) {
    report_ok(sprintf("All %s lopnr are unique (one death per person)", format(n_unique, big.mark = ",")))
  } else {
    report_error(sprintf("Duplicate lopnr in death registry: %d unique out of %d rows", n_unique, nrow(raw_dor)))
  }

  # Check for cause of death column
  if ("cause" %in% names(raw_dor)) {
    report_ok("Cause of death column present")
  }

  # Check date range
  if ("date_death" %in% names(raw_dor)) {
    date_range <- range(raw_dor$date_death, na.rm = TRUE)
    report_info(sprintf("Death date range: %s to %s", date_range[1], date_range[2]))
  }

  rm(raw_dor)
} else {
  report_error("File not found: raw_dor.rds")
}

# =============================================================================
# 7. Script 07: raw_diagnoses_cohort.rds
# =============================================================================
cat("\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("7. SCRIPT 07: raw_diagnoses_cohort.rds\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

if (rds_exists("raw_diagnoses_cohort.rds")) {
  raw_diagnoses_cohort <- read_rds("raw_diagnoses_cohort.rds")
  report_info(sprintf("Rows: %s", format(nrow(raw_diagnoses_cohort), big.mark = ",")))

  # Check required columns
  required_cols <- c("lopnr", "dia", "diagn_date")
  missing_cols <- setdiff(required_cols, names(raw_diagnoses_cohort))
  if (length(missing_cols) == 0) {
    report_ok("All required columns present")
  } else {
    report_error(sprintf("Missing columns: %s", paste(missing_cols, collapse = ", ")))
  }

  # Check unique lopnr count
  n_unique <- n_distinct(raw_diagnoses_cohort$lopnr)
  report_info(sprintf("Unique lopnr: %s", format(n_unique, big.mark = ",")))

  # Check all lopnr are in cohort
  if (rds_exists("cohort_lopnrs.rds")) {
    cohort_lopnrs <- read_rds("cohort_lopnrs.rds")$lopnr
    n_in_cohort <- sum(unique(raw_diagnoses_cohort$lopnr) %in% cohort_lopnrs)
    if (n_in_cohort == n_unique) {
      report_ok("All diagnoses lopnr are in cohort")
    } else {
      report_error(sprintf("%d lopnr not in cohort", n_unique - n_in_cohort))
    }
    rm(cohort_lopnrs)
  }

  # Check diagnosis code distribution
  if ("dia" %in% names(raw_diagnoses_cohort)) {
    dia_prefix <- substr(raw_diagnoses_cohort$dia, 1, 1)
    prefix_counts <- table(dia_prefix)
    report_info(sprintf("Diagnosis prefix distribution: %s",
                        paste(names(prefix_counts), prefix_counts, sep = "=", collapse = ", ")))
  }

  rm(raw_diagnoses_cohort)
} else {
  report_error("File not found: raw_diagnoses_cohort.rds")
}

# =============================================================================
# 8. Script 08: raw_diagnoses_parents.rds
# =============================================================================
cat("\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("8. SCRIPT 08: raw_diagnoses_parents.rds\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

if (rds_exists("raw_diagnoses_parents.rds")) {
  raw_diagnoses_parents <- read_rds("raw_diagnoses_parents.rds")
  report_info(sprintf("Rows: %s", format(nrow(raw_diagnoses_parents), big.mark = ",")))

  # Check required columns
  required_cols <- c("lopnr", "dia", "diagn_date")
  missing_cols <- setdiff(required_cols, names(raw_diagnoses_parents))
  if (length(missing_cols) == 0) {
    report_ok("All required columns present")
  } else {
    report_error(sprintf("Missing columns: %s", paste(missing_cols, collapse = ", ")))
  }

  # Check unique lopnr count
  n_unique <- n_distinct(raw_diagnoses_parents$lopnr)
  report_info(sprintf("Unique parent lopnr: %s", format(n_unique, big.mark = ",")))

  # Check all lopnr are in parent_lopnrs
  if (rds_exists("parent_lopnrs.rds")) {
    parent_lopnrs <- read_rds("parent_lopnrs.rds")$lopnr
    n_in_parents <- sum(unique(raw_diagnoses_parents$lopnr) %in% parent_lopnrs)
    if (n_in_parents == n_unique) {
      report_ok("All parent diagnoses lopnr are in parent_lopnrs")
    } else {
      report_error(sprintf("%d lopnr not in parent_lopnrs", n_unique - n_in_parents))
    }
    rm(parent_lopnrs)
  }

  # Check diagnosis codes (should be F32/F33, X60-X84, Y10-Y34)
  if ("dia" %in% names(raw_diagnoses_parents)) {
    dia_prefix <- substr(raw_diagnoses_parents$dia, 1, 3)
    valid_prefixes <- c("F32", "F33",
                        paste0("X", 60:84),
                        paste0("Y", 10:34))
    invalid <- dia_prefix[!dia_prefix %in% valid_prefixes] |> unique()
    if (length(invalid) == 0) {
      report_ok("All diagnosis codes are valid (F32/F33, X60-X84, Y10-Y34)")
    } else {
      report_error(sprintf("Invalid diagnosis prefixes: %s", paste(head(invalid, 10), collapse = ", ")))
    }

    # Count by category
    n_depression <- sum(dia_prefix %in% c("F32", "F33"))
    n_suicidal <- sum(dia_prefix %in% c(paste0("X", 60:84), paste0("Y", 10:34)))
    report_info(sprintf("Depression diagnoses: %s, Suicidal behavior: %s",
                        format(n_depression, big.mark = ","),
                        format(n_suicidal, big.mark = ",")))
  }

  rm(raw_diagnoses_parents)
} else {
  report_error("File not found: raw_diagnoses_parents.rds")
}

# =============================================================================
# 9. Script 09: raw_lisa.rds
# =============================================================================
cat("\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("9. SCRIPT 09: raw_lisa.rds (Education/Income)\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

if (rds_exists("raw_lisa.rds")) {
  raw_lisa <- read_rds("raw_lisa.rds")
  report_info(sprintf("Rows: %s", format(nrow(raw_lisa), big.mark = ",")))

  # Check required columns
  required_cols <- c("lopnr", "year", "income")
  missing_cols <- setdiff(required_cols, names(raw_lisa))
  if (length(missing_cols) == 0) {
    report_ok("All required columns present")
  } else {
    report_error(sprintf("Missing columns: %s", paste(missing_cols, collapse = ", ")))
  }

  # Check unique lopnr count
  n_unique <- n_distinct(raw_lisa$lopnr)
  report_info(sprintf("Unique lopnr: %s", format(n_unique, big.mark = ",")))

  # Check all lopnr are in parent_lopnrs
  if (rds_exists("parent_lopnrs.rds")) {
    parent_lopnrs <- read_rds("parent_lopnrs.rds")$lopnr
    n_in_parents <- sum(unique(raw_lisa$lopnr) %in% parent_lopnrs)
    pct <- 100 * n_in_parents / n_unique
    if (pct > 95) {
      report_ok(sprintf("%.1f%% of LISA lopnr are in parent_lopnrs", pct))
    } else {
      report_warning(sprintf("Only %.1f%% of LISA lopnr are in parent_lopnrs", pct))
    }
    rm(parent_lopnrs)
  }

  # Check year range
  if ("year" %in% names(raw_lisa)) {
    year_range <- range(raw_lisa$year, na.rm = TRUE)
    report_info(sprintf("Year range: %d-%d", year_range[1], year_range[2]))
    if (year_range[1] >= 2004 && year_range[2] <= 2020) {
      report_ok("Year range within expected bounds (2004-2020)")
    } else {
      report_warning("Year range outside expected bounds")
    }
  }

  # Check education columns
  edu_cols <- grep("edu", names(raw_lisa), value = TRUE)
  if (length(edu_cols) > 0) {
    report_info(sprintf("Education columns: %s", paste(edu_cols, collapse = ", ")))
  }

  rm(raw_lisa)
} else {
  report_error("File not found: raw_lisa.rds")
}

# =============================================================================
# 10. Script 10: raw_hospitalization.rds
# =============================================================================
cat("\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("10. SCRIPT 10: raw_hospitalization.rds\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")

if (rds_exists("raw_hospitalization.rds")) {
  raw_hospitalization <- read_rds("raw_hospitalization.rds")
  report_info(sprintf("Rows: %s", format(nrow(raw_hospitalization), big.mark = ",")))

  # Check required columns
  required_cols <- c("lopnr", "diagn_date")
  missing_cols <- setdiff(required_cols, names(raw_hospitalization))
  if (length(missing_cols) == 0) {
    report_ok("All required columns present")
  } else {
    report_error(sprintf("Missing columns: %s", paste(missing_cols, collapse = ", ")))
  }

  # Check unique lopnr count
  n_unique <- n_distinct(raw_hospitalization$lopnr)
  report_info(sprintf("Unique lopnr: %s", format(n_unique, big.mark = ",")))

  # Check all lopnr are in cohort
  if (rds_exists("cohort_lopnrs.rds")) {
    cohort_lopnrs <- read_rds("cohort_lopnrs.rds")$lopnr
    n_in_cohort <- sum(unique(raw_hospitalization$lopnr) %in% cohort_lopnrs)
    if (n_in_cohort == n_unique) {
      report_ok("All hospitalization lopnr are in cohort")
    } else {
      report_error(sprintf("%d lopnr not in cohort", n_unique - n_in_cohort))
    }
    rm(cohort_lopnrs)
  }

  # Check stay_days if present
  if ("stay_days" %in% names(raw_hospitalization)) {
    n_negative <- sum(raw_hospitalization$stay_days < 0, na.rm = TRUE)
    if (n_negative == 0) {
      report_ok("All stay_days are non-negative")
    } else {
      report_error(sprintf("%d records have negative stay_days", n_negative))
    }
  }

  # Check source column if present
  if ("source" %in% names(raw_hospitalization)) {
    source_vals <- unique(raw_hospitalization$source)
    report_info(sprintf("Source values: %s", paste(source_vals, collapse = ", ")))
  }

  rm(raw_hospitalization)
} else {
  report_error("File not found: raw_hospitalization.rds")
}

# =============================================================================
# Summary
# =============================================================================
cat("\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("VERIFICATION SUMMARY\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat(sprintf("Errors:   %d\n", errors))
cat(sprintf("Warnings: %d\n", warnings))

if (errors == 0) {
  cat("\nAll verification checks passed!\n")
} else {
  cat("\nPlease review and fix the errors above.\n")
}

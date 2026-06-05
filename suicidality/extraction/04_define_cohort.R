# define_cohort.R
# Phase 1b: Define cohort from bootstrap data
#
# This is the only script that does both extraction and processing.
# It's necessary to break the circular dependency: we need the cohort
# to know what to extract, but we need data to define the cohort.
#
# Per Lagerberg 2023: Cohort includes BOTH initiators (SSRI within 28 days)
# AND non-initiators (no SSRI within 28 days of diagnosis)
#
# Steps:
# 1. Filter by age 6-24 at diagnosis
# 2. Apply 365-day antidepressant washout (using N06A)
# 3. Find SSRI prescriptions (for classification, not exclusion)
# 4. Extract parent lopnrs from v_parent
#
# Inputs: raw_diagnoses_index.rds, raw_prescriptions.rds, raw_individual.rds
# Outputs: cohort_lopnrs.rds, parent_lopnrs.rds, cohort_base.rds

library(DBI)
library(odbc)
library(dplyr)
library(here)
here::i_am("suicidality/extraction/04_define_cohort.R")

source(here("suicidality", "extraction", "lib", "common.R"))

define_cohort <- function(output_dir = rds_output_dir()) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat("=== define_cohort.R ===\n\n")

  # Load bootstrap data
  raw_diagnoses_index <- read_rds("raw_diagnoses_index.rds")
  raw_prescriptions <- read_rds("raw_prescriptions.rds")
  raw_individual <- read_rds("raw_individual.rds")

  cat("raw_diagnoses_index rows:", nrow(raw_diagnoses_index), "\n")
  cat("raw_prescriptions rows:", nrow(raw_prescriptions), "\n")
  cat("raw_individual rows:", nrow(raw_individual), "\n")

  # Step 1: Join diagnoses with birth dates and filter by age
  cat("\nStep 1: Age filtering (6-24)...\n")

  diagnoses_with_age <- raw_diagnoses_index %>%
    left_join(raw_individual %>% select(lopnr, bdate), by = "lopnr") %>%
    mutate(
      # Use proper year-based age calculation (same as final age in cohort_base)
      year_diff = as.integer(format(diagn_date, "%Y")) - as.integer(format(bdate, "%Y")),
      bday_occurred = (format(diagn_date, "%m%d") >= format(bdate, "%m%d")),
      age_diagn = as.integer(year_diff - ifelse(bday_occurred, 0L, 1L))
    ) %>%
    filter(age_diagn >= 6 & age_diagn <= 24) %>%
    select(-year_diff, -bday_occurred)

  cat("Diagnoses after age filter:", nrow(diagnoses_with_age), "\n")
  cat("Unique lopnr after age filter:", n_distinct(diagnoses_with_age$lopnr), "\n")

  # Step 2: Apply 365-day antidepressant washout
  cat("\nStep 2: Applying 365-day washout...\n")

  # Get N06A prescriptions (all antidepressants) for washout
  n06a_prescriptions <- raw_prescriptions %>%
    filter(substr(atc, 1, 4) == "N06A") %>%
    select(lopnr, prescr = edatum) %>%
    distinct()

  cat("N06A prescription records:", nrow(n06a_prescriptions), "\n")

  # Get first diagnosis per lopnr (to filter prescriptions)
  first_diagn <- diagnoses_with_age %>%
    arrange(lopnr, diagn_date) %>%
    group_by(lopnr) %>%
    slice(1) %>%
    ungroup()

  # Filter prescriptions to cohort members only
  cohort_lopnrs_temp <- unique(first_diagn$lopnr)
  n06a_prescriptions <- n06a_prescriptions %>%
    filter(lopnr %in% cohort_lopnrs_temp) %>%
    arrange(lopnr, prescr)

  cat("N06A prescriptions for cohort:", nrow(n06a_prescriptions), "\n")

  # Join diagnoses with prescriptions
  # A diagnosis is eligible if:
  # - No prior prescriptions (all prescr dates are NA or after diagn_date)
  # - OR all prior prescriptions are >= 365 days before diagnosis
  washout <- 365L

  # Join all diagnoses with all prescriptions for the same lopnr
  base_prescr <- diagnoses_with_age %>%
    left_join(n06a_prescriptions, by = "lopnr", relationship = "many-to-many") %>%
    arrange(lopnr, diagn_date, prescr) %>%
    group_by(lopnr, diagn_date, prescr) %>%
    slice(1) %>%
    ungroup()

  # Calculate diff = diagn_date - prescr
  base_prescr <- base_prescr %>%
    mutate(diff = as.integer(diagn_date - prescr))

  # For each diagnosis, check eligibility
  safe_min <- function(x) if (all(is.na(x))) NA_integer_ else min(x, na.rm = TRUE)
  safe_max <- function(x) if (all(is.na(x))) NA_integer_ else max(x, na.rm = TRUE)

  alldiff_diagn <- base_prescr %>%
    group_by(lopnr, diagn_date, dia, bdate, source) %>%
    summarise(
      min_diff = safe_min(diff),
      max_diff = safe_max(diff),
      .groups = "drop"
    )

  # A diagnosis is eligible if:
  # 1. No prescriptions before it (max_diff <= 0 or NA)
  # 2. OR all prescriptions are >= 365 days before (min_diff >= 365 among positive diffs)
  alldiff_keep <- alldiff_diagn %>%
    mutate(
      keep = case_when(
        max_diff <= 0 ~ 1L,
        is.na(min_diff) & is.na(max_diff) ~ 1L,
        min_diff >= washout ~ 1L,
        TRUE ~ 0L
      )
    ) %>%
    filter(keep == 1)

  # For diagnoses with mixed diffs, check positive diffs only
  posdiff <- base_prescr %>%
    filter(diff > 0)

  posdiff_diagn <- posdiff %>%
    group_by(lopnr, diagn_date) %>%
    summarise(
      dia = max(dia, na.rm = TRUE),
      bdate = max(bdate, na.rm = TRUE),
      source = max(source, na.rm = TRUE),
      min_diff = safe_min(diff),
      max_diff = safe_max(diff),
      .groups = "drop"
    ) %>%
    mutate(keep = ifelse(min_diff >= washout, 1L, 0L)) %>%
    filter(keep == 1)

  # Combine eligible diagnoses
  eligible_diagnoses <- bind_rows(alldiff_keep, posdiff_diagn) %>%
    select(lopnr, diagn_date, dia, bdate, source) %>%
    distinct() %>%
    arrange(lopnr, diagn_date)

  cat("Eligible diagnoses after washout:", nrow(eligible_diagnoses), "\n")
  cat("Unique lopnr after washout:", n_distinct(eligible_diagnoses$lopnr), "\n")

  # Step 3: Find SSRI prescriptions and classify initiators vs non-initiators
  # Per Lagerberg 2023: initiators = SSRI (N06AB) within 28 days of diagnosis
  # Non-initiators = no SSRI within 28 days of diagnosis
  # BOTH groups are included in the cohort
  cat("\nStep 3: Identifying SSRI prescriptions...\n")

  # Get first eligible diagnosis per lopnr
  first_eligible <- eligible_diagnoses %>%
    arrange(lopnr, diagn_date) %>%
    group_by(lopnr) %>%
    slice(1) %>%
    ungroup()

  cat("First eligible diagnoses:", nrow(first_eligible), "\n")

  # Get N06AB (SSRI) prescriptions for eligible patients
  ssri_prescriptions <- raw_prescriptions %>%
    filter(substr(atc, 1, 5) == "N06AB") %>%
    select(lopnr, atc, prescr = edatum) %>%
    filter(lopnr %in% first_eligible$lopnr)

  cat("SSRI prescriptions for eligible:", nrow(ssri_prescriptions), "\n")

  # Find first SSRI prescription on or after diagnosis (for those who have any)
  first_ssri_after_diagn <- first_eligible %>%
    left_join(ssri_prescriptions, by = "lopnr", relationship = "many-to-many") %>%
    filter(prescr >= diagn_date) %>%  # Only prescriptions on/after diagnosis
    arrange(lopnr, prescr) %>%
    group_by(lopnr) %>%
    slice(1) %>%
    ungroup() %>%
    select(lopnr, atc, prescr)

  cat("Patients with SSRI after diagnosis:", nrow(first_ssri_after_diagn), "\n")

  # LEFT JOIN back to first_eligible to include ALL eligible patients
  # (both those with and without SSRI prescriptions after diagnosis)
  cohort_base <- first_eligible %>%
    left_join(first_ssri_after_diagn, by = "lopnr")

  # Calculate age at diagnosis (proper year calculation)
  cohort_base <- cohort_base %>%
    mutate(
      year_diff = as.integer(format(diagn_date, "%Y")) - as.integer(format(bdate, "%Y")),
      bday_occurred = (format(diagn_date, "%m%d") >= format(bdate, "%m%d")),
      age = as.integer(year_diff - ifelse(bday_occurred, 0L, 1L)),
      agecat = ifelse(age >= 18, "18-24", "6-17")
    ) %>%
    select(-year_diff, -bday_occurred)

  cat("Final cohort (initiators + non-initiators):", nrow(cohort_base), "\n")

  # Step 4: Extract parent lopnrs
  cat("\nStep 4: Extracting parent lopnrs...\n")

  con <- db_connect()
  on.exit(dbDisconnect(con))

  cohort_lopnrs <- unique(cohort_base$lopnr)

  parents <- dbGetQuery(con, "
    SELECT lopnr, lopnrmor, lopnrfar
    FROM dbo.v_parent
  ") %>%
    filter(lopnr %in% cohort_lopnrs)

  cat("Parent records:", nrow(parents), "\n")

  # Join parents with cohort
  cohort_base <- cohort_base %>%
    left_join(parents, by = "lopnr")

  # Get unique parent lopnrs
  mother_lopnrs <- unique(parents$lopnrmor[!is.na(parents$lopnrmor)])
  father_lopnrs <- unique(parents$lopnrfar[!is.na(parents$lopnrfar)])
  parent_lopnrs <- unique(c(mother_lopnrs, father_lopnrs))

  cat("Unique mothers:", length(mother_lopnrs), "\n")
  cat("Unique fathers:", length(father_lopnrs), "\n")
  cat("Total unique parents:", length(parent_lopnrs), "\n")

  # Save outputs
  save_rds(data.frame(lopnr = cohort_lopnrs), "cohort_lopnrs.rds")
  save_rds(data.frame(lopnr = parent_lopnrs), "parent_lopnrs.rds")
  save_rds(cohort_base, "cohort_base.rds")

  cat("\nSaved cohort_lopnrs.rds:", length(cohort_lopnrs), "lopnrs\n")
  cat("Saved parent_lopnrs.rds:", length(parent_lopnrs), "lopnrs\n")
  cat("Saved cohort_base.rds:", nrow(cohort_base), "rows\n")

  # Save cohort flow summary for flowchart
  n_with_ssri <- sum(!is.na(cohort_base$prescr))
  cohort_summary <- list(
    n_diagnoses_initial = nrow(raw_diagnoses_index),
    n_unique_initial = n_distinct(raw_diagnoses_index$lopnr),
    n_after_age_filter = n_distinct(diagnoses_with_age$lopnr),
    n_eligible_after_washout = n_distinct(eligible_diagnoses$lopnr),
    n_final_cohort = length(cohort_lopnrs),
    n_with_ssri_after_diagnosis = n_with_ssri,
    n_without_ssri_after_diagnosis = length(cohort_lopnrs) - n_with_ssri
  )
  save_rds(cohort_summary, "cohort_flow_summary.rds")

  cat("\n=== define_cohort.R completed ===\n")
  invisible(list(cohort_lopnrs = cohort_lopnrs, parent_lopnrs = parent_lopnrs))
}

if (sys.nframe() == 0) {
  define_cohort()
}

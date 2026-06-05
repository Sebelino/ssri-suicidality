# process_cov_medications.R
# Phase 2: Process prescriptions to create medication covariates
#
# Creates indicators for prior medication use before follow-up start
#
# Inputs: main_12wks_28_tmp.rds, raw_prescriptions_cohort.rds
# Output: cov_medications.rds, othermeds_28.rds

library(dplyr)
library(here)
here::i_am("suicidality/extraction/20_process_cov_medications.R")

source(here("suicidality", "extraction", "lib", "common.R"))

process_cov_medications <- function(output_dir = rds_output_dir(), grace_days = 28L) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat(sprintf("=== process_cov_medications.R (grace_days = %d) ===\n\n", grace_days))

  # Load data
  main_file <- sprintf("main_12wks_%d_tmp.rds", grace_days)
  main_tmp <- read_rds(main_file)
  raw_prescriptions <- read_rds("raw_prescriptions_cohort.rds")

  cat(main_file, "rows:", nrow(main_tmp), "\n")
  cat("raw_prescriptions_cohort rows:", nrow(raw_prescriptions), "\n")

  # Get fu_start dates
  fu_start_data <- main_tmp %>%
    select(lopnr, fu_start)

  cohort_lopnrs <- unique(fu_start_data$lopnr)

  # Filter prescriptions to cohort members
  # Note: Lithium (N05AN) is NOT excluded here - it's needed for med_mood_stabilizer
  # Lithium is excluded from med_antipsychotic specifically in line 78
  cat("\nFiltering prescriptions...\n")

  meds_filtered <- raw_prescriptions %>%
    filter(lopnr %in% cohort_lopnrs)

  cat("Prescriptions after filtering:", nrow(meds_filtered), "\n")

  # Save othermeds_28 for time-varying analysis (28-day grace only; pp pipeline
  # does not consume this for the 14-day sensitivity).
  if (grace_days == 28L) {
    othermeds_28 <- meds_filtered %>%
      rename(otherprescr = edatum)
    save_rds(othermeds_28, "othermeds_28.rds")
    cat("Saved othermeds_28.rds:", nrow(othermeds_28), "rows\n")
  }

  # Filter to 90 days (3 months) before fu_start for covariates
  # Paper: "medication receipt within last 3 months"
  meds_cov <- meds_filtered %>%
    inner_join(fu_start_data, by = "lopnr") %>%
    filter(edatum >= fu_start - 90 & edatum <= fu_start) %>%  # Include same-day meds
    select(lopnr, atc)

  cat("Medications in 90-day window before fu_start:", nrow(meds_cov), "\n")

  # Define medication groups
  # med_antipsychotic: N05A excl N05AN (antipsychotics, excluding lithium)
  # med_hypnotic: N05C (hypnotics/sedatives)
  # med_benzodiazepine: N05BA (benzodiazepines)
  # med_antiepileptic: N03A excl mood stabilizers (antiepileptics, excluding valproate/lamotrigine/carbamazepine)
  # med_stimulant: N06B (psychostimulants/ADHD meds)
  # med_addiction: N07B (drugs for addictive disorders)
  # med_opioid: N02A (opioids)
  # med_mood_stabilizer: N05AN (lithium), N03AG01 (valproate), N03AX09 (lamotrigine), N03AF01 (carbamazepine)

  cat("\nCreating medication indicators...\n")

  # Mood stabilizer ATC codes (subset of N03A used as mood stabilizers)
  mood_stabilizer_codes <- c("N03AG01", "N03AX09", "N03AF01")

  cov_medications <- fu_start_data %>%
    left_join(
      meds_cov %>%
        mutate(
          med_antipsychotic = if_else(substr(atc, 1, 4) == "N05A" & !grepl("^N05AN", atc), 1L, 0L),
          med_hypnotic = if_else(substr(atc, 1, 4) == "N05C", 1L, 0L),
          med_benzodiazepine = if_else(substr(atc, 1, 5) == "N05BA", 1L, 0L),
          # Antiepileptics excluding mood stabilizers (valproate, lamotrigine, carbamazepine)
          med_antiepileptic = if_else(substr(atc, 1, 4) == "N03A" & !(atc %in% mood_stabilizer_codes), 1L, 0L),
          med_stimulant = if_else(substr(atc, 1, 4) == "N06B", 1L, 0L),
          med_addiction = if_else(substr(atc, 1, 4) == "N07B", 1L, 0L),
          med_opioid = if_else(substr(atc, 1, 4) == "N02A", 1L, 0L),
          # Mood stabilizers: lithium (N05AN), valproate (N03AG01), lamotrigine (N03AX09), carbamazepine (N03AF01)
          med_mood_stabilizer = if_else(grepl("^N05AN", atc) | atc %in% mood_stabilizer_codes, 1L, 0L)
        ) %>%
        group_by(lopnr) %>%
        summarise(
          med_antipsychotic = max(med_antipsychotic, na.rm = TRUE),
          med_hypnotic = max(med_hypnotic, na.rm = TRUE),
          med_benzodiazepine = max(med_benzodiazepine, na.rm = TRUE),
          med_antiepileptic = max(med_antiepileptic, na.rm = TRUE),
          med_stimulant = max(med_stimulant, na.rm = TRUE),
          med_addiction = max(med_addiction, na.rm = TRUE),
          med_opioid = max(med_opioid, na.rm = TRUE),
          med_mood_stabilizer = max(med_mood_stabilizer, na.rm = TRUE),
          .groups = "drop"
        ),
      by = "lopnr"
    ) %>%
    mutate(across(c(med_antipsychotic, med_hypnotic, med_benzodiazepine, med_antiepileptic,
                    med_stimulant, med_addiction, med_opioid, med_mood_stabilizer),
                  ~if_else(is.na(.), 0L, .))) %>%
    select(-fu_start)

  cat("\nMedication covariate summary:\n")
  cov_medications %>%
    summarise(across(-lopnr, sum)) %>%
    tidyr::pivot_longer(everything(), names_to = "covariate", values_to = "count") %>%
    print()

  out_file <- if (grace_days == 28L) "cov_medications.rds" else sprintf("cov_medications_%d.rds", grace_days)
  save_rds(cov_medications, out_file)
  cat("\nSaved", out_file, "\n")

  cat("\n=== process_cov_medications.R completed ===\n")
  invisible(cov_medications)
}

if (sys.nframe() == 0) {
  process_cov_medications()
}

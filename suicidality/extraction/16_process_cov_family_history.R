# process_cov_family_history.R
# Phase 2: Process parent diagnoses to create family history covariates
#
# Inputs: cohort_base.rds, raw_diagnoses_parents.rds
# Output: cov_family_history.rds

library(dplyr)
library(here)
here::i_am("suicidality/extraction/16_process_cov_family_history.R")

source(here("suicidality", "extraction", "lib", "common.R"))

process_cov_family_history <- function(output_dir = rds_output_dir()) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat("=== process_cov_family_history.R ===\n\n")

  # Load data
  cohort_base <- read_rds("cohort_base.rds")
  raw_diagnoses_parents <- read_rds("raw_diagnoses_parents.rds")

  cat("cohort_base rows:", nrow(cohort_base), "\n")
  cat("raw_diagnoses_parents rows:", nrow(raw_diagnoses_parents), "\n")

  # Suicidal behavior codes
  suicidal_codes <- c(paste0("X", 60:84), paste0("Y", 10:34))
  depression_codes <- c("F32", "F33")

  # Drop chapter-range placeholder codes ("F32-", "F33-", etc.) — see BUGS
  # discussion: their 3-char prefix collides with real F32/F33 / X.. / Y..
  # codes and would inflate parental depression / suicidal-behaviour flags
  # by a small but non-zero amount.
  parents_clean <- raw_diagnoses_parents %>%
    filter(!grepl("-", dia))

  # Get suicidal history per parent
  cat("\nProcessing suicidal history...\n")

  parent_suicidal <- parents_clean %>%
    filter(substr(dia, 1, 3) %in% suicidal_codes) %>%
    select(lopnr) %>%
    distinct() %>%
    mutate(has_suicidal = 1L)

  cat("Parents with suicidal history:", nrow(parent_suicidal), "\n")

  # Get depression history per parent
  cat("Processing depression history...\n")

  parent_depression <- parents_clean %>%
    filter(substr(dia, 1, 3) %in% depression_codes) %>%
    select(lopnr) %>%
    distinct() %>%
    mutate(has_depression = 1L)

  cat("Parents with depression history:", nrow(parent_depression), "\n")

  # Join with cohort to get family history
  cov_family_history <- cohort_base %>%
    select(lopnr, lopnrmor, lopnrfar) %>%
    # Mother suicidal
    left_join(parent_suicidal %>% rename(lopnrmor = lopnr, mor_suicidal = has_suicidal),
              by = "lopnrmor") %>%
    # Father suicidal
    left_join(parent_suicidal %>% rename(lopnrfar = lopnr, far_suicidal = has_suicidal),
              by = "lopnrfar") %>%
    # Mother depression
    left_join(parent_depression %>% rename(lopnrmor = lopnr, mor_depr = has_depression),
              by = "lopnrmor") %>%
    # Father depression
    left_join(parent_depression %>% rename(lopnrfar = lopnr, far_depr = has_depression),
              by = "lopnrfar") %>%
    mutate(
      # Check if both parents are missing (no family history available)
      both_parents_missing = is.na(lopnrmor) & is.na(lopnrfar),
      # Family history: count of parents (0, 1, 2, or 99 for missing)
      mor_suicidal = if_else(is.na(mor_suicidal), 0L, mor_suicidal),
      far_suicidal = if_else(is.na(far_suicidal), 0L, far_suicidal),
      mor_depr = if_else(is.na(mor_depr), 0L, mor_depr),
      far_depr = if_else(is.na(far_depr), 0L, far_depr),
      fh_suicidal = if_else(both_parents_missing, 99L, as.integer(mor_suicidal + far_suicidal)),
      fh_depr = if_else(both_parents_missing, 99L, as.integer(mor_depr + far_depr))
    ) %>%
    select(lopnr, fh_suicidal, fh_depr)

  cat("\nFamily history summary:\n")
  cat("fh_suicidal: 0 =", sum(cov_family_history$fh_suicidal == 0),
      ", 1 =", sum(cov_family_history$fh_suicidal == 1),
      ", 2 =", sum(cov_family_history$fh_suicidal == 2),
      ", 99 (missing) =", sum(cov_family_history$fh_suicidal == 99), "\n")
  cat("fh_depr: 0 =", sum(cov_family_history$fh_depr == 0),
      ", 1 =", sum(cov_family_history$fh_depr == 1),
      ", 2 =", sum(cov_family_history$fh_depr == 2),
      ", 99 (missing) =", sum(cov_family_history$fh_depr == 99), "\n")

  save_rds(cov_family_history, "cov_family_history.rds")
  cat("\nSaved cov_family_history.rds\n")

  cat("\n=== process_cov_family_history.R completed ===\n")
  invisible(cov_family_history)
}

if (sys.nframe() == 0) {
  process_cov_family_history()
}

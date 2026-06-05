# process_covariates_assembly.R
# Phase 2: Assemble all covariates into base_cov_28
#
# Inputs: base_28.rds, cov_family_history.rds, cov_education.rds, cov_income.rds,
#         cov_diagnoses.rds, cov_medications.rds, cov_hospitalizations.rds
# Outputs: base_cov_28.rds

library(dplyr)
library(here)
here::i_am("suicidality/extraction/22_process_covariates_assembly.R")

source(here("suicidality", "extraction", "lib", "common.R"))

process_covariates_assembly <- function(output_dir = rds_output_dir(), grace_days = 28L) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat(sprintf("=== process_covariates_assembly.R (grace_days = %d) ===\n\n", grace_days))

  # Load base data
  base_28 <- read_rds("base_28.rds")
  cat("base_28 rows:", nrow(base_28), "\n")

  # Load family covariates (time-invariant; shared across grace periods)
  cov_family_history <- read_rds("cov_family_history.rds")
  cov_education <- read_rds("cov_education.rds")
  cov_income <- read_rds("cov_income.rds")

  cat("cov_family_history rows:", nrow(cov_family_history), "\n")
  cat("cov_education rows:", nrow(cov_education), "\n")
  cat("cov_income rows:", nrow(cov_income), "\n")

  # Load individual covariates (fu_start-dependent; grace-period-specific)
  suffix <- if (grace_days == 28L) "" else sprintf("_%d", grace_days)
  cov_diagnoses <- read_rds(sprintf("cov_diagnoses%s.rds", suffix))
  cov_medications <- read_rds(sprintf("cov_medications%s.rds", suffix))
  cov_hospitalizations <- read_rds(sprintf("cov_hospitalizations%s.rds", suffix))

  cat("cov_diagnoses rows:", nrow(cov_diagnoses), "\n")
  cat("cov_medications rows:", nrow(cov_medications), "\n")
  cat("cov_hospitalizations rows:", nrow(cov_hospitalizations), "\n")

  # Assemble family covariates
  cat("\nAssembling family covariates...\n")

  cov_family <- base_28 %>%
    select(lopnr, lopnrmor, lopnrfar) %>%
    left_join(cov_family_history, by = "lopnr") %>%
    left_join(cov_education, by = "lopnr") %>%
    left_join(cov_income, by = "lopnr") %>%
    select(lopnr, lopnrmor, lopnrfar, fh_depr, fh_suicidal, edufam_cat, inc_cat)

  cat("cov_family rows:", nrow(cov_family), "\n")

  # Assemble individual covariates
  cat("Assembling individual covariates...\n")

  cov_individual <- cov_diagnoses %>%
    left_join(cov_medications, by = "lopnr") %>%
    left_join(cov_hospitalizations, by = "lopnr")

  cat("cov_individual rows:", nrow(cov_individual), "\n")

  # Final assembly
  cat("\nAssembling base_cov_28...\n")

  base_cov_28 <- base_28 %>%
    left_join(cov_family %>% select(-lopnrmor, -lopnrfar), by = "lopnr") %>%
    left_join(cov_individual, by = "lopnr")

  cat("After joining:", nrow(base_cov_28), "rows\n")
  cat("Final base_cov_28 rows:", nrow(base_cov_28), "\n")
  cat("Columns:", paste(names(base_cov_28), collapse = ", "), "\n")

  out_file <- if (grace_days == 28L) "base_cov_28.rds" else sprintf("base_cov_%d.rds", grace_days)
  save_rds(base_cov_28, out_file)
  cat("\nSaved", out_file, "\n")

  cat("\n=== process_covariates_assembly.R completed ===\n")
  invisible(base_cov_28)
}

if (sys.nframe() == 0) {
  process_covariates_assembly()
}

# process_cov_education.R
# Phase 2: Process LISA data to create family education covariate
#
# Education categories:
# - 0: Primary (SUN levels 1-2)
# - 1: Secondary (SUN levels 3-4)
# - 2: Tertiary (SUN levels 5-7)
# - 99: Missing
#
# Inputs: cohort_base.rds, raw_lisa.rds
# Output: cov_education.rds

library(dplyr)
library(here)
here::i_am("suicidality/extraction/17_process_cov_education.R")

source(here("suicidality", "extraction", "lib", "common.R"))

process_cov_education <- function(output_dir = rds_output_dir()) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat("=== process_cov_education.R ===\n\n")

  # Load data
  cohort_base <- read_rds("cohort_base.rds")
  raw_lisa <- read_rds("raw_lisa.rds")

  cat("cohort_base rows:", nrow(cohort_base), "\n")
  cat("raw_lisa rows:", nrow(raw_lisa), "\n")

  # Prepare cohort with year of diagnosis
  edu_infile <- cohort_base %>%
    select(lopnr, diagn_date, lopnrmor, lopnrfar) %>%
    mutate(year = as.integer(format(diagn_date, "%Y")))

  max_year <- max(edu_infile$year, na.rm = TRUE)

  # Process education levels
  # Use edu_old1 if available, otherwise edu_old2
  cat("\nProcessing education levels...\n")

  edu_data <- raw_lisa %>%
    mutate(
      edu = case_when(
        !is.na(edu_old1) & edu_old1 != " " & edu_old1 != "*" ~ edu_old1,
        !is.na(edu_old2) & edu_old2 != " " & edu_old2 != "*" ~ edu_old2,
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(edu) & edu != " " & edu != "*") %>%
    filter(is.na(year) | year <= max_year) %>%
    select(lopnr, year, edu)

  cat("Education records after filtering:", nrow(edu_data), "\n")

  # Function to get highest education for parent up to child's diagnosis year
  get_parent_edu <- function(child_data, parent_col, edu_data) {
    child_data %>%
      select(lopnr, year, parent_lopnr = !!sym(parent_col)) %>%
      filter(!is.na(parent_lopnr)) %>%
      left_join(edu_data, by = c("parent_lopnr" = "lopnr"), relationship = "many-to-many",
                suffix = c("_child", "_edu")) %>%
      filter(is.na(year_edu) | year_child >= year_edu) %>%
      arrange(lopnr, desc(edu)) %>%
      group_by(lopnr) %>%
      slice(1) %>%
      ungroup() %>%
      select(lopnr, edu)
  }

  # Get mother's education
  cat("Getting mother's education...\n")
  mother_edu <- get_parent_edu(edu_infile, "lopnrmor", edu_data) %>%
    rename(mother_edu = edu)

  # Get father's education
  cat("Getting father's education...\n")
  father_edu <- get_parent_edu(edu_infile, "lopnrfar", edu_data) %>%
    rename(father_edu = edu)

  # Combine and get highest family education
  cov_education <- edu_infile %>%
    select(lopnr) %>%
    left_join(mother_edu, by = "lopnr") %>%
    left_join(father_edu, by = "lopnr") %>%
    mutate(
      mother_edu = if_else(mother_edu == " " | mother_edu == "*", NA_character_, mother_edu),
      father_edu = if_else(father_edu == " " | father_edu == "*", NA_character_, father_edu),
      # Family education = highest of mother/father (convert to numeric for comparison)
      mother_edu_num = as.integer(mother_edu),
      father_edu_num = as.integer(father_edu),
      famedu = case_when(
        is.na(mother_edu) & is.na(father_edu) ~ NA_character_,
        is.na(mother_edu) ~ father_edu,
        is.na(father_edu) ~ mother_edu,
        mother_edu_num > father_edu_num ~ mother_edu,
        TRUE ~ father_edu
      ),
      # Categorize
      edufam_cat = case_when(
        famedu %in% c("1", "2") ~ 0L,      # Primary
        famedu %in% c("3", "4") ~ 1L,      # Secondary
        famedu %in% c("5", "6", "7") ~ 2L, # Tertiary
        is.na(famedu) ~ 99L                # Missing
      )
    ) %>%
    select(lopnr, edufam_cat)

  cat("\nEducation category distribution:\n")
  cov_education %>%
    count(edufam_cat) %>%
    print()

  save_rds(cov_education, "cov_education.rds")
  cat("\nSaved cov_education.rds\n")

  cat("\n=== process_cov_education.R completed ===\n")
  invisible(cov_education)
}

if (sys.nframe() == 0) {
  process_cov_education()
}

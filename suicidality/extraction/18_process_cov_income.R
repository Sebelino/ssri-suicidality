# process_cov_income.R
# Phase 2: Process LISA data to create family income covariate
#
# Income categories (based on mother's family income):
# - 1: <0
# - 2: 0
# - 3: 0-p20
# - 4: p20-p80
# - 5: >p80
# - NOINFO: Missing
#
# Inputs: cohort_base.rds, raw_lisa.rds
# Output: cov_income.rds

library(dplyr)
library(here)
here::i_am("suicidality/extraction/18_process_cov_income.R")

source(here("suicidality", "extraction", "lib", "common.R"))

process_cov_income <- function(output_dir = rds_output_dir()) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat("=== process_cov_income.R ===\n\n")

  # Load data
  cohort_base <- read_rds("cohort_base.rds")
  raw_lisa <- read_rds("raw_lisa.rds")

  cat("cohort_base rows:", nrow(cohort_base), "\n")
  cat("raw_lisa rows:", nrow(raw_lisa), "\n")

  # Compute income percentiles by year
  cat("\nComputing income percentiles by year...\n")

  inc_pctls <- raw_lisa %>%
    filter(!is.na(income)) %>%
    group_by(year) %>%
    summarise(
      p20 = quantile(income, 0.20, na.rm = TRUE, type = 2),
      p80 = quantile(income, 0.80, na.rm = TRUE, type = 2),
      .groups = "drop"
    )

  cat("Percentiles computed for", nrow(inc_pctls), "years\n")

  # Prepare cohort with year of diagnosis
  infile <- cohort_base %>%
    select(lopnr, diagn_date, lopnrmor) %>%
    mutate(year = as.integer(format(diagn_date, "%Y")))

  max_year <- max(infile$year, na.rm = TRUE)

  # Get mother's income
  cat("Getting mother's income...\n")

  mother_income <- raw_lisa %>%
    filter(!is.na(income)) %>%
    filter(year <= max_year) %>%
    select(lopnr, inc_year = year, inc = income)

  cat("Mother income records:", nrow(mother_income), "\n")

  # Join with cohort (using mother's lopnr)
  iuc_inc <- infile %>%
    filter(!is.na(lopnrmor)) %>%
    inner_join(mother_income, by = c("lopnrmor" = "lopnr"), relationship = "many-to-many") %>%
    filter(inc_year <= year)

  # Take income closest to diagnosis date
  iuc_inc <- iuc_inc %>%
    arrange(lopnr, desc(inc_year)) %>%
    group_by(lopnr) %>%
    slice(1) %>%
    ungroup()

  cat("After selecting closest year:", nrow(iuc_inc), "\n")

  # Join with percentiles and categorize
  inc_cohort_28 <- iuc_inc %>%
    left_join(inc_pctls, by = c("inc_year" = "year")) %>%
    mutate(
      inc_cat = case_when(
        inc < 0 ~ "<0",
        inc == 0 ~ "0",
        inc > 0 & inc <= p20 ~ "0-p20",
        inc > p20 & inc <= p80 ~ "p20-p80",
        inc > p80 ~ ">p80",
        TRUE ~ NA_character_
      )
    ) %>%
    select(lopnr, inc, inc_cat)

  # Convert to numeric codes
  cov_income <- inc_cohort_28 %>%
    mutate(
      inc_cat = case_when(
        inc_cat == "<0" ~ 1L,
        inc_cat == "0" ~ 2L,
        inc_cat == "0-p20" ~ 3L,
        inc_cat == "p20-p80" ~ 4L,
        inc_cat == ">p80" ~ 5L,
        TRUE ~ 99L
      )
    ) %>%
    select(lopnr, inc_cat)

  # Ensure all lopnrs are included
  all_lopnrs <- unique(cohort_base$lopnr)
  cov_income <- data.frame(lopnr = all_lopnrs) %>%
    left_join(cov_income, by = "lopnr") %>%
    mutate(inc_cat = if_else(is.na(inc_cat), 99L, inc_cat))

  cat("\nIncome category distribution:\n")
  cov_income %>%
    count(inc_cat) %>%
    print()

  save_rds(cov_income, "cov_income.rds")
  cat("\nSaved cov_income.rds\n")

  cat("\n=== process_cov_income.R completed ===\n")
  invisible(cov_income)
}

if (sys.nframe() == 0) {
  process_cov_income()
}

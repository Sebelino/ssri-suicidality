# process_final_cohorts.R
# Phase 2: Create final analysis cohorts with all variables
#
# Inputs: main_12wks_28_tmp.rds, main_52wks_28_tmp.rds,
#         pp_12wks_max_tmp.rds, pp_52wks_max_tmp.rds,
#         base_cov_28.rds, raw_individual.rds, raw_diagnoses_index.rds
# Outputs: main_12wks_28.rds, main_52wks_28.rds,
#          pp_12wks_max.rds, pp_52wks_max.rds

library(dplyr)
library(here)
here::i_am("suicidality/extraction/24_process_final_cohorts.R")

source(here("suicidality", "extraction", "lib", "common.R"))

process_final_cohorts <- function(output_dir = rds_output_dir(), grace_days = 28L) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat(sprintf("=== process_final_cohorts.R (grace_days = %d) ===\n\n", grace_days))

  # Load shared data (covariate assembly uses base_cov_28 name for default,
  # and base_cov_<grace> for non-default grace periods).
  base_cov_file <- if (grace_days == 28L) "base_cov_28.rds" else sprintf("base_cov_%d.rds", grace_days)
  base_cov_28 <- read_rds(base_cov_file)
  raw_individual <- read_rds("raw_individual.rds")
  raw_diagnoses_index <- read_rds("raw_diagnoses_index.rds")

  cat(base_cov_file, "rows:", nrow(base_cov_28), "\n")

  # Pre-compute shared lookups
  # `atc` is already on main_tmp (the temp main cohort), so dropping it from
  # cov_cols avoids a join-time collision (atc.x/atc.y) and lets main_tmp's
  # `atc` flow through to the final main RDS unchanged. PP doesn't carry atc.
  cov_cols <- base_cov_28 %>%
    select(-lopnr, -diagn_date, -dia, -bdate, -atc, -prescr, -age, -agecat,
           -date_death, -date_emig, -lopnrmor, -lopnrfar)

  pp_cov_cols <- cov_cols %>%
    select(-any_of(c("med_antipsychotic", "med_benzodiazepine", "opsych", "exp")))

  sex_map <- raw_individual %>%
    select(lopnr, sex) %>%
    mutate(female = if_else(sex == 2, 1L, 0L)) %>%
    select(lopnr, female)

  first_diagn_source <- raw_diagnoses_index %>%
    group_by(lopnr) %>%
    arrange(diagn_date) %>%
    slice(1) %>%
    ungroup() %>%
    select(lopnr, source)

  int_to_num <- function(df) {
    df %>% mutate(across(where(is.integer), as.numeric))
  }

  # Helper: finalize one pair of main + pp datasets. PP datasets only exist
  # for the default 28-day grace cohort (no time-varying analysis is run
  # against the 14-day sensitivity cohort).
  build_pp <- (grace_days == 28L)
  finalize_cohort <- function(followup_weeks) {
    main_file <- sprintf("main_%dwks_%d_tmp.rds", followup_weeks, grace_days)
    cat(sprintf("\n--- Finalizing %d-week cohorts (grace=%d) ---\n", followup_weeks, grace_days))

    main_tmp <- read_rds(main_file)
    cat(main_file, "rows:", nrow(main_tmp), "\n")

    pp_tmp <- if (build_pp) {
      pp_file <- sprintf("pp_%dwks_max_tmp.rds", followup_weeks)
      cat(pp_file, "rows: ", appendLF = FALSE)
      out <- read_rds(pp_file)
      cat(nrow(out), "\n")
      out
    } else NULL

    # Join covariates
    main_out <- main_tmp %>%
      left_join(base_cov_28 %>% select(lopnr, names(cov_cols)), by = "lopnr")

    if (build_pp) {
      pp_out <- pp_tmp %>%
        left_join(base_cov_28 %>% select(lopnr, names(pp_cov_cols)), by = "lopnr")
    }

    # Add year
    main_out <- main_out %>%
      mutate(year = as.integer(format(diagn_date, "%Y")))

    if (build_pp) {
      pp_out <- pp_out %>%
        left_join(main_out %>% select(lopnr, year) %>% distinct(), by = "lopnr")
    }

    # Add sex
    main_out <- main_out %>% left_join(sex_map, by = "lopnr")
    if (build_pp) pp_out <- pp_out %>% left_join(sex_map, by = "lopnr")

    # Add source
    main_out <- main_out %>%
      left_join(first_diagn_source, by = "lopnr", suffix = c("", "_diagn"))

    if ("source" %in% names(main_out) && "source_diagn" %in% names(main_out)) {
      main_out <- main_out %>%
        mutate(source = coalesce(source, source_diagn)) %>%
        select(-source_diagn)
    } else if ("source_diagn" %in% names(main_out)) {
      main_out <- main_out %>% rename(source = source_diagn)
    }

    if (build_pp) pp_out <- pp_out %>% left_join(first_diagn_source, by = "lopnr")

    cat("Source distribution:\n")
    print(table(main_out$source, useNA = "ifany"))

    # Ensure inc_cat is integer
    main_out <- main_out %>% mutate(inc_cat = as.integer(inc_cat))
    if (build_pp) pp_out <- pp_out %>% mutate(inc_cat = as.integer(inc_cat))

    if (build_pp) {
      pp_out <- pp_out %>%
        left_join(main_out %>% select(lopnr, age, diagn_date, cens_switch) %>% distinct(), by = "lopnr") %>%
        mutate(anypsych_tv = if_else(med_antipsychotic == 1 | med_benzodiazepine == 1 | opsych == 1, 1L, 0L))
    }

    # Convert to numeric
    main_out <- int_to_num(main_out)
    if (build_pp) pp_out <- int_to_num(pp_out) else pp_out <- NULL

    # Save
    main_out_name <- sprintf("main_%dwks_%d.rds", followup_weeks, grace_days)
    save_rds(main_out, main_out_name)
    cat("Saved", main_out_name, ":", nrow(main_out), "rows\n")

    # PP cohort is built only for the default 28-day grace period.
    if (grace_days == 28L) {
      pp_out_name <- sprintf("pp_%dwks_max.rds", followup_weeks)
      save_rds(pp_out, pp_out_name)
      cat("Saved", pp_out_name, ":", nrow(pp_out), "rows\n")
    }

    list(main = main_out, pp = pp_out)
  }

  results_12 <- finalize_cohort(12)
  results_52 <- if (grace_days == 28L) finalize_cohort(52) else NULL

  cat("\n=== process_final_cohorts.R completed ===\n")
  invisible(list(
    main_12wks = results_12$main, pp_12wks_max = results_12$pp,
    main_52wks = if (!is.null(results_52)) results_52$main else NULL,
    pp_52wks_max = if (!is.null(results_52)) results_52$pp else NULL
  ))
}

if (sys.nframe() == 0) {
  process_final_cohorts()
}

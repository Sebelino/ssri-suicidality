# lib/extract_main_followup.R
# Shared logic for extracting main follow-up cohort (12-week)
# Aligned with Lagerberg et al. 2023 methodology (28-day grace period, no psychosis censoring)

library(dplyr)

# Consistent random sampling function using R's RNG with a fixed seed
sample_control_fu_start <- function(cases, controls, seed = 42) {
  ratio <- nrow(controls) / nrow(cases)

  # Multiply cases by ceiling of ratio to have enough samples
  cases2 <- cases %>%
    slice(rep(1:n(), each = ceiling(ratio)))

  # Use consistent seeding
  set.seed(seed)
  ranlop <- cases2 %>%
    mutate(ran = runif(n())) %>%
    arrange(ran)

  control_predi_diff <- ranlop$predi_diff[1:nrow(controls)]

  controls %>%
    select(-predi_diff) %>%
    mutate(predi_diff = control_predi_diff) %>%
    mutate(fu_start = diagn_date + predi_diff) %>%
    select(-predi_diff)
}

#' Extract main follow-up cohort
#'
#' @param followup_weeks Number of weeks for follow-up (12 or 52)
#' @param output_dir Directory containing input RDS files and where output is saved
#' @param random_seed Seed for random number generation (default: 42)
#' @param grace_days Number of days for the SSRI-initiation grace period
#'   (default 28, per Lagerberg 2023). The 14-day variant rebuilds the cohort
#'   with predi_diff <= 14 as initiators and frequency-matches non-initiator
#'   fu_start to the 14-day initiator distribution. The death/emigration
#'   eligibility filter then spans 14 instead of 28 days.
#' @return The extracted cohort (invisibly)
extract_main_followup <- function(followup_weeks, output_dir = "output/rds", random_seed = 42, grace_days = 28L) {
  stopifnot(followup_weeks %in% c(12, 52))
  stopifnot(is.numeric(grace_days), length(grace_days) == 1, grace_days > 0)
  grace_days <- as.integer(grace_days)

  followup_days <- followup_weeks * 7
  outcome_col <- paste0("sb", followup_weeks)
  outcome_pp_col <- paste0("sb", followup_weeks, "_pp")
  outcome_itt_col <- paste0("sb", followup_weeks, "_itt")
  fu_end_col <- paste0("fu_end", followup_weeks)
  output_file <- sprintf("main_%dwks_%d_tmp.rds", followup_weeks, grace_days)

  cat(sprintf("Extracting %s ...\n", output_file))

  # Load required datasets
  base_28 <- readRDS(file.path(output_dir, "base_28.rds"))
  dia_all_28 <- readRDS(file.path(output_dir, "dia_all_28.rds"))

  cat("base_28 rows:", nrow(base_28), "\n")

  # Step 1: Assign CC status (initiator/non-initiator based on prescription timing).
  # Lagerberg 2023: 28-day grace period (default); 14-day variant is a sensitivity.
  main1 <- base_28 %>%
    mutate(
      predi_diff = as.integer(prescr - diagn_date),
      cc = case_when(
        !is.na(predi_diff) & predi_diff <= grace_days ~ 1L,  # Within grace_days = initiator
        !is.na(predi_diff) & predi_diff > grace_days ~ 0L,   # After grace_days = non-initiator
        is.na(predi_diff) ~ 0L                                # No prescription = non-initiator
      )
    )

  cat("CC=1:", sum(main1$cc == 1), "CC=0:", sum(main1$cc == 0), "\n")

  # Step 2: Add switch_dat and switch flags
  main2 <- main1 %>%
    mutate(
      switch_dat = if_else(cc == 0 & !is.na(prescr), prescr, as.Date(NA)),
      switch = if_else(cc == 0 & !is.na(prescr), 1L, 0L)
    )

  # Step 3: Split into cases and controls
  cases <- main2 %>% filter(cc == 1) %>% mutate(fu_start = prescr)
  controls <- main2 %>% filter(cc == 0)

  cat("Cases:", nrow(cases), "Controls:", nrow(controls), "\n")

  # Step 4: Assign fu_start to controls using random sampling (Zhou et al. 2005)
  cat("Assigning fu_start to controls using random sampling (seed=", random_seed, ")...\n", sep = "")
  control_date2 <- sample_control_fu_start(cases, controls, seed = random_seed)

  # Step 5: Join fu_start back with main2
  main3 <- main2 %>%
    left_join(
      cases %>% select(lopnr, fu_start) %>% rename(tmpcc1 = fu_start),
      by = "lopnr"
    ) %>%
    left_join(
      control_date2 %>% select(lopnr, fu_start) %>% rename(tmpcc0 = fu_start),
      by = "lopnr"
    )

  # Step 6: Create main4 with fu_start and follow-up end dates
  main4 <- main3 %>%
    mutate(
      fu_start = if_else(cc == 1, tmpcc1, tmpcc0),
      fu_end12 = fu_start + (12*7),
      fu_end52 = fu_start + (52*7)
    ) %>%
    select(-tmpcc1, -tmpcc0)

  cat("main4 rows:", nrow(main4), "\n")

  # Step 7: Join with outcomes - filter by the CORRECT follow-up window
  fu_end_for_outcomes <- main4[[fu_end_col]]

  iucc_outcomes <- main4 %>%
    select(lopnr, fu_start) %>%
    mutate(fu_end_outcome = fu_end_for_outcomes) %>%
    left_join(dia_all_28 %>% select(lopnr, date_fail), by = "lopnr", relationship = "many-to-many") %>%
    filter(fu_start < date_fail & date_fail <= fu_end_outcome) %>%  # Exclude day 0 events (strict inequality)
    arrange(lopnr, date_fail) %>%
    group_by(lopnr) %>%
    slice(1) %>%
    ungroup() %>%
    select(lopnr, date_fail)

  main5 <- main4 %>%
    left_join(iucc_outcomes, by = "lopnr") %>%
    mutate(admin_end = as.Date("2020-12-31"))

  cat("main5 rows:", nrow(main5), "\n")

  # Step 8: Create outcomes and censoring flags
  # Lagerberg 2023: Censor on death, emigration, outcome, admin end (NO psychosis censoring)
  main_wks <- main5 %>%
    mutate(fu_end_val = .data[[fu_end_col]]) %>%
    mutate(
      # Outcome within follow-up period (exclude day 0 events)
      !!outcome_col := if_else(fu_start < date_fail & date_fail <= fu_end_val, 1L, 0L, missing = 0L),

      # Censoring flags (Lagerberg 2023: death, emigration, admin end only)
      cens_death = if_else(fu_start <= date_death & date_death <= fu_end_val, 1L, 0L, missing = 0L),
      cens_emig = if_else(fu_start <= date_emig & date_emig <= fu_end_val, 1L, 0L, missing = 0L),
      cens_switch = if_else(fu_start <= switch_dat & switch_dat <= fu_end_val, 1L, 0L, missing = 0L),
      cens_admin = if_else(fu_start <= admin_end & admin_end <= fu_end_val, 1L, 0L, missing = 0L),

      cens_deathemig = if_else(cens_death == 1 | cens_emig == 1, 1L, 0L)
    )

  # Step 9: Calculate per-protocol and ITT follow-up end dates
  # Lagerberg 2023: No psychosis censoring
  main_wks <- main_wks %>%
    rowwise() %>%
    mutate(
      fu_end_pp = min(fu_end_val, date_death, date_emig, switch_dat, admin_end, date_fail, na.rm = TRUE),
      fu_end_itt = min(fu_end_val, date_death, date_emig, admin_end, date_fail, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      fu_end_pp = if_else(is.infinite(fu_end_pp), fu_end_val, fu_end_pp),
      fu_end_itt = if_else(is.infinite(fu_end_itt), fu_end_val, fu_end_itt),

      !!outcome_pp_col := if_else(fu_start < date_fail & date_fail <= fu_end_pp, 1L, 0L, missing = 0L),
      !!outcome_itt_col := if_else(fu_start < date_fail & date_fail <= fu_end_itt, 1L, 0L, missing = 0L)
    ) %>%
    mutate(
      # Adjust censoring if event occurs on same day
      cens_switch = if_else(switch_dat == date_fail & !is.na(switch_dat) & .data[[outcome_pp_col]] == 1, 0L, cens_switch),
      cens_emig = if_else(date_emig == date_fail & !is.na(date_emig) & .data[[outcome_itt_col]] == 1, 0L, cens_emig),
      cens_admin = if_else(admin_end == date_fail & !is.na(admin_end) & .data[[outcome_itt_col]] == 1, 0L, cens_admin),

      cens_deathemig = if_else(cens_death == 1 | cens_emig == 1 | cens_admin == 1, 1L, 0L)
    )

  # Step 10: Exclude individuals who die/emigrate before fu_start
  main_wks_28_tmp <- main_wks %>%
    filter(!((!is.na(date_death) & date_death <= fu_start) |
               (!is.na(date_emig) & date_emig <= fu_start)))

  cat("main_", followup_weeks, "wks_28_tmp rows:", nrow(main_wks_28_tmp), "\n", sep = "")
  cat("Unique lopnr:", length(unique(main_wks_28_tmp$lopnr)), "\n")

  # Reorder columns to match expected output
  main_wks_28_tmp <- main_wks_28_tmp %>%
    select(lopnr, diagn_date, dia, bdate, atc, prescr, age, agecat, date_death, date_emig,
           predi_diff, cc, switch_dat, switch, fu_start, fu_end12, fu_end52, date_fail,
           admin_end, all_of(outcome_col), cens_death, cens_emig, cens_switch,
           cens_admin, cens_deathemig, fu_end_pp, fu_end_itt,
           all_of(outcome_pp_col), all_of(outcome_itt_col))

  # Save to RDS
  saveRDS(main_wks_28_tmp, file.path(output_dir, output_file))
  cat("Saved to", file.path(output_dir, output_file), "\n")

  cat("process_followup (", followup_weeks, " weeks) completed successfully.\n", sep = "")

  invisible(main_wks_28_tmp)
}

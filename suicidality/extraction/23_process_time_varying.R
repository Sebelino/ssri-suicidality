# process_time_varying.R
# Phase 2: Process time-varying medication data and create per-protocol cohorts
#
# Creates time-varying medication episodes and per-protocol cohorts
#
# Inputs: main_12wks_28_tmp.rds, main_52wks_28_tmp.rds, othermeds_28.rds, raw_prescriptions_cohort.rds
# Outputs: pp_12wks_max_tmp.rds, pp_52wks_max_tmp.rds

library(dplyr)
library(tidyr)
library(data.table)
library(here)
here::i_am("suicidality/extraction/23_process_time_varying.R")

source(here("suicidality", "extraction", "lib", "common.R"))
source(here("suicidality", "extraction", "lib", "Macros.R"))

process_time_varying <- function(output_dir = rds_output_dir()) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat("=== process_time_varying.R ===\n\n")

  # Load data
  main_tmp <- read_rds("main_12wks_28_tmp.rds")
  othermeds_28 <- read_rds("othermeds_28.rds")
  raw_prescriptions <- read_rds("raw_prescriptions_cohort.rds")

  cat("main_12wks_28_tmp rows:", nrow(main_tmp), "\n")
  cat("othermeds_28 rows:", nrow(othermeds_28), "\n")
  cat("raw_prescriptions_cohort rows:", nrow(raw_prescriptions), "\n")

  cohort_lopnrs <- unique(main_tmp$lopnr)

  # Step 1: Create meds_tv
  cat("\nStep 1: Creating meds_tv...\n")

  # Get N06A (SSRI) prescriptions for cohort
  n06a_prescriptions <- raw_prescriptions %>%
    filter(lopnr %in% cohort_lopnrs) %>%
    filter(substr(atc, 1, 4) == "N06A") %>%
    select(lopnr, atc, edatum)

  cat("N06A prescriptions for cohort:", nrow(n06a_prescriptions), "\n")

  # Combine othermeds_28 with N06A prescriptions
  meds1 <- othermeds_28 %>%
    rename(edatum = otherprescr)

  meds2 <- n06a_prescriptions

  meds_tv <- bind_rows(meds1, meds2) %>%
    arrange(lopnr, edatum) %>%
    mutate(date = edatum)

  cat("meds_tv rows:", nrow(meds_tv), "\n")


  # Step 2: Classify medications and create episodes
  cat("\nStep 2: Classifying medications and creating episodes...\n")

  # Mood stabilizer ATC codes (subset of N03A used as mood stabilizers, matches baseline definition)
  mood_stabilizer_codes <- c("N03AG01", "N03AX09", "N03AF01")

  meds_tv <- meds_tv %>%
    mutate(
      exp = if_else(substr(atc, 1, 5) == "N06AB", 1L, 0L),  # SSRIs
      med_antipsychotic = if_else(substr(atc, 1, 4) == "N05A" & !grepl("^N05AN", atc), 1L, 0L),  # Antipsychotics excl lithium
      med_benzodiazepine = if_else(substr(atc, 1, 5) == "N05BA", 1L, 0L),  # Benzodiazepines
      opsych = if_else(
        substr(atc, 1, 4) %in% c("N02A", "N05C", "N06B", "N07B") |
        (substr(atc, 1, 4) == "N03A" & !(atc %in% mood_stabilizer_codes)) |  # Antiepileptics excl mood stabilizers (matches baseline)
        substr(atc, 1, 5) == "N05AN" |  # Lithium (mood stabilizer)
        (substr(atc, 1, 4) == "N06A" & substr(atc, 1, 5) != "N06AB"),  # Other antidepressants
        1L, 0L
      )
    )

  cat("exp=1:", sum(meds_tv$exp == 1), "\n")
  cat("med_antipsychotic=1:", sum(meds_tv$med_antipsychotic == 1), "\n")
  cat("med_benzodiazepine=1:", sum(meds_tv$med_benzodiazepine == 1), "\n")
  cat("opsych=1:", sum(meds_tv$opsych == 1), "\n")

  # Create medication episodes using othertreat function
  create_episodes <- function(meds, flag_col, medname, addnumber = 0L, gap_days = 120L) {
    meds_filtered <- meds %>%
      filter(.data[[flag_col]] == 1) %>%
      select(lopnr, date)

    if (nrow(meds_filtered) == 0) {
      return(data.frame(lopnr = integer(), start = as.Date(character()), stop = as.Date(character())))
    }

    # Use the othertreat function from Macros.R
    # Returns columns: lopnr, medname (e.g. "exp"), start, end
    result <- othertreat(meds_filtered, medname = medname, addnumber = addnumber, gap_days = gap_days)

    # Filter: end >= start
    result <- result[result$end >= result$start, ]

    # Rename end -> stop for compatibility
    names(result)[names(result) == "end"] <- "stop"

    result
  }

  cat("\n--- Creating exp episodes ---\n")
  expmed <- create_episodes(meds_tv, "exp", "exp", addnumber = 40L, gap_days = 120L)
  cat("Episodes:", nrow(expmed), "\n")

  cat("\n--- Creating med_antipsychotic episodes ---\n")
  other_antipsychotic <- create_episodes(meds_tv, "med_antipsychotic", "med_antipsychotic", addnumber = 21L, gap_days = 120L)
  cat("Episodes:", nrow(other_antipsychotic), "\n")

  cat("\n--- Creating med_benzodiazepine episodes ---\n")
  other_benzodiazepine <- create_episodes(meds_tv, "med_benzodiazepine", "med_benzodiazepine", addnumber = 17L, gap_days = 120L)
  cat("Episodes:", nrow(other_benzodiazepine), "\n")

  cat("\n--- Creating opsych episodes ---\n")
  opsych <- create_episodes(meds_tv, "opsych", "opsych", addnumber = 17L, gap_days = 120L)
  cat("Episodes:", nrow(opsych), "\n")

  # Shared helper: add medication exposure to weekly periods
  add_exposure <- function(periods, episodes, col_name) {
    if (nrow(episodes) == 0) {
      periods[[col_name]] <- 0L
      return(periods)
    }

    # Join periods with episodes
    periods_dt <- as.data.table(periods)
    episodes_dt <- as.data.table(episodes)

    setkey(periods_dt, lopnr)
    setkey(episodes_dt, lopnr)

    # Check for overlap
    merged <- periods_dt[episodes_dt, on = "lopnr", allow.cartesian = TRUE, nomatch = 0]
    merged <- merged[week_start <= stop & week_end >= start]
    merged <- merged[, .(lopnr, week_start, exposed = 1L)]
    merged <- unique(merged)

    # Join back
    periods_dt <- merge(periods_dt, merged, by = c("lopnr", "week_start"), all.x = TRUE)
    periods_dt[[col_name]] <- ifelse(is.na(periods_dt$exposed), 0L, 1L)
    periods_dt$exposed <- NULL

    as.data.frame(periods_dt)
  }

  # Shared helper: create per-protocol cohort for a given follow-up duration
  create_pp_cohort <- function(main_data, followup_weeks) {
    sb_col <- paste0("sb", followup_weeks, "_pp")

    cat(sprintf("\n=== Creating pp_%dwks_max_tmp ===\n", followup_weeks))

    sb_pp <- main_data %>%
      select(lopnr, cc, fu_start, fu_end_pp, !!sym(sb_col))

    cat("Initial cohort rows:", nrow(sb_pp), "\n")

    cat("Splitting into weekly periods...\n")

    weekly_periods <- sb_pp %>%
      rowwise() %>%
      mutate(
        weeks = list(seq(0, max(0L, as.integer(fu_end_pp - fu_start - 1) %/% 7), by = 1))
      ) %>%
      unnest(weeks) %>%
      ungroup() %>%
      mutate(
        week_start = fu_start + weeks * 7,
        week_end = pmin(week_start + 6, fu_end_pp)
      ) %>%
      filter(week_start <= fu_end_pp)

    cat("Weekly periods:", nrow(weekly_periods), "\n")

    cat("Adding medication exposure...\n")

    weekly_periods <- add_exposure(weekly_periods, expmed, "exp")

    # Per-protocol: initiators must be on treatment
    weekly_periods <- weekly_periods %>%
      filter(!(cc == 1 & exp == 0))

    cat("After removing cc=1 & exp=0:", nrow(weekly_periods), "\n")

    weekly_periods <- add_exposure(weekly_periods, other_antipsychotic, "med_antipsychotic")
    weekly_periods <- add_exposure(weekly_periods, other_benzodiazepine, "med_benzodiazepine")
    weekly_periods <- add_exposure(weekly_periods, opsych, "opsych")

    cat("After all exposure additions:", nrow(weekly_periods), "\n")

    cat("Aggregating by lopnr and week...\n")

    pp_tmp <- weekly_periods %>%
      group_by(lopnr, weeks) %>%
      summarise(
        cc = max(cc, na.rm = TRUE),
        fu_start = min(fu_start, na.rm = TRUE),
        fu_end_pp = max(fu_end_pp, na.rm = TRUE),
        !!sb_col := max(.data[[sb_col]], na.rm = TRUE),
        week_start = min(week_start, na.rm = TRUE),
        week_end = max(week_end, na.rm = TRUE),
        exp = max(exp, na.rm = TRUE),
        med_antipsychotic = max(med_antipsychotic, na.rm = TRUE),
        med_benzodiazepine = max(med_benzodiazepine, na.rm = TRUE),
        opsych = max(opsych, na.rm = TRUE),
        .groups = "drop"
      )

    cat("Final rows:", nrow(pp_tmp), "\n")

    out_name <- sprintf("pp_%dwks_max_tmp.rds", followup_weeks)
    save_rds(pp_tmp, out_name)
    cat("Saved", out_name, "\n")

    pp_tmp
  }

  # Step 3: Create per-protocol cohorts
  create_pp_cohort(main_tmp, 12)

  main_52_tmp <- read_rds("main_52wks_28_tmp.rds")
  create_pp_cohort(main_52_tmp, 52)

  cat("\n=== process_time_varying.R completed ===\n")
}

if (sys.nframe() == 0) {
  process_time_varying()
}

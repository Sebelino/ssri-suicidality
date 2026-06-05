# process_cov_diagnoses.R
# Phase 2: Process diagnoses to create diagnosis covariates (Table S3)
#
# Creates indicators for prior psychiatric diagnoses before follow-up start
# Using Table S3 categories from Lagerberg et al. 2023
#
# Inputs: main_12wks_28_tmp.rds, raw_diagnoses_cohort.rds
# Output: cov_diagnoses.rds

library(dplyr)
library(here)
here::i_am("suicidality/extraction/19_process_cov_diagnoses.R")

source(here("suicidality", "extraction", "lib", "common.R"))

process_cov_diagnoses <- function(output_dir = rds_output_dir(), grace_days = 28L) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat(sprintf("=== process_cov_diagnoses.R (grace_days = %d) ===\n\n", grace_days))

  # Load data
  main_file <- sprintf("main_12wks_%d_tmp.rds", grace_days)
  main_tmp <- read_rds(main_file)
  raw_diagnoses_cohort <- read_rds("raw_diagnoses_cohort.rds")

  cat(main_file, "rows:", nrow(main_tmp), "\n")
  cat("raw_diagnoses_cohort rows:", nrow(raw_diagnoses_cohort), "\n")

  # Get fu_start dates for filtering
  fu_start_data <- main_tmp %>%
    select(lopnr, fu_start)

  # Filter diagnoses to before fu_start
  max_date <- max(fu_start_data$fu_start, na.rm = TRUE)

  # Drop chapter-range placeholder codes (e.g. "F00-F09", "F40-F48",
  # "F00-F999") that the Swedish NPR sometimes records when no specific
  # ICD-10 code is given. Their 3-char prefix collides with real codes
  # (e.g. "F40-F48" -> dia3 = "F40") and would inflate diag_organic by
  # several thousand patients with no actual organic mental disorder.
  diagn_filtered <- raw_diagnoses_cohort %>%
    filter(diagn_date <= max_date) %>%
    inner_join(fu_start_data, by = "lopnr") %>%
    filter(diagn_date <= fu_start) %>%  # Include same-day diagnoses (precede SSRI dispensation)
    filter(!grepl("-", dia)) %>%
    select(lopnr, dia)

  cat("Diagnoses before fu_start:", nrow(diagn_filtered), "\n")

  # Create Table S3 diagnosis indicators
  cat("\nCreating Table S3 diagnosis indicators...\n")

  cov_diagnoses <- fu_start_data %>%
    left_join(
      diagn_filtered %>%
        mutate(
          dia3 = substr(dia, 1, 3),
          dia4 = substr(dia, 1, 4),

          # Table S3 diagnoses (Lagerberg et al. 2023)

          # 1. Organic mental disorder (F00-F09)
          diag_organic = if_else(dia3 >= "F00" & dia3 <= "F09", 1L, 0L),

          # 2a. Alcohol use disorder (F10)
          diag_alcohol = if_else(dia3 == "F10", 1L, 0L),

          # 2b. Substance use disorder excluding alcohol and tobacco (F11-F19 excl F17)
          diag_sud = if_else(dia3 >= "F11" & dia3 <= "F19" & dia3 != "F17", 1L, 0L),

          # 3. Schizophrenia and psychotic disorder (F20-F29)
          diag_psychotic = if_else(dia3 >= "F20" & dia3 <= "F29", 1L, 0L),

          # 4. Bipolar/manic disorders (F30-F31)
          diag_bipolar = if_else(dia3 %in% c("F30", "F31"), 1L, 0L),

          # 5. Major depressive disorder (F32, F33)
          diag_mdd = if_else(dia3 %in% c("F32", "F33"), 1L, 0L),

          # 6. Phobic anxiety disorder (F40.0-F40.2)
          diag_phobic = if_else(dia4 %in% c("F400", "F401", "F402"), 1L, 0L),

          # 7. Other anxiety disorders - panic/generalized (F41.0-F41.1)
          diag_anxiety_other = if_else(dia4 %in% c("F410", "F411"), 1L, 0L),

          # 8. OCD (F42)
          diag_ocd = if_else(dia3 == "F42", 1L, 0L),

          # 9. Reaction to severe stress and adjustment disorders (F43)
          diag_stress = if_else(dia3 == "F43", 1L, 0L),

          # 10. Anorexia nervosa (F50.0-F50.1)
          diag_anorexia = if_else(dia4 %in% c("F500", "F501"), 1L, 0L),

          # 11. Bulimia nervosa (F50.2-F50.3)
          diag_bulimia = if_else(dia4 %in% c("F502", "F503"), 1L, 0L),

          # 12. Non-organic sleep disorders (F51)
          diag_sleep = if_else(dia3 == "F51", 1L, 0L),

          # 13. Cluster B personality disorder - dissocial/emotionally unstable (F60.2-F60.3)
          diag_personality_cluster_b = if_else(dia4 %in% c("F602", "F603"), 1L, 0L),

          # 14. Intellectual disability (F70-F79)
          diag_intellectual_disability = if_else(dia3 >= "F70" & dia3 <= "F79", 1L, 0L),

          # 15. Autism spectrum disorder (F84.0, F84.1, F84.5, F84.8, F84.9)
          diag_autism = if_else(dia4 %in% c("F840", "F841", "F845", "F848", "F849"), 1L, 0L),

          # 16. Hyperkinetic disorder / ADHD (F90)
          diag_adhd = if_else(dia3 == "F90", 1L, 0L),

          # 17. Conduct disorders (F91)
          diag_conduct = if_else(dia3 == "F91", 1L, 0L),

          # 18. Overdose/poisoning (T36-T51, X40-X49)
          diag_overdose = if_else(
            (dia3 >= "T36" & dia3 <= "T51") | (dia3 >= "X40" & dia3 <= "X49"),
            1L, 0L
          ),

          # 19. Suicidal behaviour (X60-X84, Y10-Y34)
          diag_suicidal = if_else(
            (dia3 >= "X60" & dia3 <= "X84") | (dia3 >= "Y10" & dia3 <= "Y34"),
            1L, 0L
          )
        ) %>%
        group_by(lopnr) %>%
        summarise(
          diag_organic = max(diag_organic, na.rm = TRUE),
          diag_alcohol = max(diag_alcohol, na.rm = TRUE),
          diag_sud = max(diag_sud, na.rm = TRUE),
          diag_psychotic = max(diag_psychotic, na.rm = TRUE),
          diag_bipolar = max(diag_bipolar, na.rm = TRUE),
          diag_mdd = max(diag_mdd, na.rm = TRUE),
          diag_phobic = max(diag_phobic, na.rm = TRUE),
          diag_anxiety_other = max(diag_anxiety_other, na.rm = TRUE),
          diag_ocd = max(diag_ocd, na.rm = TRUE),
          diag_stress = max(diag_stress, na.rm = TRUE),
          diag_anorexia = max(diag_anorexia, na.rm = TRUE),
          diag_bulimia = max(diag_bulimia, na.rm = TRUE),
          diag_sleep = max(diag_sleep, na.rm = TRUE),
          diag_personality_cluster_b = max(diag_personality_cluster_b, na.rm = TRUE),
          diag_intellectual_disability = max(diag_intellectual_disability, na.rm = TRUE),
          diag_autism = max(diag_autism, na.rm = TRUE),
          diag_adhd = max(diag_adhd, na.rm = TRUE),
          diag_conduct = max(diag_conduct, na.rm = TRUE),
          diag_overdose = max(diag_overdose, na.rm = TRUE),
          diag_suicidal = max(diag_suicidal, na.rm = TRUE),
          .groups = "drop"
        ),
      by = "lopnr"
    ) %>%
    mutate(across(starts_with("diag_"), ~if_else(is.na(.), 0L, .))) %>%
    select(-fu_start)

  cat("\nDiagnosis covariate summary (Table S3):\n")
  cov_diagnoses %>%
    summarise(across(-lopnr, sum)) %>%
    tidyr::pivot_longer(everything(), names_to = "covariate", values_to = "count") %>%
    arrange(desc(count)) %>%
    print(n = 20)

  out_file <- if (grace_days == 28L) "cov_diagnoses.rds" else sprintf("cov_diagnoses_%d.rds", grace_days)
  save_rds(cov_diagnoses, out_file)
  cat("\nSaved", out_file, "\n")

  cat("\n=== process_cov_diagnoses.R completed ===\n")
  invisible(cov_diagnoses)
}

if (sys.nframe() == 0) {
  process_cov_diagnoses()
}

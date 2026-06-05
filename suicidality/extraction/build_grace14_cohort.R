#!/usr/bin/env Rscript
# build_grace14_cohort.R
#
# Build the 14-day-grace-period sensitivity cohort. Re-runs the parameterized
# pipeline (steps 15, 19, 20, 21, 22, 24) with grace_days = 14 to produce
# main_12wks_14.rds. The default 28-day cohort is unaffected and the PP
# pipeline (script 23) is not re-run (no PP analysis for the 14-day variant).

library(here)
here::i_am("suicidality/extraction/build_grace14_cohort.R")

source(here("suicidality", "extraction", "lib", "common.R"))
source(here("suicidality", "extraction", "lib", "extract_main_followup.R"))
source(here("suicidality", "extraction", "19_process_cov_diagnoses.R"))
source(here("suicidality", "extraction", "20_process_cov_medications.R"))
source(here("suicidality", "extraction", "21_process_cov_hospitalizations.R"))
source(here("suicidality", "extraction", "22_process_covariates_assembly.R"))
source(here("suicidality", "extraction", "24_process_final_cohorts.R"))

GRACE <- 14L

cat(sprintf("\n=== build_grace14_cohort.R (grace_days = %d) ===\n\n", GRACE))

# Step 1: Followup cohort assembly (12-week only -- no 52-week sensitivity needed)
extract_main_followup(followup_weeks = 12, output_dir = rds_output_dir(), random_seed = 42, grace_days = GRACE)

# Step 2: Covariates relative to fu_start
process_cov_diagnoses(grace_days = GRACE)
process_cov_medications(grace_days = GRACE)
process_cov_hospitalizations(grace_days = GRACE)

# Step 3: Assemble covariates
process_covariates_assembly(grace_days = GRACE)

# Step 4: Final cohort assembly
process_final_cohorts(grace_days = GRACE)

cat(sprintf("\n=== build_grace14_cohort.R completed ===\n"))

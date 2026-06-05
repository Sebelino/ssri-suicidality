# process_followup.R
# Phase 2: Case/control assignment and follow-up windows
#
# Uses the shared extract_main_followup logic from lib/extract_main_followup.R
#
# Inputs: base_28.rds, dia_all_28.rds
# Outputs: main_12wks_28_tmp.rds, main_52wks_28_tmp.rds

library(dplyr)
library(here)
here::i_am("suicidality/extraction/15_process_followup.R")

source(here("suicidality", "extraction", "lib", "common.R"))
source(here("suicidality", "extraction", "lib", "extract_main_followup.R"))

process_followup <- function(output_dir = rds_output_dir()) {
  cat("=== process_followup.R ===\n\n")

  # 12-week follow-up
  extract_main_followup(followup_weeks = 12, output_dir = output_dir, random_seed = 42)

  # 52-week follow-up
  extract_main_followup(followup_weeks = 52, output_dir = output_dir, random_seed = 42)

  cat("\n=== process_followup.R completed ===\n")
}

if (sys.nframe() == 0) {
  process_followup()
}

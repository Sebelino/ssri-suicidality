#!/usr/bin/env Rscript
# extract_all.R
# Master script that runs the extraction pipeline.
#
# Scripts 01-11 require database access (extraction).
# Scripts 12-25 only process RDS files (no database access).
#
# Each script runs as a subprocess for memory isolation.
# Logs are written to log/<run-timestamp>/ with stdout, stderr, and metadata files.
#
# Usage:
#   cd suicidality/extraction
#   Rscript extract_all.R           # Run all scripts
#   Rscript extract_all.R --dry-run # Show execution order without running
#   Rscript extract_all.R --from 5  # Resume from script #5
#   Rscript extract_all.R --list    # List scripts with numbers

# Scripts in dependency order
scripts <- c(
  "01_raw_diagnoses_index.R",
  "02_raw_prescriptions_all.R",
  "03_raw_individual_bootstrap.R",
  "04_define_cohort.R",
  "05_raw_migration.R",
  "06_raw_dor.R",
  "07_raw_diagnoses_cohort.R",
  "08_raw_diagnoses_parents.R",
  "09_raw_lisa.R",
  "10_raw_hospitalization.R",
  "11_raw_prescriptions_cohort.R",
  "12_process_base.R",
  "13_process_outcomes.R",
  "14_process_censoring.R",
  "15_process_followup.R",
  "16_process_cov_family_history.R",
  "17_process_cov_education.R",
  "18_process_cov_income.R",
  "19_process_cov_diagnoses.R",
  "20_process_cov_medications.R",
  "21_process_cov_hospitalizations.R",
  "22_process_covariates_assembly.R",
  "23_process_time_varying.R",
  "24_process_final_cohorts.R"
)

hr <- function(char = "=") paste(rep(char, 60), collapse = "")

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args
list_only <- "--list" %in% args
from_idx <- 1

if ("--from" %in% args) {
  from_pos <- which(args == "--from")
  if (from_pos < length(args)) {
    from_idx <- as.integer(args[from_pos + 1])
    if (is.na(from_idx) || from_idx < 1 || from_idx > length(scripts)) {
      stop(sprintf("--from must be between 1 and %d", length(scripts)))
    }
  }
}

# List mode
if (list_only) {
  cat("Extraction Pipeline (in dependency order):\n")
  cat("Scripts 01-11: Database extraction (require database access)\n")
  cat("Scripts 12-24: RDS processing (no database access)\n\n")

  for (i in seq_along(scripts)) {
    # Add separator between extraction and processing scripts
    if (i == 12) {
      cat("\n")
    }
    cat(sprintf("  %2d. %s\n", i, scripts[i]))
  }
  cat(sprintf("\nTotal: %d scripts\n", length(scripts)))
  quit(status = 0)
}

# Create log directory with timestamp
run_timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_dir <- file.path("log", run_timestamp)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

# Write pipeline metadata
writeLines(
  c(
    sprintf("run_timestamp: %s", run_timestamp),
    sprintf("start_time: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    sprintf("from_idx: %d", from_idx),
    sprintf("total_scripts: %d", length(scripts)),
    "",
    "scripts:",
    paste0("  - ", scripts)
  ),
  file.path(log_dir, "pipeline_metadata.txt")
)

cat(sprintf("Log directory: %s\n\n", normalizePath(log_dir)))

# Print execution plan
cat(hr(), "\n")
cat("Extraction Pipeline\n")
cat(hr(), "\n\n")

cat("Execution order:\n")
for (i in seq_along(scripts)) {
  status <- if (i < from_idx) "[skip]" else if (dry_run) "[planned]" else "[pending]"
  cat(sprintf("  %2d. %-45s %s\n", i, scripts[i], status))
}
cat("\n")

if (dry_run) {
  cat("Dry run complete. Remove --dry-run to execute.\n")
  quit(status = 0)
}

# Execute scripts as subprocesses
cat(sprintf("Starting from script #%d: %s\n\n", from_idx, scripts[from_idx]))

start_time <- Sys.time()
failed <- NULL

for (i in from_idx:length(scripts)) {
  script <- scripts[i]
  script_name <- sub("\\.R$", "", basename(script))

  cat("\n", hr(), "\n", sep = "")
  cat(sprintf("[%d/%d] %s\n", i, length(scripts), script))
  cat(hr(), "\n")
  cat(sprintf("Started: %s\n\n", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))

  script_start <- Sys.time()

  # Create log files for this script
  stdout_log <- file.path(log_dir, sprintf("%02d_%s_stdout.log", i, script_name))
  stderr_log <- file.path(log_dir, sprintf("%02d_%s_stderr.log", i, script_name))
  metadata_file <- file.path(log_dir, sprintf("%02d_%s_metadata.txt", i, script_name))

  # Write script metadata
  writeLines(
    c(
      sprintf("script: %s", script),
      sprintf("index: %d/%d", i, length(scripts)),
      sprintf("start_time: %s", format(script_start, "%Y-%m-%d %H:%M:%S")),
      sprintf("stdout_log: %s", basename(stdout_log)),
      sprintf("stderr_log: %s", basename(stderr_log))
    ),
    metadata_file
  )

  # Run script with tee to capture logs while printing to console
  # Use bash with pipefail and PIPESTATUS to capture exit code correctly
  cmd <- sprintf(
    "set -o pipefail; Rscript %s 2> >(tee %s >&2) | tee %s; exit ${PIPESTATUS[0]}",
    shQuote(script),
    shQuote(stderr_log),
    shQuote(stdout_log)
  )
  exit_code <- system(paste("bash -c", shQuote(cmd)))

  script_elapsed <- difftime(Sys.time(), script_start, units = "mins")

  # Update metadata with completion info
  write(
    c(
      sprintf("end_time: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      sprintf("duration_minutes: %.1f", as.numeric(script_elapsed)),
      sprintf("exit_code: %d", exit_code)
    ),
    metadata_file,
    append = TRUE
  )

  if (exit_code == 0) {
    cat(sprintf("\nCompleted in %.1f minutes\n", as.numeric(script_elapsed)))
  } else {
    failed <- script
    cat(sprintf("\nFailed with exit code %d after %.1f minutes\n", exit_code, as.numeric(script_elapsed)))
    cat(sprintf("\nTo resume, run: Rscript extract_all.R --from %d\n", i))
    break
  }
}

# Summary
total_elapsed <- difftime(Sys.time(), start_time, units = "mins")

cat("\n", hr(), "\n", sep = "")
cat("SUMMARY\n")
cat(hr(), "\n")

# Update pipeline metadata with completion info
write(
  c(
    "",
    sprintf("end_time: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    sprintf("total_duration_minutes: %.1f", as.numeric(total_elapsed)),
    sprintf("status: %s", if (is.null(failed)) "success" else "failed"),
    if (!is.null(failed)) sprintf("failed_at: %s", failed) else NULL
  ),
  file.path(log_dir, "pipeline_metadata.txt"),
  append = TRUE
)

if (is.null(failed)) {
  cat(sprintf("All %d scripts completed successfully!\n", length(scripts) - from_idx + 1))
  cat(sprintf("Total time: %.1f minutes\n", as.numeric(total_elapsed)))
} else {
  cat(sprintf("Pipeline failed at: %s\n", failed))
  cat(sprintf("Time before failure: %.1f minutes\n", as.numeric(total_elapsed)))
}

cat(sprintf("\nLog directory: %s\n", normalizePath(log_dir)))

quit(status = if (is.null(failed)) 0 else 1)

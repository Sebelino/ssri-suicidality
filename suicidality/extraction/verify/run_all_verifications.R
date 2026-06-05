# run_all_verifications.R
# Runs all verification scripts and produces a summary report

library(here)
here::i_am("suicidality/extraction/verify/run_all_verifications.R")

cat("======================================================================\n")
cat("RUNNING ALL VERIFICATION SCRIPTS\n")
cat("======================================================================\n\n")

# List of verification scripts
scripts <- c(
  "validate_data_consistency.R",
  "verify_atc_consistency.R",
  "verify_date_boundaries.R",
  "verify_division_by_zero.R",
  "verify_family_history_filter.R",
  "verify_followup_dates.R",
  "verify_hospitalization_source.R",
  "verify_icd_codes.R",
  "verify_lithium_fix.R",
  "verify_main_data_integrity.R",
  "verify_na_handling.R",
  "verify_paper_alignment.R",
  "verify_pp_variables.R",
  "verify_propensity_covariates.R",
  "verify_propensity_models.R",
  "verify_raw_extraction.R",
  "verify_rds_match.R",
  "verify_time_varying.R"
)

# Track results
results <- data.frame(
  script = character(),
  status = character(),
  errors = integer(),
  warnings = integer(),
  stringsAsFactors = FALSE
)

# Run each script
for (script in scripts) {
  cat("\n")
  cat("======================================================================\n")
  cat(sprintf("RUNNING: %s\n", script))
  cat("======================================================================\n")

  tryCatch({
    source(here("suicidality", "extraction", "verify", script))

    # Get errors and warnings from global environment if they exist
    err <- if (exists("errors", envir = .GlobalEnv)) get("errors", envir = .GlobalEnv) else 0
    warn <- if (exists("warnings", envir = .GlobalEnv)) get("warnings", envir = .GlobalEnv) else 0

    status <- if (err > 0) "FAILED" else if (warn > 0) "WARNINGS" else "PASSED"

    results <- rbind(results, data.frame(
      script = script,
      status = status,
      errors = err,
      warnings = warn,
      stringsAsFactors = FALSE
    ))

    # Clean up
    if (exists("errors", envir = .GlobalEnv)) rm("errors", envir = .GlobalEnv)
    if (exists("warnings", envir = .GlobalEnv)) rm("warnings", envir = .GlobalEnv)

  }, error = function(e) {
    cat(sprintf("ERROR running %s: %s\n", script, e$message))
    results <<- rbind(results, data.frame(
      script = script,
      status = "ERROR",
      errors = 1,
      warnings = 0,
      stringsAsFactors = FALSE
    ))
  })
}

# Summary
cat("\n")
cat("======================================================================\n")
cat("VERIFICATION SUMMARY\n")
cat("======================================================================\n\n")

cat(sprintf("%-40s %-10s %6s %8s\n", "Script", "Status", "Errors", "Warnings"))
cat(paste(rep("-", 70), collapse = ""), "\n")

for (i in 1:nrow(results)) {
  cat(sprintf("%-40s %-10s %6d %8d\n",
              results$script[i],
              results$status[i],
              results$errors[i],
              results$warnings[i]))
}

total_errors <- sum(results$errors)
total_warnings <- sum(results$warnings)
failed <- sum(results$status == "FAILED" | results$status == "ERROR")

cat(paste(rep("-", 70), collapse = ""), "\n")
cat(sprintf("%-40s %-10s %6d %8d\n", "TOTAL", "", total_errors, total_warnings))

cat("\n")
if (total_errors > 0) {
  cat(sprintf("RESULT: %d scripts FAILED with %d total errors\n", failed, total_errors))
} else if (total_warnings > 0) {
  cat(sprintf("RESULT: All scripts passed but with %d warnings\n", total_warnings))
} else {
  cat("RESULT: All verification scripts PASSED!\n")
}

# run_all_verifications.R
# Run all iCF verification scripts

library(here)
here::i_am("suicidality/analysis-icf/verify/run_all_verifications.R")

cat("============================================================\n")
cat("           iCF VERIFICATION SUITE\n")
cat("============================================================\n\n")

verification_scripts <- c(
  "verify_data_preparation.R",
  "verify_icf_implementation.R"
)

results <- list()

for (script in verification_scripts) {
  script_path <- here("suicidality", "analysis-icf", "verify", script)

  cat("\n")
  cat("============================================================\n")
  cat("Running:", script, "\n")
  cat("============================================================\n\n")

  tryCatch({
    source(script_path)
    results[[script]] <- "COMPLETED"
  }, error = function(e) {
    cat("\nERROR in", script, ":", e$message, "\n")
    results[[script]] <- paste("FAILED:", e$message)
  })
}

cat("\n")
cat("============================================================\n")
cat("           VERIFICATION SUITE COMPLETE\n")
cat("============================================================\n\n")

cat("Script Results:\n")
for (script in names(results)) {
  status <- if (is.character(results[[script]])) results[[script]] else "COMPLETED"
  cat("  ", script, ":", status, "\n")
}

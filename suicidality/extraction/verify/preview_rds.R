#!/usr/bin/env Rscript
# preview_rds.R
# Preview the first 3 rows of each RDS file created by the extraction pipeline.
#
# Usage:
#   Rscript preview_rds.R              # Preview all RDS files
#   Rscript preview_rds.R <file.rds>   # Preview specific file

library(here)
here::i_am("suicidality/extraction/verify/preview_rds.R")

source(here("suicidality", "extraction", "lib", "common.R"))

hr <- function(char = "=") paste(rep(char, 70), collapse = "")

preview_rds <- function(file_path) {
  cat("\n", hr(), "\n", sep = "")
  cat(basename(file_path), "\n")
  cat(hr(), "\n")

  data <- tryCatch(
    readRDS(file_path),
    error = function(e) {
      cat("Error reading file:", e$message, "\n")
      return(NULL)
    }
  )

  if (is.null(data)) return(invisible(NULL))

  if (is.data.frame(data)) {
    cat("Dimensions:", nrow(data), "rows x", ncol(data), "cols\n")
    cat("Columns:", paste(names(data), collapse = ", "), "\n\n")
    print(head(data, 3))
  } else if (is.list(data)) {
    cat("Type: list with", length(data), "elements\n")
    for (name in names(data)) {
      cat("\n$", name, ":\n", sep = "")
      elem <- data[[name]]
      if (is.data.frame(elem)) {
        cat("  Dimensions:", nrow(elem), "rows x", ncol(elem), "cols\n")
        print(head(elem, 3))
      } else {
        print(head(elem, 3))
      }
    }
  } else {
    cat("Type:", class(data), "\n")
    print(head(data, 3))
  }

  invisible(data)
}

# RDS files created by the extraction pipeline (in dependency order)
PIPELINE_RDS <- c(
  "raw_diagnoses_index.rds",
  "raw_prescriptions.rds",
  "raw_individual.rds",
  "cohort_lopnrs.rds",
  "parent_lopnrs.rds",
  "cohort_base.rds",
  "cohort_flow_summary.rds",
  "raw_migration.rds",
  "raw_dor.rds",
  "raw_diagnoses_cohort.rds",
  "raw_diagnoses_parents.rds",
  "raw_lisa.rds",
  "raw_hospitalization.rds",
  "raw_prescriptions_cohort.rds",
  "base_28.rds",
  "dia_all_28.rds",
  "cens_hosp_28.rds",
  "main_12wks_28_tmp.rds",
  "cov_family_history.rds",
  "cov_education.rds",
  "cov_income.rds",
  "cov_diagnoses.rds",
  "cov_medications.rds",
  "othermeds_28.rds",
  "cov_hospitalizations.rds",
  "base_cov_28.rds",
  "pp_12wks_max_tmp.rds",
  "main_12wks_28.rds",
  "pp_12wks_max.rds"
)

main <- function() {
  args <- commandArgs(trailingOnly = TRUE)
  output_dir <- rds_output_dir()

  if (length(args) > 0) {
    # Preview specific file(s)
    for (arg in args) {
      if (file.exists(arg)) {
        preview_rds(arg)
      } else {
        file_path <- file.path(output_dir, arg)
        if (file.exists(file_path)) {
          preview_rds(file_path)
        } else {
          cat("File not found:", arg, "\n")
        }
      }
    }
  } else {
    # Preview all pipeline RDS files
    cat("Previewing RDS files from:", output_dir, "\n")

    for (rds_file in PIPELINE_RDS) {
      file_path <- file.path(output_dir, rds_file)
      if (file.exists(file_path)) {
        preview_rds(file_path)
      } else {
        cat("\n", hr(), "\n", sep = "")
        cat(rds_file, " [NOT FOUND]\n", sep = "")
      }
    }
  }

  cat("\n", hr(), "\n", sep = "")
  cat("Done.\n")
}

if (sys.nframe() == 0) {
  main()
}

#!/usr/bin/env Rscript
# verify_rds_match.R
# Verifies that two RDS files or directories contain identical data
#
# Usage:
#   Rscript verify_rds_match.R <path1> <path2> [options]
#
# Examples:
#   Rscript verify_rds_match.R file1.rds file2.rds
#   Rscript verify_rds_match.R output/rds output/rds.bak
#   Rscript verify_rds_match.R output/rds output/rds.bak --ignore-new --verbose
#
# Options:
#   --ignore-new    Ignore files that only exist in path1 (directories only)
#   --verbose       Show detailed comparison for mismatches

suppressPackageStartupMessages(library(dplyr))

args <- commandArgs(trailingOnly = TRUE)

# Parse options
ignore_new <- "--ignore-new" %in% args
verbose <- "--verbose" %in% args
args <- args[!args %in% c("--ignore-new", "--verbose")]

if (length(args) < 2) {
  cat("Usage: Rscript verify_rds_match.R <path1> <path2> [--ignore-new] [--verbose]\n")
  cat("\nCompares two .rds files or all .rds files between two directories.\n")
  cat("\nOptions:\n")
  cat("  --ignore-new  Ignore files that only exist in path1 (directories only)\n")
  cat("  --verbose     Show detailed comparison for mismatches\n")
  quit(status = 1)
}

path1 <- args[1]
path2 <- args[2]

# Check if paths exist
if (!file.exists(path1)) {
  cat("Error: Path does not exist:", path1, "\n")
  quit(status = 1)
}
if (!file.exists(path2)) {
  cat("Error: Path does not exist:", path2, "\n")
  quit(status = 1)
}

# Helper function to compare two RDS files
# Sorts dataframes before comparing to handle non-deterministic row ordering
compare_rds <- function(file1, file2, verbose = FALSE) {
  data1 <- tryCatch(readRDS(file1), error = function(e) NULL)
  data2 <- tryCatch(readRDS(file2), error = function(e) NULL)

  if (is.null(data1)) {
    return(list(status = "error", message = paste("Cannot read:", file1)))
  }
  if (is.null(data2)) {
    return(list(status = "error", message = paste("Cannot read:", file2)))
  }

  # Sort dataframes by all columns before comparing (handles non-deterministic row order)
  if (is.data.frame(data1) && is.data.frame(data2)) {
    data1 <- data1 %>% arrange(across(everything()))
    data2 <- data2 %>% arrange(across(everything()))
  }

  if (identical(data1, data2)) {
    return(list(status = "identical"))
  }

  # Check if equal after sorting (handles attribute differences)
  eq <- all.equal(data1, data2)
  if (isTRUE(eq)) {
    return(list(status = "identical"))
  }

  # Not identical - gather details
  details <- list()
  if (is.character(eq)) {
    details$differences <- eq
  }

  if (is.data.frame(data1) && is.data.frame(data2)) {
    details$dims1 <- c(nrow(data1), ncol(data1))
    details$dims2 <- c(nrow(data2), ncol(data2))
  }

  return(list(status = "mismatch", details = details))
}

# Determine if comparing files or directories
is_file1 <- file.info(path1)$isdir == FALSE
is_file2 <- file.info(path2)$isdir == FALSE

if (is_file1 && is_file2) {
  # Compare two files
  cat("Comparing RDS files:\n")
  cat("  file1:", normalizePath(path1), "\n")
  cat("  file2:", normalizePath(path2), "\n\n")

  result <- compare_rds(path1, path2, verbose)

  if (result$status == "error") {
    cat("ERROR:", result$message, "\n")
    quit(status = 1)
  } else if (result$status == "identical") {
    cat("Files are IDENTICAL\n")
    quit(status = 0)
  } else {
    cat("Files are DIFFERENT\n\n")

    if (!is.null(result$details$dims1)) {
      cat("  file1:", result$details$dims1[1], "rows x", result$details$dims1[2], "cols\n")
      cat("  file2:", result$details$dims2[1], "rows x", result$details$dims2[2], "cols\n\n")
    }

    if (verbose && !is.null(result$details$differences)) {
      cat("Differences:\n")
      for (msg in head(result$details$differences, 10)) {
        cat("  ", msg, "\n")
      }
      if (length(result$details$differences) > 10) {
        cat("  ... and", length(result$details$differences) - 10, "more\n")
      }
    }

    quit(status = 1)
  }

} else if (!is_file1 && !is_file2) {
  # Compare two directories
  dir1 <- path1
  dir2 <- path2

  cat("Comparing RDS directories:\n")
  cat("  dir1:", normalizePath(dir1), "\n")
  cat("  dir2:", normalizePath(dir2), "\n\n")

  # List files
  files1 <- list.files(dir1, pattern = "[.]rds$", full.names = FALSE)
  files2 <- list.files(dir2, pattern = "[.]rds$", full.names = FALSE)

  cat("Files in dir1:", length(files1), "\n")
  cat("Files in dir2:", length(files2), "\n\n")

  # Check for files only in one directory
  only_in_dir1 <- setdiff(files1, files2)
  only_in_dir2 <- setdiff(files2, files1)

  exit_code <- 0

  if (length(only_in_dir1) > 0) {
    if (ignore_new) {
      cat("Files only in dir1 (ignored):\n")
    } else {
      cat("Files only in dir1:\n")
      exit_code <- 1
    }
    for (f in only_in_dir1) cat("  ", f, "\n")
    cat("\n")
  }

  if (length(only_in_dir2) > 0) {
    cat("Files only in dir2 (MISSING from dir1):\n")
    for (f in only_in_dir2) cat("  ", f, "\n")
    cat("\n")
    exit_code <- 1
  }

  # Compare common files
  common_files <- intersect(files1, files2)
  cat("Comparing", length(common_files), "common files...\n\n")

  mismatches <- c()
  identical_count <- 0

  for (f in common_files) {
    file1 <- file.path(dir1, f)
    file2 <- file.path(dir2, f)

    result <- compare_rds(file1, file2, verbose)

    if (result$status == "error") {
      cat("ERROR:", f, "-", result$message, "\n")
      mismatches <- c(mismatches, f)
    } else if (result$status == "identical") {
      identical_count <- identical_count + 1
    } else {
      mismatches <- c(mismatches, f)
      cat("MISMATCH:", f, "\n")

      if (verbose) {
        if (!is.null(result$details$differences)) {
          for (msg in head(result$details$differences, 5)) {
            cat("  ", msg, "\n")
          }
          if (length(result$details$differences) > 5) {
            cat("  ... and", length(result$details$differences) - 5, "more differences\n")
          }
        }

        if (!is.null(result$details$dims1)) {
          cat("  dir1:", result$details$dims1[1], "rows x", result$details$dims1[2], "cols\n")
          cat("  dir2:", result$details$dims2[1], "rows x", result$details$dims2[2], "cols\n")
        }
        cat("\n")
      }
    }
  }

  # Summary
  cat("\n")
  cat("========================================\n")
  cat("SUMMARY\n")
  cat("========================================\n")
  cat("Identical files:", identical_count, "/", length(common_files), "\n")

  if (length(mismatches) > 0) {
    cat("Mismatched files:", length(mismatches), "\n")
    for (f in mismatches) cat("  ", f, "\n")
    exit_code <- 1
  }

  if (length(only_in_dir2) > 0) {
    cat("Missing from dir1:", length(only_in_dir2), "\n")
  }

  if (!ignore_new && length(only_in_dir1) > 0) {
    cat("Extra in dir1:", length(only_in_dir1), "\n")
  }

  if (exit_code == 0) {
    cat("\nAll files match!\n")
  } else {
    cat("\nVerification FAILED\n")
  }

  quit(status = exit_code)

} else {
  cat("Error: Both paths must be either files or directories\n")
  cat("  path1:", path1, "(", if (is_file1) "file" else "directory", ")\n")
  cat("  path2:", path2, "(", if (is_file2) "file" else "directory", ")\n")
  quit(status = 1)
}

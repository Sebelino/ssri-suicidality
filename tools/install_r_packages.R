#!/usr/bin/env Rscript
# install_r_packages.R
#
# Idempotent CRAN bootstrap for the ssri-suicidality pipeline. Installs every
# package the production code (extraction + analysis + iCF + hdiCF) loads via
# `library()` or `::`. Already-installed packages are skipped, so this is safe
# to re-run.
#
# Called by tools/reproduce_on_tensor.sh as the first stage of the SLURM chain.
# Can also be run manually:
#   module load R/4.5.1 GCCcore/13.2.0
#   Rscript tools/install_r_packages.R

required <- c(
  # Database / IO
  "DBI", "odbc", "haven", "jsonlite",

  # Data manipulation
  "data.table", "dplyr", "tidyr", "tidyverse", "vctrs",

  # Plotting
  "ggplot2", "patchwork", "scales", "survminer",

  # Path resolution
  "here",

  # Stats / modelling
  "boot", "MASS", "survival", "tableone", "epiDisplay",

  # Causal forests + tree visualisation
  "grf", "partykit",

  # Misc helpers used by setup / installs
  "remotes"
)

# Use the user's personal library (created on first install if missing)
user_lib <- Sys.getenv("R_LIBS_USER", unset = file.path("~", "R", "library"))
user_lib <- path.expand(user_lib)
if (!dir.exists(user_lib)) {
  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
}
.libPaths(c(user_lib, .libPaths()))

cat("R version : ", R.version.string, "\n")
cat("User libPath: ", user_lib, "\n\n")

repos <- c(CRAN = "https://cloud.r-project.org")

installed <- rownames(installed.packages())
missing   <- setdiff(required, installed)

if (length(missing) == 0) {
  cat("All ", length(required), " required packages already installed.\n", sep = "")
} else {
  cat("Installing ", length(missing), " missing packages:\n  ",
      paste(missing, collapse = ", "), "\n\n", sep = "")
  install.packages(missing, lib = user_lib, repos = repos, Ncpus = max(1, parallel::detectCores() - 1))
}

# Verify everything loads. Failure here aborts the SLURM chain before extract
# rather than burning a job on a missing dependency.
cat("\nVerifying loadability:\n")
ok <- TRUE
for (pkg in required) {
  res <- suppressPackageStartupMessages(
    tryCatch(requireNamespace(pkg, quietly = TRUE), error = function(e) FALSE))
  cat(sprintf("  %-15s %s\n", pkg, if (isTRUE(res)) "OK" else "FAIL"))
  if (!isTRUE(res)) ok <- FALSE
}

if (!ok) {
  stop("One or more required packages could not be loaded; see above.")
}
cat("\nAll dependencies satisfied.\n")

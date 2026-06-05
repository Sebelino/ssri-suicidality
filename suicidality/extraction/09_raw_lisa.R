# raw_lisa.R
# Phase 1c: Extract LISA data (education, income) for parents
#
# Depends on: parent_lopnrs.rds
#
# Output: raw_lisa.rds
#   Columns: lopnr, year, edu_old1, edu_old2, income

library(DBI)
library(odbc)
library(dplyr)
library(here)
here::i_am("suicidality/extraction/09_raw_lisa.R")

source(here("suicidality", "extraction", "lib", "common.R"))

raw_lisa <- function(output_dir = rds_output_dir()) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  cat("=== raw_lisa.R ===\n\n")

  # Load parent lopnrs
  parent_lopnrs <- read_rds("parent_lopnrs.rds")$lopnr
  cat("Parent lopnrs:", length(parent_lopnrs), "\n")

  con <- db_connect()
  on.exit(dbDisconnect(con))

  # Extract LISA data for parents (education and income)
  cat("Extracting LISA data (batched)...\n")

  raw_lisa <- batch_query(con, "
    SELECT lopnr, ar, sun2000niva_old, SUN2020NIVA_OLD, dispinkfam04
    FROM dbo.v_lisa
    WHERE ar BETWEEN 2004 AND 2019
      AND lopnr IN (%s)
  ", parent_lopnrs) %>%
    rename(
      year = ar,
      edu_old1 = sun2000niva_old,
      edu_old2 = SUN2020NIVA_OLD,
      income = dispinkfam04
    ) %>%
    mutate(year = as.integer(year))

  cat("LISA records:", nrow(raw_lisa), "\n")
  cat("Unique parent lopnr:", n_distinct(raw_lisa$lopnr), "\n")
  cat("Year range:", min(raw_lisa$year), "-", max(raw_lisa$year), "\n")

  save_rds(raw_lisa, "raw_lisa.rds")
  cat("Saved raw_lisa.rds\n")

  cat("\n=== raw_lisa.R completed ===\n")
  invisible(raw_lisa)
}

if (sys.nframe() == 0) {
  raw_lisa()
}

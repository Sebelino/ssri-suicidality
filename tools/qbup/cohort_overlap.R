library(DBI)
library(odbc)
library(here)

source(here("suicidality", "extraction", "lib", "common.R"))

cohort <- read_rds("main_12wks_28.rds")
cohort_lopnr <- unique(cohort$lopnr)
cat("Cohort rows:", nrow(cohort), "\n")
cat("Distinct LOPNR in cohort (pre-CCA):", length(cohort_lopnr), "\n")

con <- db_connect("Psych4")

qbup_views <- c(
  "V_QBUP_EPISOD", "V_QBUP_DIAGNOS", "V_QBUP_ATGARD",
  "V_QBUP_CGAS",   "V_QBUP_SDQ",     "V_QBUP_KONTAKTORSAK"
)

per_view <- list()
for (v in qbup_views) {
  ids <- dbGetQuery(con, sprintf("SELECT DISTINCT LOPNR FROM dbo.%s", v))$LOPNR
  per_view[[v]] <- ids
  cat(sprintf("  %-22s distinct LOPNR: %d\n", v, length(ids)))
}

qbup_any <- unique(unlist(per_view))
cat("\nDistinct LOPNR appearing in ANY QBUP view:", length(qbup_any), "\n")

overlap <- intersect(cohort_lopnr, qbup_any)
cat("\n=== OVERLAP ===\n")
cat("Cohort (pre-CCA)               :", length(cohort_lopnr), "\n")
cat("QBUP (any view)                :", length(qbup_any), "\n")
cat("Intersect                      :", length(overlap), "\n")
cat(sprintf("Share of cohort in QBUP        : %.1f%%\n",
            100 * length(overlap) / length(cohort_lopnr)))

cat("\n=== PER-VIEW OVERLAP WITH COHORT ===\n")
for (v in qbup_views) {
  n <- length(intersect(cohort_lopnr, per_view[[v]]))
  cat(sprintf("  %-22s %6d (%.1f%% of cohort)\n",
              v, n, 100 * n / length(cohort_lopnr)))
}

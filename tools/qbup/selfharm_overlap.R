library(DBI); library(odbc); library(here)
source(here("suicidality", "extraction", "lib", "common.R"))

cohort <- read_rds("main_12wks_28.rds")
cohort_lopnr <- unique(cohort$lopnr)

con <- db_connect("Psych4")

# Use distinct list of all kontaktorsak categories, filter in R to avoid encoding issues
all_ko <- dbGetQuery(con, "SELECT DISTINCT KONTAKTORSAK FROM dbo.V_QBUP_KONTAKTORSAK")
cat("Categories matching suicide/self-harm:\n")
hits <- all_ko$KONTAKTORSAK[grepl("Suicid|skadebeteende|sjalvskade", all_ko$KONTAKTORSAK,
                                  ignore.case = TRUE, useBytes = TRUE)]
print(hits)

ids_per_cat <- list()
for (cat_name in hits) {
  ids <- dbGetQuery(con,
    "SELECT DISTINCT LOPNR FROM dbo.V_QBUP_KONTAKTORSAK WHERE KONTAKTORSAK = ?",
    params = list(cat_name))$LOPNR
  ids_per_cat[[cat_name]] <- ids
  n_overlap <- length(intersect(cohort_lopnr, ids))
  cat(sprintf("\n  %-40s national=%d  cohort_overlap=%d (%.2f%%)\n",
              cat_name, length(ids), n_overlap, 100 * n_overlap / length(cohort_lopnr)))
}

any_sui <- unique(unlist(ids_per_cat))
overlap <- intersect(cohort_lopnr, any_sui)
cat(sprintf("\n=== UNION (any suicide-related kontaktorsak) ===\n"))
cat(sprintf("  National distinct LOPNRs : %d\n", length(any_sui)))
cat(sprintf("  Cohort overlap           : %d (%.2f%% of cohort)\n",
            length(overlap), 100 * length(overlap) / length(cohort_lopnr)))

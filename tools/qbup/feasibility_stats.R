library(DBI)
library(odbc)
library(here)
library(dplyr)

source(here("suicidality", "extraction", "lib", "common.R"))

cohort <- read_rds("main_12wks_28.rds")
cat("Cohort columns hint:\n"); print(intersect(c("lopnr", "age", "kon", "fu_start"), names(cohort)))

cohort_lopnr <- unique(cohort$lopnr)

con <- db_connect("Psych4")

# 1) Episode-level: distinct cohort LOPNRs in QBUP
qbup_lopnr <- dbGetQuery(con, "SELECT DISTINCT LOPNR FROM dbo.V_QBUP_EPISOD")$LOPNR
in_qbup <- cohort_lopnr %in% qbup_lopnr

# 2) Age stratification of overlap
if ("age" %in% names(cohort)) {
  age_strata <- cut(cohort$age, breaks = c(5, 11, 14, 17, 24), include.lowest = TRUE,
                    labels = c("6-11", "12-14", "15-17", "18-24"))
  tab <- data.frame(
    age = age_strata,
    in_qbup = cohort$lopnr %in% qbup_lopnr
  ) |>
    group_by(age) |>
    summarise(n = n(), in_qbup = sum(in_qbup), pct = round(100 * sum(in_qbup) / n(), 1))
  cat("\n=== Age-stratified QBUP overlap ===\n")
  print(tab)
}

# 3) Suicide-related kontaktorsak: how many cohort members ever had one?
suicide_ko <- dbGetQuery(con, "
  SELECT DISTINCT LOPNR, KONTAKTORSAK
  FROM dbo.V_QBUP_KONTAKTORSAK
  WHERE KONTAKTORSAK LIKE '%Suicid%' OR KONTAKTORSAK LIKE '%jvskade%'
")
cat("\n=== Suicide-related KONTAKTORSAK ===\n")
print(table(suicide_ko$KONTAKTORSAK))
cat("Distinct LOPNRs with any suicide-related kontaktorsak (national):",
    length(unique(suicide_ko$LOPNR)), "\n")
overlap_sui <- intersect(cohort_lopnr, unique(suicide_ko$LOPNR))
cat("Cohort members with any suicide-related kontaktorsak:", length(overlap_sui),
    sprintf("(%.2f%% of cohort)", 100 * length(overlap_sui) / length(cohort_lopnr)), "\n")

# 4) Date range of QBUP episodes (rough time coverage)
date_range <- dbGetQuery(con, "
  SELECT MIN(START_EP_DATUM) AS min_start, MAX(START_EP_DATUM) AS max_start,
         MIN(SLUT_EP_DATUM)  AS min_end,   MAX(SLUT_EP_DATUM)  AS max_end
  FROM dbo.V_QBUP_EPISOD
")
cat("\n=== QBUP date coverage ===\n"); print(date_range)

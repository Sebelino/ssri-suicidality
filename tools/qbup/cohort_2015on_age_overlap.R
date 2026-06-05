library(DBI); library(odbc); library(here); library(dplyr)
source(here("suicidality", "extraction", "lib", "common.R"))

cohort <- read_rds("main_12wks_28.rds")

qbup_start <- as.Date("2015-01-07")
cohort_post2015 <- cohort[cohort$fu_start >= qbup_start, ]

cat("Full pre-CCA cohort:", nrow(cohort), "\n")
cat("Restricted to fu_start >= 2015-01-07:", nrow(cohort_post2015),
    sprintf(" (%.1f%% of cohort)\n", 100 * nrow(cohort_post2015) / nrow(cohort)))
cat("fu_start range in restricted cohort:",
    as.character(range(cohort_post2015$fu_start)), "\n\n")

con <- db_connect("Psych4")
qbup_lopnr <- dbGetQuery(con, "SELECT DISTINCT LOPNR FROM dbo.V_QBUP_EPISOD")$LOPNR

age_strata <- cut(cohort_post2015$age, breaks = c(5, 11, 14, 17, 24),
                  include.lowest = TRUE,
                  labels = c("6-11", "12-14", "15-17", "18-24"))

tab <- data.frame(
  age = age_strata,
  in_qbup = cohort_post2015$lopnr %in% qbup_lopnr
) |>
  group_by(age) |>
  summarise(n = n(), in_qbup = sum(in_qbup),
            pct = round(100 * sum(in_qbup) / n(), 1),
            .groups = "drop")

total_overlap <- sum(cohort_post2015$lopnr %in% qbup_lopnr)
cat("=== QBUP overlap, cohort restricted to fu_start >= 2015-01-07 ===\n")
print(tab)
cat(sprintf("\nTotal: %d / %d (%.1f%%)\n",
            total_overlap, nrow(cohort_post2015),
            100 * total_overlap / nrow(cohort_post2015)))

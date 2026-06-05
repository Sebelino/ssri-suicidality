# Summary_statistics.R
# Descriptive statistics for the SSRI and suicidal behavior study cohort

library(dplyr)
library(tableone)
library(here)
here::i_am("suicidality/analysis/Summary_statistics.R")

source(here("suicidality", "analysis", "common.R"))

cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("SSRI AND SUICIDAL BEHAVIOR STUDY - COHORT SUMMARY\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")

# Load datasets. Table 2 (baseline_table.tex) describes the complete-case
# analysis cohort. A second table (excluded_baseline_table.tex) describes the
# patients dropped by the CC filter side-by-side with the CC cohort, on the
# four partially-observed covariates plus demographics, and is shown in §A as
# a supplementary table. Together they replace the previous "pre-CCA cohort
# with Missing rows" design (BUGS #11 superseded 2026-05-13).
data_12wks_full <- read_rds_file("main_12wks_28.rds")
data_12wks <- filter_complete_cases(data_12wks_full)

# Calculate actual follow-up time in weeks (truncated by event/death/emigration/admin)
data_12wks$fu_weeks <- as.numeric(data_12wks$fu_end_itt - data_12wks$fu_start) / 7

# =============================================================================
# SECTION 1: COHORT SIZE
# =============================================================================
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("1. COHORT SIZE\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n\n")

cat("12-week follow-up cohort:\n")
cat("  Total N:", nrow(data_12wks), "\n")
cat("  SSRI (cc=1):", sum(data_12wks$cc == 1), "\n")
cat("  Control (cc=0):", sum(data_12wks$cc == 0), "\n\n")

# =============================================================================
# SECTION 2: SUICIDAL BEHAVIOR EVENTS
# =============================================================================
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("2. SUICIDAL BEHAVIOR EVENTS\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n\n")

cat("12-week follow-up:\n")
cat("  Total events:", sum(data_12wks$sb12_itt, na.rm = TRUE), "\n")
cat("  Events in SSRI group:", sum(data_12wks$sb12_itt[data_12wks$cc == 1], na.rm = TRUE), "\n")
cat("  Events in Control group:", sum(data_12wks$sb12_itt[data_12wks$cc == 0], na.rm = TRUE), "\n")
cat("  Event rate (overall):", sprintf("%.2f%%", 100 * mean(data_12wks$sb12_itt, na.rm = TRUE)), "\n")
cat("  Event rate (SSRI):", sprintf("%.2f%%", 100 * mean(data_12wks$sb12_itt[data_12wks$cc == 1], na.rm = TRUE)), "\n")
cat("  Event rate (Control):", sprintf("%.2f%%", 100 * mean(data_12wks$sb12_itt[data_12wks$cc == 0], na.rm = TRUE)), "\n\n")

# =============================================================================
# SECTION 3: DEMOGRAPHICS
# =============================================================================
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("3. DEMOGRAPHICS (12-week cohort)\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n\n")

cat("Age at diagnosis:\n")
cat("  Mean (SD):", sprintf("%.1f (%.1f)", mean(data_12wks$age, na.rm = TRUE), sd(data_12wks$age, na.rm = TRUE)), "\n")
cat("  Median [IQR]:", sprintf("%.1f [%.1f-%.1f]",
    median(data_12wks$age, na.rm = TRUE),
    quantile(data_12wks$age, 0.25, na.rm = TRUE),
    quantile(data_12wks$age, 0.75, na.rm = TRUE)), "\n")
cat("  Range:", sprintf("%.1f - %.1f", min(data_12wks$age, na.rm = TRUE), max(data_12wks$age, na.rm = TRUE)), "\n\n")

cat("Sex:\n")
cat("  Female:", sum(data_12wks$female == 1, na.rm = TRUE),
    sprintf("(%.1f%%)", 100 * mean(data_12wks$female == 1, na.rm = TRUE)), "\n")
cat("  Male:", sum(data_12wks$female == 0, na.rm = TRUE),
    sprintf("(%.1f%%)", 100 * mean(data_12wks$female == 0, na.rm = TRUE)), "\n\n")

cat("Calendar year of diagnosis:\n")
cat("  Mean (SD):", sprintf("%.1f (%.1f)", mean(data_12wks$year, na.rm = TRUE), sd(data_12wks$year, na.rm = TRUE)), "\n")
cat("  Range:", sprintf("%d - %d", min(data_12wks$year, na.rm = TRUE), max(data_12wks$year, na.rm = TRUE)), "\n\n")

# =============================================================================
# SECTION 4: FOLLOW-UP TIME
# =============================================================================
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("4. FOLLOW-UP TIME\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n\n")

cat("12-week cohort (weeks):\n")
cat("  Mean (SD):", sprintf("%.1f (%.1f)", mean(data_12wks$fu_weeks, na.rm = TRUE), sd(data_12wks$fu_weeks, na.rm = TRUE)), "\n")
cat("  Median [IQR]:", sprintf("%.1f [%.1f-%.1f]",
    median(data_12wks$fu_weeks, na.rm = TRUE),
    quantile(data_12wks$fu_weeks, 0.25, na.rm = TRUE),
    quantile(data_12wks$fu_weeks, 0.75, na.rm = TRUE)), "\n\n")

# =============================================================================
# SECTION 5: BASELINE CHARACTERISTICS BY TREATMENT GROUP
# =============================================================================
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("5. BASELINE CHARACTERISTICS BY TREATMENT GROUP (12-week cohort)\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n\n")

# Prepare data for tableone
data_12wks$treatment <- factor(data_12wks$cc, levels = c(0, 1), labels = c("Control", "SSRI"))

# Define variables for the table (using Table S3 diagnosis variables)
vars <- c("age", "female", "year", "hosp",
          "diag_mdd", "diag_bipolar", "diag_psychotic",
          "diag_phobic", "diag_anxiety_other", "diag_ocd", "diag_stress",
          "diag_anorexia", "diag_bulimia", "diag_sleep",
          "diag_sud", "diag_personality_cluster_b",
          "diag_adhd", "diag_intellectual_disability", "diag_autism", "diag_conduct",
          "diag_organic", "diag_overdose", "diag_suicidal",
          "med_antipsychotic", "med_hypnotic", "med_benzodiazepine", "med_antiepileptic", "med_opioid", "med_stimulant",
          "fh_suicidal", "fh_depr")

# Define categorical variables
catVars <- c("female", "hosp",
             "diag_mdd", "diag_bipolar", "diag_psychotic",
             "diag_phobic", "diag_anxiety_other", "diag_ocd", "diag_stress",
             "diag_anorexia", "diag_bulimia", "diag_sleep",
             "diag_sud", "diag_personality_cluster_b",
             "diag_adhd", "diag_intellectual_disability", "diag_autism", "diag_conduct",
             "diag_organic", "diag_overdose", "diag_suicidal",
             "med_antipsychotic", "med_hypnotic", "med_benzodiazepine", "med_antiepileptic", "med_opioid", "med_stimulant",
             "fh_suicidal", "fh_depr")

# Create table
table1 <- CreateTableOne(vars = vars, strata = "treatment", data = data_12wks,
                         factorVars = catVars, test = FALSE)
print(table1, showAllLevels = TRUE, formatOptions = list(big.mark = ","))

# =============================================================================
# SECTION 6: PSYCHIATRIC COMORBIDITIES SUMMARY (Table S3)
# =============================================================================
cat("\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("6. PSYCHIATRIC COMORBIDITIES - Table S3 (12-week cohort)\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n\n")

comorbidities <- data.frame(
  Condition = c(
    "Major depressive disorder (F32-F33)",
    "Bipolar/manic disorders (F30-F31)",
    "Psychotic disorders (F20-F29)",
    "Phobic anxiety (F40.0-F40.2)",
    "Other anxiety - panic/GAD (F41.0-F41.1)",
    "OCD (F42)",
    "Stress/adjustment (F43)",
    "Anorexia nervosa (F50.0-F50.1)",
    "Bulimia nervosa (F50.2-F50.3)",
    "Sleep disorders (F51)",
    "Substance use disorder (F10-F19)",
    "Cluster B personality (F60.2-F60.3)",
    "ADHD (F90)",
    "Intellectual disability (F70-F79)",
    "Autism spectrum (F84)",
    "Conduct disorder (F91)",
    "Organic mental disorder (F00-F09)",
    "Overdose/poisoning (T36-T51, X40-X49)",
    "Prior suicidal behavior (X60-X84, Y10-Y34)"
  ),
  N = c(
    sum(data_12wks$diag_mdd, na.rm = TRUE),
    sum(data_12wks$diag_bipolar, na.rm = TRUE),
    sum(data_12wks$diag_psychotic, na.rm = TRUE),
    sum(data_12wks$diag_phobic, na.rm = TRUE),
    sum(data_12wks$diag_anxiety_other, na.rm = TRUE),
    sum(data_12wks$diag_ocd, na.rm = TRUE),
    sum(data_12wks$diag_stress, na.rm = TRUE),
    sum(data_12wks$diag_anorexia, na.rm = TRUE),
    sum(data_12wks$diag_bulimia, na.rm = TRUE),
    sum(data_12wks$diag_sleep, na.rm = TRUE),
    sum(data_12wks$diag_sud, na.rm = TRUE),
    sum(data_12wks$diag_personality_cluster_b, na.rm = TRUE),
    sum(data_12wks$diag_adhd, na.rm = TRUE),
    sum(data_12wks$diag_intellectual_disability, na.rm = TRUE),
    sum(data_12wks$diag_autism, na.rm = TRUE),
    sum(data_12wks$diag_conduct, na.rm = TRUE),
    sum(data_12wks$diag_organic, na.rm = TRUE),
    sum(data_12wks$diag_overdose, na.rm = TRUE),
    sum(data_12wks$diag_suicidal, na.rm = TRUE)
  ),
  Percent = sprintf("%.1f%%", 100 * c(
    mean(data_12wks$diag_mdd, na.rm = TRUE),
    mean(data_12wks$diag_bipolar, na.rm = TRUE),
    mean(data_12wks$diag_psychotic, na.rm = TRUE),
    mean(data_12wks$diag_phobic, na.rm = TRUE),
    mean(data_12wks$diag_anxiety_other, na.rm = TRUE),
    mean(data_12wks$diag_ocd, na.rm = TRUE),
    mean(data_12wks$diag_stress, na.rm = TRUE),
    mean(data_12wks$diag_anorexia, na.rm = TRUE),
    mean(data_12wks$diag_bulimia, na.rm = TRUE),
    mean(data_12wks$diag_sleep, na.rm = TRUE),
    mean(data_12wks$diag_sud, na.rm = TRUE),
    mean(data_12wks$diag_personality_cluster_b, na.rm = TRUE),
    mean(data_12wks$diag_adhd, na.rm = TRUE),
    mean(data_12wks$diag_intellectual_disability, na.rm = TRUE),
    mean(data_12wks$diag_autism, na.rm = TRUE),
    mean(data_12wks$diag_conduct, na.rm = TRUE),
    mean(data_12wks$diag_organic, na.rm = TRUE),
    mean(data_12wks$diag_overdose, na.rm = TRUE),
    mean(data_12wks$diag_suicidal, na.rm = TRUE)
  ))
)

print(comorbidities, row.names = FALSE)

# =============================================================================
# SECTION 7: CONCOMITANT MEDICATIONS
# =============================================================================
cat("\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("7. CONCOMITANT MEDICATIONS (12-week cohort)\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n\n")

medications <- data.frame(
  Medication = c("Antipsychotics", "Hypnotics/Sedatives", "Benzodiazepines",
                 "Antiepileptics", "Opioids", "Stimulants"),
  N = c(sum(data_12wks$med_antipsychotic, na.rm = TRUE),
        sum(data_12wks$med_hypnotic, na.rm = TRUE),
        sum(data_12wks$med_benzodiazepine, na.rm = TRUE),
        sum(data_12wks$med_antiepileptic, na.rm = TRUE),
        sum(data_12wks$med_opioid, na.rm = TRUE),
        sum(data_12wks$med_stimulant, na.rm = TRUE)),
  Percent = sprintf("%.1f%%", 100 * c(
    mean(data_12wks$med_antipsychotic, na.rm = TRUE),
    mean(data_12wks$med_hypnotic, na.rm = TRUE),
    mean(data_12wks$med_benzodiazepine, na.rm = TRUE),
    mean(data_12wks$med_antiepileptic, na.rm = TRUE),
    mean(data_12wks$med_opioid, na.rm = TRUE),
    mean(data_12wks$med_stimulant, na.rm = TRUE)))
)

print(medications, row.names = FALSE)

# =============================================================================
# SECTION 8: FAMILY HISTORY
# =============================================================================
cat("\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("8. FAMILY HISTORY (12-week cohort)\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n\n")

cat("Family history of suicidal behavior (number of parents):\n")
cat("  0 parents:", sum(data_12wks$fh_suicidal == 0, na.rm = TRUE),
    sprintf("(%.1f%%)", 100 * mean(data_12wks$fh_suicidal == 0, na.rm = TRUE)), "\n")
cat("  1 parent:", sum(data_12wks$fh_suicidal == 1, na.rm = TRUE),
    sprintf("(%.1f%%)", 100 * mean(data_12wks$fh_suicidal == 1, na.rm = TRUE)), "\n")
cat("  2 parents:", sum(data_12wks$fh_suicidal == 2, na.rm = TRUE),
    sprintf("(%.1f%%)", 100 * mean(data_12wks$fh_suicidal == 2, na.rm = TRUE)), "\n\n")

cat("Family history of depression (number of parents):\n")
cat("  0 parents:", sum(data_12wks$fh_depr == 0, na.rm = TRUE),
    sprintf("(%.1f%%)", 100 * mean(data_12wks$fh_depr == 0, na.rm = TRUE)), "\n")
cat("  1 parent:", sum(data_12wks$fh_depr == 1, na.rm = TRUE),
    sprintf("(%.1f%%)", 100 * mean(data_12wks$fh_depr == 1, na.rm = TRUE)), "\n")
cat("  2 parents:", sum(data_12wks$fh_depr == 2, na.rm = TRUE),
    sprintf("(%.1f%%)", 100 * mean(data_12wks$fh_depr == 2, na.rm = TRUE)), "\n\n")

# =============================================================================
# SECTION 9: CARE SETTING
# =============================================================================
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("9. CARE SETTING (12-week cohort)\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n\n")

cat("Source of diagnosis:\n")
source_tab <- table(data_12wks$source, useNA = "ifany")
for (s in names(source_tab)) {
  pct <- 100 * source_tab[s] / sum(source_tab)
  cat(sprintf("  %s: %d (%.1f%%)\n", s, source_tab[s], pct))
}

cat("\nHospitalization status:\n")
hosp_tab <- table(data_12wks$hosp, useNA = "ifany")
for (h in names(hosp_tab)) {
  pct <- 100 * hosp_tab[h] / sum(hosp_tab)
  label <- switch(as.character(h),
                  "0" = "Outpatient only",
                  "1" = "Inpatient",
                  "2" = "Inpatient (other)",
                  "3" = "Inpatient (other)",
                  h)
  cat(sprintf("  %s (%s): %d (%.1f%%)\n", h, label, hosp_tab[h], pct))
}

# =============================================================================
# SECTION 10: GENERATE LATEX TABLE FOR THESIS (Table 2: Baseline characteristics)
# =============================================================================
cat("\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("10. GENERATING LATEX BASELINE TABLE\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n\n")

# Helper: compute SMD for a binary variable
smd_binary <- function(x, group) {
  p1 <- mean(x[group == 1], na.rm = TRUE)
  p0 <- mean(x[group == 0], na.rm = TRUE)
  pooled_sd <- sqrt((p1 * (1 - p1) + p0 * (1 - p0)) / 2)
  if (pooled_sd == 0) return(0)
  (p1 - p0) / pooled_sd
}

# Helper: compute SMD for a continuous variable
smd_continuous <- function(x, group) {
  m1 <- mean(x[group == 1], na.rm = TRUE)
  m0 <- mean(x[group == 0], na.rm = TRUE)
  s1 <- sd(x[group == 1], na.rm = TRUE)
  s0 <- sd(x[group == 0], na.rm = TRUE)
  pooled_sd <- sqrt((s1^2 + s0^2) / 2)
  if (pooled_sd == 0) return(0)
  (m1 - m0) / pooled_sd
}

# Helper: format n (%)
fmt_np <- function(x, group_val) {
  sub <- data_12wks[data_12wks$cc == group_val, ]
  n <- sum(sub[[x]], na.rm = TRUE)
  pct <- 100 * mean(sub[[x]], na.rm = TRUE)
  sprintf("%s (%.1f)", formatC(n, big.mark = ",", format = "d"), pct)
}

# Helper: format mean (SD)
fmt_ms <- function(x, group_val) {
  sub <- data_12wks[data_12wks$cc == group_val, ]
  m <- mean(sub[[x]], na.rm = TRUE)
  s <- sd(sub[[x]], na.rm = TRUE)
  sprintf("%.1f (%.1f)", m, s)
}

W <- data_12wks$cc

# Build rows: list of (label, ssri_str, ctrl_str, smd_val)
rows <- list()

# Helper: section header row (italic label, no values, no SMD)
section_header <- function(label) {
  list(list(sprintf("\\textit{%s}", label), "", "", NA))
}

# ---- Demographics ----
rows <- c(rows, section_header("Demographics"))
rows <- c(rows, list(list(
  "\\quad Female, n (\\%)",
  fmt_np("female", 1), fmt_np("female", 0),
  smd_binary(data_12wks$female, W)
)))
# Helper: format n (%) for a categorical level
fmt_np_level <- function(var, level, group_val) {
  vec <- data_12wks[[var]][data_12wks$cc == group_val]
  n <- sum(vec == level, na.rm = TRUE)
  pct <- 100 * mean(vec == level, na.rm = TRUE)
  sprintf("%s (%.1f)", formatC(n, big.mark = ",", format = "d"), pct)
}

# Helper: SMD for a binary indicator (level vs not)
smd_level <- function(var, level) {
  ind <- as.numeric(data_12wks[[var]] == level)
  smd_binary(ind, W)
}

# Helper: multi-level SMD for categorical variable (Yang & Dalton 2012).
# Generalizes Cohen's d to multi-category variables by computing a Mahalanobis-
# style distance between the proportion vectors in the two groups.
smd_multilevel <- function(var, group_var = W) {
  vec <- data_12wks[[var]]
  levels_all <- sort(unique(vec[!is.na(vec)]))
  if (length(levels_all) < 2) return(0)
  # Drop one level to avoid singularity (proportions sum to 1)
  levs <- levels_all[-length(levels_all)]
  p1 <- sapply(levs, function(l) mean(vec[group_var == 1] == l, na.rm = TRUE))
  p0 <- sapply(levs, function(l) mean(vec[group_var == 0] == l, na.rm = TRUE))
  diff <- p1 - p0
  # Average covariance matrix
  cov_mat <- function(p) diag(p, length(p)) - outer(p, p)
  S <- (cov_mat(p1) + cov_mat(p0)) / 2
  # Use generalized inverse for robustness
  S_inv <- tryCatch(solve(S), error = function(e) MASS::ginv(S))
  as.numeric(sqrt(t(diff) %*% S_inv %*% diff))
}

# Helper: build a categorical block (header row + indented level rows).
# The block header is indented one level (\quad) under the section header,
# and individual levels are indented two levels (\quad\quad).
build_cat_block <- function(var, header_label, level_labels) {
  hdr_smd <- smd_multilevel(var)
  block <- list(list(paste0("\\quad ", header_label), "", "", hdr_smd))
  for (ll in level_labels) {
    lv <- ll[[1]]
    lab <- paste0("\\quad\\quad ", ll[[2]])
    block <- c(block, list(list(
      lab,
      fmt_np_level(var, lv, 1),
      fmt_np_level(var, lv, 0),
      NA  # no SMD on individual levels
    )))
  }
  block
}

# Age category (derived from continuous age: 0=Children 6-11, 1=Adolescents 12-17, 2=Young adults 18-24)
data_12wks$age_cat <- ifelse(data_12wks$age < 12, 0L,
                       ifelse(data_12wks$age < 18, 1L, 2L))
rows <- c(rows, build_cat_block("age_cat",
  "Age category (years)",
  list(list(0, "6--11"), list(1, "12--17"), list(2, "18--24"))))

# ---- Socioeconomic factors ----
rows <- c(rows, section_header("Socioeconomic factors"))

rows <- c(rows, build_cat_block("source",
  "Source of depression diagnosis",
  list(list("O", "Outpatient"),
       list("S", "Inpatient"),
       list("T", "Other/unknown source"))))

rows <- c(rows, build_cat_block("edufam_cat",
  "Parental education",
  list(list(0, "Primary"),
       list(1, "Secondary"),
       list(2, "Post-secondary"))))

rows <- c(rows, build_cat_block("inc_cat",
  "Family income",
  list(list(1,  "Negative"),
       list(2,  "Zero"),
       list(3,  "1st--20th percentile"),
       list(4,  "20th--80th percentile"),
       list(5,  "$>$80th percentile"))))

# ---- Family history ----
fmt_np_vec <- function(vec, group_val) {
  sub <- vec[data_12wks$cc == group_val]
  n <- sum(sub, na.rm = TRUE)
  pct <- 100 * mean(sub, na.rm = TRUE)
  sprintf("%s (%.1f)", formatC(n, big.mark = ",", format = "d"), pct)
}
# CC cohort: sentinel-99 has been filtered out, so fh is a clean count in
# {0, 1, 2}. Display as three levels (matches how iCF uses the variable
# on its integer scale, so the tree can split between any consecutive
# pair of levels).
rows <- c(rows, section_header("Family history (count of biological parents)"))
rows <- c(rows, build_cat_block("fh_suicidal",
  "Suicidal behavior",
  list(list(0, "Neither parent"),
       list(1, "One parent"),
       list(2, "Both parents"))))
rows <- c(rows, build_cat_block("fh_depr",
  "Depression",
  list(list(0, "Neither parent"),
       list(1, "One parent"),
       list(2, "Both parents"))))

# ---- Prior psychiatric diagnoses ----
# Combine F40 (phobic) + F41 (other anxiety) into a single anxiety variable
# to match the propensity score model
data_12wks$diag_anxiety <- as.integer(
  (data_12wks$diag_phobic %in% 1) | (data_12wks$diag_anxiety_other %in% 1)
)

diag_vars <- list(
  c("diag_suicidal", "\\quad Suicidal behavior, n (\\%)"),
  c("diag_overdose", "\\quad Overdose/poisoning, n (\\%)"),
  c("diag_anxiety", "\\quad Anxiety disorder (F40, F41), n (\\%)"),
  c("diag_stress", "\\quad Stress/adjustment disorder, n (\\%)"),
  c("diag_adhd", "\\quad ADHD, n (\\%)"),
  c("diag_sud", "\\quad Substance use disorder, n (\\%)"),
  c("diag_alcohol", "\\quad Alcohol use disorder, n (\\%)"),
  c("diag_ocd", "\\quad OCD, n (\\%)"),
  c("diag_sleep", "\\quad Sleep disorder, n (\\%)"),
  c("diag_anorexia", "\\quad Anorexia nervosa, n (\\%)"),
  c("diag_bulimia", "\\quad Bulimia nervosa, n (\\%)"),
  c("diag_autism", "\\quad Autism spectrum disorder, n (\\%)"),
  c("diag_conduct", "\\quad Conduct disorder, n (\\%)"),
  c("diag_bipolar", "\\quad Bipolar disorder, n (\\%)"),
  c("diag_psychotic", "\\quad Psychotic disorder, n (\\%)"),
  c("diag_organic", "\\quad Organic mental disorder, n (\\%)"),
  c("diag_personality_cluster_b", "\\quad Cluster B personality disorder, n (\\%)"),
  c("diag_intellectual_disability", "\\quad Intellectual disability, n (\\%)")
)

rows <- c(rows, section_header("Prior psychiatric diagnoses"))
for (dv in diag_vars) {
  if (dv[1] %in% names(data_12wks)) {
    rows <- c(rows, list(list(
      dv[2], fmt_np(dv[1], 1), fmt_np(dv[1], 0),
      smd_binary(data_12wks[[dv[1]]], W)
    )))
  }
}

# ---- Prior medications ----
med_vars <- list(
  c("med_antipsychotic", "\\quad Antipsychotic, n (\\%)"),
  c("med_hypnotic", "\\quad Hypnotic/sedative, n (\\%)"),
  c("med_benzodiazepine", "\\quad Benzodiazepine, n (\\%)"),
  c("med_stimulant", "\\quad Psychostimulant, n (\\%)"),
  c("med_opioid", "\\quad Opioid, n (\\%)"),
  c("med_antiepileptic", "\\quad Antiepileptic, n (\\%)"),
  c("med_mood_stabilizer", "\\quad Mood stabilizer, n (\\%)"),
  c("med_addiction", "\\quad Addiction medication, n (\\%)")
)

rows <- c(rows, section_header("Prior medications"))
for (mv in med_vars) {
  if (mv[1] %in% names(data_12wks)) {
    rows <- c(rows, list(list(
      mv[2], fmt_np(mv[1], 1), fmt_np(mv[1], 0),
      smd_binary(data_12wks[[mv[1]]], W)
    )))
  }
}

# ---- Healthcare utilization ----
rows <- c(rows, section_header("Healthcare utilization"))
rows <- c(rows, list(list(
  "\\quad Psychiatric hospitalization, n (\\%)",
  fmt_np("hosp", 1), fmt_np("hosp", 0),
  smd_binary(as.numeric(data_12wks$hosp > 0), W)
)))

# ---- SSRI type (initiators only) ----
# Derived from `atc` (the first SSRI prescription on/after diagnosis), kept
# in main_12wks_28.rds by extraction/24_process_final_cohorts.R.
ssri_atc_to_name <- c(
  "N06AB03" = "Fluoxetine",
  "N06AB04" = "Citalopram",
  "N06AB05" = "Paroxetine",
  "N06AB06" = "Sertraline",
  "N06AB08" = "Fluvoxamine",
  "N06AB10" = "Escitalopram"
)
n_initiators <- sum(W == 1)
rows <- c(rows, section_header("SSRI type (initiators only)"))
if ("atc" %in% names(data_12wks)) {
  initiator_atc <- data_12wks$atc[W == 1]
  ssri_counts <- sort(table(initiator_atc), decreasing = TRUE)
  for (atc_code in names(ssri_counts)) {
    label <- ssri_atc_to_name[[atc_code]]
    if (is.null(label)) label <- atc_code  # fall back to raw code if unknown
    n_st <- ssri_counts[[atc_code]]
    pct <- 100 * n_st / n_initiators
    rows <- c(rows, list(list(
      sprintf("\\quad %s, n (\\%%)", label),
      sprintf("%s (%.1f)", formatC(n_st, big.mark = ",", format = "d"), pct),
      "",  # non-initiators column blank
      NA   # SMD undefined when one group has no values
    )))
  }
} else {
  # Fallback for older RDS files without `atc`. Re-run extraction script 24
  # after the change that retains `atc` in the final cohort.
  ssri_type_counts <- list(
    list("Sertraline",   19604),
    list("Fluoxetine",   14148),
    list("Escitalopram",  4986),
    list("Citalopram",    3898),
    list("Paroxetine",     303)
  )
  for (st in ssri_type_counts) {
    n_st <- st[[2]]
    pct <- 100 * n_st / n_initiators
    rows <- c(rows, list(list(
      sprintf("\\quad %s, n (\\%%)", st[[1]]),
      sprintf("%s (%.1f)", formatC(n_st, big.mark = ",", format = "d"), pct),
      "",
      NA
    )))
  }
}

# Generate LaTeX
tex_lines <- c(
  "% Auto-generated by Summary_statistics.R -- do not edit manually",
  sprintf("%% Generated: %s", Sys.time())
)

for (i in seq_along(rows)) {
  r <- rows[[i]]
  smd_str <- if (is.na(r[[4]])) "" else sprintf("%.2f", abs(r[[4]]))
  tex_lines <- c(tex_lines,
    sprintf("%s & %s & %s & %s \\\\", r[[1]], r[[2]], r[[3]], smd_str)
  )
}

# Note: no \bottomrule here — the LaTeX table environment supplies its own
# (compatible with both \begin{tabular} and \begin{longtable})

icf_output_dir <- here("suicidality", "analysis-icf", "output")
tex_file <- file.path(icf_output_dir, "baseline_table.tex")
writeLines(tex_lines, tex_file)
cat("Saved:", tex_file, "\n")

# =============================================================================
# SECTION: EXCLUDED-CASES COMPARISON TABLE
# =============================================================================
# Compares the patients dropped by filter_complete_cases() (group 1, "Excluded")
# with the CC analysis cohort (group 0, "Included") on demographics, source,
# the four partially-observed covariates (with Missing sub-rows), and a small
# set of outcome-adjacent prior diagnoses. The point is to let the reader judge
# whether the dropped 5.72% differ systematically from the CC cohort.
cat("\n", "-" |> rep(70) |> paste(collapse = ""), "\n", sep = "")
cat("EXCLUDED-CASES COMPARISON TABLE\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n\n")

ex_data <- data_12wks_full
ex_data$excluded <- as.integer(
  ex_data$edufam_cat == 99 | ex_data$inc_cat == 99 |
  ex_data$fh_suicidal == 99 | ex_data$fh_depr == 99
)
cat("Excluded (group 1):", sum(ex_data$excluded == 1), "\n")
cat("Included (group 0):", sum(ex_data$excluded == 0), "\n\n")

W_ex <- ex_data$excluded

fmt_np_ex <- function(var, group_val) {
  vec <- ex_data[[var]][W_ex == group_val]
  n <- sum(vec, na.rm = TRUE)
  pct <- 100 * mean(vec, na.rm = TRUE)
  sprintf("%s (%.1f)", formatC(n, big.mark = ",", format = "d"), pct)
}
fmt_np_level_ex <- function(var, level, group_val) {
  vec <- ex_data[[var]][W_ex == group_val]
  n <- sum(vec == level, na.rm = TRUE)
  pct <- 100 * mean(vec == level, na.rm = TRUE)
  sprintf("%s (%.1f)", formatC(n, big.mark = ",", format = "d"), pct)
}
fmt_np_vec_ex <- function(vec, group_val) {
  sub <- vec[W_ex == group_val]
  n <- sum(sub, na.rm = TRUE)
  pct <- 100 * mean(sub, na.rm = TRUE)
  sprintf("%s (%.1f)", formatC(n, big.mark = ",", format = "d"), pct)
}
smd_binary_ex <- function(x) {
  p1 <- mean(x[W_ex == 1], na.rm = TRUE)
  p0 <- mean(x[W_ex == 0], na.rm = TRUE)
  pooled_var <- (p1 * (1 - p1) + p0 * (1 - p0)) / 2
  if (pooled_var == 0) return(0)
  (p1 - p0) / sqrt(pooled_var)
}
smd_multilevel_ex <- function(var) {
  vec <- ex_data[[var]]
  levels_all <- sort(unique(vec[!is.na(vec)]))
  if (length(levels_all) < 2) return(0)
  levs <- levels_all[-length(levels_all)]
  p1 <- sapply(levs, function(l) mean(vec[W_ex == 1] == l, na.rm = TRUE))
  p0 <- sapply(levs, function(l) mean(vec[W_ex == 0] == l, na.rm = TRUE))
  diff <- p1 - p0
  cov_mat <- function(p) diag(p, length(p)) - outer(p, p)
  S <- (cov_mat(p1) + cov_mat(p0)) / 2
  S_inv <- tryCatch(solve(S), error = function(e) MASS::ginv(S))
  as.numeric(sqrt(t(diff) %*% S_inv %*% diff))
}

build_cat_block_ex <- function(var, header_label, level_labels) {
  hdr_smd <- smd_multilevel_ex(var)
  block <- list(list(paste0("\\quad ", header_label), "", "", hdr_smd))
  for (ll in level_labels) {
    lv <- ll[[1]]
    lab <- paste0("\\quad\\quad ", ll[[2]])
    block <- c(block, list(list(
      lab,
      fmt_np_level_ex(var, lv, 1),
      fmt_np_level_ex(var, lv, 0),
      NA
    )))
  }
  block
}

ex_rows <- list()
ex_rows <- c(ex_rows, section_header("Demographics"))
ex_rows <- c(ex_rows, list(list(
  "\\quad Female, n (\\%)",
  fmt_np_ex("female", 1), fmt_np_ex("female", 0),
  smd_binary_ex(ex_data$female)
)))
ex_data$age_cat <- ifelse(ex_data$age < 12, 0L,
                   ifelse(ex_data$age < 18, 1L, 2L))
ex_rows <- c(ex_rows, build_cat_block_ex("age_cat",
  "Age category (years)",
  list(list(0, "6--11"), list(1, "12--17"), list(2, "18--24"))))

ex_rows <- c(ex_rows, section_header("Source of depression diagnosis"))
ex_rows <- c(ex_rows, build_cat_block_ex("source",
  "Care setting",
  list(list("O", "Outpatient"),
       list("S", "Inpatient"),
       list("T", "Other/unknown source"))))

# Four CCA covariates: show observed levels + a Missing sub-row per variable
ex_rows <- c(ex_rows, section_header("Partially-observed covariates"))
ex_rows <- c(ex_rows, build_cat_block_ex("edufam_cat",
  "Parental education",
  list(list(0, "Primary"),
       list(1, "Secondary"),
       list(2, "Post-secondary"),
       list(99, "Missing"))))
ex_rows <- c(ex_rows, build_cat_block_ex("inc_cat",
  "Family income",
  list(list(1,  "Negative"),
       list(2,  "Zero"),
       list(3,  "1st--20th percentile"),
       list(4,  "20th--80th percentile"),
       list(5,  "$>$80th percentile"),
       list(99, "Missing"))))

# Family history: three observed levels (0, 1, 2 biological parents with the
# relevant ICD-10 history) plus a Missing sub-row (sentinel-99 = no parental
# linkage from the Multi-Generation Register).
ex_rows <- c(ex_rows, build_cat_block_ex("fh_suicidal",
  "Family history of suicidal behavior",
  list(list(0,  "Neither parent"),
       list(1,  "One parent"),
       list(2,  "Both parents"),
       list(99, "Missing"))))
ex_rows <- c(ex_rows, build_cat_block_ex("fh_depr",
  "Family history of depression",
  list(list(0,  "Neither parent"),
       list(1,  "One parent"),
       list(2,  "Both parents"),
       list(99, "Missing"))))

# Outcome-adjacent prior diagnoses (always observed)
ex_rows <- c(ex_rows, section_header("Outcome-adjacent prior diagnoses"))
for (dv in list(
  c("diag_suicidal", "\\quad Prior suicidal behavior, n (\\%)"),
  c("diag_overdose", "\\quad Overdose/poisoning, n (\\%)")
)) {
  if (dv[1] %in% names(ex_data)) {
    ex_rows <- c(ex_rows, list(list(
      dv[2],
      fmt_np_ex(dv[1], 1), fmt_np_ex(dv[1], 0),
      smd_binary_ex(ex_data[[dv[1]]])
    )))
  }
}

ex_tex_lines <- c(
  "% Auto-generated by Summary_statistics.R -- do not edit manually",
  sprintf("%% Generated: %s", Sys.time())
)
for (i in seq_along(ex_rows)) {
  r <- ex_rows[[i]]
  smd_str <- if (is.na(r[[4]])) "" else sprintf("%.2f", abs(r[[4]]))
  ex_tex_lines <- c(ex_tex_lines,
    sprintf("%s & %s & %s & %s \\\\", r[[1]], r[[2]], r[[3]], smd_str)
  )
}
ex_tex_file <- file.path(icf_output_dir, "excluded_baseline_table.tex")
writeLines(ex_tex_lines, ex_tex_file)
cat("Saved:", ex_tex_file, "\n")

cat("\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("END OF SUMMARY\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")

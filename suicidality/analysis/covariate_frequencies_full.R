# covariate_frequencies_full.R
# Generate frequency tables for all covariates in main_12wks_28.rds
# Includes Table S3 (diagnoses) covariates

library(dplyr)
library(here)
here::i_am("suicidality/analysis/covariate_frequencies_full.R")

source(here("suicidality", "analysis", "common.R"))

# Load data (complete-case)
data <- filter_complete_cases(read_rds_file("main_12wks_28.rds"))
n <- nrow(data)

cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("COVARIATE FREQUENCY TABLES (FULL DATASET)\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")
cat("Total N:", format(n, big.mark = ","), "\n\n")

# Helper function to print frequency table
print_freq <- function(data, var, labels = NULL) {
  cat("-" |> rep(50) |> paste(collapse = ""), "\n")
  cat(var, "\n")
  cat("-" |> rep(50) |> paste(collapse = ""), "\n")

  tbl <- table(data[[var]], useNA = "ifany")

  for (i in seq_along(tbl)) {
    val <- names(tbl)[i]
    if (is.na(val)) val <- "NA"
    cnt <- tbl[i]
    pct <- 100 * cnt / n

    label <- ""
    if (!is.null(labels) && val %in% names(labels)) {
      label <- paste0(" (", labels[val], ")")
    }

    cat(sprintf("  %-12s %8s  (%5.1f%%)%s\n",
                val, format(cnt, big.mark = ","), pct, label))
  }
  cat("\n")
}

# Helper for binary variables
print_binary_summary <- function(data, vars, section_title, labels = NULL) {
  cat("-" |> rep(70) |> paste(collapse = ""), "\n")
  cat(section_title, "\n")
  cat("-" |> rep(70) |> paste(collapse = ""), "\n")

  results <- data.frame(
    var = vars,
    yes_n = sapply(vars, function(v) sum(data[[v]] == 1, na.rm = TRUE)),
    yes_pct = sapply(vars, function(v) 100 * mean(data[[v]] == 1, na.rm = TRUE))
  ) |> arrange(desc(yes_n))

  for (i in seq_len(nrow(results))) {
    var_name <- results$var[i]
    label_str <- if (!is.null(labels) && var_name %in% names(labels)) {
      paste0("  ", labels[var_name])
    } else {
      ""
    }
    cat(sprintf("  %-35s %8s  (%5.1f%%)%s\n",
                var_name,
                format(results$yes_n[i], big.mark = ","),
                results$yes_pct[i],
                label_str))
  }
  cat("\n")
}

# =============================================================================
# DEMOGRAPHICS
# =============================================================================
cat("DEMOGRAPHICS\n")
print_freq(data, "female", c("0" = "Male", "1" = "Female"))
print_freq(data, "cc", c("0" = "Control", "1" = "SSRI initiator"))

# Age summary
cat("-" |> rep(50) |> paste(collapse = ""), "\n")
cat("age\n")
cat("-" |> rep(50) |> paste(collapse = ""), "\n")
cat(sprintf("  Mean (SD):     %.1f (%.1f)\n", mean(data$age, na.rm = TRUE), sd(data$age, na.rm = TRUE)))
cat(sprintf("  Median [IQR]:  %.0f [%.0f-%.0f]\n",
            median(data$age, na.rm = TRUE), quantile(data$age, 0.25, na.rm = TRUE), quantile(data$age, 0.75, na.rm = TRUE)))
cat(sprintf("  Range:         %d - %d\n", min(data$age, na.rm = TRUE), max(data$age, na.rm = TRUE)))
cat("\n")

print_freq(data, "agecat")

# Year
cat("-" |> rep(50) |> paste(collapse = ""), "\n")
cat("year (calendar year of diagnosis)\n")
cat("-" |> rep(50) |> paste(collapse = ""), "\n")
year_tbl <- table(data$year)
for (yr in names(year_tbl)) {
  cnt <- year_tbl[yr]
  pct <- 100 * cnt / n
  cat(sprintf("  %s %8s  (%5.1f%%)\n", yr, format(cnt, big.mark = ","), pct))
}
cat("\n")

# =============================================================================
# FAMILY HISTORY (parental diagnoses - count of parents)
# =============================================================================
cat("FAMILY HISTORY (parental diagnoses - count of parents)\n")
cat("-" |> rep(50) |> paste(collapse = ""), "\n")
cat("fh_suicidal - X60-X84, Y10-Y34: Suicidal behavior in parents\n")
cat("-" |> rep(50) |> paste(collapse = ""), "\n")
print_freq(data, "fh_suicidal", c("0" = "0 parents", "1" = "1 parent", "2" = "2 parents", "99" = "Missing"))
cat("-" |> rep(50) |> paste(collapse = ""), "\n")
cat("fh_depr - F32-F33: Depression in parents\n")
cat("-" |> rep(50) |> paste(collapse = ""), "\n")
print_freq(data, "fh_depr", c("0" = "0 parents", "1" = "1 parent", "2" = "2 parents", "99" = "Missing"))

# =============================================================================
# SOCIOECONOMIC
# =============================================================================
cat("SOCIOECONOMIC\n")
print_freq(data, "edufam_cat", c("0" = "Low", "1" = "Medium", "2" = "High", "99" = "Missing"))
print_freq(data, "inc_cat", c("1" = "<0", "2" = "0", "3" = "0-p20", "4" = "p20-p80", "5" = ">p80", "99" = "Missing"))

# =============================================================================
# HOSPITALIZATIONS
# =============================================================================
cat("HOSPITALIZATIONS\n")
print_freq(data, "hosp", c("0" = "No prior hospitalization", "1" = "Any prior hospitalization"))

# =============================================================================
# PRIOR PSYCHIATRIC DIAGNOSES - TABLE S3
# =============================================================================
cat("PRIOR PSYCHIATRIC DIAGNOSES - TABLE S3\n")
diag_vars_s3 <- c(
  "diag_mdd",
  "diag_adhd",
  "diag_stress",
  "diag_sud",
  "diag_suicidal",
  "diag_overdose",
  "diag_autism",
  "diag_anxiety_other",
  "diag_sleep",
  "diag_phobic",
  "diag_organic",
  "diag_anorexia",
  "diag_ocd",
  "diag_conduct",
  "diag_psychotic",
  "diag_intellectual_disability",
  "diag_bipolar",
  "diag_personality_cluster_b",
  "diag_bulimia"
)
diag_labels_s3 <- c(
  "diag_mdd"                     = "F32-F33: Major depressive disorder",
  "diag_adhd"                    = "F90: ADHD/Hyperkinetic disorder",
  "diag_stress"                  = "F43: Stress/adjustment disorders",
  "diag_sud"                     = "F10-F19 excl F17: Substance use disorder",
  "diag_suicidal"                = "X60-X84, Y10-Y34: Suicidal behavior",
  "diag_overdose"                = "T36-T51, X40-X49: Overdose/poisoning",
  "diag_autism"                  = "F84.0/1/5/8/9: Autism spectrum disorders",
  "diag_anxiety_other"           = "F41.0-F41.1: Panic/generalized anxiety",
  "diag_sleep"                   = "F51: Non-organic sleep disorders",
  "diag_phobic"                  = "F40.0-F40.2: Phobic anxiety disorders",
  "diag_organic"                 = "F00-F09: Organic mental disorders",
  "diag_anorexia"                = "F50.0-F50.1: Anorexia nervosa",
  "diag_ocd"                     = "F42: OCD",
  "diag_conduct"                 = "F91: Conduct disorders",
  "diag_psychotic"               = "F20-F29: Schizophrenia/psychotic disorders",
  "diag_intellectual_disability" = "F70-F79: Intellectual disability",
  "diag_bipolar"                 = "F30-F31: Bipolar/manic disorders",
  "diag_personality_cluster_b"   = "F60.2-F60.3: Cluster B personality disorders",
  "diag_bulimia"                 = "F50.2-F50.3: Bulimia nervosa"
)
print_binary_summary(data, diag_vars_s3, "Table S3 diagnoses (any time before follow-up)", diag_labels_s3)

# =============================================================================
# PRIOR MEDICATIONS (90 days before follow-up)
# =============================================================================
cat("PRIOR MEDICATIONS (90 days before follow-up)\n")

med_vars <- c("med_hypnotic", "med_stimulant", "med_antipsychotic", "med_benzodiazepine",
              "med_opioid", "med_antiepileptic", "med_mood_stabilizer", "med_addiction")
med_labels <- c(
  "med_hypnotic"        = "N05C: Hypnotics/sedatives",
  "med_stimulant"       = "N06B: Psychostimulants/ADHD meds",
  "med_antipsychotic"   = "N05A excl N05AN: Antipsychotics (excl lithium)",
  "med_benzodiazepine"  = "N05BA: Benzodiazepines",
  "med_opioid"          = "N02A: Opioids",
  "med_antiepileptic"   = "N03A excl mood stabilizers: Antiepileptics",
  "med_mood_stabilizer" = "N05AN, N03AG01, N03AX09, N03AF01: Mood stabilizers",
  "med_addiction"       = "N07B: Drugs for addictive disorders"
)
print_binary_summary(data, med_vars, "Medication use", med_labels)

# =============================================================================
# OUTCOMES
# =============================================================================
cat("OUTCOMES\n")
print_freq(data, "sb12_itt", c("0" = "No event", "1" = "Suicidal behavior"))

# =============================================================================
# INDEX DEPRESSION DIAGNOSIS (dia column)
# =============================================================================
cat("INDEX DEPRESSION DIAGNOSIS\n")
cat("-" |> rep(70) |> paste(collapse = ""), "\n")
cat("The F32/F33 diagnosis that qualified each individual for the cohort.\n\n")

# ICD-10 depression code descriptions
dia_labels <- c(
  "F32"   = "Depressive episode",
  "F32-"  = "Depressive episode (unspecified)",
  "F320"  = "Mild depressive episode",
  "F321"  = "Moderate depressive episode",
  "F3212" = "Moderate depressive episode (detailed)",
  "F322"  = "Severe depressive episode without psychosis",
  "F323"  = "Severe depressive episode with psychosis",
  "F323A" = "Severe depression with mood-congruent psychosis",
  "F323W" = "Severe depression with psychosis, other",
  "F328"  = "Other depressive episodes",
  "F329"  = "Depressive episode, unspecified",
  "F33"   = "Recurrent depressive disorder",
  "F330"  = "Recurrent depression: current episode mild",
  "F331"  = "Recurrent depression: current episode moderate",
  "F332"  = "Recurrent depression: current episode severe",
  "F333"  = "Recurrent depression: severe with psychosis",
  "F338"  = "Other recurrent depressive disorders",
  "F339"  = "Recurrent depressive disorder, unspecified"
)

dia_freq <- data %>%
  count(dia, name = "n_individuals") %>%
  mutate(pct = 100 * n_individuals / n) %>%
  arrange(desc(n_individuals))

cat(sprintf("  %-8s %8s  %6s  %s\n", "Code", "N", "%", "Description"))
cat(sprintf("  %-8s %8s  %6s  %s\n", "--------", "------", "----", paste(rep("-", 50), collapse="")))

for (i in seq_len(nrow(dia_freq))) {
  code <- dia_freq$dia[i]
  n_ind <- dia_freq$n_individuals[i]
  pct <- dia_freq$pct[i]
  desc <- if (code %in% names(dia_labels)) dia_labels[code] else ""
  cat(sprintf("  %-8s %8s  (%5.1f%%)  %s\n",
              code, format(n_ind, big.mark = ","), pct, desc))
}
cat("\n")

cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("END OF FREQUENCY TABLES\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n")

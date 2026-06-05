# cate_by_source.R
# Compute IPW-adjusted treatment effect of SSRI initiation on suicidal behavior,
# stratified by care setting (inpatient vs outpatient) at the index depression
# diagnosis. Addresses reviewer concern that inpatient patients (more severe
# depression) may have different treatment effects.
#
# Uses the same ITT methodology as ITT_12wks.R.

library(survival)
library(dplyr)
library(here)
here::i_am("suicidality/analysis/cate_by_source.R")

source(here("suicidality", "analysis", "common.R"))

# =============================================================================
# LOAD DATA
# =============================================================================

data_12 <- filter_complete_cases(
  as.data.frame(readRDS(here("suicidality", "extraction", "output", "rds", "main_12wks_28.rds")))
)

# =============================================================================
# PROPENSITY SCORE MODEL (same as ITT_12wks.R)
# =============================================================================

build_ps_formula <- function() {
  demo_vars <- "female + age + year"
  socio_vars <- paste(
    "relevel(as.factor(edufam_cat), ref='1')",
    "I(as.integer(source == 'S'))",
    "relevel(as.factor(inc_cat), ref='2')",
    sep = " + "
  )
  fh_vars <- paste(
    "relevel(as.factor(fh_suicidal), ref='0')",
    "relevel(as.factor(fh_depr), ref='0')",
    sep = " + "
  )
  hosp_vars <- "hosp"

  # Diagnosis + medication vars (same as main analysis)
  diag_vars <- paste(
    "diag_alcohol", "diag_sud", "diag_phobic", "diag_anxiety_other",
    "diag_stress", "diag_adhd", "diag_autism", "diag_ocd",
    "diag_conduct", "diag_suicidal", "diag_overdose",
    sep = " + "
  )
  med_vars <- paste(
    "med_antipsychotic", "med_hypnotic", "med_benzodiazepine",
    "med_antiepileptic", "med_stimulant", "med_opioid",
    sep = " + "
  )

  as.formula(paste("cc ~", demo_vars, "+", socio_vars, "+", fh_vars,
                   "+", hosp_vars, "+", diag_vars, "+", med_vars))
}

# =============================================================================
# ANALYSIS FUNCTION
# =============================================================================

analyze_stratum <- function(data, stratum_label, outcome_col, fu_end_col,
                            weeks = 12) {
  cat(sprintf("\n--- %s (n=%d) ---\n", stratum_label, nrow(data)))

  data$t_end <- ceiling((data[[fu_end_col]] - data$fu_start) / 7)
  data$outcome <- data[[outcome_col]]

  # If no variation in the binary source indicator within stratum, drop it
  # from the PS formula.
  has_source_var <- length(unique(as.integer(data$source == "S"))) > 1

  # Fit PS without 'source' if stratifying on it
  if (!has_source_var) {
    ps_formula <- as.formula(
      "cc ~ female + age + year + " %+%
      "relevel(as.factor(edufam_cat), ref='1') + " %+%
      "relevel(as.factor(inc_cat), ref='2') + " %+%
      "relevel(as.factor(fh_suicidal), ref='0') + " %+%
      "relevel(as.factor(fh_depr), ref='0') + hosp + " %+%
      "diag_alcohol + diag_sud + diag_phobic + diag_anxiety_other + " %+%
      "diag_stress + diag_adhd + diag_autism + diag_ocd + " %+%
      "diag_conduct + diag_suicidal + diag_overdose + " %+%
      "med_antipsychotic + med_hypnotic + med_benzodiazepine + " %+%
      "med_antiepileptic + med_stimulant + med_opioid"
    )
  } else {
    ps_formula <- build_ps_formula()
  }

  # Fit PS
  p.denom <- glm(ps_formula, data = data, family = binomial())
  data$pd.cc <- predict(p.denom, type = "response")
  p.num <- glm(cc ~ 1, data = data, family = binomial())
  data$pn.cc <- predict(p.num, type = "response")

  # Stabilized weights, truncated at 99th percentile
  data$sw.a <- ifelse(data$cc == 1,
                      data$pn.cc / data$pd.cc,
                      (1 - data$pn.cc) / (1 - data$pd.cc))
  q99 <- quantile(data$sw.a, 0.99, na.rm = TRUE)
  data$sw.a <- pmin(data$sw.a, q99)

  # Weighted Kaplan-Meier
  km_fit <- survfit(Surv(t_end, outcome) ~ cc, weights = sw.a,
                    cluster = lopnr, data = data)

  surv_t <- summary(km_fit, times = weeks)
  control_idx <- which(surv_t$strata == "cc=0")
  treated_idx <- which(surv_t$strata == "cc=1")

  risk_control <- (1 - surv_t$surv[control_idx]) * 100
  risk_treated <- (1 - surv_t$surv[treated_idx]) * 100
  se_control <- surv_t$std.err[control_idx] * 100
  se_treated <- surv_t$std.err[treated_idx] * 100

  rd <- risk_treated - risk_control
  rd_se <- sqrt(se_control^2 + se_treated^2)
  rd_lower <- rd - 1.96 * rd_se
  rd_upper <- rd + 1.96 * rd_se

  # Risk Ratio (delta method on log scale)
  if (risk_control > 0 && risk_treated > 0) {
    rr <- risk_treated / risk_control
    log_rr_se <- sqrt((se_treated / risk_treated)^2 + (se_control / risk_control)^2)
    rr_lower <- exp(log(rr) - 1.96 * log_rr_se)
    rr_upper <- exp(log(rr) + 1.96 * log_rr_se)
  } else {
    rr <- NA_real_
    rr_lower <- NA_real_
    rr_upper <- NA_real_
  }

  n_treated <- sum(data$cc == 1)
  n_control <- sum(data$cc == 0)
  events_treated <- sum(data$cc == 1 & data$outcome == 1)
  events_control <- sum(data$cc == 0 & data$outcome == 1)

  cat(sprintf("  N: %d treated, %d control\n", n_treated, n_control))
  cat(sprintf("  Events: %d treated, %d control\n", events_treated, events_control))
  cat(sprintf("  Risk: %.2f%% treated vs %.2f%% control\n", risk_treated, risk_control))
  cat(sprintf("  RD: %+.2f pp (95%% CI: %.2f, %.2f)\n", rd, rd_lower, rd_upper))
  cat(sprintf("  RR: %.2f (95%% CI: %.2f, %.2f)\n", rr, rr_lower, rr_upper))

  data.frame(
    stratum = stratum_label,
    weeks = weeks,
    n_treated = n_treated,
    n_control = n_control,
    events_treated = events_treated,
    events_control = events_control,
    risk_treated = risk_treated,
    risk_control = risk_control,
    rd = rd,
    rd_lower = rd_lower,
    rd_upper = rd_upper,
    rr = rr,
    rr_lower = rr_lower,
    rr_upper = rr_upper
  )
}

`%+%` <- function(a, b) paste0(a, b)

# =============================================================================
# RUN BY SOURCE
# =============================================================================

results <- list()

cat("\n============================================================\n")
cat("12-WEEK ANALYSIS\n")
cat("============================================================\n")

# Per-stratum diagnostic analyses (kept for cate_by_source.csv only).
# Source is treated as binary: non-inpatient (O or T) vs. inpatient (S).
for (src in c("non_inpatient", "S")) {
  label <- if (src == "S") "Inpatient" else "Non-inpatient"
  sub <- if (src == "S") {
    data_12 %>% dplyr::filter(source == "S")
  } else {
    data_12 %>% dplyr::filter(source != "S")
  }
  results[[paste0(label, "_12")]] <- analyze_stratum(
    sub, paste(label, "(12 weeks)"),
    outcome_col = "sb12_itt", fu_end_col = "fu_end_itt", weeks = 12
  )
}

# Sensitivity analysis: main ITT on cohort excluding inpatient diagnoses.
# Includes outpatient (source = "O") and primary care (source = "T").
cat("\n--- Sensitivity: Excluding inpatient diagnoses (12 weeks) ---\n")
sub_excl_inp <- data_12 %>% dplyr::filter(source != "S")
results[["ExclInpatients_12"]] <- analyze_stratum(
  sub_excl_inp, "Excluding inpatients (12 weeks)",
  outcome_col = "sb12_itt", fu_end_col = "fu_end_itt", weeks = 12
)

# Sensitivity analysis: main ITT on a cohort rebuilt with a 14-day grace
# period (vs. primary 28-day). main_12wks_14.rds is produced by
# extraction/build_grace14_cohort.R: re-runs the extraction with
# predi_diff <= 14 as initiator, frequency-matches non-initiator fu_start
# to the 14-day initiator distribution, and re-derives the fu_start-dependent
# covariates. Cohort size is essentially identical to the 28-day cohort
# (the death/emigration eligibility filter spans 14 vs. 28 days; a handful
# of patients flip in/out depending on the random fu_start assignment).
cat("\n--- Sensitivity: 14-day grace period (12 weeks) ---\n")
sub_grace14 <- filter_complete_cases(
  as.data.frame(readRDS(here("suicidality", "extraction", "output", "rds", "main_12wks_14.rds")))
)
results[["Grace14_12"]] <- analyze_stratum(
  sub_grace14, "14-day grace period (12 weeks)",
  outcome_col = "sb12_itt", fu_end_col = "fu_end_itt", weeks = 12
)

# =============================================================================
# SAVE RESULTS
# =============================================================================

all_results <- do.call(rbind, results)
rownames(all_results) <- NULL

output_dir <- here("suicidality", "analysis", "output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

write.csv(all_results, file.path(output_dir, "cate_by_source.csv"), row.names = FALSE)
cat("\nSaved: cate_by_source.csv\n")

cat("\n=== SUMMARY TABLE ===\n")
print(all_results)

# =============================================================================
# GENERATE LATEX MACROS
# =============================================================================

format_n <- function(n) format(n, big.mark = "{,}", scientific = FALSE)
format_rd <- function(x) if (x >= 0) sprintf("+%.2f", x) else sprintf("%.2f", x)
format_ci <- function(x) sprintf("%.2f", x)

lines <- c(
  "% Auto-generated by cate_by_source.R -- do not edit manually",
  "% CATE stratified by care setting (inpatient vs outpatient)",
  ""
)

for (i in seq_len(nrow(all_results))) {
  r <- all_results[i, ]

  if (grepl("Excluding inpatients", r$stratum)) {
    # Sensitivity analysis: cohort with inpatient index diagnoses excluded.
    prefix <- "sensExclInpTwelve"
  } else if (grepl("14-day grace", r$stratum)) {
    # Sensitivity analysis: 14-day grace period (vs main 28-day).
    prefix <- "sensGraceFourteenTwelve"
  } else {
    setting <- if (grepl("Non-inpatient", r$stratum)) "NonInp" else "Inp"
    prefix <- paste0("sbs", setting, "Twelve")  # e.g., sbsNonInpTwelve / sbsInpTwelve
  }

  lines <- c(lines,
    sprintf("\\newcommand{\\%sN}{%s}", prefix, format_n(r$n_treated + r$n_control)),
    sprintf("\\newcommand{\\%sNTreated}{%s}", prefix, format_n(r$n_treated)),
    sprintf("\\newcommand{\\%sNControl}{%s}", prefix, format_n(r$n_control)),
    sprintf("\\newcommand{\\%sEvents}{%s}", prefix, format_n(r$events_treated + r$events_control)),
    sprintf("\\newcommand{\\%sEventsTreated}{%s}", prefix, format_n(r$events_treated)),
    sprintf("\\newcommand{\\%sEventsControl}{%s}", prefix, format_n(r$events_control)),
    sprintf("\\newcommand{\\%sRiskTreated}{%.2f}", prefix, r$risk_treated),
    sprintf("\\newcommand{\\%sRiskControl}{%.2f}", prefix, r$risk_control),
    sprintf("\\newcommand{\\%sRD}{%s}", prefix, format_rd(r$rd)),
    sprintf("\\newcommand{\\%sRDLower}{%s}", prefix, format_ci(r$rd_lower)),
    sprintf("\\newcommand{\\%sRDUpper}{%s}", prefix, format_ci(r$rd_upper))
  )

  # Add RR macros if available (NA-safe)
  if (!is.na(r$rr)) {
    lines <- c(lines,
      sprintf("\\newcommand{\\%sRR}{%.2f}", prefix, r$rr),
      sprintf("\\newcommand{\\%sRRLower}{%s}", prefix, format_ci(r$rr_lower)),
      sprintf("\\newcommand{\\%sRRUpper}{%s}", prefix, format_ci(r$rr_upper))
    )
  }
}

writeLines(lines, file.path(output_dir, "cate_by_source_values.tex"))
cat("Saved: cate_by_source_values.tex\n")

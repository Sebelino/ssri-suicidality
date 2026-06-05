# ITT_12wks.R
# Intention-to-treat analysis at 12 weeks follow-up
# Uses inverse probability weighting with ALL available covariates

library(survival)
library(dplyr)
library(ggplot2)
library(survminer)
library(here)
here::i_am("suicidality/analysis/ITT_12wks.R")

source(here("suicidality", "analysis", "common.R"))

# =============================================================================
# PROPENSITY SCORE MODEL SPECIFICATION
# =============================================================================
# All available covariates from the extraction pipeline

build_ps_formula <- function(include_female = TRUE) {
  # Demographics. include_female should be FALSE when fitting within a single
  # sex stratum (e.g., the female-only or male-only analysis), where `female`
  # is constant and would be dropped by glm() anyway.
  demo_vars <- if (include_female) "female + age + year" else "age + year"

  # Socioeconomic (categorical; complete-case cohort has no sentinel-99 levels).
  # Reference levels are the modal category for each variable so coefficients
  # are interpreted relative to the most common patient profile and SEs are
  # tighter than they would be against a small reference category.
  # source: outpatient (O) and other/unknown (T) are merged as a single
  # "non-inpatient" category, contrasted against inpatient (S). T is small
  # (~0.4% of cohort) and behaves like O for modelling purposes.
  socio_vars <- paste(
    "relevel(as.factor(edufam_cat), ref='1')",   # secondary, modal (~44% of cohort)
    "I(as.integer(source == 'S'))",              # 1 = inpatient, 0 = outpatient or other/unknown
    "relevel(as.factor(inc_cat), ref='4')",      # quintile 4, modal (~57%)
    sep = " + "
  )

  # Family history (categorical; complete-case cohort)
  fh_vars <- paste(
    "relevel(as.factor(fh_suicidal), ref='0')",
    "relevel(as.factor(fh_depr), ref='0')",
    sep = " + "
  )

  # Hospitalization
  hosp_vars <- "hosp"

  # All diagnosis covariates.
  # Note: diag_anxiety is a combined variable (diag_phobic | diag_anxiety_other)
  # created in analyze_cohort. diag_mdd is intentionally omitted because it is
  # constant=1 in this cohort by construction (every patient has F32/F33 by
  # cohort entry), so glm() would drop it as a singular column anyway.
  # diag_organic (F00-F09) is dropped: zero observed cases in this 6-24 cohort
  # (the only F0x records in the registry are chapter-range placeholders that
  # extraction/19_process_cov_diagnoses.R filters out). A zero-variance
  # covariate contributes an NA coefficient and no predicted-probability
  # signal; dropping it removes a benign warning from glm().
  diag_vars <- paste(
    "diag_bipolar", "diag_psychotic", "diag_alcohol", "diag_sud",
    "diag_autism", "diag_adhd", "diag_suicidal", "diag_overdose",
    "diag_stress", "diag_anxiety",
    "diag_sleep", "diag_anorexia", "diag_bulimia",
    "diag_ocd", "diag_conduct", "diag_intellectual_disability", "diag_personality_cluster_b",
    sep = " + "
  )

  # All medication covariates
  med_vars <- paste(
    "med_antipsychotic", "med_hypnotic", "med_benzodiazepine", "med_antiepileptic",
    "med_stimulant", "med_opioid", "med_mood_stabilizer", "med_addiction",
    sep = " + "
  )

  # Build formula
  formula_str <- paste("cc ~", demo_vars, "+", socio_vars, "+", fh_vars, "+",
                       hosp_vars, "+", diag_vars, "+", med_vars)
  as.formula(formula_str)
}

# =============================================================================
# ANALYSIS FUNCTION
# =============================================================================

analyze_cohort <- function(data, label, include_female = TRUE) {
  # Compute follow-up time in weeks
  data$t_end <- ceiling((data$fu_end_itt - data$fu_start) / 7)

  # Create combined anxiety variable to match paper's single "Anxiety disorder diagnosis"
  # Paper combines anxiety disorders; we have diag_phobic (F40) and diag_anxiety_other (F41)
  data$diag_anxiety <- as.integer(data$diag_phobic == 1 | data$diag_anxiety_other == 1)

  # Fit propensity score model (denominator)
  ps_formula <- build_ps_formula(include_female)
  p.denom <- glm(ps_formula, data = data, family = binomial())
  data$pd.cc <- predict(p.denom, type = "response")

  # Numerator (marginal probability of treatment)
  p.num <- glm(cc ~ 1, data = data, family = binomial())
  data$pn.cc <- predict(p.num, type = "response")

  # Compute stabilized weights
  data$sw.a <- ifelse(data$cc == 1,
                      data$pn.cc / data$pd.cc,
                      (1 - data$pn.cc) / (1 - data$pd.cc))

  # Truncate at 99th percentile
  q99 <- quantile(data$sw.a, 0.99, na.rm = TRUE)
  data$sw.a <- pmin(data$sw.a, q99)

  # Fit weighted Kaplan-Meier
  km_fit <- survfit(Surv(t_end, sb12_itt) ~ cc, weights = sw.a, cluster = lopnr, data = data)

  # Get survival estimates at 12 weeks
  surv_12 <- summary(km_fit, times = 12)

  # Extract risks (1 - survival) using strata names for robustness
  # Strata are named "cc=0" (control) and "cc=1" (treated)
  control_idx <- which(surv_12$strata == "cc=0")
  treated_idx <- which(surv_12$strata == "cc=1")

  risk_control <- (1 - surv_12$surv[control_idx]) * 100
  risk_treated <- (1 - surv_12$surv[treated_idx]) * 100
  se_control <- surv_12$std.err[control_idx] * 100
  se_treated <- surv_12$std.err[treated_idx] * 100

  # Risk Difference
  rd <- risk_treated - risk_control
  rd_se <- sqrt(se_control^2 + se_treated^2)
  rd_lower <- rd - 1.96 * rd_se
  rd_upper <- rd + 1.96 * rd_se

  # Risk Ratio (delta method for CI)
  # Defensive check: avoid division by zero and log(0)
  if (risk_control > 0 && risk_treated > 0) {
    rr <- risk_treated / risk_control
    log_rr_se <- sqrt((se_treated / risk_treated)^2 + (se_control / risk_control)^2)
    rr_lower <- exp(log(rr) - 1.96 * log_rr_se)
    rr_upper <- exp(log(rr) + 1.96 * log_rr_se)
  } else {
    warning("Zero risk in one group - risk ratio undefined")
    rr <- NA_real_
    rr_lower <- NA_real_
    rr_upper <- NA_real_
  }

  # Sample sizes
  n_total <- nrow(data)
  n_treated <- sum(data$cc == 1, na.rm = TRUE)
  n_control <- sum(data$cc == 0, na.rm = TRUE)
  events_treated <- sum(data$cc == 1 & data$sb12_itt == 1, na.rm = TRUE)
  events_control <- sum(data$cc == 0 & data$sb12_itt == 1, na.rm = TRUE)

  # Print results
  cat("\n")
  cat("============================================================\n")
  cat(sprintf("Age group: %s\n", label))
  cat("============================================================\n")
  cat(sprintf("N total: %d (Treated: %d, Control: %d)\n", n_total, n_treated, n_control))
  cat(sprintf("Events: %d (Treated: %d, Control: %d)\n", events_treated + events_control, events_treated, events_control))
  cat("\n")
  cat(sprintf("Risk at 12 weeks (Control): %.2f%% (95%% CI: %.2f%%, %.2f%%)\n",
              risk_control, risk_control - 1.96 * se_control, risk_control + 1.96 * se_control))
  cat(sprintf("Risk at 12 weeks (Treated): %.2f%% (95%% CI: %.2f%%, %.2f%%)\n",
              risk_treated, risk_treated - 1.96 * se_treated, risk_treated + 1.96 * se_treated))
  cat("\n")
  cat(sprintf("Risk Difference: %.2f%% (95%% CI: %.2f%%, %.2f%%)\n", rd, rd_lower, rd_upper))
  cat(sprintf("Risk Ratio:      %.2f (95%% CI: %.2f, %.2f)\n", rr, rr_lower, rr_upper))
  cat("============================================================\n")

  # Return results
  invisible(list(
    label = label,
    n_total = n_total,
    n_treated = n_treated,
    n_control = n_control,
    events_treated = events_treated,
    events_control = events_control,
    risk_control = risk_control,
    risk_treated = risk_treated,
    se_control = se_control,
    se_treated = se_treated,
    rd = rd,
    rd_lower = rd_lower,
    rd_upper = rd_upper,
    rr = rr,
    rr_lower = rr_lower,
    rr_upper = rr_upper,
    km_fit = km_fit,
    data = data
  ))
}

# =============================================================================
# MAIN ANALYSIS
# =============================================================================

cat("\n")
cat("############################################################\n")
cat("# ITT Analysis at 12 Weeks - Risk Difference and Risk Ratio\n")
cat("############################################################\n")

# Load data (complete-case analysis: drop sentinel-99 rows for parental
# education, family income, and the two family-history variables).
full_data <- filter_complete_cases(read_rds_file("main_12wks_28.rds"))

# Main analyses by age group
results_6_24 <- analyze_cohort(full_data, "6-24 (Full cohort)")
results_6_17 <- analyze_cohort(full_data[full_data$age >= 6 & full_data$age <= 17, ], "6-17 (Children/Adolescents)")
results_18_24 <- analyze_cohort(full_data[full_data$age >= 18 & full_data$age <= 24, ], "18-24 (Young adults)")

# Summary table
cat("\n")
cat("############################################################\n")
cat("# Summary Table\n")
cat("############################################################\n\n")

cat(sprintf("%-25s %10s %20s %20s\n", "Age Group", "N", "RD (95% CI)", "RR (95% CI)"))
cat(sprintf("%-25s %10s %20s %20s\n", "---------", "--", "-----------", "-----------"))

for (r in list(results_6_24, results_6_17, results_18_24)) {
  cat(sprintf("%-25s %10d  %5.2f (%5.2f, %5.2f)  %5.2f (%4.2f, %4.2f)\n",
              r$label, r$n_total,
              r$rd, r$rd_lower, r$rd_upper,
              r$rr, r$rr_lower, r$rr_upper))
}

# Comparison with Lagerberg et al. 2023
cat("\n")
cat("############################################################\n")
cat("# Comparison with Lagerberg et al. 2023 (ITT, 12 weeks)\n")
cat("############################################################\n\n")
cat("Lagerberg et al. 2023 used ages 6-59 with N=162,267\n")
cat("Current analysis uses ages 6-24\n\n")
cat(sprintf("%-12s | %-30s | %-30s\n", "Age Group", "Current Analysis", "Lagerberg et al. 2023"))
cat(sprintf("%-12s | %-30s | %-30s\n", "--------", "----------------", "---------------------"))
cat(sprintf("%-12s | RD=%5.2f%% RR=%4.2f (%4.2f-%4.2f) | RD=%5.2f%% RR=%4.2f (%4.2f-%4.2f)\n",
            "6-17 years", results_6_17$rd, results_6_17$rr, results_6_17$rr_lower, results_6_17$rr_upper,
            1.48, 2.90, 1.72, 4.91))
cat(sprintf("%-12s | RD=%5.2f%% RR=%4.2f (%4.2f-%4.2f) | RD=%5.2f%% RR=%4.2f (%4.2f-%4.2f)\n",
            "18-24 years", results_18_24$rd, results_18_24$rr, results_18_24$rr_lower, results_18_24$rr_upper,
            0.27, 1.59, 1.11, 2.28))

# =============================================================================
# COX MODEL (Hazard Ratio)
# =============================================================================

cat("\n")
cat("############################################################\n")
cat("# Cox Proportional Hazards Model\n")
cat("############################################################\n\n")

cox <- coxph(Surv(results_6_24$data$t_end, results_6_24$data$sb12_itt) ~ cc,
             cluster = lopnr, data = results_6_24$data, weights = results_6_24$data$sw.a)
print(summary(cox))

# =============================================================================
# SURVIVAL PLOTS
# =============================================================================

cat("\n")
cat("############################################################\n")
cat("# Generating Survival Plots\n")
cat("############################################################\n")

# Main survival plot (full cohort)
p_main <- suppressWarnings(ggsurvplot(
  results_6_24$km_fit,
  data = results_6_24$data,
  conf.int = TRUE,
  conf.int.alpha = 0.25,
  ylim = c(0, 0.04),
  risk.table = TRUE,
  risk.table.title = "No. at risk",
  risk.table.fontsize = 3,
  risk.table.height = 0.15,
  tables.y.text.col = FALSE,
  tables.theme = theme_cleantable(),
  xlab = "Time in weeks",
  ylab = "Risk of suicidal behavior",
  font.x = c(11, "plain", "black"),
  font.y = c(11, "plain", "black"),
  font.tickslab = c(10, "plain", "black"),
  legend = "right",
  legend.title = "",
  legend.labs = c("Non-initiators", "SSRI initiators"),
  font.legend = c(10, "plain", "black"),
  fun = "event"
))

p_main$table <- p_main$table +
  theme(plot.title = element_text(size = 10, color = "black", face = "plain", hjust = -0.12),
        axis.text.y = element_text(size = 10))

suppressMessages(suppressWarnings(print(p_main)))
pdf(output_path("survplot_itt_main.pdf"))
suppressMessages(suppressWarnings(print(p_main, newpage = FALSE)))
dev.off()
cat("Generated: survplot_itt_main.pdf\n")

# Age-stratified plots
create_age_plot <- function(result, title) {
  suppressWarnings(ggsurvplot(
    result$km_fit,
    data = result$data,
    conf.int = TRUE,
    conf.int.alpha = 0.25,
    ylim = c(0, 0.04),
    risk.table = TRUE,
    risk.table.title = "No. at risk",
    risk.table.fontsize = 3,
    risk.table.height = 0.15,
    tables.y.text.col = FALSE,
    tables.theme = theme_cleantable(),
    title = title,
    xlab = "Time in weeks",
    ylab = "Risk of suicidal behavior",
    font.x = c(11, "plain", "black"),
    font.y = c(11, "plain", "black"),
    font.tickslab = c(10, "plain", "black"),
    legend = "right",
    legend.title = "",
    legend.labs = c("Non-initiators", "SSRI initiators"),
    font.legend = c(10, "plain", "black"),
    font.main = c(11, "bold", "black"),
    fun = "event"
  ))
}

plot_6_17 <- create_age_plot(results_6_17, "Age 6-17 years")
plot_18_24 <- create_age_plot(results_18_24, "Age 18-24 years")

plot_6_17$table <- plot_6_17$table +
  theme(plot.title = element_text(size = 10, color = "black", face = "plain", hjust = -0.12),
        axis.text.y = element_text(size = 10))
plot_18_24$table <- plot_18_24$table +
  theme(plot.title = element_text(size = 10, color = "black", face = "plain", hjust = -0.12),
        axis.text.y = element_text(size = 10))

age_strata_combined <- suppressMessages(suppressWarnings(arrange_ggsurvplots(
  list(plot_6_17, plot_18_24),
  print = FALSE,
  ncol = 1, nrow = 2
)))

suppressMessages(suppressWarnings(ggsave(
  output_path("survplot_age_strata.pdf"),
  age_strata_combined,
  width = 18, height = 28, units = "cm"
)))
cat("Generated: survplot_age_strata.pdf\n")

# =============================================================================
# SEX-STRATIFIED ANALYSIS
# =============================================================================

cat("\n")
cat("############################################################\n")
cat("# Sex-Stratified Analysis\n")
cat("############################################################\n")

results_female <- analyze_cohort(full_data[full_data$female == 1, ], "Female", include_female = FALSE)
results_male <- analyze_cohort(full_data[full_data$female == 0, ], "Male", include_female = FALSE)

# =============================================================================
# SENSITIVITY ANALYSES
# =============================================================================

cat("\n")
cat("############################################################\n")
cat("# Sensitivity Analysis: Exclude source='M'\n")
cat("############################################################\n")

data_no_M <- full_data[full_data$source != "M", ]
results_no_M <- analyze_cohort(data_no_M, "Excluding source=M")

cat("\n")
cat("############################################################\n")
cat("# Sensitivity Analysis: Exclude antipsychotic/antiepileptic users\n")
cat("############################################################\n")

data_no_psych <- full_data[full_data$med_antipsychotic != 1 & full_data$med_antiepileptic != 1, ]
results_no_psych <- analyze_cohort(data_no_psych, "Excluding antipsychotic/antiepileptic")

cat("\n")
cat("############################################################\n")
cat("# Analysis Complete\n")
cat("############################################################\n")

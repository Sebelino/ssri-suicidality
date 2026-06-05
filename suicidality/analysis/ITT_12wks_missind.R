# ITT_12wks_missind.R
# Sensitivity analysis: missing-indicator method for the four partially-observed
# covariates (edufam_cat, inc_cat, fh_suicidal, fh_depr). The headline ITT_12wks
# analysis is complete-case; this script re-runs the 12-week ITT on the FULL
# eligible cohort (no CCA filter) using a SINGLE binary indicator (`any_miss`)
# for missingness in at least one of the four covariates. The four covariates
# themselves are imputed to a fixed reference value when missing:
#   * edufam_cat: 99 -> 1 (modal level, secondary education)
#   * inc_cat:    99 -> 4 (modal level, 20th--80th income percentile)
#   * fh_suicidal: 99 -> 0
#   * fh_depr:     99 -> 0
# The PS model is the same as the headline ITT specification plus `any_miss`.
#
# Output: stdout summary + output/missind_values.tex with \sensMissInd* macros.

library(survival)
library(dplyr)
library(here)
here::i_am("suicidality/analysis/ITT_12wks_missind.R")

source(here("suicidality", "analysis", "common.R"))

# =============================================================================
# RECODE COVARIATES FOR MISSING-INDICATOR METHOD
# =============================================================================

prepare_missind <- function(df) {
  # Single binary indicator: 1 if any of the four covariates is missing.
  df$any_miss <- as.integer(df$edufam_cat == 99 |
                            df$inc_cat    == 99 |
                            df$fh_suicidal == 99 |
                            df$fh_depr    == 99)

  # When any_miss == 1, force ALL four covariates to their modal/reference
  # level (not just the actually-missing ones). This way the `any_miss`
  # indicator is a clean "missing-stratum" flag whose patients sit at the
  # reference level for all four covariates, and the indicator coefficient
  # absorbs everything those patients contribute through these four covariates.
  # The trade-off is that observed non-reference values among the missing-
  # stratum patients are discarded.
  df$edufam_cat  <- ifelse(df$any_miss == 1, 1, df$edufam_cat)
  df$inc_cat     <- ifelse(df$any_miss == 1, 4, df$inc_cat)
  df$fh_suicidal <- ifelse(df$any_miss == 1, 0, df$fh_suicidal)
  df$fh_depr     <- ifelse(df$any_miss == 1, 0, df$fh_depr)
  df
}

# =============================================================================
# PROPENSITY SCORE MODEL (mirrors ITT_12wks.R + missing indicators)
# =============================================================================

build_ps_formula_missind <- function() {
  demo_vars  <- "female + age + year"
  socio_vars <- paste(
    "relevel(as.factor(edufam_cat), ref='1')",
    "I(as.integer(source == 'S'))",
    "relevel(as.factor(inc_cat), ref='4')",
    sep = " + "
  )
  fh_vars <- paste(
    "relevel(as.factor(fh_suicidal), ref='0')",
    "relevel(as.factor(fh_depr), ref='0')",
    sep = " + "
  )
  miss_vars <- "any_miss"
  hosp_vars <- "hosp"
  diag_vars <- paste(
    "diag_bipolar", "diag_psychotic", "diag_alcohol", "diag_sud",
    "diag_autism", "diag_adhd", "diag_suicidal", "diag_overdose",
    "diag_stress", "diag_anxiety",
    "diag_sleep", "diag_anorexia", "diag_bulimia",
    "diag_ocd", "diag_conduct", "diag_intellectual_disability",
    "diag_personality_cluster_b",
    sep = " + "
  )
  med_vars <- paste(
    "med_antipsychotic", "med_hypnotic", "med_benzodiazepine",
    "med_antiepileptic", "med_stimulant", "med_opioid",
    "med_mood_stabilizer", "med_addiction",
    sep = " + "
  )
  as.formula(paste("cc ~", demo_vars, "+", socio_vars, "+", fh_vars, "+",
                   miss_vars, "+", hosp_vars, "+", diag_vars, "+", med_vars))
}

# =============================================================================
# ANALYSIS
# =============================================================================

analyze_missind <- function(data) {
  data$t_end <- ceiling((data$fu_end_itt - data$fu_start) / 7)
  data$diag_anxiety <- as.integer(data$diag_phobic == 1 | data$diag_anxiety_other == 1)

  p.denom <- glm(build_ps_formula_missind(), data = data, family = binomial())
  data$pd.cc <- predict(p.denom, type = "response")
  p.num <- glm(cc ~ 1, data = data, family = binomial())
  data$pn.cc <- predict(p.num, type = "response")

  data$sw.a <- ifelse(data$cc == 1,
                      data$pn.cc / data$pd.cc,
                      (1 - data$pn.cc) / (1 - data$pd.cc))
  q99 <- quantile(data$sw.a, 0.99, na.rm = TRUE)
  data$sw.a <- pmin(data$sw.a, q99)

  km_fit <- survfit(Surv(t_end, sb12_itt) ~ cc, weights = sw.a,
                    cluster = lopnr, data = data)
  surv_12 <- summary(km_fit, times = 12)

  control_idx <- which(surv_12$strata == "cc=0")
  treated_idx <- which(surv_12$strata == "cc=1")

  risk_control <- (1 - surv_12$surv[control_idx]) * 100
  risk_treated <- (1 - surv_12$surv[treated_idx]) * 100
  se_control   <- surv_12$std.err[control_idx] * 100
  se_treated   <- surv_12$std.err[treated_idx] * 100

  rd       <- risk_treated - risk_control
  rd_se    <- sqrt(se_control^2 + se_treated^2)
  rd_lower <- rd - 1.96 * rd_se
  rd_upper <- rd + 1.96 * rd_se

  rr       <- risk_treated / risk_control
  log_rr_se <- sqrt((se_treated / risk_treated)^2 + (se_control / risk_control)^2)
  rr_lower <- exp(log(rr) - 1.96 * log_rr_se)
  rr_upper <- exp(log(rr) + 1.96 * log_rr_se)

  list(
    n_total        = nrow(data),
    n_treated      = sum(data$cc == 1),
    n_control      = sum(data$cc == 0),
    events_treated = sum(data$cc == 1 & data$sb12_itt == 1),
    events_control = sum(data$cc == 0 & data$sb12_itt == 1),
    risk_control   = risk_control,
    risk_treated   = risk_treated,
    rd = rd, rd_lower = rd_lower, rd_upper = rd_upper,
    rr = rr, rr_lower = rr_lower, rr_upper = rr_upper
  )
}

# =============================================================================
# MAIN
# =============================================================================

cat("############################################################\n")
cat("# ITT Sensitivity Analysis: Missing-Indicator Method\n")
cat("############################################################\n\n")

# Full eligible cohort -- do NOT apply filter_complete_cases().
full_data <- read_rds_file("main_12wks_28.rds")
cat(sprintf("Full eligible cohort: N = %d\n", nrow(full_data)))

n_any_miss <- sum(full_data$edufam_cat == 99 |
                  full_data$inc_cat    == 99 |
                  full_data$fh_suicidal == 99 |
                  full_data$fh_depr    == 99)
cat(sprintf("Patients with at least one sentinel-99: %d (%.2f%%)\n",
            n_any_miss, 100 * n_any_miss / nrow(full_data)))

data_mi <- prepare_missind(full_data)
res <- analyze_missind(data_mi)

cat("\n------------------------------------------------------------\n")
cat(sprintf("N total: %d (Treated: %d, Control: %d)\n",
            res$n_total, res$n_treated, res$n_control))
cat(sprintf("Events: %d (Treated: %d, Control: %d)\n",
            res$events_treated + res$events_control,
            res$events_treated, res$events_control))
cat(sprintf("Risk (Control): %.2f%% | Risk (Treated): %.2f%%\n",
            res$risk_control, res$risk_treated))
cat(sprintf("RD: %.2f pp (95%% CI: %.2f, %.2f)\n",
            res$rd, res$rd_lower, res$rd_upper))
cat(sprintf("RR: %.2f      (95%% CI: %.2f, %.2f)\n",
            res$rr, res$rr_lower, res$rr_upper))
cat("------------------------------------------------------------\n")

# =============================================================================
# EXPORT LATEX MACROS
# =============================================================================

output_dir <- here("suicidality", "analysis", "output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

format_n  <- function(n) format(n, big.mark = "{,}", scientific = FALSE)
format_rd <- function(x) if (x >= 0) sprintf("+%.2f", x) else sprintf("%.2f", x)
format_ci <- function(x) sprintf("%.2f", x)

prefix <- "sensMissInd"
lines <- c(
  "% Auto-generated by ITT_12wks_missind.R -- do not edit manually",
  "% Missing-indicator sensitivity analysis (full eligible cohort)",
  "",
  sprintf("\\newcommand{\\%sN}{%s}",              prefix, format_n(res$n_total)),
  sprintf("\\newcommand{\\%sNTreated}{%s}",       prefix, format_n(res$n_treated)),
  sprintf("\\newcommand{\\%sNControl}{%s}",       prefix, format_n(res$n_control)),
  sprintf("\\newcommand{\\%sEvents}{%s}",         prefix, format_n(res$events_treated + res$events_control)),
  sprintf("\\newcommand{\\%sEventsTreated}{%s}",  prefix, format_n(res$events_treated)),
  sprintf("\\newcommand{\\%sEventsControl}{%s}",  prefix, format_n(res$events_control)),
  sprintf("\\newcommand{\\%sRiskTreated}{%.2f}",  prefix, res$risk_treated),
  sprintf("\\newcommand{\\%sRiskControl}{%.2f}",  prefix, res$risk_control),
  sprintf("\\newcommand{\\%sRD}{%s}",             prefix, format_rd(res$rd)),
  sprintf("\\newcommand{\\%sRDLower}{%s}",        prefix, format_ci(res$rd_lower)),
  sprintf("\\newcommand{\\%sRDUpper}{%s}",        prefix, format_ci(res$rd_upper)),
  sprintf("\\newcommand{\\%sRR}{%.2f}",           prefix, res$rr),
  sprintf("\\newcommand{\\%sRRLower}{%s}",        prefix, format_ci(res$rr_lower)),
  sprintf("\\newcommand{\\%sRRUpper}{%s}",        prefix, format_ci(res$rr_upper))
)

writeLines(lines, file.path(output_dir, "missind_values.tex"))
cat(sprintf("\nSaved: %s\n", file.path(output_dir, "missind_values.tex")))

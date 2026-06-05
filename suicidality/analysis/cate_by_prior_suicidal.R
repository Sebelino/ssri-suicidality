# cate_by_prior_suicidal.R
# IPW-adjusted ITT effect stratified by prior suicidal-behavior diagnosis
# (`diag_suicidal`). Motivated by the iCF result that `diag_suicidal` has the
# highest variable importance but does not appear in any voted tree split at
# any depth -- this script directly tests whether the IPW risk-difference
# differs between patients with and without a prior suicidal-behavior diagnosis.
#
# Output: stdout summary + output/stratified_prior_suicidal_values.tex
#         with \stratPriorSuic{No,Yes}* macros mirroring the cate_by_source
#         convention.

library(survival)
library(dplyr)
library(here)
here::i_am("suicidality/analysis/cate_by_prior_suicidal.R")

source(here("suicidality", "analysis", "common.R"))

# =============================================================================
# LOAD DATA
# =============================================================================

data_12 <- filter_complete_cases(read_rds_file("main_12wks_28.rds"))

# =============================================================================
# PS MODEL (mirrors ITT_12wks.R; `diag_suicidal` is dropped from the PS
# formula when stratifying on it)
# =============================================================================

build_ps_formula <- function(include_diag_suicidal = TRUE) {
  demo_vars <- "female + age + year"
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
  hosp_vars <- "hosp"

  diag_terms <- c(
    "diag_bipolar", "diag_psychotic", "diag_alcohol", "diag_sud",
    "diag_autism", "diag_adhd",
    if (include_diag_suicidal) "diag_suicidal",
    "diag_overdose",
    "diag_stress", "diag_anxiety",
    "diag_sleep", "diag_anorexia", "diag_bulimia",
    "diag_ocd", "diag_conduct", "diag_intellectual_disability",
    "diag_personality_cluster_b"
  )
  diag_vars <- paste(diag_terms, collapse = " + ")
  med_vars <- paste(
    "med_antipsychotic", "med_hypnotic", "med_benzodiazepine",
    "med_antiepileptic", "med_stimulant", "med_opioid",
    "med_mood_stabilizer", "med_addiction",
    sep = " + "
  )

  as.formula(paste("cc ~", demo_vars, "+", socio_vars, "+", fh_vars, "+",
                   hosp_vars, "+", diag_vars, "+", med_vars))
}

# =============================================================================
# ANALYSIS FUNCTION
# =============================================================================

analyze_stratum <- function(data, stratum_label, include_diag_suicidal) {
  cat(sprintf("\n--- %s (n=%d) ---\n", stratum_label, nrow(data)))

  data$t_end <- ceiling((data$fu_end_itt - data$fu_start) / 7)
  data$diag_anxiety <- as.integer(data$diag_phobic == 1 | data$diag_anxiety_other == 1)

  p.denom <- glm(build_ps_formula(include_diag_suicidal),
                 data = data, family = binomial())
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

  if (risk_control > 0 && risk_treated > 0) {
    rr <- risk_treated / risk_control
    log_rr_se <- sqrt((se_treated / risk_treated)^2 +
                      (se_control / risk_control)^2)
    rr_lower <- exp(log(rr) - 1.96 * log_rr_se)
    rr_upper <- exp(log(rr) + 1.96 * log_rr_se)
  } else {
    rr <- NA_real_; rr_lower <- NA_real_; rr_upper <- NA_real_
  }

  n_treated      <- sum(data$cc == 1)
  n_control      <- sum(data$cc == 0)
  events_treated <- sum(data$cc == 1 & data$sb12_itt == 1)
  events_control <- sum(data$cc == 0 & data$sb12_itt == 1)

  cat(sprintf("  N: %d treated, %d control\n", n_treated, n_control))
  cat(sprintf("  Events: %d treated, %d control\n", events_treated, events_control))
  cat(sprintf("  Risk: %.2f%% treated vs %.2f%% control\n", risk_treated, risk_control))
  cat(sprintf("  RD: %+.2f pp (95%% CI: %.2f, %.2f)\n", rd, rd_lower, rd_upper))
  cat(sprintf("  RR: %.2f (95%% CI: %.2f, %.2f)\n", rr, rr_lower, rr_upper))

  data.frame(
    stratum        = stratum_label,
    n_treated      = n_treated,
    n_control      = n_control,
    events_treated = events_treated,
    events_control = events_control,
    risk_treated   = risk_treated,
    risk_control   = risk_control,
    rd = rd, rd_lower = rd_lower, rd_upper = rd_upper,
    rr = rr, rr_lower = rr_lower, rr_upper = rr_upper
  )
}

# =============================================================================
# RUN
# =============================================================================

cat("\n============================================================\n")
cat("12-WEEK ITT STRATIFIED BY PRIOR SUICIDAL BEHAVIOR (diag_suicidal)\n")
cat("============================================================\n")

cat(sprintf("\nFull CCA cohort: n=%d\n", nrow(data_12)))
cat(sprintf("  diag_suicidal == 0 (no prior): n=%d\n", sum(data_12$diag_suicidal == 0)))
cat(sprintf("  diag_suicidal == 1 (prior):    n=%d\n", sum(data_12$diag_suicidal == 1)))

res_no  <- analyze_stratum(data_12 %>% filter(diag_suicidal == 0),
                           "No prior suicidal behavior (12 weeks)",
                           include_diag_suicidal = FALSE)
res_yes <- analyze_stratum(data_12 %>% filter(diag_suicidal == 1),
                           "Prior suicidal behavior (12 weeks)",
                           include_diag_suicidal = FALSE)

all_results <- rbind(res_no, res_yes)
rownames(all_results) <- NULL

output_dir <- here("suicidality", "analysis", "output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

write.csv(all_results,
          file.path(output_dir, "stratified_prior_suicidal.csv"),
          row.names = FALSE)
cat("\nSaved: stratified_prior_suicidal.csv\n")

# =============================================================================
# LATEX MACROS
# =============================================================================

format_n  <- function(n) format(n, big.mark = "{,}", scientific = FALSE)
format_rd <- function(x) if (x >= 0) sprintf("+%.2f", x) else sprintf("%.2f", x)
format_ci <- function(x) sprintf("%.2f", x)

emit_macros <- function(prefix, r) {
  c(
    sprintf("\\newcommand{\\%sN}{%s}",             prefix, format_n(r$n_treated + r$n_control)),
    sprintf("\\newcommand{\\%sNTreated}{%s}",      prefix, format_n(r$n_treated)),
    sprintf("\\newcommand{\\%sNControl}{%s}",      prefix, format_n(r$n_control)),
    sprintf("\\newcommand{\\%sEvents}{%s}",        prefix, format_n(r$events_treated + r$events_control)),
    sprintf("\\newcommand{\\%sEventsTreated}{%s}", prefix, format_n(r$events_treated)),
    sprintf("\\newcommand{\\%sEventsControl}{%s}", prefix, format_n(r$events_control)),
    sprintf("\\newcommand{\\%sRiskTreated}{%.2f}", prefix, r$risk_treated),
    sprintf("\\newcommand{\\%sRiskControl}{%.2f}", prefix, r$risk_control),
    sprintf("\\newcommand{\\%sRD}{%s}",            prefix, format_rd(r$rd)),
    sprintf("\\newcommand{\\%sRDLower}{%s}",       prefix, format_ci(r$rd_lower)),
    sprintf("\\newcommand{\\%sRDUpper}{%s}",       prefix, format_ci(r$rd_upper)),
    sprintf("\\newcommand{\\%sRR}{%.2f}",          prefix, r$rr),
    sprintf("\\newcommand{\\%sRRLower}{%s}",       prefix, format_ci(r$rr_lower)),
    sprintf("\\newcommand{\\%sRRUpper}{%s}",       prefix, format_ci(r$rr_upper))
  )
}

lines <- c(
  "% Auto-generated by cate_by_prior_suicidal.R -- do not edit manually",
  "% ITT stratified by prior suicidal-behavior diagnosis",
  "",
  emit_macros("stratPriorSuicNo",  res_no),
  "",
  emit_macros("stratPriorSuicYes", res_yes)
)

writeLines(lines, file.path(output_dir, "stratified_prior_suicidal_values.tex"))
cat("Saved: stratified_prior_suicidal_values.tex\n")

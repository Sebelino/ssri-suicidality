# plot_qini.R
# Qini curve and AUTOC for SSRI treatment targeting based on causal forest CATE.
#
# Adapted from Sverdrup, Petukhova & Wager (2025) and the AIPW-based Qini /
# AUTOC formulation used in grf::rank_average_treatment_effect().
#
# The Qini curve shows the cumulative *observed* (AIPW-adjusted) treatment
# effect among the top-q fraction of patients ranked by predicted CATE. AUTOC
# integrates this curve and tests whether the predicted CATE actually tracks
# the empirical treatment effect (validation), not just whether the model's
# own CATE predictions span a wide range (model self-consistency).

library(dplyr)
library(ggplot2)
library(here)
here::i_am("suicidality/analysis/plot_qini.R")
source(here("suicidality", "analysis", "common.R"))

# =============================================================================
# LOAD DATA
# =============================================================================

# Individual CATE predictions plus nuisance estimates from the raw causal forest
results <- readRDS(here("suicidality", "analysis-icf", "output", "icf_results.rds"))

# Cohort outcomes and treatment, joined to the same patient ordering as
# results$cate_individual (which mirrors the rows of step1$X / icf_data.rds,
# i.e. the complete-case cohort used to fit the iCF).
data <- filter_complete_cases(
  as.data.frame(readRDS(here("suicidality", "extraction", "output", "rds", "main_12wks_28.rds")))
)

# Sanity check: row count must match between CATE predictions and cohort
if (length(results$cate_individual) != nrow(data)) {
  stop(sprintf(
    "Row count mismatch: cate_individual has %d entries, data has %d rows.\nDid the cohort change since icf_results.rds was generated?",
    length(results$cate_individual), nrow(data)
  ))
}

output_dir <- here("suicidality", "analysis", "output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# AIPW SCORES (doubly-robust transformation of the causal forest output)
# =============================================================================

#' Compute AIPW scores for individual treatment effect.
#'
#' Following Athey, Tibshirani & Wager (2019) "Generalized random forests" and
#' the formulation used in grf::get_scores() / rank_average_treatment_effect():
#'
#'   Γ_i = τ̂(X_i) + (W_i − ê(X_i)) / (ê(X_i)(1 − ê(X_i)))
#'                 · (Y_i − m̂(X_i) − (W_i − ê(X_i)) · τ̂(X_i))
#'
#' where τ̂ is CATE, ê is propensity (W.hat), m̂ is marginal outcome
#' regression (Y.hat). Γ_i is an unbiased (under correct nuisance) estimate
#' of the individual treatment effect Y_i(1) − Y_i(0).
#'
#' Using AIPW scores as the gain measure (rather than predicted CATE alone)
#' is what makes the Qini curve a *validation* of the targeting policy
#' against observed outcomes, rather than a self-consistency check on the
#' forest's own predictions.
compute_aipw_scores <- function(cate_hat, W.hat, Y.hat, Y, W) {
  # Clip propensity scores away from 0/1 to prevent division blowup
  eps <- 1e-3
  W.hat <- pmin(pmax(W.hat, eps), 1 - eps)

  residual <- Y - Y.hat - (W - W.hat) * cate_hat
  cate_hat + (W - W.hat) / (W.hat * (1 - W.hat)) * residual
}

cat("Computing AIPW scores from raw causal forest nuisance estimates...\n")
gamma <- compute_aipw_scores(
  cate_hat = results$cate_individual,
  W.hat    = results$W.hat,
  Y.hat    = results$Y.hat,
  Y        = data$sb12_itt,
  W        = data$cc
)

# Sanity check: mean(gamma) should approximately equal the IPW-adjusted ATE
cat(sprintf("AIPW-implied ATE: %.4f (compare to ITT main RD ~0.0103)\n", mean(gamma)))

# =============================================================================
# QINI CURVE COMPUTATION
# =============================================================================

#' Compute Qini curve values from AIPW scores ranked by predicted CATE.
#'
#' The "gain" at fraction q is the cumulative AIPW score among the top-q
#' fraction of patients ranked by predicted CATE. This is the empirically
#' estimated number of suicidal-behavior events that would be prevented by
#' withholding SSRI from the top-q most-harmed patients (events_prevented =
#' -gain when gain measures CATE = harm). Higher gain = more events
#' prevented (since CATE > 0 = treatment increases risk).
#'
#' @param cate_hat Individual CATE predictions used for *ranking* only.
#' @param aipw     AIPW scores used for *aggregating* the gain (the unbiased
#'                  per-patient treatment effect estimate).
#' @param n_points Number of points to evaluate along the curve.
compute_qini <- function(cate_hat, aipw, n_points = 200) {
  n <- length(cate_hat)

  # Sort by predicted CATE descending (most harmed first) and aggregate AIPW
  ord <- order(cate_hat, decreasing = TRUE)
  aipw_sorted <- aipw[ord]
  cum_aipw <- cumsum(aipw_sorted)

  # Random-policy baseline: linear in q, slope = total cumulative gain / n
  total_gain <- sum(aipw)

  eval_idx <- unique(c(1, round(seq(1, n, length.out = n_points)), n))
  eval_idx <- sort(unique(eval_idx))

  q <- eval_idx / n
  gain <- cum_aipw[eval_idx]
  random_gain <- q * total_gain

  data.frame(
    q = q,
    n_withheld = eval_idx,
    gain = gain,
    random_gain = random_gain
  )
}

#' Bootstrap Qini curve confidence intervals.
compute_qini_ci <- function(cate_hat, aipw, n_boot = 500, n_points = 200, alpha = 0.05) {
  n <- length(cate_hat)
  base_qini <- compute_qini(cate_hat, aipw, n_points)

  boot_gains <- matrix(NA, nrow = nrow(base_qini), ncol = n_boot)
  set.seed(42)
  for (b in seq_len(n_boot)) {
    idx <- sample(n, n, replace = TRUE)
    bq <- compute_qini(cate_hat[idx], aipw[idx], n_points)
    boot_gains[, b] <- bq$gain
  }

  base_qini$ci_lower <- apply(boot_gains, 1, quantile, probs = alpha / 2)
  base_qini$ci_upper <- apply(boot_gains, 1, quantile, probs = 1 - alpha / 2)
  base_qini
}

# =============================================================================
# AUTOC COMPUTATION
# =============================================================================

#' Compute AUTOC (Area Under the Targeting Operator Characteristic curve)
#' using AIPW scores. Following Yadlowsky, Pellegrini, Lionetto, Braune &
#' Tibshirani (2024), the AUTOC is the rank-weighted treatment effect:
#'
#'   AUTOC = mean(Γ_i · frac_rank_i) − mean(Γ_i) / 2
#'
#' where frac_rank_i = (n − rank_i + 0.5) / n with ranks computed from
#' descending CATE (so the highest-CATE patient gets frac_rank close to 1).
#' AUTOC > 0 means high-CATE patients have higher AIPW than random — i.e.,
#' the targeting policy beats random.
compute_autoc <- function(cate_hat, aipw) {
  n <- length(cate_hat)
  ranks <- rank(-cate_hat, ties.method = "average")
  frac_rank <- (n - ranks + 0.5) / n
  mean(aipw * frac_rank) - mean(aipw) / 2
}

#' Bootstrap AUTOC and a Wald p-value against the null AUTOC = 0.
compute_autoc_inference <- function(cate_hat, aipw, n_boot = 1000) {
  observed <- compute_autoc(cate_hat, aipw)
  n <- length(cate_hat)

  boot_autoc <- numeric(n_boot)
  set.seed(42)
  for (b in seq_len(n_boot)) {
    idx <- sample(n, n, replace = TRUE)
    boot_autoc[b] <- compute_autoc(cate_hat[idx], aipw[idx])
  }

  se <- sd(boot_autoc)
  z <- observed / se
  p_value <- 2 * pnorm(-abs(z))
  ci_lower <- quantile(boot_autoc, 0.025)
  ci_upper <- quantile(boot_autoc, 0.975)

  list(
    autoc = observed,
    se = se,
    z = z,
    p_value = p_value,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  )
}

# =============================================================================
# COMPUTE QINI CURVE AND AUTOC (12-WEEK)
# =============================================================================

cat("Computing 12-week Qini curve with bootstrap CIs...\n")
qini <- compute_qini_ci(
  cate_hat = results$cate_individual,
  aipw     = gamma,
  n_boot   = 500
)

cat("Computing 12-week AUTOC with bootstrap inference...\n")
autoc <- compute_autoc_inference(results$cate_individual, gamma, n_boot = 1000)

cat(sprintf(
  "12-week AUTOC: %.5f (SE=%.5f, z=%.2f, p=%.4f, 95%% CI [%.5f, %.5f])\n",
  autoc$autoc, autoc$se, autoc$z, autoc$p_value, autoc$ci_lower, autoc$ci_upper
))

format_autoc_label <- function(autoc_res) {
  pp <- autoc_res$autoc * 100  # convert to percentage points
  se_pp <- autoc_res$se * 100
  p <- autoc_res$p_value
  if (p < 1e-10) {
    p_str <- "< 1e-10"
  } else if (p < 0.001) {
    p_str <- sprintf("%.1e", p)
  } else {
    p_str <- sprintf("%.3f", p)
  }
  sprintf("AUTOC = %.3f pp (SE = %.4f, p %s)", pp, se_pp, p_str)
}

label_str <- format_autoc_label(autoc)

# =============================================================================
# PLOT: QINI CURVE
# =============================================================================

n_total <- nrow(data)

p <- ggplot(qini, aes(x = n_withheld)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "grey80", alpha = 0.5) +
  geom_line(aes(y = gain), linewidth = 0.8) +
  geom_line(aes(y = random_gain), linetype = "dashed", colour = "grey40", linewidth = 0.6) +
  annotate("text", x = n_total * 0.02, y = max(qini$gain) * 0.95,
           label = label_str, hjust = 0, size = 3.8, fontface = "italic") +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Qini Curve (12-week)",
    subtitle = "Events prevented by withholding SSRI from most-harmed patients",
    x = "Patients withheld from SSRI (ranked by predicted CATE)",
    y = "Suicidal behavior events prevented (AIPW-adjusted)"
  ) +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(output_dir, "qini_12wks.pdf"), p, width = 8, height = 5.5)
cat("Saved: qini_12wks.pdf\n")

# =============================================================================
# EXPORT TEX MACROS
# =============================================================================

format_pval_tex <- function(p) {
  if (p < 1e-10) return("$<10^{-10}$")
  if (p < 0.001) {
    expo <- floor(log10(p))
    mantissa <- p / 10^expo
    return(sprintf("%.2f \\times 10^{%d}", mantissa, expo))
  }
  sprintf("%.3f", p)
}

tex_lines <- c(
  "% Auto-generated by plot_qini.R -- do not edit manually",
  sprintf("\\newcommand{\\autocTwelve}{%.3f}", autoc$autoc * 100),
  sprintf("\\newcommand{\\autocTwelveSE}{%.3f}", autoc$se * 100),
  sprintf("\\newcommand{\\autocTwelveCILower}{%.3f}", autoc$ci_lower * 100),
  sprintf("\\newcommand{\\autocTwelveCIUpper}{%.3f}", autoc$ci_upper * 100),
  sprintf("\\newcommand{\\autocTwelveZ}{%.2f}", autoc$z),
  sprintf("\\newcommand{\\autocTwelvePvalue}{%s}", format_pval_tex(autoc$p_value))
)
writeLines(tex_lines, file.path(output_dir, "qini_values.tex"))
cat("Saved: qini_values.tex\n")

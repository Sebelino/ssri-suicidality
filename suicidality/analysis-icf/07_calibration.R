# 07_calibration.R
# CATE calibration: do predicted treatment effects match observed effects?
#
# Bins patients by predicted CATE (deciles), computes IPTW-weighted observed
# treatment effects within each bin, and compares predicted vs observed.
#
# Input:  data/icf_data.rds, output/cate_comparison.rds
# Output: output/calibration_plot.pdf, output/calibration_summary.txt

library(tidyverse)
library(ggplot2)
library(patchwork)
library(here)
here::i_am("suicidality/analysis-icf/07_calibration.R")

# =============================================================================
# LOAD DATA
# =============================================================================

cat("Loading data...\n")

output_dir <- here("suicidality", "analysis-icf", "output")
icf_data <- readRDS(here("suicidality", "analysis-icf", "data", "icf_data.rds"))
comp <- readRDS(file.path(output_dir, "cate_comparison.rds"))

Y <- icf_data$Y
W <- icf_data$W

cat(sprintf("N = %d, events = %d, treated = %d\n", length(Y), sum(Y), sum(W)))

# =============================================================================
# CALIBRATION FUNCTION
# =============================================================================

#' Compute calibration table: bin by predicted CATE, compute observed effect
#'
#' @param tau Predicted CATE vector (on probability scale)
#' @param Y Outcome vector (0/1)
#' @param W Treatment vector (0/1)
#' @param e Propensity scores
#' @param n_bins Number of bins (deciles by default)
#' @return data.frame with columns: bin, n, predicted_cate, observed_cate, se
calibrate <- function(tau, Y, W, e, n_bins = 10) {
  # Clip propensity scores
  e <- pmax(pmin(e, 0.99), 0.01)

  # AIPW score per individual (unbiased estimate of individual's treatment effect)
  # This is the standard doubly-robust score used in grf's calibration tests
  aipw_score <- tau +
    W * (Y - tau) / e -         # treated correction
    (1 - W) * (Y - 0) / (1 - e) # control correction... simplified:
  # Actually, let's use a cleaner formulation. Within each bin, compute the
  # IPTW-weighted difference in means.

  bin <- cut(tau, breaks = quantile(tau, probs = seq(0, 1, length.out = n_bins + 1)),
             include.lowest = TRUE, labels = FALSE)

  results <- data.frame(
    bin = integer(), n = integer(),
    predicted_cate = numeric(), observed_cate = numeric(), se = numeric()
  )

  for (b in sort(unique(bin))) {
    idx <- which(bin == b)
    n_b <- length(idx)
    Y_b <- Y[idx]
    W_b <- W[idx]
    e_b <- e[idx]
    tau_b <- tau[idx]

    # IPTW-weighted rates
    w_treat <- W_b / e_b
    w_ctrl <- (1 - W_b) / (1 - e_b)
    rate_treat <- sum(w_treat * Y_b) / sum(w_treat)
    rate_ctrl <- sum(w_ctrl * Y_b) / sum(w_ctrl)
    obs_cate <- rate_treat - rate_ctrl

    # Bootstrap SE (fast, 200 reps)
    set.seed(b)
    boot_cates <- replicate(200, {
      s <- sample(length(idx), replace = TRUE)
      wt <- W_b[s] / e_b[s]
      wc <- (1 - W_b[s]) / (1 - e_b[s])
      sum(wt * Y_b[s]) / sum(wt) - sum(wc * Y_b[s]) / sum(wc)
    })
    se_b <- sd(boot_cates)

    results <- rbind(results, data.frame(
      bin = b, n = n_b,
      predicted_cate = mean(tau_b),
      observed_cate = obs_cate,
      se = se_b
    ))
  }

  results
}

# =============================================================================
# COMPUTE CALIBRATION
# =============================================================================

cat("\nComputing calibration (10 bins)...\n")

cal_cf <- calibrate(comp$tau_cf, Y, W, comp$W.hat) %>% mutate(method = "Causal Forest")
cat("  Causal Forest done\n")
cal_dr <- calibrate(comp$tau_dr, Y, W, comp$W.hat) %>% mutate(method = "Doubly-Robust")
cat("  Doubly-Robust done\n")
cal_tl <- calibrate(comp$tau_tl, Y, W, comp$W.hat) %>% mutate(method = "T-Learner")
cat("  T-Learner done\n")

cal_all <- bind_rows(cal_cf, cal_dr, cal_tl) %>%
  mutate(
    predicted_pp = predicted_cate * 100,
    observed_pp = observed_cate * 100,
    se_pp = se * 100
  )

# =============================================================================
# CALIBRATION PLOT
# =============================================================================

cat("\nCreating calibration plot...\n")

method_colors <- c(
  "Causal Forest" = "steelblue",
  "Doubly-Robust" = "coral",
  "T-Learner" = "forestgreen"
)

make_cal_panel <- function(cal_df, method_name) {
  df <- cal_df %>% filter(method == method_name)
  # Use the same range for x and y so the y=x calibration line is at 45 degrees
  # and predicted/observed are directly visually comparable.
  lims <- range(c(df$predicted_pp,
                   df$observed_pp + 1.96 * df$se_pp,
                   df$observed_pp - 1.96 * df$se_pp))
  pad <- diff(lims) * 0.05
  lims <- c(lims[1] - pad, lims[2] + pad)

  # Fit the same regression that produces the reported slope/intercept macros
  fit <- lm(observed_pp ~ predicted_pp, data = df)
  fit_slope <- unname(coef(fit)[2])
  fit_intercept <- unname(coef(fit)[1])
  panel_subtitle <- sprintf("slope = %.2f, intercept = %.2f (ideal = 1, 0)",
                            fit_slope, fit_intercept)

  ggplot(df, aes(x = predicted_pp, y = observed_pp)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey50") +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE,
                color = method_colors[method_name],
                fill  = method_colors[method_name],
                linewidth = 0.8, alpha = 0.15) +
    geom_errorbar(aes(ymin = observed_pp - 1.96 * se_pp,
                      ymax = observed_pp + 1.96 * se_pp),
                  width = 0.02, color = method_colors[method_name]) +
    geom_point(size = 3, color = method_colors[method_name]) +
    coord_cartesian(xlim = lims, ylim = lims) +
    labs(title = method_name,
         subtitle = panel_subtitle,
         x = "Mean predicted CATE (pp)",
         y = "Observed IPTW effect (pp)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 11, face = "bold"),
          plot.subtitle = element_text(size = 9))
}

p1 <- make_cal_panel(cal_all, "Causal Forest")
p2 <- make_cal_panel(cal_all, "Doubly-Robust")
p3 <- make_cal_panel(cal_all, "T-Learner")

p_cal <- ((p1 | p2) / (p3 | plot_spacer())) +
  plot_annotation(
    title = "CATE Calibration: Predicted vs Observed Treatment Effects",
    subtitle = "Patients binned into deciles of predicted CATE; error bars = 95% bootstrap CI.\nDashed grey: perfect calibration (y = x). Solid colored: linear fit with 95% CI band.",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11)
    )
  )

ggsave(
  file.path(output_dir, "calibration_plot.pdf"),
  p_cal, width = 10, height = 9
)
cat("Saved: calibration_plot.pdf\n")

# =============================================================================
# TEXT SUMMARY
# =============================================================================

cat("\nGenerating calibration summary...\n")

summary_lines <- c(
  "=== CATE Calibration Summary ===",
  sprintf("Generated: %s", Sys.time()),
  sprintf("N patients: %d", length(Y)),
  sprintf("Bins: 10 (deciles of predicted CATE)"),
  ""
)

# Macro prefix per method for the LaTeX export.
method_prefix <- c(
  "Causal Forest" = "calibCF",
  "Doubly-Robust" = "calibDR",
  "T-Learner"     = "calibTL"
)
tex_lines <- c(
  "% Auto-generated by 07_calibration.R -- do not edit manually",
  sprintf("%% Generated: %s", Sys.time())
)

for (method_name in c("Causal Forest", "Doubly-Robust", "T-Learner")) {
  df <- cal_all %>% filter(method == method_name)

  # Calibration slope: regress observed on predicted
  fit <- lm(observed_pp ~ predicted_pp, data = df)
  slope <- unname(coef(fit)[2])
  intercept <- unname(coef(fit)[1])
  r2 <- summary(fit)$r.squared

  # Mean absolute calibration error
  mace <- mean(abs(df$predicted_pp - df$observed_pp))

  summary_lines <- c(summary_lines,
    sprintf("--- %s ---", method_name),
    sprintf("  Calibration slope:     %.3f  (ideal = 1.0)", slope),
    sprintf("  Calibration intercept: %.3f  (ideal = 0.0)", intercept),
    sprintf("  R-squared:             %.3f", r2),
    sprintf("  Mean abs cal error:    %.3f pp", mace),
    "",
    sprintf("  %5s  %6s  %12s  %12s  %8s",
            "Bin", "N", "Predicted", "Observed", "SE"),
    sprintf("  %5s  %6s  %12s  %12s  %8s",
            "", "", "(pp)", "(pp)", "(pp)")
  )

  for (i in seq_len(nrow(df))) {
    summary_lines <- c(summary_lines,
      sprintf("  %5d  %6d  %12.3f  %12.3f  %8.3f",
              df$bin[i], df$n[i], df$predicted_pp[i], df$observed_pp[i], df$se_pp[i])
    )
  }

  summary_lines <- c(summary_lines, "")

  # LaTeX macros for each method (intended for thesis prose).
  px <- method_prefix[[method_name]]
  tex_lines <- c(tex_lines,
    sprintf("\\newcommand{\\%sSlope}{%s%.2f}", px, ifelse(slope < 0, "$-$", ""), abs(slope)),
    sprintf("\\newcommand{\\%sIntercept}{%s%.2f}", px, ifelse(intercept < 0, "$-$", ""), abs(intercept)),
    sprintf("\\newcommand{\\%sRsq}{%.3f}", px, r2),
    sprintf("\\newcommand{\\%sMACE}{%.2f}", px, mace),
    sprintf("\\newcommand{\\%sPredMin}{%s%.2f}", px, ifelse(min(df$predicted_pp) < 0, "$-$", ""), abs(min(df$predicted_pp))),
    sprintf("\\newcommand{\\%sPredMax}{%s%.2f}", px, ifelse(max(df$predicted_pp) < 0, "$-$", ""), abs(max(df$predicted_pp))),
    sprintf("\\newcommand{\\%sObsMin}{%s%.2f}", px, ifelse(min(df$observed_pp) < 0, "$-$", ""), abs(min(df$observed_pp))),
    sprintf("\\newcommand{\\%sObsMax}{%s%.2f}", px, ifelse(max(df$observed_pp) < 0, "$-$", ""), abs(max(df$observed_pp)))
  )
}

summary_file <- file.path(output_dir, "calibration_summary.txt")
writeLines(summary_lines, summary_file)
cat("Saved:", summary_file, "\n")

tex_file <- file.path(output_dir, "calibration_values.tex")
writeLines(tex_lines, tex_file)
cat("Saved:", tex_file, "\n")

cat("\n==============================================\n")
cat("CALIBRATION COMPLETE\n")
cat("==============================================\n")

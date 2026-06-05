# plot_qini_hdicf.R
# Qini curve and AUTOC for the hdiCF causal forest. Mirrors plot_qini.R but
# reads from the hdiCF results object and uses Y/W stored inside that object
# (since the hdiCF prep step trims a few rows, the cohort RDS row count does
# not match cate_individual).

library(dplyr)
library(ggplot2)
library(here)
here::i_am("suicidality/analysis/plot_qini_hdicf.R")

# =============================================================================
# LOAD DATA
# =============================================================================

results <- readRDS(here("suicidality", "analysis-hdicf", "output", "icf_results.rds"))

# hdiCF prep trims rows (e.g. extreme weights), so use Y/W stored in the
# results object after the bug-fix preserved them. cate_individual, W.hat,
# Y.hat, Y, and W are all aligned to the same patient ordering.
n <- length(results$cate_individual)
stopifnot(length(results$Y) == n,
          length(results$W) == n,
          length(results$W.hat) == n,
          length(results$Y.hat) == n)

output_dir <- here("suicidality", "analysis", "output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# =============================================================================
# AIPW SCORES
# =============================================================================

compute_aipw_scores <- function(cate_hat, W.hat, Y.hat, Y, W) {
  eps <- 1e-3
  W.hat <- pmin(pmax(W.hat, eps), 1 - eps)
  residual <- Y - Y.hat - (W - W.hat) * cate_hat
  cate_hat + (W - W.hat) / (W.hat * (1 - W.hat)) * residual
}

cat("Computing AIPW scores from hdiCF nuisance estimates...\n")
gamma <- compute_aipw_scores(
  cate_hat = results$cate_individual,
  W.hat    = results$W.hat,
  Y.hat    = results$Y.hat,
  Y        = results$Y,
  W        = results$W
)

cat(sprintf("AIPW-implied ATE: %.4f (compare to ITT main RD ~0.0103)\n", mean(gamma)))

# =============================================================================
# QINI CURVE COMPUTATION
# =============================================================================

compute_qini <- function(cate_hat, aipw, n_points = 200) {
  n <- length(cate_hat)
  ord <- order(cate_hat, decreasing = TRUE)
  cum_aipw <- cumsum(aipw[ord])
  total_gain <- sum(aipw)
  eval_idx <- sort(unique(c(1, round(seq(1, n, length.out = n_points)), n)))
  q <- eval_idx / n
  data.frame(
    q = q,
    n_withheld = eval_idx,
    gain = cum_aipw[eval_idx],
    random_gain = q * total_gain
  )
}

compute_qini_ci <- function(cate_hat, aipw, n_boot = 500, n_points = 200, alpha = 0.05) {
  n <- length(cate_hat)
  base_qini <- compute_qini(cate_hat, aipw, n_points)
  boot_gains <- matrix(NA, nrow = nrow(base_qini), ncol = n_boot)
  set.seed(42)
  for (b in seq_len(n_boot)) {
    idx <- sample(n, n, replace = TRUE)
    boot_gains[, b] <- compute_qini(cate_hat[idx], aipw[idx], n_points)$gain
  }
  base_qini$ci_lower <- apply(boot_gains, 1, quantile, probs = alpha / 2)
  base_qini$ci_upper <- apply(boot_gains, 1, quantile, probs = 1 - alpha / 2)
  base_qini
}

# =============================================================================
# AUTOC COMPUTATION
# =============================================================================

compute_autoc <- function(cate_hat, aipw) {
  n <- length(cate_hat)
  ranks <- rank(-cate_hat, ties.method = "average")
  frac_rank <- (n - ranks + 0.5) / n
  mean(aipw * frac_rank) - mean(aipw) / 2
}

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
  list(
    autoc = observed,
    se = se,
    z = z,
    p_value = 2 * pnorm(-abs(z)),
    ci_lower = quantile(boot_autoc, 0.025),
    ci_upper = quantile(boot_autoc, 0.975)
  )
}

cat("Computing 12-week Qini curve with bootstrap CIs...\n")
qini <- compute_qini_ci(results$cate_individual, gamma, n_boot = 500)

cat("Computing 12-week AUTOC with bootstrap inference...\n")
autoc <- compute_autoc_inference(results$cate_individual, gamma, n_boot = 1000)

cat(sprintf(
  "hdiCF 12-week AUTOC: %.5f (SE=%.5f, z=%.2f, p=%.4f, 95%% CI [%.5f, %.5f])\n",
  autoc$autoc, autoc$se, autoc$z, autoc$p_value, autoc$ci_lower, autoc$ci_upper
))

format_autoc_label <- function(autoc_res) {
  pp <- autoc_res$autoc * 100
  se_pp <- autoc_res$se * 100
  p <- autoc_res$p_value
  p_str <- if (p < 1e-10) "< 1e-10" else if (p < 0.001) sprintf("%.1e", p) else sprintf("%.3f", p)
  sprintf("AUTOC = %.3f pp (SE = %.4f, p %s)", pp, se_pp, p_str)
}

label_str <- format_autoc_label(autoc)

# =============================================================================
# PLOT
# =============================================================================

p <- ggplot(qini, aes(x = n_withheld)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "grey80", alpha = 0.5) +
  geom_line(aes(y = gain), linewidth = 0.8) +
  geom_line(aes(y = random_gain), linetype = "dashed", colour = "grey40", linewidth = 0.6) +
  annotate("text", x = n * 0.02, y = max(qini$gain) * 0.95,
           label = label_str, hjust = 0, size = 3.8, fontface = "italic") +
  scale_x_continuous(labels = scales::comma) +
  labs(
    title = "Qini Curve (12-week, hdiCF)",
    subtitle = "Events prevented by withholding SSRI from most-harmed patients",
    x = "Patients withheld from SSRI (ranked by predicted CATE)",
    y = "Suicidal behavior events prevented (AIPW-adjusted)"
  ) +
  theme_bw(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave(file.path(output_dir, "qini_12wks_hdicf.pdf"), p, width = 8, height = 5.5)
cat("Saved: qini_12wks_hdicf.pdf\n")

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
  "% Auto-generated by plot_qini_hdicf.R -- do not edit manually",
  sprintf("\\newcommand{\\autocHdicfTwelve}{%.3f}", autoc$autoc * 100),
  sprintf("\\newcommand{\\autocHdicfTwelveSE}{%.3f}", autoc$se * 100),
  sprintf("\\newcommand{\\autocHdicfTwelveCILower}{%.3f}", autoc$ci_lower * 100),
  sprintf("\\newcommand{\\autocHdicfTwelveCIUpper}{%.3f}", autoc$ci_upper * 100),
  sprintf("\\newcommand{\\autocHdicfTwelveZ}{%.2f}", autoc$z),
  sprintf("\\newcommand{\\autocHdicfTwelvePvalue}{%s}", format_pval_tex(autoc$p_value))
)
writeLines(tex_lines, file.path(output_dir, "qini_values_hdicf.tex"))
cat("Saved: qini_values_hdicf.tex\n")

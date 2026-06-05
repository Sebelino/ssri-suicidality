# 06_visualize_cate_comparison.R
# Visualize CATE method comparison results
#
# Creates four plots comparing Causal Forest, Doubly-Robust Learner, and T-Learner:
#   1. Three-panel variable importance (top 20 per method)
#   2. CATE distribution comparison (overlay)
#   3. Pairwise CATE scatter plots
#   4. CATE vs baseline risk (three panels)
#
# Input:  output/cate_comparison.rds
# Output: output/cate_vi_comparison.pdf, cate_distribution_comparison.pdf,
#         cate_pairwise_scatter.pdf, cate_risk_comparison.pdf

library(tidyverse)
library(ggplot2)
library(patchwork)
library(here)
here::i_am("suicidality/analysis-icf/06_visualize_cate_comparison.R")

# =============================================================================
# LOAD RESULTS
# =============================================================================

cat("Loading CATE comparison results...\n")

output_dir <- here("suicidality", "analysis-icf", "output")
results <- readRDS(file.path(output_dir, "cate_comparison.rds"))

method_colors <- c(
  "Causal Forest" = "steelblue",
  "Doubly-Robust" = "coral",
  "T-Learner" = "forestgreen"
)

# Winsorize extreme CATE estimates (clip to 1st–99th percentile)
winsorize <- function(x, probs = c(0.01, 0.99)) {
  q <- quantile(x, probs, na.rm = TRUE)
  pmax(pmin(x, q[2]), q[1])
}

tau_dr_w <- winsorize(results$tau_dr)
tau_tl_w <- winsorize(results$tau_tl)

cat(sprintf(
  "Winsorized DR:  [%.4f, %.4f] -> [%.4f, %.4f] (1st-99th pctl)\n",
  min(results$tau_dr), max(results$tau_dr), min(tau_dr_w), max(tau_dr_w)
))
cat(sprintf(
  "Winsorized T-L: [%.4f, %.4f] -> [%.4f, %.4f] (1st-99th pctl)\n",
  min(results$tau_tl), max(results$tau_tl), min(tau_tl_w), max(tau_tl_w)
))

# =============================================================================
# PLOT 1: THREE-PANEL VARIABLE IMPORTANCE (raw + human-readable versions)
# =============================================================================

cat("\nCreating variable importance comparison plots...\n")

# Load human-readable variable labels
var_labels <- jsonlite::fromJSON(
  here("suicidality", "analysis-icf", "variable_labels.json")
)

make_vi_panel <- function(vi_vec, title, labels = NULL) {
  vi_df <- data.frame(
    variable = names(vi_vec),
    importance = as.numeric(vi_vec),
    stringsAsFactors = FALSE
  ) %>%
    arrange(desc(importance)) %>%
    head(20)

  if (!is.null(labels)) {
    vi_df <- vi_df %>%
      mutate(
        display = ifelse(variable %in% names(labels),
                         labels[variable],
                         variable),
        display = factor(display, levels = rev(display))
      )
  } else {
    vi_df <- vi_df %>%
      mutate(display = factor(variable, levels = rev(variable)))
  }

  ggplot(vi_df, aes(x = display, y = importance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = title, x = NULL, y = "Importance") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 11, face = "bold"),
      axis.text.y = element_text(size = 8)
    )
}

# Raw version
p_vi_cf_raw <- make_vi_panel(results$vi_cf, "A: Causal Forest")
p_vi_dr_raw <- make_vi_panel(results$vi_dr, "B: Doubly-Robust Learner")
p_vi_tl_raw <- make_vi_panel(results$vi_tl, "C: T-Learner")

p_vi_raw <- (p_vi_cf_raw | p_vi_dr_raw | p_vi_tl_raw) +
  plot_annotation(
    title = "Variable Importance Comparison Across CATE Estimation Methods",
    subtitle = "Top 20 variables by weighted split frequency",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11)
    )
  )

ggsave(
  file.path(output_dir, "cate_vi_comparison_raw.pdf"),
  p_vi_raw, width = 15, height = 6
)
cat("Saved: cate_vi_comparison_raw.pdf\n")

# Human-readable version
p_vi_cf <- make_vi_panel(results$vi_cf, "A: Causal Forest", var_labels)
p_vi_dr <- make_vi_panel(results$vi_dr, "B: Doubly-Robust Learner", var_labels)
p_vi_tl <- make_vi_panel(results$vi_tl, "C: T-Learner", var_labels)

p_vi_combined <- (p_vi_cf | p_vi_dr | p_vi_tl) +
  plot_annotation(
    title = "Variable Importance Comparison Across CATE Estimation Methods",
    subtitle = "Top 20 variables by weighted split frequency",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11)
    )
  )

ggsave(
  file.path(output_dir, "cate_vi_comparison.pdf"),
  p_vi_combined, width = 15, height = 6
)
cat("Saved: cate_vi_comparison.pdf\n")

# =============================================================================
# PLOT 2: CATE DISTRIBUTION COMPARISON
# =============================================================================

cat("\nCreating CATE distribution comparison plot...\n")

n <- results$n

# Build sorted CATE curves for all three methods
build_sorted <- function(tau, method_name) {
  tau_sorted <- sort(tau, decreasing = TRUE)
  data.frame(
    percentile = seq_along(tau_sorted) / length(tau_sorted),
    cate = tau_sorted * 100,  # convert to percentage points
    method = method_name,
    stringsAsFactors = FALSE
  )
}

cate_dist_df <- bind_rows(
  build_sorted(results$tau_cf, "Causal Forest"),
  build_sorted(results$tau_dr, "Doubly-Robust"),
  build_sorted(results$tau_tl, "T-Learner")
)

ate_lines <- data.frame(
  method = c("Causal Forest", "Doubly-Robust", "T-Learner"),
  ate = c(results$ate_cf, results$ate_dr, results$ate_tl) * 100,
  stringsAsFactors = FALSE
)

p_dist <- ggplot(cate_dist_df, aes(x = percentile, y = cate, color = method)) +
  geom_line(linewidth = 0.5) +
  geom_hline(data = ate_lines, aes(yintercept = ate, color = method),
             linetype = "dashed", linewidth = 0.6) +
  scale_color_manual(values = method_colors) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "Distribution of Individual-Level CATE Estimates",
    subtitle = "Sorted from highest to lowest; dashed lines = ATE",
    x = "Percentile of patients (ranked by CATE)",
    y = "CATE (percentage points)",
    color = "Method"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "top"
  )

ggsave(
  file.path(output_dir, "cate_distribution_comparison.pdf"),
  p_dist, width = 10, height = 6
)
cat("Saved: cate_distribution_comparison.pdf\n")

# Winsorized version
cate_dist_w_df <- bind_rows(
  build_sorted(results$tau_cf, "Causal Forest"),
  build_sorted(tau_dr_w, "Doubly-Robust"),
  build_sorted(tau_tl_w, "T-Learner")
)

p_dist_w <- ggplot(cate_dist_w_df, aes(x = percentile, y = cate, color = method)) +
  geom_line(linewidth = 0.5) +
  geom_hline(data = ate_lines, aes(yintercept = ate, color = method),
             linetype = "dashed", linewidth = 0.6) +
  scale_color_manual(values = method_colors) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(
    title = "Distribution of Individual-Level CATE Estimates (Winsorized)",
    subtitle = "DR and T-Learner clipped to 1st–99th percentile; dashed lines = ATE",
    x = "Percentile of patients (ranked by CATE)",
    y = "CATE (percentage points)",
    color = "Method"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "top"
  )

ggsave(
  file.path(output_dir, "cate_distribution_comparison_winsorized.pdf"),
  p_dist_w, width = 10, height = 6
)
cat("Saved: cate_distribution_comparison_winsorized.pdf\n")

# =============================================================================
# PLOT 3: PAIRWISE CATE SCATTER PLOTS
# =============================================================================

cat("\nCreating pairwise CATE scatter plots...\n")

make_scatter <- function(tau_x, tau_y, label_x, label_y) {
  df <- data.frame(x = tau_x * 100, y = tau_y * 100)
  r <- cor(tau_x, tau_y)

  ggplot(df, aes(x = x, y = y)) +
    geom_point(alpha = 0.03, size = 0.3, color = "steelblue") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
    annotate("text", x = min(df$x) + 0.1 * diff(range(df$x)),
             y = max(df$y) - 0.05 * diff(range(df$y)),
             label = sprintf("r = %.3f", r),
             size = 4, fontface = "bold") +
    labs(x = paste(label_x, "(pp)"), y = paste(label_y, "(pp)")) +
    theme_minimal() +
    theme(plot.title = element_text(size = 11, face = "bold"))
}

p_s1 <- make_scatter(results$tau_cf, results$tau_dr, "Causal Forest", "Doubly-Robust")
p_s2 <- make_scatter(results$tau_cf, results$tau_tl, "Causal Forest", "T-Learner")
p_s3 <- make_scatter(results$tau_dr, results$tau_tl, "Doubly-Robust", "T-Learner")

p_scatter <- p_s1 | p_s2 | p_s3
p_scatter <- p_scatter +
  plot_annotation(
    title = "Pairwise Agreement Between CATE Estimation Methods",
    subtitle = "Each point = one patient; red dashed line = perfect agreement",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11)
    )
  )

ggsave(
  file.path(output_dir, "cate_pairwise_scatter.pdf"),
  p_scatter, width = 15, height = 5
)
cat("Saved: cate_pairwise_scatter.pdf\n")

# Winsorized version
p_s1_w <- make_scatter(results$tau_cf, tau_dr_w, "Causal Forest", "Doubly-Robust")
p_s2_w <- make_scatter(results$tau_cf, tau_tl_w, "Causal Forest", "T-Learner")
p_s3_w <- make_scatter(tau_dr_w, tau_tl_w, "Doubly-Robust", "T-Learner")

p_scatter_w <- p_s1_w | p_s2_w | p_s3_w
p_scatter_w <- p_scatter_w +
  plot_annotation(
    title = "Pairwise Agreement Between CATE Estimation Methods (Winsorized)",
    subtitle = "DR and T-Learner clipped to 1st–99th pctl; red dashed line = perfect agreement",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11)
    )
  )

ggsave(
  file.path(output_dir, "cate_pairwise_scatter_winsorized.pdf"),
  p_scatter_w, width = 15, height = 5
)
cat("Saved: cate_pairwise_scatter_winsorized.pdf\n")

# =============================================================================
# PLOT 4: CATE VS BASELINE RISK (THREE PANELS)
# =============================================================================

cat("\nCreating CATE vs baseline risk comparison plot...\n")

make_risk_panel <- function(tau, method_name, y_hat, n_smooth = 10000) {
  df <- data.frame(
    baseline_risk = y_hat * 100,
    cate = tau * 100
  )
  set.seed(42)
  df_smooth <- df[sample(nrow(df), min(n_smooth, nrow(df))), ]

  ggplot(df, aes(x = baseline_risk, y = cate)) +
    geom_point(alpha = 0.05, size = 0.3, color = "steelblue") +
    geom_smooth(data = df_smooth, method = "loess", color = "red",
                linewidth = 0.8, se = TRUE) +
    geom_hline(yintercept = mean(tau) * 100, linetype = "dashed", color = "grey40") +
    labs(title = method_name, x = "Baseline risk (pp)", y = "CATE (pp)") +
    theme_minimal() +
    theme(plot.title = element_text(size = 11, face = "bold"))
}

p_r1 <- make_risk_panel(results$tau_cf, "A: Causal Forest", results$Y.hat)
p_r2 <- make_risk_panel(results$tau_dr, "B: Doubly-Robust Learner", results$Y.hat)
p_r3 <- make_risk_panel(results$tau_tl, "C: T-Learner", results$Y.hat)

p_risk <- p_r1 | p_r2 | p_r3
p_risk <- p_risk +
  plot_annotation(
    title = "Treatment Effect Heterogeneity by Baseline Risk",
    subtitle = "Individual CATE vs predicted outcome risk; dashed line = ATE",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11)
    )
  )

ggsave(
  file.path(output_dir, "cate_risk_comparison.pdf"),
  p_risk, width = 15, height = 5
)
cat("Saved: cate_risk_comparison.pdf\n")

# Winsorized version
p_r1_w <- make_risk_panel(results$tau_cf, "A: Causal Forest", results$Y.hat)
p_r2_w <- make_risk_panel(tau_dr_w, "B: Doubly-Robust (Winsorized)", results$Y.hat)
p_r3_w <- make_risk_panel(tau_tl_w, "C: T-Learner (Winsorized)", results$Y.hat)

p_risk_w <- p_r1_w | p_r2_w | p_r3_w
p_risk_w <- p_risk_w +
  plot_annotation(
    title = "Treatment Effect Heterogeneity by Baseline Risk (Winsorized)",
    subtitle = "DR and T-Learner clipped to 1st–99th pctl; dashed line = ATE",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11)
    )
  )

ggsave(
  file.path(output_dir, "cate_risk_comparison_winsorized.pdf"),
  p_risk_w, width = 15, height = 5
)
cat("Saved: cate_risk_comparison_winsorized.pdf\n")

# Zoomed version (baseline risk 0-1 pp, CATE 0.5-1.5 pp)
p_r1_z <- make_risk_panel(results$tau_cf, "A: Causal Forest", results$Y.hat) +
  coord_cartesian(xlim = c(0, 2), ylim = c(0, 2))
p_r2_z <- make_risk_panel(results$tau_dr, "B: Doubly-Robust Learner", results$Y.hat) +
  coord_cartesian(xlim = c(0, 2), ylim = c(0, 2))
p_r3_z <- make_risk_panel(results$tau_tl, "C: T-Learner", results$Y.hat) +
  coord_cartesian(xlim = c(0, 2), ylim = c(0, 2))

p_risk_z <- p_r1_z | p_r2_z | p_r3_z
p_risk_z <- p_risk_z +
  plot_annotation(
    title = "Treatment Effect Heterogeneity by Baseline Risk (Zoomed)",
    subtitle = "Baseline risk 0–2 pp, CATE 0–2 pp; dashed line = ATE",
    theme = theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11)
    )
  )

ggsave(
  file.path(output_dir, "cate_risk_comparison_zoomed.pdf"),
  p_risk_z, width = 15, height = 5
)
cat("Saved: cate_risk_comparison_winsorized.pdf\n")

# =============================================================================
# TEXT SUMMARY (machine-readable companion to PDF plots)
# =============================================================================

cat("\nGenerating text summary of comparison results...\n")

summary_lines <- c(
  "=== CATE Method Comparison Summary ===",
  sprintf("Generated: %s", Sys.time()),
  sprintf("N patients: %d", results$n),
  "",
  "--- ATE by method (percentage points) ---",
  sprintf("  Causal Forest:  %.4f", results$ate_cf * 100),
  sprintf("  Doubly-Robust:  %.4f", results$ate_dr * 100),
  sprintf("  T-Learner:      %.4f", results$ate_tl * 100),
  "",
  "--- CATE range (percentage points) ---",
  sprintf("  Causal Forest:  [%.4f, %.4f]", min(results$tau_cf) * 100, max(results$tau_cf) * 100),
  sprintf("  Doubly-Robust:  [%.4f, %.4f]", min(results$tau_dr) * 100, max(results$tau_dr) * 100),
  sprintf("  T-Learner:      [%.4f, %.4f]", min(results$tau_tl) * 100, max(results$tau_tl) * 100),
  "",
  "--- CATE range after winsorization (1st-99th pctl, pp) ---",
  sprintf("  Doubly-Robust:  [%.4f, %.4f]", min(tau_dr_w) * 100, max(tau_dr_w) * 100),
  sprintf("  T-Learner:      [%.4f, %.4f]", min(tau_tl_w) * 100, max(tau_tl_w) * 100),
  "",
  "--- CATE quantiles (percentage points) ---"
)

for (method_name in c("Causal Forest", "Doubly-Robust", "T-Learner")) {
  tau_vec <- switch(method_name,
    "Causal Forest" = results$tau_cf,
    "Doubly-Robust" = results$tau_dr,
    "T-Learner"     = results$tau_tl
  )
  q <- quantile(tau_vec * 100, probs = c(0.01, 0.05, 0.25, 0.50, 0.75, 0.95, 0.99))
  summary_lines <- c(summary_lines,
    sprintf("  %s:", method_name),
    sprintf("    P1=%.3f  P5=%.3f  P25=%.3f  P50=%.3f  P75=%.3f  P95=%.3f  P99=%.3f",
            q[1], q[2], q[3], q[4], q[5], q[6], q[7])
  )
}

summary_lines <- c(summary_lines,
  "",
  "--- Pairwise correlations (raw) ---",
  sprintf("  CF vs DR:  r = %.4f", cor(results$tau_cf, results$tau_dr)),
  sprintf("  CF vs TL:  r = %.4f", cor(results$tau_cf, results$tau_tl)),
  sprintf("  DR vs TL:  r = %.4f", cor(results$tau_dr, results$tau_tl)),
  "",
  "--- Pairwise correlations (winsorized DR/TL) ---",
  sprintf("  CF vs DR:  r = %.4f", cor(results$tau_cf, tau_dr_w)),
  sprintf("  CF vs TL:  r = %.4f", cor(results$tau_cf, tau_tl_w)),
  sprintf("  DR vs TL:  r = %.4f", cor(tau_dr_w, tau_tl_w)),
  "",
  "--- CATE vs baseline risk (Spearman rho) ---",
  sprintf("  CF:  rho = %.4f", cor(results$tau_cf, results$Y.hat, method = "spearman")),
  sprintf("  DR:  rho = %.4f", cor(results$tau_dr, results$Y.hat, method = "spearman")),
  sprintf("  TL:  rho = %.4f", cor(results$tau_tl, results$Y.hat, method = "spearman")),
  "",
  "--- Top 6 variables per method ---"
)

for (method_name in c("Causal Forest", "Doubly-Robust", "T-Learner")) {
  vi_vec <- switch(method_name,
    "Causal Forest" = results$vi_cf,
    "Doubly-Robust" = results$vi_dr,
    "T-Learner"     = results$vi_tl
  )
  top6 <- sort(vi_vec, decreasing = TRUE)[1:6]
  summary_lines <- c(summary_lines, sprintf("  %s:", method_name))
  for (i in seq_along(top6)) {
    summary_lines <- c(summary_lines,
      sprintf("    %d. %s (%.4f)", i, names(top6)[i], top6[i])
    )
  }
}

summary_file <- file.path(output_dir, "cate_comparison_summary.txt")
writeLines(summary_lines, summary_file)
cat("Saved:", summary_file, "\n")

# =============================================================================
# SUMMARY
# =============================================================================

cat("\n==============================================\n")
cat("VISUALIZATION COMPLETE\n")
cat("==============================================\n")
cat("\nOutput files saved to:", output_dir, "\n")
cat("\nGenerated files:\n")
list.files(output_dir, pattern = "cate_.*\\.(pdf|txt)$") %>%
  paste(" -", .) %>%
  cat(sep = "\n")
cat("\n")

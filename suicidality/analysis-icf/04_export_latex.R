# 04_export_latex.R
# Export iCF results as LaTeX macros and table fragments
#
# Outputs (in output/):
#   icf_values.tex          - \newcommand definitions for all numeric values
#   icf_varimp_list.tex     - Top variable importance as \begin{enumerate}
#   icf_subgroups_table.tex - Subgroup table rows for \input

library(here)
here::i_am("suicidality/analysis-icf/04_export_latex.R")

# =============================================================================
# LOAD RESULTS
# =============================================================================

output_dir <- here("suicidality", "analysis-icf", "output")
results <- readRDS(file.path(output_dir, "icf_results.rds"))
config <- readRDS(file.path(output_dir, "config.rds"))
vi_df <- read.csv(file.path(output_dir, "variable_importance.csv"))

# Headline depth selection: prefer the 1-SE-rule result (results$reported_depth)
# when present and different from the CV-argmin; otherwise fall back to the
# CV-argmin. The Wang 2024 CV-argmin remains in results$best_depth for
# transparency and is reported via \icfCVMinDepth below.
reported_depth <- results$reported_depth %||% results$best_depth
reported_dname <- paste0("D", reported_depth)
cv_min_depth   <- results$best_depth
is_flat <- !is.null(results$diagnostics$one_se) && isTRUE(results$diagnostics$one_se$is_flat)

if (reported_depth == cv_min_depth) {
  cat(sprintf("Headline depth: D%d (CV-argmin). Using results$cate.\n", reported_depth))
  cate_df <- results$cate
  reported_subgroup_id <- results$subgroup_id
  reported_n_subgroups <- results$n_subgroups
  reported_voted_structure <- results$final_result$voted_structure
  reported_vote_frequency  <- results$final_result$vote_frequency
} else {
  cat(sprintf("Headline depth: D%d (1-SE rule; CV-argmin was D%d). Using all_depth_results$%s.\n",
              reported_depth, cv_min_depth, reported_dname))
  ar <- results$all_depth_results[[reported_dname]]
  if (is.null(ar) || is.null(ar$cate) || nrow(ar$cate) == 0) {
    stop(sprintf("all_depth_results$%s missing or empty — cannot export 1-SE headline depth.", reported_dname))
  }
  cate_df <- ar$cate
  reported_subgroup_id <- ar$final_result$subgroup_id
  reported_n_subgroups <- ar$final_result$n_subgroups
  reported_voted_structure <- ar$final_result$voted_structure
  reported_vote_frequency  <- ar$final_result$vote_frequency
}

# =============================================================================
# VARIABLE NAME MAPPING
# =============================================================================

# Human-readable labels for variable names
var_labels <- c(
  age = "Age",
  age_cat = "Age group",
  year = "Calendar year",
  female = "Sex",
  edufam_cat = "Parental education level",
  source = "Healthcare source",
  inc_cat = "Family income",
  hosp = "Prior psychiatric hospitalization",
  fh_suicidal = "Family history of suicidal behavior",
  fh_depr = "Family history of depression",
  diag_suicidal = "Prior suicidal behavior diagnosis",
  diag_overdose = "Prior overdose/poisoning",
  diag_alcohol = "Alcohol use disorder",
  diag_sud = "Substance use disorder",
  diag_bipolar = "Bipolar disorder",
  diag_psychotic = "Psychotic disorders",
  diag_stress = "Stress/adjustment disorders",
  diag_phobic = "Phobic anxiety disorder",
  diag_anxiety_other = "Other anxiety disorders",
  diag_sleep = "Sleep disorders",
  diag_organic = "Organic mental disorders",
  diag_anorexia = "Anorexia nervosa",
  diag_bulimia = "Bulimia nervosa",
  diag_ocd = "OCD",
  diag_conduct = "Conduct disorder",
  diag_adhd = "ADHD",
  diag_autism = "Autism spectrum disorder",
  diag_intellectual_disability = "Intellectual disability",
  diag_personality_cluster_b = "Cluster B personality disorder",
  med_antipsychotic = "Antipsychotic medication",
  med_hypnotic = "Hypnotic medication",
  med_benzodiazepine = "Benzodiazepine medication",
  med_antiepileptic = "Antiepileptic medication",
  med_stimulant = "Stimulant medication",
  med_opioid = "Opioid medication",
  med_mood_stabilizer = "Mood stabilizer medication",
  med_addiction = "Addiction medication"
)

# Condition-to-LaTeX mapping for subgroup labels
condition_to_latex <- function(cond) {
  # female>0 -> Female, female<=0 -> Male
  if (grepl("^female>0$", cond)) return("Female")
  if (grepl("^female<=0$", cond)) return("Male")
  # age_cat: 0=Children (6-11), 1=Adolescents (12-17), 2=Young adults (18-24)
  # age_cat<=0 -> Children, age_cat>0 & <=1 -> Adolescents or younger, etc.
  if (grepl("^age_cat", cond)) {
    m <- regmatches(cond, regexec("^age_cat(<=|>)(.+)$", cond))[[1]]
    if (length(m) == 3) {
      op <- m[2]
      val <- as.numeric(m[3])
      age_labels <- c("0" = "age 6--11",
                       "1" = "age 12--17",
                       "2" = "age 18--24")
      if (op == "<=" && val == 0) return("age 6--11")
      if (op == "<=" && val == 1) return("age 6--17")
      if (op == ">"  && val == 0) return("age 12--24")
      if (op == ">"  && val == 1) return("age 18--24")
      # Fallback
      return(paste0("Age group ", op, val))
    }
  }
  # Binary 0/1 covariates (fh_*, hosp, med_*, diag_*) split between 0 and 1,
  # so var<=0 means "no" and var>0 means "yes". Render those in plain English.
  m <- regmatches(cond, regexec("^(\\w+)(<=|>)(.+)$", cond))[[1]]
  if (length(m) == 4) {
    var <- m[2]
    op <- m[3]
    val <- m[4]
    binary_human <- list(
      fh_suicidal = c(no = "no family history of suicidal behavior",
                      yes = "family history of suicidal behavior"),
      fh_depr = c(no = "no family history of depression",
                  yes = "family history of depression"),
      hosp = c(no = "no prior psychiatric hospitalization",
               yes = "prior psychiatric hospitalization")
    )
    if (var %in% names(binary_human) && val == "0") {
      return(binary_human[[var]][[if (op == "<=") "no" else "yes"]])
    }
    # edufam_cat splits at <=1 (primary/secondary) vs >1 (post-secondary).
    if (var == "edufam_cat" && val == "1") {
      return(if (op == "<=") "primary/secondary parental education"
             else "post-secondary parental education")
    }
    # diag_*/med_* binary indicators
    if (grepl("^(diag|med)_", var) && val == "0") {
      base <- if (var %in% names(var_labels)) tolower(var_labels[var]) else sub("^(diag|med)_", "", var)
      return(paste0(if (op == "<=") "no " else "", base))
    }
    # Fallback: var $\leq$ val
    op_tex <- if (op == "<=") "$\\leq$" else "$>$"
    label <- if (var %in% names(var_labels)) var_labels[var] else var
    return(paste0(label, " ", op_tex, val))
  }
  cond
}

# Parse SG_cond1_cond2_... into human-readable definition
parse_subgroup_label <- function(label) {
  # Remove "SG_" prefix, split on "_" (but not within conditions)
  stripped <- sub("^SG_", "", label)
  # Split into conditions: each is var<=val or var>val
  conditions <- strsplit(stripped, "(?<=\\d)_(?=[a-z])", perl = TRUE)[[1]]
  parts <- vapply(conditions, condition_to_latex, character(1))
  paste(parts, collapse = ", ")
}

# =============================================================================
# FORMAT HELPERS
# =============================================================================

format_pvalue_latex <- function(p) {
  if (p < 0.001) {
    exponent <- floor(log10(p))
    mantissa <- p / 10^exponent
    sprintf("%.2f \\times 10^{%d}", mantissa, exponent)
  } else {
    sprintf("%.3f", p)
  }
}

format_n <- function(n) {
  format(n, big.mark = "{,}", scientific = FALSE)
}

format_cate <- function(cate) {
  # Defensive against NA: a future tree with a small / zero-event subgroup
  # could produce NA iptw_cate; emit "NA" rather than failing or producing
  # a literal "+NA" string in the .tex output.
  if (is.na(cate)) return("NA")
  if (cate >= 0) sprintf("+%.2f", cate) else sprintf("%.2f", cate)
}

# LaTeX-safe command name suffix: SGA, SGB, SGC, ...
sg_suffix <- function(i) {
  paste0("SG", LETTERS[i])
}

# =============================================================================
# GENERATE icf_values.tex
# =============================================================================

cat("Generating icf_values.tex...\n")

lines <- character()
cmd <- function(name, value) {
  lines <<- c(lines, sprintf("\\newcommand{\\%s}{%s}", name, value))
}
comment <- function(text) {
  lines <<- c(lines, paste0("% ", text))
}

comment("Auto-generated by 04_export_latex.R -- do not edit manually")
comment("")

comment("=== Configuration ===")
cmd("icfKFolds", config$K)
cmd("icfNTrees", config$n_trees)
cmd("icfNIterationsCV", config$n_iterations)
cmd("icfNIterationsFinal", config$n_iterations_final %||% config$n_iterations)
cmd("icfPThreshold", config$p_threshold)
lines <- c(lines, "")

comment("=== Heterogeneity test ===")
cmd("icfHetPvalue", format_pvalue_latex(results$het_p_value))
lines <- c(lines, "")

comment("=== Variable selection ===")
cmd("icfNSelectedVars", length(results$var_names))
lines <- c(lines, "")

comment("=== Cross-validation ===")
# The "reported" depth (\icfBestDepth) is the headline depth used for the
# main tree, table, and figures. Under the 1-SE rule it may differ from the
# CV-argmin (\icfCVMinDepth). \icfDepthIsFlat is "true" iff the 1-SE rule
# fires (more than one depth within 1 SE of CV-min).
cmd("icfBestDepth", reported_depth)
cmd("icfCVMinDepth", cv_min_depth)
cmd("icfNSubgroups", reported_n_subgroups)
cmd("icfDepthIsFlat", if (is_flat) "true" else "false")
if (!is.null(results$diagnostics$one_se)) {
  ose <- results$diagnostics$one_se
  cmd("icfOneSEPlateau", paste0("D", ose$plateau, collapse = ", "))
  cmd("icfOneSEValue", sprintf("%.2e", ose$se))
}
if (!is.null(results$diagnostics$cv_mse_relrange)) {
  cmd("icfCVMseRelRange", sprintf("%.2e", results$diagnostics$cv_mse_relrange))
}
cmd("icfVotedStructure", reported_voted_structure)
cmd("icfVoteFrequency", sprintf("%.2f", reported_vote_frequency))
lines <- c(lines, "")

comment("=== Per-subgroup values (ordered by subgroup_id) ===")
for (i in seq_len(nrow(cate_df))) {
  s <- sg_suffix(i)
  row <- cate_df[i, ]
  comment(paste0(s, ": ", row$label))
  cmd(paste0("icf", s, "N"), format_n(row$n_total))
  cmd(paste0("icf", s, "NTreated"), format_n(row$n_treated))
  cmd(paste0("icf", s, "NControl"), format_n(row$n_control))
  cmd(paste0("icf", s, "RateTreated"), sprintf("%.2f", row$rate_treated))
  cmd(paste0("icf", s, "RateControl"), sprintf("%.2f", row$rate_control))
  cmd(paste0("icf", s, "CATE"), format_cate(row$iptw_cate))
  if ("ci_lower" %in% names(row) && !is.na(row$ci_lower)) {
    cmd(paste0("icf", s, "CILower"), sprintf("%.2f", row$ci_lower))
    cmd(paste0("icf", s, "CIUpper"), sprintf("%.2f", row$ci_upper))
  }
}
lines <- c(lines, "")

comment("=== CATE summary ===")
max_cate <- max(cate_df$iptw_cate, na.rm = TRUE)
min_cate <- min(cate_df$iptw_cate, na.rm = TRUE)
if (any(is.na(cate_df$iptw_cate))) {
  warning(sprintf("CATE summary: %d of %d subgroups have NA iptw_cate; max/min/ratio computed over the rest.",
                  sum(is.na(cate_df$iptw_cate)), nrow(cate_df)))
}
cmd("icfCATEmax", format_cate(max_cate))
cmd("icfCATEmin", format_cate(min_cate))
if (is.finite(max_cate) && is.finite(min_cate) &&
    min_cate != 0 && sign(max_cate) == sign(min_cate)) {
  cmd("icfCATEratio", sprintf("%.1f", max_cate / min_cate))
} else {
  cmd("icfCATEratio", "NA")
}
lines <- c(lines, "")

# Per-depth sensitivity macros (\icfDTwoNSubgroups, \icfDThreeVoteFreq,
# \icfDFourSGACATE, ...) so the thesis can present a table or figure for
# each candidate depth without re-deriving values by hand. LaTeX macro
# names cannot contain digits, so depth indices are spelled out.
depth_word <- function(d) c("Two", "Three", "Four", "Five", "Six")[d - 1]
if (!is.null(results$all_depth_results)) {
  comment("=== Per-depth sensitivity values (D2..D5) ===")
  for (d in config$depths) {
    dname <- paste0("D", d)
    ar <- results$all_depth_results[[dname]]
    if (is.null(ar) || is.null(ar$cate) || nrow(ar$cate) == 0) next
    prefix <- paste0("icfD", depth_word(d))
    fr <- ar$final_result
    cmd(paste0(prefix, "NSubgroups"), nrow(ar$cate))
    if (!is.null(fr)) {
      cmd(paste0(prefix, "VotedStructure"), fr$voted_structure)
      cmd(paste0(prefix, "VoteFreq"), sprintf("%.2f", fr$vote_frequency))
      if (!is.null(fr$vote_distribution) && nrow(fr$vote_distribution) > 0) {
        cmd(paste0(prefix, "NDistinctStruct"), nrow(fr$vote_distribution))
      }
    }
    cmd(paste0(prefix, "MeanCVMse"), sprintf("%.6f", results$mean_cv_mse[dname]))
    if (!is.null(results$diagnostics$cv_mse_fold_sd) &&
        dname %in% names(results$diagnostics$cv_mse_fold_sd)) {
      cmd(paste0(prefix, "CVMseSD"),
          sprintf("%.6f", results$diagnostics$cv_mse_fold_sd[[dname]]))
    }
    # Depth-distribution hit rate: fraction of diagnostic forests that
    # achieved the target depth exactly. The "remainder" -- forests that
    # finished off-target -- is split between shallower (truncated) and
    # deeper (grew further) buckets and exposed as two extra macros.
    if (!is.null(results$depth_diagnostics) &&
        dname %in% names(results$depth_diagnostics)) {
      ad <- results$depth_diagnostics[[dname]]$actual_depths
      if (length(ad) > 0) {
        hit_pct      <- 100 * mean(ad == d)
        shallow_pct  <- 100 * mean(ad <  d)
        deep_pct     <- 100 * mean(ad >  d)
        cmd(paste0(prefix, "HitRate"),       sprintf("%.0f", hit_pct))
        cmd(paste0(prefix, "ShallowerPct"),  sprintf("%.0f", shallow_pct))
        cmd(paste0(prefix, "DeeperPct"),     sprintf("%.0f", deep_pct))
      }
    }
    for (i in seq_len(nrow(ar$cate))) {
      s <- sg_suffix(i)
      row <- ar$cate[i, ]
      cmd(paste0(prefix, s, "N"), format_n(row$n_total))
      cmd(paste0(prefix, s, "NTreated"), format_n(row$n_treated))
      cmd(paste0(prefix, s, "NControl"), format_n(row$n_control))
      cmd(paste0(prefix, s, "RateTreated"), sprintf("%.2f", row$rate_treated))
      cmd(paste0(prefix, s, "RateControl"), sprintf("%.2f", row$rate_control))
      # Crude RD with Wald 95% CI (difference of two proportions).
      # rate_* are in percent, so divide by 100 to get proportions.
      p1 <- row$rate_treated / 100
      p2 <- row$rate_control / 100
      n1 <- row$n_treated; n2 <- row$n_control
      crude_rd <- (p1 - p2) * 100   # pp
      se_rd    <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2) * 100
      cmd(paste0(prefix, s, "CrudeRD"),      format_cate(crude_rd))
      cmd(paste0(prefix, s, "CrudeRDLower"), sprintf("%.2f", crude_rd - 1.96 * se_rd))
      cmd(paste0(prefix, s, "CrudeRDUpper"), sprintf("%.2f", crude_rd + 1.96 * se_rd))
      cmd(paste0(prefix, s, "CATE"), format_cate(row$iptw_cate))
      if ("ci_lower" %in% names(row) && !is.na(row$ci_lower)) {
        cmd(paste0(prefix, s, "CILower"), sprintf("%.2f", row$ci_lower))
        cmd(paste0(prefix, s, "CIUpper"), sprintf("%.2f", row$ci_upper))
      }
    }
  }
  lines <- c(lines, "")
}

writeLines(lines, file.path(output_dir, "icf_values.tex"))
cat("Saved: icf_values.tex\n")

# =============================================================================
# GENERATE icf_varimp_list.tex
# =============================================================================

cat("Generating icf_varimp_list.tex...\n")

# Match the count to the number of variables actually retained by the
# above-mean-VI selection step (results$var_names). Previously hardcoded to
# 6, which silently dropped one variable when the selection retained 7.
n_top <- min(length(results$var_names), nrow(vi_df))
vi_top <- vi_df[seq_len(n_top), ]

vi_lines <- "% Auto-generated by 04_export_latex.R -- do not edit manually"
vi_lines <- c(vi_lines, "\\begin{enumerate}")
for (i in seq_len(n_top)) {
  var <- vi_top$variable[i]
  imp <- vi_top$importance[i]
  label <- if (var %in% names(var_labels)) var_labels[var] else var
  vi_lines <- c(vi_lines, sprintf("    \\item %s (importance: %.3f)", label, imp))
}
vi_lines <- c(vi_lines, "\\end{enumerate}")

writeLines(vi_lines, file.path(output_dir, "icf_varimp_list.tex"))
cat("Saved: icf_varimp_list.tex\n")

# =============================================================================
# GENERATE icf_subgroups_table.tex
# =============================================================================

cat("Generating icf_subgroups_table.tex...\n")

format_cate_with_ci <- function(cate_val, ci_lo, ci_hi) {
  point <- format_cate(cate_val)
  if (!is.na(ci_lo) && !is.na(ci_hi)) {
    sprintf("%s (%.2f, %.2f)", point, ci_lo, ci_hi)
  } else {
    point
  }
}

tbl_lines <- character()
tbl_lines <- c(tbl_lines, "% iCF subgroups")
for (i in seq_len(nrow(cate_df))) {
  row <- cate_df[i, ]
  definition <- parse_subgroup_label(row$label)
  ci_lo <- if ("ci_lower" %in% names(row)) row$ci_lower else NA
  ci_hi <- if ("ci_upper" %in% names(row)) row$ci_upper else NA
  tbl_lines <- c(tbl_lines, sprintf(
    "%s & %s & %.2f & %.2f & %s \\\\",
    definition, format_n(row$n_total),
    row$rate_treated, row$rate_control,
    format_cate_with_ci(row$iptw_cate, ci_lo, ci_hi)
  ))
}

tbl_lines <- c(tbl_lines, "\\bottomrule")

writeLines(tbl_lines, file.path(output_dir, "icf_subgroups_table.tex"))
cat("Saved: icf_subgroups_table.tex\n")

# =============================================================================
# GENERATE icf_vi_stability_table.tex
# =============================================================================
# Per-variable selection_freq / median rank / rank IQR across the n_vi_seeds
# refits in step 1's vi_stability diagnostic. Top N variables by selection_freq;
# columns mirror what step3b prints to the log.

if (!is.null(results$diagnostics$vi_stability$per_variable)) {
  cat("Generating icf_vi_stability_table.tex...\n")
  vs <- results$diagnostics$vi_stability
  pv <- vs$per_variable
  # Already sorted (selection_freq desc, then median_rank asc); keep top-10.
  n_top_vs <- min(10L, nrow(pv))
  pv <- pv[seq_len(n_top_vs), ]

  vs_lines <- c(
    "% Auto-generated by 04_export_latex.R -- do not edit manually",
    sprintf("%% Across n_seeds = %d raw-CF refits.", vs$n_seeds),
    sprintf("%% Mean off-diagonal Jaccard of selection sets: %.2f", vs$mean_offdiag_jaccard)
  )
  for (i in seq_len(nrow(pv))) {
    var <- pv$variable[i]
    label <- if (var %in% names(var_labels)) var_labels[[var]] else paste0("\\texttt{", gsub("_", "\\\\_", var), "}")
    vs_lines <- c(vs_lines, sprintf(
      "%s & \\texttt{%s} & %.2f & %g & %g \\\\",
      label, gsub("_", "\\\\_", var),
      pv$selection_freq[i], pv$median_rank[i], pv$rank_iqr[i]
    ))
  }
  vs_lines <- c(vs_lines, "\\bottomrule")
  writeLines(vs_lines, file.path(output_dir, "icf_vi_stability_table.tex"))
  cat("Saved: icf_vi_stability_table.tex\n")

  # Macros for the surrounding prose: n_seeds and the mean off-diagonal Jaccard.
  cmd("icfViStabNSeeds", vs$n_seeds)
  cmd("icfViStabJaccard", sprintf("%.2f", vs$mean_offdiag_jaccard))
  # Variables always selected (selection_freq == 1) and the churning set.
  always <- pv$variable[pv$selection_freq == 1]
  cmd("icfViStabAlwaysCount", length(always))
  cmd("icfViStabAlwaysList",
      paste0("\\texttt{", gsub("_", "\\\\_", always), "}", collapse = ", "))
  # Re-write icf_values.tex with the appended macros.
  writeLines(lines, file.path(output_dir, "icf_values.tex"))
  cat("Re-saved: icf_values.tex (added VI-stability macros)\n")
}

# =============================================================================
# DONE
# =============================================================================

cat("\n=== LaTeX Export Complete ===\n")
cat("Files saved to:", output_dir, "\n")

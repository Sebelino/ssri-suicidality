# 05_export_latex.R
# Export hdiCF results as LaTeX macros and table fragments
#
# Outputs (in output/):
#   hdicf_values.tex          - \newcommand definitions for all numeric values
#   hdicf_varimp_list.tex     - Top variable importance as \begin{enumerate}
#   hdicf_subgroups_table.tex - Subgroup table rows for \input

library(here)
here::i_am("suicidality/analysis-hdicf/05_export_latex.R")

# =============================================================================
# LOAD RESULTS
# =============================================================================

output_dir <- here("suicidality", "analysis-hdicf", "output")
results <- readRDS(file.path(output_dir, "icf_results.rds"))
config <- readRDS(file.path(output_dir, "config.rds"))
vi_df <- read.csv(file.path(output_dir, "variable_importance.csv"))

# Headline depth selection: prefer the 1-SE-rule result (results$reported_depth)
# when present and different from the CV-argmin; otherwise fall back to the
# CV-argmin. The Wang 2024 CV-argmin remains in results$best_depth for
# transparency and is reported via \hdicfCVMinDepth below.
`%||%` <- function(a, b) if (is.null(a)) b else a
reported_depth <- results$reported_depth %||% results$best_depth
reported_dname <- paste0("D", reported_depth)
cv_min_depth   <- results$best_depth
is_flat <- !is.null(results$diagnostics$one_se) && isTRUE(results$diagnostics$one_se$is_flat)

if (reported_depth == cv_min_depth) {
  cat(sprintf("Headline depth: D%d (CV-argmin). Using results$cate.\n", reported_depth))
  cate_df <- results$cate
  reported_n_subgroups <- results$n_subgroups
  reported_voted_structure <- results$final_result$voted_structure
  reported_vote_frequency  <- results$final_result$vote_frequency
} else {
  cat(sprintf("Headline depth: D%d (1-SE rule; CV-argmin was D%d). Using all_depth_results$%s.\n",
              reported_depth, cv_min_depth, reported_dname))
  ar <- results$all_depth_results[[reported_dname]]
  if (is.null(ar) || is.null(ar$cate) || nrow(ar$cate) == 0) {
    stop(sprintf("all_depth_results$%s missing or empty -- cannot export 1-SE headline depth.", reported_dname))
  }
  cate_df <- ar$cate
  reported_n_subgroups <- ar$final_result$n_subgroups
  reported_voted_structure <- ar$final_result$voted_structure
  reported_vote_frequency  <- ar$final_result$vote_frequency
}

# =============================================================================
# VARIABLE NAME MAPPING
# =============================================================================

# Human-readable labels for demographic variable names
var_labels <- c(
  age = "Age",
  age_cat = "Age group",
  year = "Calendar year",
  female = "Sex",
  edufam_cat = "Parental education",
  source = "Care setting",
  inc_cat = "Family income",
  fh_suicidal = "Family history of suicidal behavior",
  fh_depr = "Family history of depression",
  hosp = "Prior psychiatric hospitalization",
  diag_organic = "Prior organic mental disorder",
  diag_alcohol = "Prior alcohol use disorder",
  diag_sud = "Prior substance use disorder",
  diag_psychotic = "Prior psychotic disorder",
  diag_bipolar = "Prior bipolar disorder",
  diag_mdd = "Prior major depressive disorder",
  diag_phobic = "Prior phobic anxiety",
  diag_anxiety_other = "Prior anxiety disorder",
  diag_ocd = "Prior OCD",
  diag_stress = "Prior stress-related disorder",
  diag_anorexia = "Prior anorexia",
  diag_bulimia = "Prior bulimia",
  diag_sleep = "Prior sleep disorder",
  diag_personality_cluster_b = "Prior cluster B personality disorder",
  diag_intellectual_disability = "Prior intellectual disability",
  diag_autism = "Prior autism spectrum disorder",
  diag_adhd = "Prior ADHD",
  diag_conduct = "Prior conduct disorder",
  diag_overdose = "Prior overdose/poisoning",
  diag_suicidal = "Prior suicidal behavior",
  med_antipsychotic = "Prior antipsychotic use",
  med_hypnotic = "Prior hypnotic/sedative use",
  med_benzodiazepine = "Prior benzodiazepine use",
  med_antiepileptic = "Prior antiepileptic use",
  med_stimulant = "Prior stimulant use",
  med_opioid = "Prior opioid use",
  med_mood_stabilizer = "Prior mood stabilizer use",
  med_addiction = "Prior addiction medication use"
)

# Condition-to-LaTeX mapping for subgroup labels
condition_to_latex <- function(cond) {
  # female>0 -> Female, female<=0 -> Male
  if (grepl("^female>0$", cond)) return("Female")
  if (grepl("^female<=0$", cond)) return("Male")
  # age_cat: 0=Children (6-11), 1=Adolescents (12-17), 2=Young adults (18-24)
  if (grepl("^age_cat", cond)) {
    m <- regmatches(cond, regexec("^age_cat(<=|>)(.+)$", cond))[[1]]
    if (length(m) == 3) {
      op <- m[2]
      val <- as.numeric(m[3])
      if (op == "<=" && val == 0) return("age 6--11")
      if (op == "<=" && val == 1) return("age 6--17")
      if (op == ">"  && val == 0) return("age 12--24")
      if (op == ">"  && val == 1) return("age 18--24")
      return(paste0("Age group ", op, val))
    }
  }
  # inc_cat: 1=<0, 2==0, 3=1st-20th pct, 4=20th-80th pct, 5=>80th pct, 9=missing
  # The 9-sentinel for missing means a split at 4.5 puts both quintile 5
  # AND missing-income patients on the ">4" side. Render with this in mind.
  if (grepl("^inc_cat", cond)) {
    m <- regmatches(cond, regexec("^inc_cat(<=|>)(.+)$", cond))[[1]]
    if (length(m) == 3) {
      op <- m[2]
      val <- as.numeric(m[3])
      if (op == "<=" && val == 4) return("Family income below top 20\\%")
      if (op == ">"  && val == 4) return("Family income top 20\\% or missing")
      if (op == "<=" && val == 3) return("Family income below 20th percentile")
      if (op == ">"  && val == 3) return("Family income above 20th percentile or missing")
      # Generic fallback for other thresholds
      sym <- if (op == "<=") "$\\leq$" else "$>$"
      return(paste0("Family income ", sym, val))
    }
  }
  # HD feature names with dot notation: dx.inp.XXX, dx.out.XXX, rx.XXXXX
  if (grepl("^dx\\.inp\\.", cond)) {
    # Extract: dx.inp.F32<=0 -> var=dx.inp.F32, op, val
    m <- regmatches(cond, regexec("^(dx\\.inp\\.[A-Z0-9]+)(<=|>)(.+)$", cond))[[1]]
    if (length(m) == 4) {
      code <- sub("^dx\\.inp\\.", "", m[2])
      op <- if (m[3] == "<=") "$\\leq$" else "$>$"
      return(paste0("Inpatient: ", code, " ", op, m[4]))
    }
  }
  if (grepl("^dx\\.out\\.", cond)) {
    m <- regmatches(cond, regexec("^(dx\\.out\\.[A-Z0-9]+)(<=|>)(.+)$", cond))[[1]]
    if (length(m) == 4) {
      code <- sub("^dx\\.out\\.", "", m[2])
      op <- if (m[3] == "<=") "$\\leq$" else "$>$"
      return(paste0("Outpatient: ", code, " ", op, m[4]))
    }
  }
  if (grepl("^rx\\.", cond)) {
    m <- regmatches(cond, regexec("^(rx\\.[A-Z0-9]+)(<=|>)(.+)$", cond))[[1]]
    if (length(m) == 4) {
      code <- sub("^rx\\.", "", m[2])
      op <- if (m[3] == "<=") "$\\leq$" else "$>$"
      return(paste0("Prescription: ", code, " ", op, m[4]))
    }
  }
  # General case: var<=X -> Label $\leq$X
  m <- regmatches(cond, regexec("^(\\w+)(<=|>)(.+)$", cond))[[1]]
  if (length(m) == 4) {
    var <- m[2]
    op <- if (m[3] == "<=") "$\\leq$" else "$>$"
    val <- m[4]
    label <- if (var %in% names(var_labels)) var_labels[var] else var
    return(paste0(label, " ", op, val))
  }
  cond
}

# Parse SG_cond1_cond2_... into human-readable definition. Conditions are
# joined by commas; long labels rely on the table's paragraph column type
# (p{...}) to wrap across lines automatically.
parse_subgroup_label <- function(label) {
  stripped <- sub("^SG_", "", label)
  # Split into conditions: each is var<=val or var>val
  conditions <- strsplit(stripped, "(?<=\\d)_(?=[a-z])", perl = TRUE)[[1]]
  parts <- vapply(conditions, condition_to_latex, character(1))
  paste(parts, collapse = ", ")
}

# HD feature label fallback for variable importance
hd_var_label <- function(var) {
  if (var %in% names(var_labels)) return(var_labels[var])
  if (grepl("^dx\\.inp\\.", var)) return(paste0("Inpatient: ", sub("^dx\\.inp\\.", "", var)))
  if (grepl("^dx\\.out\\.", var)) return(paste0("Outpatient: ", sub("^dx\\.out\\.", "", var)))
  if (grepl("^rx\\.", var)) return(paste0("Prescription: ", sub("^rx\\.", "", var)))
  var
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
  if (cate >= 0) sprintf("+%.2f", cate) else sprintf("%.2f", cate)
}

# LaTeX-safe command name suffix: SGA, SGB, SGC, ...
sg_suffix <- function(i) {
  paste0("SG", LETTERS[i])
}

# =============================================================================
# GENERATE hdicf_values.tex
# =============================================================================

cat("Generating hdicf_values.tex...\n")

lines <- character()
cmd <- function(name, value) {
  lines <<- c(lines, sprintf("\\newcommand{\\%s}{%s}", name, value))
}
comment <- function(text) {
  lines <<- c(lines, paste0("% ", text))
}

comment("Auto-generated by 05_export_latex.R -- do not edit manually")
comment("")

comment("=== Configuration ===")
cmd("hdicfKFolds", config$K)
cmd("hdicfNTrees", config$n_trees)
cmd("hdicfNIterationsCV", config$n_iterations)
cmd("hdicfNIterationsFinal", config$n_iterations_final %||% config$n_iterations)
cmd("hdicfPThreshold", config$p_threshold)
lines <- c(lines, "")

comment("=== Heterogeneity test ===")
cmd("hdicfHetPvalue", format_pvalue_latex(results$het_p_value))
lines <- c(lines, "")

comment("=== Variable selection ===")
cmd("hdicfNSelectedVars", length(results$var_names))
lines <- c(lines, "")

comment("=== HD feature counts ===")
hd_path <- here("suicidality", "analysis-hdicf", "data", "hd_features.rds")
if (file.exists(hd_path)) {
  hd_feats <- setdiff(names(readRDS(hd_path)), "lopnr")
  cmd("hdicfNHDInp",      sum(startsWith(hd_feats, "dx.inp.")))
  cmd("hdicfNHDOut",      sum(startsWith(hd_feats, "dx.out.")))
  cmd("hdicfNHDRx",       sum(startsWith(hd_feats, "rx.")))
  cmd("hdicfNHDFeatures", length(hd_feats))
}
lines <- c(lines, "")

comment("=== Cross-validation ===")
# The "reported" depth (\hdicfBestDepth) is the headline depth used for the
# main tree, table, and figures. Under the 1-SE rule it may differ from the
# CV-argmin (\hdicfCVMinDepth). \hdicfDepthIsFlat is "true" iff the 1-SE rule
# fires (more than one depth within 1 SE of CV-min).
cmd("hdicfBestDepth", reported_depth)
cmd("hdicfCVMinDepth", cv_min_depth)
cmd("hdicfNSubgroups", reported_n_subgroups)
cmd("hdicfDepthIsFlat", if (is_flat) "true" else "false")
if (!is.null(results$diagnostics$one_se)) {
  ose <- results$diagnostics$one_se
  cmd("hdicfOneSEPlateau", paste0("D", ose$plateau, collapse = ", "))
  cmd("hdicfOneSEValue", sprintf("%.2e", ose$se))
}
if (!is.null(results$diagnostics$cv_mse_relrange)) {
  cmd("hdicfCVMseRelRange", sprintf("%.2e", results$diagnostics$cv_mse_relrange))
}
cmd("hdicfVotedStructure", reported_voted_structure)
cmd("hdicfVoteFrequency", sprintf("%.2f", reported_vote_frequency))
lines <- c(lines, "")

comment("=== Depth-distribution hit rates ===")
if (!is.null(results$depth_diagnostics)) {
  depth_word <- c(`2` = "Two", `3` = "Three", `4` = "Four", `5` = "Five")
  for (d in c(2, 3, 4, 5)) {
    dname <- paste0("D", d)
    dd <- results$depth_diagnostics[[dname]]
    if (!is.null(dd) && !is.null(dd$actual_depths)) {
      hit <- mean(dd$actual_depths == d) * 100
      cmd(paste0("hdicfD", depth_word[as.character(d)], "HitRate"), sprintf("%.0f", hit))
    }
  }
}
lines <- c(lines, "")

comment("=== Per-subgroup values (ordered by subgroup_id) ===")
for (i in seq_len(nrow(cate_df))) {
  s <- sg_suffix(i)
  row <- cate_df[i, ]
  comment(paste0(s, ": ", row$label))
  cmd(paste0("hdicf", s, "N"), format_n(row$n_total))
  cmd(paste0("hdicf", s, "NTreated"), format_n(row$n_treated))
  cmd(paste0("hdicf", s, "NControl"), format_n(row$n_control))
  cmd(paste0("hdicf", s, "RateTreated"), sprintf("%.2f", row$rate_treated))
  cmd(paste0("hdicf", s, "RateControl"), sprintf("%.2f", row$rate_control))
  cmd(paste0("hdicf", s, "CATE"), format_cate(row$iptw_cate))
  if ("ci_lower" %in% names(row) && !is.na(row$ci_lower)) {
    cmd(paste0("hdicf", s, "CILower"), sprintf("%.2f", row$ci_lower))
    cmd(paste0("hdicf", s, "CIUpper"), sprintf("%.2f", row$ci_upper))
  }
}
lines <- c(lines, "")

comment("=== CATE summary ===")
cmd("hdicfCATEmax", format_cate(max(cate_df$iptw_cate)))
cmd("hdicfCATEmin", format_cate(min(cate_df$iptw_cate)))
min_cate <- min(cate_df$iptw_cate)
if (min_cate != 0 && sign(max(cate_df$iptw_cate)) == sign(min_cate)) {
  cate_ratio <- max(cate_df$iptw_cate) / min_cate
  cmd("hdicfCATEratio", sprintf("%.1f", cate_ratio))
} else {
  cmd("hdicfCATEratio", "NA")
}
lines <- c(lines, "")

# Per-depth sensitivity macros (\hdicfDTwoNSubgroups, \hdicfDThreeVoteFreq,
# \hdicfDFourSGACATE, ...) so the thesis can present a table or figure for
# each candidate depth without re-deriving values by hand. LaTeX macro names
# cannot contain digits, so depth indices are spelled out.
depth_word <- function(d) c("Two", "Three", "Four", "Five", "Six")[d - 1]
if (!is.null(results$all_depth_results)) {
  comment("=== Per-depth sensitivity values (D2..D5) ===")
  for (d in config$depths) {
    dname <- paste0("D", d)
    ar <- results$all_depth_results[[dname]]
    if (is.null(ar) || is.null(ar$cate) || nrow(ar$cate) == 0) next
    prefix <- paste0("hdicfD", depth_word(d))
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
    for (i in seq_len(nrow(ar$cate))) {
      s <- sg_suffix(i)
      row <- ar$cate[i, ]
      cmd(paste0(prefix, s, "N"), format_n(row$n_total))
      cmd(paste0(prefix, s, "NTreated"), format_n(row$n_treated))
      cmd(paste0(prefix, s, "NControl"), format_n(row$n_control))
      cmd(paste0(prefix, s, "RateTreated"), sprintf("%.2f", row$rate_treated))
      cmd(paste0(prefix, s, "RateControl"), sprintf("%.2f", row$rate_control))
      cmd(paste0(prefix, s, "CATE"), format_cate(row$iptw_cate))
      if ("ci_lower" %in% names(row) && !is.na(row$ci_lower)) {
        cmd(paste0(prefix, s, "CILower"), sprintf("%.2f", row$ci_lower))
        cmd(paste0(prefix, s, "CIUpper"), sprintf("%.2f", row$ci_upper))
      }
    }
  }
  lines <- c(lines, "")
}

writeLines(lines, file.path(output_dir, "hdicf_values.tex"))
cat("Saved: hdicf_values.tex\n")

# =============================================================================
# GENERATE hdicf_varimp_list.tex
# =============================================================================

cat("Generating hdicf_varimp_list.tex...\n")

# Match the count to the number of variables actually retained by the
# top-10% selection step (results$var_names). Previously hardcoded to 6,
# which silently dropped variables when the selection retained more.
n_top <- min(length(results$var_names), nrow(vi_df))
vi_top <- vi_df[seq_len(n_top), ]

vi_lines <- "% Auto-generated by 05_export_latex.R -- do not edit manually"
vi_lines <- c(vi_lines, "\\begin{enumerate}")
for (i in seq_len(n_top)) {
  var <- vi_top$variable[i]
  imp <- vi_top$importance[i]
  label <- hd_var_label(var)
  vi_lines <- c(vi_lines, sprintf("    \\item %s (importance: %.3f)", label, imp))
}
vi_lines <- c(vi_lines, "\\end{enumerate}")

writeLines(vi_lines, file.path(output_dir, "hdicf_varimp_list.tex"))
cat("Saved: hdicf_varimp_list.tex\n")

# =============================================================================
# GENERATE hdicf_subgroups_table.tex
# =============================================================================

cat("Generating hdicf_subgroups_table.tex...\n")

format_cate_with_ci <- function(cate_val, ci_lo, ci_hi) {
  point <- format_cate(cate_val)
  if (!is.na(ci_lo) && !is.na(ci_hi)) {
    sprintf("%s (%.2f, %.2f)", point, ci_lo, ci_hi)
  } else {
    point
  }
}

tbl_lines <- character()
tbl_lines <- c(tbl_lines, "% hdiCF subgroups")
for (i in seq_len(nrow(cate_df))) {
  row <- cate_df[i, ]
  definition <- parse_subgroup_label(row$label)
  ci_lo <- if ("ci_lower" %in% names(row)) row$ci_lower else NA
  ci_hi <- if ("ci_upper" %in% names(row)) row$ci_upper else NA
  tbl_lines <- c(tbl_lines, sprintf(
    "hdiCF SG%d & %s & %s & %.2f & %.2f & %s \\\\",
    i, definition, format_n(row$n_total),
    row$rate_treated, row$rate_control,
    format_cate_with_ci(row$iptw_cate, ci_lo, ci_hi)
  ))
}

tbl_lines <- c(tbl_lines, "\\bottomrule")

writeLines(tbl_lines, file.path(output_dir, "hdicf_subgroups_table.tex"))
cat("Saved: hdicf_subgroups_table.tex\n")

# =============================================================================
# DONE
# =============================================================================

cat("\n=== LaTeX Export Complete ===\n")
cat("Files saved to:", output_dir, "\n")

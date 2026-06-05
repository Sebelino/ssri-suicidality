# 04_validate_subgroups.R
# Stratified validation of iCF subgroups
#
# Assigns patients to iCF-discovered subgroups and runs the standard
# IPW-weighted Kaplan-Meier analysis (same method as ITT_12wks.R)
# within each subgroup as a sanity check.
#
# Compares the target-trial RDs with the iCF CATE estimates.

required_packages <- c("survival", "dplyr", "here")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
  library(pkg, character.only = TRUE)
}
here::i_am("suicidality/analysis-icf/04_validate_subgroups.R")

# Source shared utilities
source(here("suicidality", "analysis", "common.R"))

# Inline assign_subgroups (copied from icf/core.R) so that this script can run
# locally without grf installed. core.R imports grf at the top of the file,
# but only walks a discrete tree -- no grf calls are needed for validation.
assign_subgroups <- function(X, splits, var_names) {
  if (is.null(splits) || nrow(splits) == 0) {
    return(list(subgroup_id = rep(1L, nrow(X)),
                subgroup_labels = "SG_all",
                n_subgroups = 1L))
  }
  n_obs <- nrow(X)
  subgroup <- rep("root", n_obs)
  traversal_order <- character(0)
  assign_node <- function(obs_idx, node_id, path) {
    node_split <- splits[splits$node_id == node_id, ]
    if (nrow(node_split) == 0) {
      subgroup[obs_idx] <<- path
      traversal_order <<- c(traversal_order, path)
      return()
    }
    var_name <- node_split$variable[1]
    split_val <- node_split$split_value[1]
    var_idx <- which(var_names == var_name)
    if (length(var_idx) == 0) {
      warning(paste("Variable", var_name, "not found in var_names"))
      subgroup[obs_idx] <<- path
      traversal_order <<- c(traversal_order, path)
      return()
    }
    if (length(var_idx) > 1) var_idx <- var_idx[1]
    x_vals <- X[obs_idx, var_idx, drop = TRUE]
    left_obs <- obs_idx[x_vals <= split_val]
    right_obs <- obs_idx[x_vals > split_val]
    if (length(left_obs) > 0) {
      assign_node(left_obs, node_split$left_child[1],
                  paste0(path, "_", var_name, "<=", round(split_val, 2)))
    }
    if (length(right_obs) > 0) {
      assign_node(right_obs, node_split$right_child[1],
                  paste0(path, "_", var_name, ">", round(split_val, 2)))
    }
  }
  assign_node(seq_len(n_obs), 1, "SG")
  list(subgroup_id = match(subgroup, traversal_order),
       subgroup_labels = traversal_order,
       n_subgroups = length(traversal_order))
}

# =============================================================================
# IPW ANALYSIS FUNCTION (adapted from ITT_12wks.R)
# =============================================================================

analyze_subgroup <- function(data, label, include_female = TRUE) {
  # Compute follow-up time in weeks
  data$t_end <- ceiling((data$fu_end_itt - data$fu_start) / 7)

  # Create combined anxiety variable (matches ITT_12wks.R)
  data$diag_anxiety <- as.integer(data$diag_phobic == 1 | data$diag_anxiety_other == 1)

  # Helpers: when the iCF tree restricts to a single level of a covariate
  # (e.g. fh_depr<=0 makes fh_depr constant = 0 within the subgroup), the
  # corresponding factor term has a single level and breaks glm(). Same problem
  # if the chosen reference level happens not to be present in the stratum.
  # Drop constants entirely, and fall back to as.factor() without relevel when
  # the reference level is missing.
  has_var <- function(x) length(unique(stats::na.omit(x))) >= 2
  factor_term <- function(varname, ref) {
    x <- data[[varname]]
    if (!has_var(x)) return(NULL)
    levs <- as.character(unique(stats::na.omit(x)))
    if (as.character(ref) %in% levs) {
      sprintf("relevel(as.factor(%s), ref='%s')", varname, ref)
    } else {
      sprintf("as.factor(%s)", varname)
    }
  }
  binary_term <- function(varname) {
    if (!has_var(data[[varname]])) return(NULL)
    varname
  }

  demo_terms <- c(if (include_female) binary_term("female"), "age", "year")
  socio_terms <- c(
    factor_term("edufam_cat", "1"),
    factor_term("source", "O"),
    factor_term("inc_cat", "4")
  )
  fh_terms <- c(
    factor_term("fh_suicidal", "0"),
    factor_term("fh_depr", "0")
  )
  binary_vars <- c(
    "hosp",
    "diag_bipolar", "diag_psychotic", "diag_alcohol", "diag_sud",
    "diag_autism", "diag_adhd", "diag_suicidal", "diag_overdose",
    "diag_stress", "diag_anxiety",
    "diag_sleep", "diag_anorexia", "diag_bulimia",
    "diag_ocd", "diag_conduct", "diag_intellectual_disability",
    "diag_personality_cluster_b",
    "med_antipsychotic", "med_hypnotic", "med_benzodiazepine", "med_antiepileptic",
    "med_stimulant", "med_opioid", "med_mood_stabilizer", "med_addiction"
  )
  binary_terms <- unlist(lapply(binary_vars, binary_term))
  all_terms <- c(demo_terms, socio_terms, fh_terms, binary_terms)
  ps_formula <- as.formula(paste("cc ~", paste(all_terms, collapse = " + ")))

  # Fit PS model
  p.denom <- glm(ps_formula, data = data, family = binomial())
  data$pd.cc <- predict(p.denom, type = "response")
  p.num <- glm(cc ~ 1, data = data, family = binomial())
  data$pn.cc <- predict(p.num, type = "response")

  # Stabilized weights, truncated at 99th percentile
  data$sw.a <- ifelse(data$cc == 1,
                      data$pn.cc / data$pd.cc,
                      (1 - data$pn.cc) / (1 - data$pd.cc))
  data$sw.a <- pmin(data$sw.a, quantile(data$sw.a, 0.99, na.rm = TRUE))

  # Weighted KM
  km_fit <- survfit(Surv(t_end, sb12_itt) ~ cc, weights = sw.a,
                    cluster = lopnr, data = data)
  surv_12 <- summary(km_fit, times = 12)

  control_idx <- which(surv_12$strata == "cc=0")
  treated_idx <- which(surv_12$strata == "cc=1")

  risk_control <- (1 - surv_12$surv[control_idx]) * 100
  risk_treated <- (1 - surv_12$surv[treated_idx]) * 100
  se_control <- surv_12$std.err[control_idx] * 100
  se_treated <- surv_12$std.err[treated_idx] * 100

  rd <- risk_treated - risk_control
  rd_se <- sqrt(se_control^2 + se_treated^2)

  n_treated <- sum(data$cc == 1)
  n_control <- sum(data$cc == 0)

  cat(sprintf("  N: %d treated, %d control\n", n_treated, n_control))
  cat(sprintf("  RD: %.2f%% (95%% CI: %.2f%%, %.2f%%)\n",
              rd, rd - 1.96 * rd_se, rd + 1.96 * rd_se))

  list(
    label = label,
    n_treated = n_treated,
    n_control = n_control,
    rd = rd,
    rd_lower = rd - 1.96 * rd_se,
    rd_upper = rd + 1.96 * rd_se
  )
}

# =============================================================================
# LOAD DATA
# =============================================================================

cat("\n")
cat("############################################################\n")
cat("# Stratified Validation of iCF Subgroups\n")
cat("############################################################\n")

# Load full cohort (complete-case)
full_data <- filter_complete_cases(read_rds_file("main_12wks_28.rds"))
cat("Full cohort:", nrow(full_data), "patients\n")

# Load iCF results
output_dir <- here("suicidality", "analysis-icf", "output")
results <- readRDS(file.path(output_dir, "icf_results.rds"))

# Use the 1-SE-reported headline depth (results$reported_depth) when present
# and different from the CV-argmin (results$best_depth); otherwise validate
# against the CV-argmin tree (Wang 2024 default behavior).
`%||%` <- function(a, b) if (is.null(a)) b else a
reported_depth <- results$reported_depth %||% results$best_depth
reported_dname <- paste0("D", reported_depth)
if (!is.null(results$reported_depth) && reported_depth != results$best_depth) {
  cat(sprintf("Validating headline tree at D%d (1-SE rule); CV-argmin was D%d.\n",
              reported_depth, results$best_depth))
  ar <- results$all_depth_results[[reported_dname]]
  if (is.null(ar) || is.null(ar$final_result)) {
    stop(sprintf("all_depth_results$%s missing — cannot validate 1-SE headline depth.",
                 reported_dname))
  }
  synthetic_splits <- ar$final_result$synthetic_splits
  validation_labels <- ar$final_result$subgroup_labels
  validation_n_subgroups <- ar$final_result$n_subgroups
  validation_cate <- ar$cate
} else {
  cat(sprintf("Validating tree at D%d (CV-argmin).\n", reported_depth))
  synthetic_splits <- results$final_result$synthetic_splits
  validation_labels <- results$subgroup_labels
  validation_n_subgroups <- results$n_subgroups
  validation_cate <- results$cate
}

cat("iCF subgroups:", validation_n_subgroups, "\n")
cat("Labels:", paste(validation_labels, collapse = ", "), "\n")

# =============================================================================
# ASSIGN PATIENTS TO SUBGROUPS
# =============================================================================

cat("\n--- Assigning patients to iCF subgroups ---\n")

# Reconstruct the same covariates as 01_prepare_data.R
# We need to apply the same transformations to get matching variable values
cohort <- full_data %>%
  mutate(
    age_cat = case_when(
      age < 12 ~ 0L,
      age < 18 ~ 1L,
      TRUE     ~ 2L
    ),
    # source: 1 = inpatient, 0 = outpatient or other/unknown (must match
    # the iCF prep script's source coding so subgroup membership lines up).
    source_num = as.numeric(source == "S"),
    # Cohort is complete-case (no sentinel-99 values remain), so the
    # ordinal categorical variables map directly to their integer codes.
    edufam_cat  = as.numeric(edufam_cat),
    inc_cat     = as.numeric(inc_cat),
    fh_suicidal = as.numeric(fh_suicidal),
    fh_depr     = as.numeric(fh_depr)
  )

# Build the covariate matrix using the iCF variable names
# (must match var_names from the iCF results)
var_names <- results$var_names

# Map iCF variable names to cohort column names
# Most are identical except 'source' which we renamed to avoid conflict
col_map <- setNames(var_names, var_names)
col_map["source"] <- "source_num"

X <- matrix(NA, nrow = nrow(cohort), ncol = length(var_names))
colnames(X) <- var_names
for (i in seq_along(var_names)) {
  col <- col_map[var_names[i]]
  if (col %in% names(cohort)) {
    X[, i] <- as.numeric(cohort[[col]])
  } else {
    warning("Variable not found in cohort: ", var_names[i])
  }
}

# Assign subgroups (using the headline-depth synthetic splits selected above)
subgroups <- assign_subgroups(
  X,
  synthetic_splits,
  var_names
)

cohort$subgroup_id <- subgroups$subgroup_id
cohort$subgroup_label <- subgroups$subgroup_labels[subgroups$subgroup_id]

cat("\nSubgroup distribution:\n")
cohort %>%
  count(subgroup_id, subgroup_label) %>%
  print()

# Drop patients with NA in subgroup assignment (from missing covariates)
n_before <- nrow(cohort)
cohort <- cohort %>% filter(!is.na(subgroup_id))
n_after <- nrow(cohort)
if (n_before > n_after) {
  cat(sprintf("Dropped %d patients with missing subgroup assignment\n",
              n_before - n_after))
}

# =============================================================================
# RUN STRATIFIED ANALYSIS
# =============================================================================

cat("\n--- Running IPW analysis per subgroup ---\n")

validation_results <- list()

for (sg in sort(unique(cohort$subgroup_id))) {
  sg_label <- subgroups$subgroup_labels[sg]
  sg_data <- cohort %>% filter(subgroup_id == sg)

  cat(sprintf("\n>>> Subgroup %d: %s (n=%d) <<<\n", sg, sg_label, nrow(sg_data)))

  # Run the same IPW-weighted KM analysis as ITT_12wks.R. analyze_subgroup
  # dynamically drops covariates that are constant within the stratum
  # (including `female` when the subgroup is defined by sex), so we always
  # request its inclusion and let the helper handle the constant case.
  result <- tryCatch(
    analyze_subgroup(sg_data, sg_label, include_female = TRUE),
    error = function(e) {
      cat("  ERROR:", e$message, "\n")
      list(label = sg_label, rd = NA, rd_lower = NA, rd_upper = NA)
    }
  )

  validation_results[[sg]] <- result
}

# =============================================================================
# COMPARISON TABLE
# =============================================================================

cat("\n")
cat("############################################################\n")
cat("# Comparison: iCF CATE vs Target-Trial RD\n")
cat("############################################################\n\n")

icf_cate <- validation_cate

cat(sprintf("%-35s | %22s | %22s\n",
            "Subgroup", "iCF CATE (95% CI)", "TT-RD (95% CI)"))
cat(paste(rep("-", 85), collapse = ""), "\n")

for (sg in sort(unique(cohort$subgroup_id))) {
  icf_row <- icf_cate[icf_cate$subgroup_id == sg, ]
  val <- validation_results[[sg]]

  icf_str <- sprintf("%5.2f (%5.2f, %5.2f)",
                     icf_row$iptw_cate, icf_row$ci_lower, icf_row$ci_upper)
  tt_str <- if (!is.na(val$rd)) {
    sprintf("%5.2f (%5.2f, %5.2f)", val$rd, val$rd_lower, val$rd_upper)
  } else {
    "       N/A"
  }

  cat(sprintf("%-35s | %22s | %22s\n", icf_row$label, icf_str, tt_str))
}

cat("\nNote: iCF CATE uses IPTW from causal forest propensity scores.\n")
cat("      TT-RD uses IPW from logistic regression (same as main ITT analysis).\n")

# =============================================================================
# SAVE
# =============================================================================

# Save validation results
saveRDS(validation_results, file.path(output_dir, "validation_results.rds"))
cat("\nSaved: validation_results.rds\n")

# Emit per-subgroup macros so the thesis prose / tables can reference the
# stratified IPW RDs without hardcoding the numbers. Macro suffix matches the
# subgroup ordering used by 04_export_latex.R (SGA, SGB, SGC, ...).
sg_suffix <- function(i) {
  paste0("SG", LETTERS[i])
}
fmt_signed <- function(x) {
  if (is.na(x)) return("NA")
  sprintf("%s%.2f", ifelse(x < 0, "$-$", "+"), abs(x))
}
fmt_ci <- function(x) {
  if (is.na(x)) return("NA")
  sprintf("%s%.2f", ifelse(x < 0, "$-$", ""), abs(x))
}
tex_lines <- c(
  "% Auto-generated by 04_validate_subgroups.R -- do not edit manually",
  sprintf("%% Generated: %s", Sys.time())
)
for (sg in sort(unique(cohort$subgroup_id))) {
  val <- validation_results[[sg]]
  prefix <- paste0("icf", sg_suffix(sg))
  tex_lines <- c(tex_lines,
    sprintf("\\newcommand{\\%sStratIPW}{%s}",      prefix, fmt_signed(val$rd)),
    sprintf("\\newcommand{\\%sStratIPWLower}{%s}", prefix, fmt_ci(val$rd_lower)),
    sprintf("\\newcommand{\\%sStratIPWUpper}{%s}", prefix, fmt_ci(val$rd_upper))
  )
}
validation_tex <- file.path(output_dir, "validation_values.tex")
writeLines(tex_lines, validation_tex)
cat("Saved: validation_values.tex\n")

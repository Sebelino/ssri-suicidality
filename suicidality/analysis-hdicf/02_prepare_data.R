# 02_prepare_data.R
# Prepare data for hdiCF analysis: PS trimming + final dataset
#
# Combines HD features with demographics, trims on propensity score
# common support, removes low-frequency features, and outputs the
# same format as analysis-icf: data.frame(Y, W, covariates...)
#
# Usage: Rscript 02_prepare_data.R
# Input:  data/hd_features.rds, extraction/output/rds/main_12wks_28.rds
# Output: data/icf_data.rds, data/covar_names.rds

library(dplyr)
library(tidyr)
library(grf)
library(here)
here::i_am("suicidality/analysis-hdicf/02_prepare_data.R")

source(here("suicidality", "analysis-icf", "paths.R"))

# =============================================================================
# LOAD DATA
# =============================================================================

cat(sprintf("Loading main cohort (variant: %s)...\n", variant_label()))
source(here("suicidality", "analysis", "common.R"))
raw <- as.data.frame(readRDS(here("suicidality", "extraction", "output",
                                  "rds", "main_12wks_28.rds")))
cohort <- if (variant_label() == "missind") {
  apply_missind_recoding(raw)
} else {
  filter_complete_cases(raw)
}

cat("Loading HD features...\n")
hd_features <- readRDS(hd_features_path())

cat("Cohort size:", nrow(cohort), "\n")
cat("HD features:", ncol(hd_features) - 1, "features for", nrow(hd_features), "patients\n")

# =============================================================================
# EXTRACT OUTCOME, TREATMENT, AND DEMOGRAPHICS
# =============================================================================

cat("\nPreparing base variables...\n")

# Include the same curated covariates as analysis-icf (01_prepare_data.R)
# Demographics
demo_vars <- c("female", "year")

# Socioeconomic
socio_vars <- c("edufam_cat", "source", "inc_cat")

# Family history
fh_vars <- c("fh_suicidal", "fh_depr")

# Hospitalization
hosp_vars <- c("hosp")

# Diagnosis covariates (diag_mdd excluded — zero variance in depression cohort)
diag_vars <- c(
  "diag_bipolar", "diag_psychotic", "diag_alcohol", "diag_sud",
  "diag_autism", "diag_adhd", "diag_suicidal", "diag_overdose",
  "diag_stress", "diag_phobic", "diag_anxiety_other",
  "diag_sleep", "diag_anorexia", "diag_bulimia",
  "diag_ocd", "diag_conduct", "diag_intellectual_disability",
  "diag_personality_cluster_b"
)

# Medication covariates
med_vars <- c(
  "med_antipsychotic", "med_hypnotic", "med_benzodiazepine",
  "med_antiepileptic", "med_stimulant", "med_opioid",
  "med_mood_stabilizer", "med_addiction"
)

# In the missind variant, expose `any_miss` as a splittable covariate so the
# raw CF / VI / voted-tree can flag missingness as an effect modifier if it
# carries signal.
miss_vars <- if (variant_label() == "missind") "any_miss" else character(0)

curated_vars <- c(demo_vars, socio_vars, fh_vars, hosp_vars, diag_vars,
                  med_vars, miss_vars)

base <- cohort %>%
  dplyr::select(
    lopnr,
    Y = sb12_itt,
    W = cc,
    age,
    all_of(curated_vars)
  ) %>%
  dplyr::mutate(
    Y = as.numeric(Y),
    W = as.numeric(W),
    # Categorize age: 0=Children 6-11, 1=Adolescents 12-17, 2=Young adults 18-24
    age_cat = case_when(
      age < 12 ~ 0L,
      age < 18 ~ 1L,
      TRUE     ~ 2L
    ),
    # Convert source to numeric (O=0, S=1, M=2, T=3)
    # source: 1 = inpatient (S), 0 = outpatient (O) or other/unknown (T).
    source = as.integer(source == "S"),
    # Cohort is complete-case (sentinel-99 patients dropped at load), so
    # edufam_cat / inc_cat / fh_suicidal / fh_depr carry only legitimate
    # category integers.
    edufam_cat  = as.integer(edufam_cat),
    inc_cat     = as.integer(inc_cat),
    fh_suicidal = as.integer(fh_suicidal),
    fh_depr     = as.integer(fh_depr)
  ) %>%
  dplyr::select(-age)

# =============================================================================
# JOIN HD FEATURES
# =============================================================================

cat("Joining HD features...\n")

merged <- base %>%
  dplyr::left_join(hd_features, by = "lopnr")

# Fill NAs with 0 for HD feature columns
hd_cols <- setdiff(names(hd_features), "lopnr")
merged[hd_cols] <- lapply(merged[hd_cols], function(x) {
  ifelse(is.na(x), 0L, x)
})

# Drop lopnr
merged <- merged %>% dplyr::select(-lopnr)

cat("Merged dataset:", nrow(merged), "x", ncol(merged), "\n")

# =============================================================================
# PROPENSITY SCORE TRIMMING
# =============================================================================

cat("\n=== Propensity Score Trimming ===\n")

covar_cols <- setdiff(names(merged), c("Y", "W"))
X_all <- as.matrix(merged[, covar_cols])

cat("Fitting regression forest for propensity scores...\n")
ps_forest <- regression_forest(X_all, merged$W, num.trees = 500, seed = 43L)
W.hat <- predict(ps_forest)$predictions

# Common support bounds
ps_treated <- W.hat[merged$W == 1]
ps_control <- W.hat[merged$W == 0]

lower_bound <- max(min(ps_treated), min(ps_control))
upper_bound <- min(max(ps_treated), max(ps_control))

cat(sprintf("PS range treated:  [%.4f, %.4f]\n", min(ps_treated), max(ps_treated)))
cat(sprintf("PS range control:  [%.4f, %.4f]\n", min(ps_control), max(ps_control)))
cat(sprintf("Common support:    [%.4f, %.4f]\n", lower_bound, upper_bound))

keep <- W.hat >= lower_bound & W.hat <= upper_bound
n_trimmed <- sum(!keep)
cat(sprintf("Trimmed: %d patients (%.1f%%)\n", n_trimmed, 100 * n_trimmed / nrow(merged)))

merged <- merged[keep, ]
cat(sprintf("Remaining: %d patients\n", nrow(merged)))

# =============================================================================
# HD FEATURE CONSOLIDATION (Wang et al. 2025)
# =============================================================================

cat("\n=== HD Feature Consolidation ===\n")

# Per hdiCF paper: merge higher ordinal levels into lower ones until each
# level has >= m observations. This preserves features that would otherwise
# be dropped entirely.
min_cell <- 20
n_consolidated <- 0L

for (v in hd_cols) {
  if (!(v %in% names(merged))) next
  # Merge levels top-down: 3 -> 2 -> 1 -> binary (0 vs >=1)
  for (high in c(3L, 2L)) {
    low <- high - 1L
    if (sum(merged[[v]] == high) < min_cell) {
      merged[[v]][merged[[v]] == high] <- low
      n_consolidated <- n_consolidated + 1L
    }
  }
  # If level 1 still too sparse, collapse to binary (0 vs 1)
  if (sum(merged[[v]] == 1) < min_cell && sum(merged[[v]] > 0) >= min_cell) {
    merged[[v]] <- ifelse(merged[[v]] > 0, 1L, 0L)
    n_consolidated <- n_consolidated + 1L
  }
}

# Drop features where all non-zero levels combined are still too sparse
drop_vars <- character()
for (v in hd_cols) {
  if (!(v %in% names(merged))) next
  if (sum(merged[[v]] > 0) < min_cell) {
    drop_vars <- c(drop_vars, v)
  }
}

drop_vars <- unique(drop_vars)
if (length(drop_vars) > 0) {
  merged <- merged[, !(names(merged) %in% drop_vars)]
  cat(sprintf("Dropped %d features with < %d non-zero observations\n",
              length(drop_vars), min_cell))
}
cat(sprintf("Consolidated %d ordinal levels across remaining features\n",
            n_consolidated))

# =============================================================================
# FINALIZE
# =============================================================================

# Ensure all numeric, drop NAs
icf_data <- merged %>%
  dplyr::mutate(across(everything(), as.numeric)) %>%
  as.data.frame()

n_before <- nrow(icf_data)
na_per_col <- colSums(is.na(icf_data))
cols_with_na <- na_per_col[na_per_col > 0]

if (length(cols_with_na) > 0) {
  cat("\nMissing values by column:\n")
  for (col in names(cols_with_na)) {
    cat(sprintf("  %s: %d (%.1f%%)\n", col, cols_with_na[col],
                100 * cols_with_na[col] / n_before))
  }
}

icf_data <- tidyr::drop_na(icf_data)
n_dropped <- n_before - nrow(icf_data)
if (n_dropped > 0) {
  cat(sprintf("Dropped %d rows with NAs\n", n_dropped))
}

covar_names <- setdiff(names(icf_data), c("Y", "W"))

cat(sprintf("\nFinal dataset: %d patients x %d covariates\n",
            nrow(icf_data), length(covar_names)))
cat("Treatment distribution:\n")
print(table(icf_data$W))
cat("\nOutcome distribution:\n")
print(table(icf_data$Y))

# Save
output_dir <- here("suicidality", "analysis-hdicf", "data")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

data_out <- icf_data_path("analysis-hdicf")
saveRDS(icf_data, data_out)
cat("\nSaved:", data_out, "\n")

covar_out <- file.path(output_dir, paste0("covar_names", variant_suffix(), ".rds"))
saveRDS(covar_names, covar_out)
cat("Saved:", covar_out, "\n")

cat("\n=== Data Preparation Complete ===\n")

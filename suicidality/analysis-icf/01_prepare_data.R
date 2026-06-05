# 01_prepare_data.R
# Prepare data for iterative Causal Forest (iCF) analysis
#
# This script loads the SSRI-suicidality cohort data and prepares it
# for use with the iCF algorithm.

# Load required packages
library(dplyr)
library(tidyr)
library(here)
here::i_am("suicidality/analysis-icf/01_prepare_data.R")

source(here("suicidality", "analysis-icf", "paths.R"))

# =============================================================================
# LOAD DATA
# =============================================================================

cat(sprintf("Loading main analysis dataset (variant: %s)...\n", variant_label()))
source(here("suicidality", "analysis", "common.R"))
raw <- as.data.frame(readRDS(here("suicidality", "extraction", "output",
                                  "rds", "main_12wks_28.rds")))

if (variant_label() == "missind") {
  # Sensitivity: keep the full eligible cohort, recode the four sentinel-99
  # variables (force them to their modal reference level when any_miss == 1)
  # and add a single `any_miss` indicator to the iCF feature matrix.
  data <- apply_missind_recoding(raw)
} else {
  data <- filter_complete_cases(raw)
}

cat("Dataset dimensions:", dim(data), "\n")

# =============================================================================
# PREPARE DATA FOR iCF
# =============================================================================
# iCF requires:
# - Y: outcome (binary for suicidal behavior)
# - W: treatment indicator (1 = SSRI, 0 = control)
# - X: covariates (features)
#
# Note: iCF works on risk difference scale. For binary outcomes,
# we use the binary outcome directly and the transformed outcome
# Y* = Y/PS if W=1, -Y/(1-PS) if W=0 is computed internally.

prepare_icf_data <- function(data) {

  # Define covariate list matching the existing analysis
  # Demographics
  # Note: age is categorized into 3 clinical groups (children, adolescents,
  # young adults) per supervisor guidance.
  demo_vars <- c("female", "age_cat", "year")

  # Socioeconomic (convert to factors for categorical handling)
  socio_vars <- c("edufam_cat", "source", "inc_cat")

  # Family history
  fh_vars <- c("fh_suicidal", "fh_depr")

  # Hospitalization
  hosp_vars <- c("hosp")

  # Diagnosis covariates
  # Note: diag_mdd excluded because all cohort members have depression diagnosis
  # (the variable has zero variance and adds no information)
  # Keep anxiety subtypes separate per Table S3:
  # - diag_phobic: Phobic anxiety disorder (F40.0-F40.2)
  # - diag_anxiety_other: Other anxiety disorders (F41.0-F41.1)
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

  # In missind mode, append the `any_miss` indicator as a splittable covariate
  # so the iCF VI/voted-tree can flag missingness as an effect modifier if it
  # carries signal.
  miss_vars <- if (variant_label() == "missind") "any_miss" else character(0)

  all_covars <- c(demo_vars, socio_vars, fh_vars, hosp_vars, diag_vars,
                  med_vars, miss_vars)

  # Build iCF dataset
  # Format: Y, W, X1, X2, ..., Xp
  icf_data <- data %>%
    dplyr::select(
      Y = sb12_itt,           # Outcome: suicidal behavior (ITT)
      W = cc,                 # Treatment: SSRI initiation
      age,                    # Raw age, will be categorized below
      all_of(all_covars[all_covars != "age_cat"])
    ) %>%
    # Handle categorical variables - convert to numeric for iCF.
    # Cohort is complete-case (sentinel-99 patients dropped at load), so
    # edufam_cat / inc_cat / fh_suicidal / fh_depr carry only legitimate
    # category integers and no recoding is needed.
    dplyr::mutate(
      # Categorize age: 0=Children 6-11, 1=Adolescents 12-17, 2=Young adults 18-24
      age_cat = case_when(
        age < 12 ~ 0L,
        age < 18 ~ 1L,
        TRUE     ~ 2L
      ),
      # source: 1 = inpatient (S), 0 = outpatient (O) or other/unknown (T).
      # O and T are merged because T is small (~0.4%) and behaves like O for
      # both PS modelling and the iCF subgroup search; the contrast of interest
      # is inpatient vs. non-inpatient care setting.
      source = as.integer(source == "S"),
      edufam_cat = as.integer(edufam_cat),
      inc_cat    = as.integer(inc_cat),
      fh_suicidal = as.integer(fh_suicidal),
      fh_depr     = as.integer(fh_depr)
    ) %>%
    # Drop raw age (replaced by age_cat)
    dplyr::select(-age) %>%
    # Ensure all variables are numeric
    dplyr::mutate(across(everything(), as.numeric)) %>%
    as.data.frame()

  # Document missingness before dropping NAs
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

  # Remove rows with any NA values (iCF cannot handle missing data)
  icf_data <- tidyr::drop_na(icf_data)
  n_after <- nrow(icf_data)
  n_dropped <- n_before - n_after

  cat(sprintf("\nDropped %d rows (%.1f%%) due to missing values\n",
              n_dropped, 100 * n_dropped / n_before))
  cat(sprintf("Remaining: %d rows\n", n_after))

  return(icf_data)
}

# =============================================================================
# PREPARE AND SAVE DATA
# =============================================================================

cat("\nPreparing data for iCF...\n")
icf_data <- prepare_icf_data(data)

cat("iCF dataset dimensions:", dim(icf_data), "\n")
cat("Treatment distribution:\n")
print(table(icf_data$W))
cat("\nOutcome distribution:\n")
print(table(icf_data$Y))
cat("\nOutcome by treatment:\n")
print(table(icf_data$W, icf_data$Y))

# Calculate event rates
event_rates <- icf_data %>%
  group_by(W) %>%
  summarise(
    n = n(),
    events = sum(Y),
    rate = mean(Y) * 100
  )
cat("\nEvent rates by treatment group:\n")
print(event_rates)

# Save prepared data
output_dir <- here("suicidality", "analysis-icf", "data")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

data_out <- icf_data_path("analysis-icf")
saveRDS(icf_data, data_out)
cat("\nData saved to:", data_out, "\n")

# Also save covariate names for reference (variant-suffixed to match data file)
covar_names <- setdiff(names(icf_data), c("Y", "W"))
saveRDS(covar_names, file.path(output_dir,
                               paste0("covar_names", variant_suffix(), ".rds")))

cat("\n=== Data Preparation Complete ===\n")
cat("Next step: Run 02_run_icf.R\n")

# 01_generate_hd_features.R
# Generate high-dimensional ordinal features from raw ICD-10 + ATC codes
#
# Per Wang et al. 2025 (hdiCF), features are encoded as ordinal variables
# based on within-patient frequency:
#   0 = no occurrence
#   1 = once
#   2 = sporadic (2 to 75th percentile of frequency among those with code)
#   3 = frequent (> 75th percentile)
#
# Creates ordinal features for:
#   - Inpatient diagnoses (3-digit ICD-10, top 200, min 1%, on or before fu_start)
#   - Outpatient diagnoses (3-digit ICD-10, top 200, min 1%, on or before fu_start)
#   - Prescriptions (4th-level ATC, 90-day lookback inclusive of fu_start,
#                    top 200, min 1%, excluding antidepressants N06A*)
#
# Usage: Rscript 01_generate_hd_features.R
# Output: data/hd_features.rds

library(dplyr)
library(tidyr)
library(here)
here::i_am("suicidality/analysis-hdicf/01_generate_hd_features.R")

source(here("suicidality", "analysis-icf", "paths.R"))

# =============================================================================
# LOAD COHORT (for lopnr + baseline date)
# =============================================================================

cat(sprintf("Loading main cohort (variant: %s)...\n", variant_label()))
source(here("suicidality", "analysis", "common.R"))
raw <- as.data.frame(readRDS(here("suicidality", "extraction", "output",
                                  "rds", "main_12wks_28.rds")))
cohort <- if (variant_label() == "missind") {
  # Missind sensitivity uses the full eligible cohort; HD features are
  # computed for every patient with a valid baseline date.
  apply_missind_recoding(raw)
} else {
  filter_complete_cases(raw)
}
cat("Cohort size:", nrow(cohort), "\n")

baseline <- cohort %>%
  dplyr::select(lopnr, fu_start)

# =============================================================================
# ORDINAL ENCODING HELPER
# =============================================================================

#' Encode within-patient frequency as ordinal variable (Wang et al. 2025)
#' 0 = no occurrence, 1 = once, 2 = sporadic, 3 = frequent
#' Sporadic: 2 to 75th percentile; Frequent: > 75th percentile
encode_frequency <- function(count_vec) {
  # count_vec: integer counts per patient (only patients with >= 1 occurrence)
  # Degenerate case: all counts identical — treat as binary presence
  if (length(unique(count_vec)) == 1) {
    return(rep(1L, length(count_vec)))
  }
  p75 <- quantile(count_vec, 0.75)
  # If p75 equals max, nobody would be encoded as "frequent" — fall back to
  # median split so the ordinal variable retains at least two distinct levels.
  if (p75 >= max(count_vec)) {
    p75 <- quantile(count_vec, 0.50)
  }
  dplyr::case_when(
    count_vec <= 1  ~ 1L,
    count_vec <= p75 ~ 2L,
    TRUE             ~ 3L
  )
}

# =============================================================================
# DIAGNOSIS FEATURES
# =============================================================================

cat("\n=== Generating Diagnosis Features ===\n")

raw_dx <- readRDS(here("suicidality", "extraction", "output", "rds", "raw_diagnoses_cohort.rds"))
cat("Raw diagnoses loaded:", nrow(raw_dx), "records\n")

# Filter to pre-baseline diagnoses (inclusive of fu_start, matching curated
# diagnosis covariates in extraction/19_process_cov_diagnoses.R: same-day
# diagnoses precede the SSRI dispensation chronologically and count as baseline)
dx <- raw_dx %>%
  dplyr::inner_join(baseline, by = "lopnr") %>%
  dplyr::filter(diagn_date <= fu_start) %>%
  # Drop chapter-range placeholder codes (e.g. "F00-F09", "F40-F48",
  # "F00-F999") that the Swedish NPR records when no specific ICD-10 code
  # is given. Their 3-char prefix would collide with real codes — e.g.
  # "F40-F48" -> code3 = "F40" would mix with real F40.X phobic anxiety
  # codes, and "F00-F999" -> code3 = "F00" would create a phantom
  # Alzheimer's-disease feature with no actual Alzheimer's patients.
  dplyr::filter(!grepl("-", dia)) %>%
  dplyr::mutate(
    code3 = substr(dia, 1, 3),
    # S=specialist inpatient, O=outpatient, M=primary care, T=therapeutic
    setting = ifelse(source == "S", "inp", "out")
  )

cat("Pre-baseline diagnoses:", nrow(dx), "\n")

n_patients <- nrow(baseline)

generate_dx_features <- function(dx_data, setting_label, max_features = 200, min_prev = 0.01) {
  cat(sprintf("  Processing %s diagnoses...\n", setting_label))

  # Identify codes passing prevalence filter
  dx_sub <- dx_data %>%
    dplyr::filter(setting == setting_label) %>%
    dplyr::distinct(lopnr, code3) %>%
    dplyr::group_by(code3) %>%
    dplyr::summarise(n = n(), .groups = "drop") %>%
    dplyr::mutate(prev = n / n_patients) %>%
    dplyr::filter(prev >= min_prev) %>%
    dplyr::arrange(desc(n)) %>%
    dplyr::slice_head(n = max_features)

  cat(sprintf("    Codes passing prevalence filter: %d\n", nrow(dx_sub)))

  keep_codes <- dx_sub$code3

  # Count per-patient frequency for each code
  freq <- dx_data %>%
    dplyr::filter(setting == setting_label, code3 %in% keep_codes) %>%
    dplyr::group_by(lopnr, code3) %>%
    dplyr::summarise(n = n(), .groups = "drop")

  # Apply ordinal encoding per code
  freq <- freq %>%
    dplyr::group_by(code3) %>%
    dplyr::mutate(value = encode_frequency(n)) %>%
    dplyr::ungroup() %>%
    dplyr::select(lopnr, code3, value)

  # Pivot to wide format
  wide <- freq %>%
    tidyr::pivot_wider(
      id_cols = lopnr,
      names_from = code3,
      values_from = value,
      values_fill = 0L,
      names_prefix = paste0("dx.", setting_label, ".")
    )

  cat(sprintf("    Generated %d features for %d patients\n",
              ncol(wide) - 1, nrow(wide)))
  wide
}

dx_inp <- generate_dx_features(dx, "inp")
dx_out <- generate_dx_features(dx, "out")

# =============================================================================
# PRESCRIPTION FEATURES
# =============================================================================

cat("\n=== Generating Prescription Features ===\n")

raw_rx <- readRDS(here("suicidality", "extraction", "output", "rds", "raw_prescriptions_cohort.rds"))
cat("Raw prescriptions loaded:", nrow(raw_rx), "records\n")

# Filter to 90-day lookback before baseline (inclusive of fu_start, matching
# curated medication covariates in extraction/20_process_cov_medications.R,
# per Lagerberg 2023 "medication receipt within last 3 months").
#
# Exclude antidepressants (ATC N06A*) from HD prescription features. Including
# them would leak treatment into the covariate set: with the inclusive (<=)
# filter, every initiator's first SSRI dispensation falls on fu_start itself,
# so rx.N06AB would be ~100% prevalent among initiators and rare among
# non-initiators -- a near-perfect treatment proxy rather than a baseline
# effect modifier. The curated med_* covariate set is a whitelist that
# already excludes N06A by construction; this filter brings the HD set into
# the same antidepressant-free regime.
rx <- raw_rx %>%
  dplyr::inner_join(baseline, by = "lopnr") %>%
  dplyr::filter(edatum >= fu_start - 90, edatum <= fu_start) %>%
  dplyr::mutate(atc4 = substr(atc, 1, 5)) %>%
  dplyr::filter(substr(atc4, 1, 4) != "N06A")

cat("Prescriptions in 90-day lookback:", nrow(rx), "\n")

# Identify codes passing prevalence filter
rx_codes <- rx %>%
  dplyr::distinct(lopnr, atc4) %>%
  dplyr::group_by(atc4) %>%
  dplyr::summarise(n = n(), .groups = "drop") %>%
  dplyr::mutate(prev = n / n_patients) %>%
  dplyr::filter(prev >= 0.01) %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::slice_head(n = 200)

cat(sprintf("ATC codes passing prevalence filter: %d\n", nrow(rx_codes)))

keep_atc <- rx_codes$atc4

# Count per-patient frequency for each ATC code
rx_freq <- rx %>%
  dplyr::filter(atc4 %in% keep_atc) %>%
  dplyr::group_by(lopnr, atc4) %>%
  dplyr::summarise(n = n(), .groups = "drop")

# Apply ordinal encoding per code
rx_freq <- rx_freq %>%
  dplyr::group_by(atc4) %>%
  dplyr::mutate(value = encode_frequency(n)) %>%
  dplyr::ungroup() %>%
  dplyr::select(lopnr, atc4, value)

# Pivot to wide format
rx_wide <- rx_freq %>%
  tidyr::pivot_wider(
    id_cols = lopnr,
    names_from = atc4,
    values_from = value,
    values_fill = 0L,
    names_prefix = "rx."
  )

cat(sprintf("Generated %d prescription features for %d patients\n",
            ncol(rx_wide) - 1, nrow(rx_wide)))

# =============================================================================
# MERGE AND SAVE
# =============================================================================

cat("\n=== Merging Features ===\n")

hd_features <- baseline %>%
  dplyr::select(lopnr) %>%
  dplyr::left_join(dx_inp, by = "lopnr") %>%
  dplyr::left_join(dx_out, by = "lopnr") %>%
  dplyr::left_join(rx_wide, by = "lopnr")

# Fill NAs with 0 (patient had no records for that code)
feature_cols <- setdiff(names(hd_features), "lopnr")
hd_features[feature_cols] <- lapply(hd_features[feature_cols], function(x) {
  ifelse(is.na(x), 0L, x)
})

cat(sprintf("Final HD features: %d patients x %d features\n",
            nrow(hd_features), length(feature_cols)))

# Save
output_dir <- here("suicidality", "analysis-hdicf", "data")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

hd_out <- hd_features_path()
saveRDS(hd_features, hd_out)
cat("Saved:", hd_out, "\n")

cat("\n=== HD Feature Generation Complete ===\n")

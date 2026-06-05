# Data Analysis — SSRI and Suicidal Behavior Risk

R scripts for analysing the 12-week ITT effect of SSRI initiation on suicidal-behavior risk in Swedish youth (ages 6–24) with a first depression diagnosis. Methodology follows Lagerberg et al. 2023 (target trial emulation with stabilised inverse-probability weights and weighted Kaplan–Meier estimation).

## Study Population

- **Age range:** 6–24 years
- **Inclusion:** First recorded depression diagnosis (ICD-10 F32 or F33, excluding F33.4)
- **Outcome:** Suicidal behavior (ICD-10 X60–X84 intentional self-harm; Y10–Y34 undetermined intent)
- **Treatment:** SSRI (ATC N06AB) initiation within 28 days of diagnosis

## Scripts (run from repo root)

### Headline analyses

- **`ITT_12wks.R`** — Headline intention-to-treat analysis. Stabilised IPW from a logistic propensity-score model, weighted Kaplan-Meier, 12-week risk difference and risk ratio with 95 % CIs. Also runs sex-stratified and age-stratified variants. Outputs survival plots and risk estimates.
- **`Summary_statistics.R`** — Generates Table 2 (`baseline_table.tex`): pre-IPW baseline characteristics by treatment group with SMDs.
- **`export_thesis_values.R`** — Computes ITT estimates and writes all `\newcommand` macros used in `thesis.tex` to `thesis_values.tex` (study N, event counts, RDs, RRs, sex- and age-stratified estimates, study period dates).

### Sensitivity analyses

- **`cate_by_source.R`** — Stratified analyses (outpatient vs. inpatient) plus the two pre-registered sensitivity analyses: cohort with inpatient index diagnoses excluded, and 14-day grace-period reanalysis. Writes `cate_by_source_values.tex` (LaTeX macros).

### Heterogeneity-aware analyses

- **`plot_qini.R`** — Qini curve and AUTOC for the iCF causal forest (AIPW-based; Sverdrup et al. 2025). Reads `analysis-icf/output/icf_results.rds` for `cate_individual`, `W.hat`, `Y.hat`. Writes `qini_12wks.pdf` and `qini_values.tex`.
- **`plot_qini_hdicf.R`** — Same for the hdiCF forest, reading `analysis-hdicf/output/icf_results.rds`. Writes `qini_12wks_hdicf.pdf` and `qini_values_hdicf.tex`.

### Plotting / supplementary

- **`predi_diff_distribution.R`** — Histogram of days from diagnosis to SSRI dispensation among initiators.
- **`plot_cumulative_incidence.R`** — Auxiliary cumulative-incidence plotting.

### Diagnostics / utilities

- **`Prediction_12wks.R`** — Cox regression with and without treatment, for descriptive comparison.
- **`covariate_frequencies_full.R`**, **`f_code_frequencies.R`** — Covariate-prevalence and ICD-10 F-code summaries.
- **`validate_paper_results.R`** — Reproduce key Lagerberg-paper numbers from the cohort.
- **`common.R`** — Shared utility functions (RDS reading, etc.). Sourced by other scripts; not run directly.

## Data dependencies

All scripts read from `suicidality/extraction/output/rds/`:

- `main_12wks_28.rds` — assembled analysis cohort (one row per patient, baseline covariates + 12-week ITT outcome).

The `plot_qini*.R` scripts additionally read `icf_results.rds` from the corresponding pipeline output directory.

## Key variables

- `cc` — treatment indicator (1 = SSRI initiator, 0 = non-initiator)
- `sb12_itt` — suicidal-behavior outcome at 12 weeks (ITT)
- `fu_start`, `fu_end_itt` — follow-up window
- `lopnr` — pseudonymised patient identifier
- `diag_*`, `med_*`, `fh_*`, `edufam_cat`, `inc_cat`, `source`, `hosp` — baseline covariates (see thesis Table 1 for definitions)

## Output

Outputs are written to `suicidality/analysis/output/` (Qini curves, sensitivity-analysis CSVs, supplementary figures). LaTeX exports for the thesis (`baseline_table.tex`, `thesis_values.tex`, `cate_by_source_values.tex`, `qini_values*.tex`) are picked up by `report/Makefile` from this directory.

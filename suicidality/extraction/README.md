# Data Extraction Pipeline

Data extraction and cohort creation for a study on antidepressant initiation and suicidal behavior in youth. The code implements an emulated target trial approach to analyze the relationship between antidepressant treatment initiation and subsequent suicidal behavior.

## Overview

The pipeline creates study cohorts from Swedish registry data for an epidemiological analysis examining youth (ages 6-24) diagnosed with depression between 2006-2019, following them for suicidal behavior after antidepressant initiation.

## Directory Structure

```
extraction/
├── 01_raw_diagnoses_index.R   # First extraction script
├── ...                        # Scripts 02-23
├── 24_process_final_cohorts.R # Final processing script
├── extract_all.R              # Pipeline runner
├── lib/                       # Shared R libraries
└── output/                    # Generated datasets
    └── rds/                   # R-generated files
```

| Path | Description |
|------|-------------|
| `01_*.R` - `24_*.R` | Numbered extraction and processing scripts |
| [`lib/`](lib/) | Shared R libraries |
| [`output/`](output/) | Generated datasets (gitignored) |
| [`../../tools/`](../../tools/) | SLURM batch scripts for HPC execution |

## Quick Start

### Running the Pipeline

```bash
# Run the full extraction pipeline
Rscript extract_all.R

# Preview execution order
Rscript extract_all.R --dry-run

# List all scripts with numbers
Rscript extract_all.R --list

# Resume from a specific step
Rscript extract_all.R --from 5
```

### Pipeline Scripts

Scripts are numbered by execution order. Scripts 01-11 require database access (~100 min total); scripts 12-25 only process RDS files (~2 min total).

| # | Script | Description | Time |
|---|--------|-------------|------|
| 01 | raw_diagnoses_index | Extract depression diagnoses (F32/F33) | ~1 min |
| 02 | raw_prescriptions_all | Extract psychotropic prescriptions | ~19 min |
| 03 | raw_individual_bootstrap | Extract birth dates and sex | ~7 min |
| 04 | define_cohort | Define cohort (age, washout, SSRI, parents) | ~9 min |
| 05 | raw_migration | Extract emigration dates | <1 min |
| 06 | raw_dor | Extract death dates and causes | ~1 min |
| 07 | raw_diagnoses_cohort | Extract diagnoses for cohort | ~4 min |
| 08 | raw_diagnoses_parents | Extract diagnoses for parents | ~35 min |
| 09 | raw_lisa | Extract LISA data (education, income) | ~19 min |
| 10 | raw_hospitalization | Extract hospitalization data | ~2 min |
| 11 | raw_prescriptions_cohort | Extract prescriptions for cohort | ~3 min |
| 12 | process_base | Create base cohort with death/emigration | <1 min |
| 13 | process_outcomes | Identify suicidal behavior outcomes | <1 min |
| 14 | process_censoring | Identify hospitalization censoring | <1 min |
| 15 | process_followup | Create 12-week follow-up cohort | <1 min |
| 16 | process_cov_family_history | Create family history covariates | <1 min |
| 17 | process_cov_education | Create education covariate | <1 min |
| 18 | process_cov_income | Create income covariate | <1 min |
| 19 | process_cov_diagnoses | Create diagnosis covariates | <1 min |
| 20 | process_cov_medications | Create medication covariates | <1 min |
| 21 | process_cov_hospitalizations | Create hospitalization covariate | <1 min |
| 22 | process_covariates_assembly | Assemble all covariates | <1 min |
| 23 | process_time_varying | Create per-protocol cohort | ~1 min |
| 24 | process_final_cohorts | Create final analysis datasets | <1 min |

## Study Design

### Emulated Target Trial

- **Target population**: Youth with new depression diagnosis
- **Intervention**: Initiation of SSRI medication within 28 days
- **Comparator**: No SSRI initiation within 28 days
- **Outcome**: Suicidal behavior (ICD-10 X60-X84, Y10-Y34)
- **Follow-up**: 12 weeks from treatment assignment

### Inclusion Criteria

- Depression diagnosis (ICD-10 F32/F33) between 2006-2019
- Age 6-24 years at diagnosis
- Washout period of >=365 days from previous antidepressant exposure (N06A)

### Treatment Assignment

- **Initiators**: SSRI prescription (N06AB) within 28 days of diagnosis
- **Non-initiators**: No SSRI prescription within 28 days of diagnosis

## Data Sources

The scripts access Swedish national registers through ODBC connections:

- **v_npr_dia** - National Patient Register (diagnoses)
- **v_lmr** - Prescribed Drug Register
- **v_individual** - Individual demographics
- **v_migration** - Migration register
- **v_dor_bas/v_dor_orsak** - Cause of Death Register
- **v_parent** - Multi-Generation Register
- **v_lisa** - LISA (education, income)

## Output Datasets

The pipeline generates analysis-ready cohorts:

| Dataset | Description |
|---------|-------------|
| `main_12wks_28.rds` | 12-week intention-to-treat cohort |
| `pp_12wks_max.rds` | 12-week per-protocol cohort with time-varying exposures |
| `base_cov_28.rds` | Baseline covariates dataset |

## Prerequisites

- R with packages: DBI, odbc, dplyr, tidyr, data.table, here
- Direct database access to Swedish registry databases
- Appropriate permissions and ethical approvals

## Important Notes

- All personal identifiers are pseudonymized (lopnr variables)
- The code implements appropriate privacy protections for registry data
- Methodology follows Lagerberg et al. 2023
- Per-protocol analysis censors at treatment discontinuation

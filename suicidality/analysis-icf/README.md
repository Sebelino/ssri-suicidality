# Iterative Causal Forest (iCF) Analysis

This directory implements the iterative Causal Forest (iCF) method for identifying
subgroups with heterogeneous treatment effects (HTEs) in the SSRI-suicidality study.

## Method Overview

The iCF algorithm (Wang et al., 2024) identifies important subgroups with heterogeneous
treatment effects without prior knowledge of treatment-covariate interactions.

**Key steps:**
1. Grow a raw causal forest to test for heterogeneity and select important variables
2. Tune minimum leaf size for different tree depths (D2, D3, D4, D5)
3. Iteratively grow CFs at each depth, extract best trees, and perform plurality voting
4. Build transformed outcome models for each depth's subgroup decision
5. Select the final subgroup decision via cross-validation

## Scripts

### `01_prepare_data.R`

Loads and prepares data for iCF analysis.

**Inputs:**
- Extracted cohort data from `../extraction/` pipeline

**Outputs (in `data/`):**

| File | Description |
|------|-------------|
| `icf_data.rds` | Analysis-ready data frame with treatment indicator (W), outcome (Y), and covariate matrix (X) |
| `covar_names.rds` | Vector of covariate names used in the analysis |

---

### `02_run_icf.R`

Main iCF analysis script. Runs the full iterative causal forest algorithm.

**Inputs:**
- `data/icf_data.rds` from step 01

**Configuration:**
- K-fold CV: 5 folds
- Trees per forest: 200
- Iterations per depth: 50
- Candidate depths: 2, 3, 4, 5
- P-value threshold: 0.1

**Outputs (in `output/`):**

| File | Description |
|------|-------------|
| `icf_results.rds` | Main results object containing all iCF outputs (see structure below) |
| `cate_summary.csv` | Subgroup-level CATE estimates with sample sizes and event rates |
| `variable_importance.csv` | Variable importance scores from the initial causal forest |
| `config.rds` | Configuration parameters used for the analysis |

**Structure of `icf_results.rds`:**

```r
List of 14:
 $ het_p_value     : num    # Heterogeneity test p-value from initial CF
 $ selected_vars   : chr    # Indices of variables with importance > mean
 $ var_names       : chr    # Names of selected variables
 $ var_importance  : num    # Variable importance scores (all variables)
 $ cv_mse          : list   # Cross-validated MSE for each depth
 $ mean_cv_mse     : num    # Mean CV MSE by depth
 $ best_depth      : num    # Selected tree depth
 $ final_result    : list   # Final tree structure and subgroup assignments
 $ subgroup_id     : int    # Subgroup assignment for each observation
 $ subgroup_labels : chr    # Human-readable subgroup definitions
 $ n_subgroups     : int    # Number of identified subgroups
 $ cate            : data.frame  # CATE estimates by subgroup
 $ W.hat           : num    # Estimated propensity scores
 $ Y.hat           : num    # Estimated expected outcomes
```

**Structure of `cate_summary.csv`:**

| Column | Description |
|--------|-------------|
| `subgroup_id` | Numeric subgroup identifier (1 to N) |
| `label` | Subgroup definition string (e.g., "SG_age<=19_female>0") |
| `n_total` | Total sample size in subgroup |
| `n_treated` | Number of SSRI initiators |
| `n_control` | Number of non-initiators |
| `events_treated` | Outcome events among treated |
| `events_control` | Outcome events among controls |
| `rate_treated` | Event rate (%) among treated |
| `rate_control` | Event rate (%) among controls |
| `crude_rd` | Crude risk difference (percentage points) |
| `iptw_cate` | IPTW-adjusted CATE (percentage points) |

**Structure of `variable_importance.csv`:**

| Column | Description |
|--------|-------------|
| `variable` | Covariate name |
| `importance` | Importance score (weighted split frequency from initial CF) |

---

### `03_visualize_results.R`

Generates visualizations of the iCF results.

**Inputs:**
- `output/icf_results.rds`
- `output/variable_importance.csv`
- `output/config.rds`

**Outputs (in `output/`):**

| File | Description |
|------|-------------|
| `variable_importance.pdf` | Bar chart of top 20 variables by importance score |
| `decision_tree.pdf` | Visual representation of the subgroup decision tree with CATE estimates at leaf nodes |
| `cate_forest_plot.pdf` | Forest plot showing CATE point estimates by subgroup, ordered by effect size |
| `crude_event_rates_by_subgroup.pdf` | Grouped bar chart comparing event rates between treated and control within each subgroup (human-readable labels) |
| `crude_event_rates_by_subgroup_raw.pdf` | Same as above but with raw subgroup labels |
| `subgroup_decision.txt` | Text summary of the final subgroup decision, including configuration, selected variables, and CATE table |

---

## Directory Structure

```
analysis-icf/
├── README.md
├── 01_prepare_data.R
├── 02_run_icf.R
├── 03_visualize_results.R
├── icf/
│   └── icf_algorithm.R      # Core iCF implementation
├── data/
│   ├── icf_data.rds         # Prepared analysis data
│   └── covar_names.rds      # Covariate names
└── output/
    ├── icf_results.rds      # Main results object
    ├── cate_summary.csv     # CATE estimates by subgroup
    ├── variable_importance.csv
    ├── config.rds
    ├── variable_importance.pdf
    ├── decision_tree.pdf
    ├── cate_forest_plot.pdf
    ├── crude_event_rates_by_subgroup.pdf
    ├── crude_event_rates_by_subgroup_raw.pdf
    └── subgroup_decision.txt
```

## Dependencies

Install required packages:
```r
install.packages(c("grf", "tidyverse", "here"))
```

## Usage

```r
# Step 1: Prepare data
source("01_prepare_data.R")

# Step 2: Run iCF analysis (computationally intensive, ~15 hours)
source("02_run_icf.R")

# Step 3: Visualize results
source("03_visualize_results.R")
```

For cluster submission:
```bash
# From repository root
./tools/submit.sh suicidality/analysis-icf/01_prepare_data.R
./tools/submit.sh suicidality/analysis-icf/02_run_icf.R
./tools/submit.sh suicidality/analysis-icf/03_visualize_results.R
```

## Reference

Wang T, Keil AP, Kim S, Wyss R, Htoo PT, Funk MJ, Buse JB, Kosorok MR, Stürmer T.
Iterative Causal Forest: A Novel Algorithm for Subgroup Identification.
*Am J Epidemiol.* 2024;193(5):764-776. https://doi.org/10.1093/aje/kwad219

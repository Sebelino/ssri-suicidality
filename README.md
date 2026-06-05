# ssri-suicidality — reproduction guide

Target trial emulation of SSRI initiation and suicidal behaviour in Swedish
youth (ages 6–24) with a new depression diagnosis (ICD-10 F32/F33), based on
Lagerberg et al., *Neuropsychopharmacology* (2023). This repository contains
every line of code needed to rebuild the cohort, run the headline analysis and
the two subgroup analyses (iCF and hdiCF), and compile the thesis PDF.

The pipeline is designed to be run end-to-end on MEB's
[tensor HPC cluster](https://staff.ki.se/our-ki/local-web/for-staff-at-the-department-of-medical-epidemiology-and-biostatistics/it-at-meb/computing-services-at-meb)
from a single local command:

```bash
./tools/reproduce_on_tensor.sh
```

The sections below describe what that does, what you need first, and how to
inspect the results.

---

## 1. What gets reproduced

| Stage            | Script(s)                                                                 | Outputs |
|------------------|---------------------------------------------------------------------------|---------|
| Extraction       | `suicidality/extraction/01_*.R` … `24_*.R` (via `Extract_all.R`)          | Analysis-ready RDS cohorts (`main_12wks_28.rds`, `pp_12wks_max.rds`, `base_cov_28.rds`) |
| Headline analysis| `suicidality/analysis/ITT_12wks.R` + Table-2 / sensitivity scripts        | Survival plots, IPW estimates, `thesis_values.tex` |
| iCF subgroups    | `suicidality/analysis-icf/01_*.R` … `04_*.R`                              | Decision trees, CATE forest plots, `icf_values.tex` |
| hdiCF subgroups  | `suicidality/analysis-hdicf/01_*.R` … `06_*.R`                            | HD-feature trees, `hdicf_values.tex`, `hd_features_table.tex` |
| Qini curves      | `suicidality/analysis/plot_qini.R`, `plot_qini_hdicf.R`                   | `qini_12wks.pdf`, `qini_values.tex` |
| Thesis build     | `suicidality/report/Makefile`                                             | `suicidality/report/thesis.pdf` |

End-to-end wallclock is ≈30 h on tensor (most of it in iCF step 1 + hdiCF
step 1, which fit causal forests on the N≈88 k cohort).

---

## 2. Prerequisites

### Local machine
You only need the things required to push code and trigger a remote job:

- `git`, `bash`, `rsync`, `ssh`
- An SSH alias `tensor` pointing at the cluster (see `~/.ssh/config`). The
  orchestrator does a one-shot `ssh -o BatchMode=yes tensor true` to confirm
  passwordless access before submitting anything, so make sure your key (or
  Kerberos ticket) is valid first.

You do **not** need R, mamba, or any database driver locally just to
reproduce — every script runs on the cluster. If you want to develop or run
parts of the pipeline locally, see [§ Local development](#5-local-development).

### Cluster (tensor)
Setup is described in the MEB IT
[computing services page](https://staff.ki.se/our-ki/local-web/for-staff-at-the-department-of-medical-epidemiology-and-biostatistics/it-at-meb/computing-services-at-meb)
(access requires a KI staff account). The orchestrator assumes the cluster provides:

- A SLURM scheduler with default partition
- `module load R/4.5.1`, `GCCcore/13.2.0`, `unixODBC`, `mebauth`
  (these are the modules used by every job in the chain)
- A `texlive` module (for the thesis build)
- ODBC access to the Swedish registry views listed in
  [`suicidality/extraction/README.md`](suicidality/extraction/README.md) —
  `v_npr_dia`, `v_lmr`, `v_individual`, `v_migration`, `v_dor_bas`,
  `v_dor_orsak`, `v_parent`, `v_lisa`.

> **Database access requires the appropriate MEB approvals.** If you do not
> have DB credentials, you can still reproduce everything downstream of
> extraction; see `--skip-extract` below.

---

## 3. Reproducing on tensor

From the repository root, on your local machine:

```bash
./tools/reproduce_on_tensor.sh
```

This will:

1. `rsync` the working tree to `tensor:~/jobs/repro_<timestamp>/repo/`,
   excluding `.git/`, RDS / SAS / RData artefacts, and the gitignored
   working dirs (`output/`, `gitignore/`).
2. SSH into tensor and submit a SLURM job chain whose dependency graph is:

```
r_bootstrap ──► extract ──► analysis_main ──┐
                        │                   │
                        ├─► icf_prep ─► icf_step1 ─► icf_step2[1-20] ─► icf_step3a[1-20] ─► icf_step3b ─► {icf_viz, icf_latex, icf_validate, qini_icf}
                        │                                                                                                                            │
                        └─► hdicf_hdgen ─► hdicf_prep ─► hdicf_step1 ─► hdicf_step2[1-20] ─► hdicf_step3a[1-20] ─► hdicf_step3b ─► {hdicf_viz, hdicf_latex, qini_hdicf}
                                                                                                                                                     │
                                                                                                                                          thesis_build ◄┘
```

`r_bootstrap` runs `tools/install_r_packages.R`, which idempotently installs
every CRAN package the pipeline imports into `~/R/library/`. On a fresh
account this takes ~10 min; on a populated one it's a no-op.

Every job runs from `~/jobs/<TAG>/repo/`, so two concurrent reproductions
never share state. Logs land in `~/jobs/<TAG>/logs/` and a manifest of
submitted job IDs is written to `~/jobs/<TAG>/job_chain.txt`.

### Options

```text
--name TAG          Override the job dir name (default: repro_<timestamp>)
--resume TAG        Resume an existing ~/jobs/<TAG>/ run. Re-rsyncs the code so
                    edits propagate, then submits only the stages whose final
                    artefacts are not yet on disk. Mutually exclusive with
                    --name.
--force             Re-submit every stage even when its artefacts exist
                    (overrides the resume skip logic).
--host HOST         SSH alias (default: tensor)
--skip-extract      Reuse RDS files already in the repo (carries them over rsync)
--skip-icf          Do not submit the iCF pipeline
--skip-hdicf        Do not submit the hdiCF pipeline
--skip-thesis       Do not build the thesis pdf
--dry-run           Print the plan, do not submit
```

### Restarting an interrupted run

Every stage probes for a definitive artefact on disk (e.g. iCF step 3b is
considered done iff `analysis-icf/output/icf_results.rds` exists). When you
re-launch with `--resume <TAG>`, completed stages are skipped and downstream
jobs lose their `afterok:` dep on them — so the chain picks up at the first
stage whose output is missing.

```bash
# A run crashed at hdicf_step1 after iCF and extraction finished.
./tools/reproduce_on_tensor.sh --resume repro_20260527_142904

# Same as above, but also re-run anything I edited since
# (the rsync always copies fresh code; this flag also invalidates outputs).
./tools/reproduce_on_tensor.sh --resume repro_20260527_142904 --force
```

The job manifest at `~/jobs/<TAG>/job_chain.txt` is appended on each resume,
so the run history is preserved.

Typical re-runs:

```bash
# Re-build the thesis after editing only analysis scripts
./tools/reproduce_on_tensor.sh --skip-extract

# Quick smoke test of the orchestrator
./tools/reproduce_on_tensor.sh --dry-run

# Skip the heavy subgroup pipelines, just rebuild headline + thesis
./tools/reproduce_on_tensor.sh --skip-icf --skip-hdicf
```

### Monitoring while the chain is running

Every fresh run also updates the symlink `tensor:~/jobs/latest →
~/jobs/<TAG>/`, so you can refer to the current run as `~/jobs/latest`
without typing the tag.

```bash
# Submitted job IDs for this run
ssh tensor 'cat ~/jobs/latest/job_chain.txt'

# Live SLURM queue
ssh tensor 'squeue -u $USER'

# Tail every log file in the run
ssh tensor 'tail -f ~/jobs/latest/logs/*.out'
```

If a single SLURM job fails, the rest of the chain is held with `Dependency`
state. Inspect the failing log, fix the bug, then re-launch with
`./tools/reproduce_on_tensor.sh --resume <TAG>` — completed stages are
detected on disk and skipped, so the chain picks up where it broke.

### Downloading the finished job directory

After the final `thesis_build` job has completed, use `tools/download_latest.sh`
to pull the entire job dir back to your laptop:

```bash
# Defaults: --tag <whatever ~/jobs/latest points at>, full download into ./<TAG>/
./tools/download_latest.sh

# Smaller download — skip the multi-MB RDS bundles, keep logs/figures/macros/thesis
./tools/download_latest.sh --lean

# Specific tag, custom destination
./tools/download_latest.sh --tag repro_20260527_142904 --dest ~/ssri-runs/headline
```

Under the hood it's a single `rsync -avz --progress`; with `--lean` it
excludes `repo/suicidality/extraction/output/rds/`,
`repo/suicidality/analysis-*/data/`, and the iCF/hdiCF output `.rds` files.

Once the download finishes, the layout under `./<TAG>/` is:

```
<TAG>/
├── job_chain.txt                 # Per-stage SLURM job IDs (one block per re-run)
├── logs/                         # SLURM stdout/stderr for every submitted stage
│   ├── 00_rdeps_<jobid>.out
│   ├── 01_extract_<jobid>.out
│   ├── 02_analysis_<jobid>.out
│   ├── icf_01_prep_<jobid>.out
│   ├── icf_02_step1_<jobid>.out
│   ├── icf_03_step2_<arrayid>_<task>.out      (one per fold×depth)
│   ├── icf_04_step3a_<arrayid>_<task>.out     (one per batch)
│   ├── icf_05_step3b_<jobid>.out
│   ├── icf_06_viz_<jobid>.out                 ... etc for 07-11
│   ├── icf_09_cate_<jobid>.out
│   ├── icf_13_step1_missind_<jobid>.out
│   ├── hdicf_*.out                            (parallel naming for hdiCF)
│   ├── missind_triage_<jobid>.out
│   └── 99_thesis_<jobid>.out
└── repo/                         # Isolated copy of the codebase + outputs
    └── suicidality/
        ├── extraction/
        │   ├── output/rds/                    # Cohort RDS files
        │   │   ├── main_12wks_28.rds          # 28-day grace ITT cohort (headline)
        │   │   ├── main_12wks_14.rds          # 14-day grace sensitivity cohort
        │   │   ├── pp_12wks_max.rds           # 12-week per-protocol cohort
        │   │   └── base_cov_28.rds            # Baseline covariates
        │   └── log/<timestamp>/               # Per-script logs from Extract_all.R
        ├── analysis/
        │   ├── survplot_itt_main.pdf          # ITT KM curve (Fig 4.1)
        │   ├── survplot_age_strata.pdf        # Age-stratified KM
        │   ├── predi_diff_distribution.pdf    # Prescription-to-diagnosis lag
        │   └── output/
        │       ├── qini_12wks.pdf             # Qini (iCF CATE)
        │       ├── qini_12wks_hdicf.pdf       # Qini (hdiCF CATE)
        │       ├── cate_by_source_values.tex  # Sensitivity macros
        │       └── *.tex / *.csv              # Other macro/data exports
        ├── analysis-icf/
        │   ├── data/icf_data.rds              # Prepared (W, Y, X) matrix
        │   ├── data/icf_data_missind.rds      # Missing-indicator variant
        │   └── output/
        │       ├── icf_results.rds            # Terminal iCF artefact (everything in one bundle)
        │       ├── icf_step1_missind.rds      # Sensitivity step-1
        │       ├── decision_tree*.pdf         # Tree visualisations per depth
        │       ├── cate_forest_plot*.pdf      # CATE forest plots
        │       ├── variable_importance.pdf
        │       ├── calibration_plot.pdf
        │       ├── icf_values.tex             # Macros consumed by the thesis
        │       ├── baseline_table.tex         # Table 2
        │       ├── thesis_values.tex          # Headline numbers (RD, RR, etc.)
        │       └── missind_triage_values.tex  # Missing-indicator triage macros
        ├── analysis-hdicf/
        │   ├── data/hd_features.rds           # HD feature matrix
        │   └── output/                        # Parallel structure to analysis-icf
        │       ├── icf_results.rds            # Terminal hdiCF artefact
        │       ├── hdicf_values.tex
        │       ├── hd_features_table.tex
        │       └── ...
        └── report/
            ├── thesis.pdf                     # ← The compiled thesis
            ├── thesis.log                     # pdflatex run log
            ├── figures/                       # Copies of all figures used by the thesis
            ├── generated/                     # Copies of all .tex macro files
            └── config.mk                      # Makefile config pointing at this run's outputs
```

The two terminal artefacts most readers care about are
`<TAG>/repo/suicidality/report/thesis.pdf` (the compiled thesis) and
`<TAG>/repo/suicidality/analysis-icf/output/icf_results.rds` (the iCF results
object). For SLURM forensics, start in `<TAG>/logs/`.

---

## 4. Repository layout

```
ssri-suicidality/
├── tools/                        # SSH/SLURM orchestration
│   ├── reproduce_on_tensor.sh    # ← the one-command pipeline (this file)
│   ├── download_latest.sh              # download a finished job dir back to ./
│   ├── submit.sh                 # one-off single-script submission
│   ├── submit_icf_parallel.sh    # standalone iCF chain (no extraction)
│   ├── submit_hdicf_parallel.sh  # standalone hdiCF chain (no extraction)
│   ├── sync-repo-to-tensor.sh    # rsync helper used by submit*.sh
│   ├── tail-job.sh, download_s2.sh, cleanup.sh
│   └── run_r.sbatch              # generic SBATCH wrapper
├── suicidality/
│   ├── extraction/               # Scripts 01–24 (DB extract + RDS processing)
│   ├── analysis/                 # ITT, sensitivity, Qini, Table-2 exporter
│   ├── analysis-icf/             # iCF subgroup pipeline (see its README)
│   ├── analysis-hdicf/           # hdiCF subgroup pipeline
│   ├── diagnostics/              # Stand-alone probes (CF reproducibility, …)
│   ├── scripts/                  # Python helpers (codebook, flowcharts)
│   ├── Documents/                # Static inputs: cohort flowchart, logbook
│   └── report/                   # LaTeX thesis (separate gitignored repo)
├── environment.yml               # mamba env for local development
├── post_install.sh               # ODBC / Kerberos setup on macOS
└── README.md                     # ← you are here
```

`suicidality/report/` is gitignored from this top-level repo because the
thesis manuscript is versioned in its own private git repo. The orchestrator
rsyncs its working tree to tensor as-is.

---

## 5. Local development

If you want to develop the analysis code locally (e.g. iterate on a plot
without resubmitting to tensor), set up the conda environment:

```bash
mamba env create -f environment.yml
mamba activate thesis
./post_install.sh
```

`post_install.sh` installs the few CRAN packages not available on conda-forge
(`odbc`, `partykit`, `tidyverse`) and, on macOS, configures the
`msodbcsql18` driver plus a file-based Kerberos cache so that registry ODBC
connections work outside the cluster.

Once the environment is ready you can run any individual script the same way
the SLURM jobs do — every script uses `here::i_am()` so paths resolve
regardless of working directory:

```bash
Rscript suicidality/analysis/ITT_12wks.R
Rscript suicidality/analysis-icf/01_prepare_data.R
```

The thesis builds locally with a `texlive` install:

```bash
cd suicidality/report
./configure --icf-dir ../analysis-icf/output --analysis-dir ../analysis
make
```

---

## 6. Where to look for more detail

- [`suicidality/extraction/README.md`](suicidality/extraction/README.md) —
  per-script descriptions, runtimes, and registry data sources.
- [`suicidality/analysis-icf/README.md`](suicidality/analysis-icf/README.md) —
  iCF method walkthrough and output schema.

## 7. References

- Lagerberg T, Matthews AA, Zhu N, et al. *Effect of selective serotonin
  reuptake inhibitor treatment following diagnosis of depression on suicidal
  behaviour risk: a target trial emulation.* Neuropsychopharmacology
  2023;48:1760–1768.
- Wang T, Keil AP, Kim S, et al. *Iterative Causal Forest: A Novel Algorithm
  for Subgroup Identification.* Am J Epidemiol 2024;193(5):764–776.
- Wang T, Pate V, Wyss R, et al. *High-dimensional iterative causal forest
  (hdiCF) for subgroup identification using health care claims data.*
  Am J Epidemiol 2025;194(7):2085–2097.

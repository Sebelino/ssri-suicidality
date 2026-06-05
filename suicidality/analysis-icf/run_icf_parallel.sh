#!/usr/bin/env bash
set -euo pipefail

# run_icf_parallel.sh
# SLURM pipeline for the full iCF analysis
#
# Pipeline:
#   Prep -> Step 1 -> Step 2[1-20] -> Step 3a[1-20] -> Step 3b -> {Viz, LaTeX, Validate}
#        \-> CATE estimation -> CATE visualization   (parallel with iCF)
#
# Usage: bash suicidality/analysis-icf/run_icf_parallel.sh
# Monitor: squeue -u $USER

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$REPO_DIR"

ICF_DIR="suicidality/analysis-icf"
LOG_DIR="$REPO_DIR/$ICF_DIR/logs"
mkdir -p "$LOG_DIR"

# ---- Prep: Data preparation (~5 min, 1 core, 4 GB) ----
JOBID0=$(sbatch \
  --job-name=icf_prep \
  -c 1 \
  --mem=4G \
  --time=1:00:00 \
  --output="$LOG_DIR/prep_%j.out" \
  --error="$LOG_DIR/prep_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO_DIR
    module load R/4.5.1
    module load GCCcore/13.2.0
    Rscript $ICF_DIR/01_prepare_data.R
  " | awk '{print $4}')

echo "Prep submitted: job $JOBID0"

# ---- Step 1: Raw CF + variable selection (~2h, 4 cores, 8 GB) ----
JOBID1=$(sbatch \
  --job-name=icf_step1 \
  --dependency=afterok:${JOBID0} \
  -c 4 \
  --mem=8G \
  --time=4:00:00 \
  --output="$LOG_DIR/step1_%j.out" \
  --error="$LOG_DIR/step1_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO_DIR
    module load R/4.5.1
    module load GCCcore/13.2.0
    Rscript $ICF_DIR/02a_icf_step1.R
  " | awk '{print $4}')

echo "Step 1 submitted: job $JOBID1, depends on $JOBID0"

# ---- Step 2: CV fold x depth array (20 jobs, ~2h each, 2 cores, 4 GB) ----
# Array task IDs 1-20 map to fold/depth:
#   fold  = ((task_id - 1) %  5) + 1   → 1..5
#   depth = ((task_id - 1) /  5) + 2   → 2..5
JOBID2=$(sbatch \
  --job-name=icf_step2 \
  --array=1-20 \
  --dependency=afterok:${JOBID1} \
  -c 2 \
  --mem=4G \
  --time=4:00:00 \
  --output="$LOG_DIR/step2_%A_%a.out" \
  --error="$LOG_DIR/step2_%A_%a.out" \
  --wrap='
    set -euo pipefail
    cd '"$REPO_DIR"'
    module load R/4.5.1
    module load GCCcore/13.2.0
    FOLD=$(( (SLURM_ARRAY_TASK_ID - 1) % 5 + 1 ))
    DEPTH=$(( (SLURM_ARRAY_TASK_ID - 1) / 5 + 2 ))
    echo "Task $SLURM_ARRAY_TASK_ID → fold=$FOLD depth=$DEPTH"
    Rscript '"$ICF_DIR"'/02b_icf_step2_fold.R "$FOLD" "$DEPTH"
  ' | awk '{print $4}')

echo "Step 2 submitted: array job $JOBID2 (tasks 1-20), depends on $JOBID1"

# ---- Step 3a: Batch forest growing (array[1-N_BATCHES], 2c/4G/4h each) ----
N_BATCHES=20
JOBID3A=$(sbatch \
  --job-name=icf_step3a \
  --array=1-${N_BATCHES} \
  --dependency=afterok:${JOBID2} \
  -c 2 \
  --mem=4G \
  --time=4:00:00 \
  --output="$LOG_DIR/step3a_%A_%a.out" \
  --error="$LOG_DIR/step3a_%A_%a.out" \
  --wrap='
    set -euo pipefail
    cd '"$REPO_DIR"'
    module load R/4.5.1
    module load GCCcore/13.2.0
    Rscript '"$ICF_DIR"'/02c1_icf_step3_batch.R "$SLURM_ARRAY_TASK_ID" '"$N_BATCHES"'
  ' | awk '{print $4}')

echo "Step 3a submitted: array job $JOBID3A (tasks 1-$N_BATCHES), depends on $JOBID2"

# ---- Step 3b: Merge + finalize (4c/8G/4h, depends on 3a) ----
JOBID3B=$(sbatch \
  --job-name=icf_step3b \
  --dependency=afterok:${JOBID3A} \
  -c 4 \
  --mem=8G \
  --time=4:00:00 \
  --output="$LOG_DIR/step3b_%j.out" \
  --error="$LOG_DIR/step3b_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO_DIR
    module load R/4.5.1
    module load GCCcore/13.2.0
    Rscript $ICF_DIR/02c2_icf_step3_merge.R
  " | awk '{print $4}')

echo "Step 3b submitted: job $JOBID3B, depends on $JOBID3A"

# ---- Viz: Result visualization (~5 min, 1 core, 4 GB) ----
JOBID4=$(sbatch \
  --job-name=icf_viz \
  --dependency=afterok:${JOBID3B} \
  -c 1 \
  --mem=4G \
  --time=0:30:00 \
  --output="$LOG_DIR/viz_%j.out" \
  --error="$LOG_DIR/viz_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO_DIR
    module load R/4.5.1
    module load GCCcore/13.2.0
    Rscript $ICF_DIR/03_visualize_results.R
  " | awk '{print $4}')

echo "Viz submitted: job $JOBID4, depends on $JOBID3B"

# ---- LaTeX: Export values for report (~1 min, 1 core, 4 GB) ----
JOBID5=$(sbatch \
  --job-name=icf_latex \
  --dependency=afterok:${JOBID3B} \
  -c 1 \
  --mem=4G \
  --time=0:30:00 \
  --output="$LOG_DIR/latex_%j.out" \
  --error="$LOG_DIR/latex_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO_DIR
    module load R/4.5.1
    module load GCCcore/13.2.0
    Rscript $ICF_DIR/04_export_latex.R
  " | awk '{print $4}')

echo "LaTeX submitted: job $JOBID5, depends on $JOBID3B"

# ---- Validation: Stratified sanity check (~10 min, 1 core, 8 GB) ----
JOBID6=$(sbatch \
  --job-name=icf_validate \
  --dependency=afterok:${JOBID3B} \
  -c 1 \
  --mem=8G \
  --time=1:00:00 \
  --output="$LOG_DIR/validate_%j.out" \
  --error="$LOG_DIR/validate_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO_DIR
    module load R/4.5.1
    module load GCCcore/13.2.0
    Rscript $ICF_DIR/04_validate_subgroups.R
  " | awk '{print $4}')

echo "Validation submitted: job $JOBID6, depends on $JOBID3B"

# ---- CATE comparison: 3 methods (~2h, 4 cores, 32 GB) — parallel with iCF ----
JOBID_CATE=$(sbatch \
  --job-name=cate_est \
  --dependency=afterok:${JOBID0} \
  -c 4 \
  --mem=32G \
  --time=4:00:00 \
  --output="$LOG_DIR/cate_est_%j.out" \
  --error="$LOG_DIR/cate_est_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO_DIR
    module load R/4.5.1
    module load GCCcore/13.2.0
    Rscript $ICF_DIR/05_cate_comparison.R
  " | awk '{print $4}')

echo "CATE estimation submitted: job $JOBID_CATE, depends on $JOBID0"

# ---- CATE visualization (~5 min, 1 core, 4 GB) ----
JOBID_CVIZ=$(sbatch \
  --job-name=cate_viz \
  --dependency=afterok:${JOBID_CATE} \
  -c 1 \
  --mem=4G \
  --time=0:30:00 \
  --output="$LOG_DIR/cate_viz_%j.out" \
  --error="$LOG_DIR/cate_viz_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO_DIR
    module load R/4.5.1
    module load GCCcore/13.2.0
    Rscript $ICF_DIR/06_visualize_cate_comparison.R
  " | awk '{print $4}')

echo "CATE viz submitted: job $JOBID_CVIZ, depends on $JOBID_CATE"
echo ""
echo "iCF pipeline:  $JOBID0 → $JOBID1 → $JOBID2 (array 1-20) → $JOBID3A (array 1-$N_BATCHES) → $JOBID3B → {$JOBID4, $JOBID5, $JOBID6}"
echo "CATE compare:  $JOBID0 → $JOBID_CATE → $JOBID_CVIZ"
echo "Monitor with: squeue -u \$USER"
echo "Logs in: $LOG_DIR"

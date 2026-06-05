#!/usr/bin/env bash
set -euo pipefail

# run_hdicf_parallel.sh
# SLURM pipeline for the full hdiCF analysis
#
# Submits an 8-stage pipeline:
#   Step 0: HD feature generation (01_generate_hd_features.R)
#   Step 1: Data preparation + PS trimming (02_prepare_data.R)
#   Step 2: Raw causal forest + variable selection (03a_icf_step1.R)
#   Step 3: CV fold x depth iterations (20 jobs as array)
#   Step 4a: Batch forest growing (20 array jobs)
#   Step 4b: Merge + finalize
#   Step 5: Visualization
#   Step 6: LaTeX export
#
# Usage: bash suicidality/analysis-hdicf/run_hdicf_parallel.sh
# Monitor: squeue -u $USER

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/../.." && pwd)"
cd "$REPO_DIR"

HDICF_DIR="suicidality/analysis-hdicf"
LOG_DIR="$REPO_DIR/$HDICF_DIR/logs"
mkdir -p "$LOG_DIR"

# ---- Step 0: HD feature generation (1c/8G/1h) ----
JOBID0=$(sbatch \
  --job-name=hdicf_hdgen \
  -c 1 \
  --mem=8G \
  --time=1:00:00 \
  --output="$LOG_DIR/hdgen_%j.out" \
  --error="$LOG_DIR/hdgen_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO_DIR
    module load R/4.5.1
    module load GCCcore/13.2.0
    Rscript $HDICF_DIR/01_generate_hd_features.R
  " | awk '{print $4}')

echo "HD gen submitted: job $JOBID0"

# ---- Step 1: Data preparation + PS trimming (4c/16G/2h) ----
JOBID1=$(sbatch \
  --job-name=hdicf_prep \
  --dependency=afterok:${JOBID0} \
  -c 4 \
  --mem=16G \
  --time=2:00:00 \
  --output="$LOG_DIR/prep_%j.out" \
  --error="$LOG_DIR/prep_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO_DIR
    module load R/4.5.1
    module load GCCcore/13.2.0
    Rscript $HDICF_DIR/02_prepare_data.R
  " | awk '{print $4}')

echo "Prep submitted: job $JOBID1, depends on $JOBID0"

# ---- Step 2: Raw CF + variable selection (4c/16G/8h) ----
JOBID2=$(sbatch \
  --job-name=hdicf_step1 \
  --dependency=afterok:${JOBID1} \
  -c 4 \
  --mem=16G \
  --time=8:00:00 \
  --output="$LOG_DIR/step1_%j.out" \
  --error="$LOG_DIR/step1_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO_DIR
    module load R/4.5.1
    module load GCCcore/13.2.0
    Rscript $HDICF_DIR/03a_icf_step1.R
  " | awk '{print $4}')

echo "Step 1 submitted: job $JOBID2, depends on $JOBID1"

# ---- Step 3: CV fold x depth array (20 jobs, 2c/4G/4h each) ----
# Array task IDs 1-20 map to fold/depth:
#   fold  = ((task_id - 1) %  5) + 1   → 1..5
#   depth = ((task_id - 1) /  5) + 2   → 2..5
JOBID3=$(sbatch \
  --job-name=hdicf_step2 \
  --array=1-20 \
  --dependency=afterok:${JOBID2} \
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
    Rscript '"$HDICF_DIR"'/03b_icf_step2_fold.R "$FOLD" "$DEPTH"
  ' | awk '{print $4}')

echo "Step 2 submitted: array job $JOBID3 (tasks 1-20), depends on $JOBID2"

# ---- Step 4a: Batch forest growing (array[1-N_BATCHES], 2c/4G/4h each) ----
N_BATCHES=20
JOBID4A=$(sbatch \
  --job-name=hdicf_step3a \
  --array=1-${N_BATCHES} \
  --dependency=afterok:${JOBID3} \
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
    Rscript '"$HDICF_DIR"'/03c1_icf_step3_batch.R "$SLURM_ARRAY_TASK_ID" '"$N_BATCHES"'
  ' | awk '{print $4}')

echo "Step 3a submitted: array job $JOBID4A (tasks 1-$N_BATCHES), depends on $JOBID3"

# ---- Step 4b: Merge + finalize (4c/8G/4h, depends on 4a) ----
JOBID4B=$(sbatch \
  --job-name=hdicf_step3b \
  --dependency=afterok:${JOBID4A} \
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
    Rscript $HDICF_DIR/03c2_icf_step3_merge.R
  " | awk '{print $4}')

echo "Step 3b submitted: job $JOBID4B, depends on $JOBID4A"

# ---- Step 5: Visualization (1c/4G/0.5h) ----
JOBID5=$(sbatch \
  --job-name=hdicf_viz \
  --dependency=afterok:${JOBID4B} \
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
    Rscript $HDICF_DIR/04_visualize_results.R
  " | awk '{print $4}')

echo "Viz submitted: job $JOBID5, depends on $JOBID4B"

# ---- Step 6: LaTeX export (1c/4G/0.5h) — parallel with viz ----
JOBID6=$(sbatch \
  --job-name=hdicf_latex \
  --dependency=afterok:${JOBID4B} \
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
    Rscript $HDICF_DIR/05_export_latex.R
  " | awk '{print $4}')

echo "LaTeX submitted: job $JOBID6, depends on $JOBID4B"
echo ""
echo "Pipeline: $JOBID0 → $JOBID1 → $JOBID2 → $JOBID3 (array 1-20) → $JOBID4A (array 1-$N_BATCHES) → $JOBID4B → {$JOBID5, $JOBID6}"
echo "Monitor with: squeue -u \$USER"
echo "Logs in: $LOG_DIR"

#!/usr/bin/env bash
set -euo pipefail

# submit_icf_parallel.sh
# Sync repo to Tensor and submit the full iCF SLURM pipeline.
#
# Pipeline:
#   prepare_data -> step1 -> step2[1-20] -> step3a[1-20] -> step3b -> {visualize, latex, validate}
#                \-> cate_est -> cate_viz   (CATE method comparison, parallel with iCF)
#
# Usage: ./tools/submit_icf_parallel.sh

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

REMOTE_REPO="~/work/ssri-suicidality"
ICF_DIR="suicidality/analysis-icf"
LOG_DIR="$ICF_DIR/logs"

# ---- Sync ----
echo "Syncing repo to Tensor..."
"$SCRIPT_DIR/sync-repo-to-tensor.sh"

# ---- Submit pipeline ----
echo ""
echo "Submitting iCF pipeline..."

ssh tensor bash -s -- "$REMOTE_REPO" "$ICF_DIR" "$LOG_DIR" <<'REMOTE'
set -euo pipefail

REPO="$1"
ICF_DIR="$2"
LOG_DIR="$3"

cd "$REPO"
mkdir -p "$LOG_DIR"

# Step 0: Data preparation (1 core, 4 GB, 1h)
JOBID0=$(sbatch \
  --job-name=icf_prep \
  --parsable \
  -c 1 \
  --mem=4G \
  --time=1:00:00 \
  --output="$LOG_DIR/prep_%j.out" \
  --error="$LOG_DIR/prep_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO
    module load R/4.5.1
    module load GCCcore/13.2.0
    Rscript $ICF_DIR/01_prepare_data.R
  ")
echo "Prep:   job $JOBID0  (1c / 4G / 1h)"

# Step 1: Raw CF + variable selection + depth diagnostics (4 cores, 32 GB, 12h)
# Memory bumped from 8G to 32G on 2026-05-12 after job 710734 was OOM-killed
# at 6h53m. Step 1 fits 6 raw CFs (1 main + 5 in vi_stability) at num.trees =
# 2000 each, plus nuisance regression forests, plus 50-iteration depth
# diagnostics; 8G is too tight on the CCA cohort (N = 88,427).
JOBID1=$(sbatch \
  --job-name=icf_step1 \
  --parsable \
  --dependency=afterok:${JOBID0} \
  -c 4 \
  --mem=32G \
  --time=12:00:00 \
  --output="$LOG_DIR/step1_%j.out" \
  --error="$LOG_DIR/step1_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO
    module load R/4.5.1
    module load GCCcore/13.2.0
    Rscript $ICF_DIR/02a_icf_step1.R
  ")
echo "Step 1: job $JOBID1  (4c / 32G / 12h)  dep=$JOBID0"

# Step 2: CV array — 20 tasks (2 cores, 4 GB, 4h each)
JOBID2=$(sbatch \
  --job-name=icf_step2 \
  --parsable \
  --array=1-20 \
  --dependency=afterok:${JOBID1} \
  -c 2 \
  --mem=4G \
  --time=4:00:00 \
  --output="$LOG_DIR/step2_%A_%a.out" \
  --error="$LOG_DIR/step2_%A_%a.out" \
  --wrap='
    set -euo pipefail
    cd '"$REPO"'
    module load R/4.5.1
    module load GCCcore/13.2.0
    FOLD=$(( (SLURM_ARRAY_TASK_ID - 1) % 5 + 1 ))
    DEPTH=$(( (SLURM_ARRAY_TASK_ID - 1) / 5 + 2 ))
    echo "Task $SLURM_ARRAY_TASK_ID -> fold=$FOLD depth=$DEPTH"
    Rscript '"$ICF_DIR"'/02b_icf_step2_fold.R "$FOLD" "$DEPTH"
  ')
echo "Step 2: array job $JOBID2 [1-20]  (2c / 4G / 4h each)  dep=$JOBID1"

# Step 3a: Batch forest growing — 20 tasks (2 cores, 4 GB, 4h each)
N_BATCHES=20
JOBID3A=$(sbatch \
  --job-name=icf_step3a \
  --parsable \
  --array=1-${N_BATCHES} \
  --dependency=afterok:${JOBID2} \
  -c 2 \
  --mem=4G \
  --time=4:00:00 \
  --output="$LOG_DIR/step3a_%A_%a.out" \
  --error="$LOG_DIR/step3a_%A_%a.out" \
  --wrap='
    set -euo pipefail
    cd '"$REPO"'
    module load R/4.5.1
    module load GCCcore/13.2.0
    Rscript '"$ICF_DIR"'/02c1_icf_step3_batch.R "$SLURM_ARRAY_TASK_ID" '"$N_BATCHES"'
  ')
echo "Step 3a: array job $JOBID3A [1-$N_BATCHES]  (2c / 4G / 4h each)  dep=$JOBID2"

# Step 3b: Merge + finalize + per-depth trees (4 cores, 16 GB, 8h)
# Memory bumped 8G -> 16G defensively after step1 OOM on the CCA cohort;
# step3b merges 20 batch results and re-grows per-depth trees, so similar
# memory pressure to step1 is plausible.
JOBID3B=$(sbatch \
  --job-name=icf_step3b \
  --parsable \
  --dependency=afterok:${JOBID3A} \
  -c 4 \
  --mem=16G \
  --time=8:00:00 \
  --output="$LOG_DIR/step3b_%j.out" \
  --error="$LOG_DIR/step3b_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO
    module load R/4.5.1
    module load GCCcore/13.2.0
    Rscript $ICF_DIR/02c2_icf_step3_merge.R
  ")
echo "Step 3b: job $JOBID3B  (4c / 16G / 8h)  dep=$JOBID3A"

# Step 4: Visualize results (1 core, 4 GB, 0.5h)
JOBID4=$(sbatch \
  --job-name=icf_viz \
  --parsable \
  --dependency=afterok:${JOBID3B} \
  -c 1 \
  --mem=4G \
  --time=0:30:00 \
  --output="$LOG_DIR/viz_%j.out" \
  --error="$LOG_DIR/viz_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO
    module load R/4.5.1
    module load GCCcore/13.2.0
    Rscript $ICF_DIR/03_visualize_results.R
  ")
echo "Viz:    job $JOBID4  (1c / 4G / 0.5h)  dep=$JOBID3B"

# Step 5: Export LaTeX values (1 core, 4 GB, 0.5h) — parallel with viz
JOBID5=$(sbatch \
  --job-name=icf_latex \
  --parsable \
  --dependency=afterok:${JOBID3B} \
  -c 1 \
  --mem=4G \
  --time=0:30:00 \
  --output="$LOG_DIR/latex_%j.out" \
  --error="$LOG_DIR/latex_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO
    module load R/4.5.1
    module load GCCcore/13.2.0
    Rscript $ICF_DIR/04_export_latex.R
  ")
echo "LaTeX:  job $JOBID5  (1c / 4G / 0.5h)  dep=$JOBID3B"

# Step 6: Validation — stratified sanity check (1 core, 8 GB, 1h) — parallel with viz/latex
JOBID6=$(sbatch \
  --job-name=icf_validate \
  --parsable \
  --dependency=afterok:${JOBID3B} \
  -c 1 \
  --mem=8G \
  --time=1:00:00 \
  --output="$LOG_DIR/validate_%j.out" \
  --error="$LOG_DIR/validate_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO
    module load R/4.5.1
    module load GCCcore/13.2.0
    Rscript $ICF_DIR/04_validate_subgroups.R
  ")
echo "Valid:  job $JOBID6  (1c / 8G / 1h)  dep=$JOBID3B"

# ---- CATE comparison: 3 methods (4 cores, 32 GB, 4h) — parallel with iCF ----
JOBID_CATE=$(sbatch \
  --job-name=cate_est \
  --parsable \
  --dependency=afterok:${JOBID0} \
  -c 4 \
  --mem=32G \
  --time=4:00:00 \
  --output="$LOG_DIR/cate_est_%j.out" \
  --error="$LOG_DIR/cate_est_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO
    module load R/4.5.1
    module load GCCcore/13.2.0
    Rscript $ICF_DIR/05_cate_comparison.R
  ")
echo "CATE:   job $JOBID_CATE  (4c / 32G / 4h)  dep=$JOBID0"

# CATE visualization (1 core, 4 GB, 0.5h)
JOBID_CVIZ=$(sbatch \
  --job-name=cate_viz \
  --parsable \
  --dependency=afterok:${JOBID_CATE} \
  -c 1 \
  --mem=4G \
  --time=0:30:00 \
  --output="$LOG_DIR/cate_viz_%j.out" \
  --error="$LOG_DIR/cate_viz_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO
    module load R/4.5.1
    module load GCCcore/13.2.0
    Rscript $ICF_DIR/06_visualize_cate_comparison.R
  ")
echo "CATE viz: job $JOBID_CVIZ  (1c / 4G / 0.5h)  dep=$JOBID_CATE"

# ---- Thesis values export (1 core, 8 GB, 1h) — parallel with iCF ----
ANALYSIS_DIR="suicidality/analysis"
JOBID_THESIS=$(sbatch \
  --job-name=thesis_vals \
  --parsable \
  -c 1 \
  --mem=8G \
  --time=1:00:00 \
  --output="$LOG_DIR/thesis_vals_%j.out" \
  --error="$LOG_DIR/thesis_vals_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO
    module load R/4.5.1
    module load GCCcore/13.2.0
    Rscript $ANALYSIS_DIR/export_thesis_values.R
  ")
echo "Thesis: job $JOBID_THESIS  (1c / 8G / 1h)  no dep"

echo ""
echo "iCF pipeline:  $JOBID0 -> $JOBID1 -> ${JOBID2}[1-20] -> ${JOBID3A}[1-$N_BATCHES] -> $JOBID3B -> {$JOBID4, $JOBID5, $JOBID6}"
echo "CATE compare:  $JOBID0 -> $JOBID_CATE -> $JOBID_CVIZ"
echo "Thesis vals:   $JOBID_THESIS"
echo ""
squeue -u "$USER" --format="%.10i %.9P %.20j %.2t %.10M %.5C %.8m %.12l"
REMOTE

echo ""
echo "Monitor:  ssh tensor 'squeue -u \$USER'"
echo "Logs:     ssh tensor 'cat $REMOTE_REPO/$LOG_DIR/{prep,step,viz,latex,validate,cate}*'"

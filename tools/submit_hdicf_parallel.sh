#!/usr/bin/env bash
set -euo pipefail

# submit_hdicf_parallel.sh
# Sync repo to Tensor and submit the full hdiCF SLURM pipeline.
#
# Pipeline: hdgen -> prepare -> step1 -> step2[1-20] -> step3a[1-20] -> step3b -> {visualize, latex}
#
# Usage: ./tools/submit_hdicf_parallel.sh

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

REMOTE_REPO="~/work/ssri-suicidality"
HDICF_DIR="suicidality/analysis-hdicf"
LOG_DIR="$HDICF_DIR/logs"

# ---- Sync ----
echo "Syncing repo to Tensor..."
"$SCRIPT_DIR/sync-repo-to-tensor.sh"

# ---- Submit pipeline ----
echo ""
echo "Submitting hdiCF pipeline..."

ssh tensor bash -s -- "$REMOTE_REPO" "$HDICF_DIR" "$LOG_DIR" <<'REMOTE'
set -euo pipefail

REPO="$1"
HDICF_DIR="$2"
LOG_DIR="$3"

cd "$REPO"
mkdir -p "$LOG_DIR"

# Step 0: HD feature generation (1 core, 8 GB, 1h)
JOBID0=$(sbatch \
  --job-name=hdicf_hdgen \
  --parsable \
  -c 1 \
  --mem=8G \
  --time=1:00:00 \
  --output="$LOG_DIR/hdgen_%j.out" \
  --error="$LOG_DIR/hdgen_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO
    module load R/4.5.1
    module load GCCcore/13.2.0
    Rscript $HDICF_DIR/01_generate_hd_features.R
  ")
echo "HD gen: job $JOBID0  (1c / 8G / 1h)"

# Step 1: Data preparation + PS trimming (4 cores, 16 GB, 2h)
JOBID1=$(sbatch \
  --job-name=hdicf_prep \
  --parsable \
  --dependency=afterok:${JOBID0} \
  -c 4 \
  --mem=16G \
  --time=2:00:00 \
  --output="$LOG_DIR/prep_%j.out" \
  --error="$LOG_DIR/prep_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO
    module load R/4.5.1
    module load GCCcore/13.2.0
    Rscript $HDICF_DIR/02_prepare_data.R
  ")
echo "Prep:   job $JOBID1  (4c / 16G / 2h)  dep=$JOBID0"

# Step 2: Raw CF + variable selection + depth diagnostics (4 cores, 32 GB, 16h)
# Memory bumped 16G -> 32G on 2026-05-12 in parallel with the base-iCF step1
# bump after job 710734 OOMed at 8G. hdiCF has more candidate split variables
# (curated + HD features) so the per-forest memory ceiling is even higher than
# base iCF; the vi_stability gc()/rm() fix in icf_algorithm.R applies here too.
JOBID2=$(sbatch \
  --job-name=hdicf_step1 \
  --parsable \
  --dependency=afterok:${JOBID1} \
  -c 4 \
  --mem=32G \
  --time=16:00:00 \
  --output="$LOG_DIR/step1_%j.out" \
  --error="$LOG_DIR/step1_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO
    module load R/4.5.1
    module load GCCcore/13.2.0
    Rscript $HDICF_DIR/03a_icf_step1.R
  ")
echo "Step 1: job $JOBID2  (4c / 32G / 16h)  dep=$JOBID1"

# Step 3: CV array — 20 tasks (2 cores, 4 GB, 4h each)
JOBID3=$(sbatch \
  --job-name=hdicf_step2 \
  --parsable \
  --array=1-20 \
  --dependency=afterok:${JOBID2} \
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
    Rscript '"$HDICF_DIR"'/03b_icf_step2_fold.R "$FOLD" "$DEPTH"
  ')
echo "Step 2: array job $JOBID3 [1-20]  (2c / 4G / 4h each)  dep=$JOBID2"

# Step 3a: Batch forest growing — 20 tasks (2 cores, 4 GB, 4h each)
N_BATCHES=20
JOBID4A=$(sbatch \
  --job-name=hdicf_step3a \
  --parsable \
  --array=1-${N_BATCHES} \
  --dependency=afterok:${JOBID3} \
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
    Rscript '"$HDICF_DIR"'/03c1_icf_step3_batch.R "$SLURM_ARRAY_TASK_ID" '"$N_BATCHES"'
  ')
echo "Step 3a: array job $JOBID4A [1-$N_BATCHES]  (2c / 4G / 4h each)  dep=$JOBID3"

# Step 3b: Merge + finalize + per-depth trees (4 cores, 16 GB, 8h)
# Memory bumped 8G -> 16G in parallel with the base-iCF step3b bump; step3b
# merges 20 batch results and re-grows per-depth trees, so memory pressure
# similar to step1.
JOBID4B=$(sbatch \
  --job-name=hdicf_step3b \
  --parsable \
  --dependency=afterok:${JOBID4A} \
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
    Rscript $HDICF_DIR/03c2_icf_step3_merge.R
  ")
echo "Step 3b: job $JOBID4B  (4c / 16G / 8h)  dep=$JOBID4A"

# Step 4: Visualize results (1 core, 4 GB, 0.5h)
JOBID5=$(sbatch \
  --job-name=hdicf_viz \
  --parsable \
  --dependency=afterok:${JOBID4B} \
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
    Rscript $HDICF_DIR/04_visualize_results.R
  ")
echo "Viz:    job $JOBID5  (1c / 4G / 0.5h)  dep=$JOBID4B"

# Step 5: Export LaTeX values (1 core, 4 GB, 0.5h) — parallel with viz
JOBID6=$(sbatch \
  --job-name=hdicf_latex \
  --parsable \
  --dependency=afterok:${JOBID4B} \
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
    Rscript $HDICF_DIR/05_export_latex.R
    Rscript $HDICF_DIR/06_export_hd_features_table.R
  ")
echo "LaTeX:  job $JOBID6  (1c / 4G / 0.5h)  dep=$JOBID4B"

echo ""
echo "Pipeline: $JOBID0 -> $JOBID1 -> $JOBID2 -> ${JOBID3}[1-20] -> ${JOBID4A}[1-$N_BATCHES] -> $JOBID4B -> {$JOBID5, $JOBID6}"
echo ""
squeue -u "$USER" --format="%.10i %.9P %.20j %.2t %.10M %.5C %.8m %.12l"
REMOTE

echo ""
echo "Monitor:  ssh tensor 'squeue -u \$USER'"
echo "Logs:     ssh tensor 'cat $REMOTE_REPO/$LOG_DIR/{hdgen,prep,step,viz,latex}*'"

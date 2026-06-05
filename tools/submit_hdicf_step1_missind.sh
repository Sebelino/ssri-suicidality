#!/usr/bin/env bash
set -euo pipefail

# submit_hdicf_step1_missind.sh
# Step-1-only triage rerun of the hdiCF pipeline under the missing-indicator
# variant (full eligible cohort + `any_miss` indicator; see analysis-icf/paths.R
# and analysis/common.R::apply_missind_recoding). Submits only the HD-feature
# generation, prep, and step1 jobs -- enough to answer:
#   * does `any_miss` show up in the hdiCF variable-importance ranking?
#   * does the top of the VI list shift, including the HD-feature side?
#
# Outputs (variant-suffixed, coexist with headline artefacts):
#   suicidality/analysis-hdicf/data/hd_features_missind.rds
#   suicidality/analysis-hdicf/data/icf_data_missind.rds
#   suicidality/analysis-hdicf/output/icf_step1_missind.rds
#   suicidality/analysis-hdicf/output/depth_distribution_D*_missind.pdf
#
# Usage: ./tools/submit_hdicf_step1_missind.sh

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

REMOTE_REPO="~/work/ssri-suicidality"
HDICF_DIR="suicidality/analysis-hdicf"
LOG_DIR="$HDICF_DIR/logs"

# ---- Sync ----
echo "Syncing repo to Tensor..."
"$SCRIPT_DIR/sync-repo-to-tensor.sh"

# ---- Submit step-1 chain (hdgen -> prep -> step1) under ICF_VARIANT=missind ----
echo ""
echo "Submitting hdiCF step-1 triage (missing-indicator variant)..."

ssh tensor bash -s -- "$REMOTE_REPO" "$HDICF_DIR" "$LOG_DIR" <<'REMOTE'
set -euo pipefail

REPO="$1"
HDICF_DIR="$2"
LOG_DIR="$3"

cd "$REPO"
mkdir -p "$LOG_DIR"

# HD-gen: build hd_features_missind.rds from the full eligible cohort (1c/8G/1h)
JOBID0=$(sbatch \
  --job-name=hdicf_hdgen_missind \
  --parsable \
  -c 1 \
  --mem=8G \
  --time=1:00:00 \
  --output="$LOG_DIR/hdgen_missind_%j.out" \
  --error="$LOG_DIR/hdgen_missind_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO
    module load R/4.5.1
    module load GCCcore/13.2.0
    export ICF_VARIANT=missind
    Rscript $HDICF_DIR/01_generate_hd_features.R
  ")
echo "HD gen: job $JOBID0  (1c / 8G / 1h)   ICF_VARIANT=missind"

# Prep: data prep + PS trimming on the missind cohort (4c/16G/2h)
JOBID1=$(sbatch \
  --job-name=hdicf_prep_missind \
  --parsable \
  --dependency=afterok:${JOBID0} \
  -c 4 \
  --mem=16G \
  --time=2:00:00 \
  --output="$LOG_DIR/prep_missind_%j.out" \
  --error="$LOG_DIR/prep_missind_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO
    module load R/4.5.1
    module load GCCcore/13.2.0
    export ICF_VARIANT=missind
    Rscript $HDICF_DIR/02_prepare_data.R
  ")
echo "Prep:   job $JOBID1  (4c / 16G / 2h)  ICF_VARIANT=missind  dep=$JOBID0"

# Step 1: Raw CF + VI + depth diagnostics on the missind cohort (4c/32G/16h)
# Same memory budget as the headline hdiCF step1 (which already includes
# the curated + HD feature set; cohort is ~6% larger so headroom is fine).
JOBID2=$(sbatch \
  --job-name=hdicf_step1_missind \
  --parsable \
  --dependency=afterok:${JOBID1} \
  -c 4 \
  --mem=32G \
  --time=16:00:00 \
  --output="$LOG_DIR/step1_missind_%j.out" \
  --error="$LOG_DIR/step1_missind_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO
    module load R/4.5.1
    module load GCCcore/13.2.0
    export ICF_VARIANT=missind
    Rscript $HDICF_DIR/03a_icf_step1.R
  ")
echo "Step 1: job $JOBID2  (4c / 32G / 16h)  ICF_VARIANT=missind  dep=$JOBID1"

echo ""
echo "Step-1 triage: $JOBID0 -> $JOBID1 -> $JOBID2"
echo ""
squeue -u "$USER" --format="%.10i %.9P %.20j %.2t %.10M %.5C %.8m %.12l"
REMOTE

echo ""
echo "Monitor:  ssh tensor 'squeue -u \$USER'"
echo "Logs:     ssh tensor 'cat $REMOTE_REPO/$LOG_DIR/{hdgen,prep,step1}_missind_*'"

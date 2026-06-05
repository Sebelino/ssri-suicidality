#!/usr/bin/env bash
set -euo pipefail

# submit_icf_step1_missind.sh
# Step-1-only triage rerun of the iCF pipeline under the missing-indicator
# variant (full eligible cohort + `any_miss` indicator; see analysis-icf/paths.R
# and analysis/common.R::apply_missind_recoding). Submits only the prep job
# and the raw-CF + variable-selection step1 job -- enough to answer:
#   * does `any_miss` show up in the variable-importance ranking?
#   * does the top of the VI list shift vs. the headline CCA run?
#   * does the variable-selection stability diagnostic change?
#
# Outputs (variant-suffixed, coexist with headline artefacts):
#   suicidality/analysis-icf/data/icf_data_missind.rds
#   suicidality/analysis-icf/output/icf_step1_missind.rds
#   suicidality/analysis-icf/output/depth_distribution_D*_missind.pdf
#
# Usage: ./tools/submit_icf_step1_missind.sh

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

REMOTE_REPO="~/work/ssri-suicidality"
ICF_DIR="suicidality/analysis-icf"
LOG_DIR="$ICF_DIR/logs"

# ---- Sync ----
echo "Syncing repo to Tensor..."
"$SCRIPT_DIR/sync-repo-to-tensor.sh"

# ---- Submit step-1 chain (prep -> step1) under ICF_VARIANT=missind ----
echo ""
echo "Submitting iCF step-1 triage (missing-indicator variant)..."

ssh tensor bash -s -- "$REMOTE_REPO" "$ICF_DIR" "$LOG_DIR" <<'REMOTE'
set -euo pipefail

REPO="$1"
ICF_DIR="$2"
LOG_DIR="$3"

cd "$REPO"
mkdir -p "$LOG_DIR"

# Prep: missing-indicator data preparation (1 core, 4 GB, 1h)
JOBID0=$(sbatch \
  --job-name=icf_prep_missind \
  --parsable \
  -c 1 \
  --mem=4G \
  --time=1:00:00 \
  --output="$LOG_DIR/prep_missind_%j.out" \
  --error="$LOG_DIR/prep_missind_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO
    module load R/4.5.1
    module load GCCcore/13.2.0
    export ICF_VARIANT=missind
    Rscript $ICF_DIR/01_prepare_data.R
  ")
echo "Prep:   job $JOBID0  (1c / 4G / 1h)   ICF_VARIANT=missind"

# Step 1: Raw CF + VI + depth diagnostics (4 cores, 32 GB, 12h)
# Same memory budget as the headline step1; cohort is ~6% larger (93,795 vs
# 88,427) so headroom remains comfortable.
JOBID1=$(sbatch \
  --job-name=icf_step1_missind \
  --parsable \
  --dependency=afterok:${JOBID0} \
  -c 4 \
  --mem=32G \
  --time=12:00:00 \
  --output="$LOG_DIR/step1_missind_%j.out" \
  --error="$LOG_DIR/step1_missind_%j.out" \
  --wrap="
    set -euo pipefail
    cd $REPO
    module load R/4.5.1
    module load GCCcore/13.2.0
    export ICF_VARIANT=missind
    Rscript $ICF_DIR/02a_icf_step1.R
  ")
echo "Step 1: job $JOBID1  (4c / 32G / 12h)  ICF_VARIANT=missind  dep=$JOBID0"

echo ""
echo "Step-1 triage: $JOBID0 -> $JOBID1"
echo ""
squeue -u "$USER" --format="%.10i %.9P %.20j %.2t %.10M %.5C %.8m %.12l"
REMOTE

echo ""
echo "Monitor:  ssh tensor 'squeue -u \$USER'"
echo "Logs:     ssh tensor 'cat $REMOTE_REPO/$LOG_DIR/{prep,step1}_missind_*'"

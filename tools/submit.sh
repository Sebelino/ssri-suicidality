#!/usr/bin/env bash
set -euo pipefail

# Usage: ./submit.sh <r_script.R>
# Examples:
#   ./submit.sh 12_process_base.R                           # extraction script
#   ./submit.sh suicidality/analysis-icf/01_prepare_data.R  # full path

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

# If path contains /, use as-is; otherwise assume suicidality/extraction/
if [[ "$1" == */* ]]; then
  r_script="$1"
else
  r_script="suicidality/extraction/$1"
fi
shift
extra_args="$*"

# Sync repo to tensor
"$REPO_DIR/tools/sync-repo-to-tensor.sh"

# Create job dir, update 'latest', submit job
ssh tensor bash -s -- "$r_script" "$extra_args" <<'EOF'
set -euo pipefail

R_SCRIPT="$1"
EXTRA_ARGS="${2:-}"

JOBS_DIR="$HOME/jobs"
TS=$(date +"%Y%m%d_%H%M%S")
JOBDIR="$JOBS_DIR/job_${TS}"

mkdir -p "$JOBDIR"

# Atomically update 'latest' symlink
ln -sfn "$JOBDIR" "$JOBS_DIR/latest"

JOBID=$(sbatch \
  --output="$JOBDIR/slurm.out" \
  --error="$JOBDIR/slurm.err" \
  "$HOME/work/ssri-suicidality/tools/run_r.sbatch" \
  "$R_SCRIPT" $EXTRA_ARGS | awk '{print $4}')

echo "$JOBID" > "$JOBDIR/jobid"
echo "Submitted job $JOBID → $JOBDIR"
EOF

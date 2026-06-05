#!/usr/bin/env bash
set -euo pipefail

# ---------------------------------------------------------------------
# Argument checking
# ---------------------------------------------------------------------

if [[ $# -ne 1 ]]; then
  echo "Usage: $0 path/to/script.R"
  exit 1
fi

LOCAL_SCRIPT="$1"

if [[ ! -f "$LOCAL_SCRIPT" ]]; then
  echo "Error: R script not found: $LOCAL_SCRIPT"
  exit 1
fi

# ---------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------

SSH_HOST="tensor"

SCRIPT_NAME="$(basename "$LOCAL_SCRIPT")"
SCRIPT_STEM="${SCRIPT_NAME%.R}"

REMOTE_HOME="$(ssh tensor pwd)"

REMOTE_DIR="$REMOTE_HOME/.tensor_run"
REMOTE_SCRIPT="$REMOTE_DIR/$SCRIPT_NAME"
REMOTE_JOB="$REMOTE_DIR/run_${SCRIPT_STEM}.sbatch"
REMOTE_LOG="$REMOTE_DIR/run_${SCRIPT_STEM}.log"
REMOTE_STATUS="$REMOTE_DIR/run_${SCRIPT_STEM}.status"

SLURM_CPUS=4
SLURM_MEM=16G
SLURM_TIME=2:00:00
SLURM_JOB_NAME="run_${SCRIPT_STEM}"

# ---------------------------------------------------------------------
# Copy script
# ---------------------------------------------------------------------

echo "▶ Copying script to tensor..."
ssh -T "$SSH_HOST" "mkdir -p '$REMOTE_DIR'"
scp "$LOCAL_SCRIPT" "$SSH_HOST:$REMOTE_SCRIPT"

# ---------------------------------------------------------------------
# Create job script
# ---------------------------------------------------------------------

ssh -T "$SSH_HOST" <<EOF
set -euo pipefail

cat > "$REMOTE_JOB" <<'SBATCH_EOF'
#!/usr/bin/env bash
#SBATCH --job-name=${SLURM_JOB_NAME}
#SBATCH -c ${SLURM_CPUS}
#SBATCH --mem=${SLURM_MEM}
#SBATCH -t ${SLURM_TIME}
#SBATCH --output=${REMOTE_LOG}
#SBATCH --error=${REMOTE_LOG}

set -euo pipefail

echo "▶ On compute node: \$(hostname)"

module purge
module load R/4.5.1
module load GCCcore/13.2.0
module load unixODBC
module load mebauth

echo "▶ Running R script..."
Rscript "$REMOTE_SCRIPT"
status=\$?

echo "▶ R script finished with exit code \$status"
echo "\$status" > "$REMOTE_STATUS"

exit \$status
SBATCH_EOF

chmod +x "$REMOTE_JOB"
EOF

# ---------------------------------------------------------------------
# Submit job (ROBUST JOB ID PARSING)
# ---------------------------------------------------------------------

echo "▶ Submitting Slurm job..."

JOB_ID="$(
ssh -T "$SSH_HOST" "sbatch '$REMOTE_JOB'" | grep -oE '[0-9]+$'
)"

echo "▶ Job ID: $JOB_ID"
echo "▶ Streaming output..."

# ---------------------------------------------------------------------
# Stream output live
# ---------------------------------------------------------------------

ssh -T "$SSH_HOST" <<EOF
set -euo pipefail

while [[ ! -f "$REMOTE_LOG" ]]; do
  sleep 0.2
done

tail -f "$REMOTE_LOG" &
TAIL_PID=\$!

while squeue -j "$JOB_ID" 2>/dev/null | grep -q "$JOB_ID"; do
  sleep 1
done

kill "\$TAIL_PID"
EOF

# ---------------------------------------------------------------------
# Retrieve exit code (race-free)
# ---------------------------------------------------------------------

ssh -T "$SSH_HOST" <<EOF
set -euo pipefail
while [[ ! -f "$REMOTE_STATUS" ]]; do
  sleep 0.2
done
EOF

EXIT_CODE="$(ssh -T "$SSH_HOST" "cat '$REMOTE_STATUS'")"

# ---------------------------------------------------------------------
# Cleanup (SAFE)
# ---------------------------------------------------------------------

ssh -T "$SSH_HOST" "rm -f '$REMOTE_JOB' '$REMOTE_SCRIPT' '$REMOTE_STATUS' '$REMOTE_LOG'"

# ---------------------------------------------------------------------
# Final status
# ---------------------------------------------------------------------

if [[ "$EXIT_CODE" -ne 0 ]]; then
  echo "✖ Remote R job failed with exit code $EXIT_CODE"
  exit "$EXIT_CODE"
fi

echo "✔ Remote R job completed successfully"
echo "▶ Done."

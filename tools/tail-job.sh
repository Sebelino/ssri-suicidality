#!/usr/bin/env bash
set -euo pipefail

ssh tensor bash -s <<'EOF'
set -euo pipefail

JOBS_DIR="$HOME/jobs"
JOBDIR="$(readlink -f "$JOBS_DIR/latest")"

OUT="$JOBDIR/slurm.out"
JOBID="$(cat "$JOBDIR/jobid")"

echo "📄 Job dir: $JOBDIR"
echo "🆔 Job ID:  $JOBID"
echo

# Wait for output file to appear
while [[ ! -f "$OUT" ]]; do
  sleep 0.2
done

# Tail output
tail -n +1 -f "$OUT" &
TAIL_PID=$!

# Block until job leaves queue
while squeue -j "$JOBID" -h | grep -q .; do
  sleep 2
done

# Stop tailing
kill "$TAIL_PID" 2>/dev/null || true
wait "$TAIL_PID" 2>/dev/null || true

echo
echo "✅ Job $JOBID finished"

# Final status (authoritative)
sacct -j "$JOBID" --format=JobID,State,ExitCode --noheader
EOF

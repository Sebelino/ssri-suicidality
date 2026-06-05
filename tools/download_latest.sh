#!/usr/bin/env bash
set -euo pipefail

# download_latest.sh
#
# Download a reproduce_on_tensor.sh job directory from the cluster.
# Defaults to the run that ~/jobs/latest currently points at, so the most
# common usage is just:
#
#   ./tools/download_latest.sh
#
# Options:
#   --tag TAG      Specific job tag to fetch (default: resolve ~/jobs/latest)
#   --host HOST    SSH alias (default: tensor)
#   --dest DIR     Local destination directory (default: ./job_results/)
#   --lean         Skip the multi-MB RDS bundles (extraction cohorts and the
#                  iCF/hdiCF data/output .rds files). Keeps logs, figures,
#                  .tex macros, and the compiled thesis.
#   --dry-run      Print the rsync command, do not transfer
#   -h, --help     Show this help

usage() { sed -n '3,18p' "$0"; }

HOST="tensor"
TAG=""
DEST=""
LEAN=0
DRY_RUN=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --tag)     TAG="$2"; shift 2 ;;
    --host)    HOST="$2"; shift 2 ;;
    --dest)    DEST="$2"; shift 2 ;;
    --lean)    LEAN=1; shift ;;
    --dry-run) DRY_RUN=1; shift ;;
    -h|--help) usage; exit 0 ;;
    *) echo "Unknown option: $1" >&2; usage >&2; exit 1 ;;
  esac
done

# Preflight: confirm ssh works
if ! ssh -o BatchMode=yes -o ConnectTimeout=10 "$HOST" true 2>/dev/null; then
  echo "ERROR: cannot ssh to '$HOST' non-interactively." >&2
  exit 1
fi

# Resolve the tag if not given: dereference ~/jobs/latest on the remote.
if [[ -z "$TAG" ]]; then
  REMOTE_PATH=$(ssh "$HOST" "readlink -f \"\$HOME/jobs/latest\" 2>/dev/null") || true
  if [[ -z "$REMOTE_PATH" ]]; then
    echo "ERROR: ~/jobs/latest does not exist on $HOST. Pass --tag TAG explicitly." >&2
    exit 1
  fi
  TAG=$(basename "$REMOTE_PATH")
  echo "Resolved ~/jobs/latest -> $TAG"
else
  REMOTE_PATH="\$HOME/jobs/$TAG"
fi

# Confirm the directory exists
if ! ssh "$HOST" "test -d \"$REMOTE_PATH\""; then
  echo "ERROR: $HOST:$REMOTE_PATH does not exist." >&2
  exit 1
fi

# Local destination
if [[ -z "$DEST" ]]; then
  DEST="./job_results"
fi

EXCLUDES=()
if [[ "$LEAN" -eq 1 ]]; then
  EXCLUDES+=(
    --exclude='repo/suicidality/extraction/output/rds/'
    --exclude='repo/suicidality/analysis-icf/data/'
    --exclude='repo/suicidality/analysis-hdicf/data/'
    --exclude='repo/suicidality/analysis-icf/output/*.rds'
    --exclude='repo/suicidality/analysis-hdicf/output/*.rds'
  )
fi

mkdir -p "$DEST"

echo ""
echo "From:  $HOST:$REMOTE_PATH/"
echo "To:    $DEST/"
echo "Mode:  $([[ "$LEAN" -eq 1 ]] && echo lean || echo full)"
echo ""

if [[ "$DRY_RUN" -eq 1 ]]; then
  echo "(dry run)"
  echo "rsync -avz --progress ${EXCLUDES[*]+${EXCLUDES[*]}} \"$HOST:$REMOTE_PATH/\" \"$DEST/\""
  exit 0
fi

# The ${arr[@]+"${arr[@]}"} idiom expands to nothing when the array is empty
# (vs ${arr[@]-} which yields a stray empty-string arg that openrsync on macOS
# misparses as a local source path).
rsync -avz --progress ${EXCLUDES[@]+"${EXCLUDES[@]}"} "$HOST:$REMOTE_PATH/" "$DEST/"

echo ""
echo "Done. Highlights:"
[[ -f "$DEST/repo/suicidality/report/thesis.pdf" ]] && \
  echo "  thesis pdf:       $DEST/repo/suicidality/report/thesis.pdf"
[[ -f "$DEST/job_chain.txt" ]] && \
  echo "  job manifest:     $DEST/job_chain.txt"
[[ -d "$DEST/logs" ]] && \
  echo "  slurm logs:       $DEST/logs/"
[[ -f "$DEST/repo/suicidality/analysis-icf/output/icf_results.rds" ]] && \
  echo "  iCF results:      $DEST/repo/suicidality/analysis-icf/output/icf_results.rds"
[[ -f "$DEST/repo/suicidality/analysis-hdicf/output/icf_results.rds" ]] && \
  echo "  hdiCF results:    $DEST/repo/suicidality/analysis-hdicf/output/icf_results.rds"

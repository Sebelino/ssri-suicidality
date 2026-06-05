#!/usr/bin/env bash
#SBATCH -c 4
#SBATCH --time=48:00:00
#SBATCH --mem=32G

set -euo pipefail

if [[ $# -lt 1 ]]; then
  echo "Usage: sbatch run_r.sbatch <script.R> [args...]"
  exit 1
fi

RSCRIPT="$1"
shift

cd "$HOME/work/ssri-suicidality"
module load R/4.5.1
module load GCCcore/13.2.0
module load unixODBC
module load mebauth

# Run from the script's directory so relative paths work
cd "$(dirname "$RSCRIPT")"
Rscript "$(basename "$RSCRIPT")" "$@"

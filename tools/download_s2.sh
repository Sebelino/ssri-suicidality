#!/usr/bin/env bash
set -euo pipefail
# Downloads R-generated output files from the remote cluster

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

rsync -avz tensor:~/work/ssri-suicidality/suicidality/extraction/output/rds/ "$REPO_DIR/suicidality/extraction/output/rds/"

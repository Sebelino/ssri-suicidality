#!/bin/bash
set -euo pipefail
# Sync RDS extraction output to tensor

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

rsync -avz --progress \
    "$REPO_DIR/suicidality/extraction/output/rds/" \
    tensor:~/work/ssri-suicidality/suicidality/extraction/output/rds/

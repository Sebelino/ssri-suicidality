#!/bin/bash
set -euo pipefail
# Sync repository to tensor, excluding .git/ and .rds files

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"

rsync -avz --progress \
    --exclude='.git/' \
    --exclude='*.rds' \
    --exclude='*.sas7bdat' \
    --exclude='*.RData' \
    --exclude='*.Rdata' \
    --exclude='gitignore/' \
    --exclude='output/' \
    "$REPO_DIR/" \
    tensor:~/work/ssri-suicidality/

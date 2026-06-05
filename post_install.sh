#!/usr/bin/env bash

set -Eeuo pipefail

mamba env update -n thesis -f environment.yml

Rscript -e 'install.packages(c("odbc", "lubridate", "tidyverse", "partykit"), repos="https://cloud.r-project.org")'

if [[ "$(uname)" == "Darwin" ]]; then
    brew install msodbcsql18

    # Create activation script to set ODBC driver path
    ACTIVATE_DIR="$CONDA_PREFIX/etc/conda/activate.d"
    mkdir -p "$ACTIVATE_DIR"
    cat > "$ACTIVATE_DIR/env_vars.sh" << 'EOF'
#!/bin/bash
export ODBCSYSINI=/opt/homebrew/etc
# Use file-based Kerberos cache (MIT Kerberos in mamba doesn't support macOS KCM)
export KRB5CCNAME=FILE:/tmp/krb5cc_$(id -u)
EOF
fi

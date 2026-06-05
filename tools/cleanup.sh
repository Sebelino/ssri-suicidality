#!/usr/bin/env bash
# cleanup.sh - Kill hanging extraction scripts and database connections
#
# Usage:
#   ./cleanup.sh        # Show what would be killed (dry run)
#   ./cleanup.sh -f     # Actually kill the processes
#   ./cleanup.sh --force

set -euo pipefail

FORCE=false
if [[ "${1:-}" == "-f" || "${1:-}" == "--force" ]]; then
  FORCE=true
fi

echo "=============================================="
echo "Extraction Cleanup Script"
echo "=============================================="
echo ""

# Find R processes running extraction scripts
echo "=== R processes running extraction scripts ==="
R_PIDS=$(ps aux | grep -E "/R .*--file=.*\.R|Rscript.*\.R" | grep -v grep | grep -v RStudio | awk '{print $2}' || true)

if [[ -z "$R_PIDS" ]]; then
  echo "None found"
else
  ps aux | grep -E "/R .*--file=.*\.R|Rscript.*\.R" | grep -v grep | grep -v RStudio
  echo ""
  echo "PIDs: $R_PIDS"
fi

# Find processes with database connections to MEB
echo ""
echo "=== Processes with SQL Server connections (port 1433) ==="
DB_PIDS=$(lsof -i :1433 2>/dev/null | grep -E "ESTABLISHED|SYN_SENT" | awk '{print $2}' | sort -u || true)

if [[ -z "$DB_PIDS" ]]; then
  echo "None found"
else
  lsof -i :1433 2>/dev/null | head -20
  echo ""
  echo "PIDs with active connections: $DB_PIDS"
fi

# Find bash wrapper processes for extraction
echo ""
echo "=== Bash wrapper processes ==="
BASH_PIDS=$(ps aux | grep -E "bash.*Rscript.*\.R" | grep -v grep | awk '{print $2}' || true)

if [[ -z "$BASH_PIDS" ]]; then
  echo "None found"
else
  ps aux | grep -E "bash.*Rscript.*\.R" | grep -v grep
  echo ""
  echo "PIDs: $BASH_PIDS"
fi

# Find tee processes for log files
echo ""
echo "=== Tee processes (log writers) ==="
TEE_PIDS=$(ps aux | grep -E "tee.*log/.*\.log" | grep -v grep | awk '{print $2}' || true)

if [[ -z "$TEE_PIDS" ]]; then
  echo "None found"
else
  ps aux | grep -E "tee.*log/.*\.log" | grep -v grep
  echo ""
  echo "PIDs: $TEE_PIDS"
fi

# Combine all PIDs
ALL_PIDS=$(echo "$R_PIDS $DB_PIDS $BASH_PIDS $TEE_PIDS" | tr ' ' '\n' | sort -u | grep -v '^$' | tr '\n' ' ')

echo ""
echo "=============================================="

if [[ -z "$ALL_PIDS" ]]; then
  echo "No processes to kill. All clean!"
  exit 0
fi

echo "All PIDs to kill: $ALL_PIDS"
echo ""

if [[ "$FORCE" == "true" ]]; then
  echo "Killing processes..."
  for pid in $ALL_PIDS; do
    if ps -p "$pid" > /dev/null 2>&1; then
      echo "  Killing PID $pid..."
      kill "$pid" 2>/dev/null || true
    fi
  done

  # Wait a moment then check if any survived
  sleep 2

  SURVIVORS=""
  for pid in $ALL_PIDS; do
    if ps -p "$pid" > /dev/null 2>&1; then
      SURVIVORS="$SURVIVORS $pid"
    fi
  done

  if [[ -n "$SURVIVORS" ]]; then
    echo ""
    echo "Some processes survived, sending SIGKILL..."
    for pid in $SURVIVORS; do
      echo "  Force killing PID $pid..."
      kill -9 "$pid" 2>/dev/null || true
    done
  fi

  echo ""
  echo "Done. Verifying cleanup..."
  sleep 1

  # Final check
  REMAINING=$(ps aux | grep -E "/R .*--file=.*\.R|Rscript.*\.R" | grep -v grep | grep -v RStudio || true)
  if [[ -z "$REMAINING" ]]; then
    echo "All extraction processes killed successfully."
  else
    echo "WARNING: Some processes may still be running:"
    echo "$REMAINING"
  fi

  # Check DB connections
  DB_CHECK=$(lsof -i :1433 2>/dev/null | grep -E "ESTABLISHED" || true)
  if [[ -z "$DB_CHECK" ]]; then
    echo "All database connections closed."
  else
    echo "WARNING: Some database connections may still be open:"
    echo "$DB_CHECK"
  fi
else
  echo "Dry run - no processes killed."
  echo "Run with -f or --force to actually kill these processes:"
  echo ""
  echo "  ./cleanup.sh -f"
fi

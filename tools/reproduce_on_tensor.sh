#!/usr/bin/env bash
set -euo pipefail

# reproduce_on_tensor.sh
#
# One-command reproduction of the full study on the tensor HPC cluster.
#
# What it does:
#   1. Rsyncs the local working tree to tensor:~/jobs/<TAG>/repo/
#      (excludes .git/, RDS/SAS/RData artefacts, gitignore/, output/)
#   2. Submits a single SLURM job chain (extract -> analysis -> iCF / hdiCF -> thesis)
#      with --dependency=afterok edges, isolated to that job directory.
#   3. Returns the top-level job IDs and the monitoring command.
#
# After submission you can ssh tensor and follow ~/jobs/<TAG>/logs/.
# The thesis pdf lands at ~/jobs/<TAG>/repo/suicidality/report/thesis.pdf
# once the full chain (~30+ h wallclock) completes.
#
# Usage:
#   ./tools/reproduce_on_tensor.sh [options]
#
# Options:
#   --name TAG          Job dir name under ~/jobs/ (default: repro_<timestamp>)
#   --resume TAG        Resume an existing ~/jobs/<TAG>/ run: re-rsync code,
#                       skip stages whose output artefacts are already on disk.
#                       Mutually exclusive with --name.
#   --force             Re-submit every stage even if artefacts exist
#                       (overrides the resume skip logic; works with or without
#                       --resume).
#   --host HOST         SSH alias for the cluster (default: tensor)
#   --skip-extract      Reuse RDS files already in repo/suicidality/extraction/output/
#                       (useful when the DB extraction has been run before)
#   --skip-icf          Do not run the iCF pipeline
#   --skip-hdicf        Do not run the hdiCF pipeline
#   --skip-thesis       Do not build the thesis pdf
#   --dry-run           Print the rsync + submission plan, do not execute
#   -h, --help          Show this help

usage() {
  sed -n '3,30p' "$0"
}

HOST="tensor"
TAG=""
RESUME_TAG=""
FORCE=0
SKIP_EXTRACT=0
SKIP_ICF=0
SKIP_HDICF=0
SKIP_THESIS=0
DRY_RUN=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --name)         TAG="$2"; shift 2 ;;
    --resume)       RESUME_TAG="$2"; shift 2 ;;
    --force)        FORCE=1; shift ;;
    --host)         HOST="$2"; shift 2 ;;
    --skip-extract) SKIP_EXTRACT=1; shift ;;
    --skip-icf)     SKIP_ICF=1; shift ;;
    --skip-hdicf)   SKIP_HDICF=1; shift ;;
    --skip-thesis)  SKIP_THESIS=1; shift ;;
    --dry-run)      DRY_RUN=1; shift ;;
    -h|--help)      usage; exit 0 ;;
    *) echo "Unknown option: $1" >&2; usage >&2; exit 1 ;;
  esac
done

if [[ -n "$RESUME_TAG" && -n "$TAG" ]]; then
  echo "ERROR: --resume and --name are mutually exclusive." >&2
  exit 1
fi
if [[ -n "$RESUME_TAG" ]]; then
  TAG="$RESUME_TAG"
fi
if [[ -z "$TAG" ]]; then
  TAG="repro_$(date +%Y%m%d_%H%M%S)"
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
REMOTE_BASE="\$HOME/jobs/$TAG"
REMOTE_REPO="$REMOTE_BASE/repo"
REMOTE_LOGS="$REMOTE_BASE/logs"

echo "=============================================================="
echo " Reproducing ssri-suicidality on $HOST"
echo " Local repo : $REPO_DIR"
echo " Remote tag : $TAG"
echo " Remote base: $REMOTE_BASE"
echo "=============================================================="

# ---- Preflight: confirm ssh works ----
if ! ssh -o BatchMode=yes -o ConnectTimeout=10 "$HOST" true 2>/dev/null; then
  echo "ERROR: cannot ssh to '$HOST' non-interactively." >&2
  echo "Add an entry to ~/.ssh/config and make sure your key/Kerberos session is valid." >&2
  exit 1
fi

# ---- Resume target validation ----
if [[ -n "$RESUME_TAG" ]]; then
  if ! ssh "$HOST" "test -d \"\$HOME/jobs/$RESUME_TAG/repo\""; then
    echo "ERROR: --resume target ~/jobs/$RESUME_TAG/repo does not exist on $HOST." >&2
    exit 1
  fi
  echo "Resuming run: $RESUME_TAG"
fi

# ---- Rsync the codebase into the job dir ----
echo ""
echo "[1/2] Syncing codebase to $HOST:$REMOTE_REPO/ ..."

if [[ "$DRY_RUN" -eq 1 ]]; then
  echo "      (dry run; would rsync the repo and submit the chain below)"
else
  # Pre-create the job dir and every output directory that pipeline scripts
  # write into without their own dir.create() guard. Cheap, idempotent, and
  # avoids "cannot open file" failures partway through a long job.
  ssh "$HOST" "mkdir -p \
    \"$REMOTE_REPO\" \
    \"$REMOTE_LOGS\" \
    \"$REMOTE_REPO/suicidality/extraction/output/rds\" \
    \"$REMOTE_REPO/suicidality/extraction/log\" \
    \"$REMOTE_REPO/suicidality/analysis/output\" \
    \"$REMOTE_REPO/suicidality/analysis-icf/data\" \
    \"$REMOTE_REPO/suicidality/analysis-icf/output\" \
    \"$REMOTE_REPO/suicidality/analysis-icf/logs\" \
    \"$REMOTE_REPO/suicidality/analysis-hdicf/data\" \
    \"$REMOTE_REPO/suicidality/analysis-hdicf/output\" \
    \"$REMOTE_REPO/suicidality/analysis-hdicf/logs\""
  # Atomic 'latest' symlink so ssh tensor cd ~/jobs/latest always points at
  # the most recently submitted run (matches the convention of tools/submit.sh).
  ssh "$HOST" "ln -sfn \"$REMOTE_BASE\" \"\$HOME/jobs/latest\""
  # Pass 1: codebase, no data artefacts
  rsync -az --delete \
      --exclude='.git/' \
      --exclude='*.rds' \
      --exclude='*.sas7bdat' \
      --exclude='*.RData' \
      --exclude='*.Rdata' \
      --exclude='.Rhistory' \
      --exclude='.Rproj.user/' \
      --exclude='gitignore/' \
      --exclude='output/' \
      --exclude='log/' \
      --exclude='logs/' \
      --exclude='suicidality/report/thesis.pdf' \
      --exclude='suicidality/report/figures/' \
      --exclude='suicidality/report/generated/' \
      --exclude='suicidality/report/config.mk' \
      --exclude='suicidality/report/*.aux' \
      --exclude='suicidality/report/*.log' \
      --exclude='suicidality/report/*.toc' \
      --exclude='suicidality/report/*.bbl' \
      --exclude='suicidality/report/*.blg' \
      --exclude='suicidality/report/*.out' \
      "$REPO_DIR/" \
      "$HOST:$REMOTE_REPO/"

  # Pass 2: when reusing previously-extracted data, carry only the
  # extraction/output/rds/ directory (everything else under output/ will be
  # regenerated by the SLURM chain).
  if [[ "$SKIP_EXTRACT" -eq 1 ]]; then
    if [[ ! -d "$REPO_DIR/suicidality/extraction/output/rds" ]]; then
      echo "ERROR: --skip-extract requires local RDS files in" >&2
      echo "       $REPO_DIR/suicidality/extraction/output/rds/" >&2
      exit 1
    fi
    echo "      (--skip-extract: also syncing extraction/output/rds/)"
    rsync -az \
        "$REPO_DIR/suicidality/extraction/output/rds/" \
        "$HOST:$REMOTE_REPO/suicidality/extraction/output/rds/"
  fi
fi

# ---- Submit the SLURM chain ----
echo ""
echo "[2/2] Submitting SLURM job chain on $HOST ..."

submit_remote() {
  ssh "$HOST" bash -s -- \
      "$REMOTE_BASE" "$SKIP_EXTRACT" "$SKIP_ICF" "$SKIP_HDICF" "$SKIP_THESIS" "$FORCE" <<'REMOTE'
set -euo pipefail

BASE="$1"
SKIP_EXTRACT="$2"
SKIP_ICF="$3"
SKIP_HDICF="$4"
SKIP_THESIS="$5"
FORCE="$6"

REPO="$BASE/repo"
LOGS="$BASE/logs"
mkdir -p "$LOGS"

# -------- Resume / skip-if-done helpers --------
# Each stage probes for a definitive artefact. If present (and --force is off),
# the stage is skipped and downstream jobs lose their dep on it.

stage_done() {
  # Usage: stage_done <artefact> [<artefact> ...]
  # Returns 0 (done) if ALL listed artefacts exist; 1 otherwise. --force always
  # returns 1 so every stage runs.
  [[ "$FORCE" -eq 1 ]] && return 1
  local f
  for f in "$@"; do
    [[ -f "$REPO/$f" ]] || return 1
  done
  return 0
}

# Build a "--dependency=afterok:JID" clause from a single JID, or "" if empty.
# Trailing `return 0` so a falsy `[[ -n ... ]]` doesn't trip `set -e`.
dep_of() {
  local jid="$1"
  [[ -n "$jid" ]] && echo "--dependency=afterok:$jid"
  return 0
}

# Build a combined dependency clause from multiple JIDs; skips empties.
combine_deps() {
  local out="" j
  for j in "$@"; do
    [[ -n "$j" ]] && out="${out:+$out,}$j"
  done
  [[ -n "$out" ]] && echo "--dependency=afterok:$out"
  return 0
}

# Common SLURM wrapper: cd into the isolated repo copy, load modules, run a
# command. All R scripts are executed via this wrapper so that here::here()
# resolves inside ~/jobs/<TAG>/repo/ rather than the shared ~/work/ checkout.
make_wrap() {
  local body="$1"
  cat <<WRAP
set -euo pipefail
cd "$REPO"
module load R/4.5.1
module load GCCcore/13.2.0
module load unixODBC || true
module load mebauth  || true
# texlive provides pdfcrop, used by 03_visualize_results.R to tightly crop
# the decision-tree PDFs after ggsave(). Without it the cropping step prints
# a warning and ships uncropped cairo PDFs with whitespace around each tree,
# which the thesis Makefile then embeds with awkward spacing.
module load texlive  || true
$body
WRAP
}

# ---------- 0. R bootstrap (install missing CRAN deps into ~/R/library) ----------
# Idempotent; on a fully-installed system this finishes in seconds.
JID_BOOT=$(sbatch --parsable \
  --job-name="ssri_rdeps" \
  -c 4 --mem=8G --time=1:00:00 \
  --output="$LOGS/00_rdeps_%j.out" \
  --error="$LOGS/00_rdeps_%j.out" \
  --wrap="$(make_wrap 'Rscript tools/install_r_packages.R')")
echo "r_bootstrap:    job $JID_BOOT  (4c / 8G / 1h)"
DEP_BOOT="--dependency=afterok:$JID_BOOT"

# ---------- 1. Extraction (DB scripts 01-11 + processing 12-24) ----------
JID_EXTRACT=""
if [[ "$SKIP_EXTRACT" -eq 1 ]]; then
  echo "extract:        skipped (--skip-extract); relying on RDS files in repo"
elif stage_done \
       "suicidality/extraction/output/rds/main_12wks_28.rds" \
       "suicidality/extraction/output/rds/main_12wks_14.rds" \
       "suicidality/extraction/output/rds/pp_12wks_max.rds"; then
  echo "extract:        already complete (RDS cohorts present)"
else
  # 48G needed for 23_process_time_varying.R: it splits the 52-week PP cohort
  # (~93k patients * 52 weeks = ~3.4M weekly periods) and joins multi-million
  # row prescription tables. 16G OOMed on this script as of 2026-05-27.
  JID_EXTRACT=$(sbatch --parsable \
    --job-name="ssri_extract" \
    $(dep_of "$JID_BOOT") \
    -c 4 --mem=48G --time=24:00:00 \
    --output="$LOGS/01_extract_%j.out" \
    --error="$LOGS/01_extract_%j.out" \
    --wrap="$(make_wrap '
cd suicidality/extraction
Rscript Extract_all.R
# Also build the 14-day grace-period sensitivity cohort (main_12wks_14.rds),
# which the analysis stage uses in cate_by_source.R for the sensitivity arm.
Rscript build_grace14_cohort.R')")
  echo "extract:        job $JID_EXTRACT  (4c / 48G / 24h)  dep=$JID_BOOT"
fi

# ---------- 2. Headline analysis (ITT, missingness, Table-2 macros) ----------
# These scripts only need the extracted RDS files. They generate the figures
# and .tex macros consumed directly by the thesis (Table 2, ITT survival, etc.).
JID_ANALYSIS=""
if stage_done "suicidality/analysis-icf/output/thesis_values.tex"; then
  echo "analysis_main:  already complete (thesis_values.tex present)"
else
  ANALYSIS_PRE_CMDS='
Rscript suicidality/analysis/ITT_12wks.R
Rscript suicidality/analysis/ITT_12wks_missind.R
Rscript suicidality/analysis/missingness_patterns.R
Rscript suicidality/analysis/predi_diff_distribution.R
Rscript suicidality/analysis/cate_by_source.R
Rscript suicidality/analysis/cate_by_prior_suicidal.R
Rscript suicidality/analysis/Summary_statistics.R
Rscript suicidality/analysis/export_thesis_values.R
'
  JID_ANALYSIS=$(sbatch --parsable \
    --job-name="ssri_analysis" \
    $(combine_deps "$JID_BOOT" "$JID_EXTRACT") \
    -c 4 --mem=16G --time=4:00:00 \
    --output="$LOGS/02_analysis_%j.out" \
    --error="$LOGS/02_analysis_%j.out" \
    --wrap="$(make_wrap "$ANALYSIS_PRE_CMDS")")
  echo "analysis_main:  job $JID_ANALYSIS  (4c / 16G / 4h)   deps=[$JID_BOOT,$JID_EXTRACT]"
fi

# ---------- 3. iCF pipeline (analysis-icf/) ----------
JID_ICF_PREP=""
JID_ICF_S1=""
JID_ICF_S2=""
JID_ICF_S3A=""
JID_ICF_S3B=""
JID_ICF_VIZ=""
JID_ICF_LATEX=""
JID_ICF_VAL=""
ICF_DEPS=()
if [[ "$SKIP_ICF" -eq 1 ]]; then
  echo "icf:            skipped (--skip-icf)"
else
  ICF_DIR="suicidality/analysis-icf"

  if stage_done "$ICF_DIR/data/icf_data.rds"; then
    echo "icf_prep:       already complete"
  else
    JID_ICF_PREP=$(sbatch --parsable \
      --job-name="icf_prep" \
      $(combine_deps "$JID_BOOT" "$JID_EXTRACT") \
      -c 1 --mem=4G --time=1:00:00 \
      --output="$LOGS/icf_01_prep_%j.out" \
      --error="$LOGS/icf_01_prep_%j.out" \
      --wrap="$(make_wrap "Rscript $ICF_DIR/01_prepare_data.R")")
    echo "icf_prep:       job $JID_ICF_PREP   (1c / 4G / 1h)"
  fi

  # step3b's merge script file.remove()s the upstream icf_step1.rds, cv_fold*,
  # and icf_batch_* artefacts as a space-saving cleanup. Once the terminal
  # icf_results.rds exists, the upstream checks below would all fail — so use
  # the terminal artefact as a short-circuit "everything upstream was done".
  icf_terminal_done=0
  [[ "$FORCE" -eq 0 && -f "$REPO/$ICF_DIR/output/icf_results.rds" ]] && icf_terminal_done=1

  if [[ "$icf_terminal_done" -eq 1 ]] || stage_done "$ICF_DIR/output/icf_step1.rds"; then
    echo "icf_step1:      already complete"
  else
    JID_ICF_S1=$(sbatch --parsable \
      --job-name="icf_step1" \
      $(dep_of "$JID_ICF_PREP") \
      -c 4 --mem=32G --time=12:00:00 \
      --output="$LOGS/icf_02_step1_%j.out" \
      --error="$LOGS/icf_02_step1_%j.out" \
      --wrap="$(make_wrap "Rscript $ICF_DIR/02a_icf_step1.R")")
    echo "icf_step1:      job $JID_ICF_S1   (4c / 32G / 12h)  dep=$JID_ICF_PREP"
  fi

  # Step2 produces 20 cv-fold files (5 folds x 4 depths). Skip if all present
  # OR if step3b already consumed them.
  icf_step2_done=1
  for f in 1 2 3 4 5; do
    for d in 2 3 4 5; do
      [[ -f "$REPO/$ICF_DIR/output/icf_cv_fold${f}_depth${d}.rds" ]] || icf_step2_done=0
    done
  done
  if [[ "$icf_terminal_done" -eq 1 ]] || [[ "$FORCE" -eq 0 && "$icf_step2_done" -eq 1 ]]; then
    echo "icf_step2:      already complete"
  else
    JID_ICF_S2=$(sbatch --parsable \
      --job-name="icf_step2" \
      --array=1-20 \
      $(dep_of "$JID_ICF_S1") \
      -c 2 --mem=4G --time=4:00:00 \
      --output="$LOGS/icf_03_step2_%A_%a.out" \
      --error="$LOGS/icf_03_step2_%A_%a.out" \
      --wrap="$(make_wrap '
FOLD=$(( (SLURM_ARRAY_TASK_ID - 1) % 5 + 1 ))
DEPTH=$(( (SLURM_ARRAY_TASK_ID - 1) / 5 + 2 ))
Rscript suicidality/analysis-icf/02b_icf_step2_fold.R "$FOLD" "$DEPTH"')")
    echo "icf_step2:      array $JID_ICF_S2 [1-20]  (2c / 4G / 4h)  dep=$JID_ICF_S1"
  fi

  # Step3a produces 20 batch files — likewise cleaned up by step3b.
  icf_step3a_done=1
  for b in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20; do
    [[ -f "$REPO/$ICF_DIR/output/icf_batch_${b}.rds" ]] || icf_step3a_done=0
  done
  if [[ "$icf_terminal_done" -eq 1 ]] || [[ "$FORCE" -eq 0 && "$icf_step3a_done" -eq 1 ]]; then
    echo "icf_step3a:     already complete"
  else
    JID_ICF_S3A=$(sbatch --parsable \
      --job-name="icf_step3a" \
      --array=1-20 \
      $(dep_of "$JID_ICF_S2") \
      -c 2 --mem=4G --time=4:00:00 \
      --output="$LOGS/icf_04_step3a_%A_%a.out" \
      --error="$LOGS/icf_04_step3a_%A_%a.out" \
      --wrap="$(make_wrap '
Rscript suicidality/analysis-icf/02c1_icf_step3_batch.R "$SLURM_ARRAY_TASK_ID" 20')")
    echo "icf_step3a:     array $JID_ICF_S3A [1-20]  (2c / 4G / 4h)  dep=$JID_ICF_S2"
  fi

  if stage_done "$ICF_DIR/output/icf_results.rds"; then
    echo "icf_step3b:     already complete (icf_results.rds present)"
  else
    JID_ICF_S3B=$(sbatch --parsable \
      --job-name="icf_step3b" \
      $(dep_of "$JID_ICF_S3A") \
      -c 4 --mem=16G --time=8:00:00 \
      --output="$LOGS/icf_05_step3b_%j.out" \
      --error="$LOGS/icf_05_step3b_%j.out" \
      --wrap="$(make_wrap "Rscript $ICF_DIR/02c2_icf_step3_merge.R")")
    echo "icf_step3b:     job $JID_ICF_S3B  (4c / 16G / 8h)  dep=$JID_ICF_S3A"
  fi

  if stage_done "$ICF_DIR/output/variable_importance.pdf"; then
    echo "icf_viz:        already complete"
  else
    JID_ICF_VIZ=$(sbatch --parsable \
      --job-name="icf_viz" \
      $(dep_of "$JID_ICF_S3B") \
      -c 1 --mem=4G --time=0:30:00 \
      --output="$LOGS/icf_06_viz_%j.out" \
      --error="$LOGS/icf_06_viz_%j.out" \
      --wrap="$(make_wrap "Rscript $ICF_DIR/03_visualize_results.R")")
    echo "icf_viz:        job $JID_ICF_VIZ  (1c / 4G / 0.5h)  dep=$JID_ICF_S3B"
  fi

  if stage_done "$ICF_DIR/output/icf_values.tex"; then
    echo "icf_latex:      already complete"
  else
    JID_ICF_LATEX=$(sbatch --parsable \
      --job-name="icf_latex" \
      $(dep_of "$JID_ICF_S3B") \
      -c 1 --mem=4G --time=0:30:00 \
      --output="$LOGS/icf_07_latex_%j.out" \
      --error="$LOGS/icf_07_latex_%j.out" \
      --wrap="$(make_wrap "Rscript $ICF_DIR/04_export_latex.R")")
    echo "icf_latex:      job $JID_ICF_LATEX  (1c / 4G / 0.5h)  dep=$JID_ICF_S3B"
  fi

  if stage_done "$ICF_DIR/output/validation_values.tex"; then
    echo "icf_validate:   already complete"
  else
    JID_ICF_VAL=$(sbatch --parsable \
      --job-name="icf_validate" \
      $(dep_of "$JID_ICF_S3B") \
      -c 1 --mem=8G --time=1:00:00 \
      --output="$LOGS/icf_08_validate_%j.out" \
      --error="$LOGS/icf_08_validate_%j.out" \
      --wrap="$(make_wrap "Rscript $ICF_DIR/04_validate_subgroups.R")")
    echo "icf_validate:   job $JID_ICF_VAL  (1c / 8G / 1h)  dep=$JID_ICF_S3B"
  fi

  # CATE-method comparison + calibration (parallel branch, depends on icf_prep,
  # not on step3b). Produces calibration_plot.pdf, cate_vi_comparison.pdf,
  # cate_distribution_comparison.pdf — all referenced by the thesis Makefile.
  JID_ICF_CATE=""
  JID_ICF_CVIZ=""
  JID_ICF_CALIB=""
  if stage_done "$ICF_DIR/output/cate_comparison.rds"; then
    echo "icf_cate:       already complete"
  else
    JID_ICF_CATE=$(sbatch --parsable \
      --job-name="icf_cate" \
      $(dep_of "$JID_ICF_PREP") \
      -c 4 --mem=32G --time=4:00:00 \
      --output="$LOGS/icf_09_cate_%j.out" \
      --error="$LOGS/icf_09_cate_%j.out" \
      --wrap="$(make_wrap "Rscript $ICF_DIR/05_cate_comparison.R")")
    echo "icf_cate:       job $JID_ICF_CATE  (4c / 32G / 4h)  dep=$JID_ICF_PREP"
  fi

  if stage_done "$ICF_DIR/output/cate_vi_comparison.pdf"; then
    echo "icf_cate_viz:   already complete"
  else
    JID_ICF_CVIZ=$(sbatch --parsable \
      --job-name="icf_cate_viz" \
      $(dep_of "$JID_ICF_CATE") \
      -c 1 --mem=4G --time=0:30:00 \
      --output="$LOGS/icf_10_cate_viz_%j.out" \
      --error="$LOGS/icf_10_cate_viz_%j.out" \
      --wrap="$(make_wrap "Rscript $ICF_DIR/06_visualize_cate_comparison.R")")
    echo "icf_cate_viz:   job $JID_ICF_CVIZ  (1c / 4G / 0.5h)  dep=$JID_ICF_CATE"
  fi

  if stage_done "$ICF_DIR/output/calibration_plot.pdf"; then
    echo "icf_calibration: already complete"
  else
    JID_ICF_CALIB=$(sbatch --parsable \
      --job-name="icf_calibration" \
      $(dep_of "$JID_ICF_CATE") \
      -c 1 --mem=4G --time=0:30:00 \
      --output="$LOGS/icf_11_calibration_%j.out" \
      --error="$LOGS/icf_11_calibration_%j.out" \
      --wrap="$(make_wrap "Rscript $ICF_DIR/07_calibration.R")")
    echo "icf_calibration: job $JID_ICF_CALIB  (1c / 4G / 0.5h)  dep=$JID_ICF_CATE"
  fi

  ICF_DEPS+=("$JID_ICF_VIZ" "$JID_ICF_LATEX" "$JID_ICF_VAL" "$JID_ICF_CVIZ" "$JID_ICF_CALIB")

  # Missing-indicator sensitivity triage (iCF). Runs prep + step1 under
  # ICF_VARIANT=missind on the FULL eligible cohort (no complete-case drop).
  # Used by 04_export_missind_triage.R to produce missind_triage_values.tex.
  JID_ICF_MISS_PREP=""
  JID_ICF_MISS_S1=""
  if stage_done "$ICF_DIR/data/icf_data_missind.rds"; then
    echo "icf_prep_missind:    already complete"
  else
    JID_ICF_MISS_PREP=$(sbatch --parsable \
      --job-name="icf_prep_missind" \
      $(combine_deps "$JID_BOOT" "$JID_EXTRACT") \
      -c 1 --mem=4G --time=1:00:00 \
      --output="$LOGS/icf_12_prep_missind_%j.out" \
      --error="$LOGS/icf_12_prep_missind_%j.out" \
      --wrap="$(make_wrap "export ICF_VARIANT=missind; Rscript $ICF_DIR/01_prepare_data.R")")
    echo "icf_prep_missind:    job $JID_ICF_MISS_PREP  (1c / 4G / 1h)"
  fi
  if stage_done "$ICF_DIR/output/icf_step1_missind.rds"; then
    echo "icf_step1_missind:   already complete"
  else
    JID_ICF_MISS_S1=$(sbatch --parsable \
      --job-name="icf_step1_missind" \
      $(dep_of "$JID_ICF_MISS_PREP") \
      -c 4 --mem=32G --time=12:00:00 \
      --output="$LOGS/icf_13_step1_missind_%j.out" \
      --error="$LOGS/icf_13_step1_missind_%j.out" \
      --wrap="$(make_wrap "export ICF_VARIANT=missind; Rscript $ICF_DIR/02a_icf_step1.R")")
    echo "icf_step1_missind:   job $JID_ICF_MISS_S1  (4c / 32G / 12h)  dep=$JID_ICF_MISS_PREP"
  fi
fi

# ---------- 4. hdiCF pipeline (analysis-hdicf/) ----------
JID_HD_GEN=""
JID_HD_PREP=""
JID_HD_S1=""
JID_HD_S2=""
JID_HD_S3A=""
JID_HD_S3B=""
JID_HD_VIZ=""
JID_HD_LATEX=""
HDICF_DEPS=()
if [[ "$SKIP_HDICF" -eq 1 ]]; then
  echo "hdicf:          skipped (--skip-hdicf)"
else
  HD_DIR="suicidality/analysis-hdicf"

  if stage_done "$HD_DIR/data/hd_features.rds"; then
    echo "hdicf_hdgen:    already complete"
  else
    JID_HD_GEN=$(sbatch --parsable \
      --job-name="hdicf_hdgen" \
      $(combine_deps "$JID_BOOT" "$JID_EXTRACT") \
      -c 1 --mem=8G --time=1:00:00 \
      --output="$LOGS/hdicf_01_hdgen_%j.out" \
      --error="$LOGS/hdicf_01_hdgen_%j.out" \
      --wrap="$(make_wrap "Rscript $HD_DIR/01_generate_hd_features.R")")
    echo "hdicf_hdgen:    job $JID_HD_GEN  (1c / 8G / 1h)"
  fi

  if stage_done "$HD_DIR/data/icf_data.rds"; then
    echo "hdicf_prep:     already complete"
  else
    JID_HD_PREP=$(sbatch --parsable \
      --job-name="hdicf_prep" \
      $(dep_of "$JID_HD_GEN") \
      -c 4 --mem=16G --time=2:00:00 \
      --output="$LOGS/hdicf_02_prep_%j.out" \
      --error="$LOGS/hdicf_02_prep_%j.out" \
      --wrap="$(make_wrap "Rscript $HD_DIR/02_prepare_data.R")")
    echo "hdicf_prep:     job $JID_HD_PREP  (4c / 16G / 2h)  dep=$JID_HD_GEN"
  fi

  # Same upstream-cleanup pattern as iCF: hdiCF step3b deletes its upstream
  # intermediates after merging.
  hd_terminal_done=0
  [[ "$FORCE" -eq 0 && -f "$REPO/$HD_DIR/output/icf_results.rds" ]] && hd_terminal_done=1

  if [[ "$hd_terminal_done" -eq 1 ]] || stage_done "$HD_DIR/output/icf_step1.rds"; then
    echo "hdicf_step1:    already complete"
  else
    JID_HD_S1=$(sbatch --parsable \
      --job-name="hdicf_step1" \
      $(dep_of "$JID_HD_PREP") \
      -c 4 --mem=32G --time=16:00:00 \
      --output="$LOGS/hdicf_03_step1_%j.out" \
      --error="$LOGS/hdicf_03_step1_%j.out" \
      --wrap="$(make_wrap "Rscript $HD_DIR/03a_icf_step1.R")")
    echo "hdicf_step1:    job $JID_HD_S1  (4c / 32G / 16h)  dep=$JID_HD_PREP"
  fi

  hd_step2_done=1
  for f in 1 2 3 4 5; do
    for d in 2 3 4 5; do
      [[ -f "$REPO/$HD_DIR/output/icf_cv_fold${f}_depth${d}.rds" ]] || hd_step2_done=0
    done
  done
  if [[ "$hd_terminal_done" -eq 1 ]] || [[ "$FORCE" -eq 0 && "$hd_step2_done" -eq 1 ]]; then
    echo "hdicf_step2:    already complete"
  else
    JID_HD_S2=$(sbatch --parsable \
      --job-name="hdicf_step2" \
      --array=1-20 \
      $(dep_of "$JID_HD_S1") \
      -c 2 --mem=4G --time=4:00:00 \
      --output="$LOGS/hdicf_04_step2_%A_%a.out" \
      --error="$LOGS/hdicf_04_step2_%A_%a.out" \
      --wrap="$(make_wrap '
FOLD=$(( (SLURM_ARRAY_TASK_ID - 1) % 5 + 1 ))
DEPTH=$(( (SLURM_ARRAY_TASK_ID - 1) / 5 + 2 ))
Rscript suicidality/analysis-hdicf/03b_icf_step2_fold.R "$FOLD" "$DEPTH"')")
    echo "hdicf_step2:    array $JID_HD_S2 [1-20]  (2c / 4G / 4h)  dep=$JID_HD_S1"
  fi

  hd_step3a_done=1
  for b in 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20; do
    [[ -f "$REPO/$HD_DIR/output/icf_batch_${b}.rds" ]] || hd_step3a_done=0
  done
  if [[ "$hd_terminal_done" -eq 1 ]] || [[ "$FORCE" -eq 0 && "$hd_step3a_done" -eq 1 ]]; then
    echo "hdicf_step3a:   already complete"
  else
    JID_HD_S3A=$(sbatch --parsable \
      --job-name="hdicf_step3a" \
      --array=1-20 \
      $(dep_of "$JID_HD_S2") \
      -c 2 --mem=4G --time=4:00:00 \
      --output="$LOGS/hdicf_05_step3a_%A_%a.out" \
      --error="$LOGS/hdicf_05_step3a_%A_%a.out" \
      --wrap="$(make_wrap '
Rscript suicidality/analysis-hdicf/03c1_icf_step3_batch.R "$SLURM_ARRAY_TASK_ID" 20')")
    echo "hdicf_step3a:   array $JID_HD_S3A [1-20]  (2c / 4G / 4h)  dep=$JID_HD_S2"
  fi

  if stage_done "$HD_DIR/output/icf_results.rds"; then
    echo "hdicf_step3b:   already complete"
  else
    JID_HD_S3B=$(sbatch --parsable \
      --job-name="hdicf_step3b" \
      $(dep_of "$JID_HD_S3A") \
      -c 4 --mem=16G --time=8:00:00 \
      --output="$LOGS/hdicf_06_step3b_%j.out" \
      --error="$LOGS/hdicf_06_step3b_%j.out" \
      --wrap="$(make_wrap "Rscript $HD_DIR/03c2_icf_step3_merge.R")")
    echo "hdicf_step3b:   job $JID_HD_S3B  (4c / 16G / 8h)  dep=$JID_HD_S3A"
  fi

  if stage_done "$HD_DIR/output/variable_importance.pdf"; then
    echo "hdicf_viz:      already complete"
  else
    JID_HD_VIZ=$(sbatch --parsable \
      --job-name="hdicf_viz" \
      $(dep_of "$JID_HD_S3B") \
      -c 1 --mem=4G --time=0:30:00 \
      --output="$LOGS/hdicf_07_viz_%j.out" \
      --error="$LOGS/hdicf_07_viz_%j.out" \
      --wrap="$(make_wrap "Rscript $HD_DIR/04_visualize_results.R")")
    echo "hdicf_viz:      job $JID_HD_VIZ  (1c / 4G / 0.5h)  dep=$JID_HD_S3B"
  fi

  if stage_done "$HD_DIR/output/hdicf_values.tex"; then
    echo "hdicf_latex:    already complete"
  else
    JID_HD_LATEX=$(sbatch --parsable \
      --job-name="hdicf_latex" \
      $(dep_of "$JID_HD_S3B") \
      -c 1 --mem=4G --time=0:30:00 \
      --output="$LOGS/hdicf_08_latex_%j.out" \
      --error="$LOGS/hdicf_08_latex_%j.out" \
      --wrap="$(make_wrap "
Rscript $HD_DIR/05_export_latex.R
Rscript $HD_DIR/06_export_hd_features_table.R")")
    echo "hdicf_latex:    job $JID_HD_LATEX  (1c / 4G / 0.5h)  dep=$JID_HD_S3B"
  fi

  HDICF_DEPS+=("$JID_HD_VIZ" "$JID_HD_LATEX")

  # Missing-indicator sensitivity triage (hdiCF): hdgen + prep + step1 under
  # ICF_VARIANT=missind. Outputs hdicf icf_step1_missind.rds.
  JID_HD_MISS_GEN=""
  JID_HD_MISS_PREP=""
  JID_HD_MISS_S1=""
  if stage_done "$HD_DIR/data/hd_features_missind.rds"; then
    echo "hdicf_hdgen_missind: already complete"
  else
    JID_HD_MISS_GEN=$(sbatch --parsable \
      --job-name="hdicf_hdgen_missind" \
      $(combine_deps "$JID_BOOT" "$JID_EXTRACT") \
      -c 1 --mem=8G --time=1:00:00 \
      --output="$LOGS/hdicf_09_hdgen_missind_%j.out" \
      --error="$LOGS/hdicf_09_hdgen_missind_%j.out" \
      --wrap="$(make_wrap "export ICF_VARIANT=missind; Rscript $HD_DIR/01_generate_hd_features.R")")
    echo "hdicf_hdgen_missind: job $JID_HD_MISS_GEN  (1c / 8G / 1h)"
  fi
  if stage_done "$HD_DIR/data/icf_data_missind.rds"; then
    echo "hdicf_prep_missind:  already complete"
  else
    JID_HD_MISS_PREP=$(sbatch --parsable \
      --job-name="hdicf_prep_missind" \
      $(dep_of "$JID_HD_MISS_GEN") \
      -c 4 --mem=16G --time=2:00:00 \
      --output="$LOGS/hdicf_10_prep_missind_%j.out" \
      --error="$LOGS/hdicf_10_prep_missind_%j.out" \
      --wrap="$(make_wrap "export ICF_VARIANT=missind; Rscript $HD_DIR/02_prepare_data.R")")
    echo "hdicf_prep_missind:  job $JID_HD_MISS_PREP  (4c / 16G / 2h)  dep=$JID_HD_MISS_GEN"
  fi
  if stage_done "$HD_DIR/output/icf_step1_missind.rds"; then
    echo "hdicf_step1_missind: already complete"
  else
    JID_HD_MISS_S1=$(sbatch --parsable \
      --job-name="hdicf_step1_missind" \
      $(dep_of "$JID_HD_MISS_PREP") \
      -c 4 --mem=32G --time=16:00:00 \
      --output="$LOGS/hdicf_11_step1_missind_%j.out" \
      --error="$LOGS/hdicf_11_step1_missind_%j.out" \
      --wrap="$(make_wrap "export ICF_VARIANT=missind; Rscript $HD_DIR/03a_icf_step1.R")")
    echo "hdicf_step1_missind: job $JID_HD_MISS_S1  (4c / 32G / 16h)  dep=$JID_HD_MISS_PREP"
  fi
fi

# ---------- 4b. Missing-indicator triage exporter ----------
# Produces missind_triage_values.tex from the two step1_missind.rds files.
JID_MISS_TRIAGE=""
if [[ "$SKIP_ICF" -eq 0 && "$SKIP_HDICF" -eq 0 ]]; then
  if stage_done "suicidality/analysis-icf/output/missind_triage_values.tex"; then
    echo "missind_triage: already complete"
  else
    JID_MISS_TRIAGE=$(sbatch --parsable \
      --job-name="missind_triage" \
      $(combine_deps "$JID_ICF_MISS_S1" "$JID_HD_MISS_S1") \
      -c 1 --mem=4G --time=0:30:00 \
      --output="$LOGS/missind_triage_%j.out" \
      --error="$LOGS/missind_triage_%j.out" \
      --wrap="$(make_wrap "Rscript suicidality/analysis-icf/04_export_missind_triage.R")")
    echo "missind_triage:      job $JID_MISS_TRIAGE  (1c / 4G / 0.5h)  dep=$JID_ICF_MISS_S1,$JID_HD_MISS_S1"
  fi
fi

# ---------- 5. Qini plots (need both iCF and hdiCF results) ----------
JID_QINI=""
JID_QINI_HD=""
QINI_DEPS=()
if [[ "$SKIP_ICF" -eq 0 ]]; then
  if stage_done "suicidality/analysis/output/qini_12wks.pdf"; then
    echo "qini_icf:       already complete"
  else
    JID_QINI=$(sbatch --parsable \
      --job-name="qini_icf" \
      $(dep_of "$JID_ICF_S3B") \
      -c 1 --mem=4G --time=0:30:00 \
      --output="$LOGS/qini_icf_%j.out" \
      --error="$LOGS/qini_icf_%j.out" \
      --wrap="$(make_wrap 'Rscript suicidality/analysis/plot_qini.R')")
    echo "qini_icf:       job $JID_QINI  (1c / 4G / 0.5h)  dep=$JID_ICF_S3B"
  fi
  QINI_DEPS+=("$JID_QINI")
fi
if [[ "$SKIP_HDICF" -eq 0 ]]; then
  if stage_done "suicidality/analysis/output/qini_12wks_hdicf.pdf"; then
    echo "qini_hdicf:     already complete"
  else
    JID_QINI_HD=$(sbatch --parsable \
      --job-name="qini_hdicf" \
      $(dep_of "$JID_HD_S3B") \
      -c 1 --mem=4G --time=0:30:00 \
      --output="$LOGS/qini_hdicf_%j.out" \
      --error="$LOGS/qini_hdicf_%j.out" \
      --wrap="$(make_wrap 'Rscript suicidality/analysis/plot_qini_hdicf.R')")
    echo "qini_hdicf:     job $JID_QINI_HD  (1c / 4G / 0.5h)  dep=$JID_HD_S3B"
  fi
  QINI_DEPS+=("$JID_QINI_HD")
fi

# ---------- 6. Thesis build ----------
JID_THESIS=""
if [[ "$SKIP_THESIS" -eq 1 ]]; then
  echo "thesis:         skipped (--skip-thesis)"
elif stage_done "suicidality/report/thesis.pdf"; then
  echo "thesis_build:   already complete (thesis.pdf present)"
else
  # Use the ${arr[@]-} idiom so empty arrays don't trip `set -u`.
  DEP_STR="$(combine_deps "$JID_ANALYSIS" "${ICF_DEPS[@]-}" "${HDICF_DEPS[@]-}" "${QINI_DEPS[@]-}" "$JID_MISS_TRIAGE")"

  # Write a config.mk that points the thesis Makefile at the LIVE output dirs
  # in this job's repo copy (not the 2026-05-22 backup snapshots).
  THESIS_CMD='
cat > suicidality/report/config.mk <<MK
ANALYSIS_DIR = ../analysis
ANALYSIS_OUTPUT_DIR = ../analysis/output
ICF_DIR = ../analysis-icf/output
HDICF_DIR = ../analysis-hdicf/output
DOCUMENTS_DIR = ../Documents
ICF_RERUN_DIR = ../analysis-icf/output
HDICF_RERUN_DIR = ../analysis-hdicf/output
MK
cd suicidality/report
module load texlive || true
make clean
make
echo "Thesis pdf at: $(pwd)/thesis.pdf"
'
  JID_THESIS=$(sbatch --parsable \
    --job-name="thesis_build" \
    $DEP_STR \
    -c 1 --mem=4G --time=1:00:00 \
    --output="$LOGS/99_thesis_%j.out" \
    --error="$LOGS/99_thesis_%j.out" \
    --wrap="$(make_wrap "$THESIS_CMD")")
  echo "thesis_build:   job $JID_THESIS  (1c / 4G / 1h)  ${DEP_STR:-(no deps)}"
fi

# ---------- Summary ----------
# A blank JID means the stage either was --skip-xxx'd or was already done on
# disk (resume case). The manifest writes "skip" for both, with the run log
# above distinguishing the two reasons.
{
  echo "job_dir: $BASE"
  date "+submitted_at: %Y-%m-%dT%H:%M:%S%z"
  echo "r_bootstrap:    ${JID_BOOT:-skip}"
  echo "extract:        ${JID_EXTRACT:-skip}"
  echo "analysis_main:  ${JID_ANALYSIS:-skip}"
  echo "icf_prep:       ${JID_ICF_PREP:-skip}"
  echo "icf_step1:      ${JID_ICF_S1:-skip}"
  echo "icf_step2:      ${JID_ICF_S2:-skip}"
  echo "icf_step3a:     ${JID_ICF_S3A:-skip}"
  echo "icf_step3b:     ${JID_ICF_S3B:-skip}"
  echo "icf_viz:        ${JID_ICF_VIZ:-skip}"
  echo "icf_latex:      ${JID_ICF_LATEX:-skip}"
  echo "icf_validate:   ${JID_ICF_VAL:-skip}"
  echo "icf_cate:       ${JID_ICF_CATE:-skip}"
  echo "icf_cate_viz:   ${JID_ICF_CVIZ:-skip}"
  echo "icf_calibration: ${JID_ICF_CALIB:-skip}"
  echo "icf_prep_missind:    ${JID_ICF_MISS_PREP:-skip}"
  echo "icf_step1_missind:   ${JID_ICF_MISS_S1:-skip}"
  echo "hdicf_hdgen_missind: ${JID_HD_MISS_GEN:-skip}"
  echo "hdicf_prep_missind:  ${JID_HD_MISS_PREP:-skip}"
  echo "hdicf_step1_missind: ${JID_HD_MISS_S1:-skip}"
  echo "missind_triage:      ${JID_MISS_TRIAGE:-skip}"
  echo "hdicf_hdgen:    ${JID_HD_GEN:-skip}"
  echo "hdicf_prep:     ${JID_HD_PREP:-skip}"
  echo "hdicf_step1:    ${JID_HD_S1:-skip}"
  echo "hdicf_step2:    ${JID_HD_S2:-skip}"
  echo "hdicf_step3a:   ${JID_HD_S3A:-skip}"
  echo "hdicf_step3b:   ${JID_HD_S3B:-skip}"
  echo "hdicf_viz:      ${JID_HD_VIZ:-skip}"
  echo "hdicf_latex:    ${JID_HD_LATEX:-skip}"
  echo "qini_icf:       ${JID_QINI:-skip}"
  echo "qini_hdicf:     ${JID_QINI_HD:-skip}"
  echo "thesis_build:   ${JID_THESIS:-skip}"
} >> "$BASE/job_chain.txt"

echo ""
echo "Job chain written to $BASE/job_chain.txt"
squeue -u "$USER" --format="%.10i %.9P %.20j %.2t %.10M %.5C %.8m %.12l" 2>/dev/null || true
REMOTE
}

if [[ "$DRY_RUN" -eq 1 ]]; then
  echo "(dry run; SLURM submission skipped)"
  exit 0
fi

submit_remote

echo ""
echo "=============================================================="
echo " Submitted. To monitor:"
echo "   ssh $HOST 'squeue -u \$USER'"
echo "   ssh $HOST 'tail -f $REMOTE_LOGS/*.out'"
echo " Job manifest:"
echo "   ssh $HOST 'cat $REMOTE_BASE/job_chain.txt'"
echo " When the chain finishes, the thesis pdf is at:"
echo "   $HOST:$REMOTE_REPO/suicidality/report/thesis.pdf"
echo "=============================================================="

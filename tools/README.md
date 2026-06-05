# tools

Utility scripts for syncing and running jobs on the tensor HPC cluster.

## Sync Scripts

| Script | Purpose |
|--------|---------|
| `sync-repo-to-tensor.sh` | Sync repo to tensor (excludes `.git/`, `*.rds`, `*.sas7bdat`, `*.RData`) |
| `sync-rds-to-tensor.sh` | Sync RDS extraction output to tensor |

## Cluster Scripts

| Script | Purpose |
|--------|---------|
| `submit.sh` | Submit an R script as a SLURM job |
| `run_r.sbatch` | SLURM batch template (4 cores, 32GB RAM, 2hr limit) |
| `tail-job.sh` | Monitor running job output in real-time |
| `download_s2.sh` | Download RDS results from cluster |
| `cleanup.sh` | Kill hanging R processes and database connections |

## Workflow

### 1. Submit a job

```bash
# Extraction script (shorthand)
./tools/submit.sh 01_raw_diagnoses_index.R

# Any script (full path)
./tools/submit.sh suicidality/analysis-icf/01_prepare_data.R
```

This:
- Syncs the repo to tensor
- Creates a timestamped job directory (`~/jobs/job_YYYYMMDD_HHMMSS/`)
- Submits the SLURM job

### 2. Monitor the job

```bash
./tools/tail-job.sh
```

Streams the job output until completion, then shows final status.

### 3. Download results

```bash
./tools/download_s2.sh
```

Downloads RDS files from tensor to local `suicidality/extraction/output/rds/`.

## Other Scripts

| Script | Purpose |
|--------|---------|
| `run_on_tensor.sh` | Run a single R script on tensor (standalone, no cluster scripts) |
| `db_conn_test.R` | Test database connection |

## Configuration

The scripts assume:
- Remote host alias: `tensor` (configured in `~/.ssh/config`)
- Remote repo path: `~/work/ssri-suicidality/`
- Required modules: `R/4.5.1`, `GCCcore/13.2.0`, `unixODBC`, `mebauth`
- SLURM resources: 4 cores, 32GB RAM, 48hr time limit

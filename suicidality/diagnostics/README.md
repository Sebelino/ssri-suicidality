# Diagnostics

Stand-alone R scripts that probe pipeline behaviour without being part of the
production pipeline. Each writes its own `.rds` result alongside the script
(gitignored).

Run from the repo root with `Rscript suicidality/diagnostics/<name>.R`.

## Available diagnostics

### `diag_raw_cf_reproducibility.R`

Fits the raw causal forest twice on the current `analysis-icf/data/icf_data.rds`
and reports VI agreement (Spearman rank correlation, top-N Jaccard,
above-mean selection-set overlap). Used to confirm that the production pipeline
produces byte-identical VI run-to-run. Wall time ~6 min on this cohort.

The historical motivation was the May 12 finding that two tensor iCF runs
produced visually different VI plots; the diagnostic isolated the cause to
`tune.parameters = "all"` rather than seed variation, and was the basis for
the reproducibility fix (commit `cf2c8b5`).

### `diag_raw_cf_seed_variability.R`

Fits the raw causal forest with 5 different seeds and reports pairwise
Spearman rank correlation, top-N Jaccard, the above-mean selection set per
seed, and the tuned hyperparameters chosen by `grf` for each seed. Quantifies
how much VI moves under seed variation alone. Wall time ~13 min on this
cohort.

This is the diagnostic that produced the headline finding documented in §2.6
of the thesis: under `tune.parameters = "all"`, sample.fraction varied
0.06–0.46 and min.node.size varied 38–561 across 5 seeds, producing above-mean
selection sets of 4–14 variables. Re-run with the production pipeline
(`tune.parameters = "none"`) to confirm the variability is now bounded.

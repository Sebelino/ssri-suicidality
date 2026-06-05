# paths.R
# Variant-aware path resolution for the iCF / hdiCF pipelines.
#
# Reads the env var ICF_VARIANT:
#   ""        -> headline complete-case pipeline (default, original behaviour)
#   "missind" -> missing-indicator sensitivity (full eligible cohort, single
#                `any_miss` covariate, all four CCA vars forced to ref level
#                when any_miss == 1; see common.R::apply_missind_recoding).
#
# Files are written with the variant suffix appended to base filenames so
# the headline and sensitivity artefacts coexist without clobbering each other.
# Output figures and plots stay in the same `output/` directory; only RDS
# bundles, latex macros, and the iCF step1 result get suffixed.

# Internal: read the variant once. Anything other than "missind" or "" is an
# error -- typo protection so that running with ICF_VARIANT=Missind doesn't
# silently produce a CCA result.
.icf_variant <- function() {
  v <- Sys.getenv("ICF_VARIANT", unset = "")
  if (!(v %in% c("", "missind"))) {
    stop(sprintf("Unknown ICF_VARIANT='%s'. Allowed: '' (default) or 'missind'.", v))
  }
  v
}

# Suffix appended to filenames in the current variant. Empty for headline.
variant_suffix <- function() {
  v <- .icf_variant()
  if (v == "") "" else paste0("_", v)
}

# Convenience: pipeline name for log lines. Returns "headline" or "missind".
variant_label <- function() {
  v <- .icf_variant()
  if (v == "") "headline" else v
}

# Resolve the per-variant prepared-data path
# (analysis-icf/data/icf_data{suffix}.rds or
#  analysis-hdicf/data/icf_data{suffix}.rds, depending on which dir sourced
#  this file).
icf_data_path <- function(analysis_dir) {
  here::here("suicidality", analysis_dir, "data",
             paste0("icf_data", variant_suffix(), ".rds"))
}

# Resolve the per-variant step1 output path
# (analysis-icf/output/icf_step1{suffix}.rds, etc.).
icf_step1_path <- function(analysis_dir) {
  here::here("suicidality", analysis_dir, "output",
             paste0("icf_step1", variant_suffix(), ".rds"))
}

# Resolve the per-variant HD-features path (hdiCF only).
hd_features_path <- function() {
  here::here("suicidality", "analysis-hdicf", "data",
             paste0("hd_features", variant_suffix(), ".rds"))
}

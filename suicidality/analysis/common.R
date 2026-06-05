# Common utilities for analysis scripts
# Uses here() for path resolution - works from any directory

library(here)

# Directory containing RDS data files
rds_data_dir <- function() {
  here("suicidality", "extraction", "output", "rds")
}

# Read an RDS file from the extraction output directory
read_rds_file <- function(filename) {
  df <- readRDS(file.path(rds_data_dir(), filename))

  # Convert data.table to data.frame for compatibility with analysis scripts
  as.data.frame(df)
}

# Get path for output files in the analysis directory
output_path <- function(filename) {
  here("suicidality", "analysis", filename)
}

# Drop rows with sentinel-99 (missing) in any of the four partially-observed
# covariates: parental education, family income, family history of suicidal
# behavior, family history of depression. Used for the main analysis and the
# subgroup analyses (complete-case policy). Prints the drop count for the run log.
filter_complete_cases <- function(df) {
  miss_vars <- c("edufam_cat", "inc_cat", "fh_suicidal", "fh_depr")
  miss_vars <- intersect(miss_vars, names(df))
  if (length(miss_vars) == 0) return(df)
  miss <- Reduce(`|`, lapply(miss_vars, function(v) df[[v]] == 99))
  n_dropped <- sum(miss, na.rm = TRUE)
  if (n_dropped > 0) {
    cat(sprintf("filter_complete_cases: dropped %d / %d rows (%.2f%%) with sentinel-99 in %s\n",
                n_dropped, nrow(df), 100 * n_dropped / nrow(df),
                paste(miss_vars, collapse = "/")))
  }
  df[!miss, , drop = FALSE]
}

# Missing-indicator recoding: keep all rows (no CCA drop), add a single binary
# `any_miss` indicator (=1 iff any of the four sentinel-99 vars is 99), and
# force ALL four covariates to their modal reference level for those patients
# so the indicator absorbs the entire missing-stratum contribution. Mirrors
# ITT_12wks_missind.R / cate_by_prior_suicidal.R, used by the iCF/hdiCF
# step-1 sensitivity rerun.
apply_missind_recoding <- function(df) {
  df$any_miss <- as.integer(df$edufam_cat == 99 |
                            df$inc_cat    == 99 |
                            df$fh_suicidal == 99 |
                            df$fh_depr    == 99)
  df$edufam_cat  <- ifelse(df$any_miss == 1, 1, df$edufam_cat)
  df$inc_cat     <- ifelse(df$any_miss == 1, 4, df$inc_cat)
  df$fh_suicidal <- ifelse(df$any_miss == 1, 0, df$fh_suicidal)
  df$fh_depr     <- ifelse(df$any_miss == 1, 0, df$fh_depr)
  cat(sprintf("apply_missind_recoding: any_miss == 1 for %d / %d rows (%.2f%%)\n",
              sum(df$any_miss == 1), nrow(df),
              100 * sum(df$any_miss == 1) / nrow(df)))
  df
}

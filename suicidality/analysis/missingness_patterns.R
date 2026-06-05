# missingness_patterns.R
#
# Cross-tabulate the 16 missingness patterns across the four partially-observed
# covariates that drive the complete-case analysis cohort:
#
#   - edufam_cat (parental education)
#   - inc_cat (family income)
#   - fh_suicidal (family history of suicidal behavior)
#   - fh_depr (family history of depression)
#
# Each carries a sentinel-99 value when missing. The script counts how many
# patients fall into each of the 2^4 = 16 patterns (e.g. all four missing;
# only edu missing; none missing; ...).
#
# Output: stdout table + output/missingness_patterns.csv

library(dplyr)
library(here)
here::i_am("suicidality/analysis/missingness_patterns.R")

source(here("suicidality", "analysis", "common.R"))

# Pre-CCA cohort (we want to see the missingness, so do *not* filter)
data <- read_rds_file("main_12wks_28.rds")
n_total <- nrow(data)
cat(sprintf("Pre-CCA cohort N = %d\n\n", n_total))

# Binary missingness indicators (1 = sentinel-99 missing, 0 = observed)
missing_vars <- c("edufam_cat", "inc_cat", "fh_suicidal", "fh_depr")
m <- as.data.frame(lapply(missing_vars, function(v) as.integer(data[[v]] == 99)))
names(m) <- missing_vars

# Cross-tabulate: count individuals per pattern. Patterns with zero
# patients are omitted from the printed table per project policy.
tbl <- m %>%
  group_by(across(all_of(missing_vars))) %>%
  summarise(n = as.integer(n()), .groups = "drop") %>%
  filter(n > 0) %>%
  mutate(pct = round(100 * n / n_total, 2),
         n_missing = edufam_cat + inc_cat + fh_suicidal + fh_depr) %>%
  arrange(n_missing, desc(n))

# Pretty pattern labels: dot for observed, X for missing, in column order
# (edu, inc, fh_suic, fh_depr).
fmt_pattern <- function(e, i, fs, fd) {
  paste0(
    ifelse(e == 1, "X", "."),
    ifelse(i == 1, "X", "."),
    ifelse(fs == 1, "X", "."),
    ifelse(fd == 1, "X", ".")
  )
}
tbl$pattern <- with(tbl,
  fmt_pattern(edufam_cat, inc_cat, fh_suicidal, fh_depr))

# Human-readable description of which variables are missing
describe <- function(e, i, fs, fd) {
  parts <- c()
  if (e == 1)  parts <- c(parts, "edu")
  if (i == 1)  parts <- c(parts, "inc")
  if (fs == 1) parts <- c(parts, "fh_suic")
  if (fd == 1) parts <- c(parts, "fh_depr")
  if (length(parts) == 0) "none missing" else paste(parts, collapse = " + ")
}
tbl$description <- mapply(describe, tbl$edufam_cat, tbl$inc_cat,
                          tbl$fh_suicidal, tbl$fh_depr)

# Print the 16-row table
out <- tbl %>%
  transmute(pattern, n_missing,
            edu = edufam_cat, inc = inc_cat,
            fh_suic = fh_suicidal, fh_depr,
            n, pct, description)

cat("Pattern columns: edu | inc | fh_suic | fh_depr (1 = sentinel-99 missing)\n")
cat("Pattern string: 4-char code (. = observed, X = missing) in same order\n\n")
print(out, row.names = FALSE)

# Roll-up by number of missing variables (omit any 'n_missing' level with
# zero patients, consistent with the main table)
rollup <- tbl %>%
  group_by(n_missing) %>%
  summarise(n_patients = sum(n),
            n_patterns = n(),
            pct = round(100 * sum(n) / n_total, 2),
            .groups = "drop") %>%
  filter(n_patients > 0) %>%
  arrange(n_missing)
cat("\n--- Roll-up by number of missing variables ---\n")
print(rollup, row.names = FALSE)

# Sanity check: rows should sum to n_total
stopifnot(sum(tbl$n) == n_total)

# Save CSV
output_dir <- here("suicidality", "analysis", "output")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
out_csv <- file.path(output_dir, "missingness_patterns.csv")
write.csv(out, out_csv, row.names = FALSE)
cat(sprintf("\nSaved: %s\n", out_csv))

# ---- LaTeX table body for the thesis (Supplementary Tables) ----
# Emits row-only tex; wrapper begin{tabular}/{table} lives in thesis.tex.
fmt_indicator <- function(missing) ifelse(missing == 1, "$\\times$", "$\\checkmark$")
fmt_n <- function(n) formatC(n, big.mark = ",", format = "d")

human_label <- function(e, i, fs, fd) {
  if (e + i + fs + fd == 0) return("None missing")
  if (e + i + fs + fd == 4) return("All four missing")
  parts <- c(
    if (e == 1)  "Parental education",
    if (i == 1)  "family income",
    if (fs == 1) "FH suicidal behavior",
    if (fd == 1) "FH depression"
  )
  # First word capitalised: lower-case the rest. Anchored at first part.
  parts[1] <- paste0(toupper(substr(parts[1], 1, 1)),
                     substr(parts[1], 2, nchar(parts[1])))
  if (length(parts) > 1) {
    parts[-1] <- tolower(parts[-1])
  }
  paste0(paste(parts, collapse = " + "), " missing")
}
tex_rows <- tbl %>%
  arrange(n_missing, desc(n)) %>%
  rowwise() %>%
  mutate(
    label = human_label(edufam_cat, inc_cat, fh_suicidal, fh_depr),
    pe = fmt_indicator(edufam_cat),
    fi = fmt_indicator(inc_cat),
    fhs = fmt_indicator(fh_suicidal),
    fhd = fmt_indicator(fh_depr),
    n_str = fmt_n(n),
    pct_str = sprintf("%.2f", pct)
  ) %>%
  ungroup() %>%
  select(label, pe, fi, fhs, fhd, n_str, pct_str)

# No leading comments: the .tex is \input{} inside a tabular environment, and
# stray comment-only lines before the first \\ row trip up LaTeX's tabular
# parser ('Misplaced \noalign' at \bottomrule).
tex_lines <- character(0)
for (i in seq_len(nrow(tex_rows))) {
  r <- tex_rows[i, ]
  tex_lines <- c(tex_lines,
    sprintf("%s & %s & %s & %s & %s & %s & %s \\\\",
            r$label, r$pe, r$fi, r$fhs, r$fhd, r$n_str, r$pct_str))
}
# Booktabs \bottomrule lives inside the input file (matches the project
# convention for icf_subgroups_table etc.) so the surrounding tabular
# environment in thesis.tex doesn't trip the 'Misplaced \noalign'
# parse hiccup that arises when \bottomrule sits on a line after \input{}.
tex_lines <- c(tex_lines, "\\bottomrule")

out_tex <- file.path(output_dir, "missingness_patterns.tex")
writeLines(tex_lines, out_tex)
cat(sprintf("Saved: %s\n", out_tex))

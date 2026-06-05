# Macros.R
# SAS-macro translations as R functions (data.table-based)

suppressPackageStartupMessages({
  library(data.table)
  library(lubridate)
})

# ---- helpers ---------------------------------------------------------------

# Convert to data.table with copy semantics
# - For data.frame: as.data.table() already creates a new object, no copy needed
# - For data.table: copy() is needed to avoid modifying the original
as_dt_copy <- function(x) {
  if (is.data.table(x)) copy(x) else as.data.table(x)
}

as_date_yyyymmdd <- function(x) {
  # x can be character "YYYYMMDD" or numeric; returns Date
  if (inherits(x, "Date")) return(x)
  x <- as.character(x)
  x[x %in% c("", "NA")] <- NA_character_
  as.IDate(x, format = "%Y%m%d")
}

as_date_yymmdd <- function(x) {
  # yymmdd8 as used in SAS here is actually "YYYYMMDD"
  as_date_yyyymmdd(x)
}

min_date_na <- function(...) {
  # rowwise min across Dates with NA handling:
  # - if all missing -> NA
  xs <- list(...)
  # convert to IDate to keep data.table-friendly behavior
  xs <- lapply(xs, function(z) as.IDate(z))
  out <- do.call(pmin, c(xs, list(na.rm = TRUE)))
  # pmin(..., na.rm=TRUE) returns Inf if all NA in some cases -> fix
  out[is.infinite(out)] <- NA
  out
}

age_years_intck_C <- function(bdate, diagn_date) {
  # SAS: intck('YEAR', bdate, diagn_date, 'C')
  # Best match: whole years elapsed (birthday-based)
  as.integer(interval(as.Date(bdate), as.Date(diagn_date)) %/% years(1))
}

deterministic_runif <- function(id_vec) {
  # deterministic "random" in [0,1) from integer-ish ids (lopnr)
  # avoids relying on global RNG order; stable across runs
  x <- as.double(id_vec)
  frac <- abs(sin(x * 12.9898) * 43758.5453)
  frac - floor(frac)
}

# ---- eligible_diagn --------------------------------------------------------
# SAS macro: %eligible_diagn(base=, prescr=, washout=, outfile=)
# Here: returns eligible diagnoses (one row per diagnosis, not automatically first per person)
eligible_diagn <- function(base_dt, prescr_dt, washout = 365L) {
  base_dt   <- as_dt_copy(base_dt)
  prescr_dt <- as_dt_copy(prescr_dt)

  stopifnot(all(c("lopnr", "diagn_date") %in% names(base_dt)))
  stopifnot(all(c("lopnr", "prescr") %in% names(prescr_dt)))

  base_dt[,  diagn_date := data.table::as.IDate(diagn_date)]
  prescr_dt[, prescr    := data.table::as.IDate(prescr)]

  # Make a shared rolling-join key name that exists in BOTH tables
  base2  <- base_dt[, .(lopnr, bdate, dia, diagn_date)]
  base2[, join_date := diagn_date]

  prescr2 <- prescr_dt[, .(lopnr, join_date = prescr)]

  data.table::setkey(prescr2, lopnr, join_date)

  # Rolling join: last prescription on/before join_date, and DO NOT roll forward past first rx
  tmp <- prescr2[
    base2,
    on       = .(lopnr, join_date),
    roll     = TRUE,
    rollends = c(FALSE, TRUE)
  ]

  # tmp$join_date is the matched prior prescription date (or NA if none)
  tmp[, prior_gap := as.integer(diagn_date - join_date)]
  tmp[, keep := is.na(join_date) | prior_gap >= as.integer(washout)]

  tmp[keep == TRUE, .(lopnr, bdate, diagn_date, dia)]
}

# ---- othertreat (120-day gaps) --------------------------------------------
# %othertreat(input=, output=, medname=, addnumber=)
othertreat <- function(input_dt, medname, addnumber = 0L, gap_days = 120L) {
  dt <- as_dt_copy(input_dt)
  stopifnot(all(c("lopnr", "date") %in% names(dt)))
  dt[, mdate := as.IDate(date)]
  setorder(dt, lopnr, mdate)

  # collapse into episodes separated by >gap_days
  dt[, prev_mdate := shift(mdate), by = lopnr]
  dt[, new_episode := is.na(prev_mdate) | (mdate - prev_mdate) > gap_days]
  dt[, episode_id := cumsum(new_episode), by = lopnr]

  ep <- dt[, .(
    start = min(mdate),
    end = max(mdate) + as.integer(addnumber)
  ), by = .(lopnr, episode_id)]

  ep[, (medname) := 1L]
  ep[, episode_id := NULL]
  setcolorder(ep, c("lopnr", "start", "end", medname))
  ep[]
}

# exact SAS variant with 60-day gaps
othertreat_60 <- function(input_dt, medname, addnumber = 0L) {
  othertreat(input_dt, medname = medname, addnumber = addnumber, gap_days = 60L)
}

# ---- addmed ----------------------------------------------------------------
# %addmed(cohortfile=, medfile=, medname=, outfile=)
# Adds medication indicator and SPLITS periods like SAS macro.
# Uses half-open intervals [start, end) for SAS compatibility.
# Preserves all columns from the input cohort.
# SAS splits each cohort period into sub-periods: before/during/after medication.
# Vectorized implementation using data.table by-group operations.
addmed <- function(cohort_dt, med_dt, medname, adjust_periods = FALSE) {
  a <- as_dt_copy(cohort_dt)
  b <- as_dt_copy(med_dt)

  stopifnot(all(c("lopnr", "period_start", "period_end") %in% names(a)))
  stopifnot(all(c("lopnr", "start", "end") %in% names(b)))
  stopifnot(medname %in% names(b))

  a[, `:=`(period_start = as.IDate(period_start), period_end = as.IDate(period_end))]
  b[, `:=`(start = as.IDate(start), end = as.IDate(end))]

  # Add row ID to track original rows
  a[, .row_id := .I]

  # Store original period bounds before any modifications
  a[, orig_period_start := period_start]
  a[, orig_period_end := period_end]

  # Rename medication columns for join
  b2 <- b[, .(lopnr, start2 = start, end2 = end, med2 = 1L)]

  # Cartesian join on lopnr (like SAS SQL join)
  twomed1 <- merge(a, b2, by = "lopnr", all.x = TRUE, allow.cartesian = TRUE)

  # If no medication match, set med2=0
  twomed1[is.na(med2), med2 := 0L]

  # If medication doesn't overlap period (start2 >= period_end OR end2 <= period_start), set med2=0
  # Using half-open interval semantics: [period_start, period_end) overlaps [start2, end2)
  twomed1[!is.na(start2) & (start2 >= orig_period_end | end2 <= orig_period_start), med2 := 0L]

  # Sort by lopnr, period_start, med2, start2 (like SAS proc sort)
  setorder(twomed1, lopnr, orig_period_start, med2, start2)

  # Remove duplicate non-medication rows (keep first per .row_id when med2=0)
  twomed1[, is_first := seq_len(.N) == 1, by = .row_id]
  twomed3 <- twomed1[med2 == 1L | is_first == TRUE]
  twomed3[, is_first := NULL]

  # Sort again by lopnr, period_start, descending med2, start2
  setorder(twomed3, lopnr, orig_period_start, -med2, start2)

  # Remove duplicate non-medication rows again after re-sort
  twomed3[, is_first := seq_len(.N) == 1, by = .row_id]
  twomed4 <- twomed3[med2 == 1L | is_first == TRUE]
  twomed4[, is_first := NULL]

  # Clip medication to period boundaries (for med2=1 rows)
  twomed4[med2 == 1L & start2 < orig_period_start, start2 := orig_period_start]
  twomed4[med2 == 1L & end2 > orig_period_end, end2 := orig_period_end]

  # Sort by .row_id, start2, end2, med2
  setorder(twomed4, .row_id, start2, end2, med2)

  # Get columns to carry forward (excluding temporary/control columns)
  carry_cols <- setdiff(names(a), c("period_start", "period_end", ".row_id", "orig_period_start", "orig_period_end"))

  # Vectorized period splitting using data.table by-group operations
  result <- twomed4[, {
    ps <- orig_period_start[1]
    pe <- orig_period_end[1]

    # Get medication rows
    med_mask <- med2 == 1L
    n_med <- sum(med_mask)

    if (n_med == 0) {
      # No medication overlap - single row with med=0
      list(
        period_start = ps,
        period_end = pe,
        med2 = 0L
      )
    } else {
      # Get medication intervals, sorted by start
      ms <- start2[med_mask]
      me <- end2[med_mask]
      ord <- order(ms)
      ms <- ms[ord]
      me <- me[ord]

      # Merge overlapping medication intervals
      merged_starts <- integer(0)
      merged_ends <- integer(0)
      cur_start <- ms[1]
      cur_end <- me[1]

      if (length(ms) > 1) {
        for (k in 2:length(ms)) {
          if (ms[k] <= cur_end) {
            # Overlapping or adjacent - extend current interval
            cur_end <- max(cur_end, me[k])
          } else {
            # Gap - save current and start new
            merged_starts <- c(merged_starts, cur_start)
            merged_ends <- c(merged_ends, cur_end)
            cur_start <- ms[k]
            cur_end <- me[k]
          }
        }
      }
      merged_starts <- c(merged_starts, cur_start)
      merged_ends <- c(merged_ends, cur_end)

      # Build output segments: before, during, after each medication interval
      out_ps <- integer(0)
      out_pe <- integer(0)
      out_med <- integer(0)

      prev_end <- ps

      for (k in seq_along(merged_starts)) {
        m_start <- merged_starts[k]
        m_end <- merged_ends[k]

        # Gap before medication
        if (m_start > prev_end) {
          out_ps <- c(out_ps, prev_end)
          out_pe <- c(out_pe, m_start)
          out_med <- c(out_med, 0L)
        }

        # Medication period
        actual_start <- max(m_start, prev_end)
        out_ps <- c(out_ps, actual_start)
        out_pe <- c(out_pe, m_end)
        out_med <- c(out_med, 1L)

        prev_end <- m_end
      }

      # Trailing gap after last medication
      if (prev_end < pe) {
        out_ps <- c(out_ps, prev_end)
        out_pe <- c(out_pe, pe)
        out_med <- c(out_med, 0L)
      }

      list(
        period_start = as.IDate(out_ps),
        period_end = as.IDate(out_pe),
        med2 = out_med
      )
    }
  }, by = .row_id]

  # Merge back the carry-forward columns
  carry_data <- unique(a[, c(".row_id", carry_cols), with = FALSE], by = ".row_id")
  result <- merge(result, carry_data, by = ".row_id", all.x = TRUE)

  # Rename med2 to medname
  setnames(result, "med2", medname)

  # Clean up temp columns
  result[, .row_id := NULL]

  setorder(result, lopnr, period_start)
  result[]
}

# ---- checkfu ----------------------------------------------------------------
# %checkfu(infile=)
checkfu <- function(dt) {
  x <- as_dt_copy(dt)
  stopifnot(all(c("lopnr", "period_start", "period_end") %in% names(x)))
  x[, `:=`(period_start = as.IDate(period_start), period_end = as.IDate(period_end))]
  x[, period_time := as.integer(period_end - period_start)]
  if ("outp" %in% names(x)) x[outp == "1", period_time := 0L]

  fu <- x[, .(sum_days = sum(period_time, na.rm = TRUE)), by = lopnr]
  fu[, sum_yrs := sum_days / 365.25]
  list(
    mean_years = mean(fu$sum_yrs, na.rm = TRUE),
    mean_days = mean(fu$sum_days, na.rm = TRUE),
    fu = fu
  )
}

# ---- mean_prescr ------------------------------------------------------------
# %mean_prescr(med=, outp=) operating on a medsx table
mean_prescr <- function(medsx_dt, med_col, max_gap_days = 120L) {
  dt <- as_dt_copy(medsx_dt)
  stopifnot(all(c("lopnr", "date") %in% names(dt)))
  stopifnot(med_col %in% names(dt))

  dt <- dt[get(med_col) == 1L]
  setorder(dt, lopnr, date)
  dt <- unique(dt, by = c("lopnr", "date"))

  # keep only lopnr with >1 prescriptions
  dt[, presc_no := .N, by = lopnr]
  dt <- dt[presc_no > 1L]

  # compute gaps between consecutive prescriptions
  setorder(dt, lopnr, date)
  dt[, nextdate := shift(as.IDate(date), type = "lead"), by = lopnr]
  dt[, time_days := as.integer(nextdate - as.IDate(date))]
  dt <- dt[!is.na(time_days) & time_days <= max_gap_days]

  list(
    mean = mean(dt$time_days, na.rm = TRUE),
    median = median(dt$time_days, na.rm = TRUE),
    sd = sd(dt$time_days, na.rm = TRUE),
    q1 = quantile(dt$time_days, 0.25, na.rm = TRUE),
    q3 = quantile(dt$time_days, 0.75, na.rm = TRUE),
    detail = dt[, .(lopnr, date, nextdate, time_days)]
  )
}

# ---- multiply & add_date (for Zhou-style fu_start assignment) ---------------
multiply_cases <- function(cases_dt, ratio) {
  x <- as_dt_copy(cases_dt)
  k <- ceiling(ratio)
  x <- x[rep(seq_len(.N), each = k)]
  x[]
}

assign_control_predi_diff <- function(cases_dt, controls_dt, seed = 1L) {
  cases    <- as_dt_copy(cases_dt)
  controls <- as_dt_copy(controls_dt)

  stopifnot("predi_diff" %in% names(cases))
  stopifnot("diagn_date" %in% names(controls))

  set.seed(seed)
  diffs <- cases$predi_diff
  assigned <- sample(diffs, size = nrow(controls), replace = TRUE)

  controls[, predi_diff := assigned]
  controls[, fu_start := as.IDate(diagn_date) + as.integer(predi_diff)]
  controls[, predi_diff := NULL]
  controls[]
}

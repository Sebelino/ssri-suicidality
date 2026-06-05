# diagnose_batch13.R
# Diagnostic script to identify why batch 13 of raw_diagnoses_parents.R hangs

library(DBI)
library(odbc)
library(dplyr)
library(here)
here::i_am("suicidality/extraction/diagnose_batch13.R")

source(here("suicidality", "extraction", "lib", "common.R"))

diagnose_batch13 <- function() {
  cat("=== Diagnosing batch 13 hang ===\n\n")

  # Load parent lopnrs
  parent_lopnrs <- read_rds("parent_lopnrs.rds")$lopnr
  cat("Total parent lopnrs:", length(parent_lopnrs), "\n")

  # Calculate batch 13 range
  batch_size <- 5000
  batch_num <- 13
  start_idx <- (batch_num - 1) * batch_size + 1
  end_idx <- min(batch_num * batch_size, length(parent_lopnrs))

  batch13_ids <- parent_lopnrs[start_idx:end_idx]
  cat("Batch 13 range: indices", start_idx, "to", end_idx, "\n")
  cat("Batch 13 size:", length(batch13_ids), "\n")
  cat("Batch 13 lopnr range:", min(batch13_ids), "to", max(batch13_ids), "\n\n")

  con <- db_connect()
  on.exit(dbDisconnect(con))

  # The query template from the original script
  query_template <- "
    SELECT lopnr, dia, x_indatum
    FROM dbo.v_npr_dia
    WHERE lopnr IN (%s)
      AND (
        LEFT(dia, 3) IN ('F32', 'F33')
        OR LEFT(dia, 3) IN ('X60', 'X61', 'X62', 'X63', 'X64', 'X65', 'X66', 'X67', 'X68', 'X69',
                            'X70', 'X71', 'X72', 'X73', 'X74', 'X75', 'X76', 'X77', 'X78', 'X79',
                            'X80', 'X81', 'X82', 'X83', 'X84')
        OR LEFT(dia, 3) IN ('Y10', 'Y11', 'Y12', 'Y13', 'Y14', 'Y15', 'Y16', 'Y17', 'Y18', 'Y19',
                            'Y20', 'Y21', 'Y22', 'Y23', 'Y24', 'Y25', 'Y26', 'Y27', 'Y28', 'Y29',
                            'Y30', 'Y31', 'Y32', 'Y33', 'Y34')
      )
  "

  # Try smaller sub-batches within batch 13
  sub_batch_size <- 500
  n_sub_batches <- ceiling(length(batch13_ids) / sub_batch_size)

  cat("Testing", n_sub_batches, "sub-batches of size", sub_batch_size, "\n")
  cat("=========================================\n\n")

  for (i in seq_len(n_sub_batches)) {
    sub_start <- (i - 1) * sub_batch_size + 1
    sub_end <- min(i * sub_batch_size, length(batch13_ids))
    sub_ids <- batch13_ids[sub_start:sub_end]

    cat("Sub-batch", i, "of", n_sub_batches,
        "(indices", start_idx + sub_start - 1, "to", start_idx + sub_end - 1, ")",
        "lopnrs", min(sub_ids), "-", max(sub_ids), "...")

    start_time <- Sys.time()

    tryCatch({
      id_str <- paste(sub_ids, collapse = ",")
      query <- sprintf(query_template, id_str)
      result <- DBI::dbGetQuery(con, query)

      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      cat(" OK (", nrow(result), " rows, ", round(elapsed, 2), "s)\n", sep = "")
    }, error = function(e) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
      cat(" ERROR after", round(elapsed, 2), "s:", conditionMessage(e), "\n")
    })
  }

  cat("\n=== Diagnosis complete ===\n")
}

if (sys.nframe() == 0) {
  diagnose_batch13()
}

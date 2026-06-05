# Common utilities for extraction scripts
# Uses here() for path resolution - works from any directory

library(here)

# Output directory for RDS files
rds_output_dir <- function() {

  here("suicidality", "extraction", "output", "rds")
}

# Read an RDS file from the output directory
read_rds <- function(filename) {
  readRDS(file.path(rds_output_dir(), filename))
}

# Save an RDS file to the output directory
save_rds <- function(data, filename) {
  dir <- rds_output_dir()
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  saveRDS(data, file.path(dir, filename))
}

# Check if an RDS file exists
rds_exists <- function(filename) {
  file.exists(file.path(rds_output_dir(), filename))
}

# Database connection helper
# Before running, authenticate by running:
#   kinit <MEB_ID>@MEB.KI.SE
# Set MEB_SQL_HOST to the MEB SQL Server hostname (ask MEB IT).
db_connect <- function(database = "Psych4") {
  server <- Sys.getenv("MEB_SQL_HOST")
  if (!nzchar(server)) {
    stop("Set the MEB_SQL_HOST environment variable to the MEB SQL Server hostname.")
  }
  DBI::dbConnect(
    odbc::odbc(),
    Driver = "ODBC Driver 18 for SQL Server",
    Server = server,
    Database = database,
    Trusted_Connection = "Yes",
    TrustServerCertificate = "Yes"
  )
}

# Execute a query in batches to avoid huge SQL IN clauses
# query_template should contain %s where the comma-separated IDs will be inserted
batch_query <- function(con, query_template, ids, batch_size = 5000) {
  if (length(ids) == 0) return(data.frame())

  results <- list()
  n_batches <- ceiling(length(ids) / batch_size)

  for (i in seq_len(n_batches)) {
    start_idx <- (i - 1) * batch_size + 1
    end_idx <- min(i * batch_size, length(ids))
    batch_ids <- ids[start_idx:end_idx]

    id_str <- paste(batch_ids, collapse = ",")
    query <- sprintf(query_template, id_str)
    results[[i]] <- DBI::dbGetQuery(con, query)

    cat("  Batch", i, "of", n_batches, "\n")
  }

  dplyr::bind_rows(results)
}

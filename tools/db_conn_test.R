# Before running, authenticate by running:
#
# kinit <MEB_ID>@MEB.KI.SE
#
# Set MEB_SQL_HOST to the MEB SQL Server hostname (ask MEB IT).

library(DBI)
library(odbc)

server <- Sys.getenv("MEB_SQL_HOST")
if (!nzchar(server)) {
  stop("Set the MEB_SQL_HOST environment variable to the MEB SQL Server hostname.")
}

con <- dbConnect(
  odbc(),
  Driver = "ODBC Driver 18 for SQL Server",
  Server = server,
  Database = "Psych4",
  Trusted_Connection = "Yes",
  TrustServerCertificate = "Yes"
)

dbGetQuery(con, "
  SELECT
    DB_NAME()   AS database_name,
    @@SERVERNAME AS server_name
")

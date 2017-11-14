library(dbplyr)
fp_con <- src_postgres(
  host = Sys.getenv("FAME_HOSTNAME"),
  dbname = Sys.getenv("FAME_DB_NAME"),
  user = Sys.getenv("FAME_USERNAME"),
  password = Sys.getenv("FAME_PASSWORD")
)

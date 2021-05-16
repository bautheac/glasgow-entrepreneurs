



library(dplyr)
con <- DBI::dbConnect(
  RPostgreSQL::PostgreSQL(),
  host = "localhost",
  dbname = "etfdb",
  user = "postgres",
  password = "password"
  )

DBI::dbListTables(con)

stock_db <- tbl(con, "stock")

stock_db %>% filter(exchange == "ARCA")

arca_stocks <- stock_db %>% collect()


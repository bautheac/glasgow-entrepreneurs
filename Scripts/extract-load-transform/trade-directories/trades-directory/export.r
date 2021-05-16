
# To Excel ####

# Raw

## Load entries
path_load <- here::here(
  "Scripts", "extract-load-transform", "trade-directories", "trades-directory",
  "trade-directory-records-raw.rds"
)
readr::read_rds(trades_directory_clean, path_write)

file_path <- here::here("Data", "Trade-directories-sample.xlsx")
openxlsx::write.xlsx(
  dplyr::filter(
    trades_directory_raw,
    stringr::str_detect(address.trade.street, "^[AB]")
  ),
  file_path,
  sheetName = "professions-raw",
  append = TRUE
)


# Clean

## Load entries

path_load <- here::here(
  "Scripts", "extract-load-transform", "trade-directories", "trades-directory",
  "trade-directory-records-clean.rds"
)
readr::read_rds(trades_directory_clean, path_write)


wb <- openxlsx::loadWorkbook(file = file_path)
openxlsx::addWorksheet(wb, "professions-clean")

openxlsx::writeData(
  wb, 
  "professions-clean", 
  dplyr::filter(
    trades_directory_clean, 
    stringr::str_detect(address.trade.street, "^[AB]")
  )
)

openxlsx::saveWorkbook(wb, file = file_path, overwrite = TRUE)
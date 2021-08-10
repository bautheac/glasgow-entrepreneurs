
# To Excel ####








## Sample ####
file_path <- here::here("Data", "Trade-directories-sample.xlsx")

### Raw ####

#### Load entries ####
path_load <- here::here(
  "Scripts", "extract-load-transform", "trade-directories", "trades-directory",
  "trade-directory-records-raw.rds"
)
trades_directory_raw <- readr::read_rds(path_load)

openxlsx::write.xlsx(
  dplyr::filter(
    trades_directory_raw,
    stringr::str_detect(address.trade.street, "^[AB]")
  ),
  file_path,
  sheetName = "professions-raw",
  append = TRUE
)


### Clean ####

#### Load entries ####
path_load <- here::here(
  "Scripts", "extract-load-transform", "trade-directories", "trades-directory",
  "trade-directory-records-clean.rds"
)
trades_directory_clean <- readr::read_rds(path_load)

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

















## Whole ####
file_path <- here::here("Data", "Trade-directories.xlsx")

### Raw ####

#### Load entries ####
path_load <- here::here(
  "Scripts", "extract-load-transform", "trade-directories", "trades-directory",
  "trade-directory-records-raw.rds"
)
trades_directory_raw <- readr::read_rds(path_load)

openxlsx::write.xlsx(
  trades_directory_raw,
  file_path,
  sheetName = "trade-directory-raw",
  append = TRUE
)


### Clean ####

#### Load entries ####
path_load <- here::here(
  "Scripts", "extract-load-transform", "trade-directories", "trades-directory",
  "trade-directory-records-clean.rds"
)
trades_directory_clean <- readr::read_rds(path_load)

wb <- openxlsx::loadWorkbook(file = file_path)
openxlsx::addWorksheet(wb, "trade-directory-clean")

openxlsx::writeData(
  wb, 
  "trade-directory-clean", 
  trades_directory_clean
)

openxlsx::saveWorkbook(wb, file = file_path, overwrite = TRUE)



















## Combined ####
file_path <- here::here("Data", "Trade-directories-combined.xlsx")

### Load entries ####
path_load <- here::here(
  "Scripts", "extract-load-transform", "trade-directories", "trades-directory",
  "trade-directory-records-combined.rds"
)
trades_directory_combined <- readr::read_rds(path_load)

## Write ####
openxlsx::write.xlsx(
  trades_directory_combined,
  file_path, sheetName = "trade-directory-combined", append = TRUE
)





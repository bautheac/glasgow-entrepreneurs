library(magrittr)

# Load modules ####
clean <- modules::use(
  here::here(
    "Scripts", "extract-load-transform", "trade-directories", "modules", "clean.r"
  )
)


# Extract, load raw trade directory data ####

## Load data from excel file
path_load <- here::here(
  "Data", "4 - 1861-1862 Glasgow Trade Directory Editing file with Residential.xlsx"
  )
trades_directory_raw <- openxlsx::read.xlsx(path_load) %>% tibble::as_tibble() 

## Select profession, full name and full trade address
trades_directory_raw <- dplyr::select(
    trades_directory_raw,
    profession = Aaaaaa, 
    type = `Other.Buinsess`, 
    surname = `Listing.Name/Surname`, 
    forename = `Forename[s]`, 
    address.trade.street = TradeStreet, 
    address.trade.number = `Trade.Address.Number`
  )

## Keep "own account type"
trades_directory_raw <- dplyr::filter(trades_directory_raw, type == "OWN ACCOUNT") %>% 
  dplyr::select(-c(type)) %>%
  dplyr::arrange(address.trade.street, address.trade.number)


# Transform (clean) trade directory data ####

## Clean profession
trades_directory_clean <- dplyr::mutate(
  trades_directory_raw,
  profession = gsub("(.*)\\(see.*", "\\1", profession, ignore.case = TRUE, perl = TRUE),
  forename = clean$forename(forename)
  # address.trade.street = clean$address_body(address.trade.street),
  # address.trade.number = clean$address_numbers(address.trade.number)
) %>%
  # dplyr::arrange(address.trade.street, address.trade.number)
  dplyr::arrange(forename)



# Save ####

## Raw
path_write <- here::here(
  "Scripts", "extract-load-transform", "trade-directories", "trades-directory",
  "trade-directory-records-raw.rds"
)
readr::write_rds(trades_directory_raw, path_write)

## Clean
path_write <- here::here(
  "Scripts", "extract-load-transform", "trade-directories", "trades-directory",
  "trade-directory-records-clean.rds"
)
readr::write_rds(trades_directory_clean, path_write)




library(magrittr); library(dplyr); library(modules)
library(here); library(openxlsx); library(readr)

# Load modules ####
clean <- use(
  here(
    "Scripts", "extract-load-transform", "trade-directories", "modules", "clean.r"
  )
)


# Extract, load raw trade directory data ####

## Load data from excel file
path_load <- here(
  "Data", "4 - 1861-1862 Glasgow Trade Directory Editing file with Residential.xlsx"
  )
trades_directory_raw <- read.xlsx(path_load) %>% tibble::as_tibble() 

## Select profession, full name and full trade address
trades_directory_raw <- select(
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
  select(-c(type)) %>%
  arrange(address.trade.street, address.trade.number)


# Transform (clean) trade directory data ####

## Clean profession
trades_directory_clean <- mutate(
  trades_directory_raw,
  profession = gsub("(.*)\\(.*", "\\1", profession, ignore.case = TRUE, perl = TRUE),
) %>% arrange(profession)

## Clean forename
trades_directory_clean <- mutate(
  trades_directory_raw,
  forename = clean$forename(forename)
) %>% arrange(forename)

## Clean surname
trades_directory_clean <- mutate(
  trades_directory_raw,
  surname = clean$surname(surname)
) %>% arrange(surname)

trades_directory_clean <- arrange(trades_directory_clean, surname)


# Save ####

## Raw
path_write <- here(
  "Scripts", "extract-load-transform", "trade-directories", "trades-directory",
  "trade-directory-records-raw.rds"
)
write_rds(trades_directory_raw, path_write)

## Clean
path_write <- here(
  "Scripts", "extract-load-transform", "trade-directories", "trades-directory",
  "trade-directory-records-clean.rds"
)
write_rds(trades_directory_clean, path_write)




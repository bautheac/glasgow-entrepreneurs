library(magrittr); library(tidyverse); library(modules)
library(here); library(openxlsx); library(readr)

# Load modules ####
clean <- use(
  here(
    "Scripts", "extract-load-transform", "trade-directories", "modules", "clean.r"
  )
)





# Update column names ####

## Extract, load raw trade directory data ####

### Load data from excel file
path_load <- here(
  "Data", "4 - 1861-1862 Glasgow Trade Directory Editing file with Residential.xlsx"
)
trades_directory_raw <- read.xlsx(path_load) %>% tibble::as_tibble() 

## Select profession, full name and full trade address
trades_directory_raw <- select(
  trades_directory_raw,
  page = Pg.Number,
  rank = Rank.Order,
  occupation = Aaaaaa, 
  type = `Other.Buinsess`, 
  surname = `Listing.Name/Surname`, 
  forename = `Forename[s]`, 
  address.trade.body = TradeStreet, 
  address.trade.number = `Trade.Address.Number`
)

### Keep "own account type"
trades_directory_raw <- dplyr::filter(trades_directory_raw, type == "OWN ACCOUNT") %>% 
  select(-c(type)) %>%
  mutate(registery = "1861-1862", .before = page) %>%
  arrange(address.trade.body, address.trade.number)


## Transform (clean) trade directory data ####

### Trim extra white spaces
trades_directory_clean <- mutate(
  trades_directory_raw, across(.cols = everything(), str_squish)
) 

### Clean occupation
trades_directory_clean <- mutate(
  trades_directory_clean,
  occupation = gsub("(.*)\\(.*", "\\1", occupation, ignore.case = TRUE, perl = TRUE),
  occupation = str_squish(occupation)
) 

### Clean forename
trades_directory_clean <- mutate(
  trades_directory_clean,
  forename = clean$forename(forename)
) 

### Clean surname
trades_directory_clean <- mutate(
  trades_directory_clean,
  surname = clean$surname(surname)
) 

### Clean address
trades_directory_clean <- mutate(
  trades_directory_clean,
  address.trade.number = clean$address_number(address.trade.number),
  address.trade.body = clean$address_body(address.trade.body),
  address.trade.body = ifelse(
    (is.na(address.trade.number) & is.na(address.trade.body)), 
    "No trade address found", address.trade.body
  )
) 

### Arrange by surname, forename, occupation, address number and body
trades_directory_clean <- select(
  trades_directory_clean,
  registery, page, rank, surname, forename, occupation, 
  address.trade.number, address.trade.body
) %>%
  arrange(
    surname, forename, occupation, address.trade.number, address.trade.body
  )
# arrange(address.trade.body)

## IO ####

### Raw
path_write <- here(
  "Scripts", "extract-load-transform", "trade-directories", "trades-directory",
  "trade-directory-records-raw.rds"
)
write_rds(trades_directory_raw, path_write)

### Clean
path_write <- here(
  "Scripts", "extract-load-transform", "trade-directories", "trades-directory",
  "trade-directory-records-clean.rds"
)
write_rds(trades_directory_clean, path_write)



## Sample ####
sample <- filter(trades_directory_clean, stringr::str_detect(surname, "^A"))
### IO ####
path_write <- here(
  "Scripts", "extract-load-transform", "trade-directories", "trades-directory",
  "trade-directory-records-clean-sample.rds"
)
write_rds(sample, path_write)


















# Keep original column names ####

## Extract, load raw trade directory data ####

### Load data from excel file
path_load <- here(
  "Data", "4 - 1861-1862 Glasgow Trade Directory Editing file with Residential.xlsx"
)
trades_directory_raw <- read.xlsx(path_load) %>% tibble::as_tibble() %>%
  arrange(
    Aaaaaa, `Rank.Order`, `Listing.Name/Surname`, `Forename[s]`, 
    TradeStreet, `Trade.Address.Number`
  ) %>%
  mutate(across(.cols = everything(), as.character)) %>%
  group_by(Aaaaaa, Rank.Order) %>% filter(row_number() == n()) %>%
  ungroup()


### Keep "own account type"
trades_directory_own <- dplyr::filter(
  trades_directory_raw, `Other.Buinsess` == "OWN ACCOUNT"
) 

trades_directory_joins <- dplyr::filter(
  trades_directory_raw, `Other.Buinsess` != "OWN ACCOUNT"
)

## Transform (clean) trade directory data ####

### Trim extra white spaces
trades_directory_own <- mutate(
  trades_directory_own, across(.cols = everything(), str_squish)
) 

### Separate number from address body in residential address
trades_directory_own <- mutate(
  trades_directory_own,
  Residential.Street = regmatches(
    `Residental.No.`, 
    gregexpr("^[0-9,./\\s]+\\K.+", `Residental.No.`, ignore.case=FALSE, perl=TRUE)
  ),
  Residential.Street = gsub(
    "character\\(0\\)", "", Residential.Street, ignore.case = TRUE, perl = TRUE
  ),
  `Residental.No.` = gsub(
    "^[0-9,./\\s]+\\K.+", "", `Residental.No.`, ignore.case = TRUE, perl = TRUE
  )
) 

### Clean occupation
trades_directory_own <- mutate(
  trades_directory_own,
  Aaaaaa = gsub("(.*)\\(.*", "\\1", Aaaaaa, ignore.case = TRUE, perl = TRUE),
  Aaaaaa = str_squish(Aaaaaa)
) 

### Clean forename
trades_directory_own <- mutate(
  trades_directory_own, across(.cols = matches("[fF]orename"), clean$forename)
) 

### Clean surname
trades_directory_own <- mutate(
  trades_directory_own, across(.cols = matches("[sS]urname"), clean$surname)
)

### Clean address numbers
trades_directory_own <- mutate(
  trades_directory_own, 
  across(.cols = matches("(?:Address.Number|Residental.No.)"), clean$address_number)
)

### Clean address body
trades_directory_own <- mutate(
  trades_directory_own, 
  across(.cols = matches("(?:Street|Building)"), clean$address_body),
  across(
    .cols = matches("(?:Street|Building)"), 
    function(x){
      clean <- gsub("(\\(\\w+\\b)\\.$", "\\1\\)\\.", x, ignore.case = TRUE, perl = TRUE)
    }
  )
)


## Reconcile datasets ####

trades_directory_combined <- bind_rows(trades_directory_joins, trades_directory_own) %>%
  mutate(
    `Other.Buinsess` = factor(`Other.Buinsess`, levels = c("OWN ACCOUNT", unique(trades_directory_joins$`Other.Buinsess`)))
  ) %>% 
# Arrange by type of business (own vs. joins), occupation, rank, surname, forename, 
# trade address body, trade address number
  arrange(
    `Other.Buinsess`, Aaaaaa, `Rank.Order`, `Listing.Name/Surname`, `Forename[s]`, 
    TradeStreet, `Trade.Address.Number`
  )


## Save ####

### Raw
path_write <- here(
  "Scripts", "extract-load-transform", "trade-directories", "trades-directory",
  "trade-directory-records-raw.rds"
)
write_rds(trades_directory_raw, path_write)

### Own
path_write <- here(
  "Scripts", "extract-load-transform", "trade-directories", "trades-directory",
  "trade-directory-records-own-accounts.rds"
)
write_rds(trades_directory_own, path_write)

### Combined
path_write <- here(
  "Scripts", "extract-load-transform", "trade-directories", "trades-directory",
  "trade-directory-records-combined.rds"
)
write_rds(trades_directory_combined, path_write)
























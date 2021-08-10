library(magrittr); library(here); library(tidyverse); library(modules)


# IO ####
## Trades directory ####
path_trades_directory <- here(
  "Scripts", "extract-load-transform", "trade-directories", "trades-directory",
  "trade-directory-records-clean-sample.rds"
)
trades_directory_clean <- read_rds(path_trades_directory)
## General directory ####
path_general_directory <- here(
  "Scripts", "extract-load-transform", "trade-directories", "general-directory",
  "general-directory-records-clean-sample.rds"
)
general_directory_clean <- read_rds(path_general_directory)


# Combine ####

general_directory_sample <- mutate(
  general_directory_clean,
  across(.cols = matches("number$"), ~ gsub("\\.$", ", ", .x))
) %>%
  unite(
    "address.trade", matches("address.trade"), sep = "", remove = FALSE, na.rm = TRUE
  ) %>%
  unite(
    "address.house", matches("address.house"), sep = "", remove = FALSE, na.rm = TRUE
  ) %>%
  select(-matches("^address\\.(?:house|trade)\\.")) %>%
  mutate(
    address.trade = map(
      address.trade,
      ~ ifelse(.x == "No trade address found", stringi::stri_rand_strings(1L, 22L), .x)
    ) %>% unlist(),
    across(
      .cols = matches("name"),
      ~ gsub("(?:[\\s.,]*)?(?:Mrs|Miss(?:es)?)(?:[\\s.,]*)?", "", .x),
      .names = "{.col}_edit"
    )
  ) %>%
  mutate(across(.cols = everything(), ~ gsub("\\.$", "", .x))) %>%
  unite("name", matches("name_edit"), sep = " ", remove = FALSE, na.rm = TRUE) %>%
  unite(
    "match.string", c(matches("name_edit"), "address.trade"), sep = " - ", 
    remove = FALSE, na.rm = TRUE
  ) %>%
  select(-c(name, matches("name_edit"))) %>%
  relocate(match.string, .after = last_col())

trade_directory_sample <- unite(
    trades_directory_clean,
    "address.trade", matches("address.trade"), sep = ", ", remove = FALSE, na.rm = TRUE
  ) %>%
  select(-matches("^address\\.trade\\.")) %>%
  mutate(
    across(
      .cols = matches("name"),
      ~ gsub("(?:[\\s.,]*)?(?:Mrs|Miss(?:es)?)(?:[\\s.,]*)?", "", .x),
      .names = "{.col}_edit"
    )
  ) %>%
  mutate(across(.cols = everything(), ~ gsub("\\.$", "", .x))) %>%
  unite("name", matches("name_edit"), sep = " ", remove = FALSE, na.rm = TRUE) %>%
  unite(
    "match.string", c(matches("name_edit"), matches("address")), sep = " - ", 
    remove = FALSE, na.rm = TRUE
  ) %>%
  select(-c(name, matches("name_edit"))) %>%
  relocate(match.string, .after = last_col())

combined <- fuzzyjoin::stringdist_left_join(
  trade_directory_sample, 
  select(general_directory_sample, match.string, address.house),
  by = c("match.string" = "match.string"), max_dist = 5L
) %>%
  select(-matches("match")) %>%
  mutate(
    address.house = if_else(
      is.na(address.house), "Failled to match with general directory", address.house
    )
  ) %>%
  group_by(surname, forename, occupation, address.trade, address.house) %>%
  filter(row_number() == n()) %>%
  arrange(surname, forename, address.trade, occupation)


# IO ####
path_combined <- here(
  "Scripts", "extract-load-transform", "trade-directories", "trade-directories-sample.rds"
)
write_rds(combined, path_combined)
path_combined <- here(
  "Scripts", "extract-load-transform", "trade-directories", "trade-directories-sample.csv"
)
write_csv(combined, path_combined)




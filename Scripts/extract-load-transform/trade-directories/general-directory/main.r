library(magrittr); library(here); library(tidyverse); library(modules)

# Load modules ####
globals <- use(
  here("Scripts", "extract-load-transform", "trade-directories", "modules", "globals.r")
)
filter <- use(
  here("Scripts", "extract-load-transform", "trade-directories", "modules", "filter.r")
)
clean <- use(
  here("Scripts", "extract-load-transform", "trade-directories", "modules", "clean.r")
)

# Using parsed data from python2 pod parser
path <- here("Scripts", "extract-load-transform", "trade-directories", "general-directory", "python-pod-parser")
directories <- c("1861-1862")

## Convert parsed text file to tibble
general_directory <- NULL
general_directory_raw <- map_df(directories, function(x){
  
  # browser()
  con <- file(paste(path, x, "general-directory.txt", sep = "/"), "r")
  # con <- file(paste(path, x, "general-directory-sample.txt", sep = "/"), "r")
  
  while ( TRUE ) {
    # browser()
    line = readLines(con, n = 1)
    message(line)
    if ( length(line) == 0 ) {
      break
    }
    if (str_detect(line, "Rejected")) 
      NULL 
    else {
      general_directory <<- bind_rows(
        general_directory,
        str_split_fixed(line, "[\t\\s]\\|[\t\\s]", n = 6L) %>% 
          set_colnames(
            c("page", "surname", "forename", "occupation", "occupation category", "addresses")
          ) %>% 
          as_tibble()
      )
    }
  }
  mutate(general_directory, directory = x, page = gsub("(?:page|\t)", "", page, ignore.case = TRUE, perl = TRUE)) %>%
    mutate(across(.cols = everything(), str_squish)) %>% select(directory, everything())
})
  

## IO ####
path_raw <- here(
  "Scripts", "extract-load-transform", "trade-directories", "general-directory",
  "general-directory-records-raw.rds"
)
### Save ####
write_rds(general_directory_raw, path_raw)
### Load ####
general_directory_raw <- read_rds(path_raw)





## Fix structure ####

### split trade and house addresses when both are provided
# if occupation terminates in "; house" or variant, delete and add "house, " to the beginning of addresses columns
regex_house <- ";\\s[bh][op](?:use|-)$"
general_directory_structured <- mutate(
  general_directory_raw,
  addresses = ifelse(
    grepl(regex_house, occupation, ignore.case=TRUE, perl=TRUE),
    paste("house", addresses, sep = ", "), addresses
  ) %>% unlist(),
  occupation = ifelse(
    grepl(regex_house, occupation, ignore.case=TRUE, perl=TRUE),
    gsub(regex_house, "", occupation, ignore.case=TRUE, perl=TRUE), occupation
  ) %>% unlist()
)

# clean beginning and end of addresses
general_directory_structured <- mutate(
  general_directory_structured, across(.cols = matches("^address"), clean$address_clean_ends)
)

# fix occupation in addresses column when possible: move back to occupation column.
regex_occupations <- paste0(
  "^(?:and\\s?)?(?:", 
  paste(globals$occupations, collapse = "|"), 
  ").+(?=;|\\d|[A-Z]|$)"
)
general_directory_structured <- mutate(
  general_directory_structured,
  occupation = ifelse(
    grepl(regex_occupations, addresses, ignore.case=FALSE, perl=TRUE),
    paste(occupation, regmatches(addresses, gregexpr(regex_occupations, addresses, ignore.case=FALSE, perl=TRUE)), sep = " "),
    occupation
  ) %>% unlist(),
  addresses = ifelse(
    grepl(regex_occupations, addresses, ignore.case=FALSE, perl=TRUE),
    gsub(regex_occupations, "", addresses, ignore.case=FALSE, perl=TRUE), 
    addresses
  ) %>% unlist()
)

# clean beginning and end of addresses
general_directory_structured <- mutate(
  general_directory_structured, across(.cols = matches("^address"), clean$address_clean_ends)
)

# get rid of "depot", "office", "store", "works" or "workshops" address prefix
general_directory_structured <- mutate(
  general_directory_structured,
  addresses = gsub("\\b(?:depot|(?<!post\\s)office|stores|work(?:shop)?s?)\\b", "", addresses, ignore.case=TRUE, perl=TRUE)
)

# clean beginning and end of addresses
general_directory_structured <- mutate(
  general_directory_structured, across(.cols = matches("^address"), clean$address_clean_ends)
)

# create trade and house addresses by splitting raw addresses on "house" or variant
## if "residence" matches, don't match "house", otherwise match "house".  
regex_house <- "(?:^|[;,„\\s]*)\\b(res(?:id)?(?:ence)?)?(?(1)()|((?:(?:[bht]|li|jh)[aop])(?:[ui\\/]se)?s?))\\b[.,„\\s]+"
general_directory_structured <- separate(
  general_directory_structured, col = addresses, into = c("addresses.trade", "address.house"),
  sep = regex_house, remove = TRUE, extra = "merge"
) %>% mutate(address.house = ifelse(is.na(address.house), "", address.house))

# clean beginning and end of addresses
general_directory_structured <- mutate(
  general_directory_structured, 
  across(.cols = matches("^address"), clean$address_clean_ends)
)

# clean address numbers
general_directory_structured <- mutate(
  general_directory_structured,
  across(.cols = matches("^address"), clean$address_number)
) 

# split multiple trade addresses
split <- paste0(
  "(?<=",
  paste(
    "^",
    ";",
    "(?<=\\D\\D)\\s\\ba[an]d\\b\\s(?=[A-Z0-9])",
    "(?<=\\D\\D)\\s\\ba[an]d,\\b\\s(?=[A-Z0-9])",
    "(?<=[\\D.])\\s\\&\\s(?=[A-Z0-9])",
    "(?<=[\\D.])\\&(?=[A-Z0-9])",
    "(?<!and|\\d),\\s(?!and|\\D)",
    sep = "|"),
  ")",
  ".+?",
  "(?=(?:",
  paste(
    ";",
    "$",
    "(?<=\\D\\D)\\s\\ba[an]d\\b\\s(?=[A-Z0-9])",
    "(?<=\\D\\D)\\s\\ba[an]d,\\b\\s(?=[A-Z0-9])",
    # "(?<=\\D),\\s\\ba[an]d\\b\\s(?=[A-Z0-9])",
    "(?<=[\\D.])\\s\\&\\s(?=[A-Z0-9])",
    "(?<=[\\D.])\\&(?=[A-Z0-9])",
    "(?<!and|\\d),\\s(?!and|\\D)",
    sep = "|"),
  "))"
)

general_directory_structured <- mutate(
  general_directory_structured,
  address.trade = ifelse(
    addresses.trade == "", addresses.trade,
    regmatches(
      addresses.trade, gregexpr(split, addresses.trade, ignore.case=FALSE, perl=TRUE)
    )
  )
) %>% unnest(address.trade)

# clean beginning and end of addresses
general_directory_structured <- mutate(
  general_directory_structured, 
  across(.cols = matches("^address"), clean$address_clean_ends)
)

# clean failed split on "and"
general_directory_structured <- mutate(
  general_directory_structured,
  address.trade = ifelse(
    grepl("^and\\s\\d", address.trade, ignore.case=TRUE, perl=TRUE),
    regmatches(address.trade, gregexpr("^and\\s\\K.+", address.trade, perl=TRUE)),
    address.trade
  ) %>% unlist()
) 

# clean beginning and end of addresses
general_directory_structured <- mutate(
  general_directory_structured, 
  across(.cols = matches("^address"), clean$address_clean_ends)
)


## IO ####
path_structured <- here(
  "Scripts", "extract-load-transform", "trade-directories", "general-directory",
  "general-directory-records-structured.rds"
)
### Save ####
write_rds(general_directory_structured, path_structured)
### Load ####
general_directory_structured <- read_rds(path_structured)








## Clean entries ####

### Get rid of irrelevant info
general_directory_clean <- mutate(
  general_directory_structured,
  across(
    .cols = everything(), 
    ~ gsub("(?:character\\(0\\)?\\.?|\\s— See.+|Appendix.+)", "", .x, ignore.case = TRUE, perl = TRUE)
  ),
) 

### Clean occupation
general_directory_clean <- mutate(
  general_directory_clean,
  occupation = gsub("(.*)\\(.*", "\\1", occupation, ignore.case = TRUE, perl = TRUE),
  occupation = str_squish(occupation) %>% str_to_sentence()
) 

### Clean name ####

#### Clean credentials ####
sub_regex <- paste0(
  "[[:punct:][:blank:]]*(?:", paste(globals$titles$pattern, collapse = "|"), 
  ")[[:punct:][:blank:]]*"
)
general_directory_clean <- mutate(
  general_directory_clean,
  across(.cols = matches("name"), ~ gsub(sub_regex, "", .x, ignore.case = TRUE, perl = TRUE)),
) 

#### Clean forename
general_directory_clean <- mutate(
  general_directory_clean,
  forename = clean$forename(forename) %>% str_to_title()
) 

#### Clean surname
general_directory_clean <- mutate(
  general_directory_clean,
  surname = clean$surname(surname) %>% str_to_title()
) 

### Clean addresses

#### Split numbers and address bodies
general_directory_clean <- mutate(
  general_directory_clean,
  across(
    .cols = c(address.trade, address.house), 
    ~ ifelse(
      grepl("^[0-9,\\s/]+?(?=\\s[[:alpha:]])", .x, perl=TRUE),
      regmatches(.x, gregexpr("^[0-9,\\s/]+?(?=\\s[[:alpha:]])", .x, perl=TRUE)),
      ""
    ) %>% unlist(),
    .names = "{.col}.number"
  ),
  across(
    .cols = c(address.trade, address.house), 
    ~ ifelse(
      !grepl("^$", .x, perl=TRUE),
      regmatches(.x, gregexpr("^(?:[0-9,\\s/]+?(?=[[:alpha:]]))?\\K.+", .x, perl=TRUE)),
      ""
    ) %>% unlist(),
    .names = "{.col}.body"
  )
) %>% 
  select(-c(address.trade, address.house))

#### Clean beginning and end of addresses
general_directory_clean <- mutate(
  general_directory_clean, across(.cols = matches("^address"), clean$address_clean_ends)
)

#### Clean address bodies
general_directory_clean <- mutate(
  general_directory_clean,
  across(.cols = matches("body$"), clean$address_body)
) 

#### Label missing addresses ####
general_directory_clean <- mutate(
  general_directory_clean,
  address.trade.body = ifelse(
    (address.trade.number == "" & address.trade.body == ""), 
    "No trade address found", address.trade.body
  ),
  address.house.body = ifelse(
    (address.house.number == "" & address.house.body == ""), 
    "No home address found", address.house.body
  )
)


#### Arrange by surname, forename, occupation, address number and body
general_directory_clean <- select(
  general_directory_clean,
  surname, forename, occupation, matches("^address.trade"), matches("^address.house")
) %>%
  arrange(surname, forename, occupation)



## IO ####
path_clean <- here(
  "Scripts", "extract-load-transform", "trade-directories", "general-directory",
  "general-directory-records-clean.rds"
)
### Save ####
write_rds(general_directory_clean, path_clean)
### Load ####
general_directory_clean <- read_rds(path_clean)




# Sample: Surname starts with A
sample <- filter(general_directory_clean, stringr::str_detect(surname, "^A"))
## IO ####
path_clean <- here(
  "Scripts", "extract-load-transform", "trade-directories", "general-directory",
  "general-directory-records-clean-sample.rds"
)
### Save ####
write_rds(sample, path_clean)



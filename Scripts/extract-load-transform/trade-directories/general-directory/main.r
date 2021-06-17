library(magrittr); library(here); library(tidyverse); library(modules)

# Load modules ####
globals <- use(
  here(
    "Scripts", "extract-load-transform", "trade-directories", "modules", "globals.r"
  )
)
filter <- use(
  here(
    "Scripts", "extract-load-transform", "trade-directories", "modules", "filter.r"
  )
)

# Using parsed data from python2 pod parser
path <- here("Scripts", "extract-load-transform", "trade-directories", "general-directory", "python-pod-parser")
directories <- c("1861-1862")

## Convert parsed text file to tibble
general_directory <- NULL
general_directory_raw <- map_df(directories, function(x){
  
  # browser()
  con <- file(paste(path, x, "general-directory.txt", sep = "/"), "r")
  
  while ( TRUE ) {
    # browser()
    line = readLines(con, n = 1)
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
  mutate(general_directory, page = gsub("(?:page|\t)", "", page, ignore.case = TRUE, perl = TRUE)) %>%
    mutate_all(.funs = str_squish)
  
})
  

## IO ####
path <- here(
  "Scripts", "extract-load-transform", "trade-directories", "general-directory",
  "general-directory-records-raw.rds"
)
### Save ####
write_rds(general_directory_raw, path)
### Load ####
general_directory_raw <- read_rds(path)




## Filter out ####

### Irrelevant ####

#### Companies, institutions, public places, etc. ####
# surname contains stop words as listed in the stop_places and stop_titles above
general_directory_filtered <- filter(
  general_directory_raw, 
  !grepl(paste0("\\b(?:", paste(filter$words, collapse = "|"), ")"), surname, ignore.case=TRUE, perl=TRUE)
  # , 
  # !grepl(paste0("\\b(?:", paste(filter$titles, collapse = "|"), ")"), surname, ignore.case=TRUE, perl=TRUE)
  )
#### Clerics, militaries, etc. ####
general_directory_filtered <- general_directory_filtered <- filter(
  general_directory_filtered, 
  if_all(
    surname:occupation, 
    ~ !grepl(paste0("\\b(?:", paste(filter$titles, collapse = "|"), ")"), .x, ignore.case=TRUE, perl=TRUE)
  )
)

### Partnerships ####
# surname contains an ampersand between two names
general_directory_filtered <- filter(
  general_directory_filtered, 
  if_all(
    .cols = everything(), 
    ~ !grepl(
      paste0("[A-Z](?:\\.?|[a-z]+)\\s(?:", paste(globals$ampersand, collapse = "|"), ")\\s[A-Z](?:\\.?|[a-z]+)"), 
      .x, ignore.case=TRUE, perl=TRUE)
  )
)
# surname contains "Bros" or "Brothers"
general_directory_filtered <- filter(
  general_directory_filtered, 
  if_all(
    .cols = everything(), ~ !grepl("\\bBro(?:ther)?s\\b", .x, ignore.case=TRUE, perl=TRUE)
  )
)
# any column contains "&/and Co." or "&/and son(s)" (ampersand corrected for OCR errors through the ampersand variable above)
general_directory_filtered <- filter(
  general_directory_filtered, 
  if_all(
    .cols = everything(), 
    ~ !grepl(paste0("(?:^|\\s)(?:and|", paste(globals$ampersand, collapse = "|"), ")\\s?(?:Co\\.?|Sons?)"), .x, ignore.case=TRUE, perl=TRUE)
  )
)
# any column starts with an ampersand followed by a space and a name
general_directory_filtered <- filter(
  general_directory_filtered, 
  if_all(
    .cols = everything(), 
    ~ !grepl(paste0("^(?:", paste(globals$ampersand, collapse = "|"), ")\\s[A-Z](?:\\.?|[a-z]+|[A-Z])"), .x, ignore.case=TRUE, perl=TRUE)
  )
)
# any column contains "and" or ampersand between two names
general_directory_filtered <- filter(
  general_directory_filtered, 
  if_all(
    .cols = everything(),
    ~ !grepl(
      paste0("[A-Z](?:\\.|[a-z]+)\\s(?:and|", paste(globals$ampersand, collapse = "|"), ")\\s?[A-Z](?:\\.?|[a-z]+|[A-Z])"), 
      .x, perl=TRUE
    )
  )
)

# any column contains "of" followed by an ampersand somewhere followed by a space and a capital letter
general_directory_filtered <- filter(
  general_directory_filtered, 
  if_all(
    .cols = everything(), 
    ~ !grepl(paste0("(?:^|\\s)of.+\\b,?\\s(?:", paste(globals$ampersand, collapse = "|"), ")\\s?\\b[A-Z]"), .x, perl=TRUE))
)


## IO ####
path <- here(
  "Scripts", "extract-load-transform", "trade-directories", "general-directory",
  "general-directory-records-filtered.rds"
)
### Save ####
write_rds(general_directory_filtered, path)
### Load ####
general_directory_filtered <- read_rds(path)



## Fix structure ####

### split trade and house addresses when both are provided
# if occupation terminates in "; house" or variant, delete and add "house, " to the beginning of addresses columns
general_directory_structured <- mutate(
  general_directory_filtered,
  addresses = ifelse(
    grepl(";\\sho(?:use|-)$", occupation, ignore.case=TRUE, perl=TRUE),
    paste("house", addresses, sep = ", "), addresses
  ),
  occupation = ifelse(
    grepl(";\\sho(?:use|-)$", occupation, ignore.case=TRUE, perl=TRUE),
    gsub(";\\sho(?:use|-)$", "", occupation, ignore.case = TRUE, perl = TRUE), occupation
    )
)

# create trade and house addresses by splitting raw addresses on "house" or variant
general_directory_structured <- mutate(
  general_directory_structured,
  address.trade = ifelse(
    grepl("(?<!^)\\bho(?:use)?(?:\\.|,)", addresses, ignore.case=TRUE, perl=TRUE),
    regmatches(addresses, gregexpr(".+(?=\\s?;\\sho)", addresses, perl=TRUE)),
    ifelse(grepl("^ho(?:use)?(?:\\.|,)", addresses, ignore.case=TRUE, perl=TRUE), "", addresses)
  ),
  address.house = ifelse(
    grepl("\\bho(?:use)?(?:\\.|,)", addresses, ignore.case=TRUE, perl=TRUE),
    regmatches(addresses, gregexpr("\\bho(?:use)?(?:\\.|,)\\s?\\K.+", addresses, perl=TRUE)),
    ""
  )
)


## IO ####
path <- here(
  "Scripts", "extract-load-transform", "trade-directories", "general-directory",
  "general_directory_structured.rds"
)
### Save ####
write_rds(general_directory_structured, path)
### Load ####
general_directory_structured <- read_rds(path)














# Pulling directly from directory files

library(tabulizer); library(magrittr)

# Increase memory allocation to Java from default. Avoid 'Java heap space' out
# of memory issue
options(java.parameters = "-Xmx6000m"); library(rJava)

# Java specific garbage collection function. 
jgc <- function() .jcall("java/lang/System", method = "gc")


# List individual registries here with pages detail for sections of interest.
`1861-1862` <- tibble::tibble(
  directory = rep(c("1861-1862"), 2L),
  url = rep(here::here("Data", "Trade-directory-1861-1862.pdf"), 2L),
  section = rep(c("persons", "professions")),
  start = c(69L, 465L), end = c(338L, 593L)
)


# Bind the registries defined above for systematic processing.
directories <- dplyr::bind_rows(`1861-1862`)

# Split records

split_regex <- paste0(
  "(?i)(?(?<=st|st\\.|street|street\\.|pl|pl\\.|place|place\\.",
  "|ho|ho\\.|house|house\\.|res|res\\.|resid|resid\\.|residence|residence\\.)", 
  "\\v(?=${start_letter})|(?<=\\.)\\v)"
)

# split_entries <- paste0(
#   "(?i)(?(?=st|ho|pl|st\\.|ho\\.)\\v(?=${start_letter})|(?<=\\.)\\v)"
# )
# 
# split_entries <- paste0(
#   "(?i)(?<!;\\.)(?<!;\\s?\\.)(?<!,\\s?\\.)(?<!bo\\.)(?<!jas\\.)(?<!he\\.)",
#   "(?<!ho\\.)(?<!a\\.)(?<!rev\\.)(?<!co\\.)(?<!rest\\.)(?<!house\\.)(?<!r\\.)",
#   "(?<=\\.|pl|st)",
#   "\\v",
#   "(?!;)(?!${start_letter})"
# )
# 
# split_entries <- paste0(
#   "(?i)",
#   "(?<![;,ar]\\s\\.)(?<![;,ar]\\.)",
#   "(?<!([bch]o|he|st)\\.)",
#   "\\v",
#   "(?!;)",
#   "(?=${start_letter})"
# )


general_directory_raw <- purrr::pmap_df(dplyr::filter(directories, section == "persons"), function(...){
  # browser()
  args <- rlang::list2(...)
  path <- args$url; pages <- seq(args$start, args$end)
  
  # purrr::map_chr(pages, function(x){
  purrr::map_df(69:75L, function(x){
    # purrr::map_df(75L, function(x){
    message(x)
    # browser()
    raw <- tabulizer::extract_text(path, pages = x)
    
    start_letter <- stringr::str_locate(raw, "(?<!OFFICE)\\n")[1, "end"] + 1L
    start_letter <- stringr::str_sub(raw, start_letter, start_letter)
    
    start <- regexpr("(?<=(?<!OFFICE)\\n).", raw, perl = TRUE)
    raw %<>% stringr::str_sub(start) 
    
    raw <- gsub('\\\"\\v', "", raw, perl = TRUE)
    raw <- gsub("([[:punct:]])\\s?[[:punct:]]", "\\1 ", raw, perl = TRUE)
    raw <- gsub("(?!)he\\.", "ho\\.", raw, perl = TRUE)
    raw <- gsub("\\s(—)", "\\1", raw, perl = TRUE)
    raw <- gsub("\\v(ho|res)", " \\1", raw, perl = TRUE)
    raw <- gsub("(?<=[[:alpha:]])\\.(?=[[:alpha:]])", " ", raw, perl = TRUE)
    
    records <- strsplit(raw, stringr::str_interp(split_entries), perl = TRUE)
    
    gc(); jgc()
    tibble::tibble(directory = args$directory, page = x, record = records) %>%
      tidyr::unnest(record)
  })
})



general_directory_clean <- dplyr::mutate(
  general_directory_raw,
  record = gsub("([[:punct:]])\\1", "\\1 ", record, perl = TRUE),
  record = gsub("„", ", ", record, perl = TRUE),
  record = gsub("(?<=\\w)([[:punct:]])(?=\\w)", "\\1 ", record, perl = TRUE),
  record = gsub("(?<=[[:alpha:]])-\\n(?=[[:alpha:]])", "", record, perl = TRUE),
  record = gsub(
    paste0(
      "<%|<§|\\$|4\'|4\\~|rj-|6f|<J\'|<\\||\'|<J\\*|if|<f|tf\\s|tf(?=[A-Z])|ij",
      "|cf|#|<j-|d-|cj\\*|",
      '\\scf\\s|\\sq\\s|\\sfy\\s|\\s§\\s|\\s<\\s|\\s4\"\\s'
    ), "&", record, perl = TRUE),
  record = gsub("\\s?&\\s?", " & ", record, perl = TRUE),
  record = gsub("4 Co", "& Co", record, perl = TRUE),
  record = gsub("([A-Za-z]\\.? )4( [A-Z])", "\\1&\\2", record, perl = TRUE),
  record = gsub("(\\d)\\v", "\\1 ", record, perl = TRUE),
  record = gsub("([a-z0-9])([A-Z])", "\\1 \\2", record, perl = TRUE),
  record = gsub("(\\d)\\s(\\d)", "\\1\\2", record, perl = TRUE),
  record = gsub("•", "", record, perl = TRUE),
  record = gsub("\\s?-\\s?", "-", record, perl = TRUE),
  record = gsub("\\s?\\s\\s?", " ", record, perl = TRUE),
  record = gsub("XT'", "W", record, perl = TRUE),
  record = gsub("} r", "y", record, perl = TRUE),
  record = gsub("(?i)(?<=[a-z])\\^(?=\\s)", "", record, perl = TRUE)
)

openxlsx::write.xlsx(
  general_directory_clean, 
  here::here("Data", "Trade-directories.xlsx"), 
  sheetName = "persons",
  append = TRUE
)


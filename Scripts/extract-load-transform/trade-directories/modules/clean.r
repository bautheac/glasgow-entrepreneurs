
# Load modules ####
globals <- modules::use(
  here(
    "Scripts", "extract-load-transform", "trade-directories", "modules", "globals.r"
  )
)

# Functions ####

## Main ####

### Address ####

address_body <- function(raw){
  
  # Pre-clean
  clean <- address_pre_clean(raw)
  # Places
  clean <- address_clean_places(clean)
  # Worksites
  clean <- address_clean_worksites(clean)
  # Names
  clean <- address_clean_names(clean)
  # Post clean
  clean <- address_post_clean(clean)
  
  clean
}

### Numbers ####
address_number <- function(address){
  
  clean <- address
  
  # OCR errors
  
  ## clean misread numbers
  purrr::pwalk(globals$numbers, function(pattern, replacement, ignore.case){
    clean <<- gsub(pattern, replacement, clean, ignore.case = ignore.case, perl = TRUE)
  })
  
  ## get rid of random chars around digits
  clean <- gsub('(?<=\\d)["*-](?=\\d)?', "", clean, ignore.case = FALSE, perl = TRUE)
  clean <- gsub("(?<=\\d)['](?=\\d)?", "", clean, ignore.case = FALSE, perl = TRUE)

  
  # separate 2 3-digit address numbers stuck together
  clean <- gsub("(\\d{2,3})\\s?(\\d{2,3})", "\\1, \\2", clean, ignore.case = TRUE, perl = TRUE)
  
  
  # separate address numbers with commas in place of "&" or "and".
  clean <- gsub("(\\d),?\\s?(?:f|and|&|,)\\s?(\\d)", "\\1, \\2", clean, ignore.case = TRUE, perl = TRUE)

  
  # harmonise address ranges to first number-second number
  clean <- gsub("(?<=\\d),?\\s?to,?\\s?(?=\\d)", "-", clean, ignore.case = TRUE, perl = TRUE)

  
  # separate numbers from words
  clean <- gsub("((?<=\\d))((?=[a-z]))", "\\1 \\2", clean, ignore.case = TRUE, perl = TRUE)
  
  
  # delete No. before numbers
  clean <- gsub("No[.\\s]+?(?=\\d)", "", clean, ignore.case = TRUE, perl = TRUE)
  
  
  # delete space or period between digits
  clean <- gsub("(?<=\\d)[\\s.](?=\\d(?!\\/))", "", clean, ignore.case = TRUE, perl = TRUE)
  
  # fix 1/2 issue
  clean <- gsub("0\\.5", " 1/2", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("(?<=\\d)(1\\/2)", " \\1", clean, ignore.case = TRUE, perl = TRUE)
  
  
  clean
}

### Forename ####
forename <- function(name){
  
  clean <- forename_separate_words(name)
  clean <- forename_clean_spelling(clean)
  clean <- forename_punctutation(clean)
  
  clean
}

### Surname ####
surname <- function(name){
  
  clean <- surname_clean_mac(name)
  clean <- parentheses(clean)
  clean <- surname_clean_spelling(clean)
  clean <- surname_punctutation(clean)
  
  clean
}

## Utils ####
specials <- function(x){
  
  clean <- gsub("[»■•]", "", x, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("’", "'", clean, ignore.case = TRUE, perl = TRUE)
  
  clean
}

parentheses <- function(x){
  
  clean <-gsub("(.*)\\(.*", "\\1", x, ignore.case = TRUE, perl = TRUE)
  clean
}








## Helpers ####

### Address ####

#### Pre-clean ####
address_pre_clean <- function(address){
  
  # Special characters
  clean <- specials(address)
  # Clean ends
  clean <- address_clean_ends(clean)
  # Separate words
  clean <- address_clean_attached_words(clean)
  # clean Mac name pre-fixes
  # clean <- address_clean_mac(clean)
  # Saints
  clean <- address_clean_saints(clean)
  # Possessives
  clean <- address_clean_possessives(clean)
  # Suffixes
  clean <- address_clean_suffixes(clean)
  
  clean
}


#### Post-clean ####
address_post_clean <- function(address){
  
  # Others
  clean <- address_clean_others(address)
  # Clean ends
  clean <- address_clean_ends(clean)
  
  clean
}


#### Mac ####
address_clean_mac <- function(address){
  
  clean <- address
  
  purrr::pwalk(globals$macs, function(pattern, replacement, ignore.case){
    clean <<- gsub(pattern, replacement, clean, ignore.case = ignore.case, perl = TRUE)
  })
  
  clean
}


#### Saints ####
address_clean_saints <- function(address){
  
  clean <- gsub(
    paste0(
      "\\bst[\\.,]?\\s+?(?=",
      paste0(globals$saints$pattern, collapse = "|"),
      ")"
    ),
    "Saint ",
    address, ignore.case = TRUE, perl = TRUE
  )
  
  clean <- gsub(
    paste0(
      "\\bst[\\.](?=",
      paste0(globals$saints$pattern, collapse = "|"),
      ")"
    ),
    "Saint ",
    clean, ignore.case = TRUE, perl = TRUE
  )
  
  
  purrr::pwalk(globals$saints, function(pattern, replacement, ignore.case) {
    clean <<- gsub(
      paste0("Saint\\s", pattern), paste0("Saint ", replacement),
      clean, ignore.case = ignore.case, perl = TRUE
    )
  })
  
  clean
}


#### Possessive ####
address_clean_possessives <- function(address){
  
  clean <- gsub("\\b(\\w+)\\b\\.?'?s'?", "\\1s", address, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("(\\b\\w+s\\b)'\\.?", "\\1", clean, ignore.case = TRUE, perl = TRUE)
  
  clean
}

#### Ends ####
address_clean_ends <- function(address){
  
  # Trim white space(s) at start and end as well as multiple white spaces in a row
  clean <- stringr::str_squish(address)
  # clean <- gsub("^[[:space:][:punct:]]+", "", address, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("^[\\W]+", "", address, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("\\W+$", "", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("\\s{2,}", " ", clean, ignore.case = TRUE, perl = TRUE)
  
  # Remove single letter at start or end of address.
  clean <- gsub("(?:^[A-Za-z]\\s|,?\\s[A-Za-z]$)", "", clean, ignore.case = FALSE, perl = TRUE)
  
  
  # clean <- gsub("^[A-Z]\\s", "", clean, ignore.case = FALSE, perl = TRUE)
  # clean <- gsub("(\\(\\w+))(\\.)?$", "\\1)\\2", clean, ignore.case = FALSE, perl = TRUE)
  
  # Place period at the end of address if none
  clean <- gsub("([^.])$", "\\1\\.", clean, ignore.case = TRUE, perl = TRUE)
  
  clean <- stringr::str_squish(clean)
  
  clean
}

#### Attached words ####
address_clean_attached_words <- function(address){
  
  # If two words are only separated by a period or comma, replace period with white space
  clean <- gsub(
    "([a-z])[.,]([a-z])", "\\1 \\2", address, ignore.case = TRUE, perl = TRUE
  )
  
  # if lower case followed by upper case, add space between the two.
  clean <- gsub(
    "([a-z])([A-Z])", "\\1 \\2", address, ignore.case = FALSE, perl = TRUE
  )
  
  clean <- stringr::str_squish(clean)
  
  clean
}

#### Places ####
address_clean_places <- function(address){
  
  clean <- address

  purrr::pwalk(globals$places_regex, function(pattern, replacement, ignore.case) {
    clean <<- gsub(pattern, replacement,clean, ignore.case = ignore.case, perl = TRUE
    )
  })
  

  clean <- gsub("((?<!Mil|Mill|B))(road|street)\\.?", "\\1 \\2", clean, ignore.case = TRUE, perl = TRUE)
  
  clean <- stringr::str_squish(clean)
  
  clean
}

#### Worksites ####
address_clean_worksites <- function(address){
  
  clean <- address
  purrr::pwalk(globals$worksites, function(pattern, replacement, ignore.case){
    clean <<- gsub(pattern, replacement, clean, ignore.case = ignore.case, perl = TRUE)
  })
  
  clean
}

#### Suffixes ####
address_clean_suffixes <- function(address){
  
  clean <- address
  
  purrr::pwalk(globals$suffixes, function(pattern, replacement, ignore.case) {
    clean <<- gsub(pattern, replacement,clean, ignore.case = ignore.case, perl = TRUE
    )
  })

  
  clean <- stringr::str_squish(clean)
  
  clean
}

#### Names ####
address_clean_names <- function(address){
  
  clean <- address
  
  purrr::pwalk(globals$address_names, function(pattern, replacement, ignore.case) {
    clean <<- gsub(pattern, replacement,clean, ignore.case = ignore.case, perl = TRUE
    )
  })
  
  
  clean <- stringr::str_squish(clean)
  
  clean
}

#### Others ####
address_clean_others <- function(address){
  
  # Get rid of parasite postfixes
  ## (Agents)
  clean <- gsub("(.*)\\s\\(?\\bag(?:en)?ts?\\b\\)?.?", "\\1", address, ignore.case = TRUE, perl = TRUE)
  ## (House)
  # clean <- gsub("(.*)\\s\\(house\\).?", "\\1", clean, ignore.case = TRUE, perl = TRUE)
  ## (Work)
  # clean <- gsub("(.*)\\s\\(work\\).?", "\\1", clean, ignore.case = TRUE, perl = TRUE)
  
  # Get rid of parasite punctation
  clean <- gsub("'", "", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub('\\"\\b', "", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("\\b[\\-.]\\s", " ", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("\\b([[:punct:]])[[:punct:]]\\s", "\\1 ", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("\\b[[:punct:]]\\s(?=\\w)", " ", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("(?<=\\s)[[:punct:]]\\s", "\\1", clean, ignore.case = TRUE, perl = TRUE)
  
  # Remove single letters
  # browser()
  clean <- gsub("(?:\\s[a-z]\\b|\\s\\.[a-z]\\.(?:\\s|$))", "", clean, ignore.case = FALSE, perl = TRUE)
  clean <- gsub("\\b\\s[a-z]\\s\\b", "", clean, ignore.case = FALSE, perl = TRUE)
  
  # Unwanted processing outcomes
  ## "street" processing sometimes outputs orpheans ", reet,". Replace with comma.
  clean <- gsub(", reet,", ",", clean, ignore.case = TRUE, perl = TRUE)
  
  # If placed between two words, replace period with a comma.
  clean <- gsub("\\b\\.\\s\\b", ", ", clean, ignore.case = TRUE, perl = TRUE
  )
  
  # If place not located at the end of the address and not followed by a word that 
  # is itself a place, append a comma.
  clean <- gsub(
    paste0(
      "\\b(",
      paste(globals$places_raw, collapse = "|"), 
      ")\\b(?!$|[,\\-]|\\s+?(?:\\(|",
      paste(globals$places_raw, collapse = "|"),
      "))"),
    "\\1, ", 
    clean, 
    ignore.case = TRUE, perl = TRUE
  )
  # If place not located at the beginning of the address and separated from previous
  # word-that is not itself a place -with comma and space, delete comma.
  clean <- gsub(
    paste0(
      "(?<!",
      paste(globals$places_raw, collapse = "|"),
      "),\\s+?(", 
      paste(globals$places_raw, collapse = "|"), 
      ")"
    ),
    " \\1", 
    clean, 
    ignore.case = TRUE, perl = TRUE
  )
  
  clean <- stringr::str_squish(clean)
  
  clean
}

### Forename ####

#### Separate words ####
forename_separate_words <- function(name){
  
  clean <- gsub("(?<=[a-z.])([A-Z])", " \\1", name, ignore.case = FALSE, perl = TRUE)
  clean <- gsub("([A-Z])([A-Z])", "\\1. \\2 ", clean, ignore.case = FALSE, perl = TRUE)
  clean <- gsub("([A-Za-z])&([A-Z])", "\\1 & \\2", clean, ignore.case = FALSE, perl = TRUE)
  clean <- gsub("([A-Z])\\.([A-Z]+\\b)", "\\1\\. \\2", clean, ignore.case = TRUE, perl = TRUE)
  clean
}

forename_punctutation <- function(name){
  
  # clean <- gsub("(\\b[A-Z]\\b[^.])", "\\1\\.", name, ignore.case = FALSE, perl = TRUE)
  clean <- gsub("[,'.\\s]+$", "\\.", name, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("([^.])$", "\\1\\.", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("(\\b[A-Z]\\b[^.]\\s)", "\\1\\. ", clean, ignore.case = TRUE, perl = TRUE)
  
  clean
}

#### Spelling ####
forename_clean_spelling <- function(name){
  
  clean <- name
  
  purrr::pwalk(globals$forenames, function(pattern, replacement, ignore.case) {
    clean <<- gsub(pattern, replacement,clean, ignore.case = ignore.case, perl = TRUE
    )
  })
  
  clean
}

### Surname ####

#### Mac ####
surname_clean_mac <- function(address){
  
  clean <- gsub("\\bMa?c(\\w{2,}\\b)", "Mac \\1", address, ignore.case = TRUE, perl = TRUE)
  
  clean
}

surname_punctutation <- function(name){
  
  clean <- gsub("\\*", "", name, ignore.case = TRUE, perl = TRUE)
  
  clean
}

#### Spelling ####
surname_clean_spelling <- function(name){
  
  clean <- name
  
  purrr::pwalk(globals$surnames, function(pattern, replacement, ignore.case) {
    clean <<- gsub(pattern, replacement,clean, ignore.case = ignore.case, perl = TRUE
    )
  })
  
  clean
}

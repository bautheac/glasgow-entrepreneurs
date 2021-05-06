library(tabulizer); library(magrittr)

# Increase memory allocation to Java from default. Avoid 'Java heap space' out
# of memory issue
options(java.parameters = "-Xmx6000m"); library(rJava)

# Java specific garbage collection function. 
jgc <- function() .jcall("java/lang/System", method = "gc")

# Source functions
source(here::here("Scripts", "functions.r"))


# Trade directories ####

# List individual registries here with pages detail for sections of interest.
`1861-1862` <- tibble::tibble(
  directory = rep(c("1861-1862"), 2L),
  url = rep(here::here("Data", "Trade-directory-1861-1862.pdf"), 2L),
  section = rep(c("persons", "professions")),
  start = c(69L, 465L), end = c(338L, 593L)
)



# Bind the registries defined above for systematic processing.
directories <- dplyr::bind_rows(`1861-1862`)


# Front of the book ####

split_entries <- paste0(
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


front_raw <- purrr::pmap_df(dplyr::filter(directories, section == "persons"), function(...){
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



front_clean <- dplyr::mutate(
  front_raw,
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
  persons_clean, 
  here::here("Data", "Trade-directories.xlsx"), 
  sheetName = "persons",
  append = TRUE
)



# Back of the book

back_raw <- openxlsx::read.xlsx(
  here::here("Data", "4 - 1861-1862 Glasgow Trade Directory Editing file with Residential.xlsx")
) %>% 
  tibble::as_tibble() %>% 
  dplyr::select(
    profession = Aaaaaa, type = `Other.Buinsess`, 
    surname = `Listing.Name/Surname`, forename = `Forename[s]`, 
    address.trade.street = TradeStreet, address.trade.number = `Trade.Address.Number`
  ) %>%
  dplyr::mutate_at(
    .vars = c("address.trade.street", "address.trade.number"),
    .funs = stringr::str_squish
  ) %>% 
  dplyr::filter(type == "OWN ACCOUNT") %>% dplyr::select(-c(type)) %>%
  dplyr::arrange(address.trade.street, address.trade.number)

test <- dplyr::filter(
  back_raw,
  # stringr::str_detect(address.trade.street, "(?i)Avondale")
  stringr::str_detect(surname, "Bawden"),
  stringr::str_detect(forename, "John")
)
# file_path <- here::here("Data", "Trade-directories-sample.xlsx")
# openxlsx::write.xlsx(
#   dplyr::filter(
#     back_raw, 
#     stringr::str_detect(address.trade.street, "^[AB]")
#   ), 
#   file_path, 
#   sheetName = "professions-raw",
#   append = TRUE
# )

back_clean <- dplyr::mutate(
  back_raw,
  profession = gsub("(.*)\\(see.*", "\\1", profession, ignore.case = TRUE, perl = TRUE),
  address.trade.street = clean_address(address.trade.street),
  address.trade.number = clean_numbers(address.trade.number)
) %>%
  dplyr::arrange(address.trade.street, address.trade.number)


test <- dplyr::filter(
  back_clean,
  # stringr::str_detect(address.trade.street, "(?i)Port")
  stringr::str_detect(address.trade.street, "(?i)Rol")
  # stringr::str_detect(surname, "Struthers"),
  # stringr::str_detect(forename, "Thos.")
) %>% dplyr::arrange(surname)
# test <- dplyr::filter(professions_clean, stringr::str_detect(address.trade.street, "(?i)\\bq.*y\\b"))

wb <- openxlsx::loadWorkbook(file = file_path)
openxlsx::addWorksheet(wb, "professions-clean")

openxlsx::writeData(
  wb, 
  "professions-clean", 
  dplyr::filter(
    back_clean, 
    stringr::str_detect(address.trade.street, "^[AB]")
  )
)

openxlsx::saveWorkbook(wb, file = file_path, overwrite = TRUE)



clean_address("Newstreet. Calton.")





test <- "Main st. Anderst."
test <- gsub("\\b[Ss]t(?:reet)?\\b\\.?", "Street", test, ignore.case = FALSE, perl = TRUE)
test <- gsub("street\\s*(?=(?:Vincent|Andrew))", "Saint ", test, ignore.case = TRUE, perl = TRUE)

test <- "Main StreetGorb"
test <- gsub("(street)(?=[a-z])", "\\1 ", test, ignore.case = TRUE, perl = TRUE)

test <- "Main Street, Anderston"
test <- gsub("(street)[,.]", "\\1", test, ignore.case = TRUE, perl = TRUE)

test <- "Main Street, Anderston"
test <- gsub("\\b(street)\\S?(?=\\s)\\b", "\\1", test, ignore.case = TRUE, perl = TRUE)

test <- "Abbotsford Place (house)."
test <- gsub("(.*)\\s\\(house\\).?", "\\1", test, ignore.case = TRUE, perl = TRUE)

test <- "Baird Street Street Rol."
test <- "Baird Street, off Glebe street."
test <- "Baird Street, Westmuir."
test <- gsub("(Baird\\sStreet)(.*(glebe|westmuir).*)?.*", "\\1\\2", test, ignore.case = TRUE, perl = TRUE)

test <- "Port, Dundas"
test <- gsub("\\bPort, (\\w)", "Port-\\1", test, ignore.case = TRUE, perl = TRUE)

test <- "Avondale place. Paisley road."
test <- clean_address(test)

test <- "Argyle ar."
test <- gsub("\\bar(?:c|ea)?(?:ade)?\\b\\.?", "Arcade", test, ignore.case = TRUE, perl = TRUE)

test <- "Muslin Street, Bridg."
test <- gsub("\\bMuslin Street[,\\s]+Br(?:id)?g?e?t?o?n?\\b\\.?", "Muslin Street, Bridgeton", test, ignore.case = TRUE, perl = TRUE)


m <- gregexpr("^[A-Z][a-z]{2,}$", professions$forename, perl = TRUE)
forenames <- regmatches(professions$forename, m) %>% unlist() %>% unique() %>%
  sort()
forenames <- forenames[forenames %in% c("Miss", "Misses")]
head(regmatches(professions$forename, m))
forenames <- gsub("^, ", "", address.trade, perl = TRUE)

stringr::str_extract_all(professions$forename, "^\\w{2,}(?=\\.)$")

persons <- dplyr::mutate(
  persons_clean, 
  Other.Buinsess = purrr::map_chr(record, function(x){
    if(grepl(" son[s]?", x, ignore.case = TRUE, perl = TRUE)) "& Son(s)" 
    else if (grepl(" co[\\.]?", x, ignore.case = TRUE, perl = TRUE)) "& Co." 
    else if (grepl("&", x, ignore.case = TRUE, perl = TRUE)) "Joint"
    else NA
  })
)



dplyr::filter(persons, !is.na(Other.Buinsess))

names(professions)








dplyr::filter(
  persons_clean, page == 69
) %>% tail()

dplyr::filter(
  persons_clean, page == 70
) %>% head()

dplyr::filter(persons_clean, page == 75) %>% dplyr::slice(39)

raw <- tabulizer::extract_text(here::here("Data", "Trade-directory-1861-1862.pdf"), pages = 73L)





split_entries <- paste0(
  "(?(?=regex)then|else)"
  "(?<![Ss]t\\.|[Hh]o\\.)",
  "(?<=\\.)", 
  "\\v", 
  "(?!;)"
)


split_entries <- paste0(
  "(?(?<=[Ss]t|[Hh]o|[Ss]t\\.|[Hh]o\\.)\\v(?=A)|(?<=\\.)\\v)"
)

split_entries <- paste0(
  "(?i)(?(?<=st|ho|pl|st\\.|so\\.)\\v(?=A)|(?<=\\.)\\v)"
)


test <- strsplit(persons$records, split_entries, perl = TRUE)
test[[1L]]
persons$records <- strsplit(persons$records, split_entries, perl = TRUE)
?strsplit



x <- persons$records 
gsub("\\n", " ", persons$records)
m <- gregexpr("[Hh]o(use)?[\\W]+\\K.+", x, perl = TRUE)
regmatches(x, m)







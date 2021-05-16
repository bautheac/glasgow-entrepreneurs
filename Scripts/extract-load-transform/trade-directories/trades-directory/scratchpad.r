

test <- dplyr::filter(
  trades_directory_raw,
  # stringr::str_detect(address.trade.street, "(?i)Avondale")
  stringr::str_detect(surname, "Ross"),
  stringr::str_detect(forename, "A.")
)


test <- dplyr::filter(
  trades_directory_clean,
  # stringr::str_detect(address.trade.street, "(?i)Port")
  # stringr::str_detect(address.trade.street, "Saint And.*")
  stringr::str_detect(surname, "Warden"),
  stringr::str_detect(forename, "Archd.")
) %>% dplyr::arrange(surname)
# test <- dplyr::filter(professions_clean, stringr::str_detect(address.trade.street, "(?i)\\bq.*y\\b"))


clean_address("Newstreet. Calton.")
clean_saints("Castle st. St. Rollox.")
clean_possessives("Saint James’. Kingston.")




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

test <- "Westmin, Terrace"
test <- gsub(paste0(",\\s+(", paste(places, collapse = "|"), ")"), " \\1", test, ignore.case = TRUE, perl = TRUE)



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






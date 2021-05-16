path <- "https://deriv.nls.uk/dcn6/8436/84361765.6.pdf"

file <- tempfile()
download.file(path, file)

pages <- pdftools::pdf_info(file)$pages

temp <- tabulizer::extract_text(file, pages = 446L)

stringr::str_split(temp, "\\n\\d+")

stringr::str_extract(temp, "^(?<=424 SAUCHIEHALL. STREET DIRECTORY.- -SAUCHIEHALL.).*$")


x <- c("apple", "banana", "pear")
str_view(x, "an")

dot <- "\\."
writeLines(dot)

str_view(c("ab\"\'\\c", "a\\c", "bef"), "b\"\'\\\\c")

exp <- "\\..\\..\\.."
writeLines(exp)



str_view(c("$^$"), "^\\$\\^\\$$")



str_view(stringr::words, "^y", match = TRUE)
str_view(stringr::words, "x$", match = TRUE)
str_view(stringr::words, "^.{3}$", match = TRUE)
str_view(stringr::words, "^.{7}.*$", match = TRUE)


str_view(stringr::words, "^[aeiouy]", match = TRUE)
str_view(stringr::words, "^[^aeiouy]", match = TRUE)
str_view(stringr::words, "[^e]ed$", match = TRUE)
str_subset(c("ed", stringr::words), "(^|[^e])ed$")
str_view(stringr::words, "i(ng|se)$", match = TRUE)

str_view(stringr::words, "(cei|[^c]ie)", match = TRUE)
str_view(stringr::words, "q[^u]$", match = TRUE)
str_view(stringr::words, "s[ea]", match = TRUE)

str_view(c("0621630197", "0490321749"), "^0[67][0-9]{8}", match = TRUE)
str_view(c("0621630197", "0490321749", "+33621630197"), "^(\\+33|0)[67][0-9]{8}", match = TRUE)

{0,1}
{1,}
{0,}

str_view(stringr::words, "^[^aeiouy]{3}[aeiouy]{3}([aeiouy][^aeiouy]){2,}", match = TRUE)






library(stringr)

txt <- "abdji*\\d+*lkdiflg"
str_split(txt, "\\Q*\\d+*\\E")

txt <- "c:\\temp"
str_split(txt, "\\\\")
str_split(txt, ":\\\\")
strsplit(txt, "\\", fixed = TRUE)


name <- c("jim", "john", "bill")
gsub("^(.)", "\\U\\1", name, perl = TRUE)




"([0-9])\1+"
"[Hh]o(use)?|[Rr]es(idence)?"




library(stringi)

test <- 'The\u00a0above-mentioned    features are very useful. Spam, spam, eggs, bacon, and spam.'
stri_locate_all_boundaries(test, type='line')
stri_locate_all_boundaries(test, type='word')
stri_locate_all_boundaries(test, type='sentence')
stri_locate_all_boundaries(test, type='character')
stri_locate_all_words(test)
stri_extract_all_boundaries(
  'Mr. Jones and Mrs. Brown are very happy. So am I, Prof. Smith.', 
  type = 'sentence', locale = 'en_GB@ss=standard'
)

strsplit(test, "(?<=.)(?=s)", perl = TRUE)



library(tabulizer); library(magrittr); library(stringi)

path <- here::here("Data", "Trade-directory-1861-1862.pdf")
raw <- tabulizer::extract_text(path, pages = 69L)
start <- regexpr("(?<=(?<!OFFICE)\\n).", raw, perl = TRUE)
raw %<>% stringr::str_sub(start) 



test <- strsplit(raw, "(?<=\\.)\\v", perl = TRUE) %>% unlist()
test <- strsplit(raw, "\\.\\K\\v", perl = TRUE) %>% unlist()
stri_locate_all_boundaries(raw, type='sentence')
stri_split_boundaries(raw, type='line')

x <- test[4L]
m <- gregexpr("\\B\\w{3,}\\B", x, perl=TRUE)
m <- gregexpr("(?:[Hh]o(use)?)|(?:[Rr]es(idence)?).+", x, perl = TRUE)
m <- gregexpr("[Hh]o(use)?[\\W]+\\K.+", x, perl = TRUE)
regmatches(x, m)




gsub("[\\v]", " ", test[1L], perl = TRUE)

replace <- tibble::tibble(
  pattern = c(
    "-\n", "4'", "<f", "cf", "cj-"
  ),
  replacement = c(
    "", "&", "&", "&", "&"
  )
)



trades <- tibble::tibble(
  surname = c("Sweet", "Sweet", "Sweet", "Sweet"),
  forename = c("James R.", "Donald", "James R.", "James R."),
  trade.street = c("New City rd.", "New City rd.", "Sauchiehall st.", "New City rd."),
  trade.number = c("144", "144", "144", "331")
) %>% dplyr::mutate(name = paste(surname, forename), trade = paste(trade.street, trade.number))

general <- tibble::tibble(
  surname = c("Swet", "Seet", "Swee", "weet"),
  forename = c("Jas R.", "Dld", "James", "Ja. R."),
  trade.street = c("N. Cit. rd.", "New City", "Sauch. st.", "New City road."),
  trade.number = c("14", "44", "46", "33"),
  home.street = c("Parliamentary road.", "S. Portland street.", "George Square", "Ingram street"),
  home.number = c("411", "95", "18", "20")
) %>% dplyr::mutate(name = paste(surname, forename), trade = paste(trade.street, trade.number))

fuzzyjoin::stringdist_left_join(
  trades, general, 
  by = c("name", "trade"),
  max_dist = 5
  )

fuzzyjoin::regex_left_join(trades, general, by = "surname")


fuzzyjoin::fuzzy_left_join(
  trades, general, by = c("surname"), match_fun = agrep
)

fuzzyjoin::stringdist_left_join(trades, general, by = "surname")

?agrep


library(dplyr)
library(ggplot2)
data(diamonds)

diamonds <- tbl_df(diamonds)

d <- data_frame(regex_name = c("^Idea", "mium", "Good"),
                type = 1:3)

# When they are inner_joined, only Good<->Good matches
diamonds %>%
  inner_join(d, by = c(cut = "regex_name"))

# but we can regex match them
diamonds %>%
  fuzzyjoin::regex_inner_join(d, by = c(cut = "regex_name"))


d <- data_frame(approximate_name = c("Idea", "Premiums", "Premioom",
                                     "VeryGood", "VeryGood", "Faiir"),
                type = 1:6)
# no matches when they are inner-joined:
diamonds %>%
  inner_join(d, by = c(cut = "approximate_name"))

# but we can match when they're fuzzy joined
diamonds %>%
  fuzzyjoin::stringdist_inner_join(d, by = c(cut = "approximate_name"))



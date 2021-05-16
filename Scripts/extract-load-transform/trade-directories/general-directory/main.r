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


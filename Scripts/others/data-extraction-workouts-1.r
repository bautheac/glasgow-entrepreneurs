library(tabulizer); library(magrittr)

# Increase memory allocation to Java from default. Avoid 'Java heap space' out
# of memory issue
options(java.parameters = "-Xmx6000m"); library(rJava)

# Java specific garbage collection function. 
jgc <- function() .jcall("java/lang/System", method = "gc")




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

# When read into memory, each registry page comes in as a single string. Use a 
# regex to separate records. This is very tedious for two main reasons. First, 
# there is no enforced convention for record boundaries. Some entries fit on one
# line while others span multiple lines; while a period marks the end of the 
# vast majority of entries, some entries finish in a comma, a semi-colon, etc. 
# Second, readability issues with the original document translate into some 
# characters being misinterpreted by the pdf reader. The regular expression bellow 
# separated entries upon a new line not followed by ";" while being preceded by a 
# dot or "pL" or "st", a semi-colon followed by a dot, a comma followed by no or 
# one white space followed by a dot, "bo.", "Jas.", "he.", etc.
regex_split <- 
  "(?<!;\\.)(?<!;\\s?\\.)(?<!,\\s?\\.)(?<!bo\\.)(?<!Jas\\.)(?<!he\\.)(?<!ho\\.)(?<!A\\.)(?<!Rev\\.)(?<!Co\\.)(?<!rest\\.)(?<!house\\.)(?<!r\\.)(?<!Jamaica\\.)(?<=\\.|pL|st)\\n(?!;)"

persons <- purrr::pmap_df(dplyr::filter(directories, section == "persons"), function(...){
  # browser()
  args <- rlang::list2(...)
  path <- args$url; pages <- seq(args$start, args$end)
  
  records <- purrr::map_df(pages, function(x){
  # records <- purrr::map_df(69:100L, function(x){
    
    message(x)
    
    # Read pdf in
    raw <- tabulizer::extract_text(path, pages = x)
    
    # Find the first entry by locating the first new line; start from the first 
    # character following the new line. 
    # Split the string using the regex defined above.
    records <- stringr::str_sub(
      raw, stringr::str_locate(raw, "(?<!OFFICE)\\n")[1, "end"] + 1L
    ) %>% stringr::str_split(regex_split) %>% unlist() 
    
    out <- tibble::tibble( directory = args$directory, page = x, record = records)
    gc(); jgc(); out
  })
}) %>% 
  # Keep records with a nimimum of 10 characters. 
  dplyr::filter(nchar(record) > 10L)


# Write entries to an Excel workbook for manual editing
openxlsx::write.xlsx(
  persons, 
  here::here("Data", "Trade-directories.xlsx"), 
  sheetName = "persons",
  append = TRUE
)


















































persons <- purrr::pmap_df(dplyr::filter(directories, section == "persons"), function(...){
  # browser()
  args <- rlang::list2(...)
  path <- args$url; pages <- seq(args$start, args$end)
  
  records <- purrr::map_df(pages, function(x){
    # records <- purrr::map_df(69:100L, function(x){
    
    message(x)
    
    # Read pdf in
    raw <- tabulizer::extract_text(path, pages = x)
    
    start_letter <- stringr::str_sub(
      raw, 
      stringr::str_locate(raw, "(?<!OFFICE)\\n")[1, "end"] + 1L,
      stringr::str_locate(raw, "(?<!OFFICE)\\n")[1, "end"] + 1L
    )
    end_letter <- 
      letters[which(letters == tolower(start_letter)) + 1L] %>% toupper()
    lookahead <- paste0("(?=", start_letter, "|", end_letter, ")")
    
    records <- stringr::str_sub(
      raw, stringr::str_locate(raw, "(?<!OFFICE)\\n")[1, "end"] + 1L
    ) %>% stringr::str_split(regex_split) %>% unlist() %>%
      sapply(function(x){
        stringr::str_split(
          x, paste0("(?<=t\\.|pL|st)\\n", lookahead)
        ) %>% unlist() 
        # %>%
        #   sapply(function(x) stringr::str_replace_all(x, c("\n"), " "))
      }) %>% unlist()
    
    out <- tibble::tibble( directory = args$directory, page = x, record = records)
    gc(); jgc(); out
  })
}) %>% dplyr::filter(nchar(record) > 10L)







x <- stringr::str_replace_all(x, c("<%|<§|\\$|4\'|4\\~|rj-|6f|<J\'|<\\|\'|<J\\*|if"), "&")
x <- stringr::str_replace_all(x, c("<f\n|tf\\s|tf(?=[A-Z])|ij"), "& ")
x <- stringr::str_replace_all(x, c('\\scf\\s|\\sq\\s|\\sfy\\s|\\s§\\s|\\s4\"\\s'), " & ")
x <- stringr::str_replace_all(x, c("(?<=[Ss])i.?(?=\\.)"), "t")
x <- stringr::str_replace_all(x, c("(?<=[A-Za-z])-\\s(?=[A-Za-z])"), "")
x <- stringr::str_replace_all(x, c("of(?=[A-Z])"), "of ")











dplyr::filter(persons, stringr::str_detect(record, "office"))




c("house", "ho.", "bo.", "residence")


c(
  "Death Insurance", "Appendix", "Church", "minister", "League Office",
  "Affidavit"
)







raw <- tabulizer::extract_text(path, pages = 77L)
expression <- "(?<![Ss]t\\.)(?<=\\.)\\n"
text <- "Adam, Robert, flesher, 4 King street ; house, 36 St.\nAndrew's square."
stringr::str_split(text, expression)


entries <- purrr::map_df(pages, function(x){
  
  message(x)
  
  raw <- tabulizer::extract_text(path, pages = x)
  
  start_letter <- stringr::str_sub(
    raw, 
    stringr::str_locate(raw, "(?<!OFFICE)\\n")[1, "end"] + 1L,
    stringr::str_locate(raw, "(?<!OFFICE)\\n")[1, "end"] + 1L
  )
  end_letter <- letters[which(letters == tolower(start_letter)) + 1L] %>% 
    toupper()
  lookahead <- paste0("(?=", start_letter, "|", end_letter, ")")
  
  out <- tibble::tibble(
    entry = stringr::str_sub(
      raw, stringr::str_locate(raw, "(?<!OFFICE)\\n")[1, "end"] + 1L
    ) %>% stringr::str_split(
      "(?<!;\\.)(?<!;\\s?\\.)(?<!,\\s?\\.)(?<!bo\\.)(?<!Jas\\.)(?<!he\\.)(?<!ho\\.)(?<![Ss]t\\.)(?<=\\.)\\n(?!--)(?!Andrew's)"
    ) %>% unlist() %>%
      sapply(function(x){
        x <- stringr::str_replace_all(x, c("<f\n"), "& ")
        x <- stringr::str_replace_all(x, c("<%"), "&")
        stringr::str_split(x, paste0("(?<=t\\.)\\n", lookahead)) %>% unlist()
      }) %>% unlist()
  )
  
  gc(); jgc(); out
})


dplyr::filter(
  entries,
  stringr::str_detect(entry, "Jas")
)

# (?<!,\\s\\.)(?<!ho\\.)(?<![Ss]t\\.)(?<=\\.)\\n

raw <- tabulizer::extract_text(path, pages = 69L)
entries <- tibble::tibble(
  entry = stringr::str_sub(
    raw, stringr::str_locate(raw, "(?<!OFFICE)\\n")[1, "end"] + 1L
  ) %>% stringr::str_split("(?<!ho\\.)(?<![Ss]t\\.)(?<=\\.)\\n") %>% unlist() %>%
    sapply(function(x){
      x <- stringr::str_replace_all(x, c("<f\n"), "& ")
      x <- stringr::str_replace_all(x, c("<%"), "&")
      stringr::str_split(x, paste0("(?<=t\\.)\\n", lookahead)) %>% unlist()
    }) %>% unlist()
) 





places <- c("")


text <- "Adam, Robert, flesher, 4 King street ; house, 36 St.\nAndrew's square."

lookahead <- paste0("(?=", start_letter, "|", end_letter, ")(?!.+square|road)")

stringr::str_split(text, paste0("(?<=t\\.)\\n", lookahead)) %>% unlist()






m <- regexpr("Adams(?=.+$)", raw, perl=TRUE)
m <- regexpr("Union.+(?=.+$)", raw, perl=TRUE)
m <- regexpr("\\n(.*)\\.\\n$", raw, perl=TRUE)
m <- regexpr("\\n(.*)(?=\\.\\n$)", raw, perl=TRUE)
m <- regexpr("\\n(?=.*\\.\\n$)", raw, perl=TRUE, dotall = TRUE)
regmatches(raw, m)



stringr::str_extract(raw, stringr::regex("\\.\\n[A-Z]{5}(?=.*\\.\\n$)", dotall = TRUE))



stringr::str_sub(raw, 4484, 4495)
raw[4484:4495]
text <- "Abercrombie, Alexander, of Robert Wotherspoon <% Co., 46 Dunlop street; ho. 128 South Portland st."
stringr::str_replace_all(text, c("<%"), "&")



entries <- stringr::str_sub(
  raw, stringr::str_locate(raw, "^.+\\n")[1, "end"] + 1L
) %>% stringr::str_split("(?<=\\.)\\n") %>% unlist() %>%
  sapply(function(x){
    stringr::str_replace_all(x, c("\\n|\\^"), " ")
  })







names <- stringr::str_sub(
  raw, 
  stringr::str_locate(raw, "^.+\\d{1,3}\\n")[1, "end"] + 1L
) %>% stringr::str_split("\\.\\n")




class(pmet)
stringr::str_extract(temp, "(?<=(^.+[0-9]{1,3}\\n)).+")

grep("(?<=^.+[0-9]{1,3})\\n", temp)


stringr::str_extract_all(temp, "^(?<=.+\\d{1,3})")

grepl(rex("crab" %if_prev_is% "apple"), "applecrab", perl = TRUE)

regmatches(temp, regexpr("(?<=^).*\\n$", temp, perl = T))










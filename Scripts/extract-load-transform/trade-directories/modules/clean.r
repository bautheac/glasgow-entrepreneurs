
# Variables ####

places <- c(
  "Arcade", "Arches", "Bridge", "Circus", "Close", "Court", "Crescent", "Drive",
  "Garden", "Lane", "Loan", "Place", "Port", "Quay", "Road", "Square", "Street",
  "Terrace", "Wynd"
)


# Functions ####

## Main ####

### Address ####

address_body <- function(raw){
  
  # Pre-clean
  clean <- address_pre_clean(raw)
  # Places
  clean <- address_clean_places(clean)
  # Names
  clean <- address_clean_names(clean)
  # Post clean
  clean <- address_post_clean(clean)
  
  clean
}

### Numbers ####
address_numbers <- function(address){
  
  clean <- gsub("(\\d{3})\\s?(\\d{3})", "\\1, \\2", address, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("(\\d)\\s?[,&]\\s?(\\d)", "\\1, \\2", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("(?<=\\d)\\.5", " 1/2", clean, ignore.case = TRUE, perl = TRUE)
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
  
  clean <- gsub("»", "", x, ignore.case = TRUE, perl = TRUE)
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
  # M' to Mc
  clean <- address_clean_mac(clean)
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
  
  clean <- gsub("M\\s?['c](?=\\w)", "Mac", address, ignore.case = FALSE, perl = TRUE)
  
  clean
}


#### Saints ####
address_clean_saints <- function(address){
  
  clean <- gsub(
    paste0(
      "(?<=^|s\\.\\s|st\\.\\s|street\\.\\s|west\\s|terrace\\.\\s|place\\.\\s)",
      "\\bst\\.?\\s+",
      "(?=en|[ae]n[dno]|da|[gp]e[ot]|[jm][ao][hmrs]|mu[nr]|[nv]i[cn]|ro)"
    ), 
    "Saint ", 
    address, ignore.case = TRUE, perl = TRUE
  )
  
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
  # clean <- stringr::str_squish(address)
  clean <- gsub("^\\s+", "", address, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("\\s{2,}", " ", clean, ignore.case = TRUE, perl = TRUE)
  
  # Place period at the end of address if none
  clean <- gsub("[,'.\\s]+$", "\\.", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("([^.])$", "\\1\\.", clean, ignore.case = TRUE, perl = TRUE)
  
  clean
}

#### Attached words ####
address_clean_attached_words <- function(address){
  
  # If two words are only separated by a period, replace period with white space
  clean <- gsub(
    "(\\b\\w+\\b)\\.(\\b\\w+\\b)", "\\1 \\2", address, ignore.case = TRUE, perl = TRUE
  )
  
  clean
}

#### Places ####
address_clean_places <- function(address){
  
  # Arcade
  clean <- gsub("\\barc?(?:ea)?(?:ade)?\\b\\.?", "Arcade", address, ignore.case = TRUE, perl = TRUE)
  # clean <- gsub("\\barc?(?:ade)\\b\\.?", "Arcade", address, ignore.case = TRUE, perl = TRUE)
  # Arches
  clean <- gsub("\\barch(?:es)?\\b\\.?", "Arches", clean, ignore.case = TRUE, perl = TRUE)
  # Bridge
  clean <- gsub("\\bbridge\\b\\.?", "Bridge", clean, ignore.case = TRUE, perl = TRUE)
  # Circus
  clean <- gsub("\\bcir(?:cus)?\\b\\.?", "Circus", clean, ignore.case = TRUE, perl = TRUE)
  # Close
  clean <- gsub("\\bclo(?:se)?\\b\\.?", "Close", clean, ignore.case = TRUE, perl = TRUE)
  # Court
  clean <- gsub("\\bco?(?:ur)?t?\\b,?\\.?", "Court", clean, ignore.case = TRUE, perl = TRUE)
  # Crescent
  clean <- gsub("\\bcre?s?c?(?:en)?t?\\b\\.?", "Crescent", clean, ignore.case = TRUE, perl = TRUE)
  # Drive
  clean <- gsub("\\bdri?v?e?\\b\\.?", "Drive", clean, ignore.case = TRUE, perl = TRUE)
  # Garden
  clean <- gsub("\\bgarden\\b\\.?", "Garden", clean, ignore.case = TRUE, perl = TRUE)
  # Lane
  clean <- gsub("\\b[Il][an](?:ne)?\\b\\.?", "Lane", clean, ignore.case = TRUE, perl = TRUE)
  # Loan
  # clean <- gsub("\\b[IL]o?a?n?\\.?", "Loan", clean, ignore.case = TRUE, perl = TRUE)
  # clean <- gsub("(\\w)loan\\.?", "\\1 Loan", clean, ignore.case = TRUE, perl = TRUE)
  # Place
  clean <- gsub("\\bp[il]a?(?:ce)?\\b\\.?", "Place", clean, ignore.case = TRUE, perl = TRUE)
  # Port
  clean <- gsub("\\bp(?:or)?t?\\b[,.]?", "Port", clean, ignore.case = TRUE, perl = TRUE)
  # Quay 
  clean <- gsub("\\bq[uy]?[oa]?y?\\b\\.?", "Quay", clean, ignore.case = TRUE, perl = TRUE)
  # Road
  # clean <- gsub("\\.rd\\.?", " Road", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("(?<=Garscube|Paisley)rd\\.?", " Road", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("\\b[ir](?:oa)?d\\b\\.?", "Road", clean, ignore.case = TRUE, perl = TRUE)
  # Square
  clean <- gsub("\\bsq[nu]?(?:are)?\\b\\.?", "Square", clean, ignore.case = TRUE, perl = TRUE)
  # Street
  # clean <- gsub("\\.st(?:reet)?\\.?", " Street", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("(?<=Dale|Hope|John|Kirk|Little|New)st(?:reet)?\\.?", " Street", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("(?<!^)\\b[iaes]t[ri]?[ie]?(?:et)?\\b\\.?", "Street", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("\\bstr[e']+t\\b\\.?", "Street", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("(street)(?=[a-z])", "\\1 ", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("\\b(street)\\b\\S?(?=\\s)", "\\1", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("(\\w)\\1st", "\\1\\1 Street", clean, ignore.case = TRUE, perl = TRUE)
  # clean <- gsub("(\\w)street\\.?", "\\1 Street", clean, ignore.case = TRUE, perl = TRUE)
  # Terrace
  clean <- gsub("\\bter(?:race)?\\b\\.?", "Terrace", clean, ignore.case = TRUE, perl = TRUE)
  # Wynd
  clean <- gsub("\\bwy?n?d?\\b\\.?", "Wynd", clean, ignore.case = TRUE, perl = TRUE)
  
  clean <- gsub("((?<!Mil|Mill|B))(road|street)\\.?", "\\1 \\2", clean, ignore.case = TRUE, perl = TRUE)
  
  clean
}

#### Suffixes ####
address_clean_suffixes <- function(address){
  
  # Cardinals
  ## North
  clean <- gsub("\\bNo?(?:rth)?\\b\\.?", "North", address, ignore.case = TRUE, perl = TRUE)
  ## South
  clean <- gsub("(?<!\\')\\bSo?u?(?:th)?\\b\\.?", "South", clean, ignore.case = TRUE, perl = TRUE)
  ## East
  clean <- gsub("\\bWe?(?:st)?\\b\\.?", "West", clean, ignore.case = TRUE, perl = TRUE)
  ## West
  clean <- gsub("(?<!')\\bEa?(?:st)?\\b\\.?", "East", clean, ignore.case = TRUE, perl = TRUE)
  
  # Others
  ## Main
  clean <- gsub("\\bM(?:ai)?ns?\\b\\.?", "Main", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("\\b(main)([a-z]\\.?)", "\\1 \\2", clean, ignore.case = TRUE, perl = TRUE)
  ## Great 
  clean <- gsub("\\bGr?(?:ea)?t?\\b\\.?", "Great", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("\\bC\\.(?=\\sclyde)", "Great", clean, ignore.case = TRUE, perl = TRUE)
  ## Upper
  clean <- gsub("\\bUp?p?e?r?\\b\\.?", "Upper", clean, ignore.case = TRUE, perl = TRUE)
  
  clean
}

#### Names ####
address_clean_names <- function(address){
  
  # Abbotsford
  # clean <- gsub("\\bAbbotsf?o?r?d?\\b\\.?(?=\\sP)", "Abbotsford", address, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("\\bAbbotsf?o?r?d?\\b\\.?", "Abbotsford", address, ignore.case = TRUE, perl = TRUE)
  # Abercorn
  clean <- gsub("\\bAber(?:corn)?\\b\\.?", "Abercorn", clean, ignore.case = TRUE, perl = TRUE)
  # Abercromby
  clean <- gsub("\\bAbercr?o?m?(?:by)?\\b\\.?", "Abercromby",clean, ignore.case = TRUE, perl = TRUE)
  # Anderston
  # clean <- gsub("\\bAnd?e?r?s?t?o?n?\\b\\.?", "Anderston", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("\\bAn((?!n\\s))(?(1)d?e?r?s?t?o?n?\\b)\\.?", "Anderston", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("\\bAnderstreet\\b\\.?", "Anderston", clean, ignore.case = TRUE, perl = TRUE)
  # Andrew
  clean <- gsub("\\bAndr?e?w[,.]?", "Andrew", clean, ignore.case = TRUE, perl = TRUE)
  # Annfield
  clean <- gsub("\\bAnnfi(?:eld)?\\b\\.?", "Annfield", clean, ignore.case = TRUE, perl = TRUE)
  # Argyle
  clean <- gsub("\\bArg[vy]?[l']?e?\\b\\.?", "Argyle", clean, ignore.case = TRUE, perl = TRUE)
  # Avondale
  clean <- gsub("(Avondale\\splace)", "\\1", clean, ignore.case = TRUE, perl = TRUE)
  # Ayton street, off Blackfriars street
  # clean <- gsub("(Ayton\\s(street|court|place))", "\\1", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("\\bAyton\\s+([a-z]+).+Blackfriar.+", "Ayton \\1, off Blackfriars Street", clean, ignore.case = TRUE, perl = TRUE)
  # Baird
  clean <- gsub("(Baird\\sStreet)(.*(glebe|westmuir).*)?.*", "\\1\\2", clean, ignore.case = TRUE, perl = TRUE)
  # Balmanno
  clean <- gsub("\\bBa[hl][mn]anno", "Balmanno", clean, ignore.case = TRUE, perl = TRUE)
  # Barony Glebe
  clean <- gsub("\\b(?<=Barony\\s)gl\\b\\.?", "Glebe", clean, ignore.case = TRUE, perl = TRUE)
  # Bell Street (Calton)
  clean <- gsub("\\bca[rl]ton\\b\\.?", "Calton", clean, ignore.case = TRUE, perl = TRUE)
  # Bishopbriggs
  clean <- gsub("\\bBishopb(?:riggs)?\\b\\.?", "Bishopbriggs", clean, ignore.case = TRUE, perl = TRUE)
  # Blackfriars
  clean <- gsub("\\bBlackfr?(?:iars)?\\b\\'?\\.?", "Blackfriars", clean, ignore.case = TRUE, perl = TRUE)
  # Blythswood
  clean <- gsub("\\bBlythsw?(?:ood)?\\b\\.?", "Blythswood", clean, ignore.case = TRUE, perl = TRUE)
  # Bothwell
  clean <- gsub("both\\s?w?(?:ell)?", "Bothwell", clean, ignore.case = TRUE, perl = TRUE)
  # Bridgegate
  # clean <- gsub("\\bBri?d?g?(?:eg)?a?t?e?\\b\\.?", "Bridgegate", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("\\bBridge(g)(?(1)(?:eg)?a?t?e?\\b)\\.?", "Bridgegate", clean, ignore.case = TRUE, perl = TRUE)
  # Bridgeton
  # clean <- gsub("\\bBr(?:id)?g?e?t?o?n?\\b\\.?", "Bridgeton", clean, ignore.case = TRUE, perl = TRUE)
  # clean <- gsub("\\bBridge(t)(?(1)(?:on)?\\b)\\.?", "Bridgeton", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("\\bBri?d?g?e?([tn])(?(1)(?:on)?\\b)\\.?", "Bridgeton", clean, ignore.case = TRUE, perl = TRUE)
  # Bromielaw
  # clean <- gsub("\\bBroo?m([^h]?)(?(1)[et]?\\s?(?:law)?\\b)\\.?", "Broomielaw", clean, ignore.case = TRUE, perl = TRUE)
  # Brooinielaw
  clean <- gsub("\\bBroo?[mi]n?([^h]?)(?(1)(?:[eit]+)?\\s?(?:law)?\\b)\\.?", "Broomielaw", clean, ignore.case = TRUE, perl = TRUE)
  # Brunswick
  clean <- gsub("\\bBrunsw?(?:ic)?k?\\b\\.?", "Brunswick", clean, ignore.case = TRUE, perl = TRUE)
  # Buccleuch
  clean <- gsub("\\bBuccl?(?:euch)?\\b\\.?", "Buccleuch", clean, ignore.case = TRUE, perl = TRUE)
  # Buchanan
  clean <- gsub("\\bBuch((?!an street))(?(1)a?n?(?:an)?\\b),?\\.?", "Buchanan", clean, ignore.case = TRUE, perl = TRUE)
  # Calton
  clean <- gsub("\\bCa?lt?o?n?\\b\\.?", "Calton", clean, ignore.case = TRUE, perl = TRUE)
  # Caltonmouth
  clean <- gsub("\\bCaltonmouth\\b\\.?", "Calton-mouth", clean, ignore.case = TRUE, perl = TRUE)
  # Cambridge
  clean <- gsub("\\bCa(?:m|in)br?(?:id)?g?e?\\b\\.?", "Cambridge", clean, ignore.case = TRUE, perl = TRUE)
  # Campbell 
  clean <- gsub("\\bCampb?e?l?l?\\b\\.?", "Cambridge", clean, ignore.case = TRUE, perl = TRUE)
  # Canning
  clean <- gsub("\\bCann?(?:ing)?\\b\\.?", "Canning", clean, ignore.case = TRUE, perl = TRUE)
  # Candleriggs
  clean <- gsub("\\bCandler?i?[gs]?[gs]?[gs]?\\b-?\\.?", "Candleriggs", clean, ignore.case = TRUE, perl = TRUE)
  # Carlton
  clean <- gsub("\\bCarlt?o?n?\\b\\.?", "Carlton", clean, ignore.case = TRUE, perl = TRUE)
  # Carnarvon
  clean <- gsub("\\bCarnarv(?:on)?\\b[.,]?", "Carnarvon", clean, ignore.case = TRUE, perl = TRUE)
  # Cathedral
  clean <- gsub("\\bCathedr?(?:al)?\\b\\.?", "Cathedral", clean, ignore.case = TRUE, perl = TRUE)
  # Cavendish
  clean <- gsub("\\bCavend(?:ish)?\\b\\.?", "Cathedral", clean, ignore.case = TRUE, perl = TRUE)
  # Charlotte
  clean <- gsub("\\bCha[rd](?:lotte)?\\b\\.?", "Charlotte", clean, ignore.case = TRUE, perl = TRUE)
  # Cheapside 
  clean <- gsub("\\bCheaps(?:ide)?\\b\\.?", "Cheapside", clean, ignore.case = TRUE, perl = TRUE)
  # Claremont 
  clean <- gsub("\\bClarem(?:on)?t?\\b\\.?", "Claremont", clean, ignore.case = TRUE, perl = TRUE)
  # Commerce
  clean <- gsub("\\bComm?(?:er)?c?e?\\b\\.?", "Commerce", clean, ignore.case = TRUE, perl = TRUE)
  # Cowcaddens
  clean <- gsub("\\bCow[ces]?a?d?[dns]?(?:en)?s?\\b\\.?", "Cowcaddens", clean, ignore.case = TRUE, perl = TRUE)
  # Crown Point
  clean <- gsub("\\bCrown\\s?p(?:oint)?\\b\\.?", "Crown Point", clean, ignore.case = TRUE, perl = TRUE)
  # Cumberland
  clean <- gsub("\\bCumb(?:er)?l?a?n?d?\\b\\.?", "Cumberland", clean, ignore.case = TRUE, perl = TRUE)
  # Dalhousie
  clean <- gsub("\\bDalh?(?:ousie)?\\b\\.?", "Dalhousie", clean, ignore.case = TRUE, perl = TRUE)
  # Dalmarnock
  clean <- gsub("\\bDalm?(?:ar)?n?(?:oc)?k?\\b\\.?", "Dalmarnock", clean, ignore.case = TRUE, perl = TRUE)
  # Dobbies Loan
  # clean <- gsub("\\bD[eo]b(?:bie)?'?s?\\.?", "Dobbies", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("\\bD[eo]b(?:bie)?'?s?\\.?\\s?[il]?o?a?n?\\b\\.?", "Dobbies Loan", clean, ignore.case = TRUE, perl = TRUE)
  # Douglas
  clean <- gsub("\\bDoug(?:las)?\\b\\.?", "Douglas", clean, ignore.case = TRUE, perl = TRUE)
  # Dovehill
  clean <- gsub("\\bDove\\s?hill\\b\\.?", "Dovehill", clean, ignore.case = TRUE, perl = TRUE)
  # Dundas
  clean <- gsub("\\bDun?d?(?:as)?\\b\\.?", "Dundas", clean, ignore.case = TRUE, perl = TRUE)
  # Dumbarton
  clean <- gsub("\\bDumb?(?:ar)?t?o?n?\\b\\.?", "Dumbarton", clean, ignore.case = TRUE, perl = TRUE)
  # Dunlop
  clean <- gsub("\\bDunl?(?:op)?\\b\\.?", "Dunlop", clean, ignore.case = TRUE, perl = TRUE)
  # Eglinton
  clean <- gsub("\\bEglint?(?:on)?\\b\\.?", "Eglinton", clean, ignore.case = TRUE, perl = TRUE)
  # Elmbank
  clean <- gsub("\\bElm\\s?bank\\b\\.?", "Elmbank", clean, ignore.case = TRUE, perl = TRUE)
  # Enoch
  clean <- gsub("\\bEn(?:och)?\\b\\.?", "Enoch", clean, ignore.case = TRUE, perl = TRUE)
  # Fish market
  clean <- gsub("\\bFish\\s?market\\b\\.?", "Fish market", clean, ignore.case = TRUE, perl = TRUE)
  # Frederick
  clean <- gsub("\\bFred(?:er)?(?:ic)?k?\\b\\.?", "Frederick", clean, ignore.case = TRUE, perl = TRUE)
  # Gallowgate
  clean <- gsub("\\bGa[il]l?(?:owg)?a?[t-]?e?\\b-?\\.?", "Gallowgate", clean, ignore.case = TRUE, perl = TRUE)
  # Garnethill
  clean <- gsub("\\bGa?(?:m|rn)?e?t?\\s?h?(?:ill)?\\b\\.?", "Garnethill", clean, ignore.case = TRUE, perl = TRUE)
  # Garngad
  clean <- gsub("\\bGarng(?:ad)?\\b\\.?", "Garngad", clean, ignore.case = TRUE, perl = TRUE)
  # Garscadden
  clean <- gsub("\\bGarsc?(?:ad)?(?:den)?\\b\\.?", "Garscadden", clean, ignore.case = TRUE, perl = TRUE)
  # Garscube
  clean <- gsub("\\bGarsc[un]?b?e?\\b\\.?", "Garscube", clean, ignore.case = TRUE, perl = TRUE)
  # George
  clean <- gsub("\\bGeo(?:rg)?e?\\.?", "George", clean, ignore.case = TRUE, perl = TRUE)
  # George Street
  clean <- gsub("\\bGeorgest\\b\\.?", "George Street", clean, ignore.case = TRUE, perl = TRUE)
  # Gorbals
  clean <- gsub("\\bGorb?a?[il]?s?\\b\\.?", "Gorbals", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("\\b(Gorbals).?\\b", "\\1", clean, ignore.case = TRUE, perl = TRUE)
  # Govanhill
  clean <- gsub("\\bGov((?!an street))(?(1)a?n?(?:hill)?\\b),?\\.?", "Govanhill", clean, ignore.case = TRUE, perl = TRUE)
  # Glassford
  clean <- gsub("\\bGlassf(?:or)?d?\\b\\.?", "Glassford", clean, ignore.case = TRUE, perl = TRUE)
  # Gloucester
  clean <- gsub("\\bGlouc(?:es)?(?:ter)?\\b\\.?", "Gloucester", clean, ignore.case = TRUE, perl = TRUE)
  # Gordon
  clean <- gsub("\\bGord(?:on)?\\b\\.?", "Gordon", clean, ignore.case = TRUE, perl = TRUE)
  # Graeme
  clean <- gsub("\\bGra[em]?(?:me)?\\b\\.?", "Graeme", clean, ignore.case = TRUE, perl = TRUE)
  # Greenhead
  clean <- gsub("\\bGreenh?(?:ead)?\\b\\.?", "Greenhead", clean, ignore.case = TRUE, perl = TRUE)
  # Hamilton
  clean <- gsub("\\bHam(?:il)?t?(?:on)?\\b\\.?", "Hamilton", clean, ignore.case = TRUE, perl = TRUE)
  # Hillhead
  clean <- gsub("\\bHillh(?:ead)?\\b\\.?", "Hillhead", clean, ignore.case = TRUE, perl = TRUE)
  # Holmhead
  clean <- gsub("\\bHolm((?!\\sstreet))(?(1)[bh]?(?:ead)?\\b)\\.?", "Holmhead", clean, ignore.case = TRUE, perl = TRUE)
  # Holyrood
  clean <- gsub("\\bHolyr(?:ood)?\\b\\.?", "Holyrood", clean, ignore.case = TRUE, perl = TRUE)
  # Hopetoun
  clean <- gsub("\\bHopet(?:ou)?n\\b\\.?", "Hopetoun", clean, ignore.case = TRUE, perl = TRUE)
  # Hospital
  clean <- gsub("\\bHosp(?:ital)?\\b\\.?", "Hospital", clean, ignore.case = TRUE, perl = TRUE)
  # Howard
  clean <- gsub("\\bHowa?r?d?\\b\\.?", "Howard", clean, ignore.case = TRUE, perl = TRUE)
  # Hutcheson 
  clean <- gsub("\\bHut[ce]?h?e?s?o?n?\\b\\.?", "Hutcheson", clean, ignore.case = TRUE, perl = TRUE)
  # Hutchesontown
  clean <- gsub("\\bHutchesont?(?:ow)?n\\b\\.?", "Hutchesontown", clean, ignore.case = TRUE, perl = TRUE)
  # Hydepark
  clean <- gsub("\\bHyde\\s?park\\b\\.?", "Hydepark", clean, ignore.case = TRUE, perl = TRUE)
  # Jamaica
  clean <- gsub("\\bJama(?:ica)?\\b\\.?", "Jamaica", clean, ignore.case = TRUE, perl = TRUE)
  # James
  clean <- gsub("\\bJa(?:me)?s\\b[\\.']?", "James", clean, ignore.case = TRUE, perl = TRUE)
  # John Street, Bridgeton
  clean <- gsub("\\bJohn Street[,\\s]+Bridge?t?o?n?.*", "John Street, Bridgeton", clean, ignore.case = TRUE, perl = TRUE)
  # Kelvingrove
  clean <- gsub("\\bKelvingr(?:ove)?\\b\\.?", "Kelvingrove", clean, ignore.case = TRUE, perl = TRUE)
  # Kingston
  clean <- gsub("\\bKingsto?n?\\b\\.?", "Kingston", clean, ignore.case = TRUE, perl = TRUE)
  # Kelvinhaugh
  clean <- gsub("\\bKelvinha(?:ugh)?\\b\\.?", "Kelvinhaugh", clean, ignore.case = TRUE, perl = TRUE)
  # Kennedy 
  clean <- gsub("\\bKenn(?:edy)?\\b\\.?", "Kennedy", clean, ignore.case = TRUE, perl = TRUE)
  # Kinning
  clean <- gsub("\\bKinn(?:ing)?\\b\\.?", "Kinning", clean, ignore.case = TRUE, perl = TRUE)
  # Lancefield
  clean <- gsub("\\bLo?ance(?:field)?\\b\\.?", "Lancefield", clean, ignore.case = TRUE, perl = TRUE)
  # Lansdowne
  clean <- gsub("\\bLansd(?:owne)?\\b\\.?", "Lansdowne", clean, ignore.case = TRUE, perl = TRUE)
  # Little Dovehill
  clean <- gsub("\\bLit(?:tle)?\\.?\\s?Dovehill\\b\\.?", "Little Dovehill",clean, ignore.case = TRUE, perl = TRUE)
  # Little Hamilton Street
  clean <- gsub("\\bL(?:ittle)?\\.?\\s?Hamilton Street\\b\\.?", "Little Hamilton Street",clean, ignore.case = TRUE, perl = TRUE)
  # London
  clean <- gsub("\\bLond?(?:on)?\\b\\.?", "London", clean, ignore.case = TRUE, perl = TRUE)
  # Main street
  ## Anderson
  clean <- gsub("\\bMain street[,\\s]+And?e?r?s?t?o?n?\\.?.*", "Main Street, Anderston", clean, ignore.case = TRUE, perl = TRUE)
  ## Bridgeton
  clean <- gsub("\\bMain street[,\\s]+Br(?:id)?g?e?t?o?n?\\.?.*", "Main Street, Bridgeton", clean, ignore.case = TRUE, perl = TRUE)
  # Maitland 
  clean <- gsub("\\bMa[il]tl(?:an)?d?\\b\\.?", "Maitland", clean, ignore.case = TRUE, perl = TRUE)
  # Margaret
  clean <- gsub("\\bMarg(?:aret)?\\b\\.?", "Margaret", clean, ignore.case = TRUE, perl = TRUE)
  # Marlborough
  clean <- gsub("\\bMarlboro(?:ugh)?\\b\\.?", "Marlborough", clean, ignore.case = TRUE, perl = TRUE)
  # Maxwell
  clean <- gsub("\\bMaxwe?l?l?\\b\\.?", "Maxwell", clean, ignore.case = TRUE, perl = TRUE)
  # Maxweltown
  clean <- gsub("\\bMaxweltow?n\\b\\.?", "Maxweltown", clean, ignore.case = TRUE, perl = TRUE)
  # Merchant
  clean <- gsub("\\bMerch(?:an)?t?\\b\\.?", "Merchant", clean, ignore.case = TRUE, perl = TRUE)
  # Milton
  clean <- gsub("\\Milt(?:on)?\\b\\.?", "Milton", clean, ignore.case = TRUE, perl = TRUE)
  # Moore
  clean <- gsub("\\bMoore?\\b\\.?", "Moore", clean, ignore.case = TRUE, perl = TRUE)
  # Muslin Street
  clean <- gsub("\\bMuslin\\s+Street[,\\s]+Br.+", "Muslin Street, Bridgeton", clean, ignore.case = TRUE, perl = TRUE)
  # Nelson Street
  clean <- gsub("\\bNelson\\b['\\s]+Street\\.?", "Nelson Street", clean, ignore.case = TRUE, perl = TRUE)
  # New city Road
  clean <- gsub("\\bNew\\s+Cit[vy]\\s+Road\\.?", "New City Road", clean, ignore.case = TRUE, perl = TRUE)
  # New vennel
  clean <- gsub("\\bNew\\s?ven(?:nel)?\\b\\.?", "New vennel", clean, ignore.case = TRUE, perl = TRUE)
  # Newton
  clean <- gsub("\\bNewt(?:on)?\\b\\.?", "Newton", clean, ignore.case = TRUE, perl = TRUE)
  # New Wynd
  clean <- gsub("\\bNew\\s?wy(?:nd)?\\b\\.?", "New Wynd", clean, ignore.case = TRUE, perl = TRUE)
  # Nicholson
  clean <- gsub("\\bNich?ols(?:on)?\\b\\.?", "Nicholson", clean, ignore.case = TRUE, perl = TRUE)
  # Norfolk
  clean <- gsub("\\bNorf(?:ol)?k\\b\\.?", "Norfolk", clean, ignore.case = TRUE, perl = TRUE)
  # North Court, Royal Exchange
  clean <- gsub("North\\s+Courts?.+R.+\\sE.+", "North Court, Royal Exchange", clean, ignore.case = TRUE, perl = TRUE)
  # North Dalmarnock Road
  clean <- gsub("North\\s+Dal.+arno.+\\sRoad\\.?", "North Dalmarnock Road", clean, ignore.case = TRUE, perl = TRUE)
  # North Hanover Street
  clean <- gsub("North\\s+\\bHano?v?(?:er)?\\b\\.?(?:.+Street)?", "North Hanover Street", clean, ignore.case = TRUE, perl = TRUE)
  # North Portland Street
  clean <- gsub("North\\s+\\bPortl?v?(?:and)?\\b\\.?(?:.+Street)?", "North Portland Street", clean, ignore.case = TRUE, perl = TRUE)
  # Oswald
  clean <- gsub("Oswa?ld", "Oswald", clean, ignore.case = TRUE, perl = TRUE)
  # Oxford
  clean <- gsub("\\bOxf(?:or)?d?\\b\\.?", "Oxford", clean, ignore.case = TRUE, perl = TRUE)
  # Paisley 
  clean <- gsub("\\bPais?(?:ley)?\\b'?\\.?", "Paisley", clean, ignore.case = TRUE, perl = TRUE)
  # Parliamentary
  clean <- gsub("\\bPa[ir]l?i?a?(?:m|rn)?e?n?t?a?r?y??\\b\\.?(?=\\s+Road)", "Parliamentary", clean, ignore.case = TRUE, perl = TRUE)
  # Parkholm
  clean <- gsub("\\bParkgro\\b\\.?", "Parkholm,", clean, ignore.case = TRUE, perl = TRUE)
  # Parkhouse
  clean <- gsub("\\bParkho(?:use)?\\b\\.?", "Parkhouse", clean, ignore.case = TRUE, perl = TRUE)
  # Partick
  clean <- gsub("\\bPart(?:ic)?k?\\b\\.?", "Partick", clean, ignore.case = TRUE, perl = TRUE)
  # Paterson
  clean <- gsub("\\bPaters(?:on)?\\b\\.?", "Paterson", clean, ignore.case = TRUE, perl = TRUE)
  # Pollokshaws Road
  clean <- gsub("\\bPolloksh?a?w?s?\\b\\.?(?=\\s+Road)", "Pollokshaws", clean, ignore.case = TRUE, perl = TRUE)
  # Port-Dundas Road
  clean <- gsub("\\bPort\\b.+Dundas.+Road\\.?", "Port-Dundas Road", clean, ignore.case = TRUE, perl = TRUE)
  # Port-Someplace
  clean <- gsub("\\bPort[,\\s-]\\s?(\\w)", "Port-\\1", clean, ignore.case = TRUE, perl = TRUE)
  # Port Street
  clean <- gsub("\\bPort\\b[\\s-]\\bStreet\\b\\.?", "Port Street", clean, ignore.case = TRUE, perl = TRUE)
  # Portland
  clean <- gsub("\\bPort[il]?a?[ns]?d?\\b\\.?", "Portland", clean, ignore.case = TRUE, perl = TRUE)
  # Princes something
  clean <- gsub("\\bPrince'?s'?[ –]+(\\w+)", "Princes \\1", clean, ignore.case = TRUE, perl = TRUE)
  # Queens something
  clean <- gsub("\\bQueen'?s'?[ –]+(\\w+)", "Queens \\1", clean, ignore.case = TRUE, perl = TRUE)
  # Railway
  clean <- gsub("\\bRailw(?:ay)?\\b\\.?", "Railway", clean, ignore.case = TRUE, perl = TRUE)
  # Regent
  clean <- gsub("\\bReg(?:en)?t?\\b\\.?", "Regent", clean, ignore.case = TRUE, perl = TRUE)
  # Reid Street, Bridgeton
  clean <- gsub("\\bReid(?:st)?\\b.+(?:Street\\.?|Bridg.+)", "Reid Street, Bridgeton", clean, ignore.case = TRUE, perl = TRUE)
  # Renfield
  clean <- gsub("\\bRen[fl](?:ield)?\\b\\.?", "Renfield", clean, ignore.case = TRUE, perl = TRUE)
  # Renfrew 
  clean <- gsub("\\bRenfr?(?:ew)?\\b\\.?", "Renfrew", clean, ignore.case = TRUE, perl = TRUE)
  # Rollox
  clean <- gsub("\\bRol(?:lox)?\\b\\.?", "Rollox", clean, ignore.case = TRUE, perl = TRUE)
  # Robertson
  clean <- gsub("\\bRob(?:er)?t?s?(?:on)?\\b\\.?", "Robertson", clean, ignore.case = TRUE, perl = TRUE)
  # Rosebank, Garngad road
  clean <- gsub("\\bRosebank\\b.+$", "Rosebank, Garngad road", clean, ignore.case = TRUE, perl = TRUE)
  # Rose Street, Hutchesontown
  clean <- gsub("\\bRose\\s+Street.+Hutch.+", "Rose Street, Hutchesontown", clean, ignore.case = TRUE, perl = TRUE)
  # Rottenrow
  clean <- gsub("\\bRottenr(?:ow)?\\b", "Rottenrow", clean, ignore.case = TRUE, perl = TRUE)
  # Rumford Street, Bridgeton
  clean <- gsub("\\bR[un]mford\\s+Street[,\\s]+Bridg\\.?", "Rumford Street, Bridgeton", clean, ignore.case = TRUE, perl = TRUE)
  # Royal
  clean <- gsub("\\bRoy(?:al)?\\.?", "Royal", clean, ignore.case = TRUE, perl = TRUE)
  # Royal Exchange
  clean <- gsub("\\bR.+\\bExc?h?(?:an)?(?:ange)?\\b\\.?", "Royal Exchange", clean, ignore.case = TRUE, perl = TRUE)
  # Rutherglen Loan
  clean <- gsub("\\bRuther([^f])(?(1).+)", "Rutherglen Loan", clean, ignore.case = TRUE, perl = TRUE)
  # Saint James Street, Kingston
  clean <- gsub("^Saint\\s+James\\s+Street.+kin.+", "Saint James Street, Kingston", clean, ignore.case = TRUE, perl = TRUE)
  # Saint Rollox
  clean <- gsub("^Saint Rollox.+", "Saint Rollox", clean, ignore.case = TRUE, perl = TRUE)
  # Salisbury
  clean <- gsub("\\bSalisb(?:ury)?\\b\\.?", "Salisbury", clean, ignore.case = TRUE, perl = TRUE)
  # Saltmarket
  clean <- gsub("\\bSalt.?ma?r?k?e?t?\\b\\.?", "Saltmarket", clean, ignore.case = TRUE, perl = TRUE)
  # Sandyford
  clean <- gsub("\\bSandy?f?(?:ord)?\\b\\.?", "Sandyford", clean, ignore.case = TRUE, perl = TRUE)
  # Sauchiehall
  clean <- gsub("\\bSa[nu][ce][bhl]?i?i?e?[bh]?a?(?:ll)?\\b\\.?", "Sauchiehall", clean, ignore.case = TRUE, perl = TRUE)
  # Sauchiehall Street
  clean <- gsub("\\bSauch.+St.+", "Sauchiehall Street", clean, ignore.case = TRUE, perl = TRUE)
  # Shamrock
  clean <- gsub("\\bShamr?(?:ock)?\\b\\.?", "Shamrock", clean, ignore.case = TRUE, perl = TRUE)
  # Shawlands
  clean <- gsub("\\bShawl(?:an)?ds\\b\\.?", "Shawlands", clean, ignore.case = TRUE, perl = TRUE)
  # Shettleston
  clean <- gsub("\\bShettles(?:ton)?\\b\\.?", "Shettleston", clean, ignore.case = TRUE, perl = TRUE)
  # Somerville
  clean <- gsub("\\bSomm?erville\\b\\.?", "Somerville", clean, ignore.case = TRUE, perl = TRUE)
  # South side, Glasgow Harbour
  clean <- gsub("\\bSouth\\sside.+harb.+", "South side, Glasgow Harbour", clean, ignore.case = TRUE, perl = TRUE)
  # Spoutmouth
  clean <- gsub("\\bSp.?outmouth\\b", "Spoutmouth", clean, ignore.case = TRUE, perl = TRUE)
  # Springfield
  ## name
  clean <- gsub("\\bSpringf(?:iel)?d?\\b", "Springfield", clean, ignore.case = TRUE, perl = TRUE)
  ## suffixes
  clean <- gsub("\\bSpringfield\\b[,\\s]+(?!lane|court|place).+", "Springfield.", clean, ignore.case = TRUE, perl = TRUE)
  # Stirling 
  clean <- gsub("\\bStirl(?:ing)?\\b\\.?", "Stirling", clean, ignore.case = TRUE, perl = TRUE)
  # Stobcross
  clean <- gsub("\\bStobc?r?o?s?[as]?\\b\\.?", "Stobcross", clean, ignore.case = TRUE, perl = TRUE)
  # Stobcross Street
  clean <- gsub("\\bSt[osu]bc?r?o?s?[as]?\\b\\.?(?:\\s+Street|\\s+\\w+[,.$])?", "Stobcross Street", clean, ignore.case = TRUE, perl = TRUE)
  # Stockwell
  clean <- gsub("\\bStockw?e?l?l?(?:ell)?\\b\\.?", "Stockwell", clean, ignore.case = TRUE, perl = TRUE)
  # Struthers Street
  clean <- gsub("\\bStrutherss?t?\\b\\.?(?:\\s+street)?", "Struthers Street", clean, ignore.case = TRUE, perl = TRUE)
  # Tennent 
  clean <- gsub("\\bTenn?[ae]nt\\b\\.?", "Tennent", clean, ignore.case = TRUE, perl = TRUE)
  # Thistle
  clean <- gsub("\\bThis(?:tle)?\\b\\.?", "Thistle", clean, ignore.case = TRUE, perl = TRUE)
  # Townhead
  clean <- gsub("\\bTownh(?:ead)?\\b\\.?", "Townhead", clean, ignore.case = TRUE, perl = TRUE)
  # Townmill 
  clean <- gsub("\\bTowns?(?:\\s+)?mill\\b\\.?", "Townmill", clean, ignore.case = TRUE, perl = TRUE)
  # Tradeston
  clean <- gsub("\\bTr(?:ad)?e?s?t?o?n?\\b\\.?", "Tradeston", clean, ignore.case = TRUE, perl = TRUE)
  # Trongate
  clean <- gsub("\\bTrong(?:ate)?\\b\\.?", "Trongate", clean, ignore.case = TRUE, perl = TRUE)
  # Upperfauld
  clean <- gsub("\\bUpper(?:\\s+)?fauld\\b\\.?", "Upperfauld", clean, ignore.case = TRUE, perl = TRUE)
  # Victoria
  clean <- gsub("\\bVict(?:oria)?\\b\\.?", "Victoria", clean, ignore.case = TRUE, perl = TRUE)
  # Vincent
  clean <- gsub("\\bVin[ec]?e?n?t?\\b\\.?", "Vincent", clean, ignore.case = TRUE, perl = TRUE)
  # Violet grove
  clean <- gsub("\\bViolet\\s+gro(?:ve)?\\b\\.?", "Violet grove", clean, ignore.case = TRUE, perl = TRUE)
  # Virginia 
  clean <- gsub("\\bVirg(?:inia)?\\b\\.?", "Virginia", clean, ignore.case = TRUE, perl = TRUE)
  # Walmer Crescent
  clean <- gsub("\\bWalm.+cr.+\\b\\.?", "Walmer Crescent", clean, ignore.case = TRUE, perl = TRUE)
  # Warroch 
  clean <- gsub("\\bWa[nr]r?(?:och)?\\b\\.?", "Warroch", clean, ignore.case = TRUE, perl = TRUE)
  # Washington
  clean <- gsub("\\bWashingt(?:on)?\\b\\.?", "Washington", clean, ignore.case = TRUE, perl = TRUE)
  # Water
  clean <- gsub("\\bWater\\b-?\\.?", "Water", clean, ignore.case = TRUE, perl = TRUE)
  # Wellington 
  clean <- gsub("\\bWelling(?:on)?\\b\\.?", "Wellington", clean, ignore.case = TRUE, perl = TRUE)
  # Wemyss
  clean <- gsub("\\bWemyss?\\b\\.?", "Wemyss", clean, ignore.case = TRUE, perl = TRUE)
  # West Street, Tradeston
  clean <- gsub("\\bWest\\s+str.+(?:\\s+)?trad.+\\b\\.?", "West Street, Tradeston", clean, ignore.case = TRUE, perl = TRUE)
  # Willowbank
  clean <- gsub("\\bWillowb(?:an)?k?\\b\\.?", "Willowbank", clean, ignore.case = TRUE, perl = TRUE)
  # William
  clean <- gsub("\\bW(?:illia)?m\\b\\.?", "William", clean, ignore.case = TRUE, perl = TRUE)
  # Windsor
  clean <- gsub("\\bWin(?:dsor)?\\b\\.?(?:\\s+)?(place|Terrace|Street)?", "Windsor \\1", clean, ignore.case = TRUE, perl = TRUE)
  # Woodlands
  clean <- gsub("\\bWoodla(?:nds)?\\b\\.?", "Woodlands", clean, ignore.case = TRUE, perl = TRUE)
  # Woodside
  clean <- gsub("\\bWoodsi(?:de)?\\b\\.?", "Woodside", clean, ignore.case = TRUE, perl = TRUE)
  
  clean
}

#### Others ####
address_clean_others <- function(address){
  
  # Get rid of parasite postfixes
  ## (Agents)
  clean <- gsub("(.*)\\s\\(agents\\).?", "\\1", address, ignore.case = TRUE, perl = TRUE)
  ## (House)
  # clean <- gsub("(.*)\\s\\(house\\).?", "\\1", clean, ignore.case = TRUE, perl = TRUE)
  ## (Work)
  # clean <- gsub("(.*)\\s\\(work\\).?", "\\1", clean, ignore.case = TRUE, perl = TRUE)
  
  # Unwanted processing outcomes
  ## "street" processing sometimes outputs orpheans ", reet,". Replace with comma.
  clean <- gsub(", reet,", ",", clean, ignore.case = TRUE, perl = TRUE)
  
  # If placed between two words, replace period with a comma.
  clean <- gsub("\\b\\.\\s\\b", ", ", clean, ignore.case = TRUE, perl = TRUE
  )
  
  # If place not located at the end of the address, append a comma.
  clean <- gsub(
    paste0("\\b(", paste(places, collapse = "|"), ")\\b(?!$|[,-]|\\s+\\(|\\s+street)"), "\\1, ", 
    clean, ignore.case = TRUE, perl = TRUE
  )
  # If place not located at the beginning of the address and separated from previous
  # word with comma and space delete comma.
  clean <- gsub(
    paste0(",\\s+(", paste(places, collapse = "|"), ")"), " \\1", 
    clean, ignore.case = TRUE, perl = TRUE
  )
  
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
  
  # Abraham 
  clean <- gsub("\\bAbr(?:aham)?\\b\\.?", "Abraham", name, ignore.case = TRUE, perl = TRUE)
  # Alexander
  clean <- gsub("\\bAle[sx](?:ander)?\\b\\.?", "Alexander", clean, ignore.case = TRUE, perl = TRUE)
  # Andrew 
  clean <- gsub("\\bAnd(?:re)?w?\\b\\.?", "Andrew", clean, ignore.case = TRUE, perl = TRUE)
  # Angus
  clean <- gsub("\\bAng(?:us)?\\b\\.?", "Angus", clean, ignore.case = TRUE, perl = TRUE)
  # Archibald
  clean <- gsub("\\bArch?i?b?(?:al)?d?\\b\\.?", "Archibald", clean, ignore.case = TRUE, perl = TRUE)
  # Benjamin
  clean <- gsub("\\bBenj?(?:amin)?\\b\\.?", "Benjamin", clean, ignore.case = TRUE, perl = TRUE)
  # Bernard
  clean <- gsub("\\bBern?(?:ard)?\\b\\.?", "Bernard", clean, ignore.case = TRUE, perl = TRUE)
  # Brothers
  clean <- gsub("\\bBro(?:th)?(?:er)?s?\\b\\.?", "Brothers", clean, ignore.case = TRUE, perl = TRUE)
  # Campbell
  clean <- gsub("\\bCamp?(?:bell)?\\b\\.?", "Campbell", clean, ignore.case = TRUE, perl = TRUE)
  # Catherine
  clean <- gsub("\\bCat(?:h|li)(?:erine)?\\b\\.?", "Catherine", clean, ignore.case = TRUE, perl = TRUE)
  # Charles 
  clean <- gsub("\\bCha?(?:rle)?s?\\b\\.?", "Charles", clean, ignore.case = TRUE, perl = TRUE)
  # Christopher
  clean <- gsub("\\bChristo?(?:pher)?\\b\\.?", "Christopher", clean, ignore.case = TRUE, perl = TRUE)
  # Cornelius
  clean <- gsub("\\bCor?(?:nelius)?\\b\\.?", "Cornelius", clean, ignore.case = TRUE, perl = TRUE)
  # Daniel 
  clean <- gsub("\\bDan(?:iel)?\\b\\.?", "Daniel", clean, ignore.case = TRUE, perl = TRUE)
  # Dominic 
  clean <- gsub("\\bDom(?:inic)?\\b\\.?", "Dominic", clean, ignore.case = TRUE, perl = TRUE)
  # Donald
  clean <- gsub("\\bDon(?:ald)?\\b\\.?", "Donald", clean, ignore.case = TRUE, perl = TRUE)
  # Dugald 
  clean <- gsub("\\bDug(?:ald)?\\b\\.?", "Dugald", clean, ignore.case = TRUE, perl = TRUE)
  # Duncan 
  clean <- gsub("\\bDunc?(?:an)?\\b\\.?", "Duncan", clean, ignore.case = TRUE, perl = TRUE)
  # Ebenezer
  clean <- gsub("\\bEben(?:ezer)?\\b\\.?", "Ebenezer", clean, ignore.case = TRUE, perl = TRUE)
  # Edmund
  clean <- gsub("\\bEdm(?:und)?\\b\\.?", "Edmund", clean, ignore.case = TRUE, perl = TRUE)
  # Edward
  clean <- gsub("\\bEdw(?:ar)?d?\\b\\.?", "Edward", clean, ignore.case = TRUE, perl = TRUE)
  # Elizabeth
  clean <- gsub("\\bEliza?b?(?:eth)?\\b\\.?", "Elizabeth", clean, ignore.case = TRUE, perl = TRUE)
  # Francis 
  clean <- gsub("\\bFran(?:cis)?\\b\\.?", "Francis", clean, ignore.case = TRUE, perl = TRUE)
  # Frederick
  clean <- gsub("\\bFred(?:eric)?k?\\b\\.?", "Frederick", clean, ignore.case = TRUE, perl = TRUE)
  # George
  clean <- gsub("\\bGeo(?:rge)?\\b\\.?", "George", clean, ignore.case = TRUE, perl = TRUE)
  # Gilbert
  clean <- gsub("\\bGil(?:bert)?\\b\\.?", "Gilbert", clean, ignore.case = TRUE, perl = TRUE)
  # Godfrey
  clean <- gsub("\\bGodf(?:rey)?\\b\\.?", "Godfrey", clean, ignore.case = TRUE, perl = TRUE)
  # Harry
  clean <- gsub("\\bHar(?:ry)?\\b\\.?", "Harry", clean, ignore.case = TRUE, perl = TRUE)
  # Henry
  clean <- gsub("\\bH(?:enr)?y?\\b\\.?", "Henry", clean, ignore.case = TRUE, perl = TRUE)
  # Hugh 
  clean <- gsub("\\bHu(?:gh)?\\b\\.?", "Hugh", clean, ignore.case = TRUE, perl = TRUE)
  # James 
  clean <- gsub("\\bJa?(?:me)?s\\b\\.?", "James", clean, ignore.case = TRUE, perl = TRUE)
  # John 
  clean <- gsub("\\bJ(?:[no]h)?(?:no|[nou])\\b\\.?", "John", clean, ignore.case = TRUE, perl = TRUE)
  # Joseph
  clean <- gsub("\\bJos(?:eph)?\\b\\.?", "Joseph", clean, ignore.case = TRUE, perl = TRUE)
  # Joshua
  clean <- gsub("\\bJosh(?:ua)?\\b\\.?", "Joshua", clean, ignore.case = TRUE, perl = TRUE)
  # Kenneth
  clean <- gsub("\\bKen(neth)?\\b(?(1)|\\.)", "Kenneth", clean, ignore.case = TRUE, perl = TRUE)
  # Lawrence
  clean <- gsub("\\bLawr(?:ence)?\\b\\.?", "Lawrence", clean, ignore.case = TRUE, perl = TRUE)
  # Malcolm 
  clean <- gsub("\\Malc?(?:om)?\\b\\.?", "Malcolm", clean, ignore.case = TRUE, perl = TRUE)
  # Margaret
  clean <- gsub("\\bMarg(?:are)?t?\\b\\.?", "Margaret", clean, ignore.case = TRUE, perl = TRUE)
  # Mathew
  clean <- gsub("\\bMat(hew)?\\b(?(1)|\\.)", "Mathew", clean, ignore.case = TRUE, perl = TRUE)
  # Matthew
  clean <- gsub("\\bMatt(?:hew)?\\b\\.?", "Matthew", clean, ignore.case = TRUE, perl = TRUE)
  # Michael
  clean <- gsub("\\bMich(?:ael)?\\b\\.?", "Michael", clean, ignore.case = TRUE, perl = TRUE)
  # Miss
  clean <- gsub("\\bMiss?\\b\\.?", "Miss", clean, ignore.case = TRUE, perl = TRUE)
  # Norman
  clean <- gsub("\\bNorm?(?:an)?\\b\\.?", "Norman", clean, ignore.case = TRUE, perl = TRUE)
  # Patrick
  clean <- gsub("\\bPat(?:ric)?k\\b\\.?", "Patrick", clean, ignore.case = TRUE, perl = TRUE)
  # Peter 
  clean <- gsub("\\bPet(?:er)?\\b\\.?", "Peter", clean, ignore.case = TRUE, perl = TRUE)
  # Richard
  clean <- gsub("\\bRich(?:ar)?d?\\b\\.?", "Richard", clean, ignore.case = TRUE, perl = TRUE)
  # Robert
  clean <- gsub("\\bR(?:ob)?(?:er)?[ot]b?\\b\\.?", "Robert", clean, ignore.case = TRUE, perl = TRUE)
  # Ronald
  clean <- gsub("\\bRon(?:ald)?\\b\\.?", "Ronald", clean, ignore.case = TRUE, perl = TRUE)
  # Samuel
  clean <- gsub("\\bSam(?:ue)?l?\\b\\.?", "Samuel", clean, ignore.case = TRUE, perl = TRUE)
  # Thomas
  clean <- gsub("\\bTho(?:ma)?s?\\b\\.?", "Thomas", clean, ignore.case = TRUE, perl = TRUE)
  # William
  clean <- gsub("\\bW(?:illia)?m\\b\\.?", "William", clean, ignore.case = TRUE, perl = TRUE)
  # Walter
  clean <- gsub("\\bW(?:alte)?r\\b\\.?", "Walter", clean, ignore.case = TRUE, perl = TRUE)
  
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
  
  # Abbott 
  clean <- gsub("\\bAbbott?\\b\\.?", "Abbott", name, ignore.case = TRUE, perl = TRUE)
  # Abemethy
  clean <- gsub("\\bAbe(?:m|rn)ethy\\b\\.?", "Abernethy", clean, ignore.case = TRUE, perl = TRUE)
  # Addie
  clean <- gsub("\\bAdd?ie\\b\\.?", "Addie", clean, ignore.case = TRUE, perl = TRUE)
  # Aitchison
  clean <- gsub("\\bAitch[ei]son\\b\\.?", "Aitchison", clean, ignore.case = TRUE, perl = TRUE)
  # Aitken
  clean <- gsub("\\bAitk[ei]n\\b\\.?", "Aitken", clean, ignore.case = TRUE, perl = TRUE)
  # Baillie
  clean <- gsub("\\bBail?lie\\b\\.?", "Baillie", clean, ignore.case = TRUE, perl = TRUE)
  # Bealle
  clean <- gsub("\\bBeale?\\b\\.?", "Bealle", clean, ignore.case = TRUE, perl = TRUE)
  # Blackie
  clean <- gsub("\\bBla[ce]kie\\b\\.?", "Blackie", clean, ignore.case = TRUE, perl = TRUE)
  # Blyth
  clean <- gsub("\\bBlythe?\\b\\.?", "Blyth", clean, ignore.case = TRUE, perl = TRUE)
  # Cannan
  clean <- gsub("\\bCann[ao]n\\b\\.?", "Cannan", clean, ignore.case = TRUE, perl = TRUE)
  # Connell
  clean <- gsub("\\bConnell?\\b\\.?", "Connell", clean, ignore.case = TRUE, perl = TRUE)
  # Dennison
  clean <- gsub("\\bDenn?ison\\b\\.?", "Dennison", clean, ignore.case = TRUE, perl = TRUE)
  # Dunnet
  clean <- gsub("\\bDunnett?\\b\\.?", "Dunnett", clean, ignore.case = TRUE, perl = TRUE)
  # Easton
  clean <- gsub("\\bEast[co]n\\b\\.?", "Easton", clean, ignore.case = TRUE, perl = TRUE)
  # Edmon
  clean <- gsub("\\bEdm[ou]n", "Edmon", clean, ignore.case = TRUE, perl = TRUE)
  # Finnerty
  clean <- gsub("\\bFinn[ae]rty\\b\\.?", "Finnerty", clean, ignore.case = TRUE, perl = TRUE)
  # Fiskin
  clean <- gsub("\\bFisk[ei]n\\b\\.?", "Fiskin", clean, ignore.case = TRUE, perl = TRUE)
  # Gemmell
  clean <- gsub("\\bGemm[ei]ll\\b\\.?", "Gemmell", clean, ignore.case = TRUE, perl = TRUE)
  # Hilliard
  clean <- gsub("\\bHilli?ard\\b\\.?", "Hilliard", clean, ignore.case = TRUE, perl = TRUE)
  # Hinshelwood
  clean <- gsub("\\bHin[cs]helwood\\b\\.?", "Hinshelwood", clean, ignore.case = TRUE, perl = TRUE)
  # Imrie
  clean <- gsub("\\bI(?:m|rn)rie\\b\\.?", "Imrie", clean, ignore.case = TRUE, perl = TRUE)
  # Junior
  clean <- gsub("\\b,?\\s+\\bjun(?:io)?r?\\b\\.?", " Junior", clean, ignore.case = TRUE, perl = TRUE)
  # Kinninmont
  clean <- gsub("\\bKin[nu]inmont\\b\\.?", "Kinninmont", clean, ignore.case = TRUE, perl = TRUE)
  # Lochhead
  clean <- gsub("\\bLochh?ead\\b\\.?", "Lochhead", clean, ignore.case = TRUE, perl = TRUE)
  # Loudon 
  clean <- gsub("\\bLoudou?n\\b\\.?", "Loudon", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Ainsh
  clean <- gsub("\\bMac\\sAins(?:h|li)\\b\\.?", "Mac Ainsh", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Aulay
  clean <- gsub("\\bMac\\sAul[ae]y\\b\\.?", "Mac Aulay", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Auslan
  clean <- gsub("\\bMac\\sAu?slan\\b\\.?", "Mac Auslan", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Bride
  clean <- gsub("\\bMac\\sBr[iy]de\\b\\.?", "Mac Bride", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Cleuchan
  clean <- gsub("\\bMac\\sCl[ae]uchan\\b\\.?", "Mac Cleuchan", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Clement
  clean <- gsub("\\bMac\\sClem[eo]nt\\b\\.?", "Mac Clement", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Clusky
  clean <- gsub("\\bMac\\sCluske?y\\b\\.?", "Mac Clusky", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Connochie
  clean <- gsub("\\bMac\\sConn?[aeo][cg]h(?:ie|y)\\b\\.?", "Mac Connochie", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Cormick
  clean <- gsub("\\bMac\\sCorm?[ai]ck\\b\\.?", "Mac Cormick", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Creadie
  clean <- gsub("\\bMac\\sCrea?dd?(?:ie|y)\\b\\.?", "Mac Creadie", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Crindell
  clean <- gsub("\\bMac\\sCrind[el][el]l?\\b\\.?", "Mac Crindell", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Dowall
  clean <- gsub("\\bMac\\sDow[ae]ll\\b\\.?", "Mac Dowall", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Ewan
  clean <- gsub("\\bMac\\sEw[ae]n\\b\\.?", "Mac Ewan", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Fadyen
  clean <- gsub("\\bMac\\sFad[yz][ae]?[ae]?n\\b\\.?", "Mac Fadyen", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Fedries
  clean <- gsub("\\bMac\\sFedrie?s\\b\\.?", "Mac Fedries", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Garrey
  clean <- gsub("\\bMac\\sGarre?y\\b\\.?", "Mac Garrey", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Gie
  clean <- gsub("\\bMac\\sGh?ie\\b\\.?", "Mac Gie", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Glashan
  clean <- gsub("\\bMac\\sGlash[ae]n\\b\\.?", "Mac Glashan", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Graddie
  clean <- gsub("\\bMac\\sGradd?(?:ie|y)\\b\\.?", "Mac Graddie", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Innis
  clean <- gsub("\\bMac\\sInn[ei]s\\b\\.?", "Mac Innis", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Iver
  clean <- gsub("\\bMac\\s[il]v[eo]r\b\\.?", "Mac Iver", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Kinly
  clean <- gsub("\\bMac\\sKind?l[ae]y\\b\\.?", "Mac Innis", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Kirdy
  clean <- gsub("\\bMac\\sKird(?:ie|y)\\b\\.?", "Mac Kirdy", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Lachlan
  clean <- gsub("\\bMac\\sLau?[cg]hl[ai]n\\b\\.?", "Mac Lachlan", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Laren
  clean <- gsub("\\bMac\\sLau?r[ei]n\\b\\.?", "Mac Laren", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Millan
  clean <- gsub("\\bMac\\sMill[ea]n\\b\\.?", "Mac Millan", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Naughton
  clean <- gsub("\\bMac\\sNaught[aeo]n\\b\\.?", "Mac Naughton", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Neill
  clean <- gsub("\\bMac\\sNeill?\\b\\.?", "Mac Neill", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Nie
  clean <- gsub("\\bMac\\sN[ei]e\\b\\.?", "Mac Nie", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Parlane
  clean <- gsub("\\bMac\\sParl[ea]ne?\\b\\.?", "Mac Parlane", clean, ignore.case = TRUE, perl = TRUE)
  # Mac Symon
  clean <- gsub("\\bMac\\sSymond?\\b\\.?", "Mac Symon", clean, ignore.case = TRUE, perl = TRUE)
  # Main
  clean <- gsub("\\bMains?\\b\\.?", "Mac Main", clean, ignore.case = TRUE, perl = TRUE)
  # Meighan
  clean <- gsub("\\bMeigh[ae]n\\b\\.?", "Meighan", clean, ignore.case = TRUE, perl = TRUE)
  # Meikleham
  clean <- gsub("\\bMeikle?h?am\\b\\.?", "Meikleham", clean, ignore.case = TRUE, perl = TRUE)
  # Millar
  clean <- gsub("\\bMill[ae]r\\b\\.?", "Millar", clean, ignore.case = TRUE, perl = TRUE)
  # Milligan
  clean <- gsub("\\bMillig[ae]n\\b\\.?", "Milligan", clean, ignore.case = TRUE, perl = TRUE)
  # Monie
  clean <- gsub("\\bMon(?:ie|y)\\b\\.?", "Monie", clean, ignore.case = TRUE, perl = TRUE)
  # Montague
  clean <- gsub("\\bMontague?\\b\\.?", "Montague", clean, ignore.case = TRUE, perl = TRUE)
  # Montgomery
  clean <- gsub("\\bMontgomer(?:ie|y)\\b\\.?", "Montgomery", clean, ignore.case = TRUE, perl = TRUE)
  # Moodie
  clean <- gsub("\\bMood(?:ie|y)\\b\\.?", "Moodie", clean, ignore.case = TRUE, perl = TRUE)
  # Mullen
  clean <- gsub("\\bMull[ei]n\\b\\.?", "Mullen", clean, ignore.case = TRUE, perl = TRUE)
  # Mushet
  clean <- gsub("\\bMus(?:li|h)et\\b\\.?", "Mushet", clean, ignore.case = TRUE, perl = TRUE)
  # Neil
  clean <- gsub("\\bNeill?\\b\\.?", "Neil", clean, ignore.case = TRUE, perl = TRUE)
  # Neilson
  clean <- gsub("\\bNei?lson?\\b\\.?", "Neilson", clean, ignore.case = TRUE, perl = TRUE)
  # Nichol
  clean <- gsub("\\bNich?ol\\b\\.?", "Nichol", clean, ignore.case = TRUE, perl = TRUE)
  # Nisbet
  clean <- gsub("\\bNisbett?\\b\\.?", "Nichol", clean, ignore.case = TRUE, perl = TRUE)
  # Notman
  clean <- gsub("\\bNotma(?:ri|n)\\b\\.?", "Nichol", clean, ignore.case = TRUE, perl = TRUE)
  # O'Neill
  clean <- gsub("\\bO'Neill?\\b\\.?", "O'Neill", clean, ignore.case = TRUE, perl = TRUE)
  # Oatt
  clean <- gsub("\\bOas?tt?\\b\\.?", "Oatt", clean, ignore.case = TRUE, perl = TRUE)
  # Ogilvie
  clean <- gsub("\\bOgilv(?:ie|y)\\b\\.?", "Ogilvie", clean, ignore.case = TRUE, perl = TRUE)
  # Owens
  clean <- gsub("\\bOwens?\\b\\.?", "Owens", clean, ignore.case = TRUE, perl = TRUE)
  # Patterson
  clean <- gsub("\\bPatt?erson\\b\\.?", "Patterson", clean, ignore.case = TRUE, perl = TRUE)
  # Pattison
  clean <- gsub("\\bPattie?son\\b\\.?", "Pattison", clean, ignore.case = TRUE, perl = TRUE)
  # Penney
  clean <- gsub("\\bPenne?y\\b\\.?", "Penney", clean, ignore.case = TRUE, perl = TRUE)
  # Pennycook
  clean <- gsub("\\bPennyc[ou][oi]c?k\\b\\.?", "Pennycook", clean, ignore.case = TRUE, perl = TRUE)
  # Perrie
  clean <- gsub("\\bPerr(?:ie|y)\\b\\.?", "Pennycook", clean, ignore.case = TRUE, perl = TRUE)
  # Phillips
  clean <- gsub("\\bPhillips?\\b\\.?", "Phillips", clean, ignore.case = TRUE, perl = TRUE)
  # Philps
  clean <- gsub("\\bPhilps?\\b\\.?", "Philps", clean, ignore.case = TRUE, perl = TRUE)
  # Pigott
  clean <- gsub("\\bPigott?\\b\\.?", "Pigott", clean, ignore.case = TRUE, perl = TRUE)
  # Pollock
  clean <- gsub("\\bPolloc?k\\b\\.?", "Pigott", clean, ignore.case = TRUE, perl = TRUE)
  # Priestley
  clean <- gsub("\\bPriestle?y\\b\\.?", "Pigott", clean, ignore.case = TRUE, perl = TRUE)
  # Rankine
  clean <- gsub("\\bRankine?\\b\\.?", "Rankine", clean, ignore.case = TRUE, perl = TRUE)
  # Richard
  clean <- gsub("\\bRichards?\\b\\.?", "Richard", clean, ignore.case = TRUE, perl = TRUE)
  # Roddan
  clean <- gsub("\\bRodd[ea]n\\b\\.?", "Roddan", clean, ignore.case = TRUE, perl = TRUE)
  # Rodger
  clean <- gsub("\\bRod?gers?\\b\\.?", "Rodger", clean, ignore.case = TRUE, perl = TRUE)
  # Rowand
  clean <- gsub("\\bRowand?\\b\\.?", "Rowand", clean, ignore.case = TRUE, perl = TRUE)
  # Salmond
  clean <- gsub("\\bSalmond?\\b\\.?", "Salmond", clean, ignore.case = TRUE, perl = TRUE)
  # Senior
  clean <- gsub("\\b,?\\s+\\bsen(?:ior)?\\b\\.?", " Senior", clean, ignore.case = TRUE, perl = TRUE)
  # Scouller
  clean <- gsub("\\bScoul?l[ae]r\\b\\.?", "Scouller", clean, ignore.case = TRUE, perl = TRUE)
  # Seligmann
  clean <- gsub("\\bSeligmann?\\b\\.?", "Seligmann", clean, ignore.case = TRUE, perl = TRUE)
  # Sellars
  clean <- gsub("\\bSell?[ae]rs?\\b\\.?", "Sellars", clean, ignore.case = TRUE, perl = TRUE)
  # Sheriff
  clean <- gsub("\\bSheriffs?\\b\\.?", "Sheriff", clean, ignore.case = TRUE, perl = TRUE)
  # Shields
  clean <- gsub("\\bShield?s\\b\\.?", "Shields", clean, ignore.case = TRUE, perl = TRUE)
  # Sillers
  clean <- gsub("\\bSill[ea]rs\\b\\.?", "Sillers", clean, ignore.case = TRUE, perl = TRUE)
  # Sim
  clean <- gsub("\\bSimm?\\b\\.?", "Sim", clean, ignore.case = TRUE, perl = TRUE)
  # Simpson
  clean <- gsub("\\bSimp?son\\b\\.?", "Simpson", clean, ignore.case = TRUE, perl = TRUE)
  # Slimon
  clean <- gsub("\\bSlim[ao]n\\b\\.?", "Slimon", clean, ignore.case = TRUE, perl = TRUE)
  # Smellie
  clean <- gsub("\\bSm[ei]llie\\b\\.?", "Smellie", clean, ignore.case = TRUE, perl = TRUE)
  # Smith
  clean <- gsub("\\bSm[iy]th\\b\\.?", "Smith", clean, ignore.case = TRUE, perl = TRUE)
  # Sneddon
  clean <- gsub("\\bSnedd[eo]n\\b\\.?", "Sneddon", clean, ignore.case = TRUE, perl = TRUE)
  # Sommerville
  clean <- gsub("\\bSommLerva?ill?e?\\b\\.?", "Sommerville", clean, ignore.case = TRUE, perl = TRUE)
  # Spence
  clean <- gsub("\\bSpencer?\\b\\.?", "Spence", clean, ignore.case = TRUE, perl = TRUE)
  # Steel
  clean <- gsub("\\bSteele?\\b\\.?", "Steel", clean, ignore.case = TRUE, perl = TRUE)
  # Stephen
  clean <- gsub("\\bStephens?\\b\\.?", "Stephen", clean, ignore.case = TRUE, perl = TRUE)
  # Stratton
  clean <- gsub("\\bStratt?on\\b\\.?", "Stratton", clean, ignore.case = TRUE, perl = TRUE)
  # Struthers
  clean <- gsub("\\bStruthe[rt]s\\b\\.?", "Struthers", clean, ignore.case = TRUE, perl = TRUE)
  # Swan
  clean <- gsub("\\bSwann?\\b\\.?", "Swan", clean, ignore.case = TRUE, perl = TRUE)
  # Taylor
  clean <- gsub("\\bTa[iy]lor\\b\\.?", "Taylor", clean, ignore.case = TRUE, perl = TRUE)
  # Tennent
  clean <- gsub("\\bTenn[ae]nt\\b\\.?", "Tennent", clean, ignore.case = TRUE, perl = TRUE)
  # Thompson
  clean <- gsub("\\bThomp?son\\b\\.?", "Thompson", clean, ignore.case = TRUE, perl = TRUE)
  # Thyne
  clean <- gsub("\\bThynn?e\\b\\.?", "Thyne", clean, ignore.case = TRUE, perl = TRUE)
  # Tod
  clean <- gsub("\\bTodd?\\b\\.?", "Tod", clean, ignore.case = TRUE, perl = TRUE)
  # Toner
  clean <- gsub("\\bTon[ae]r\\b\\.?", "Toner", clean, ignore.case = TRUE, perl = TRUE)
  # Toshach
  clean <- gsub("\\bTosha[co]h\\b\\.?", "Toshach", clean, ignore.case = TRUE, perl = TRUE)
  # Towart
  clean <- gsub("\\bTow[ae]rt\\b\\.?", "Towart", clean, ignore.case = TRUE, perl = TRUE)
  # Walsh
  clean <- gsub("\\bWalshe?\\b\\.?", "Walsh", clean, ignore.case = TRUE, perl = TRUE)
  # Wylde
  clean <- gsub("\\bWylde?\\b\\.?", "Wylde", clean, ignore.case = TRUE, perl = TRUE)
  # Yuill
  clean <- gsub("\\bYuille?\\b\\.?", "Yuill", clean, ignore.case = TRUE, perl = TRUE)
  
  
  clean
}

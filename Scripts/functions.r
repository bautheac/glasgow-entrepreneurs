

places <- c(
  "Arcade", "Arches", "Bridge", "Circus", "Close", "Court", "Crescent", "Drive",
  "Garden", "Lane", "Loan", "Place", "Port", "Quay", "Road", "Square", "Street",
  "Terrace", "Wynd"
)


clean_address <- function(raw){
  
  # Pre-clean
  clean <- pre_clean(raw)
  # Places
  clean <- clean_places(clean)
  # Names
  clean <- clean_names(clean)
  # Post clean
  clean <- post_clean(clean)
  
  clean
}

pre_clean <- function(address){
  
  # Special characters
  clean <- clean_specials(address)
  # Clean ends
  clean <- clean_ends(clean)
  # Separate words
  clean <- clean_attached_words(clean)
  # M' to Mc
  clean <- clean_mac(clean)
  # Saints
  clean <- clean_saints(clean)
  # Possessives
  clean <- clean_possessives(clean)
  # Suffixes
  clean <- clean_suffixes(clean)
  
  clean
}

post_clean <- function(address){
  
  # Others
  clean <- clean_others(address)
  # Clean ends
  clean <- clean_ends(clean)
  
  clean
}


clean_mac <- function(address){
  
  clean <- gsub("M\\s?['c](?=\\w)", "Mac", address, ignore.case = FALSE, perl = TRUE)
  
  clean
}


clean_saints <- function(address){
  
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


clean_possessives <- function(address){
  
  clean <- gsub("\\b(\\w+)\\b\\.?'?s'?", "\\1s", address, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("(\\b\\w+s\\b)'\\.?", "\\1", clean, ignore.case = TRUE, perl = TRUE)
  
  clean
}


clean_specials <- function(address){
  
  clean <- gsub("»", "", address, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("’", "'", clean, ignore.case = TRUE, perl = TRUE)
  
  clean
}

clean_ends <- function(address){
  
  # Trim white space(s) at start and end as well as multiple white spaces in a row
  # clean <- stringr::str_squish(address)
  clean <- gsub("^\\s+", "", address, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("\\s{2,}", " ", clean, ignore.case = TRUE, perl = TRUE)
  
  # Place period at the end of address if none
  clean <- gsub("[,'.\\s]+$", "\\.", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("([^.])$", "\\1\\.", clean, ignore.case = TRUE, perl = TRUE)
  
  clean
}

clean_attached_words <- function(address){
  
  # If two words are only separated by a period, replace period with white space
  clean <- gsub(
    "(\\b\\w+\\b)\\.(\\b\\w+\\b)", "\\1 \\2", address, ignore.case = TRUE, perl = TRUE
  )
  
  clean
}

clean_numbers <- function(address){
  
  clean <- gsub("(\\d{3})\\s?(\\d{3})", "\\1, \\2", address, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("(\\d)\\s?[,&]\\s?(\\d)", "\\1, \\2", clean, ignore.case = TRUE, perl = TRUE)
  clean <- gsub("(?<=\\d)\\.5", " 1/2", clean, ignore.case = TRUE, perl = TRUE)
  clean
}



clean_places <- function(address){
  
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

clean_suffixes <- function(address){
  
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

clean_names <- function(address){
  
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



clean_others <- function(address){
  
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

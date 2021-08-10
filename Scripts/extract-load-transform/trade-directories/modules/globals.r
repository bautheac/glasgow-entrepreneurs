

# Addresses ####

## Names ####
address_names <- tibble::tribble(
  ~pattern,                                                           ~replacement,                        ~ignore.case,
  # "\\bAbbotsf?o?r?d?\\b\\.?(?=\\sP)", "Abbotsford", TRUE,
  "\\bAbbotsf?o?r?d?\\b\\.?",                                         "Abbotsford",                        TRUE,
  "\\bAbbotsford.+\\s",                                               "Abbotsford ",                       TRUE,
  "\\bAbbotsford\\s(?:p.+)?l(?=[\\b.$])",                             "Abbotsford Place",                  TRUE,
  "\\bAber(?:corn)?\\b\\.?",                                          "Abercorn",                          TRUE,
  "\\bAbercr?o?m?(?:by)?\\b\\.?",                                     "Abercromby",                        TRUE,
  "\\bAbercrombyst\\b",                                               "Abercromby Street",                 TRUE,
  "\\bAuchincampbell\\b",                                             "Auchincampbell",                    TRUE,
  "\\bAuchingramont\\b",                                              "Auchingramont",                     TRUE,
  "\\bAdelphist\\b",                                                  "Adelphi Street",                    TRUE,
  "\\bAlpinest\\b\\.?",                                               "Alpine Street",                     TRUE,
  # "\\bAnd?e?r?s?t?o?n?\\b\\.?", "Anderston", TRUE,
  "\\bAn((?!n\\s))(?(1)d?e?r?s?t?o?n?\\b)\\.?",                       "Anderston",                         TRUE,
  "\\bA[nu]ders[lt]o(?:n|rs|u)\\b\\.?",                               "Anderston",                         TRUE,
  "\\bAnderstreet\\b\\.?",                                            "Anderston",                         TRUE,
  "\\b[A-Z]\\s\\bAnderston\\b",                                       "Anderston",                         TRUE,
  "\\bAndr?e?w\\b[,.]?",                                              "Andrew",                            TRUE,
  "\\bAnnfie?(?:ld|hl)?\\b\\.?",                                      "Annfield",                          TRUE,
  "\\bApsle[jy]s?\\b\\.?",                                            "Apsley",                            TRUE,
  "\\bArdgowan\\b",                                                   "Ardgowan",                          TRUE,
  "\\bArg[jvy]?[dl']?[ae]?\\b\\.?",                                   "Argyle",                            TRUE,
  "\\bArglye\\b\\.?",                                                 "Argyle",                            TRUE,
  "\\bArgyle\\s?s[it]\\b",                                            "Argyle street",                     TRUE,
  "\\bArlington\\b",                                                  "Arlington",                         TRUE,
  "\\bAthole\\b",                                                     "Athole",                            TRUE,
  "\\bAuchenheglish\\b",                                              "Auchenheglish",                     TRUE,
  "\\bAvondale\\b",                                                   "Avondale Street",                   TRUE,
  "\\bAvondalest(?:reet)?\\b\\.?",                                    "Avondale Street",                   TRUE,
  "\\bAyton\\s+([a-z]+).+Blackfriar.+",                               "Ayton \\1, off Blackfriars Street", TRUE,
  "\\bBack\\b",                                                       "Back",                              TRUE,
  "(Baird\\sStreet)(.*(glebe|westmuir).*)?.*",                        "\\1\\2",                            TRUE,
  "\\bBa[hl](?:rn|m|n)an[nu]o",                                       "Balmanno",                          TRUE,
  "\\bBat(?:h|li)\\b",                                                "Bath",                              TRUE,
  "\\bBarony\\b",                                                     "Barony",                            TRUE,
  "\\bBelgr[ao]ve\\b",                                                "Belgrave",                          TRUE,
  "\\bBell\\b",                                                       "Bell",                              TRUE,
  "\\bBellgr[ao]ve\\b",                                               "Bellgrove",                         TRUE,
  "\\bBenview\\b",                                                    "Benview",                           TRUE,
  "\\bBerkley\\b",                                                    "Berkley",                           TRUE,
  "\\bBirdston\\b",                                                   "Birdston",                          TRUE,
  "\\bBishop\\b",                                                     "Bishop",                            TRUE,
  "\\bBishopb(?:riggs)?\\b\\.?",                                      "Bishopbriggs",                      TRUE,
  "\\bBlackfr?(?:iars)?\\b\\'?\\.?",                                  "Blackfriars",                       TRUE,
  "\\bBlantyre\\b",                                                   "Blantyre",                          TRUE,
  "\\bBlythsw?(?:ood)?\\b\\.?",                                       "Blythswood",                        TRUE,
  "both\\s?w?(?:ell)?",                                               "Bothwell",                          TRUE,
  "\\bBoturic[hk]\\b",                                                "Boturich",                          TRUE,
  "\\bBrea?dalban[ae]\\b",                                            "Breadalbane",                       TRUE,
  "\\bBroad\\b",                                                      "Broad",                             TRUE,
  "\\bBrown\\b",                                                      "Brown",                             TRUE,
  "\\bBridgeg(?:eg)?a?t?e?\\b\\.?",                                   "Bridgegate",                        TRUE,
  "\\bBridgegatest(?:reet)?\\b",                                      "Bridgegate Street",                 TRUE,
  "\\bBridge(?:to?)?n?\\b\\??",                                       "Bridgeton",                         TRUE,
  "\\bBridge?t(?:[ao][nu])?\\b",                                      "Bridgeton",                         TRUE,
  "\\bBridgn\\b",                                                     "Bridgeton",                         TRUE,
  "\\b(?<=Street|\\.)\\s?Bridgeton\\b",                               ", Bridgeton",                       TRUE,
  # "\\bBroo?m([^h]?)(?(1)[et]?\\s?(?:law)?\\b)\\.?", "Broomielaw", TRUE,
  "\\bBroo?[mi]n?([^h]?)(?(1)(?:[eit]+)?\\s?(?:law)?j?\\b)\\.?",      "Broomielaw",                        TRUE,
  "\\bBroo[mn]i+el(?:aw)?\\b",                                        "Broomielaw",                        TRUE,
  "\\bBrunsw?(?:ic)?k?\\b\\.?",                                       "Brunswick",                         TRUE,
  "\\bB[nu][ce]cl?(?:euch)?\\b\\.?",                                  "Buccleuch",                         TRUE,
  "\\bBuch((?!an street))(?(1)a?n?(?:an)?\\b),?\\.?",                 "Buchanan",                          TRUE,
  "\\bBuchananst\\b",                                                 "Buchanan Street",                   TRUE,
  "\\bCadogan\\b",                                                    "Cadogan",                           TRUE,
  "\\bCa?l(?:t?o?n?)?\\b\\.?",                                        "Calton",                            TRUE,
  "\\bCalt(?:o[nu])?\\b\\.?",                                         "Calton",                            TRUE,
  "\\bCaltonmouth\\b\\.?",                                            "Calton-mouth",                      TRUE,
  "\\bCa(?:m|in)br?(?:id)?g?e?\\b\\.?",                               "Cambridge",                         TRUE,
  "\\bCamden\\d",                                                     "Camden",                            TRUE,
  "\\bCampb?e?l?l?\\b\\.?",                                           "Campbell",                          TRUE,
  "\\bCann?(?:ing)?\\b\\.?",                                          "Canning",                           TRUE,
  "\\bCanningst\\b",                                                  "Canning Street",                    TRUE,
  "\\bCandle?r?i?[gs]?[gs]?[gs]?\\b-?\\.?",                           "Candleriggs",                       TRUE,
  "\\bCanon\\b",                                                      "Canon",                             TRUE,
  "\\bCarlt?o?[nu]?\\b\\.?",                                          "Carlton",                           TRUE,
  "\\bCarnarv(?:on)?\\b[.,]?",                                        "Carnarvon",                         TRUE,
  "\\bCarri[ce]k\\b",                                                 "Carrick",                           TRUE,
  "\\bCastle\\b",                                                     "Castle",                            TRUE,
  "\\bCathedr?(?:al)?\\b\\.?",                                        "Cathedral",                         TRUE,
  "\\bCavend(?:ish)?\\b\\.?",                                         "Cavendish",                         TRUE,
  "\\bCharlotte\\b",                                                  "Charlotte",                         TRUE,
  "\\bCharle[3s]\\b",                                                 "Charles",                           TRUE,
  "\\bCha[rd](?:lotte)?\\b\\.?",                                      "Charlotte",                         TRUE,
  "\\bCharing\\s?Cross\\b",                                           "Charing Cross",                     TRUE,
  "\\bChatham\\b",                                                    "Chatham",                           TRUE,
  "\\bCheaps(?:ide)?\\b\\.?",                                         "Cheapside",                         TRUE,
  "\\bClarem(?:on)?t?\\b\\.?",                                        "Claremont",                         TRUE,
  "\\bCleland\\b",                                                    "Cleland",                           TRUE,
  "\\bCl[vy]de\\b",                                                   "Clyde",                             TRUE,
  "\\bC[oq][bh]urg\\b\\.?",                                           "Coburg",                            TRUE,
  "\\bCoatbridge\\b",                                                 "Coatbridge",                        TRUE,
  "\\bCollege\\b",                                                    "College",                           TRUE,
  "\\bComm?(?:er)?c?e?\\b\\.?",                                       "Commerce",                          TRUE,
  "\\bCommercest\\b",                                                 "Commerce Street",                   TRUE,
  "\\bCommercial\\b",                                                 "Commercial",                        TRUE,
  "\\bCow[ces]?[au]?d?[ci]?\\s?[dlns]?(?:e[nu])?s?\\b\\.?",           "Cowcaddens",                        TRUE,
  "\\bCraignestock\\b",                                               "Craignestock",                      TRUE,
  "\\bCraigrownie\\b",                                                "Craigrownie",                       TRUE,
  "\\bCranston\\b",                                                   "Cranston",                          TRUE,
  "\\bCrookston\\b",                                                  "Crookston",                         TRUE,
  "\\bCross[bh]ill\\b",                                               "Crosshill",                         TRUE,
  "\\bCrown\\b",                                                      "Crown",                             TRUE,
  "\\bCroy\\b",                                                       "Croy",                              TRUE,
  "\\bCrown\\s?p(?:oint)?\\b\\.?",                                    "Crown Point",                       TRUE,
  "\\bCumb(?:er)?l?a?n?d?\\b\\.?",                                    "Cumberland",                        TRUE,
  "\\bDale\\b",                                                       "Dale",                              TRUE,
  "\\bDalh?(?:ousie)?\\b\\.?",                                        "Dalhousie",                         TRUE,
  "\\bDalm?(?:a[ir])?j?[an]?(?:[ao][ce])?k?(?=\\b|rd)",               "Dalmarnock",                        TRUE,
  "\\bDahnarnock\\b",                                                 "Dalmarnock",                        TRUE,
  "\\bDalmarnockrd\\b",                                               "Dalmarnock Road",                   TRUE,
  # "\\bD[eo]b(?:bie)?'?s?\\.?", "Dobbies", TRUE,
  "\\bDixon\\b\\.?",                                                  "Dixon",                             TRUE,
  "\\bD[eo]b(?:bie)?'?s?\\.?\\s?[il]?o?a?n?\\b\\.?",                  "Dobbies Loan",                      TRUE,
  "\\bDoug(?:las)?\\b\\.?",                                           "Douglas",                           TRUE,
  "\\bDove\\s?hill\\b\\.?",                                           "Dovehill",                          TRUE,
  "\\bDowanhill\\b",                                                  "Dowanhill",                         TRUE,
  "\\bDunclutha\\b",                                                  "Dunclutha",                         TRUE,
  "\\bDry\\s?gate\\b",                                                "Drygate",                           TRUE,
  "\\bDuke\\b",                                                       "Duke",                              TRUE,
  "\\bDukest\\b",                                                     "Duke Street",                       TRUE,
  "\\bDumb?(?:ar)?t?o?n?\\b\\.?",                                     "Dumbarton",                         TRUE,
  "\\bD[nu][nu]?d?(?:as)?\\b\\.?",                                    "Dundas",                            TRUE,
  "\\bDundasst\\b",                                                   "Dundas Street",                     TRUE,
  "\\bDunl?(?:op)?\\b\\.?",                                           "Dunlop",                            TRUE,
  "\\bEgli[nu]t?(?:on)?\\b\\.?",                                      "Eglinton",                          TRUE,
  "\\bEglintonst\\b",                                                 "Eglinton Street",                   TRUE,
  "\\bElm\\s?ba[nu]k\\b\\.?",                                         "Elmbank",                           TRUE,
  "\\bElderslie\\b",                                                  "Elderslie",                         TRUE,
  "\\bEldon\\b",                                                      "Eldon",                             TRUE,
  "\\bElgin\\b",                                                      "Elgin",                             TRUE,
  "\\bElmbank\\b",                                                    "Elmbank",                           TRUE,
  "\\Enfield\\b",                                                     "Enfield",                           TRUE,
  "\\bEn(?:och)?\\b\\.?",                                             "Enoch",                             TRUE,
  "\\bFa[ir]lane",                                                    "Farlane",                           TRUE,
  "\\bFairlie\\b",                                                    "Fairlie",                           TRUE,
  "\\bFerguson\\b",                                                   "Ferguson",                          TRUE,
  "\\bFind?la[yv]\\b",                                                "Finlay",                            TRUE,
  "\\bFinnieston\\b",                                                 "Finnieston",                        TRUE,
  "\\bFish\\s?market\\b\\.?",                                         "Fish market",                       TRUE,
  "\\b[FP]ossil\\b\\.?",                                              "Fossil",                            TRUE,
  "\\bFortland\\b\\.?",                                               "Fortland",                          TRUE,
  "\\bFred(?:er)?(?:ic)?k?\\b\\.?",                                   "Frederick",                         TRUE,
  "\\bGa[il]l?(?:[jo](?:w|vi)-?(?:g|tr))?[as]?[t-]?e?\\b-?\\.?",      "Gallowgate",                        TRUE,
  "\\bGa?(?:m|rn)?e?\\s?[ft]?\\s?h?(?:i(?:li|ll|n))?\\b\\.?",         "Garnethill",                        TRUE,
  "\\bGarnethil\\sJ",                                                 "Garnethill",                        TRUE,
  "(?<=Hill).+\\K\\bGar\\b\\.?",                                      "Garnethill",                        TRUE,
  "\\bGarng(?:ad)?\\b\\.?",                                           "Garngad",                           TRUE,
  "\\bGarngadrd\\b",                                                  "Garngad Road",                      TRUE,
  "\\bGarngadhill\\b",                                                "Garngad Hill",                      TRUE,
  "\\bGarsc?(?:ad)?(?:den)?\\b\\.?",                                  "Garscadden",                        TRUE,
  "\\bGarsc[un]?[bh]?e?\\b\\.?",                                      "Garscube",                          TRUE,
  "\\bGarscube&",                                                     "Garscube",                          TRUE,
  "\\bGarscuberd\\b\\.?",                                             "Garscube Road",                     TRUE,
  "\\bGeo(?:rg)?[aeo]?\\.?",                                          "George",                            TRUE,
  "\\bGeorgest(?:reet)?\\b\\.?",                                      "George Street",                     TRUE,
  "\\bGibson\\b",                                                     "Gibson",                            TRUE,
  "\\bGlassf(?:or)?d?\\b\\.?",                                        "Glassford",                         TRUE,
  "\\bGlouc(?:es)?(?:ter)?\\b\\.?",                                   "Gloucester",                        TRUE,
  "\\bGor[bh]?a?[il]?s?\\b\\.?",                                      "Gorbals",                           TRUE,
  "\\b(Gorbals).?\\b",                                                "\\1",                               TRUE,
  "\\bGord(?:on)?\\b\\.?",                                            "Gordon",                            TRUE,
  "\\bGo\\s?van\\b",                                                  "Govan",                             TRUE,
  "\\bGov((?!an street))(?(1)a?[nu]?(?:hill)?\\b),?\\.?",             "Govanhill",                         TRUE,
  "\\bGrace\\b",                                                      "Grace",                             TRUE,
  "\\bGr[ars][ejmso]?(?:me)?\\b\\.?",                                 "Graeme",                            TRUE,
  "\\bGraham\\b\\.?",                                                 "Graham",                            TRUE,
  "\\bGranby\\b\\.?",                                                 "Granby",                            TRUE,
  "\\bGreenh?(?:ead)?\\b\\.?",                                        "Greenhead",                         TRUE,
  "\\bGreenside\\b",                                                  "Greenside",                         TRUE,
  "\\bGreenvale\\b",                                                  "Greenvale",                         TRUE,
  "\\bGrossvenor\\b",                                                 "Grossvenor",                        TRUE,
  "\\bGrove\\b",                                                      "Grove",                             TRUE,
  "\\bHam(?:il)?t?(?:on)?\\b\\.?",                                    "Hamilton",                          TRUE,
  "\\bHanover\\b",                                                    "Hanover",                           TRUE,
  "\\bHarmony\\b",                                                    "Harmony",                           TRUE,
  "\\bHigh\\b",                                                       "High",                              TRUE,
  "\\bHighst\\b",                                                     "High Street",                       TRUE,
  # "\\bHill\\sStreet,\\s\\bGar(?:nethill)?\\b\\.?", "Hill Street, Garnethill", TRUE,
  "\\bHill\\b",                                                       "Hill",                              TRUE,
  "\\bHillh(?:ead)?\\b\\.?",                                          "Hillhead",                          TRUE,
  "\\bHolm\\b",                                                       "Holm",                              TRUE,
  "\\bHolm((?!\\sstreet))(?(1)[bh]?(?:ead)?\\b)\\.?",                 "Holmhead",                          TRUE,
  "\\bHolyr(?:ood)?\\b\\.?",                                          "Holyrood",                          TRUE,
  "\\bHope\\b",                                                       "Hope",                              TRUE,
  "\\bHopest(?:reet)?\\b\\.?",                                        "Hope Street",                       TRUE,
  "\\bHopet(?:ou?)?n\\b\\.?",                                         "Hopetoun",                          TRUE,
  "\\bHosp(?:ital)?\\b\\.?",                                          "Hospital",                          TRUE,
  "\\bHouston\\b",                                                    "Houston",                           TRUE,
  "\\bHowa?r?d?\\b\\.?",                                              "Howard",                            TRUE,
  "\\bHundred\\s?acre\\s?hill\\b",                                    "Hundred Acre Hill",                 TRUE,
  "\\bHut[ce]?(?:[bhk]|li)?e?s?o?[nu]?\\b\\.?",                       "Hutcheson",                         TRUE,
  "\\bHutchesont?(?:ow)?n\\b\\.?",                                    "Hutchesontown",                     TRUE,
  "\\bHutchesonto\\b\\.?",                                            "Hutchesontown",                     TRUE,
  "\\bHutcheson.+?t\\s?own\\b\\.?",                                   "Hutchesontown",                     TRUE,
  "\\bHyde\\s?park\\b\\.?",                                           "Hydepark",                          TRUE,
  "\\bIngr(?:am)?\\b\\.?",                                            "Ingram",                            TRUE,
  "\\bJama(?:ica)?\\b\\.?",                                           "Jamaica",                           TRUE,
  "\\bJam\\b\\.?(?=\\sst)",                                           "Jamaica",                           TRUE,
  "\\bJamieson\\b",                                                   "Jamieson",                          TRUE,
  "\\bJamaicast\\b\\.?",                                              "Jamaica Street",                    TRUE,
  "\\bJa(?:me)?s\\b[\\.']?",                                          "James",                             TRUE,
  "\\bJohnst(?:reet)?\\b\\.?",                                        "John Street",                       TRUE,
  "\\bJohn Street\\b[,\\s]+\\bBridge?t?o?n?.*",                       "John Street, Bridgeton",            TRUE,
  "\\bJohn Street(?:\\b\\s[a-z]\\s\\b)?Bridgeton\\b",                 "John Street, Bridgeton",            TRUE,
  "\\bKelvi[nu]\\b",                                                  "Kelvingrove",                       TRUE,
  "\\bKelvingr(?:ove)?\\b\\.?",                                       "Kelvingrove",                       TRUE,
  "\\bKelvinha(?:ugh)?\\b\\.?",                                       "Kelvinhaugh",                       TRUE,
  "\\bKenn(?:edy)?\\b\\.?",                                           "Kennedy",                           TRUE,
  "\\bKent\\b",                                                       "Kent",                              TRUE,
  "\\bKerr\\b",                                                       "Kerr",                              TRUE,
  "\\bKile\\b",                                                       "Kile",                              TRUE,
  "\\bKillermont\\b",                                                 "Killermont",                        TRUE,
  "\\bKingst?o?n?\\b\\.?",                                            "Kingston",                          TRUE,
  "\\bKi[nu]g\\b\\.?",                                                "King",                              TRUE,
  "\\bK[il][nu]n(?:ing)?\\b\\.?",                                     "Kinning",                           TRUE,
  "\\bKirkst(?:reet)?\\b\\.?",                                        "Kirk street",                       TRUE,
  "\\bLo?ance(?:field)?\\b\\.?",                                      "Lancefield",                        TRUE,
  "\\bLandressy\\b",                                                  "Landressy",                         TRUE,
  "\\bLansd(?:owne)?\\b\\.?",                                         "Lansdowne",                         TRUE,
  "\\bLaurel\\b",                                                     "Laurel",                            TRUE,
  "\\bLittlest(?:reet)?\\b\\.?",                                      "Little Street",                     TRUE,
  "\\bLit(?:tle)?\\.?\\s?Dovehill\\b\\.?",                            "Little Dovehill",                   TRUE,
  "\\bL(?:ittle)?\\.?\\s?Hamilton Street\\b\\.?",                     "Little Hamilton Street",            TRUE,
  "\\bL[ou][nu]d?(?:on)?\\b\\.?",                                     "London",                            TRUE,
  "\\bLyne?doch\\b",                                                  "Lynedoch",                          TRUE,
  "\\bM\\s?['c]\\s?(?=\\w)",                                          "Mac ",                              TRUE,
  "\\bMac,?\\s?(?=\\w)",                                              "Mac ",                              TRUE,
  # "\\bMac(?=[a-z])", "Mac ", FALSE,
  # "\\bMac,\\s(?=[a-z])", "Mac ", FALSE,
  # "\\bMc\\b", "Mac, \\1", TRUE,
  # "\\bM\\s?['ac],?\\s?(?=\\w)", "Mac ", FALSE,
  # "\\bMain\\sSt?reee?t\\b", "Main Street", TRUE, 
  "\\bMain\\sStreet.+?([A-Z]|off)",                                   "Main Street, \\1",                  FALSE,
  "\\bMain\\sStreet([A-Z]|off)",                                      "Main Street, \\1",                  FALSE,
  "\\bMa[il]tl(?:an)?d?\\b\\.?",                                      "Maitland",                          TRUE,
  "\\bMarg(?:aret)?\\b\\.?",                                          "Margaret",                          TRUE,
  "\\bMarlboro(?:ugh)?\\b\\.?",                                       "Marlborough",                       TRUE,
  "\\bMary\\s?hill\\b",                                               "Maryhill",                          TRUE,
  "\\bMax-?we?l?l?\\b\\.?",                                           "Maxwell",                           TRUE,
  "\\bMaxweltow?n\\b\\.?",                                            "Maxweltown",                        TRUE,
  "\\bMayfieldpl\\b\\.?",                                             "Mayfield Place",                    TRUE,
  "\\bMenstrie\\b",                                                   "Menstrie",                          TRUE,
  "\\bMerch(?:an)?t?\\b\\.?",                                         "Merchant",                          TRUE,
  "\\bMiddleton\\b",                                                  "Middleton",                         TRUE,
  "\\bMile(?:[\\-\\s]*)?e[nu]d\\b\\.?",                               "Mile-end",                          TRUE,
  "\\bMill[ae]r\\b",                                                  "Miller",                            TRUE,
  "\\bMill?Road\\b",                                                  "Millroad",                          TRUE,
  "\\bMilt(?:on)?\\b\\.?",                                            "Milton",                            TRUE,
  "\\bMilton 3 t",                                                    "Milton Street",                     TRUE,
  "\\bMonteith\\b",                                                   "Monteith",                          TRUE,
  "\\bMontrose\\b",                                                   "Montrose",                          TRUE,
  "\\bMoore?\\b\\.?",                                                 "Moore",                             TRUE,
  "\\bMorrison\\b",                                                   "Morrison",                          TRUE,
  "\\bMuslin\\b",                                                     "Muslin",                            TRUE,
  "\\bMuslin\\s+Street[,\\s]+Br\\w+",                                 "Muslin Street, Bridgeton",          TRUE,
  "\\bNelson\\b",                                                     "Nelson",                            TRUE,
  "\\bNels[ou][nu]\\b.+?Street\\.?",                                  "Nelson Street",                     TRUE,
  "\\bNewhall\\b",                                                    "Newhall",                           TRUE,
  "\\bNewst(?:reet)?\\b\\.?",                                         "New Street",                        TRUE,
  "\\bNew\\s?Cit[vy}].+?Road\\.?",                                    "New City Road",                     TRUE,
  "\\bNew\\s?ven(?:ne\\s?l)?\\b\\.?",                                 "New Vennel",                        TRUE,
  "\\bNewt(?:o[an])?\\b\\.?",                                         "Newton",                            TRUE,
  "\\bNew\\s?wyn?d?\\b\\.?",                                          "New Wynd",                          TRUE,
  "\\bNich?ols(?:on)?\\b\\.?",                                        "Nicholson",                         TRUE,
  "\\bNile\\b",                                                       "Nile",                              TRUE,
  "\\bNiles[ht]\\b",                                                  "Nile Street",                       TRUE,
  "\\bNile\\sstf\\b",                                                 "Nile Street",                       TRUE,
  "\\bNorf(?:ol)?k\\b\\.?",                                           "Norfolk",                           TRUE,
  "\\bNormalplace\\b",                                                "Normal Place",                      TRUE,
  "\\bNorth\\b",                                                      "North",                             TRUE,
  "\\bNorth\\sCityrd\\b",                                             "North City Road",                   TRUE,
  "\\bNorth\\s+Courts?.+R.+\\sE.+",                                   "North Court, Royal Exchange",       TRUE,
  "\\bNorth\\s+Dal.+arno.+\\sRoad\\.?",                               "North Dalmarnock Road",             TRUE,
  "\\bNorth\\s+\\bHano?v?(?:er)?\\b\\.?(?:.+Street)?",                "North Hanover Street",              TRUE,
  "\\bNorth\\s+\\bPortl?v?(?:and)?\\b\\.?(?:.+Street)?",              "North Portland Street",             TRUE,
  "\\bOakfiel[dm]\\b",                                                "Oakfield",                          TRUE,
  "\\bOswa?ld",                                                       "Oswald",                            TRUE,
  "\\bO[nr][ers]\\b",                                                 "Orr",                               TRUE,
  "\\bOn\\b-(?=\\sstreet)",                                           "Orr",                               TRUE,
  "\\bOsborne\\b",                                                    "Osborne",                           TRUE,
  "\\bOswald\\b",                                                     "Oswald",                            TRUE,
  "\\bOxf(?:or)?d?\\b\\.?",                                           "Oxford",                            TRUE,
  "\\bPais?(?:ley)?\\b'?\\.?",                                        "Paisley",                           TRUE,
  "\\bPaisleyrd\\b\\.?",                                              "Paisley Road",                      TRUE,
  "\\bPa[ir]l?i?a?(?:m|rn)?e?n?t?a?r?y?\\b[\\.,]?(?=\\s+R(?:oa)?d)",  "Parliamentary",                     TRUE,
  "\\bParkgro\\b\\.?",                                                "Parkholm",                          TRUE,
  "\\bParkho(?:use)?\\b\\.?",                                         "Parkhouse",                         TRUE,
  "\\bPart(?:ic)?k?\\b\\.?",                                          "Partick",                           TRUE,
  "\\bParson\\b",                                                     "Parson",                            TRUE,
  "\\bPaters(?:on)?\\b\\.?",                                          "Paterson",                          TRUE,
  "\\bPeel\\b",                                                       "Peel",                              TRUE,
  "\\bPiccadilly\\b",                                                 "Piccadilly",                        TRUE,
  "\\bPitt\\b",                                                       "Pitt",                              TRUE,
  "\\bPolloc?k\\b",                                                   "Pollock",                           TRUE,
  "\\bPolloksh?a?w?s?\\b\\.?(?=\\s+?Road|$)",                         "Pollokshaws",                       TRUE,
  "(?<=street,\\s)\\bPolloks(?:haws)?\\b\\.?",                        "Pollokshaws",                       TRUE,
  "\\bPoor(?:\\s?house)?\\b",                                         "Poorhouse",                         TRUE,
  "\\bPort\\b.+?Dundas.+Road\\.?",                                    "Port-Dundas Road",                  TRUE,
  "\\bPort[,\\s\\-]\\s?(\\w)",                                        "Port-\\1",                          TRUE,
  "\\b[Pp]ort([A-Z])",                                                "Port-\\1",                          TRUE,
  "\\bPort\\b[\\s\\-,]\\bStreet\\b\\.?",                              "Port Street",                       TRUE,
  "\\bPort(?:-)er\\b",                                                "Porter",                            TRUE,
  "\\b[Pp]ort(?!-|\\s|Dundas)[il]?a?[ns]?d?\\b\\.?",                  "Portland",                          TRUE,
  "\\bPort(?:-)land\\b",                                              "Portland",                          TRUE,
  "\\bPort(?:-)ugal\\b",                                              "Portugal",                          TRUE,
  "\\bPrince'?s'?[\\s\\–]+(\\w+)",                                    "Princes \\1",                       TRUE,
  "\\bProspect\\b",                                                   "Prospect",                          TRUE,
  "\\bQueen\\b",                                                      "Queen",                             TRUE,
  "\\bQueen'?s'?[\\s\\–]+(\\w+)",                                     "Queens \\1",                        TRUE,
  "\\bRailw(?:ay)?\\b\\.?",                                           "Railway",                           TRUE,
  "\\bReg(?:en)?t?\\b\\.?",                                           "Regent",                            TRUE,
  "\\b[BR]egent\\b",                                                  "Regent",                            TRUE,
  "\\bReid(?:st)?\\b.+(?:Street\\.?|Bridg.+)",                        "Reid Street, Bridgeton",            TRUE,
  "\\bRe+n\\s?[fil](?:iel)?(?:cl|d)?\\b\\.?",                         "Renfield",                          TRUE,
  "\\bRenfieldst(?:reet)?\\b\\.?",                                    "Renfield Street",                   TRUE,
  "\\bRenfr?(?:ew)?\\b\\.?",                                          "Renfrew",                           TRUE, 
  "\\bRichard\\b",                                                    "Richard",                           TRUE, 
  "\\bRenfrewst(?:reet)?\\b\\.?",                                     "Renfrew Street",                    TRUE,
  "\\bRol(?:lo)?[sx?]\\b\\.?",                                        "Rollox",                            TRUE,
  "\\bRob(?:er)?t?s?(?:on)?\\b\\.?",                                  "Robertson",                         TRUE,
  "ropeworks",                                                        "Ropeworks",                         TRUE,
  "\\bRosebank\\b.+$",                                                "Rosebank, Garngad road",            TRUE,
  "\\bRose\\b",                                                       "Rose",                              TRUE,
  "\\bRosest(?:reet)?\\b\\.?",                                        "Rose Street",                       TRUE,
  "\\bRose\\s+?Street.+?Hutchesontown\\b",                            "Rose Street, Hutchesontown",        TRUE,
  "\\bRosehal(?:\\b]|l\\b)?",                                         "Rosehall",                          TRUE,
  "\\b[KR]otte[nu]r(?:ow)?\\b",                                       "Rottenrow",                         TRUE,
  "\\bRoy(?:al)?\\.?",                                                "Royal",                             TRUE,
  "\\bR.+\\bExc?h?(?:an)?(?:ange)?\\b\\.?",                           "Royal Exchange",                    TRUE,
  "\\bR[un]mford\\b",                                                 "Rumford",                           TRUE,
  # "\\bR[un]mford\\sStreet[,\\s]Bridg(?:eton)?\\b\\.?", "Rumford Street, Bridgeton", TRUE,
  "\\bRuther([^f])(?(1).+)",                                          "Rutherglen Loan",                   TRUE,
  "\\bRutland\\b",                                                    "Rutland",                           TRUE,
  "^Saint\\s+James\\s+Street.+kin.+",                                 "Saint James Street, Kingston",      TRUE,
  "^Saint Rollox.+",                                                  "Saint Rollox",                      TRUE,
  "\\bSaint\\sVincentpl\\b",                                          "Saint Vincent Place",               TRUE,
  "\\bSalisb(?:ury)?\\b\\.?",                                         "Salisbury",                         TRUE,
  "\\bSandy?\\s?f?(?:ord)?\\b\\.?",                                   "Sandyford",                         TRUE,
  "\\bSa[nu][ce][bhl]?i?i?e?[bh]?a?(?:ll)?\\b\\.?",                   "Sauchiehall",                       TRUE,
  "\\bSau'?c(?:hi|M)e(?:ha[il]l)?\\b",                                "Sauchiehall",                       TRUE,
  "\\bSauch.+?Str?e?e?t?",                                            "Sauchiehall Street",                TRUE,
  "\\bShamr?(?:ock)?\\b\\.?",                                         "Shamrock",                          TRUE,
  "\\bShawl(?:an)?ds\\b\\.?",                                         "Shawlands",                         TRUE,
  "\\bShawsrd\\b\\.?",                                                "Shaws Road",                        TRUE,
  "\\bShettles(?:ton)?\\b\\.?",                                       "Shettleston",                       TRUE,
  "\\bShieldhall\\b",                                                 "Shieldhall",                        TRUE,
  "\\bShuttle\\b",                                                    "Shuttle",                           TRUE,
  "\\bSo(?:uth)?ciety\\b",                                            "Society",                           TRUE,
  "\\bSo(?:uth)?ho\\b",                                               "Soho",                              TRUE,
  "\\bSo(?:uth)?merset\\b",                                           "Somerset",                          TRUE,
  "\\bSomm?erville\\b\\.?",                                           "Somerville",                        TRUE,
  "\\bSo(?:uth)?mm?erville\\b\\.?",                                   "Somerville",                        TRUE,
  "\\bSouth\\sside.+harb.+",                                          "South side, Glasgow Harbour",       TRUE,
  "\\bSp.?outmout[bh]\\b",                                            "Spoutmouth",                        TRUE,
  "\\bSpringf(?:iel)?d?\\b",                                          "Springfield",                       TRUE,
  "\\bSpringfield\\b[,\\s]+(?!lane|court|place).+",                   "Springfield.",                      TRUE,
  "\\bStanhope\\b",                                                   "Stanhope",                          TRUE,
  "\\bStanley\\b",                                                    "Stanley",                           TRUE,
  "\\bStirl(?:ing)?\\b\\.?",                                          "Stirling",                          TRUE,
  "\\bStevenson\\b",                                                  "Stevenson",                         TRUE,
  "\\bStewart\\b",                                                    "Stewart",                           TRUE,
  "\\bStirling\\b",                                                   "Stirling",                          TRUE,
  "\\bStobc?r?o?[3s]?[8as]?\\b\\.?",                                  "Stobcross",                         TRUE,
  "\\bSt[osu]bc?r?o?s?[as]?\\b\\.?(?:\\s+Street|\\s+\\w+[,.$])?",     "Stobcross Street",                  TRUE,
  "\\bStock[\\-\\s]?w?e?l?l?(?:ell)?\\b\\.?",                         "Stockwell",                         TRUE,
  "\\bStockwell\\s\\b[A-Z]\\b\\.?",                                   "Stockwell",                         TRUE,
  "\\bSuffolk\\b",                                                    "Suffolk",                           TRUE,
  "\\bSurrey\\b",                                                     "Surrey",                            TRUE,
  "\\bStrut[bh]erss?t?\\b\\.?(?:\\s+street)?",                        "Struthers Street",                  TRUE,
  "\\bTenn?[ae]nt\\b\\.?",                                            "Tennent",                           TRUE,
  "\\bThis(?:tle)?\\b\\.?",                                           "Thistle",                           TRUE,
  "\\bThomson\\b",                                                    "Thomson",                           TRUE,
  "\\bT[ao]bago\\.?",                                                 "Tobago",                            TRUE,
  "\\bTobagost(?:reet)?\\b\\.?",                                      "Tobago Street",                     TRUE,
  "\\bTo\\s?wnh(?:ead)?\\b\\.?",                                      "Townhead",                          TRUE,
  "\\bTowns?(?:\\s+)?mill\\b\\.?",                                    "Townmill",                          TRUE,
  "\\bTradesto\\s?[Bn]\\b",                                           "Tradeston",                         TRUE,
  "\\bTr(?:ad)?e?s?t?o?[bnu]?\\b\\.?",                                "Tradeston",                         TRUE,
  "\\btrade[3s]\\s?ton\\b\\.?",                                       "Tradeston",                         TRUE,
  "\\bTro[dn]g(?:at)?[ce]?\\b\\.?",                                   "Trongate",                          TRUE,
  "\\bTr.+?gate\\b\\.?",                                              "Trongate",                          TRUE,
  "\\bUnion\\b\\.?",                                                  "Union",                             TRUE,
  "\\bUpper(?:\\s+)?fauld\\b\\.?",                                    "Upperfauld",                        TRUE,
  "\\bVict(?:oria)?\\b\\.?",                                          "Victoria",                          TRUE,
  "\\bViewpark\\b",                                                   "Viewpark",                          TRUE,
  "\\bVin[ec]?e?n?t?\\b\\.?",                                         "Vincent",                           TRUE,
  "(?<=\\.)\\bVincentpl\\b\\.?",                                      ", Vincent Place",                   TRUE,
  "\\bViolet\\s+gro(?:ve)?\\b\\.?",                                   "Violet grove",                      TRUE,
  "\\bVirg(?:inia)?\\b\\.?",                                          "Virginia",                          TRUE,
  "\\bWalm.+cr.+\\b\\.?",                                             "Walmer Crescent",                   TRUE,
  "\\b(?:AV|W)alworth\\b",                                            "Walworth",                          TRUE,
  "\\bWa[nr]r?(?:oc[bh])?\\b\\.?",                                    "Warroch",                           TRUE,
  "\\bWashingt(?:on)?\\b\\.?",                                        "Washington",                        TRUE,
  "\\bWatt\\b",                                                       "Watt",                              TRUE,
  "\\bWater\\b-?\\.?",                                                "Water",                             TRUE,
  "\\bWaterloo\\b",                                                   "Waterloo",                          TRUE,
  "\\bWellcroft\\b",                                                  "Wellcroft",                         TRUE,
  "\\bWelling(?:on)?\\b\\.?",                                         "Wellington",                        TRUE,
  "\\bWemyss?\\b\\.?",                                                "Wemyss",                            TRUE,
  "\\bWest\\s+str.+(?:\\s+)?trad.+\\b\\.?",                           "West Street, Tradeston",            TRUE,
  "\\bWestern\\b",                                                    "Western",                           TRUE,
  "\\bWestmuir\\b",                                                   "Westmuir",                          TRUE,
  "\\bWhitehall\\b",                                                  "Whitehall",                         TRUE,
  "\\bWillowb(?:an)?k?\\b\\.?",                                       "Willowbank",                        TRUE,
  "\\bW(?:illia)?m\\b\\.?",                                           "William",                           TRUE,
  "\\bWilson\\b",                                                     "Wilson",                            TRUE,
  "\\bWin(?:dsor)?\\b\\.?(?:\\s+)?(place|Terrace|Street)?",           "Windsor \\1",                       TRUE,
  "\\bWoodla(?:nds)?\\b\\.?",                                         "Woodlands",                         TRUE,
  "\\bWoodsi(?:de)?\\b\\.?",                                          "Woodside",                          TRUE,
  "\\bYork\\b",                                                       "York",                              TRUE,
  "\\bYoungs\\b",                                                     "Young street",                      TRUE
)

## Numbers ####
numbers <- tibble::tribble(
  ~pattern,                                   ~replacement, ~ignore.case,
  "(?<=\\d|^)[\\]iIl](?![A-Za-z])",            "1",         FALSE,
  "[iIl](?=\\d)",                              "1",         FALSE,
  "(?<=\\s)[iIl](?=,|\\s)",                    "1",         FALSE,
  "(?<=\\d)S-'(?![A-Za-z])",                   "3",         FALSE,
  "S-'(?=\\d)",                                "3",         FALSE,
  "(?<=\\s)S-'(?=,|\\s)",                      "3",         FALSE,
  "(?<=\\d)(?:C|G|<J)(?![A-Za-z])",            "6",         FALSE,
  "(?:C|G|<J)(?=\\d)",                         "6",         FALSE,
  "(?<=\\s)(?:C|G|<J)(?=,|\\s)",               "6",         FALSE,
  "(?<=\\d)T(?![A-Za-z])",                     "7",         FALSE,
  "T(?=\\d)",                                  "7",         FALSE,
  "(?<=\\s)T(?=,|\\s)",                        "7",         FALSE,
  "(?<=\\d)S(?![A-Za-z])",                     "8",         FALSE,
  "S(?=\\d)",                                  "8",         FALSE,
  "(?<=\\s)S(?=,|\\s)",                        "8",         FALSE,
  "(?<=\\d)S(?=[A-Z])",                        "8",         FALSE,
  "\\blu\\b",                                  "10",        FALSE,
  "\\bIS\\b",                                  "18",        FALSE,
  "\\bCI\\b",                                  "61",        FALSE,
  "\\bfl\\b",                                  "71",        FALSE,
  "\\bIll\\b",                                 "111",       FALSE,
  "\\bllo(?=[A-Z])",                           "113",       FALSE,
  "\\bHi\\b",                                  "114",       FALSE,
  "(?<=\\d)-?(\\.5|£|\\^|\\||\\\\|f|h|i|j)-?", "1/2",       TRUE,
  "(?<=\\s)j(?=\\s)",                          "1/2",       TRUE
)

## Places ####

### Raw ####
places_raw <- c(
  "Academy",
  "Arcade",
  "Arches",
  "Bay",
  "Bank",
  "Bazaar",
  "Brewery",
  "Bridge",
  "Buildings",
  "Brickfields",
  "Cabinet",
  "Castle",
  "City",
  "Circus",
  "Close", 
  "Colliery",
  "Cottage",
  "Court", 
  "Crescent",
  "Dairy",
  "Drive",
  "Dyeworks",
  "Entrance",
  "Entry",
  "Establishment",
  "Factory",
  "Foundry",
  "Garden",
  "Glebe",
  "Harbour",
  "Hotel",
  "House",
  "Hill",
  "Ironworks",
  "Lane", 
  "Loan", 
  "Lodge",
  "Manufactory",
  "Market",
  "Mill",
  "Place", 
  "Point",
  "Port", 
  "Quarry",
  "Quay",
  "Road",
  "Row",
  "Show-rooms",
  "Square",
  "Stables",
  "Stand",
  "Store",
  "Street", 
  "Tanyard",
  "Terrace", 
  "Vennel",
  "Vaults",
  "Villa",
  "Warehouse",
  "Wynd"
)

### Regex ####
places_regex <- tibble::tribble(
  ~pattern,                                         ~replacement,           ~ignore.case,
  "\\bAcademy\\b",                                  "Academy",              TRUE,
  "((?:Argyle)[\\s.]?)?Arc(?:ade)?\\b",             "\\1Arcade",            TRUE,
  "\\barc?(?:ade)\\b\\.?",                          "Arcade",               TRUE,
  "\\bArches\\b",                                   "Arches",               TRUE,
  "\\barch(?:es)?\\b\\.?",                          "Arches",               TRUE,
  "\\bBay\\b",                                      "Bay",                  TRUE,
  "\\bBazaar\\b",                                   "Bazaar",               TRUE,
  "\\bBank\\b",                                     "Bank",                 TRUE,
  "\\bBranch\\b",                                   "Branch",               TRUE,
  "\\bBrewery\\b",                                  "Brewery",              TRUE,
  "\\bBrickfields\\b",                              "Brickfields",          TRUE,
  "\\bBridge\\b",                                   "Bridge",               TRUE,
  "\\bBri?d?g?e?\\b\\.?",                           "Bridge",               TRUE,
  "\\bBuildings\\b",                                "Buildings",            TRUE,
  "\\bbds\\b\\.?",                                  "Buildings",            TRUE,
  "\\bCabinet\\b",                                  "Cabinet",              TRUE,
  "\\bCastle\\b",                                   "Castle",               TRUE,
  "\\bCity\\b",                                     "City",                 TRUE,
  "\\bCircus\\b",                                   "Circus",               TRUE,
  "\\bcir(?:cus)?\\b\\.?",                          "Circus",               TRUE,
  "\\bClose\\b",                                    "Close",                TRUE,
  "\\bclo(?:se)?\\b\\.?",                           "Close",                TRUE,
  "\\bColliery\\b",                                 "Colliery",             TRUE,
  "\\bCottage\\b",                                  "Cottage",              TRUE,
  "\\bCourt\\b",                                    "Court",                TRUE,
  "\\bco?(?:ur)?t?\\b,?\\.?",                       "Court",                TRUE,
  "\\bCrescent\\b",                                 "Crescent",             TRUE,
  "\\bcre?s?c?(?:en)?t?\\b\\.?",                    "Crescent",             TRUE,
  "\\bCustoms\\b",                                  "Customs",              TRUE,
  "[HM].+?Customs\\b",                              "Her Majestys Customs", TRUE,
  "\\bDairy\\b",                                    "Dairy",                TRUE,
  "\\bDrive\\b",                                    "Drive",                TRUE,
  "\\bdri?v?e?\\b\\.?",                             "Drive",                TRUE,
  "\\bDyeworks\\b",                                 "Dyeworks",             TRUE,
  "\\bEntrance\\b",                                 "Entrance",             TRUE,
  "\\bEntry\\b",                                    "Entry",                TRUE,
  "\\bEstab(?:lishment)?\\b",                       "Establishment",        TRUE,
  "\\b(Biscuit\\s)?Factory?\\b",                    "\\1 Factory",          TRUE,
  "\\bFoundry\\b",                                  "Foundry",              TRUE,
  "\\bGarden(s)?\\b",                               "Garden\\1",            TRUE,
  "\\bgl(?:ebe)?\\b\\.?",                           "Glebe",                TRUE,
  "\\bHarbour\\b",                                  "Harbour",              TRUE,
  "\\bHill\\b",                                     "Hill",                 TRUE,
  "\\bHotel\\b",                                    "Hotel",                TRUE,
  "\\bj?[hl]ouse\\b",                               "House",                TRUE,
  "\\bIronworks\\b",                                "Ironworks",            TRUE,
  "\\bPost Office\\b",                              "Post Office",          FALSE,
  "P\\.\\s?O\\.\\s?",                               "Post Office",          FALSE,
  "\\bP.+?O(?:ffice)",                              "Post Office",          FALSE,
  "\\bPost Office(?:\\w+)?",                        "Post Office",          TRUE,
  "\\bGeneral Post Office\\b",                      "General Post Office",  FALSE,
  "G\\.\\s?Post Office\\b",                         "General Post Office",  FALSE,
  "\\bLane\\b",                                     "Lane",                 TRUE,
  "\\b[Il][an](?:ne)?\\b\\.?",                      "Lane",                 TRUE,
  "\\bLoan\\b",                                     "Loan",                 TRUE,
  "\\bLodge\\b",                                    "Lodge",                TRUE,
  "\\bManufactory\\b",                              "Manufactory",          TRUE,
  "(Cattle|Fish|S[an]lt)?[\\s.]Market\\b",          "\\1Market",            TRUE,
  "(Cattle|Fish|S[an]lt)?Market\\b",                "\\1 Market",           TRUE,
  "\\bMill\\b",                                     "Mill",                 TRUE,
  "\\bPlace\\b",                                    "Place",                TRUE,
  "\\bp[il]a?(?:ce)?\\b\\.?",                       "Place",                TRUE,
  "_place\\b",                                      "Place",                TRUE,
  "\\bPoint\\b",                                    "Point",                TRUE,
  "\\bPort\\b",                                     "Port",                 TRUE,
  "\\bp(?:or)?t?\\b[,.]?",                          "Port",                 TRUE,
  "\\bQuay\\b",                                     "Quay",                 TRUE,
  "\\bq[uy]?[oa]?y?\\b\\.?",                        "Quay",                 TRUE,
  "\\bQuarry\\b",                                   "Quarry",               TRUE,
  "\\bRoad\\b",                                     "Road",                 TRUE,
  "\\b[ir](?:[eo]a)?d\\b\\.?",                      "Road",                 TRUE,
  "1\\s-\\soad\\b",                                 "Road",                 TRUE,
  "\\brt\\s?L\\b",                                  "Road",                 TRUE,
  "\\bro\\b",                                       "Road",                 TRUE,
  "\\brow\\b",                                      "Row",                  TRUE,
  "\\bReceiving house\\b",                          "Receiving house",      TRUE,
  "\\breceiving\\s?house\\b",                       "Receiving house",      TRUE,
  "\\bShow-rooms\\b",                               "Show-rooms",           TRUE,
  "\\bSquare\\b",                                   "Square",               TRUE,
  "\\bsq[nu]?(?:are)?\\b\\.?",                      "Square",               TRUE,
  "\\bStables\\b",                                  "Stables",              TRUE,
  "\\bStand\\b",                                    "Stand",                TRUE,
  "\\bStore\\b",                                    "Store",                TRUE,
  "\\bStreet",                                      "Street",               TRUE,
  "(?<!^)\\b[iaes]t[ri]?[ie]?(?:et)?\\b\\.?",       "Street",               TRUE,
  "\\bsh\\b\\.",                                    "Street",               TRUE,
  "\\bs[ft][er]eet\\b\\.?",                         "Street",               TRUE,
  "\\bsr\\b\\.?",                                   "Street",               TRUE,
  "\\bstr[e']+t\\b\\.?",                            "Street",               TRUE,
  "sfc",                                            "Street",               TRUE,
  "\\bstl\\b",                                      "Street",               TRUE,
  "(street)(?=[a-z])",                              "\\1 ",                 TRUE,
  "(street),(?=[a-z])",                             "\\1 ",                 TRUE,
  "\\b(street)\\b[^\\s,]?(?=\\s)",                  "\\1 ",                 TRUE,
  "(\\w)\\1st",                                     "\\1\\1 Street",        TRUE,
  "3\\street",                                      "Street",               TRUE,
  "\\bTanyard\\b",                                  "Tanyard",              TRUE,
  "\\bTerrace\\b",                                  "Terrace",              TRUE,
  "\\bte[nr](?:[r\\-]ace)?\\b\\.?",                 "Terrace",              TRUE,
  "\\bVaults\\b",                                   "Vaults",               TRUE,
  "\\bVennel\\b",                                   "Vennel",               TRUE,
  "\\bVe[nr][mn]el\\b\\.?",                         "Vennel",               TRUE,
  "\\bVilla(s)?\\b",                                "Villa\\1",             TRUE,
  "\\bWarehouse\\b",                                "Warehouse",            TRUE,
  "Whole[ps]ale\\b",                                "Wholesale",            TRUE,
  "\\bWynd\\b",                                     "Wynd",                 TRUE,
  "\\bw[jy]?'?n?d?\\b\\.?",                         "Wynd",                 TRUE
)


## Saints ####
saints <- tibble::tribble(
  ~pattern,           ~replacement,   ~ignore.case,
  "Andr?e?w",         "Andrew",       TRUE,
  "Ann",              "Ann",          TRUE,
  "David",            "David",        TRUE,
  "En(?:och)?",       "Enoch",        TRUE,
  "Geo(?:rg)?e?",     "Georges",      TRUE,
  "Ja(?:me)?s",       "James",        TRUE,
  "John",             "John",         TRUE,
  "Joseph",           "Joseph",       TRUE,
  "Marg(?:aret)?",    "Margaret",     TRUE,
  "Mark",             "Mark",         TRUE,
  "Marnock",          "Marnock",      TRUE,
  "Mary",             "Mary",         TRUE,
  "Mungo",            "Mungo",        TRUE,
  "Nicholas",         "Nicholas",     TRUE,
  "Ninian",           "Ninian",       TRUE,
  "Paul",             "Paul",         TRUE,
  "Peter",            "Peter",        TRUE,
  "Rol(?:lo)?[sx?]",  "Rollox",       TRUE,
  "Vin[ec]?e?n?t?-?", "Vincent",      TRUE
)

## Suffixes ####
suffixes <- tibble::tribble(
  ~pattern,                               ~replacement, ~ignore.case,
  "\\bNo?(?:rth)?\\b(?!\\shome)\\.?",     "North",      TRUE,
  "(?<!\\')\\bSo?u?(?:th)?\\b\\.?",       "South",      TRUE,
  "\\bSo(?=[A-Z])",                       "South",      TRUE,
  "\\bso(uth)\\1",                        "South",      TRUE,
  "\\bWe?(?:st)?\\b\\.?",                 "West",       TRUE,
  "(?<!')\\bEa?(?:st)?\\b\\.?",           "East",       TRUE,
  "\\bLittle\\b",                         "Little",     TRUE,
  "\\bM(?:ai)?ns?\\b\\.?",                "Main",       TRUE,
  "\\b(main)([a-z]\\.?)",                 "\\1 \\2",    TRUE,
  "\\bGr?(?:ea)?t?\\b(?!\\.P)",           "Great",      TRUE,
  "\\bg(?!\\.P)\\b",                      "Great",      TRUE,
  "\\bC\\.(?=\\sclyde)",                  "Great",      TRUE,
  "\\bUp?p?e?r?\\b\\.?",                  "Upper",      TRUE
)


## Worksites ####
worksites <- tibble::tribble(
  ~pattern,       ~replacement,       ~ignore.case,
  "Woodyard",     "Woodyard",         TRUE
)

















































# People ####

## Forenames ####
forenames <- tibble::tribble(
  ~pattern,                                 ~replacement,                ~ignore.case,
  "\\bAbr(?:aham)?\\b\\.?",                 "Abraham",                   TRUE,
  "\\bAllan\\b",                            "Allan",                     TRUE,
  "\\bAle[sx](?:and[ae]r)?\\b\\.?",         "Alexander",                 TRUE,
  "\\bAnd(?:re)?w?\\b\\.?",                 "Andrew",                    TRUE,
  "\\bAng(?:us)?\\b\\.?",                   "Angus",                     TRUE,
  "\\bArch?i?b?(?:al)?d?\\b\\.?",           "Archibald",                 TRUE,
  "\\bArt[ht](?:ur)?\\b\\.?",               "Arthur",                    TRUE,
  "\\bA\\b\\.?",                            "Alexander",                 TRUE,
  "\\bBenj?(?:amin)?\\b\\.?",               "Benjamin",                  TRUE,
  "\\bBern?(?:ard)?\\b\\.?",                "Bernard",                   TRUE,
  "\\bBro(?:th)?(?:er)?s?\\b\\.?",          "Brothers",                  TRUE,
  "\\bB\\b\\.?",                            "Bernard",                   TRUE,
  "\\bCamp?(?:bell)?\\b\\.?",               "Campbell",                  TRUE,
  "\\bCat(?:h|li)(?:erine)?\\b\\.?",        "Catherine",                 TRUE,
  "\\bCha?(?:rle)?s?\\b\\.?",               "Charles",                   TRUE,
  "\\bChristo?(?:pher)?\\b\\.?",            "Christopher",               TRUE,
  "\\bCor?(?:nelius)?\\b\\.?",              "Cornelius",                 TRUE,
  "\\bC\\b\\.?",                            "Colin",                     TRUE,
  "\\bDan(?:iel)?\\b\\.?",                  "Daniel",                    TRUE,
  "\\bDavid\\b",                            "David",                     TRUE,
  "\\bDom(?:inic)?k?\\b\\.?",               "Dominick",                  TRUE,
  "\\bDon(?:ald)?\\b\\.?",                  "Donald",                    TRUE,
  "\\bDug(?:ald)?\\b\\.?",                  "Dugald",                    TRUE,
  "\\bDunc?(?:an)?\\b\\.?",                 "Duncan",                    TRUE,
  "\\bD\\b\\.?",                            "David",                     TRUE,
  "\\bEaton\\b",                            "Eaton",                     TRUE,
  "\\bEben(?:ezer)?\\b\\.?",                "Ebenezer",                  TRUE,
  "\\bEdm(?:und)?\\b\\.?",                  "Edmund",                    TRUE,
  "\\bEdw(?:ar)?d?\\b\\.?",                 "Edward",                    TRUE,
  "\\bEliza?b?(?:eth)?\\b\\.?",             "Elizabeth",                 TRUE,
  "\\bE\\b\\.?",                            "Edward",                    TRUE,
  "\\bFran(?:cis)?\\b\\.?",                 "Francis",                   TRUE,
  "\\bFred(?:eric)?k?\\b\\.?",              "Frederick",                 TRUE,
  "\\bF\\b\\.?",                            "Frederick",                 TRUE,
  "\\bGabriel\\b",                          "Gabriel",                   TRUE,
  "\\bGavin\\b",                            "Gavin",                     TRUE,
  "\\bGeo(?:rge)?\\b\\.?",                  "George",                    TRUE,
  "\\bGil(?:bert)?\\b\\.?",                 "Gilbert",                   TRUE,
  "\\bGodf(?:rey)?\\b\\.?",                 "Godfrey",                   TRUE,
  "\\bG\\b\\.?",                            "George",                    TRUE,
  "\\bHar(?:ry)?\\b\\.?",                   "Harry",                     TRUE,
  "\\bHector\\b",                           "Hector",                    TRUE,
  "\\bH(enr)?(?(1)y|y)\\b\\.?",             "Henry",                     TRUE,
  "\\bHen(?:ry)?\\b\\.?",                   "Henry",                     TRUE,
  "\\bHu(?:gh)?\\b\\.?",                    "Hugh",                      TRUE,
  "\\bH\\b\\.?",                            "Hugh",                      TRUE,
  "\\bI\\b\\.?",                            "Isaac",                     TRUE,
  "\\bJacob\\b",                            "Jacob",                     TRUE,
  "\\bJa?(?:me)?s\\b\\.?",                  "James",                     TRUE,
  "\\bJane\\b",                             "Jane",                      TRUE,
  "\\bJanet\\b",                            "Janet",                     TRUE,
  "\\bJasp(?:er)?\\b\\.?",                  "Jasper",                    TRUE,
  "\\bJ(?:[no][bh])?(?:no|[nou])\\b\\.?",   "John",                      TRUE,
  "\\bJ\\b\\.?\\s?W\\.?",                   "John William",              TRUE,
  "\\bJonathan\\b",                         "Jonathan",                  TRUE,
  "\\bJos(?:eph)?\\b\\.?",                  "Joseph",                    TRUE,
  "\\bJosh(?:ua)?\\b\\.?",                  "Joshua",                    TRUE,
  "\\bJ\\b\\.?",                            "John",                      TRUE,
  "\\bKen(neth)?\\b(?(1)|\\.)",             "Kenneth",                   TRUE,
  "\\bLaurence\\b",                         "Laurence",                  TRUE,
  "\\bLawr(?:ence)?\\b\\.?",                "Lawrence",                  TRUE,
  "\\Malc?(?:om)?\\b\\.?",                  "Malcolm",                   TRUE,
  "\\bMarg(?:are)?t?\\b\\.?",               "Margaret",                  TRUE,
  "\\bMary\\b",                             "Mary",                      TRUE,
  "\\bMat(hew)?\\b(?(1)|\\.)",              "Mathew",                    TRUE,
  "\\bMath(?:ew)?\\b",                      "Mathew",                    TRUE,
  "\\bMatth?e?w?\\b\\.?",                   "Matthew",                   TRUE,
  "\\bMich(?:ael)?\\b\\.?",                 "Michael",                   TRUE,
  "\\bMiss?\\b\\.?",                        "Miss",                      TRUE,
  "\\bM\\b\\.?",                            "Matthew",                   TRUE,
  "\\bNorm?(?:an)?\\b\\.?",                 "Norman",                    TRUE,
  "\\bN\\b\\.?",                            "Neil",                      TRUE,
  "\\bPat(?:ric)?k\\b\\.?",                 "Patrick",                   TRUE,
  "\\bPet(?:er)?\\b\\.?",                   "Peter",                     TRUE,
  "\\bPhilip\\b",                           "Philip",                    TRUE,
  "\\bP\\b\\.?",                            "Philip",                    TRUE,
  "\\bRich(?:ar)?d?\\b\\.?",                "Richard",                   TRUE,
  "\\bRitchie\\b",                          "Ritchie",                   TRUE,
  "\\bR(?:ob)?(?:er)?[ot]b?\\b\\.?",        "Robert",                    TRUE,
  "\\bRon(?:ald)?\\b\\.?",                  "Ronald",                    TRUE,
  "\\bRoss\\b",                             "Ross",                      TRUE,
  "\\bR\\b\\.?",                            "Robert",                    TRUE,
  "\\bSam(?:ue)?l?\\b\\.?",                 "Samuel",                    TRUE,
  "\\bStembr(?:idge)?\\b\\.?",              "Stembridge",                TRUE,
  "\\bS\\b\\.?",                            "Samuel",                    TRUE,
  "\\bTho(?:ma)?s?\\b\\.?",                 "Thomas",                    TRUE,
  "\\bTor(?:rance)?\\b\\.?",                "Torrance",                  TRUE,
  "\\bT\\b\\.?",                            "Thomas",                    TRUE,
  "\\bW(?:alte)?r\\b\\.?",                  "Walter",                    TRUE,
  "\\bW(?:illia)?(?:in|m|rn)\\b\\.?",       "William",                   TRUE,
  "\\bW\\b\\.?",                            "William",                   TRUE
)

## Macs ####
macs <- tibble::tribble(
  ~pattern,                     ~replacement,  ~ignore.case,
  "\\bM\\s?['ac],?\\s?(?=\\w)", "Mac",         FALSE,
  # "\\bMac,?\\s?(?=\\w)", "Mac", FALSE,
  # "\\bMac(?=[a-z])", "Mac", FALSE,
  "\\bMac,\\s(?=[a-z])",        "Mac",         FALSE
)

## Surnames ####
surnames <- tibble::tribble(
  ~pattern,                                         ~replacement,               ~ignore.case,
  "\\bAbbott?\\b\\.?",                              "Abbott",                   TRUE,
  "\\bAbercromb(?:ie|y)\\b",                        "Abercromby",               TRUE,
  "\\bAbe(?:m|rn)ethy\\b\\.?",                      "Abernethy",                TRUE,
  "\\bAbrams\\b",                                   "Abrams",                   TRUE,
  "\\bAdam\\b",                                     "Adam",                     TRUE,
  "\\bAdams\\b",                                    "Adams",                    TRUE,
  "\\bA\\s?damson\\b",                              "Adamson",                  TRUE,
  "\\bAdd?ie\\b",                                   "Addie",                    TRUE,
  "\\bAdrain\\b",                                   "Adrain",                   TRUE,
  "\\bAdshead\\b",                                  "Adshead",                  TRUE,
  "\\bAffleck\\b",                                  "Affleck",                  TRUE,
  "\\bA(?:h|ii)n\\b",                               "Ahn",                      TRUE,
  "\\bAgnew\\b\\,?",                                "Agnew",                    TRUE,
  "\\bAiken\\b",                                    "Aiken",                    TRUE,
  "\\bAikenhead\\b",                                "Aikenhead",                TRUE,
  "\\bAikman\\b",                                   "Aikman",                   TRUE,
  "\\bAinslie\\b",                                  "Ainslie",                  TRUE,
  "\\bAird\\b",                                     "Aird",                     TRUE,
  "\\bAitch[ei]son\\b\\.?",                         "Aitchison",                TRUE,
  "\\bAitk[ei]n\\b\\.?",                            "Aitken",                   TRUE,
  "\\bAitkenhead\\b\\.?",                           "Aitkenhead",               TRUE,
  "\\bA[il]ton\\b\\.?",                             "Aiton",                    TRUE,
  "\\bAlbrecht\\b\\.?",                             "Albrecht",                 TRUE,
  "\\bAlexander\\b",                                "Alexander",                TRUE,
  "\\bAlgie\\b",                                    "Algie",                    TRUE,
  "\\bAll?iso[nu]\\b",                              "Alison",                   TRUE,
  "\\bAlla[nu]\\b",                                 "Allan",                    TRUE,
  "\\bAllardice\\b",                                "Allardice",                TRUE,
  "\\bAlle[nu]\\b",                                 "Allen",                    TRUE,
  "\\bAll?iso[nu]\\b",                              "Allison",                  TRUE,
  "\\bAlston\\b",                                   "Alston",                   TRUE,
  "\\bAmatore\\b",                                  "Amatore",                  TRUE,
  "\\bAmbrose\\b",                                  "Ambrose",                  TRUE,
  "\\bAncell\\b",                                   "Ancell",                   TRUE,
  "\\bAnderson\\b\\,?",                             "Anderson",                 TRUE,
  "\\bAndrew\\b",                                   "Andrew",                   TRUE,
  "\\bAndrews\\b\\,?",                              "Andrews",                  TRUE,
  "\\bAngus\\b",                                    "Angus",                    TRUE,
  "\\bAnnacker\\b",                                 "Annacker",                 TRUE,
  "\\bAnnan\\b",                                    "Annan",                    TRUE,
  "\\bArbuckle\\b",                                 "Arbuckle",                 TRUE,
  "\\bArchibald\\b",                                "Archibald",                TRUE,
  "\\bArmour\\b",                                   "Armour",                   TRUE,
  "\\bArmstrong\\b",                                "Armstrong",                TRUE,
  "\\bArneil\\b",                                   "Arneil",                   TRUE,
  "\\bArnott?\\b\\.?",                              "Arnott",                   TRUE,
  "\\bArrol\\b",                                    "Arrol",                    TRUE,
  "\\bArthur\\b",                                   "Arthur",                   TRUE,
  "\\bAsher\\b",                                    "Asher",                    TRUE,
  "\\bAuchincloss\\b",                              "Auchincloss",              TRUE,
  "\\bAuchinvole\\b",                               "Auchinvole",               TRUE,
  "\\bAuchterlonie\\b",                             "Auchterlonie",             TRUE,
  "\\bAuld\\b\\,?",                                 "Auld",                     TRUE,
  "\\bAustin\\b",                                   "Austin",                   TRUE,
  "\\bBail?lie\\b\\.?",                             "Baillie",                  TRUE,
  "\\bBalderston\\b\\,?",                           "Balderston",               TRUE,
  "\\bBannerman\\b\\,?",                            "Bannerman",                TRUE,
  "\\bBarnhill\\b\\,?",                             "Barnhill",                 TRUE,
  "\\bBonn?ar\\b\\,?",                              "Bonnar",                   TRUE,
  "\\bBeale?\\b\\.?",                               "Bealle",                   TRUE,
  "\\bBell\\b\\,?",                                 "Bell",                     TRUE,
  "\\bBla[ce]kie\\b\\.?",                           "Blackie",                  TRUE,
  "\\bBlythe?\\b\\.?",                              "Blyth",                    TRUE,
  "\\bBred?ie\\b\\.?",                              "Breddie",                  TRUE,
  "\\bBrown\\b\\,?",                                "Brown",                    TRUE,
  "\\bBrownl[ei]e\\b\\,?",                          "Brownlie",                 TRUE,
  "\\bBryd[eo]n\\b\\.?",                            "Bryden",                   TRUE,
  "\\bCair[nu]s\\b[\\.,]?",                         "Cairns",                   TRUE,
  "\\bCampbell\\b\\,?",                             "Campbell",                 TRUE,
  "\\bCann[ao]n\\b\\.?",                            "Cannan",                   TRUE,
  "\\bCarswell\\b\\,?",                             "Carswell",                 TRUE,
  "\\bClark\\b\\,?",                                "Clark",                    TRUE,
  "\\bConnell?\\b\\,?",                             "Connell",                  TRUE,
  "\\bCop?land\\b\\.?",                             "Copeland",                 TRUE,
  "\\bCorbett?\\b\\.?",                             "Corbett",                  TRUE,
  "\\bCoverley?\\b\\,?",                            "Coverley",                 TRUE,
  "\\bCowan\\b\\,?",                                "Cowan",                    TRUE,
  "\\bCree\\b\\.?",                                 "Cree",                     TRUE,
  "\\bCruicks[bh]anks?\\b\\.?",                     "Cruickshanks",             TRUE,
  "\\bCunninghame?\\b\\.?",                         "Cunninghame",              TRUE,
  "\\bDansk[ei]n\\b\\.?",                           "Danskin",                  TRUE,
  "\\bDavie?s\\b\\.?",                              "Davies",                   TRUE,
  "\\bDenn?ison\\b\\.?",                            "Dennison",                 TRUE,
  "\\bDickson\\b\\,?",                              "Dickson",                  TRUE,
  "\\bDixon\\b\\,?",                                "Dixon",                    TRUE,
  "\\bDodd?s?\\b\\.?",                              "Dods",                     TRUE,
  "\\bDona[cg]h(?:ie|y)\\b\\.?",                    "Donachy",                  TRUE,
  "\\bDonovan\\b\\,?",                              "Donovan",                  TRUE,
  "\\bDunnett?\\b\\.?",                             "Dunnett",                  TRUE,
  "\\bEast[co]n\\b\\.?",                            "Easton",                   TRUE,
  "\\bEdm[ou]n\\b\\.?",                             "Edmon",                    TRUE,
  "\\bEdm[ao]nds\\b\\.?",                           "Edmands",                  TRUE,
  "\\bEdwards?\\b\\.?",                             "Edwards",                  TRUE,
  "\\bFinn[ae]rty\\b\\.?",                          "Finnerty",                 TRUE,
  "\\bFisk[ei]n\\b\\.?",                            "Fiskin",                   TRUE,
  "\\bFlind?t\\b\\.?",                              "Flindt",                   TRUE,
  "\\bGemm[ei]ll\\b\\.?",                           "Gemmell",                  TRUE,
  "\\bHilli?ard\\b\\.?",                            "Hilliard",                 TRUE,
  "\\bHin[cs]helwood\\b\\.?",                       "Hinshelwood",              TRUE,
  "\\bI(?:m|rn)rie\\b\\.?",                         "Imrie",                    TRUE,
  "\\b,?\\s+\\bjun(?:io)?r?\\b\\.?",                " Junior",                  TRUE,
  "\\bKin[nu]inmont\\b\\.?",                        "Kinninmont",               TRUE,
  "\\bLochh?ead\\b\\.?",                            "Lochhead",                 TRUE,
  "\\bLoudou?n\\b\\.?",                             "Loudon",                   TRUE,
  "\\bMac\\sAins(?:h|li)\\b\\.?",                   "Mac Ainsh",                TRUE,
  "\\bMac\\sAul[ae]y\\b\\.?",                       "Mac Aulay",                TRUE,
  "\\bMac\\sAu?slan\\b\\.?",                        "Mac Auslan",               TRUE,
  "\\bMac\\sBr[iy]de\\b\\.?",                       "Mac Bride",                TRUE,
  "\\bMac\\sCl[ae]uchan\\b\\.?",                    "Mac Cleuchan",             TRUE,
  "\\bMac\\sClem[eo]nt\\b\\.?",                     "Mac Clement",              TRUE,
  "\\bMac\\sCluske?y\\b\\.?",                       "Mac Clusky",               TRUE,
  "\\bMac\\sConn?[aeo][cg]h(?:ie|y)\\b\\.?",        "Mac Connochie",            TRUE,
  "\\bMac\\sCorm?[ai]ck\\b\\.?",                    "Mac Cormick",              TRUE,
  "\\bMac\\sCrea?dd?(?:ie|y)\\b\\.?",               "Mac Creadie",              TRUE,
  "\\bMac\\sCrind[el][el]l?\\b\\.?",                "Mac Crindell",             TRUE,
  "\\bMac\\sDow[ae]ll\\b\\.?",                      "Mac Dowall",               TRUE,
  "\\bMac\\sEw[ae]n\\b\\.?",                        "Mac Ewan",                 TRUE,
  "\\bMac\\sFad[yz][ae]?[ae]?n\\b\\.?",             "Mac Fadyen",               TRUE,
  "\\bMac\\sFedrie?s\\b\\.?",                       "Mac Fedries",              TRUE,
  "\\bMac\\sGarre?y\\b\\.?",                        "Mac Garrey",               TRUE,
  "\\bMac\\sGh?ie\\b\\.?",                          "Mac Gie",                  TRUE,
  "\\bMac\\sGlash[ae]n\\b\\.?",                     "Mac Glashan",              TRUE,
  "\\bMac\\sGradd?(?:ie|y)\\b\\.?",                 "Mac Graddie",              TRUE,
  "\\bMac\\sInn[ei]s\\b\\.?",                       "Mac Innis",                TRUE,
  "\\bMac\\s[il]v[eo]r\b\\.?",                      "Mac Iver",                 TRUE,
  "\\bMac\\sKind?l[ae]y\\b\\.?",                    "Mac Innis",                TRUE,
  "\\bMac\\sKird(?:ie|y)\\b\\.?",                   "Mac Kirdy",                TRUE,
  "\\bMac\\sLau?[cg]hl[ai]n\\b\\.?",                "Mac Lachlan",              TRUE,
  "\\bMac\\sLau?r[ei]n\\b\\.?",                     "Mac Laren",                TRUE,
  "\\bMac\\sMill[ea]n\\b\\.?",                      "Mac Millan",               TRUE,
  "\\bMac\\sNaught[aeo]n\\b\\.?",                   "Mac Naughton",             TRUE,
  "\\bMac\\sNeill?\\b\\.?",                         "Mac Neill",                TRUE,
  "\\bMac\\sN[ei]e\\b\\.?",                         "Mac Nie",                  TRUE,
  "\\bMac\\sParl[ea]ne?\\b\\.?",                    "Mac Parlane",              TRUE,
  "\\bMac\\sSymond?\\b\\.?",                        "Mac Symon",                TRUE,
  "\\bMains?\\b\\.?",                               "Mac Main",                 TRUE,
  "\\bMeigh[ae]n\\b\\.?",                           "Meighan",                  TRUE,
  "\\bMeikle?h?am\\b\\.?",                          "Meikleham",                TRUE,
  "\\bMill[ae]r\\b\\.?",                            "Millar",                   TRUE,
  "\\bMillig[ae]n\\b\\.?",                          "Milligan",                 TRUE,
  "\\bMon(?:ie|y)\\b\\.?",                          "Monie",                    TRUE,
  "\\bMontague?\\b\\.?",                            "Montague",                 TRUE,
  "\\bMontgomer(?:ie|y)\\b\\.?",                    "Montgomery",               TRUE,
  "\\bMood(?:ie|y)\\b\\.?",                         "Moodie",                   TRUE,
  "\\bMull[ei]n\\b\\.?",                            "Mullen",                   TRUE,
  "\\bMus(?:li|h)et\\b\\.?",                        "Mushet",                   TRUE,
  "\\bNeill?\\b\\.?",                               "Neil",                     TRUE,
  "\\bNei?lson?\\b\\.?",                            "Neilson",                  TRUE,
  "\\bNich?ol\\b\\.?",                              "Nichol",                   TRUE,
  "\\bNisbett?\\b\\.?",                             "Nichol",                   TRUE,
  "\\bNotma(?:ri|n)\\b\\.?",                        "Nichol",                   TRUE,
  "\\bO'Neill?\\b\\.?",                             "O'Neill",                  TRUE,
  "\\bOas?tt?\\b\\.?",                              "Oatt",                     TRUE,
  "\\bOgilv(?:ie|y)\\b\\.?",                        "Ogilvie",                  TRUE,
  "\\bOwens?\\b\\.?",                               "Owens",                    TRUE,
  "\\bPatt?erson\\b\\.?",                           "Patterson",                TRUE,
  "\\bPattie?son\\b\\.?",                           "Pattison",                 TRUE,
  "\\bPenne?y\\b\\.?",                              "Penney",                   TRUE,
  "\\bPennyc[ou][oi]c?k\\b\\.?",                    "Pennycook",                TRUE,
  "\\bPerr(?:ie|y)\\b\\.?",                         "Pennycook",                TRUE,
  "\\bPhillips?\\b\\.?",                            "Phillips",                 TRUE,
  "\\bPhilps?\\b\\.?",                              "Philps",                   TRUE,
  "\\bPigott?\\b\\.?",                              "Pigott",                   TRUE,
  "\\bPolloc?k\\b\\.?",                             "Pigott",                   TRUE,
  "\\bPriestle?y\\b\\.?",                           "Pigott",                   TRUE,
  "\\bRankine?\\b\\.?",                             "Rankine",                  TRUE,
  "\\bRichards?\\b\\.?",                            "Richard",                  TRUE,
  "\\bRodd[ea]n\\b\\.?",                            "Roddan",                   TRUE,
  "\\bRod?gers?\\b\\.?",                            "Rodger",                   TRUE,
  "\\bRowand?\\b\\.?",                              "Rowand",                   TRUE,
  "\\bSalmond?\\b\\.?",                             "Salmond",                  TRUE,
  "\\b,?\\s+\\bsen(?:ior)?\\b\\.?",                 " Senior",                  TRUE,
  "\\bScoul?l[ae]r\\b\\.?",                         "Scouller",                 TRUE,
  "\\bSeligmann?\\b\\.?",                           "Seligmann",                TRUE,
  "\\bSell?[ae]rs?\\b\\.?",                         "Sellars",                  TRUE,
  "\\bSheriffs?\\b\\.?",                            "Sheriff",                  TRUE,
  "\\bShield?s\\b\\.?",                             "Shields",                  TRUE,
  "\\bSill[ea]rs\\b\\.?",                           "Sillers",                  TRUE,
  "\\bSimm?\\b\\.?",                                "Sim",                      TRUE,
  "\\bSimp?son\\b\\.?",                             "Simpson",                  TRUE,
  "\\bSlim[ao]n\\b\\.?",                            "Slimon",                   TRUE,
  "\\bSm[ei]llie\\b\\.?",                           "Smellie",                  TRUE,
  "\\bSm[iy]th\\b\\.?",                             "Smith",                    TRUE,
  "\\bSnedd[eo]n\\b\\.?",                           "Sneddon",                  TRUE,
  "\\bSommLerva?ill?e?\\b\\.?",                     "Sommerville",              TRUE,
  "\\bSpencer?\\b\\.?",                             "Spence",                   TRUE,
  "\\bSteele?\\b\\.?",                              "Steel",                    TRUE,
  "\\bStephens?\\b\\.?",                            "Stephen",                  TRUE,
  "\\bStratt?on\\b\\.?",                            "Stratton",                 TRUE,
  "\\bStruthe[rt]s\\b\\.?",                         "Struthers",                TRUE,
  "\\bSwann?\\b\\.?",                               "Swan",                     TRUE,
  "\\bTa[iy]lor\\b\\.?",                            "Taylor",                   TRUE,
  "\\bTenn[ae]nt\\b\\.?",                           "Tennent",                  TRUE,
  "\\bThomp?son\\b\\.?",                            "Thompson",                 TRUE,
  "\\bThynn?e\\b\\.?",                              "Thyne",                    TRUE,
  "\\bTodd?\\b\\.?",                                "Tod",                      TRUE,
  "\\bTon[ae]r\\b\\.?",                             "Toner",                    TRUE,
  "\\bTosha[co]h\\b\\.?",                           "Toshach",                  TRUE,
  "\\bTow[ae]rt\\b\\.?",                            "Towart",                   TRUE,
  "\\bWalshe?\\b\\.?",                              "Walsh",                    TRUE,
  "\\bWylde?\\b\\.?",                               "Wylde",                    TRUE,
  "\\bYuille?\\b\\.?",                              "Yuill",                    TRUE
)















































# Occupations ####
occupations <- c(
  "(?:commission|emigration|insurance|packet|ship)?(?:\\sand\\s)?(?:egg|emigration|provision|rag|spirit|steam-packet)?\\s?agents?", 
  "bookman", "bookseller", "builder", 
  "cabinetmaker", 
  "(?:letter-)?carr(?:ier)?", 
  "carter", 
  "civil engineer", "coalmaster", "collector",
  "dairyman", 
  "(?:wine)?(?:\\sand\\s)?(?:spirit)?\\s?dealer",
  "draper", 
  "engineer",
  "fruiterer",
  "general outfitter",
  "(?:green-)?grocer",
  "hairdresser", 
  "house\\s(?:factor|proprietor)",
  "(?:gas meter|railway)?\\s?inspector",
  "ironmonger", "iron turner",
  "letterpress printer",
  "(?:glue)?(?:\\sand\\s)?(?:size)?\\s?maker",
  "(?:cap|heel|tobacco)?(?:\\sand\\s)?(?:snuff|toeplate)?(?:\\sand\\s)?(?:nail)?\\s?manufacturer?",
  "manager", "managing partner", "manufacturing chemist", "mason", "measurer", 
  "(?:butter|canvas|cigar|commission|fi[s3]h|grain|spirit|vegetable|wine)?(?:\\sand\\s)?(?:egg|provision|rag|spirit)?\\s?merch(?:an)?t", 
  "mil(?:liner)?\\.? and dressm(?:aker)?\\.?", 
  "minister",
  "(?:bar-|gas|medical)?\\s?officer",
  "precentor", "physician",
  "(?:letterpress)?\\s?printer", 
  "publisher",
  "river-pilot",
  "saddler", "slater", "surgeon",
  "(?:medical)?\\s?superintendent",
  "tanyard", "tin packing case maker", "teacher", "tobacconist", 
  "(?:wholesale cloth and tailor's)?\\s?trimming",
  "upholsterer",
  "victualler", 
  "warehouseman", "writer" 
)
















































# Titles ####
titles <- tibble::tribble(
  ~pattern,                     ~replacement,    ~ignore.case,
  "\\bAdvisee\\b",              "Advisee",       TRUE,
  "\\bCapt(?:ain)?\\b",         "Captain",       TRUE,
  "\\bLieutenant\\b",           "Lieutenant",    TRUE,
  "\\bMajor\\b",                "Major",         TRUE,
  "\\bRev\\b\\.",               "Minister",      TRUE
)
















































# Others ####

## Ampersand ####
ampersand <- c(
  "[<('6ciqt]?[.$§#}|*%?&£34dfijJoqSy][.$~;*■•/\\-\"'cefjy[]?", "<f \\/", "tj \\-"
)
and_single_quote <- c(
  "\\'&", "&", "\\$", "\\$■", "\\$•", "#", "§", "§•", "\\(f", "\\(J\\-", "<\\.f", 
  "<\\*", "<%", "<£■", "<£•", "<\\$■", "<§", "<§■", 
  # "<\\|\\*", "<\\|\\'", 
  "<f", "<f\\*", 
  "<\\-f", "<j", "<j\\-", "<j\\'", "<J \\-", "<J\\-", "<J\\'", "<J\\*", "<y", "<\\$•", 
  "«f", "and", "c\\$\\'", "c\\)\\'\\-", 
  "4", "4\\'",
  "cf", "cf\\'", "cj\\'", "cj\\-", "cj\\*", "cy", "d\\-", "d;", "ef", "ej\\-", 
  "f\\.", "fy", "g°\\-", "if", "ij\\'", "ij\\-", "ij \\-", "oJ", "q", "rj\\-", 
  "S;", "t j\\-", "tf", "ti;", "tj\\-", "tj \\-", "ty"
)
and_double_quote <- c(
  '<J\\"', 'cj\\"'
)

## House ####
house <- ";\\s[bh][op](?:use|-)$"

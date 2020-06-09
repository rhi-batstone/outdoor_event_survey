# This was a real dataset and business brief and therefore the data has been synthesised for publication
# Initial cleaning on cleaning_&_synthesis.Rmd - Raw data was too large to synthesise 
# Some reduction cleaning completed before synthesis


#################################################################
##                          Libraries                          ##
#################################################################
source("analysis_scripts/libraries.R")

#################################################################
##                       Reading in data                       ##
#################################################################

## ---------------------------------------------------------------
##                         Survey data                          --
## ---------------------------------------------------------------

survey_raw <- read_csv(
    "raw_data/syn_survey.csv")

## ----------------------------------------------------------------
##                          Membership Data                      --
## ----------------------------------------------------------------

scottish_membership <- read_csv(
  "raw_data/syn_membership.csv") %>% 
  rename(
    birth_year = dob)

##----------------------------------------------------------------
##             scottish auth coordinates & cleaning             --
##----------------------------------------------------------------

#Creating a vector of scottish authorities
scottish_authorities <- str_to_lower(c("Inverclyde",
                                       "Renfrewshire",
                                       "West Dunbartonshire",
                                       "East Dunbartonshire",
                                       "Glasgow",
                                       "East Renfrewshire",
                                       "North Lanarkshire",
                                       "Falkirk",
                                       "West Lothian",
                                       "Edinburgh",
                                       "Midlothian",
                                       "East Lothian",
                                       "Clackmannanshire",
                                       "Fife",
                                       "Dundee",
                                       "Angus",
                                       "Aberdeenshire",
                                       "Aberdeen",
                                       "Moray",
                                       "Highland",
                                       "Na h-Eileanan Siar",
                                       "Argyll and Bute",
                                       "Perth and Kinross",
                                       "Stirling",
                                       "North Ayrshire",
                                       "East Ayrshire",
                                       "South Ayrshire",
                                       "Dumfries and Galloway",
                                       "South Lanarkshire",
                                       "Scottish Borders",
                                       "Orkney",
                                       "Shetland"))

# cleaning the names of the authroities in the scot_authorities dataset that includes coordinates
scottish_auth_coords <- read_csv("raw_data/scot_authorities.csv") %>% 
  mutate(authority = str_to_lower(authority),
         authority = if_else(authority == "aberdeen city", "aberdeen", authority),
         authority = if_else(authority == "city of edinburgh", "edinburgh", authority),
         authority = if_else(authority == "dundee city", "dundee", authority),
         authority = if_else(authority == "glasgow city", "glasgow", authority),
         authority = if_else(authority == "orkney islands", "orkney", authority),
         authority = if_else(authority == "shetland islands", "shetland", authority)
         ) %>% 
  # Filtering only those in the survey data
  filter(authority %in% scottish_authorities)


#################################################################
##                   Cleaning Data post-synth                  ##
#################################################################

##---------------------------------------------------------------
##                        text cleaning                        --
##---------------------------------------------------------------

# Creating the travel options that will be included
transport_options <- c("car", 
                       "boat", 
                       "train", 
                       "bus", 
                       "air", 
                       "no travel", 
                       "cycle")


# Creating the travel options that will be included
accomodations <- c(
  "hostel",
  "hotel/B&B",
  "self-catered",
  "campsite",
  "with friends",
  "own home",
  "didn't go")


  survey_text_clean <- survey_raw %>% 
  #cleaning transport to area types
  mutate(
    transport_to_area = if_else(str_detect(transport_to_area, "car|hire"), "car", transport_to_area),
    transport_to_area = if_else(str_detect(transport_to_area, "cycle"), "cycle", transport_to_area),
    transport_to_area = if_else(str_detect(transport_to_area, "ferry|boat"), "boat", transport_to_area),
    transport_to_area = if_else(str_detect(transport_to_area, "train|rail"), "train", transport_to_area),
    transport_to_area = if_else(str_detect(transport_to_area, "bus|coach"), "bus", transport_to_area),
    transport_to_area = if_else(str_detect(transport_to_area, "vehicle"), "car", transport_to_area),
    transport_to_area = if_else(str_detect(transport_to_area, "lift"), "car", transport_to_area),
    transport_to_area = if_else(str_detect(transport_to_area, "air"), "air", transport_to_area),
    transport_to_area = if_else(str_detect(transport_to_area, "live|living|did not|didn't"), "no travel", transport_to_area),
    transport_to_area = if_else(!(transport_to_area %in% transport_options), "other", transport_to_area)
    #### for future consider including a warning for new categries
    ) %>% 
  # cleaning transport to event types
  mutate(
    transport_to_event = if_else(str_detect(transport_to_event, "car|hire|lift|rental"), "car", transport_to_event),
    transport_to_event = if_else(str_detect(transport_to_event, "cycle"), "cycle", transport_to_event),
    transport_to_event = if_else(str_detect(transport_to_event, "train|rail"), "train", transport_to_event),
    transport_to_event = if_else(str_detect(transport_to_event, "public|públic|bus|coach"), "bus", transport_to_event),
    transport_to_event = if_else(str_detect(transport_to_event, "live|living|did not|didn't"), "no travel", transport_to_event),
    transport_to_event = if_else(str_detect(transport_to_event, "vehicle"), "car", transport_to_event),
    transport_to_event = if_else(!(transport_to_event %in% transport_options), "other", transport_to_event)
  ) %>% 
  #cleaning local authority names
  mutate(
    authority = trimws(authority),
    authority = if_else(str_detect(authority, "highland"), "highland", authority),
    authority = if_else(str_detect(authority, "border"), "scottish borders", authority),
    authority = if_else(str_detect(authority, "glasgow"), "glasgow", authority),
    authority = if_else(str_detect(authority, "pkc|perth|kinross|tayside"), "perth and kinross", authority),
    authority = if_else(str_detect(authority, "dumfries|galloway"), "dumfries and galloway", authority),
    authority = if_else(str_detect(authority, "argyll|bute"), "argyll & bute", authority),
    authority = if_else(str_detect(authority, "aberdeensh|grampian"), "aberdeenshire", authority),
    authority = if_else(str_detect(authority, "aberdeen"), "aberdeen", authority),
    authority = if_else(str_detect(authority, "edinb"), "edinburgh", authority),
    authority = if_else(str_detect(authority, "stirling"), "stirling", authority),
    authority = if_else(str_detect(authority, "mid lothian"), "midlothian", authority),
    authority = if_else(authority == "ayr", "south ayrshire", authority),
    authority = if_else(authority == "lothian", "edinburgh", authority),
    authority = if_else(!(authority %in% scottish_authorities), "NA", authority),
    authority = na_if(authority, "NA")
  ) %>% 
    #joining coodinates to now cleaned scottish locations
    left_join(scottish_auth_coords, by = "authority"
  ) %>% 
    #cleaning accomodation types
  mutate(
    stay_accomodation = if_else(str_detect(stay_accomodation, "camp|site|comrie croft"), "campsite", stay_accomodation),
    stay_accomodation = if_else(str_detect(stay_accomodation, "hostel"), "hostel", stay_accomodation),
    stay_accomodation = if_else(str_detect(stay_accomodation, "self-cater|air bnb|airbnb"), "self-catered", stay_accomodation),
    stay_accomodation = if_else(str_detect(stay_accomodation, "hotel|b+b|b&b|guest house|inn"), "hotel/B&B", stay_accomodation),
    stay_accomodation = if_else(str_detect(stay_accomodation, "friend"), "with friends", stay_accomodation),
    stay_accomodation = if_else(str_detect(stay_accomodation, "home"), "own home", stay_accomodation),
    stay_accomodation = if_else(str_detect(stay_accomodation, "didn't|did not"), "didn't go", stay_accomodation),
    stay_accomodation = if_else(!(stay_accomodation %in% accomodations), "other", stay_accomodation)
         )


## ----------------------------------------------------------------
##                     scottish Club Coordinates                     --
## ----------------------------------------------------------------

scottish_clubs <- tibble(
  # club names renamed numerically to hide identities
  # Ayr
  "1" = "55.457349, -4.629187",
  # Aviemore
  "2" = "57.191849, -3.828991",
  # Glasgow, Bearsden
  "3" = "55.921464, -4.335141",
  # Argyll, Oban
  "4" = "56.415049, -5.471223",
  # Tranent
  "5" = "55.944708, -2.954554",
  #  Edinburgh
  "6" = "55.929523, -3.209466",
  # Edinburgh
  "7" = "55.944651, -3.189167",
  #  Stirling
  "8" = "56.116700, -3.936831",
  # Aberdeen
  "9" = "57.149953, -2.093622",
  # Glasgow
  "10" = "55.872030, -4.288243",
  # Edinburgh
  "11" = "55.953282, -3.188212",
  # Inverness
  "12" = "57.477753, -4.224784",
  # Dunfermline
  "13" = "56.071728, -3.452149",
  #  Aboyne
  "14" = "57.076624, -2.780310",
  # M Forres
  "15" = "57.609819, -3.620257",
  #  Jedburgh
  "16" = "55.477709, -2.554903",
  #  Dumfries
  "17" = "55.071043, -3.604385",
  #  Glasgow
  "18" = "55.864386, -4.251464",
  #  Perth
  "19" = "56.395157, -3.430614"
) %>%
  # tranforming the tibble to longer then separating coordinates out
  pivot_longer(1:19,
    names_to = "club",
    values_to = "coords"
  ) %>%
  separate(coords, c("latitude", "longitude"), ", ")



# -------------------------------------------------------------------------
# joining club coordinates to the membership info

scottish_membership <- scottish_membership %>%
    mutate(club = as.character(club)) %>% 
  inner_join(scottish_clubs, by = "club")



#################################################################
##                        Write to .CSV                        ##
#################################################################

write_csv(scottish_membership, "clean_data/scottish_memberships.csv")
write_csv(survey_text_clean, "clean_data/survey_data.csv")





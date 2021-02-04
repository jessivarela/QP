# Project descriptives --------------------------------------------------------
#
# Description: get basic project descriptives for sanity checks
# Last update: 01/18/2021
#
# -----------------------------------------------------------------------------


# Source libs -----------------------------------------------------------------

source(here::here("scripts", "00_load_libs.R"))

# -----------------------------------------------------------------------------

## Linguistic questionnaire info

# Load .csv file from google drive

jessiv_allinfo <- read.csv(here("data", "raw", "pupurri_demographics.csv"))

glimpse(jessiv_allinfo)

#####
# Participants per group

jessiv_allinfo %>%
  group_by(., group) %>%
  summarise(., n = n())

#####
# Females per group

jessiv_allinfo %>%
  filter(., sex == "female") %>%
  group_by(., group) %>%
  summarise(., n_females = n())

#################################
# Get mean AGE as a function of group + SD

jessiv_allinfo %>%
  group_by(., group) %>%
  summarise(min_age = min(age),
            max_age = max(age),
            mean_age = round(mean(age),2),
            sd_age = round(sd(age),2)) %>% knitr::kable()

jessiv_allinfo$DELE <- as.numeric(as.character(jessiv_allinfo$DELE))
glimpse(jessiv_allinfo)

jessiv_allinfo %>%
  filter(., group %in% c("ahs", "ihs")) %>%
  group_by(., group) %>%
  summarise(mean_DELE = mean(DELE),
            sd_DELE = sd(DELE))

#################################
# Get mean L2 AoA as a function of group + SD

jessiv_allinfo$AoA_L2 <- as.numeric(as.character(jessiv_allinfo$AoA_L2))
glimpse(jessiv_allinfo)

jessiv_allinfo %>%
  filter(., group %in% c("ahs", "ihs")) %>%
  group_by(., group) %>%
  summarise(mean_AoA_L2 = mean(AoA_L2),
            sd_AoA_L2 = sd(AoA_L2))

#####
# oldest sibling

jessiv_allinfo %>%
  filter(., oldest.sibling. == "yes") %>%
  group_by(., group) %>%
  summarise(., n_yes = n())

#################################
# Get mean percentage of English use in a week as a function of group + SD

jessiv_allinfo$perc_week_Eng <- as.numeric(as.character(jessiv_allinfo$perc_week_Eng))
glimpse(jessiv_allinfo)

jessiv_allinfo %>%
  filter(., group %in% c("ahs", "ihs")) %>%
  group_by(., group) %>%
  summarise(mean_perc_week_Eng = mean(perc_week_Eng),
            sd_perc_week_Eng = sd(perc_week_Eng))


#################################
# Get mean percentage of Spanish use in a week as a function of group + SD

jessiv_allinfo$perc_week_Spa <- as.numeric(as.character(jessiv_allinfo$perc_week_Spa))
glimpse(jessiv_allinfo)

jessiv_allinfo %>%
  filter(., group %in% c("ahs", "ihs")) %>%
  group_by(., group) %>%
  summarise(mean_perc_week_Spa = mean(perc_week_Spa),
            sd_perc_week_Spa = sd(perc_week_Spa))

#####
# like Reading per group

jessiv_allinfo %>%
  filter(., like_reading == "yes") %>%
  group_by(., group) %>%
  summarise(., n_yes = n())

#####
# handedness per group

jessiv_allinfo %>%
  filter(., handedness == "right") %>%
  group_by(., group) %>%
  summarise(., n_right = n())

jessiv_allinfo %>%
  filter(., handedness == "left") %>%
  group_by(., group) %>%
  summarise(., n_left = n())




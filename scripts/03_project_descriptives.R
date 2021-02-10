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

jessiv_allinfo <- read.csv(here("data", "raw", "demographics_original_V1.csv"))

# Clean df

jessiv_allinfo <- jessiv_allinfo[-1, ]

jessiv_allinfo <- select(jessiv_allinfo, ID, group, handedness, country_born, age_immigrate_US, DELE, age, gender, AoA_L2, en_pc_week, es_pc_week, mom.school)

jessiv_allinfo <- jessiv_allinfo[!is.na(jessiv_allinfo$age),]
  
 
#####
# Participants per group

jessiv_allinfo %>%
  group_by(., group) %>%
  summarise(., n = n())

#####
# Females per group

jessiv_allinfo %>%
  filter(., gender == "female") %>%
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


# DELE
jessiv_allinfo$DELE <- as.numeric(as.character(jessiv_allinfo$DELE))

jessiv_allinfo[!is.na(jessiv_allinfo$DELE),] %>%
  group_by(., group) %>%
  summarise(mean_DELE = mean(DELE),
            sd_DELE = sd(DELE))

#################################
# Get mean L2 AoA as a function of group + SD (L2 here is English)

jessiv_allinfo$AoA_L2 <- as.numeric(as.character(jessiv_allinfo$AoA_L2))


jessiv_allinfo[!is.na(jessiv_allinfo$AoA_L2),] %>%
  group_by(., group) %>%
  summarise(mean_AoA_L2 = mean(AoA_L2),
            sd_AoA_L2 = sd(AoA_L2))


# #####
# # oldest sibling
# 
# jessiv_allinfo %>%
#   filter(., oldest.sibling. == "yes") %>%
#   group_by(., group) %>%
#   summarise(., n_yes = n())

#################################
# Get mean percentage of English use in a week as a function of group + SD

jessiv_allinfo$en_pc_week <- as.numeric(as.character(jessiv_allinfo$en_pc_week))

jessiv_allinfo %>%
#  filter(., group %in% c("ahs", "ihs")) %>%
  group_by(., group) %>%
  summarise(mean_perc_week_Eng = mean(en_pc_week),
            sd_perc_week_Eng = sd(en_pc_week))


#################################
# Get mean percentage of Spanish use in a week as a function of group + SD

jessiv_allinfo$es_pc_week <- as.numeric(as.character(jessiv_allinfo$es_pc_week))

jessiv_allinfo %>%
  group_by(., group) %>%
  summarise(mean_perc_week_Spa = mean(es_pc_week),
            sd_perc_week_Spa = sd(es_pc_week))

#####

# Country where participants were born, and if abroad, when they immigrated to US

jessiv_allinfo %>%
  count(country_born)

jessiv_allinfo %>%
  filter(., country_born != 'US') %>%
  count(age_immigrate_US)

#####
# handedness per group

jessiv_allinfo$handedness <- trimws(jessiv_allinfo$handedness, which = c("right"))
jessiv_allinfo$handedness <- as.factor(jessiv_allinfo$handedness)

jessiv_allinfo %>%
  group_by(., group) %>%
  count(handedness)


#####
# # like Reading per group
# 
# jessiv_allinfo %>%
#   filter(., like_reading == "yes") %>%
#   group_by(., group) %>%
#   summarise(., n_yes = n())








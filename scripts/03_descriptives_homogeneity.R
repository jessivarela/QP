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

demo_hs <- read.csv(here("data", "raw", 'demographics', "demographics_original_V1.csv"))
demo_ss_l2 <- read.csv(here("data", "raw", 'demographics', "demographics_ss_l2.csv"))

# Clean df

demo_hs <- demo_hs[-1, ]

demo_hs <- demo_hs %>%
  select(., ID, group, handedness, country_born, age_immigrate_US, 
                  DELE, age, gender, AoA_L2, en_pc_week, es_pc_week) %>% #, mom.school
  rename(., participant = ID)

demo_hs <- demo_hs[!is.na(demo_hs$age),]

demo_ss_l2 <- demo_ss_l2 %>%
  select(., participant, group, handed, country, DELE, 
         age, gender, AoA_L2, percent_l1_week, percent_l2_week, mo_ES_country) %>% 
  rename(., handedness = handed,
         country_born = country,
         en_pc_week = percent_l1_week,
         es_pc_week = percent_l2_week)

demo_ss_l2$participant <- tolower(demo_ss_l2$participant)

demo_hs$mo_ES_country <- NA
demo_ss_l2$age_immigrate_US <- NA

demo_hs$AoA_L2 <- as.numeric(as.character(demo_hs$AoA_L2))
demo_hs$es_pc_week <- as.numeric(as.character(demo_hs$es_pc_week))
demo_hs$en_pc_week <- as.numeric(as.character(demo_hs$en_pc_week))

demo_ss <- demo_ss_l2 %>%
  subset(., group == 'mon')
demo_l2 <- demo_ss_l2 %>%
  subset(., group != 'mon')

demo_ss$DELE <- 56

demo_ss <- demo_ss %>%
  mutate(DELE = DELE + runif(n(), min = -0.15, max = 0.15) * (n() > 1))

demo_ss_l2 <- rbind(demo_ss, demo_l2)

demographics <- bind_rows(demo_hs, demo_ss_l2)

demographics$group <- str_replace(demographics$group, "ihs", "hs")
demographics$group <- str_replace(demographics$group, "ahs", "hs")
demographics$group <- str_replace(demographics$group, "mon", "ss")
demographics$group <- str_replace(demographics$group, "ies", "l2")
demographics$group <- str_replace(demographics$group, "aes", "l2")

demographics$DELE <- as.numeric(as.character(demographics$DELE))
demographics$AoA_L2 <- as.numeric(as.character(demographics$AoA_L2))
demographics$en_pc_week <- as.numeric(as.character(demographics$en_pc_week))
demographics$es_pc_week <- as.numeric(as.character(demographics$es_pc_week))
demographics$handedness <- trimws(demographics$handedness, which = c("right"))
demographics$handedness <- as.factor(demographics$handedness)

demographics <- demographics[!is.na(demographics$DELE),]
demographics <- demographics[!is.na(demographics$AoA_L2),]

write.csv(demographics, here("data", "clean", 'demographics.csv'), row.names = F)


#####
# Participants per group

demographics %>%
  group_by(., group) %>%
  summarise(., n = n())
# group     n
# 1 hs       50
# 2 l2       65
# 3 ss       30

#####
# Females per group

demographics %>%
  filter(., gender == "female") %>%
  group_by(., group) %>%
  summarise(., n_females = n())
# group  n_females
# 1 hs           34
# 2 l2           48
# 3 ss           20

#################################
# Get mean AGE as a function of group + SD

demographics %>%
  group_by(., group) %>%
  summarise(min_age = min(age),
            max_age = max(age),
            mean_age = round(mean(age),2),
            sd_age = round(sd(age),2)) %>% knitr::kable()
# |group | min_age| max_age| mean_age| sd_age|
#   |:-----|-------:|-------:|--------:|------:|
#   |hs    |      18|      34|    22.80|   5.82|
#   |l2    |      20|      38|    26.75|   4.54|
#   |ss    |      18|      45|    26.17|   8.82|

# DELE

demo_bilinguals <- demographics %>%
  filter(., group != 'ss')

demo_bilinguals %>%
  group_by(., group) %>%
  summarise(mean_DELE = mean(DELE),
            sd_DELE = sd(DELE))
# group mean_DELE sd_DELE
#   1 hs         38.6    5.46
# 2 l2         38.5    8.19


bartlett.test(DELE ~ group, data = demo_bilinguals)
# Bartlett's K-squared = 8.5079, df = 1, p-value = 0.003536

t.test(DELE ~ group, data = demo_bilinguals, var.equal = TRUE) # homogeneity across groups
# t = 0.12156, df = 113, p-value = 0.9035

#################################
# Get mean L2 AoA as a function of group + SD (L2 here is English)

demo_bilinguals %>%
  group_by(., group) %>%
  summarise(mean_AoA_L2 = mean(AoA_L2),
            sd_AoA_L2 = sd(AoA_L2))
# group mean_AoA_L2 sd_AoA_L2
# 1 hs            4.1      1.22
# 2 l2           16.3      5.55


#################################
# Get mean percentage of English use in a week as a function of group + SD

demo_bilinguals %>%
  # filter(., group %in% c("hs", "l2")) %>%
  group_by(., group) %>%
  summarise(mean_perc_week_Eng = mean(en_pc_week),
            sd_perc_week_Eng = sd(en_pc_week))
# group mean_perc_week_Eng sd_perc_week_Eng
# 1 hs                  55.5             17.4
# 2 l2                  66.6             17.4

bartlett.test(en_pc_week ~ group, data = demo_bilinguals) # homogeneity of variances
# Bartlett's K-squared = 1.7832e-05, df = 1, p-value = 0.9966

t.test(en_pc_week ~ group, data = demo_bilinguals, var.equal = TRUE) # homogeneity across groups
# t = -3.3883, df = 113, p-value = 0.0009689

#################################
# Get mean percentage of Spanish use in a week as a function of group + SD

demo_bilinguals %>%
  #filter(., group %in% c("hs", "l2")) %>%
  group_by(., group) %>%
  summarise(mean_perc_week_Spa = mean(es_pc_week),
            sd_perc_week_Spa = sd(es_pc_week))
# group mean_perc_week_Spa sd_perc_week_Spa
# 1 hs                  44.5             17.4
# 2 l2                  33.3             17.4

bartlett.test(es_pc_week ~ group, data = demo_bilinguals) # homogeneity of variances
# Bartlett's K-squared = 6.8132e-06, df = 1, p-value = 0.9979

t.test(es_pc_week ~ group, data = demo_bilinguals, var.equal = TRUE) # homogeneity across groups
# t = 3.4113, df = 113, p-value = 0.0008977

#####

# Country where participants were born, and if abroad, when they immigrated to US

demographics %>%
  filter(., group == "hs") %>%
  count(country_born)
# country_born  n
# 1       Mexico  9
# 2    Nicaragua  1
# 3           US 39
# 4    Venezuela  1

demographics %>%
  filter(., group == 'hs' & country_born != 'US') %>%
  count(age_immigrate_US)
# age_immigrate_US n
# 1                1 3
# 2                2 1
# 3                3 4
# 4                6 2
# 5                8 1

#####
# handedness per group

demographics %>%
  group_by(., group) %>%
  count(handedness)
# group handedness       n
# 1 hs    left             2
# 2 hs    right           47
# 3 hs    right (ambi)     1
# 4 l2    left             2
# 5 l2    right           63
# 6 ss    left             1
# 7 ss    right           29




#Table for Manuscript - need to finish 

demographics %>%
  #  filter(., group %in% c("ahs", "ihs")) %>%
  group_by(., group) %>%
  summarise(mean_perc_week_Eng = mean(en_pc_week),
            sd_perc_week_Eng = sd(en_pc_week),
            mean_AoA_L2 = mean(AoA_L2),
            sd_AoA_L2 = sd(AoA_L2), 
            mean_perc_week_Spa = mean(es_pc_week),
            sd_perc_week_Spa = sd(es_pc_week),
            mean_DELE = mean(DELE),
            sd_DELE = sd(DELE))
            



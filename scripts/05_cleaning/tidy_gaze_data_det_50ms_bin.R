# Morphosyntactic predictability: tidy stress data ----------------------------
#
# This script will load and tidy the raw eye tracking data
# with 50 ms bins and save the output to data/clean
#
# Last update: 03/24/2021

# -----------------------------------------------------------------------------


# Source libs -----------------------------------------------------------------

source(here::here("scripts", "00_load_libs.R"))
library(plyr)
library(readr)

# -----------------------------------------------------------------------------
# load data
det50 <- read.delim(here::here("data", 'raw', 'experiment', 'Output', "det_hs_50.txt"))

# Check gaze fixation columns have different values
unique(det50$AVERAGE_IA_1_SAMPLE_COUNT)  # looking at target according to IA_#_ID
unique(det50$AVERAGE_IA_2_SAMPLE_COUNT)  # looking at distractor
unique(det50$AVERAGE_IA_0_SAMPLE_COUNT)  # elsewhere

# Tidy data -------------------------------------------------------------------

# Read data
det50 <- det50 %>%
  
  #select and rename variables of interest
  select(., RECORDING_SESSION_LABEL, TRIAL_INDEX, BIN_INDEX,
         AVERAGE_IA_0_SAMPLE_COUNT, AVERAGE_IA_0_SAMPLE_COUNT_.,
         AVERAGE_IA_1_SAMPLE_COUNT, AVERAGE_IA_1_SAMPLE_COUNT_.,
         AVERAGE_IA_2_SAMPLE_COUNT, AVERAGE_IA_2_SAMPLE_COUNT_.,
         ACCURACY, id, noun_transparency, structure, gender, 
         t01, t02, t03, t04, t05, t06, target, version) %>%
  dplyr::rename(., participant = RECORDING_SESSION_LABEL,
                trial = TRIAL_INDEX, 
                bin = BIN_INDEX,
                target_count = AVERAGE_IA_1_SAMPLE_COUNT, 
                target_prop = AVERAGE_IA_1_SAMPLE_COUNT_.,
                onset_que = t01,
                onset_det = t02,
                onset_noun = t03,
                onset_target = t04,
                offset_target = t05,
                endSentence = t06,
                sentence_id = id) %>%
  
  # remove incorrect
  filter(., ACCURACY == 1) %>%
  
  # drop unused levels of factors
  droplevels(.) %>%

  # Create eLog variable and respective wts
  mutate(.,eLog = log((target_count + 0.5) / (50 - target_count + 0.5)),
         wts = 1 / (target_count + 0.5) + 1 / (50 - target_count + 0.5)) %>%
    
  # Select necessary columns
  # Gather data to prepare for bin adjustment
  # Get suffix onset label and center at 0 for each
  # participant for each item
  dplyr::select(participant, target, bin, noun_transparency, structure, gender,
                target_count, target_prop, eLog, wts, onset_target) %>%   
  gather(., landmark, lm_bin, -c(participant:wts)) %>%
  mutate(., lm_bin = (lm_bin / 50) %>% ceiling(.),
         t_onset = if_else(bin == lm_bin, TRUE, FALSE)) %>%
  
  group_by(., participant, target) %>%
  mutate(., time_zero = gazer::onset_pupil(bin, t_onset, event = c("TRUE"))) %>%
  ungroup(.)
  
# Load verbal WM
dem <- read_csv(here("data", 'clean', "demographics.csv"))
dem <- dem %>%
  select(., participant, group, DELE)
  
# Add verbal wm score to eyetracking data frame
det50 <- merge(x = det50, y = dem, by = "participant", all.x=TRUE)


# Load processing speed info
ospan_en_hs = list.files(path="./data/raw/experiment/ospan_en/", pattern="*.csv", full.names=TRUE)
ospan_en_hs = ldply(ospan_en_hs, read_csv)

ospan_es_hs = list.files(path="./data/raw/experiment/ospan_es/", pattern="*.csv", full.names=TRUE)
ospan_es_hs = ldply(ospan_es_hs, read_csv)

ospan_l2_ss = list.files(path="./data/raw/experiment/ospan_l2_ss/", pattern="*.csv", full.names=TRUE)
ospan_l2_ss = ldply(ospan_l2_ss, read_csv)

ospan_en_hs <- ospan_en_hs %>%
  select(., -X1, -trial_num, -date, -handedness) %>%
  filter(., accuracy_formula == 1) %>%
  dplyr::rename(., participant = subject_id)

ospan_es_hs <- ospan_es_hs %>%
  select(., -X1, -trial_num, -date, -handedness) %>%
  filter(., accuracy_formula == 1) %>%
  dplyr::rename(., participant = subject_id)

ospan_en_hs$group <- 'hs'
ospan_es_hs$group <- 'hs'

ospan_l2_ss <- ospan_l2_ss %>%
  select(., -X1, -trial_num, -date, -handedness) %>%
  mutate(., group = if_else(lang == 'en', 'l2', 'ss')) %>%
  filter(., accuracy_formula == 1) %>%
  filter(str_detect(subject_id, 'ae|ie|mo')) %>%
  dplyr::rename(., participant = subject_id)

ospan_l2_ss$participant <- str_replace(ospan_l2_ss$participant, "ae", "aes")
ospan_l2_ss$participant <- str_replace(ospan_l2_ss$participant, "ie", "ies")
ospan_l2_ss$participant <- str_replace(ospan_l2_ss$participant, "mo", "mon")

ospan_en <- rbind(ospan_en_hs, ospan_l2_ss)

ospan_es <- rbind(ospan_es_hs, ospan_l2_ss)

ospan_en_glm <- lmer(rt_formula ~ group + seq_length + (1 | participant),
                     data = ospan_en)

ospan_es_glm <- lmer(rt_formula ~ group + seq_length + (1 | participant),
                     data = ospan_es)

ospan_en_ranef <- ranef(ospan_en_glm) %>% as_tibble()  

ospan_es_ranef <- ranef(ospan_es_glm) %>% as_tibble()

ospan_en_re <- ospan_en_ranef %>%
  select(., grp, condval, condsd) %>%
  dplyr::rename(., participant = grp,
                ospan_en_val = condval,
                ospan_en_sd = condsd)

ospan_es_re <- ospan_es_ranef %>%
  select(., grp, condval, condsd) %>%
  dplyr::rename(., participant = grp,
                ospan_es_val = condval,
                ospan_es_sd = condsd)

ospan_ranef <- merge(ospan_en_re, ospan_es_re, by="participant")

det50 <- merge(x = det50, y = ospan_ranef, by.x = "participant", by.y = "participant", all.x=TRUE)

det50 <- det50[!is.na(det50$group), ]

write_csv(det50, here::here("data", "clean", "det_50ms_onset_target.csv"))


# change name of .csv if trigger checked different
write_csv(stress50, here("data", "clean", "det_50ms_onset_target.csv"))

# -----------------------------------------------------------------------------
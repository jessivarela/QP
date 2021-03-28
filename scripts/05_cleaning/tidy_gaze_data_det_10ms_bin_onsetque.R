# Morphosyntactic predictability: tidy stress data ----------------------------
#
# This script will load and tidy the raw eye tracking data
# with 10 ms bins and save the output to data/clean
#
# Last update: 03/23/2021
#
# -----------------------------------------------------------------------------




# Source libs -----------------------------------------------------------------

source(here::here("scripts", "00_load_libs.R"))
library(readr)


# -----------------------------------------------------------------------------

# load data
det10 <- read.delim(here::here("data", 'raw', 'experiment', 'Output', "det_hs_10.txt"))

# Check gaze fixation columns have different values
unique(det10$AVERAGE_IA_1_SAMPLE_COUNT)  # looking at target according to IA_#_ID
unique(det10$AVERAGE_IA_2_SAMPLE_COUNT)  # looking at distractor
unique(det10$AVERAGE_IA_0_SAMPLE_COUNT)  # elsewhere


# Tidy data -------------------------------------------------------------------

# Read data
det10 <- det10 %>%
  
  #select and rename variables of interest
  select(., RECORDING_SESSION_LABEL, TRIAL_INDEX, BIN_INDEX,
         AVERAGE_IA_0_SAMPLE_COUNT, AVERAGE_IA_0_SAMPLE_COUNT_.,
         AVERAGE_IA_1_SAMPLE_COUNT, AVERAGE_IA_1_SAMPLE_COUNT_.,
         AVERAGE_IA_2_SAMPLE_COUNT, AVERAGE_IA_2_SAMPLE_COUNT_.,
         ACCURACY, id, noun_transparency, structure, gender, 
         t01, t02, t03, t04, t05, t06, target) %>%
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
  mutate(.,eLog = log((target_count + 0.5) / (10 - target_count + 0.5)),
         wts = 1 / (target_count + 0.5) + 1 / (10 - target_count + 0.5)) %>%
  
  # CHANGE onset_c3 DEPENDING ON TRIGGER TO ANALYZE
  dplyr::select(participant, target, bin, noun_transparency, structure, gender,
                target_count, target_prop, eLog, wts, onset_que) %>%   
  gather(., landmark, lm_bin, -c(participant:wts)) %>%
  mutate(., lm_bin = (lm_bin / 10) %>% ceiling(.),
         t_onset = if_else(bin == lm_bin, TRUE, FALSE)) %>%

  group_by(., participant, target) %>%
  mutate(., time_zero = gazer::onset_pupil(bin, t_onset, event = c("TRUE"))) %>%
  ungroup(.)

# Load DELE and group
dem <- read_csv(here::here("data", 'clean', "demographics.csv"))
dem <- dem %>%
  select(., participant, group, DELE)

# Add DELE and group score to eyetracking data frame
det10 <- merge(x = det10, y = dem, by.x = "participant", by.y = "participant", all.x=TRUE)


# Load processing speed info
ospan_en_hs = list.files(path="./data/raw/experiment/ospan_en/", pattern="*.csv", full.names=TRUE)
ospan_en_hs = plyr::ldply(ospan_en_hs, read_csv)

ospan_es_hs = list.files(path="./data/raw/experiment/ospan_es/", pattern="*.csv", full.names=TRUE)
ospan_es_hs = plyr::ldply(ospan_es_hs, read_csv)

ospan_l2_ss = list.files(path="./data/raw/experiment/ospan_l2_ss/", pattern="*.csv", full.names=TRUE)
ospan_l2_ss = plyr::ldply(ospan_l2_ss, read_csv)

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
  
  #          case_when(
  #   endsWith(ID, "R") ~ "Recovered",
  #   endsWith(ID, "S") ~ "Sick"
  # )

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

det10 <- merge(x = det10, y = ospan_ranef, by.x = "participant", by.y = "participant", all.x=TRUE)

det10 <- det10[!is.na(det10$group), ]

write_csv(det10, here::here("data", "clean", "det_10ms_onset_que.csv"))

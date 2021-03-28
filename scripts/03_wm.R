wm_hs_en <- read.csv(here("data", "raw", 'experiment', 'wm', "wm_hs.xlsx - eng.csv"))
wm_hs_es <- read.csv(here("data", "raw", 'experiment', 'wm', "wm_hs.xlsx - span.csv"))
wm_ss_l2 <- read.csv(here("data", "raw", 'demographics', "demographics_ss_l2.csv"))

wm_hs_en <- wm_hs_en %>% select(., participant, engsets_correct) %>% drop_na()
wm_hs_es <- wm_hs_es %>% select(., participant, spansets_correct)
wm_ss_l2 <- wm_ss_l2 %>% select(., participant, group, WM_set)

wm_ss_l2$participant <- tolower(wm_ss_l2$participant)
wm_ss_l2$group <- str_replace(wm_ss_l2$group, "mon", "ss")
wm_ss_l2$group <- str_replace(wm_ss_l2$group, "ies", "l2")
wm_ss_l2$group <- str_replace(wm_ss_l2$group, "aes", "l2")

wm_ss_l2 <- wm_ss_l2[c(1, 2, 3, 3)]

colnames(wm_ss_l2) [3] <- "engsets_correct"
colnames(wm_ss_l2) [4] <- "spansets_correct"

wm_hs <- left_join(wm_hs_en, wm_hs_es, by = "participant")

wm_hs$group <- 'hs'

wm_df <- rbind(wm_ss_l2, wm_hs) %>% drop_na()


wm_df %>%
  group_by(., group) %>%
  summarise(min_en = min(engsets_correct),
            max_en = max(engsets_correct),
            mean_en = round(mean(engsets_correct),2),
            sd_en = round(sd(engsets_correct),2)) %>% knitr::kable()
#   |group | min_en| max_en| mean_en| sd_en|
#   |:-----|------:|------:|-------:|-----:|
#   |hs    |      1|     11|    5.16|  2.92|
#   |l2    |      5|     12|    8.89|  2.11|
#   |ss    |      1|     10|    6.20|  2.72|

# Variance within groups
bartlett.test(engsets_correct ~ group, data = wm_df)
# Bartlett's K-squared = 6.1686, df = 2, p-value = 0.04576

# homogeneity across groups
t.test(engsets_correct ~ group, data = wm_df %>% filter(., group != "ss"), var.equal = TRUE) 
# t = -7.9437, df = 113, p-value = 1.599e-12

t.test(engsets_correct ~ group, data = wm_df %>% filter(., group != "hs"), var.equal = TRUE) 
# t = 5.2551, df = 93, p-value = 9.364e-07

t.test(engsets_correct ~ group, data = wm_df %>% filter(., group != "l2"), var.equal = TRUE) 
# t = -1.5802, df = 78, p-value = 0.1181



wm_df %>%
  group_by(., group) %>%
  summarise(min_es = min(spansets_correct),
            max_es = max(spansets_correct),
            mean_es = round(mean(spansets_correct),2),
            sd_es = round(sd(spansets_correct),2)) %>% knitr::kable()
#   |group | min_es| max_es| mean_es| sd_es|
#   |:-----|------:|------:|-------:|-----:|
#   |hs    |      0|      9|    3.04|  2.38|
#   |l2    |      5|     12|    8.89|  2.11|
#   |ss    |      1|     10|    6.20|  2.72|

# Variance within groups
bartlett.test(spansets_correct ~ group, data = wm_df)
# Bartlett's K-squared = 2.6954, df = 2, p-value = 0.2598

# homogeneity across groups
t.test(spansets_correct ~ group, data = wm_df %>% filter(., group != "ss"), var.equal = TRUE) 
# t = -13.924, df = 113, p-value < 2.2e-16

t.test(spansets_correct ~ group, data = wm_df %>% filter(., group != "hs"), var.equal = TRUE) 
# t = 5.2551, df = 93, p-value = 9.364e-07

t.test(spansets_correct ~ group, data = wm_df %>% filter(., group != "l2"), var.equal = TRUE) 
# t = -5.4442, df = 78, p-value = 5.835e-07

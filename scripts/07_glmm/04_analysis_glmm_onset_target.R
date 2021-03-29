#
# Original script by Joseph
# Modified by Laura
# Last update: 2021/03/09
#
# GLMMs -----------------------------------------------------------------------
#
# - Target fixation as a function of group, stress (condition), and coda
#   at the offset of the first syllable (time_zero == 20)
# - This model builds on the t-test analyses by looking for between group
#   differences in target fixation at the offset of first syllable
#
# -----------------------------------------------------------------------------


# Phase structure: (que) det + noun + adjective
# Onset of target (aka adjective)

# Load data and models --------------------------------------------------------

source(here::here("scripts", "00_load_libs.R"))
# source(here::here("scripts", "02_load_data.R"))
det10target <- read_csv(here::here("data", 'clean', 'det_10ms_onset_target.csv'))

# Final model for both noun transparency and phrase structure with EN WM score
glmm_target_en <- readRDS(here("mods", "glmm", "onset_target", "glmm_prof.rds"))
# Final model for noun transparency with ES WM score
glmm_target_trans_es <- readRDS(here("mods", "glmm", "onset_target", "glmm_trans_int_es.rds"))
# Final model for phrase structure with ES WM score
glmm_target_struc_es <- readRDS(here("mods", "glmm", "onset_target", "glmm_struc_int_es.rds"))


# -----------------------------------------------------------------------------



# Data prep -------------------------------------------------------------------

# Filter time course to offset of 1st syllable (time_zero == 20, to account for 200 ms to launch saccade)
# Create sum coded fixed factors (condition)

# Merge wm scores 

wm_df <- read_csv(here::here('data', 'clean', 'wm_scores.csv'))

wm_df <- select(wm_df, -group)
det10target <- left_join(det10target, wm_df, by = "participant")

det10target <- na.omit(det10target)

# det50 <- det50[!is.na(det50$group), ]

df_10target <- det10target %>%
  filter(., time_zero == 20) %>%
  mutate(., group = fct_relevel(group, "ss", "hs", "l2"),
         noun_sum = if_else(noun_transparency == "transp", 1, -1),
         structure_sum = if_else(structure == "d_n_adj", 1, -1),
         gender_sum = if_else(gender == "masc", 1, -1),
         prof_z = (DELE - mean(DELE))/sd(DELE),
         wmen_z = (engsets_correct - mean(engsets_correct))/sd(engsets_correct),
         wmes_z = (spansets_correct - mean(spansets_correct))/sd(spansets_correct),
         AoA_sum = if_else(group != 'l2', 1, -1),
         AoA = if_else(group != 'l2', 'birth', "l2")) 


# -----------------------------------------------------------------------------



# Random effects building -----------------------------------------------------

if(F) {
  
  prop_0_ranefA <- glmer(cbind(target_count, 10 - target_count) ~ 1 +
                           (1 | participant),
                         data = df_10target, family = 'binomial',                 
                         control = glmerControl(optimizer = 'bobyqa'))
  
  prop_0_ranefB <- glmer(cbind(target_count, 10 - target_count) ~ 1 +
                           (1 | participant) +
                           (1 | target),
                         data = df_10target, family = 'binomial', 
                         control = glmerControl(optimizer = 'bobyqa'))
  
  anova(prop_0_ranefA, prop_0_ranefB, refit = F)  # keep intercept for target
  #                 Df   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
  # prop_0_ranefA    2 57149 57161 -28572    57145                         
  # prop_0_ranefB    3 56710 56729 -28352    56704 440.24  1  < 2.2e-16 ***
  
  prop_0_ranefC <- glmer(cbind(target_count, 10 - target_count) ~ 1 +
                           (1 + noun_sum | participant) +
                           (1 | target),
                         data = df_10target, family = 'binomial', 
                         control = glmerControl(optimizer = 'bobyqa'))
  
  anova(prop_0_ranefB, prop_0_ranefC, refit = F) # keep slope for target transparency
  #                 Df  AIC    BIC logLik deviance  Chisq Df Pr(>Chisq)    
  # prop_0_ranefB    3 56710 56729 -28352    56704                         
  # prop_0_ranefC    5 55766 55798 -27878    55756 948.63  2  < 2.2e-16 ***
  
  prop_0_ranefD <- glmer(cbind(target_count, 10 - target_count) ~ 1 +
                           (1 + noun_sum + structure_sum | participant) +
                           (1 | target),
                         data = df_10target, family = 'binomial', 
                         control = glmerControl(optimizer = 'bobyqa'))
  
  anova(prop_0_ranefC, prop_0_ranefD, refit = F) # keep slope for target phrase structure
  #               npar  AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
  # prop_0_ranefC    5 55766 55798 -27878    55756                         
  # prop_0_ranefD    8 54929 54979 -27456    54913 843.16  3  < 2.2e-16 ***
  
  }

# -----------------------------------------------------------------------------





# Test fixed effects ----------------------------------------------------------

if(F) {
  
  glmm_mod_0 <- prop_0_ranefD
  
  glmm_mod_AoA  <- update(glmm_mod_0,    . ~ . + AoA_sum)
  glmm_mod_prof <- update(glmm_mod_AoA,  . ~ . + prof_z)
  
  # A - noun transparency
  glmm_mod_trans  <- update(glmm_mod_prof, . ~ . + noun_sum)
  
  anova(glmm_mod_0, glmm_mod_AoA, glmm_mod_prof, glmm_mod_trans, test = "Chisq")  
  #                npar   AIC   BIC logLik deviance  Chisq  Df Pr(>Chisq)    
  # glmm_mod_0        8 54929 54979 -27456    54913                          
  # glmm_mod_AoA      9 54928 54985 -27455    54910  2.3941  1     0.1218    
  # glmm_mod_prof    10 54908 54972 -27444    54888 21.8030  1  3.021e-06 ***
  # glmm_mod_trans   11 54909 54979 -27444    54887  1.4214  1     0.2332 
  
  glmm_mod_trans_int  <- update(glmm_mod_prof, . ~ . + AoA_sum:prof_z:noun_sum)
  
  anova(glmm_mod_prof, glmm_mod_trans_int, test = "Chisq") 
  #                    npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
  # glmm_mod_prof        10 54908 54972 -27444    54888                     
  # glmm_mod_trans_int   11 54910 54980 -27444    54888 0.1159  1     0.7335
  
  
  glmm_mod_trans_gender <- update(glmm_mod_prof, . ~ . + gender_sum)
  
  ### en
  
  glmm_mod_trans_wmen <- update(glmm_mod_trans_gender, . ~ . + wmen_z)
  
  anova(glmm_mod_prof, glmm_mod_trans_gender, glmm_mod_trans_wmen, test = "Chisq") 
  #                       npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
  # glmm_mod_prof           10 54908 54972 -27444    54888                       
  # glmm_mod_trans_gender   11 54907 54977 -27442    54885 3.2648  1    0.07078 .
  # glmm_mod_trans_wmen     12 54909 54985 -27442    54885 0.4993  1    0.47979  
  
  glmm_mod_trans_int_en <- update(glmm_mod_prof, . ~ . + 
                                     AoA_sum:prof_z:noun_sum:gender_sum:wmen_z)
  
  anova(glmm_mod_prof, glmm_mod_trans_int_en, test = "Chisq") 
  #                       npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
  # glmm_mod_prof           10 54908 54972 -27444    54888                    
  # glmm_mod_trans_int_en   11 54910 54980 -27444    54888 0.275  1        0.6
  
  ### FINAL MODEL TRANSPARENCY WITH EN WM IN HS
  summary(glmm_mod_prof) 
  # Fixed effects:
  #             Estimate Std. Error z value Pr(>|z|)    
  # (Intercept) -0.020526   0.064247  -0.319    0.749    
  # AoA_sum     -0.006912   0.049774  -0.139    0.890    
  # prof_z       0.240812   0.049561   4.859 1.18e-06 ***
  
  
  # A - es
  
  glmm_mod_trans_wmes <- update(glmm_mod_trans_gender, . ~ . + wmes_z)
  
  anova(glmm_mod_prof, glmm_mod_trans_gender, glmm_mod_trans_wmes, test = "Chisq") 
  #                       npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
  # glmm_mod_prof           10 54908 54972 -27444    54888                       
  # glmm_mod_trans_gender   11 54907 54977 -27442    54885 3.2648  1    0.07078 .
  # glmm_mod_trans_wmes     12 54909 54985 -27442    54885 0.0899  1    0.76426  
  
  glmm_mod_trans_int_es <- update(glmm_mod_prof, . ~ . + 
                                    AoA_sum:prof_z:noun_sum:gender_sum:wmes_z)
  
  anova(glmm_mod_prof, glmm_mod_trans_int_es, test = "Chisq") 
  #                       npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
  # glmm_mod_prof           10 54908 54972 -27444    54888                       
  # glmm_mod_trans_int_es   11 54905 54975 -27441    54883 5.7052  1    0.01691 *
  
  ### FINAL MODEL TRANSPARENCY WITH ES WM IN HS
  summary(glmm_mod_trans_int_es) 
  # Fixed effects:
  #             Estimate Std. Error z value Pr(>|z|)    
  # (Intercept) -0.020633   0.064281  -0.321   0.7482    
  # AoA_sum                                   -0.007034   0.049771  -0.141   0.8876    
  # prof_z                                     0.241117   0.049559   4.865 1.14e-06 ***
  # AoA_sum:prof_z:noun_sum:gender_sum:wmes_z  0.028631   0.011976   2.391   0.0168 *
  
  
  
  
  # B - phrase structure
  glmm_mod_struc  <- update(glmm_mod_prof, . ~ . + structure_sum)
  
  anova(glmm_mod_0, glmm_mod_AoA, glmm_mod_prof, glmm_mod_struc, test = "Chisq")  
  #                npar   AIC   BIC logLik deviance  Chisq  Df Pr(>Chisq)    
  # glmm_mod_0        8 54929 54979 -27456    54913                          
  # glmm_mod_AoA      9 54928 54985 -27455    54910  2.3941  1    0.12179    
  # glmm_mod_prof    10 54908 54972 -27444    54888 21.8030  1  3.021e-06 ***
  # glmm_mod_struc   11 54907 54977 -27443    54885  3.1456  1    0.07613 .
  
  glmm_mod_struc_int  <- update(glmm_mod_prof, . ~ . + AoA_sum:prof_z:structure_sum)
  
  anova(glmm_mod_prof, glmm_mod_struc_int, test = "Chisq") 
  #                    npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
  # glmm_mod_prof        10 54908 54972 -27444    54888                     
  # glmm_mod_struc_int   11 54909 54979 -27443    54887 1.8272  1     0.1765
  
  glmm_mod_struc_gender <- update(glmm_mod_prof, . ~ . + gender_sum)
  
  ### en
  
  glmm_mod_struc_wmen <- update(glmm_mod_struc_gender, . ~ . + wmen_z)
  
  anova(glmm_mod_prof, glmm_mod_struc_gender, glmm_mod_struc_wmen, test = "Chisq") 
  #                       npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
  # glmm_mod_prof           10 54908 54972 -27444    54888                       
  # glmm_mod_struc_gender   11 54907 54977 -27442    54885 3.2648  1    0.07078 .
  # glmm_mod_struc_wmen     12 54909 54985 -27442    54885 0.4993  1    0.47979 
  
  glmm_mod_struc_int_en <- update(glmm_mod_prof, . ~ . + 
                                    AoA_sum:prof_z:structure_sum:gender_sum:wmen_z)
  
  anova(glmm_mod_prof, glmm_mod_struc_int_en, test = "Chisq") 
  #                       npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
  # glmm_mod_prof           10 54908 54972 -27444    54888                     
  # glmm_mod_struc_int_en   11 54909 54979 -27443    54887 1.8184  1     0.1775
  
  ### FINAL MODEL STRUCTURE WITH EN WM IN HS
  summary(glmm_mod_prof) 
  # Fixed effects:
  #             Estimate Std. Error z value Pr(>|z|)    
  # (Intercept) -0.020526   0.064247  -0.319    0.749    
  # AoA_sum     -0.006912   0.049774  -0.139    0.890    
  # prof_z       0.240812   0.049561   4.859 1.18e-06 ***
  
  # B - es
  
  glmm_mod_struc_wmes <- update(glmm_mod_struc_gender, . ~ . + wmes_z)
  
  anova(glmm_mod_prof, glmm_mod_struc_gender, glmm_mod_struc_wmes, test = "Chisq") 
  #                       npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
  # glmm_mod_prof           10 54908 54972 -27444    54888                       
  # glmm_mod_struc_gender   11 54907 54977 -27442    54885 3.2648  1    0.07078 .
  # glmm_mod_struc_wmes     12 54909 54985 -27442    54885 0.0899  1    0.76426  
  
  glmm_mod_struc_int_es <- update(glmm_mod_prof, . ~ . + 
                                    AoA_sum:prof_z:structure_sum:gender_sum:wmes_z)
  
  anova(glmm_mod_prof, glmm_mod_struc_int_es, test = "Chisq") 
  #                       npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
  # glmm_mod_prof           10 54908 54972 -27444    54888                        
  # glmm_mod_struc_int_es   11 54903 54973 -27440    54881 7.4473  1   0.006353 **
  
  ### FINAL MODEL STRUCTURE WITH ES WM IN HS
  summary(glmm_mod_struc_int_es) 
  # Fixed effects:
  #             Estimate Std. Error z value Pr(>|z|)    
  # (Intercept) -0.020345   0.064257  -0.317  0.75153    
  # AoA_sum                                        -0.006931   0.049789  -0.139  0.88929    
  # prof_z                                          0.240897   0.049578   4.859 1.18e-06 ***
  # AoA_sum:prof_z:structure_sum:gender_sum:wmes_z -0.032670   0.011961  -2.731  0.00631 ** 
  
  
}



# Save models

saveRDS(glmm_mod_0, here("mods", "glmm", "onset_target",
                                 "glmm_base.rds"))
saveRDS(glmm_mod_AoA, here("mods", "glmm", "onset_target",
                         "glmm_AoA.rds"))
saveRDS(glmm_mod_prof, here("mods", "glmm", "onset_target",
                         "glmm_prof.rds"))
saveRDS(glmm_mod_trans, here("mods", "glmm", "onset_target",
                         "glmm_trans.rds"))
saveRDS(glmm_mod_struc, here("mods", "glmm", "onset_target",
                         "glmm_struc.rds"))
saveRDS(glmm_mod_trans_int, here("mods", "glmm", "onset_target",
                             "glmm_trans_int.rds"))
saveRDS(glmm_mod_struc_int, here("mods", "glmm", "onset_target",
                             "glmm_struc_int.rds"))
saveRDS(glmm_mod_trans_gender, here("mods", "glmm", "onset_target",
                             "glmm_trans_gender.rds"))
saveRDS(glmm_mod_struc_gender, here("mods", "glmm", "onset_target",
                             "glmm_struc_gender.rds"))
saveRDS(glmm_mod_trans_wmen, here("mods", "glmm", "onset_target",
                                    "glmm_trans_wmen.rds"))
saveRDS(glmm_mod_struc_wmen, here("mods", "glmm", "onset_target",
                                    "glmm_struc_wmen.rds"))
saveRDS(glmm_mod_trans_wmes, here("mods", "glmm", "onset_target",
                                  "glmm_trans_wmes.rds"))
saveRDS(glmm_mod_struc_wmes, here("mods", "glmm", "onset_target",
                                  "glmm_struc_wmes.rds"))
saveRDS(glmm_mod_trans_int_en, here("mods", "glmm", "onset_target",
                                  "glmm_trans_int_en.rds"))
saveRDS(glmm_mod_struc_int_en, here("mods", "glmm", "onset_target",
                                  "glmm_struc_int_en.rds"))
saveRDS(glmm_mod_trans_int_es, here("mods", "glmm", "onset_target",
                                    "glmm_trans_int_es.rds"))
saveRDS(glmm_mod_struc_int_es, here("mods", "glmm", "onset_target",
                                    "glmm_struc_int_es.rds"))




######## -------------------------- NOT ADAPTED OR RUN -------------------

l2_onset_c3_dele_final %>%
  ggplot(., aes(x = l1, y = mean(target_prop), 
                color = DELE, 
                l1 = interaction(l1, DELE))) +
  facet_grid(. ~ factor(condition_sum, levels=c(1, -1))) +
  geom_hline(yintercept = 0.5, color = "black", size = 0.75, 
             lty = 3) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar',
               position = position_dodge(width = 0.5),
               width = 0.35, color = 'grey40') +
  stat_summary(fun.y = mean, geom = 'point', size = 4,
               position = position_dodge(width = 0.5)) + 
  coord_cartesian(ylim = c(0, 1)) + ylab('Target fixations') + xlab('L1') + 
  scale_x_discrete(labels = c("English", "Mandarin")) +
  scale_color_continuous(name = 'Proficiency') +
  ggtitle('Mean target fixations at V1 onset as a function of proficiency') + 
  theme_bw(base_size = 16, base_family = 'Times') -> stress_target_fix_l2_dele_onset_c3

ggsave(paste0("./figs/stress/glmm/l2/stress_target_fix_l2_dele_onset_c3.png"), stress_target_fix_l2_dele_onset_c3, width = 150,
       height = 120, units = "mm", dpi = 600)

# -----------------------------------------------------------------------------




# Model descriptives ----------------------------------------------------------
# 
MuMIn::r.squaredGLMM(prop_0_mod_final) 
#                   R2m       R2c
# theoretical 0.1644337 0.8341553
# delta       0.1528810 0.7755495

summary(prop_0_mod_final)
#                         Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)           2.2390     0.1803  12.420  < 2e-16 ***
#   groupaes             -1.0022     0.2271  -4.413 1.02e-05 ***
#   groupies             -1.4069     0.2242  -6.275 3.49e-10 ***
#   groupams             -1.3068     0.2259  -5.786 7.21e-09 ***
#   groupims             -1.6783     0.2256  -7.438 1.02e-13 ***
  
confint(prop_0_mod_final, method = "Wald")
#                            2.5 %       97.5 %
# (Intercept)           1.885679  2.5923575
# groupaes             -1.447381 -0.5571072
# groupies             -1.846331 -0.9674771
# groupams             -1.749521 -0.8641394
# groupims             -2.120526 -1.2360267



# Calculate mean target fixation as a function of group, condition, 
# for each participant. We will plot the mean and calculate the 
# bootstrapped 95% confidence interval and plot it all. 
prop_0_mod_final %>%
  group_by(., group, condition_sum, participant) %>%
  #  summarise(., meanFix = mean(target_prop)) %>%
  ggplot(., aes(x = group, y = mean(target_prop), 
                dodge = condition_sum, color = condition_sum, 
                group = interaction(group, condition_sum))) +
  geom_hline(yintercept = 0.5, color = "black", size = 0.75, 
             lty = 3) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', 
               position = position_dodge(width = 0.5), 
               width = 0.35, color = 'grey40') + 
  stat_summary(fun.y = mean, geom = 'point', size = 4,
               position = position_dodge(width = 0.5)) + 
  coord_cartesian(ylim = c(0, 1)) + ylab('Target fixations') + xlab('Group') + 
  #  scale_x_discrete(labels = c("mon", "aes", "ies", "ams", "ims")) +         
  ggtitle('Mean target fixations as a function of group\nand target type') +    
  #  scale_color_brewer(palette = "Set1", name = '', labels = c('Present', 'Preterit')) +       
  theme_bw(base_size = 16, base_family = 'Times') -> stress_rel_target_fix

# Graph to check the effects of WM

prop_0_mod_final %>%
#  group_by(., group, condition_sum, participant) %>%
  ggplot(., aes(x = group, y = mean(target_prop), 
                dodge = WM_set, color = WM_set,
                group = interaction(group, WM_set))) +
  geom_hline(yintercept = 0.5, color = "black", size = 0.75, 
             lty = 3) + 
  stat_summary(fun.data = mean_cl_boot, geom = 'errorbar', 
               position = position_dodge(width = 0.5), 
               width = 0.35, color = 'grey40') + 
  stat_summary(fun.y = mean, geom = 'point', size = 4,
               position = position_dodge(width = 0.5)) + 
  coord_cartesian(ylim = c(0, 1)) + ylab('Target fixations') + xlab('Group') + 
  #  scale_x_discrete(labels = c("mon", "aes", "ies", "ams", "ims")) +       
  ggtitle('Mean target fixations as a function of group and verbal WM') + 
  # scale_color_brewer(palette = "Set1", name = '', labels = c('Present', 'Preterit')) + 
  theme_bw(base_size = 16, base_family = 'Times') -> stress_rel_coda_fix



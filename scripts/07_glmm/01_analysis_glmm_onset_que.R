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



# Onset c1

# Load data and models --------------------------------------------------------

source(here::here("scripts", "00_load_libs.R"))
# source(here::here("scripts", "02_load_data.R"))
det10que <- read_csv(here::here("data", 'clean', 'det_10ms_onset_que.csv'))

# Final model for noun transparency with either WM score
glmm_que_trans <- readRDS(here("mods", "glmm", "onset_que", "glmm_trans.rds"))
# Final model for phrase structure with EN WM score in HS
glmm_que_struc_en <- readRDS(here("mods", "glmm", "onset_que", "glmm_struc_int_en.rds"))
# Final model for phrase structure with ES WM score in HS
glmm_que_struc_es <- readRDS(here("mods", "glmm", "onset_que", "glmm_struc_int_es.rds"))


# -----------------------------------------------------------------------------



# Data prep -------------------------------------------------------------------

# Filter time course to offset of 1st syllable (time_zero == 20, to account for 200 ms to launch saccade)
# Create sum coded fixed factors (condition)

# Merge wm scores (from 03.wm)
# wm_df <- select(wm_df, -group)
det10que <- left_join(det10que, wm_df, by = "participant")

det10que <- na.omit(det10que)

# det50 <- det50[!is.na(det50$group), ]

df_10que <- det10que %>%
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
                         data = df_10que, family = 'binomial',                 
                         control = glmerControl(optimizer = 'bobyqa'))
  
  prop_0_ranefB <- glmer(cbind(target_count, 10 - target_count) ~ 1 +
                           (1 | participant) +
                           (1 | target),
                         data = df_10que, family = 'binomial', 
                         control = glmerControl(optimizer = 'bobyqa'))
  
  anova(prop_0_ranefA, prop_0_ranefB, refit = F)  # keep intercept for target
  #                 Df    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
  # prop_0_ranefA    2 56874 56887 -28435    56870                         
  # prop_0_ranefB    3 56596 56615 -28295    56590 280.02  1  < 2.2e-16 ***
  
  prop_0_ranefC <- glmer(cbind(target_count, 10 - target_count) ~ 1 +
                           (1 + noun_sum | participant) +
                           (1 | target),
                         data = df_10que, family = 'binomial', 
                         control = glmerControl(optimizer = 'bobyqa'))
  
  anova(prop_0_ranefB, prop_0_ranefC, refit = F) # keep slope for noun transparency
  #                 Df    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)    
  # prop_0_ranefB    3 56596 56615 -28295    56590                         
  # prop_0_ranefC    5 56034 56066 -28012    56024 565.95  2  < 2.2e-16 ***
  
  prop_0_ranefD <- glmer(cbind(target_count, 10 - target_count) ~ 1 +
                           (1 + noun_sum + structure_sum | participant) +
                           (1 | target),
                         data = df_10que, family = 'binomial', 
                         control = glmerControl(optimizer = 'bobyqa'))
  
  anova(prop_0_ranefC, prop_0_ranefD, refit = F) # keep slope for noun transparency
  #               npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)    
  # prop_0_ranefC    5 56034 56066 -28012    56024                         
  # prop_0_ranefD    8 54781 54832 -27382    54765 1259.1  3  < 2.2e-16 ***
  
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
  # glmm_mod_0        8 54781 54832 -27382    54765                       
  # glmm_mod_AoA      9 54783 54840 -27382    54765 0.2860  1    0.59278  
  # glmm_mod_prof    10 54782 54845 -27381    54762 3.0390  1    0.08128 .
  # glmm_mod_trans   11 54778 54848 -27378    54756 5.3527  1    0.02069 *
  
  glmm_mod_trans_int  <- update(glmm_mod_trans, . ~ . + AoA_sum:prof_z:noun_sum)
  
  anova(glmm_mod_trans, glmm_mod_trans_int, test = "Chisq") 
  #                    npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
  # glmm_mod_trans       11 54778 54848 -27378    54756                     
  # glmm_mod_trans_int   12 54780 54856 -27378    54756 0.2053  1     0.6505
  
  
  ### en
  
  glmm_mod_trans_gender <- update(glmm_mod_trans, . ~ . + gender_sum)
  glmm_mod_trans_wmen <- update(glmm_mod_trans_gender, . ~ . + wmen_z)
  
  anova(glmm_mod_trans, glmm_mod_trans_gender, glmm_mod_trans_wmen, test = "Chisq") 
  #                       npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
  # glmm_mod_trans          11 54778 54848 -27378    54756                     
  # glmm_mod_trans_gender   12 54779 54855 -27377    54755 1.6977  1     0.1926
  # glmm_mod_trans_wmen     13 54780 54863 -27377    54754 0.4689  1     0.4935
  
  glmm_mod_trans_int_en <- update(glmm_mod_trans, . ~ . + 
                                     AoA_sum:prof_z:noun_sum:gender_sum:wmen_z)
  
  anova(glmm_mod_trans, glmm_mod_trans_int_en, test = "Chisq") 
  #                       npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
  # glmm_mod_trans          11 54778 54848 -27378    54756                     
  # glmm_mod_trans_int_en   12 54780 54857 -27378    54756 0.1471  1     0.7013
  
  ### FINAL MODEL TRANSPARENCY WITH EN WM IN HS
  summary(glmm_mod_trans) 
  # Fixed effects:
  #                       Estimate Std. Error z value Pr(>|z|)    
  # (Intercept)            -0.32237    0.05711  -5.645 1.65e-08 ***
  # AoA_sum                -0.05278    0.04718  -1.119   0.2632    
  # prof_z                  0.08268    0.04732   1.747   0.0806 .  
  # noun_sum                0.06665    0.02852   2.337   0.0194 * 
  
  
  # A - es
  
  glmm_mod_trans_wmes <- update(glmm_mod_trans_gender, . ~ . + wmes_z)
  
  anova(glmm_mod_trans, glmm_mod_trans_gender, glmm_mod_trans_wmes, test = "Chisq") 
  #                       npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
  # glmm_mod_trans          11 54778 54848 -27378    54756                     
  # glmm_mod_trans_gender   12 54779 54855 -27377    54755 1.6977  1     0.1926
  # glmm_mod_trans_wmes     13 54781 54863 -27377    54755 0.0047  1     0.9451
  
  glmm_mod_trans_int_es <- update(glmm_mod_trans, . ~ . + 
                                    AoA_sum:prof_z:noun_sum:gender_sum:wmes_z)
  
  anova(glmm_mod_trans, glmm_mod_trans_int_es, test = "Chisq") 
  #                       npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
  # glmm_mod_trans          11 54778 54848 -27378    54756                     
  # glmm_mod_trans_int_es   12 54779 54855 -27377    54755 1.7055  1     0.1916
  
  ### FINAL MODEL TRANSPARENCY WITH ES WM IN HS
  summary(glmm_mod_trans) 
  # Fixed effects:
  #                       Estimate Std. Error z value Pr(>|z|)    
  # (Intercept)            -0.32237    0.05711  -5.645 1.65e-08 ***
  # AoA_sum                -0.05278    0.04718  -1.119   0.2632    
  # prof_z                  0.08268    0.04732   1.747   0.0806 .  
  # noun_sum                0.06665    0.02852   2.337   0.0194 * 
  
  
  
  
  # B - phrase structure
  glmm_mod_struc  <- update(glmm_mod_prof, . ~ . + structure_sum)
  
  anova(glmm_mod_0, glmm_mod_AoA, glmm_mod_prof, glmm_mod_struc, test = "Chisq")  
  #                npar   AIC   BIC logLik deviance  Chisq  Df Pr(>Chisq)    
  # glmm_mod_0        8 54781 54832 -27382    54765                       
  # glmm_mod_AoA      9 54783 54840 -27382    54765 0.2860  1    0.59278  
  # glmm_mod_prof    10 54782 54845 -27381    54762 3.0390  1    0.08128 .
  # glmm_mod_struc   11 54783 54854 -27381    54761 0.1065  1    0.74422 
  
  glmm_mod_struc_int  <- update(glmm_mod_0, . ~ . + AoA_sum:prof_z:structure_sum)
  
  anova(glmm_mod_struc, glmm_mod_struc_int, test = "Chisq") 
  #                    npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
  # glmm_mod_struc_int    9 54782 54839 -27382    54764                     
  # glmm_mod_struc       11 54783 54854 -27381    54761 2.4458  2     0.2944
  
  
  ### en
  
  glmm_mod_struc_gender <- update(glmm_mod_0, . ~ . + gender_sum)
  glmm_mod_struc_wmen <- update(glmm_mod_struc_gender, . ~ . + wmen_z)
  
  anova(glmm_mod_0, glmm_mod_struc_gender, glmm_mod_struc_wmen, test = "Chisq") 
  #                       npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
  # glmm_mod_0               8 54781 54832 -27382    54765                     
  # glmm_mod_struc_gender    9 54781 54839 -27382    54763 1.6909  1     0.1935
  # glmm_mod_struc_wmen     10 54783 54847 -27382    54763 0.0009  1     0.9762
  
  glmm_mod_struc_int_en <- update(glmm_mod_0, . ~ . + 
                                    AoA_sum:prof_z:structure_sum:gender_sum:wmen_z)
  
  anova(glmm_mod_0, glmm_mod_struc_int_en, test = "Chisq") 
  #                       npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
  # glmm_mod_0               8 54781 54832 -27382    54765                       
  # glmm_mod_struc_int_en    9 54772 54830 -27377    54754 10.44  1   0.001233 **
  
  ### FINAL MODEL STRUCTURE WITH EN WM IN HS
  summary(glmm_mod_struc_int_en) 
  # Fixed effects:
  #                                                           Estimate Std. Error z value Pr(>|z|)    
  # (Intercept)                                               -0.33496    0.05794  -5.781 7.44e-09 ***
  # AoA_sum:prof_z:structure_sum:gender_sum:wmen_z             0.03697    0.01143   3.233  0.00122 ** 
  
  
  # A - es
  
  glmm_mod_struc_wmes <- update(glmm_mod_struc_gender, . ~ . + wmes_z)
  
  anova(glmm_mod_0, glmm_mod_struc_gender, glmm_mod_struc_wmes, test = "Chisq") 
  #                       npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
  # glmm_mod_0               8 54781 54832 -27382    54765                     
  # glmm_mod_struc_gender    9 54781 54839 -27382    54763 1.6909  1     0.1935
  # glmm_mod_struc_wmes     10 54782 54846 -27381    54762 0.7407  1     0.3894
  
  glmm_mod_struc_int_es <- update(glmm_mod_0, . ~ . + 
                                    AoA_sum:prof_z:structure_sum:gender_sum:wmes_z)
  
  anova(glmm_mod_0, glmm_mod_struc_int_es, test = "Chisq") 
  #                       npar   AIC   BIC logLik deviance  Chisq Df Pr(>Chisq)
  # glmm_mod_0               8 54781 54832 -27382    54765                         
  # glmm_mod_struc_int_es    9 54769 54826 -27375    54751 14.216  1  0.0001629 ***
  
  ### FINAL MODEL STRUCTURE WITH ES WM IN HS
  summary(glmm_mod_struc_int_es) 
  # Fixed effects:
  #                                                Estimate Std. Error z value Pr(>|z|)    
  # (Intercept)                                    -0.33493    0.05796  -5.778 7.55e-09 ***
  # AoA_sum:prof_z:structure_sum:gender_sum:wmes_z  0.04477    0.01187   3.773 0.000161 ***
  
  
}



# Save models

saveRDS(glmm_mod_0, here("mods", "glmm", "onset_que",
                                 "glmm_base.rds"))
saveRDS(glmm_mod_AoA, here("mods", "glmm", "onset_que",
                         "glmm_AoA.rds"))
saveRDS(glmm_mod_prof, here("mods", "glmm", "onset_que",
                         "glmm_prof.rds"))
saveRDS(glmm_mod_trans, here("mods", "glmm", "onset_que",
                         "glmm_trans.rds"))
saveRDS(glmm_mod_struc, here("mods", "glmm", "onset_que",
                         "glmm_struc.rds"))
saveRDS(glmm_mod_trans_int, here("mods", "glmm", "onset_que",
                             "glmm_trans_int.rds"))
saveRDS(glmm_mod_struc_int, here("mods", "glmm", "onset_que",
                             "glmm_struc_int.rds"))
saveRDS(glmm_mod_trans_gender, here("mods", "glmm", "onset_que",
                             "glmm_trans_gender.rds"))
saveRDS(glmm_mod_struc_gender, here("mods", "glmm", "onset_que",
                             "glmm_struc_gender.rds"))
saveRDS(glmm_mod_trans_wmen, here("mods", "glmm", "onset_que",
                                    "glmm_trans_wmen.rds"))
saveRDS(glmm_mod_struc_wmen, here("mods", "glmm", "onset_que",
                                    "glmm_struc_wmen.rds"))
saveRDS(glmm_mod_trans_wmes, here("mods", "glmm", "onset_que",
                                  "glmm_trans_wmes.rds"))
saveRDS(glmm_mod_struc_wmes, here("mods", "glmm", "onset_que",
                                  "glmm_struc_wmes.rds"))
saveRDS(glmm_mod_trans_int_en, here("mods", "glmm", "onset_que",
                                  "glmm_trans_int_en.rds"))
saveRDS(glmm_mod_struc_int_en, here("mods", "glmm", "onset_que",
                                  "glmm_struc_int_en.rds"))
saveRDS(glmm_mod_trans_int_es, here("mods", "glmm", "onset_que",
                                    "glmm_trans_int_es.rds"))
saveRDS(glmm_mod_struc_int_es, here("mods", "glmm", "onset_que",
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



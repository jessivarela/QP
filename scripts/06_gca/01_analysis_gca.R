

source(here::here("scripts", "02_load_data.R"))
 
wm_df <- select(wm_df, -group)

det50 <- left_join(det50, wm_df, by = "participant")

det50 <- na.omit(det50)

det_gc_subset <- det50 %>%
  filter(., time_zero >= -4 & time_zero <= 12) %>%
  mutate(., group = fct_relevel(group, "ss", "hs", "l2"),
         noun_sum = if_else(noun_transparency == "transp", 1, -1),
         structure_sum = if_else(structure == "d_n_adj", 1, -1),
         gender_sum = if_else(gender == "masc", 1, -1),
         prof_z = (DELE - mean(DELE))/sd(DELE),
         wmen_z = (engsets_correct - mean(engsets_correct))/sd(engsets_correct),
         wmes_z = (spansets_correct - mean(spansets_correct))/sd(spansets_correct),
         AoA_sum = if_else(group != 'l2', 1, -1)) %>%      
  poly_add_columns(., time_zero, degree = 3, prefix = "ot")


# -----------------------------------------------------------------------------



# Build up random effects to test time terms
if(F){
  
  mod_ot1 <-
    lmer(eLog ~ 1 + ot1 +
           (1 + structure_sum + ot1 | participant),
         control = lmerControl(optimizer = 'bobyqa', optCtrl=list(maxfun=3e5)),   
         data = det_gc_subset, weights = 1/wts, REML = F)
  
  mod_ot2 <-
    update(mod_ot1, . ~ . -(1 + structure_sum + ot1 | participant) +
             ot2 + (1 + structure_sum + ot1 + ot2 | participant))
  
  mod_ot3 <-
    update(mod_ot2, . ~ . -(1 + structure_sum + ot1 + ot2 | participant) +
             ot3 + (1 + structure_sum + ot1 + ot2 + ot3 | participant))
  
  anova(mod_ot1, mod_ot2, mod_ot3)
  #         npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)    
  # mod_ot1    9 414350 414433 -207166   414332                          
  # mod_ot2   14 413930 414059 -206951   413902 429.981  5  < 2.2e-16 ***
  # mod_ot3   20 413878 414062 -206919   413838  63.627  6  8.225e-12 ***
  
  mod_ot4 <- update(mod_ot3, . ~ . -(1 + structure_sum + ot1 + ot2 + ot3 | participant) +
                      + (1 + structure_sum + noun_sum + ot1 + ot2 + ot3 | participant))
  
  anova(mod_ot3, mod_ot4)
  #         npar    AIC    BIC  logLik deviance Chisq Df Pr(>Chisq)    
  # mod_ot3   20 413878 414062 -206919   413838                         
  # mod_ot4   26 413617 413856 -206782   413565 273.44  6  < 2.2e-16 ***
  
  
  mod_ot0 <- update(mod_ot4, . ~ . + (1 | target))
  
  mod_ot1a <- update(mod_ot0, . ~ . -(1 | target) + (1 + ot1 | target))
  
  mod_ot2a <- update(mod_ot1a, . ~ . -(1 + ot1 | target) +
                       + (1 + ot1 + ot2 | target))
  
  anova(mod_ot4, mod_ot0, mod_ot1a, mod_ot2a)
  #          npar   AIC   BIC logLik deviance   Chisq Df Pr(>Chisq)    
  # mod_ot4    26 413617 413856 -206782   413565                          
  # mod_ot0    27 413309 413558 -206628   413255 309.734  1  < 2.2e-16 ***
  # mod_ot1a   29 413085 413352 -206513   413027 228.321  2  < 2.2e-16 ***
  # mod_ot2a   32 413026 413320 -206481   412962  64.884  3  5.311e-14 *** 
  
  
}


# Fixed effects -----------------------------------------------------------

gca_mod_base <- mod_ot2a
# lmer(eLog ~ 1 + (ot1 + ot2 + ot3) +
#        (1 + structure_sum + noun_sum + (ot1 + ot2 + ot3) | participant) +
#        (1 + ot1 + ot2 + ot3 | target),
#      control = lmerControl(optimizer = 'bobyqa', optCtrl = list(maxfun = 3e5)), 
#      REML = F,
#      data = filter(mon_data)) 

# add AoA effect to intercept, linear slope, quadratic, and cubic time terms
gca_mod_AoA_0 <- update(gca_mod_base,    . ~ . + AoA_sum) 
gca_mod_AoA_1 <- update(gca_mod_AoA_0,   . ~ . + ot1:AoA_sum) 
gca_mod_AoA_2 <- update(gca_mod_AoA_1,   . ~ . + ot2:AoA_sum) 
gca_mod_AoA_3 <- update(gca_mod_AoA_2,   . ~ . + ot3:AoA_sum) 

AoA_anova <-
  anova(gca_mod_base, gca_mod_AoA_0, gca_mod_AoA_1,
        gca_mod_AoA_2, gca_mod_AoA_3)
#               npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)   
# gca_mod_base    33 413028 413331 -206481   412962 0.1587  1   0.690344   
# gca_mod_AoA_1   34 413020 413333 -206476   412952 9.4298  1   0.002135 **
# gca_mod_AoA_2   35 413022 413344 -206476   412952 0.1058  1   0.745027   
# gca_mod_AoA_3   36 413016 413348 -206472   412944 7.7619  1   0.005336 **


# add struciciency effect to intercept, linear slope, quadratic, and cubic time terms
gca_mod_prof_0 <- update(gca_mod_AoA_3,  . ~ . + prof_z) 
gca_mod_prof_1 <- update(gca_mod_prof_0, . ~ . + ot1:prof_z) 
gca_mod_prof_2 <- update(gca_mod_prof_1, . ~ . + ot2:prof_z) 
gca_mod_prof_3 <- update(gca_mod_prof_2, . ~ . + ot3:prof_z) 

prof_anova <-
  anova(gca_mod_AoA_3, gca_mod_prof_0, gca_mod_prof_1,
        gca_mod_prof_2, gca_mod_prof_3)
#                npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)    
# gca_mod_AoA_3    36 413016 413348 -206472   412944                          
# gca_mod_prof_0   37 413004 413345 -206465   412930 13.9323  1  0.0001895 ***
# gca_mod_prof_1   38 412980 413330 -206452   412904 26.4696  1  2.677e-07 ***
# gca_mod_prof_2   39 412981 413340 -206452   412903  0.8303  1  0.3621997    
# gca_mod_prof_3   40 412972 413340 -206446   412892 11.1615  1  0.0008351 ***


# ROUTE 1 - noun transparency

# add noun transparency effect to intercept, linear slope, quadratic, and cubic time terms
gca_mod_trans_0 <- update(gca_mod_prof_3,  . ~ . + noun_sum) 
gca_mod_trans_1 <- update(gca_mod_trans_0, . ~ . + ot1:noun_sum) 
gca_mod_trans_2 <- update(gca_mod_trans_1, . ~ . + ot2:noun_sum) 
gca_mod_trans_3 <- update(gca_mod_trans_2, . ~ . + ot3:noun_sum) 

trans_anova <-
  anova(gca_mod_prof_3, gca_mod_trans_0, gca_mod_trans_1,
        gca_mod_trans_2, gca_mod_trans_3)
#                 npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)    
# gca_mod_prof_3    40 412972 413340 -206446   412892                       
# gca_mod_trans_0   41 412971 413349 -206445   412889 2.8040  1    0.09403 .
# gca_mod_trans_1   42 412972 413358 -206444   412888 1.6132  1    0.20404  
# gca_mod_trans_2   43 412973 413369 -206444   412887 0.4442  1    0.50509  
# gca_mod_trans_3   44 412973 413378 -206443   412885 1.6163  1    0.20360   


# add interactions
gca_mod_int_trans_0 <- update(gca_mod_prof_3,        . ~ . + AoA_sum:prof_z:noun_sum) 
gca_mod_int_trans_1 <- update(gca_mod_int_trans_0,   . ~ . + ot1:AoA_sum:prof_z:noun_sum) 
gca_mod_int_trans_2 <- update(gca_mod_int_trans_1,   . ~ . + ot2:AoA_sum:prof_z:noun_sum) 
gca_mod_int_trans_3 <- update(gca_mod_int_trans_2,   . ~ . + ot3:AoA_sum:prof_z:noun_sum) 

int_transparency_anova <-
  anova(gca_mod_prof_3, gca_mod_int_trans_0, gca_mod_int_trans_1,
        gca_mod_int_trans_2, gca_mod_int_trans_3)
#                     npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)   
# gca_mod_prof_3        40 412972 413340 -206446   412892                       
# gca_mod_int_trans_0   41 412974 413351 -206446   412892 0.3545  1    0.55156  
# gca_mod_int_trans_1   42 412970 413357 -206443   412886 5.6600  1    0.01736 *
# gca_mod_int_trans_2   43 412972 413368 -206443   412886 0.1895  1    0.66334  
# gca_mod_int_trans_3   44 412973 413378 -206443   412885 0.3235  1    0.56953    


# add gender
gca_mod_trans_gender_0 <- update(gca_mod_int_trans_1, . ~ . + gender_sum)
gca_mod_trans_gender_1 <- update(gca_mod_trans_gender_0,   . ~ . + ot1:gender_sum)
gca_mod_trans_gender_2 <- update(gca_mod_trans_gender_1,   . ~ . + ot2:gender_sum)
gca_mod_trans_gender_3 <- update(gca_mod_trans_gender_2,   . ~ . + ot3:gender_sum)

trans_gender_anova <-
  anova(gca_mod_int_trans_1, gca_mod_trans_gender_0, gca_mod_trans_gender_1,
        gca_mod_trans_gender_2, gca_mod_trans_gender_3)
#                        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
# gca_mod_int_trans_1      42 412970 413357 -206443   412886                       
# gca_mod_trans_gender_0   43 412971 413367 -206443   412885 0.8565  1    0.35471  
# gca_mod_trans_gender_1   44 412968 413373 -206440   412880 4.6628  1    0.03082 *
# gca_mod_trans_gender_2   45 412970 413385 -206440   412880 0.0120  1    0.91266  
# gca_mod_trans_gender_3   46 412966 413389 -206437   412874 6.5800  1    0.01031 *

## ROUTE 1.A.

# add wm EN
gca_mod_trans_wmen_0 <- update(gca_mod_trans_gender_3, . ~ . + wmen_z)
gca_mod_trans_wmen_1 <- update(gca_mod_trans_wmen_0,   . ~ . + ot1:wmen_z)
gca_mod_trans_wmen_2 <- update(gca_mod_trans_wmen_1,   . ~ . + ot2:wmen_z)
gca_mod_trans_wmen_3 <- update(gca_mod_trans_wmen_2,   . ~ . + ot3:wmen_z)

trans_wmen_anova <-
  anova(gca_mod_trans_gender_3, gca_mod_trans_wmen_0, gca_mod_trans_wmen_1,
        gca_mod_trans_wmen_2, gca_mod_trans_wmen_3)
#                        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
# gca_mod_trans_gender_3   46 412966 413389 -206437   412874                       
# gca_mod_trans_wmen_0     47 412968 413400 -206437   412874 0.0448  1    0.83242  
# gca_mod_trans_wmen_1     48 412969 413411 -206437   412873 0.6646  1    0.41492  
# gca_mod_trans_wmen_2     49 412968 413419 -206435   412870 3.2919  1    0.06962 .
# gca_mod_trans_wmen_3     50 412969 413429 -206435   412869 0.6764  1    0.41085 

# add interactions
gca_mod_int_trans_en_0 <- update(gca_mod_trans_gender_3,   . ~ . + AoA_sum:prof_z:noun_sum:gender_sum:wmen_z)
gca_mod_int_trans_en_1 <- update(gca_mod_int_trans_en_0,   . ~ . + ot1:AoA_sum:prof_z:noun_sum:gender_sum:wmen_z)
gca_mod_int_trans_en_2 <- update(gca_mod_int_trans_en_1,   . ~ . + ot2:AoA_sum:prof_z:noun_sum:gender_sum:wmen_z)
gca_mod_int_trans_en_3 <- update(gca_mod_int_trans_en_2,   . ~ . + ot3:AoA_sum:prof_z:noun_sum:gender_sum:wmen_z)

int_transparency_en_anova <-
  anova(gca_mod_trans_gender_3, gca_mod_int_trans_en_0, gca_mod_int_trans_en_1,
        gca_mod_int_trans_en_2, gca_mod_int_trans_en_3)
#                        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)   
# gca_mod_trans_gender_3   46 412966 413389 -206437   412874                        
# gca_mod_int_trans_en_0   47 412965 413398 -206435   412871 2.8969  1   0.088749 . 
# gca_mod_int_trans_en_1   48 412966 413408 -206435   412870 0.5699  1   0.450301   
# gca_mod_int_trans_en_2   49 412968 413419 -206435   412870 0.0574  1   0.810711   
# gca_mod_int_trans_en_3   50 412962 413422 -206431   412862 8.3966  1   0.003759 **

## ROUTE 1.B.

# add wm ES
gca_mod_trans_wmes_0 <- update(gca_mod_trans_gender_3, . ~ . + wmes_z)
gca_mod_trans_wmes_1 <- update(gca_mod_trans_wmes_0,   . ~ . + ot1:wmes_z)
gca_mod_trans_wmes_2 <- update(gca_mod_trans_wmes_1,   . ~ . + ot2:wmes_z)
gca_mod_trans_wmes_3 <- update(gca_mod_trans_wmes_2,   . ~ . + ot3:wmes_z)

trans_wmes_anova <-
  anova(gca_mod_trans_gender_3, gca_mod_trans_wmes_0, gca_mod_trans_wmes_1,
        gca_mod_trans_wmes_2, gca_mod_trans_wmes_3)
#                        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
# gca_mod_trans_gender_3   46 412966 413389 -206437   412874                       
# gca_mod_trans_wmes_0     47 412966 413399 -206436   412872 1.6030  1    0.20548  
# gca_mod_trans_wmes_1     48 412963 413405 -206434   412867 4.7860  1    0.02869 *
# gca_mod_trans_wmes_2     49 412962 413413 -206432   412864 3.0892  1    0.07881 .
# gca_mod_trans_wmes_3     50 412964 413424 -206432   412864 0.6593  1    0.41680  

# add interactions
gca_mod_int_trans_es_0 <- update(gca_mod_trans_wmes_1,     . ~ . + AoA_sum:prof_z:noun_sum:gender_sum:wmes_z)
gca_mod_int_trans_es_1 <- update(gca_mod_int_trans_es_0,   . ~ . + ot1:AoA_sum:prof_z:noun_sum:gender_sum:wmes_z)
gca_mod_int_trans_es_2 <- update(gca_mod_int_trans_es_1,   . ~ . + ot2:AoA_sum:prof_z:noun_sum:gender_sum:wmes_z)
gca_mod_int_trans_es_3 <- update(gca_mod_int_trans_es_2,   . ~ . + ot3:AoA_sum:prof_z:noun_sum:gender_sum:wmes_z)

int_transparency_es_anova <-
  anova(gca_mod_trans_wmes_1, gca_mod_int_trans_es_0, gca_mod_int_trans_es_1,
        gca_mod_int_trans_es_2, gca_mod_int_trans_es_3)
#                        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)   
# gca_mod_trans_wmes_1     48 412963 413405 -206434   412867                        
# gca_mod_int_trans_es_0   49 412965 413416 -206433   412867 0.5744  1   0.448514   
# gca_mod_int_trans_es_1   50 412966 413426 -206433   412866 0.6624  1   0.415702   
# gca_mod_int_trans_es_2   51 412968 413438 -206433   412866 0.0010  1   0.975201   
# gca_mod_int_trans_es_3   52 412962 413440 -206429   412858 8.3726  1   0.003809 **



# ROUTE 2 - phrase structure

# add phrase structure effect to intercept, linear slope, quadratic, and cubic time terms
gca_mod_struc_0 <- update(gca_mod_prof_3,  . ~ . + structure_sum) 
gca_mod_struc_1 <- update(gca_mod_struc_0, . ~ . + ot1:structure_sum) 
gca_mod_struc_2 <- update(gca_mod_struc_1, . ~ . + ot2:structure_sum) 
gca_mod_struc_3 <- update(gca_mod_struc_2, . ~ . + ot3:structure_sum) 

struc_anova <-
  anova(gca_mod_prof_3, gca_mod_struc_0, gca_mod_struc_1,
        gca_mod_struc_2, gca_mod_struc_3)
#                 npar    AIC    BIC  logLik deviance   Chisq Df Pr(>Chisq)    
# gca_mod_prof_3    40 412972 413340 -206446   412892                     
# gca_mod_struc_0   41 412972 413349 -206445   412890 2.0775  1     0.1495
# gca_mod_struc_1   42 412973 413360 -206445   412889 0.6610  1     0.4162
# gca_mod_struc_2   43 412973 413369 -206444   412887 1.9006  1     0.1680
# gca_mod_struc_3   44 412975 413380 -206443   412887 0.6224  1     0.4302   


gca_mod_int_struc_0 <- update(gca_mod_prof_3,        . ~ . + AoA_sum:prof_z:structure_sum) 
gca_mod_int_struc_1 <- update(gca_mod_int_struc_0,   . ~ . + ot1:AoA_sum:prof_z:structure_sum) 
gca_mod_int_struc_2 <- update(gca_mod_int_struc_1,   . ~ . + ot2:AoA_sum:prof_z:structure_sum) 
gca_mod_int_struc_3 <- update(gca_mod_int_struc_2,   . ~ . + ot3:AoA_sum:prof_z:structure_sum) 

int_structure_anova <-
  anova(gca_mod_prof_3, gca_mod_int_struc_0, gca_mod_int_struc_1,
        gca_mod_int_struc_2, gca_mod_int_struc_3)
#                     npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)   
# gca_mod_prof_3        40 412972 413340 -206446   412892                     
# gca_mod_int_struc_0   41 412973 413351 -206446   412891 0.5596  1     0.4544
# gca_mod_int_struc_1   42 412975 413362 -206446   412891 0.0925  1     0.7610
# gca_mod_int_struc_2   43 412976 413372 -206445   412890 0.9467  1     0.3306
# gca_mod_int_struc_3   44 412978 413383 -206445   412890 0.3348  1     0.5628   


# add gender
gca_mod_struc_gender_0 <- update(gca_mod_prof_3,           . ~ . + gender_sum) 
gca_mod_struc_gender_1 <- update(gca_mod_struc_gender_0,   . ~ . + ot1:gender_sum) 
gca_mod_struc_gender_2 <- update(gca_mod_struc_gender_1,   . ~ . + ot2:gender_sum) 
gca_mod_struc_gender_3 <- update(gca_mod_struc_gender_2,   . ~ . + ot3:gender_sum) 

struc_gender_anova <-
  anova(gca_mod_prof_3, gca_mod_struc_gender_0, gca_mod_struc_gender_1,
        gca_mod_struc_gender_2, gca_mod_struc_gender_3)
#                        npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
# gca_mod_prof_3           40 412972 413340 -206446   412892                       
# gca_mod_struc_gender_0   41 412973 413350 -206446   412891 0.8683  1    0.35143  
# gca_mod_struc_gender_1   42 412970 413357 -206443   412886 4.6601  1    0.03087 *
# gca_mod_struc_gender_2   43 412972 413368 -206443   412886 0.0110  1    0.91665  
# gca_mod_struc_gender_3   44 412968 413373 -206440   412880 6.5388  1    0.01055 *

## ROUTE 2.A.
# add wm EN
gca_mod_struc_wmen_0 <- update(gca_mod_struc_gender_3, . ~ . + wmen_z) 
gca_mod_struc_wmen_1 <- update(gca_mod_struc_wmen_0,   . ~ . + ot1:wmen_z) 
gca_mod_struc_wmen_2 <- update(gca_mod_struc_wmen_1,   . ~ . + ot2:wmen_z) 
gca_mod_struc_wmen_3 <- update(gca_mod_struc_wmen_2,   . ~ . + ot3:wmen_z) 

struc_wmen_anova <-
  anova(gca_mod_struc_gender_3, gca_mod_struc_wmen_0, gca_mod_struc_wmen_1,
        gca_mod_struc_wmen_2, gca_mod_struc_wmen_3)
#                        npar    AIC    BIC  logLik deviance    Chisq Df Pr(>Chisq)    
# gca_mod_struc_gender_3   44 412968 413373 -206440   412880                           
# gca_mod_struc_wmen_0     45 412970 413384 -206440   412880   0.0333  1     0.8553    
# gca_mod_struc_wmen_1     46 412971 413395 -206440   412879   0.6932  1     0.4051    
# gca_mod_struc_wmen_2     47 413170 413603 -206538   413076   0.0000  1     1.0000    
# gca_mod_struc_wmen_3     48 412971 413413 -206438   412875 201.1089  1     <2e-16 ***

mod_type <- "gca_mod"
mod_spec <- c('_base', 
              "_AoA_0", "_AoA_1", "_AoA_2", "_AoA_3", 
              "_prof_0", "_prof_1", "_prof_2", "_prof_3",
              "_trans_0", "_trans_1", "_trans_2", "_trans_3",
              "_trans_gender_0", "_trans_gender_1", "_trans_gender_2", "_trans_gender_3",
              "_trans_wmen_0", "_trans_wmen_1", "_trans_wmen_2", "_trans_wmen_3",
              "_trans_wmes_0", "_trans_wmes_1", "_trans_wmes_2", "_trans_wmes_3",
              "_int_trans_en_0", "_int_trans_en_1", "_int_trans_en_2", "_int_trans_en_3",
              "_int_trans_es_0", "_int_trans_es_1", "_int_trans_es_2", "_int_trans_es_3",
              "_struc_0", "_struc_1", "_struc_2", "_struc_3",
              "_struc_gender_0", "_struc_gender_1", "_struc_gender_2", "_struc_gender_3",
              "_struc_wmen_0", "_struc_wmen_1", "_struc_wmen_2", "_struc_wmen_3",
              "_int_trans_0", "_int_trans_1", "_int_trans_2", "_int_trans_3",
              "_int_struc_0", "_int_struc_1", "_int_struc_2", "_int_struc_3"
)


# Store ind models in list
gca_mods <- mget(c(paste0(mod_type, mod_spec)))

save(gca_mods,
     file = here("mods", "gca", 
                 "gca_mods.Rdata"))


# save anova comparison
nested_model_comparisons <-
  mget(c("AoA_anova", "prof_anova", "trans_anova", "int_transparency_anova", "trans_gender_anova",
         "trans_wmen_anova", "trans_wmes_anova", "int_transparency_en_anova", "int_transparency_es_anova",
         "struc_anova", "int_structure_anova", "struc_gender_anova", "struc_wmen_anova"))

save(nested_model_comparisons,
     file = here("mods", "gca", 
                 "nested_model_comparisons.Rdata"))






# add interactions
gca_mod_int_struc_en_0 <- update(gca_mod_struc_wmen_3,     . ~ . + AoA_sum:prof_z:structure_sum:gender_sum:wmen_z)
gca_mod_int_struc_en_1 <- update(gca_mod_int_struc_en_0,   . ~ . + ot1:AoA_sum:prof_z:structure_sum:gender_sum:wmen_z)
gca_mod_int_struc_en_2 <- update(gca_mod_int_struc_en_1,   . ~ . + ot2:AoA_sum:prof_z:structure_sum:gender_sum:wmen_z)
gca_mod_int_struc_en_3 <- update(gca_mod_int_struc_en_2,   . ~ . + ot3:AoA_sum:prof_z:structure_sum:gender_sum:wmen_z)

int_structure_en_anova <-
  anova(gca_mod_struc_wmen_3, gca_mod_int_struc_en_0, gca_mod_int_struc_en_1,
        gca_mod_int_struc_en_2, gca_mod_int_struc_en_3)



## ROUTE 2.B.
# add wm ES
gca_mod_struc_wmes_0 <- update(gca_mod_struc_gender_3, . ~ . + wmes_z) 
gca_mod_struc_wmes_1 <- update(gca_mod_struc_wmes_0,   . ~ . + ot1:wmes_z) 
gca_mod_struc_wmes_2 <- update(gca_mod_struc_wmes_1,   . ~ . + ot2:wmes_z) 
gca_mod_struc_wmes_3 <- update(gca_mod_struc_wmes_2,   . ~ . + ot3:wmes_z) 

struc_wmes_anova <-
  anova(gca_mod_struc_gender_3, gca_mod_struc_wmes_0, gca_mod_struc_wmes_1,
        gca_mod_struc_wmes_2, gca_mod_struc_wmes_3)

# add interactions
gca_mod_int_struc_es_0 <- update(gca_mod_,   . ~ . + AoA_sum:prof_z:structure_sum:gender_sum:wmes_z)
gca_mod_int_struc_es_1 <- update(gca_mod_int_struc_en_0,   . ~ . + ot1:AoA_sum:prof_z:structure_sum:gender_sum:wmes_z)
gca_mod_int_struc_es_2 <- update(gca_mod_int_struc_en_1,   . ~ . + ot2:AoA_sum:prof_z:structure_sum:gender_sum:wmes_z)
gca_mod_int_struc_es_3 <- update(gca_mod_int_struc_en_2,   . ~ . + ot3:AoA_sum:prof_z:structure_sum:gender_sum:wmes_z)

int_structure_es_anova <-
  anova(gca_mod_, gca_mod_int_struc_es_0, gca_mod_int_struc_es_1,
        gca_mod_int_struc_es_2, gca_mod_int_struc_es_3)



# Model predictions for plotting ---------------------------------------------

# Create design dataframe for predictions
new_dat_mon <- mon_data %>%
  dplyr::select(time_zero, ot1:ot3, condition_sum) %>% #, ospan
  distinct

new_dat_l2 <- l2_data %>%
  dplyr::select(l1, time_zero, ot1:ot3, condition_sum, ospan, DELE_z, use_z) %>%
  distinct

write_csv(new_dat_l2, here::here('new_dat_l2.csv'))
new_dat_l2 <- read_csv(here::here('new_dat_l2.csv'))

# Get model predictions and SE
fits_all_mon <- predictSE(gca_mod_mon_base, new_dat_mon) %>%  
  as_tibble %>%
  bind_cols(new_dat_mon) %>%
  rename(se = se.fit) %>%
  mutate(ymin = fit - se, ymax = fit + se)

fits_all_l2_dele <- predictSE(gca_l2_mod_dele_final, new_dat_l2) %>%
  as_tibble %>%
  bind_cols(new_dat_l2) %>%
  rename(se = se.fit) %>%
  mutate(ymin = fit - se, ymax = fit + se,
         l1 = fct_recode(l1, EN = "en", MA = "ma"),
         l1 = fct_relevel(l1, "EN", "MA"))

fits_all_l2_use <- predictSE(gca_l2_mod_use_final, new_dat_l2) %>%  
  as_tibble %>%
  bind_cols(new_dat_l2) %>%
  rename(se = se.fit) %>%
  mutate(ymin = fit - se, ymax = fit + se,
         l1 = fct_recode(l1, EN = "en", MA = "ma"),
         l1 = fct_relevel(l1, "EN", "MA"))

fits_all_l2_wm <- predictSE(gca_l2_mod_wm_final, new_dat_l2) %>%  
  as_tibble %>%
  bind_cols(new_dat_l2) %>%
  rename(se = se.fit) %>%
  mutate(ymin = fit - se, ymax = fit + se,
         group = fct_recode(l1, EN = "en", MA = "ma"),
         group = fct_relevel(l1, "EN", "MA"))

# Filter preds at target syllable offset
target_offset_preds_mon <- filter(fits_all_mon, time_zero == 4) %>%
  select(stress = condition_sum, #ospan,
         elog = fit, elog_lb = ymin, elog_ub = ymax) %>%
  mutate(prob = plogis(elog),
         prob_lb = plogis(elog_lb),
         prob_ub = plogis(elog_ub)) 

target_offset_preds_l2_dele <- filter(fits_all_l2_dele, time_zero == 4) %>%
  select(l1, stress = condition_sum, DELE = DELE_z,
         elog = fit, elog_lb = ymin, elog_ub = ymax) %>%
  mutate(prob = plogis(elog),
         prob_lb = plogis(elog_lb),
         prob_ub = plogis(elog_ub)) %>%
  arrange(l1)

target_offset_preds_l2_use <- filter(fits_all_l2_use, time_zero == 4) %>%
  select(l1, stress = condition_sum, percent_l2_week = use_z,
         elog = fit, elog_lb = ymin, elog_ub = ymax) %>%
  mutate(prob = plogis(elog),
         prob_lb = plogis(elog_lb),
         prob_ub = plogis(elog_ub)) %>%
  arrange(l1)

target_offset_preds_l2_wm <- filter(fits_all_l2_wm, time_zero == 4) %>%
  select(l1, stress = condition_sum, wm = ospan,
         elog = fit, elog_lb = ymin, elog_ub = ymax) %>%
  mutate(prob = plogis(elog),
         prob_lb = plogis(elog_lb),
         prob_ub = plogis(elog_ub)) %>%
  arrange(l1)

# Save models predictions
model_preds <- mget(c("fits_all_mon", "fits_all_l2_dele", "fits_all_l2_use", "fits_all_l2_wm",
                      "target_offset_preds_mon", "target_offset_preds_l2_dele",
                      "target_offset_preds_l2_use", "target_offset_preds_l2_wm"))

save(model_preds,
     file = here("mods", "stress", "gca",
                 "model_preds.Rdata"))

# MON without WM score
saveRDS(target_offset_preds_mon, file = here("mods", "stress", "gca", "model_preds_mon.Rdata"))

# -----------------------------------------------------------------------------


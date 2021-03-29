# Time course plots -----------------------------------------------------------
#
# - This script plots the raw eye tracking time course data and the
#   model estimates from a growth curve analysis
#
# -----------------------------------------------------------------------------



# Source scripts and load models ----------------------------------------------

source(here::here("scripts", "00_load_libs.R"))
det_50 <- read_csv(here::here("data", "det_50_gca_target_onset.csv"))

# Get path to saved models 
gca_mods_path <- here("mods", "gca") 

# Load models as list and store full mod to global env
load(paste0(gca_mods_path, "/gca_mods.Rdata"))
load(paste0(gca_mods_path, "/model_preds.Rdata"))
list2env(gca_mods, globalenv())
list2env(model_preds, globalenv())

# Set path for saving figs
figs_path <- here("figs", "gca")

# -----------------------------------------------------------------------------






# Plot raw data ---------------------------------------------------------------

transparency_names <- c(
  `opaq` = 'Opaque nouns',
  `transp` = 'Transparent nouns'
)

group_names <- c(
  `hs` = 'HS',
  `l2` = 'L2',
  `ss` = 'SS'
)


det_gender <- det_50 %>%         
    filter(., time_zero >= -10, time_zero <= 20) %>%
    mutate(., noun_transparency = fct_relevel(noun_transparency, "transp", "opaq"),
           gender = fct_relevel(gender, "masc", "feme")) %>%
    ggplot(., aes(x = time_zero, y = target_prop, fill = gender)) +
    facet_grid(. ~ noun_transparency, labeller = as_labeller(transparency_names)) +  
    geom_hline(yintercept = 0.5, color = 'white', size = 3) +
    geom_vline(xintercept = 0, color = 'grey40', lty = 3) +
    geom_vline(xintercept = 4, color = 'grey40', lty = 3) +
    stat_summary(fun.y = "mean", geom = "line", size = 1) +  
    stat_summary(fun.data = mean_cl_boot, geom = 'pointrange', size = 0.5,
                 stroke = 0.5, pch = 21) +
    scale_fill_brewer(palette = 'Set1', name = "Gender", 
                       labels = c("masculine", "feminine")) +
    # scale_x_continuous(breaks = c(-10, 0, 10, 20),
    #                    labels = c("-500", "0", "500", "1000")) +
    labs(y = 'Proportion of target fixations',
         x = 'Time relative to target syllable offset (ms)',
         caption = "Mean +/- 95% CI") +
    annotate("text", x = 3.3, y = 0.02, label = '200ms',
             angle = 90, size = 3, hjust = 0) +
    ggtitle('Time course per noun gender according to noun type') +
    theme_grey(base_size = 12, base_family = "Times")

det_transparency <- det_50 %>%          
  filter(., time_zero >= -10, time_zero <= 20) %>%
  mutate(., noun_transparency = fct_relevel(noun_transparency, "transp", "opaq"),
         gender = fct_relevel(gender, "masc", "feme")) %>%
  ggplot(., aes(x = time_zero, y = target_prop, fill = noun_transparency)) +
  facet_grid(. ~ structure) +
  geom_hline(yintercept = 0.5, color = 'white', size = 3) +
  geom_vline(xintercept = 0, color = 'grey40', lty = 3) +
  geom_vline(xintercept = 4, color = 'grey40', lty = 3) +
  stat_summary(fun.y = "mean", geom = "line", size = 1) +  
  stat_summary(fun.data = mean_cl_boot, geom = 'pointrange', size = 0.5,
               stroke = 0.5, pch = 21) +
  scale_fill_brewer(palette = 'Set1', name = "Noun transparency",
                    labels = c('transparent', 'opaque')) +
  labs(y = 'Proportion of target fixations',
       x = 'Time relative to target syllable offset (ms)',
       caption = "Mean +/- 95% CI") +
  annotate("text", x = 3.3, y = 0.02, label = '200ms',
           angle = 90, size = 3, hjust = 0) +
  ggtitle('Time course per noun transparency according to phrase structure') +
  theme_grey(base_size = 12, base_family = "Times")

det_structure <- det_50 %>%          
  filter(., time_zero >= -10, time_zero <= 20) %>%
  mutate(., noun_transparency = fct_relevel(noun_transparency, "transp", "opaq"),
         gender = fct_relevel(gender, "masc", "feme"),
         group = fct_relevel(group, 'ss', 'hs', 'l2')) %>%
  ggplot(., aes(x = time_zero, y = target_prop, fill = structure)) +
  facet_grid(. ~ group, labeller = as_labeller(group_names)) +
  geom_hline(yintercept = 0.5, color = 'white', size = 3) +
  geom_vline(xintercept = 0, color = 'grey40', lty = 3) +
  geom_vline(xintercept = 4, color = 'grey40', lty = 3) +
  stat_summary(fun.y = "mean", geom = "line", size = 1) +  
  stat_summary(fun.data = mean_cl_boot, geom = 'pointrange', size = 0.5,
               stroke = 0.5, pch = 21) +
  scale_fill_brewer(palette = 'Set1', name = "Phrase structure",
                    labels = c('Gendered determiner', 'Ungendered determiner')) +
  labs(y = 'Proportion of target fixations',
       x = 'Time relative to target syllable offset (ms)',
       caption = "Mean +/- 95% CI") +
  annotate("text", x = 3.3, y = 0.02, label = '200ms',
           angle = 90, size = 3, hjust = 0) +
  ggtitle('Time course per phrase structure according to AoA (0 vs. L2') +
  theme_grey(base_size = 12, base_family = "Times")

# det_50 %>%          
#   filter(., time_zero >= -10, time_zero <= 20) %>%
#   ggplot(., aes(x = time_zero, y = target_prop, fill = structure)) +
#   geom_hline(yintercept = 0.5, color = 'white', size = 3) +
#   geom_vline(xintercept = 0, color = 'grey40', lty = 3) +
#   geom_vline(xintercept = 4, color = 'grey40', lty = 3) +
#   stat_summary(fun.y = "mean", geom = "line", size = 1) +  
#   stat_summary(fun.data = mean_cl_boot, geom = 'pointrange', size = 0.5,
#                stroke = 0.5, pch = 21)
#

det_group <- det_50 %>%
  mutate(AoA = if_else(group != 'l2', 'native', "l2")) %>%
  filter(., time_zero >= -10, time_zero <= 20) %>%
  ggplot(., aes(x = time_zero, y = target_prop, fill = AoA)) +
  geom_hline(yintercept = 0.5, color = 'white', size = 3) +
  geom_vline(xintercept = 0, color = 'grey40', lty = 3) +
  geom_vline(xintercept = 4, color = 'grey40', lty = 3) +
  stat_summary(fun.y = "mean", geom = "line", size = 1) +
  stat_summary(fun.data = mean_cl_boot, geom = 'pointrange', size = 0.5,
               stroke = 0.5, pch = 21)


# does not show gradient of continuous fill variable
# det_50 %>%          
#   filter(., time_zero >= -10, time_zero <= 20) %>%
#   ggplot(., aes(x = time_zero, y = target_prop, fill = prof_z)) + # test wm
#   geom_hline(yintercept = 0.5, color = 'white', size = 3) +
#   geom_vline(xintercept = 0, color = 'grey40', lty = 3) +
#   geom_vline(xintercept = 4, color = 'grey40', lty = 3) +
#   stat_summary(fun.y = "mean", geom = "line", size = 1) +  
#   stat_summary(fun.data = mean_cl_boot, geom = 'pointrange', size = 0.5,
#                stroke = 0.5, pch = 21) +
#   geom_point(aes(color = prof_z), alpha = .5, pch = 21, size = 0.85, show.legend = T)
#  


ggsave('det_gender.png',
       plot = det_gender, dpi = 600, device = "png",
       path = figs_path,
       height = 3.5, width = 8.5, units = 'in')
ggsave('det_transparency.png',
       plot = det_transparency, dpi = 600, device = "png",
       path = figs_path,
       height = 3.5, width = 8.5, units = 'in')
ggsave('det_group.png',
       plot = det_group, dpi = 600, device = "png",
       path = figs_path,
       height = 3.5, width = 8.5, units = 'in')
ggsave('det_structure.png',
       plot = det_structure, dpi = 600, device = "png",
       path = figs_path,
       height = 3.5, width = 8.5, units = 'in')



# -----------------------------------------------------------------------------




# Plot GCA --------------------------------------------------------------------

# Transparency + wm EN
  
transEN_AoA <- model_preds$fits_all_transEN %>%
    mutate(noun_transp = if_else(noun_sum == 1, "transparent", "opaque"),
           noun_transp = fct_relevel(noun_transp, 'transparent'),
           gender = if_else(gender_sum == 1, "masculine", "feminine"),
           gender = fct_relevel(gender, 'masculine'),
           AoA = if_else(AoA_sum == 1, 'Birth', 'L2'),
           AoA = fct_relevel(AoA, 'Birth')) %>%
    ggplot(., aes(x = time_zero, y = fit, ymax = ymax, ymin = ymin,
                  fill = AoA)) + #, color = condition
    geom_hline(yintercept = 0, lty = 3, size = 0.4) +
    geom_vline(xintercept = 4, lty = 3, size = 0.4) +
    stat_summary(fun.y = "mean", geom = "line", size = 1) + 
    # geom_ribbon(alpha = 0.2, color = "grey", show.legend = F) +
    stat_summary(fun.data = mean_cl_boot, geom = 'ribbon',fun.args=list(conf.int=0.95),
                 alpha = 0.5) +
    geom_point(aes(color = AoA), size = 0.8, show.legend = F) + # , alpha = .1, pch = 5
    scale_x_continuous(breaks = c(-4, 0, 4, 8, 12),
                     labels = c("-200", "0", "200", "400", "600")) +
    labs(x = "Time (ms) relative to target syllable offset",
         y = "Empirical logit of looks to target") +
    theme_big + legend_adj + labs(fill = "AoA")


transEN_prof <- model_preds$fits_all_transEN %>%
  mutate(noun_transp = if_else(noun_sum == 1, "transparent", "opaque"),
         noun_transp = fct_relevel(noun_transp, 'transparent'),
         gender = if_else(gender_sum == 1, "masculine", "feminine"),
         gender = fct_relevel(gender, 'masculine'),
         AoA = if_else(AoA_sum == 1, 'Birth', 'L2'),
         AoA = fct_relevel(AoA, 'Birth')) %>%
  ggplot(., aes(x = time_zero, y = fit, ymax = ymax, ymin = ymin,
                fill = prof_z)) + #, color = condition
  geom_hline(yintercept = 0, lty = 3, size = 0.4) +
  geom_vline(xintercept = 4, lty = 3, size = 0.4) +
  stat_summary(fun.y = "mean", geom = "line", size = 1) + 
  # geom_ribbon(alpha = 0.2, color = "grey", show.legend = F) +
  stat_summary(fun.data = mean_cl_boot, geom = 'ribbon',fun.args=list(conf.int=0.95),
               alpha = 0.5) +
  geom_point(aes(color = prof_z), size = 0.8, show.legend = F) + #, alpha = .1, pch = 5
  scale_x_continuous(breaks = c(-4, 0, 4, 8, 12),
                     labels = c("-200", "0", "200", "400", "600")) +
  labs(x = "Time (ms) relative to target syllable offset",
       y = "Empirical logit of looks to target") +
  theme_big + legend_adj + labs(fill = "Proficiency") +
  theme(legend.position = c(0.08, 0.8))


transEN_transparency <- model_preds$fits_all_transEN %>%
  mutate(noun_transp = if_else(noun_sum == 1, "transparent", "opaque"),
         noun_transp = fct_relevel(noun_transp, 'transparent'),
         gender = if_else(gender_sum == 1, "masculine", "feminine"),
         gender = fct_relevel(gender, 'masculine')) %>%
  ggplot(., aes(x = time_zero, y = fit, ymax = ymax, ymin = ymin,
                fill = noun_transp)) + #, color = condition
  geom_hline(yintercept = 0, lty = 3, size = 0.4) +
  geom_vline(xintercept = 4, lty = 3, size = 0.4) +
  stat_summary(fun.y = "mean", geom = "line", size = 1) + 
  # geom_ribbon(alpha = 0.2, color = "grey", show.legend = F) +
  stat_summary(fun.data = mean_cl_boot, geom = 'ribbon',fun.args=list(conf.int=0.95),
               alpha = 0.5) +
  geom_point(aes(color = noun_transp), size = 0.8, show.legend = F) +
  scale_x_continuous(breaks = c(-4, 0, 4, 8, 12),
                     labels = c("-200", "0", "200", "400", "600")) +
  labs(x = "Time (ms) relative to target syllable offset",
       y = "Empirical logit of looks to target") +
  theme_big + legend_adj + labs(fill = "Noun transparency")


transEN_gender <- model_preds$fits_all_transEN %>%
  mutate(noun_transp = if_else(noun_sum == 1, "transparent", "opaque"),
         noun_transp = fct_relevel(noun_transp, 'transparent'),
         gender = if_else(gender_sum == 1, "masculine", "feminine"),
         gender = fct_relevel(gender, 'masculine')) %>%
  ggplot(., aes(x = time_zero, y = fit, ymax = ymax, ymin = ymin,
                fill = gender)) + #, color = condition
  geom_hline(yintercept = 0, lty = 3, size = 0.4) +
  geom_vline(xintercept = 4, lty = 3, size = 0.4) +
  stat_summary(fun.y = "mean", geom = "line", size = 1) + 
  # geom_ribbon(alpha = 0.2, color = "grey", show.legend = F) +
  stat_summary(fun.data = mean_cl_boot, geom = 'ribbon',fun.args=list(conf.int=0.95),
               alpha = 0.5) +
  # geom_smooth(aes(color = gender))
  geom_point(aes(color = gender), size = 0.8, show.legend = F) +
  #scale_fill_brewer(palette = 'Set2', name = "Gender") +
  scale_x_continuous(breaks = c(-4, 0, 4, 8, 12),
                     labels = c("-200", "0", "200", "400", "600")) +
  labs(x = "Time (ms) relative to target syllable offset",
       y = "Empirical logit of looks to target") +
  theme_big + legend_adj + labs(fill = "Gender")


transEN_wm <- model_preds$fits_all_transEN %>%
  mutate(noun_transp = if_else(noun_sum == 1, "transparent", "opaque"),
         noun_transp = fct_relevel(noun_transp, 'transparent'),
         gender = if_else(gender_sum == 1, "masculine", "feminine"),
         gender = fct_relevel(gender, 'masculine'),
         AoA = if_else(AoA_sum == 1, 'Birth', 'L2'),
         AoA = fct_relevel(AoA, 'Birth')) %>%
  ggplot(., aes(x = time_zero, y = fit, ymax = ymax, ymin = ymin,
                fill = wmen_z)) + 
  geom_hline(yintercept = 0, lty = 3, size = 0.4) +
  geom_vline(xintercept = 4, lty = 3, size = 0.4) +
  stat_summary(fun.y = "mean", geom = "line", size = 1) + 
  # geom_ribbon(alpha = 0.2, color = "grey", show.legend = F) +
  stat_summary(fun.data = mean_cl_boot, geom = 'ribbon',fun.args=list(conf.int=0.95),
               alpha = 0.5) +
  geom_point(aes(color = wmen_z), size = 0.8, show.legend = F) + #, alpha = .1, pch = 5
  scale_x_continuous(breaks = c(-4, 0, 4, 8, 12),
                     labels = c("-200", "0", "200", "400", "600")) +
  labs(x = "Time (ms) relative to target syllable offset",
       y = "Empirical logit of looks to target") +
  theme_big + legend_adj + labs(fill = "WM (HS EN)") +
  theme(legend.position = c(0.08, 0.8))




# Transparency + wm ES

transES_AoA <- model_preds$fits_all_transES %>%
  mutate(noun_transp = if_else(noun_sum == 1, "transparent", "opaque"),
         noun_transp = fct_relevel(noun_transp, 'transparent'),
         gender = if_else(gender_sum == 1, "masculine", "feminine"),
         gender = fct_relevel(gender, 'masculine'),
         AoA = if_else(AoA_sum == 1, 'Birth', 'L2'),
         AoA = fct_relevel(AoA, 'Birth')) %>%
  ggplot(., aes(x = time_zero, y = fit, ymax = ymax, ymin = ymin,
                fill = AoA)) + #, color = condition
  geom_hline(yintercept = 0, lty = 3, size = 0.4) +
  geom_vline(xintercept = 4, lty = 3, size = 0.4) +
  stat_summary(fun.y = "mean", geom = "line", size = 1) + 
  # geom_ribbon(alpha = 0.2, color = "grey", show.legend = F) +
  stat_summary(fun.data = mean_cl_boot, geom = 'ribbon',fun.args=list(conf.int=0.95),
               alpha = 0.5) +
  geom_point(aes(color = AoA), size = 0.8, show.legend = F) + # , alpha = .1, pch = 5
  scale_x_continuous(breaks = c(-4, 0, 4, 8, 12),
                     labels = c("-200", "0", "200", "400", "600")) +
  labs(x = "Time (ms) relative to target syllable offset",
       y = "Empirical logit of looks to target") +
  theme_big + legend_adj + labs(fill = "AoA")


transES_prof <- model_preds$fits_all_transES %>%
  mutate(noun_transp = if_else(noun_sum == 1, "transparent", "opaque"),
         noun_transp = fct_relevel(noun_transp, 'transparent'),
         gender = if_else(gender_sum == 1, "masculine", "feminine"),
         gender = fct_relevel(gender, 'masculine'),
         AoA = if_else(AoA_sum == 1, 'Birth', 'L2'),
         AoA = fct_relevel(AoA, 'Birth')) %>%
  ggplot(., aes(x = time_zero, y = fit, ymax = ymax, ymin = ymin,
                fill = prof_z)) + #, color = condition
  geom_hline(yintercept = 0, lty = 3, size = 0.4) +
  geom_vline(xintercept = 4, lty = 3, size = 0.4) +
  stat_summary(fun.y = "mean", geom = "line", size = 1) + 
  # geom_ribbon(alpha = 0.2, color = "grey", show.legend = F) +
  stat_summary(fun.data = mean_cl_boot, geom = 'ribbon',fun.args=list(conf.int=0.95),
               alpha = 0.5) +
  geom_point(aes(color = prof_z), size = 0.8, show.legend = F) + #, alpha = .1, pch = 5
  scale_x_continuous(breaks = c(-4, 0, 4, 8, 12),
                     labels = c("-200", "0", "200", "400", "600")) +
  labs(x = "Time (ms) relative to target syllable offset",
       y = "Empirical logit of looks to target") +
  theme_big + legend_adj + labs(fill = "Proficiency") +
  theme(legend.position = c(0.08, 0.8))


transES_transparency <- model_preds$fits_all_transES %>%
  mutate(noun_transp = if_else(noun_sum == 1, "transparent", "opaque"),
         noun_transp = fct_relevel(noun_transp, 'transparent'),
         gender = if_else(gender_sum == 1, "masculine", "feminine"),
         gender = fct_relevel(gender, 'masculine')) %>%
  ggplot(., aes(x = time_zero, y = fit, ymax = ymax, ymin = ymin,
                fill = noun_transp)) + #, color = condition
  geom_hline(yintercept = 0, lty = 3, size = 0.4) +
  geom_vline(xintercept = 4, lty = 3, size = 0.4) +
  stat_summary(fun.y = "mean", geom = "line", size = 1) + 
  # geom_ribbon(alpha = 0.2, color = "grey", show.legend = F) +
  stat_summary(fun.data = mean_cl_boot, geom = 'ribbon',fun.args=list(conf.int=0.95),
               alpha = 0.5) +
  geom_point(aes(color = noun_transp), size = 0.8, show.legend = F) +
  scale_x_continuous(breaks = c(-4, 0, 4, 8, 12),
                     labels = c("-200", "0", "200", "400", "600")) +
  labs(x = "Time (ms) relative to target syllable offset",
       y = "Empirical logit of looks to target") +
  theme_big + legend_adj + labs(fill = "Noun transparency")


transES_gender <- model_preds$fits_all_transES %>%
  mutate(noun_transp = if_else(noun_sum == 1, "transparent", "opaque"),
         noun_transp = fct_relevel(noun_transp, 'transparent'),
         gender = if_else(gender_sum == 1, "masculine", "feminine"),
         gender = fct_relevel(gender, 'masculine')) %>%
  ggplot(., aes(x = time_zero, y = fit, ymax = ymax, ymin = ymin,
                fill = gender)) + #, color = condition
  geom_hline(yintercept = 0, lty = 3, size = 0.4) +
  geom_vline(xintercept = 4, lty = 3, size = 0.4) +
  stat_summary(fun.y = "mean", geom = "line", size = 1) + 
  # geom_ribbon(alpha = 0.2, color = "grey", show.legend = F) +
  stat_summary(fun.data = mean_cl_boot, geom = 'ribbon',fun.args=list(conf.int=0.95),
               alpha = 0.5) +
  # geom_smooth(aes(color = gender))
  geom_point(aes(color = gender), size = 0.8, show.legend = F) +
  #scale_fill_brewer(palette = 'Set2', name = "Gender") +
  scale_x_continuous(breaks = c(-4, 0, 4, 8, 12),
                     labels = c("-200", "0", "200", "400", "600")) +
  labs(x = "Time (ms) relative to target syllable offset",
       y = "Empirical logit of looks to target") +
  theme_big + legend_adj + labs(fill = "Gender")


transES_wm <- model_preds$fits_all_transES %>%
  mutate(noun_transp = if_else(noun_sum == 1, "transparent", "opaque"),
         noun_transp = fct_relevel(noun_transp, 'transparent'),
         gender = if_else(gender_sum == 1, "masculine", "feminine"),
         gender = fct_relevel(gender, 'masculine'),
         AoA = if_else(AoA_sum == 1, 'Birth', 'L2'),
         AoA = fct_relevel(AoA, 'Birth')) %>%
  ggplot(., aes(x = time_zero, y = fit, ymax = ymax, ymin = ymin,
                fill = wmes_z)) + 
  geom_hline(yintercept = 0, lty = 3, size = 0.4) +
  geom_vline(xintercept = 4, lty = 3, size = 0.4) +
  stat_summary(fun.y = "mean", geom = "line", size = 1) + 
  # geom_ribbon(alpha = 0.2, color = "grey", show.legend = F) +
  stat_summary(fun.data = mean_cl_boot, geom = 'ribbon',fun.args=list(conf.int=0.95),
               alpha = 0.5) +
  geom_point(aes(color = wmes_z), size = 0.8, show.legend = F) + #, alpha = .1, pch = 5
  scale_x_continuous(breaks = c(-4, 0, 4, 8, 12),
                     labels = c("-200", "0", "200", "400", "600")) +
  labs(x = "Time (ms) relative to target syllable offset",
       y = "Empirical logit of looks to target") +
  theme_big + legend_adj + labs(fill = "WM (HS ES)") +
  theme(legend.position = c(0.08, 0.8))




# Phrase structure + wm EN

strucEN_AoA <- model_preds$fits_all_strucEN %>%
  mutate(structure = if_else(structure_sum == 1, "d_n_adj", "n_adj"),
         structure = fct_relevel(structure, 'd_n_adj'),
         gender = if_else(gender_sum == 1, "masculine", "feminine"),
         gender = fct_relevel(gender, 'masculine'),
         AoA = if_else(AoA_sum == 1, 'Birth', 'L2'),
         AoA = fct_relevel(AoA, 'Birth')) %>%
  ggplot(., aes(x = time_zero, y = fit, ymax = ymax, ymin = ymin,
                fill = AoA)) + #, color = condition
  geom_hline(yintercept = 0, lty = 3, size = 0.4) +
  geom_vline(xintercept = 4, lty = 3, size = 0.4) +
  stat_summary(fun.y = "mean", geom = "line", size = 1) + 
  # geom_ribbon(alpha = 0.2, color = "grey", show.legend = F) +
  stat_summary(fun.data = mean_cl_boot, geom = 'ribbon',fun.args=list(conf.int=0.95),
               alpha = 0.5) +
  geom_point(aes(color = AoA), size = 0.8, show.legend = F) + # , alpha = .1, pch = 5
  scale_x_continuous(breaks = c(-4, 0, 4, 8, 12),
                     labels = c("-200", "0", "200", "400", "600")) +
  labs(x = "Time (ms) relative to target syllable offset",
       y = "Empirical logit of looks to target") +
  theme_big + legend_adj + labs(fill = "AoA")


strucEN_prof <- model_preds$fits_all_strucEN %>%
  mutate(structure = if_else(structure_sum == 1, "d_n_adj", "n_adj"),
         structure = fct_relevel(structure, 'd_n_adj'),
         gender = if_else(gender_sum == 1, "masculine", "feminine"),
         gender = fct_relevel(gender, 'masculine'),
         AoA = if_else(AoA_sum == 1, 'Birth', 'L2'),
         AoA = fct_relevel(AoA, 'Birth')) %>%
  ggplot(., aes(x = time_zero, y = fit, ymax = ymax, ymin = ymin,
                fill = prof_z)) + #, color = condition
  geom_hline(yintercept = 0, lty = 3, size = 0.4) +
  geom_vline(xintercept = 4, lty = 3, size = 0.4) +
  stat_summary(fun.y = "mean", geom = "line", size = 1) + 
  # geom_ribbon(alpha = 0.2, color = "grey", show.legend = F) +
  stat_summary(fun.data = mean_cl_boot, geom = 'ribbon',fun.args=list(conf.int=0.95),
               alpha = 0.5) +
  geom_point(aes(color = prof_z), size = 0.8, show.legend = F) + #, alpha = .1, pch = 5
  scale_x_continuous(breaks = c(-4, 0, 4, 8, 12),
                     labels = c("-200", "0", "200", "400", "600")) +
  labs(x = "Time (ms) relative to target syllable offset",
       y = "Empirical logit of looks to target") +
  theme_big + legend_adj + labs(fill = "Proficiency") +
  theme(legend.position = c(0.08, 0.8))


strucEN_structure <- model_preds$fits_all_strucEN %>%
  mutate(structure = if_else(structure_sum == 1, "d_n_adj", "n_adj"),
         structure = fct_relevel(structure, 'd_n_adj'),
         gender = if_else(gender_sum == 1, "masculine", "feminine"),
         gender = fct_relevel(gender, 'masculine')) %>%
  ggplot(., aes(x = time_zero, y = fit, ymax = ymax, ymin = ymin,
                fill = structure)) + #, color = condition
  geom_hline(yintercept = 0, lty = 3, size = 0.4) +
  geom_vline(xintercept = 4, lty = 3, size = 0.4) +
  stat_summary(fun.y = "mean", geom = "line", size = 1) + 
  # geom_ribbon(alpha = 0.2, color = "grey", show.legend = F) +
  stat_summary(fun.data = mean_cl_boot, geom = 'ribbon',fun.args=list(conf.int=0.95),
               alpha = 0.5) +
  geom_point(aes(color = structure), size = 0.8, show.legend = F) +
  scale_x_continuous(breaks = c(-4, 0, 4, 8, 12),
                     labels = c("-200", "0", "200", "400", "600")) +
  labs(x = "Time (ms) relative to target syllable offset",
       y = "Empirical logit of looks to target") +
  theme_big + legend_adj + labs(fill = "Phrase structure")


strucEN_gender <- model_preds$fits_all_strucEN %>%
  mutate(structure = if_else(structure_sum == 1, "d_n_adj", "n_adj"),
         structure = fct_relevel(structure, 'd_n_adj'),
         gender = if_else(gender_sum == 1, "masculine", "feminine"),
         gender = fct_relevel(gender, 'masculine')) %>%
  ggplot(., aes(x = time_zero, y = fit, ymax = ymax, ymin = ymin,
                fill = gender)) + #, color = condition
  geom_hline(yintercept = 0, lty = 3, size = 0.4) +
  geom_vline(xintercept = 4, lty = 3, size = 0.4) +
  stat_summary(fun.y = "mean", geom = "line", size = 1) + 
  # geom_ribbon(alpha = 0.2, color = "grey", show.legend = F) +
  stat_summary(fun.data = mean_cl_boot, geom = 'ribbon',fun.args=list(conf.int=0.95),
               alpha = 0.5) +
  # geom_smooth(aes(color = gender))
  geom_point(aes(color = gender), size = 0.8, show.legend = F) +
  #scale_fill_brewer(palette = 'Set2', name = "Gender") +
  scale_x_continuous(breaks = c(-4, 0, 4, 8, 12),
                     labels = c("-200", "0", "200", "400", "600")) +
  labs(x = "Time (ms) relative to target syllable offset",
       y = "Empirical logit of looks to target") +
  theme_big + legend_adj + labs(fill = "Gender")


strucEN_wm <- model_preds$fits_all_strucEN %>%
  mutate(structure = if_else(structure_sum == 1, "d_n_adj", "n_adj"),
         structure = fct_relevel(structure, 'd_n_adj'),
         gender = if_else(gender_sum == 1, "masculine", "feminine"),
         gender = fct_relevel(gender, 'masculine'),
         AoA = if_else(AoA_sum == 1, 'Birth', 'L2'),
         AoA = fct_relevel(AoA, 'Birth')) %>%
  ggplot(., aes(x = time_zero, y = fit, ymax = ymax, ymin = ymin,
                fill = wmen_z)) + 
  geom_hline(yintercept = 0, lty = 3, size = 0.4) +
  geom_vline(xintercept = 4, lty = 3, size = 0.4) +
  stat_summary(fun.y = "mean", geom = "line", size = 1) + 
  # geom_ribbon(alpha = 0.2, color = "grey", show.legend = F) +
  stat_summary(fun.data = mean_cl_boot, geom = 'ribbon',fun.args=list(conf.int=0.95),
               alpha = 0.5) +
  geom_point(aes(color = wmen_z), size = 0.8, show.legend = F) + #, alpha = .1, pch = 5
  scale_x_continuous(breaks = c(-4, 0, 4, 8, 12),
                     labels = c("-200", "0", "200", "400", "600")) +
  labs(x = "Time (ms) relative to target syllable offset",
       y = "Empirical logit of looks to target") +
  theme_big + legend_adj + labs(fill = "WM (HS EN)") +
  theme(legend.position = c(0.08, 0.8))


# Phrase structure + wm ES

strucES_AoA <- model_preds$fits_all_strucES %>%
  mutate(structure = if_else(structure_sum == 1, "d_n_adj", "n_adj"),
         structure = fct_relevel(structure, 'd_n_adj'),
         gender = if_else(gender_sum == 1, "masculine", "feminine"),
         gender = fct_relevel(gender, 'masculine'),
         AoA = if_else(AoA_sum == 1, 'Birth', 'L2'),
         AoA = fct_relevel(AoA, 'Birth')) %>%
  ggplot(., aes(x = time_zero, y = fit, ymax = ymax, ymin = ymin,
                fill = AoA)) + #, color = condition
  geom_hline(yintercept = 0, lty = 3, size = 0.4) +
  geom_vline(xintercept = 4, lty = 3, size = 0.4) +
  stat_summary(fun.y = "mean", geom = "line", size = 1) + 
  # geom_ribbon(alpha = 0.2, color = "grey", show.legend = F) +
  stat_summary(fun.data = mean_cl_boot, geom = 'ribbon',fun.args=list(conf.int=0.95),
               alpha = 0.5) +
  geom_point(aes(color = AoA), size = 0.8, show.legend = F) + # , alpha = .1, pch = 5
  scale_x_continuous(breaks = c(-4, 0, 4, 8, 12),
                     labels = c("-200", "0", "200", "400", "600")) +
  labs(x = "Time (ms) relative to target syllable offset",
       y = "Empirical logit of looks to target") +
  theme_big + legend_adj + labs(fill = "AoA")


strucES_prof <- model_preds$fits_all_strucES %>%
  mutate(structure = if_else(structure_sum == 1, "d_n_adj", "n_adj"),
         structure = fct_relevel(structure, 'd_n_adj'),
         gender = if_else(gender_sum == 1, "masculine", "feminine"),
         gender = fct_relevel(gender, 'masculine'),
         AoA = if_else(AoA_sum == 1, 'Birth', 'L2'),
         AoA = fct_relevel(AoA, 'Birth')) %>%
  ggplot(., aes(x = time_zero, y = fit, ymax = ymax, ymin = ymin,
                fill = prof_z)) + #, color = condition
  geom_hline(yintercept = 0, lty = 3, size = 0.4) +
  geom_vline(xintercept = 4, lty = 3, size = 0.4) +
  stat_summary(fun.y = "mean", geom = "line", size = 1) + 
  # geom_ribbon(alpha = 0.2, color = "grey", show.legend = F) +
  stat_summary(fun.data = mean_cl_boot, geom = 'ribbon',fun.args=list(conf.int=0.95),
               alpha = 0.5) +
  geom_point(aes(color = prof_z), size = 0.8, show.legend = F) + #, alpha = .1, pch = 5
  scale_x_continuous(breaks = c(-4, 0, 4, 8, 12),
                     labels = c("-200", "0", "200", "400", "600")) +
  labs(x = "Time (ms) relative to target syllable offset",
       y = "Empirical logit of looks to target") +
  theme_big + legend_adj + labs(fill = "Proficiency") +
  theme(legend.position = c(0.08, 0.8))


strucES_structure <- model_preds$fits_all_strucES %>%
  mutate(structure = if_else(structure_sum == 1, "d_n_adj", "n_adj"),
         structure = fct_relevel(structure, 'd_n_adj'),
         gender = if_else(gender_sum == 1, "masculine", "feminine"),
         gender = fct_relevel(gender, 'masculine')) %>%
  ggplot(., aes(x = time_zero, y = fit, ymax = ymax, ymin = ymin,
                fill = structure)) + #, color = condition
  geom_hline(yintercept = 0, lty = 3, size = 0.4) +
  geom_vline(xintercept = 4, lty = 3, size = 0.4) +
  stat_summary(fun.y = "mean", geom = "line", size = 1) + 
  # geom_ribbon(alpha = 0.2, color = "grey", show.legend = F) +
  stat_summary(fun.data = mean_cl_boot, geom = 'ribbon',fun.args=list(conf.int=0.95),
               alpha = 0.5) +
  geom_point(aes(color = structure), size = 0.8, show.legend = F) +
  scale_x_continuous(breaks = c(-4, 0, 4, 8, 12),
                     labels = c("-200", "0", "200", "400", "600")) +
  labs(x = "Time (ms) relative to target syllable offset",
       y = "Empirical logit of looks to target") +
  theme_big + legend_adj + labs(fill = "Phrase structure")


strucES_gender <- model_preds$fits_all_strucES %>%
  mutate(structure = if_else(structure_sum == 1, "d_n_adj", "n_adj"),
         structure = fct_relevel(structure, 'd_n_adj'),
         gender = if_else(gender_sum == 1, "masculine", "feminine"),
         gender = fct_relevel(gender, 'masculine')) %>%
  ggplot(., aes(x = time_zero, y = fit, ymax = ymax, ymin = ymin,
                fill = gender)) + #, color = condition
  geom_hline(yintercept = 0, lty = 3, size = 0.4) +
  geom_vline(xintercept = 4, lty = 3, size = 0.4) +
  stat_summary(fun.y = "mean", geom = "line", size = 1) + 
  # geom_ribbon(alpha = 0.2, color = "grey", show.legend = F) +
  stat_summary(fun.data = mean_cl_boot, geom = 'ribbon',fun.args=list(conf.int=0.95),
               alpha = 0.5) +
  # geom_smooth(aes(color = gender))
  geom_point(aes(color = gender), size = 0.8, show.legend = F) +
  #scale_fill_brewer(palette = 'Set2', name = "Gender") +
  scale_x_continuous(breaks = c(-4, 0, 4, 8, 12),
                     labels = c("-200", "0", "200", "400", "600")) +
  labs(x = "Time (ms) relative to target syllable offset",
       y = "Empirical logit of looks to target") +
  theme_big + legend_adj + labs(fill = "Gender")


strucES_wm <- model_preds$fits_all_strucES %>%
  mutate(structure = if_else(structure_sum == 1, "d_n_adj", "n_adj"),
         structure = fct_relevel(structure, 'd_n_adj'),
         gender = if_else(gender_sum == 1, "masculine", "feminine"),
         gender = fct_relevel(gender, 'masculine'),
         AoA = if_else(AoA_sum == 1, 'Birth', 'L2'),
         AoA = fct_relevel(AoA, 'Birth')) %>%
  ggplot(., aes(x = time_zero, y = fit, ymax = ymax, ymin = ymin,
                fill = wmes_z)) + 
  geom_hline(yintercept = 0, lty = 3, size = 0.4) +
  geom_vline(xintercept = 4, lty = 3, size = 0.4) +
  stat_summary(fun.y = "mean", geom = "line", size = 1) + 
  # geom_ribbon(alpha = 0.2, color = "grey", show.legend = F) +
  stat_summary(fun.data = mean_cl_boot, geom = 'ribbon',fun.args=list(conf.int=0.95),
               alpha = 0.5) +
  geom_point(aes(color = wmes_z), size = 0.8, show.legend = F) + #, alpha = .1, pch = 5
  scale_x_continuous(breaks = c(-4, 0, 4, 8, 12),
                     labels = c("-200", "0", "200", "400", "600")) +
  labs(x = "Time (ms) relative to target syllable offset",
       y = "Empirical logit of looks to target") +
  theme_big + legend_adj + labs(fill = "WM (HS ES)") +
  theme(legend.position = c(0.08, 0.8))



ggsave(paste0(figs_path, "/transEN_AoA.png"), transEN_AoA, width = 150,
       height = 120, units = "mm", dpi = 600)
ggsave(paste0(figs_path, "/transEN_proficiency.png"), transEN_prof, width = 150,
       height = 120, units = "mm", dpi = 600)
ggsave(paste0(figs_path, "/transEN_transparency.png"), transEN_transparency, width = 150,
       height = 120, units = "mm", dpi = 600)
ggsave(paste0(figs_path, "/transEN_gender.png"), transEN_gender, width = 150,
       height = 120, units = "mm", dpi = 600)
ggsave(paste0(figs_path, "/transEN_wm.png"), transEN_wm, width = 150,
       height = 120, units = "mm", dpi = 600)


ggsave(paste0(figs_path, "/transES_AoA.png"), transES_AoA, width = 150,
       height = 120, units = "mm", dpi = 600)
ggsave(paste0(figs_path, "/transES_proficiency.png"), transES_prof, width = 150,
       height = 120, units = "mm", dpi = 600)
ggsave(paste0(figs_path, "/transES_transaarency.png"), transES_transparency, width = 150,
       height = 120, units = "mm", dpi = 600)
ggsave(paste0(figs_path, "/transES_gender.png"), transES_gender, width = 150,
       height = 120, units = "mm", dpi = 600)
ggsave(paste0(figs_path, "/transES_wm.png"), transES_wm, width = 150,
       height = 120, units = "mm", dpi = 600)


ggsave(paste0(figs_path, "/strucEN_AoA.png"), strucEN_AoA, width = 150,
       height = 120, units = "mm", dpi = 600)
ggsave(paste0(figs_path, "/strucEN_proficiency.png"), strucEN_prof, width = 150,
       height = 120, units = "mm", dpi = 600)
ggsave(paste0(figs_path, "/strucEN_structure.png"), strucEN_structure, width = 150,
       height = 120, units = "mm", dpi = 600)
ggsave(paste0(figs_path, "/strucEN_gender.png"), strucEN_gender, width = 150,
       height = 120, units = "mm", dpi = 600)
ggsave(paste0(figs_path, "/strucEN_wm.png"), strucEN_wm, width = 150,
       height = 120, units = "mm", dpi = 600)


ggsave(paste0(figs_path, "/strucES_AoA.png"), strucES_AoA, width = 150,
       height = 120, units = "mm", dpi = 600)
ggsave(paste0(figs_path, "/strucES_proficiency.png"), strucES_prof, width = 150,
       height = 120, units = "mm", dpi = 600)
ggsave(paste0(figs_path, "/strucES_structure.png"), strucES_structure, width = 150,
       height = 120, units = "mm", dpi = 600)
ggsave(paste0(figs_path, "/strucES_gender.png"), strucES_gender, width = 150,
       height = 120, units = "mm", dpi = 600)
ggsave(paste0(figs_path, "/strucES_wm.png"), strucES_wm, width = 150,
       height = 120, units = "mm", dpi = 600)

# -----------------------------------------------------------------------------

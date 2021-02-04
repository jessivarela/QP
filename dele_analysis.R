# DELE analysis ---------------------------------------------------------------

library(TOSTER)

# Source libs -----------------------------------------------------------------

source(here::here( "scripts", "00_load_libs.R"))

# -----------------------------------------------------------------------------
dem_all <- read_csv("./data/raw/pupurri_demographics.csv")

### DELE

dem_all$DELE <- as.numeric(dem_all$DELE)
dem_all %>% 
  filter(., group %in% c("ihs", "ahs")) %>%
  group_by(., group) %>% 
  summarize(., DELE_mean = mean(DELE), 
            DELE_sd = sd(DELE), 
            n = n())
# results - include hs 

# group   DELE mean  Dele SD  n

# ahs     43.4        2.48    24
# ihs     33.9        3.68    25

bartlett.test(DELE ~ group, data = dem_all)
# Bartlett's K-squared = 3.4417, df = 1, p-value = 0.06357

# DELE toast ahs vs ihs (???)
# This part below is not working idk what I am doing here

TOSTtwo(m1 = 43.4, sd1 = 2.48, n1 = 24, # ahs
        m2 = 33.9, sd2 = 3.68, n2 = 25, # ihs
        low_eqbound_d = -0.3, 
        high_eqbound_d = 0.3, 
        alpha = 0.05)


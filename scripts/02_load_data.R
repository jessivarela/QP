# Load data -------------------------------------------------------------------

source(here::here("scripts", "01_helpers.R"))

# Gender agreement
#det10target <- readRDS(here("data", "clean", "det_10ms_onset_target.rds")) #if problems bc too big
det10target <- read_csv(here::here("data", "clean", "det_10ms_onset_target.csv"))
det50 <- read_csv(here::here("data", "clean", "det_50ms_onset_target.csv"))


# -----------------------------------------------------------------------------

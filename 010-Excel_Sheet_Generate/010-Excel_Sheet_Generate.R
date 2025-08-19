library(tidyverse)
library(readxl)

##################################################################################################

# Only run once:

# Read in list of valid combinations of 3 squares presented out of 8
og_combos_df <- read_xlsx("010-Excel_Sheet_Generate/for_generating_sq_combo.xlsx")

# Read in csv of cue words and extract one cues for now
stim_og <- read.csv("010-Excel_Sheet_Generate/stim_64_NNVB.csv")
stim_64 <- data.frame(cue = stim_og$cue)

# Create data frame of no load combos
no_load_combos <- setNames(
  data.frame(matrix(data = 1, nrow = 32, ncol = 8)),
  paste0("sq", 1:8)) %>% 
  mutate(condition = "no_load", .before = "sq1")

# Create data frame of cues, conditions, and probability
x <-rep(1:nrow(stim_64), 3)
cue_combos <- data.frame(cue = stim_64[x,]) %>% 
  arrange(cue) %>% 
  mutate(condition = rep(c("load", "load", "no_load"), nrow(.)/3)) %>% 
  mutate(trial_type = rep(c("match", "mismatch", "match"), nrow(.)/3)) %>% 
  mutate(prob = 1)

##################################################################################################

# Will be turned into a loop: 

## Add load condition column and randomly choose 32 displays
load_combos <- sample_n(og_combos_df, 32) %>% 
  mutate(condition = "load", .before = "sq1")

## New data frame with both load and no load, randomly shuffled
all_combos <- rbind(load_combos, no_load_combos)




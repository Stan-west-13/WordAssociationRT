library(tidyverse)
library(readxl)
library(rstatix)
library(ez)
library(lmerTest)

# Read in metadata df
metadata <- read_xlsx("metadata_WMRT.xlsx")
metadata <- metadata %>% 
  mutate(across(c(participant:trial_type, accuracy, square_resp), as.factor)) %>% 
  mutate(across(c(cue_rt:square_rt), as.numeric))

# Count of accuracy for each participant by WS condition: load and no load

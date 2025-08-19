library(tidyverse)
library(readxl)

##################################################################################################

# Only run once:

# Read in list of valid combinations of 3 squares presented out of 8
og_combos_df <- read_xlsx("010-Excel_Sheet_Generate/for_generating_sq_combo.xlsx")

# Create data frame of no load combos
no_load_combos <- setNames(
  data.frame(matrix(data = 1, nrow = 32, ncol = 8)),
  paste0("sq", 1:8)) %>% 
  mutate(load_condition = "no_load", .before = "sq1") %>% 
  mutate(test_condition = "match")

##################################################################################################

# Will be turned into a loop: 

## Add load condition column and randomly choose 32 displays
load_combos <- og_combos_df %>% 
  mutate(load_condition = "load", .before = "sq1") %>% 
  sample_n(., 32)

## Randomly give each combo either "match" or "mismatch" test_condition (16 each)
load_combos$test_condition <- sample(rep(c("match", "mismatch"), nrow(load_combos)/2))

## New data frame with both load and no load, randomly shuffled
all_combos <- rbind(load_combos, no_load_combos)






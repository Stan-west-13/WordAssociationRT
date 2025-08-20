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
cue_combos <- expand.grid(cue = stim_64$cue, condition = c("load","no_load"), 
                          trial_type = c("match","mismatch"),prob = 1 ) %>% 
  filter(!c(condition == "no_load" & trial_type == "mismatch")) %>% 
  arrange(cue)

##################################################################################################

# Loop for sheets: 

## Make sure counter is at 0 before you begin!!
counter = 1

while (counter <= 1){
  # Keeps track of how many sheets have been generated
  print(counter)
  counter = counter + 1
  
  # Creates meta df with randomized cue positions
  meta_df <- expand.grid(cue = sample(stim_64$cue, nrow(stim_64)))
  
  # Add load condition column and randomly choose 32 displays
  load_combos <- sample_n(og_combos_df, 32) %>% 
    mutate(condition = "load", .before = "sq1")
  
  # Data frames for within subject sheet generation 
  condition_df <- data.frame(
    condition = c("load", "no_load"),
    prob = c(0.5, 0.5)
  )
  TT_df <- data.frame(
    trial_type = c("match", "mismatch"),
    prob = c(0.5, 0.5)
  )
  
  # Loop to assign cue with condition, trial type, and combo
  for (i in 1:nrow(meta_df)){
    c <- sample(condition_df$condition, size = 1, prob = condition_df$prob)
    meta_df$condition[i] <- c 
    condition_df$prob[condition_df$condition == c] <- condition_df$prob[condition_df$condition == c] - (1/32)
  }
  
  print(meta_df)
  
  
}





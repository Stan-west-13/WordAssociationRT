#############################
# Load in packages and data
#############################

# Packages
library(tidyverse)

# Data 
filtered_corrected <- readRDS("Score_Scripts/rds_data/filter_corrected_2026-03-01.rds")
norms_dict <- readRDS("Score_Scripts/rds_data/norms_dict_2026-03-01.rds")
norms_list <- readRDS("Score_Scripts/rds_data/norms_list_2026-03-01.rds")

##################
# Creative Scores 
##################

## Creative scores are decided by whether or not a given cue-response pair is unique among 
## cue-responses pairs given in EACH CONDITION. In other words, if the frequency of the 
## cue-responses pair is 1, then that cue-responses pair is judged as creative in that condition. 
## Given our definition of creative that we give to participants (a responses they think would be 
## uncommon among other players), and this would be relative to players under each condition (e.g. 
## players in the child oriented condition would have different stereotypical responses made than in
## any of the other three conditions), cue-response pairs that are unique in a given condition 
## should be representative of creative responses. 
creative_all <- filtered_corrected %>% 
  relocate(strength_strat, .before = aoa) %>%
  relocate(type, .after = strength_strat) %>% 
  group_by(condition, cue, response) %>% 
  mutate(frequency_response = n(), .after = response) %>%
  group_by(cue) %>% 
  mutate(max_response = ifelse(frequency_response == max(frequency_response[condition == 'peer']) & (condition == 'peer'), TRUE, FALSE), 
         .after = frequency_response) %>% 
  group_by(condition, cue, corrected_response) %>% 
  mutate(frequency_corrected = n(), .after = corrected_response) %>% 
  group_by(cue) %>% 
  mutate(max_corrected = ifelse(frequency_corrected == max(frequency_corrected[condition == 'peer']) & (condition == 'peer'), TRUE, FALSE),
         .after = frequency_corrected) %>%
  ungroup() %>% 
  distinct() %>% 
  arrange(cue, desc(frequency_response), desc(frequency_corrected)) %>% 
  # this adds creative scores for both uncorrected and corrected responses
  group_by(participant, cue) %>% 
  mutate(creative_score = ifelse((response %in% norms_dict$response[(cue == norms_dict$cue) &
                                                                      (norms_dict$frequency_response == 1) &
                                                                      (condition == norms_dict$condition)]), 1, 0), .after = response) %>% 
  mutate(creative_corrected = ifelse(corrected_response %in% norms_dict$corrected_response[(cue == norms_dict$cue) &
                                                                                             (norms_dict$frequency_corrected == 1) &
                                                                                             (condition == norms_dict$condition)], 1, 0), .after = corrected_response) %>% 
  # this adds the count of creative responses per participant
  group_by(participant) %>% 
  mutate(creative_count = sum(as.integer(creative_score)), .after = response) %>%
  mutate(creative_count_corrected = sum(as.integer(creative_corrected)), 
         .after = corrected_response) %>% 
  ungroup() %>% 
  select(c(-cue_onset:-key, -response.start, -response.stop))

###########
# Analyses
###########





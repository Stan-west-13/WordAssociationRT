#############################
# Load in packages and data
#############################

# Packages
library(tidyverse)

# Data 
filtered_corrected <- readRDS("Score_Scripts/rds_data/filter_corrected_2026-03-01.rds")
norms_dict <- readRDS("Score_Scripts/rds_data/norms_dict_2026-03-01.rds")
norms_list <- readRDS("Score_Scripts/rds_data/norms_list_2026-03-01.rds")

###################
# Stereotypy Scores 
###################

stereotypy_all <- filtered_corrected %>%
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
  arrange(participant, cue, desc(frequency_response)) %>% 
  group_by(cue) %>% 
  mutate(stereotypy_score = as.integer(
    response %in% norms_list$response[norms_list$max_response == TRUE & 
                                        norms_list$cue %in% cue &
                                        norms_list$condition == 'peer']), 
    .after = response) %>%
  mutate(stereotypy_score_corrected = as.integer(
    corrected_response %in% norms_list$corrected_response[norms_list$max_corrected == TRUE & 
                                                            norms_list$cue %in% cue &
                                                            norms_list$condition == 'peer']),
    .after = corrected_response) %>% 
  # this adds the count of stereotypical responses per participant
  group_by(participant) %>% 
  mutate(stereotypy_count = sum(as.integer(stereotypy_score == 1)), .after = response) %>%
  mutate(stereotypy_count_corrected = sum(as.integer(stereotypy_score_corrected == 1)), 
         .after = corrected_response) %>% 
  select(c(-cue_onset:-key, -response.start, -response.stop))



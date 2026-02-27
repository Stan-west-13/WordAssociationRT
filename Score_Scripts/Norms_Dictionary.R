#############################
# Load in packages and data
#############################

# Packages
library(tidyverse)

# Data
d <- readRDS("psychling/mapped_response_metadata_2025-05-25.rds")

## Filtering out responses faster than 250 ms and more than
## 2.5 standard deviations away from participant response time 
## mean. Also getting rid of non-responses and NA values
filter_participants <- d %>%
  filter(!participant %in% c("TTA_067","TTA_068")) %>%
  filter(!is.na(response)) %>% 
  mutate(rt = rt *1000) %>% 
  filter(rt > 250) %>% 
  filter(!Nletters == 0) %>% 
  group_by(participant) %>% 
  mutate(z_rt_pp = (rt - mean(rt))/sd(rt)) %>% 
  filter(z_rt_pp < 2.5) %>% 
  ungroup() %>% 
  mutate(
    participant = as.factor(participant),
    condition = factor(condition, c("child", "peer", "short", "creative")),
    cue = factor(cue),
    response = str_trim(response)
  )

## This data frame establishes the corrected version of responses while 
## keeping the original response and revision columns. It also removes 
## blanks and responses that are not relevant to cue (aka, anything 
## signaling they did not know the cue). This also removes any responses
## or corrected responses that match the cue.
filter_participants_corrected <- filter_participants %>%
  mutate(corrected_response = ifelse((is.na(revision) != TRUE) & revision != "", revision, response),
         .after = revision) %>% 
  filter(response != "") %>%
  filter(response != "notsure", response != "idk", response != "notsre") %>% 
  filter(corrected_response != cue)


##########################################
# Creating norms list and norms dictionary
##########################################

# Norms List 

## This norms list has both frequency and the most commonly given
## cue response pair for EACH CONDITION. The columns max_response 
## and max_corrected are only used when doing stereotypy scoring.
## So, these two columns reflect the cue-response pair made the 
## most in the PEER condition since the peer condition is thought 
## to elicit the most stereotypical responses. 
norms_list <- filter_participants_corrected %>%
  select(condition, cue, response, revision, corrected_response) %>%
  filter(condition == 'peer') %>% 
  group_by(cue, response) %>% 
  mutate(frequency_response = n()) %>%
  group_by(cue) %>% 
  mutate(max_response = ifelse(frequency_response == max(frequency_response), TRUE, FALSE)) %>% 
  group_by(cue, corrected_response) %>% 
  mutate(frequency_corrected = n()) %>% 
  group_by(cue) %>% 
  mutate(max_corrected = ifelse(frequency_corrected == max(frequency_corrected), TRUE, FALSE)) %>%
  ungroup() %>% 
  distinct() %>% 
  arrange(cue, desc(frequency_response), desc(frequency_corrected))

### Working on this further tomorrow. (extra df is temporary until I finish testing it)
norms_list2 <- filter_participants_corrected %>%
  select(condition, cue, response, revision, corrected_response) %>%
  group_by(condition, cue, response) %>% 
  mutate(frequency_response = n()) %>%
  group_by(cue) %>% 
  mutate(max_response = ifelse(frequency_response == max(frequency_response), TRUE, FALSE)) %>% 
  group_by(cue, corrected_response) %>% 
  mutate(frequency_corrected = n()) %>% 
  group_by(cue) %>% 
  mutate(max_corrected = ifelse(frequency_corrected == max(frequency_corrected), TRUE, FALSE)) %>%
  ungroup() %>% 
  distinct() %>% 
  arrange(cue, desc(frequency_response), desc(frequency_corrected))

# Norms Dictionary

## I will work on merging these two into one df tomorrow.

norm_dic <- norms_list %>% 
  select(cue,response, frequency_response, max_response)

norm_dic_corrected <- norms_list %>% 
  select(cue, corrected_response, frequency_corrected, max_corrected) %>% 
  distinct()

##################
# Save out .rds of what is needed 
# Still need norm_dic to reference cue-response pairings


# I think that for creativity, if you add ' & (condition == norm_dic$condition)
# to what is at lines 236 and 237, I think you could have condition in the norm_dic
# to reference. you would also have to add it to the group by













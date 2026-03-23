library(tidyverse)
library(readr)
source("R/Load_Helpers.R") 

d <- load_most_recent_by_mtime("data","TTA_combined_responses-") 

## Non-response density by cue
cue_no_response_density <- d %>%
  group_by(cue, condition) %>%
  summarize(resp_d = sum(response == ""|is.na(response))/n()) %>%
  mutate(is.disp = ifelse(resp_d >= (1/3), TRUE, FALSE))


response_d <- d %>% 
  group_by(condition) %>%
  mutate(low.response = ifelse(cue %in% cue_no_response_density$cue[cue_no_response_density$is.disp == TRUE], 
                               TRUE,
                               FALSE)) %>%
  group_by(participant) %>%
  mutate(prop_noresp = sum(((response == ""|is.na(response)) & low.response == FALSE))/n())
  

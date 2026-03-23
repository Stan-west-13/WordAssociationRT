library(tidyverse)
library(ggplot2)
library(readr)
source("R/Load_Helpers.R") 

d <- load_most_recent_by_mtime("data","TTA_combined_responses-") 

## Non-response density by cue
cue_no_response_density <- d %>%
  group_by(cue, condition) %>%
  summarize(resp_d = sum(response == ""|is.na(response))/n()) %>%
  mutate(is.disp = ifelse(resp_d >= (1/3), TRUE, FALSE))

## Plot proportions
ggplot(cue_no_response_density %>%
         filter(resp_d > 0), aes(x = resp_d, y = reorder(cue, desc(-resp_d)), fill = condition))+
  geom_bar(stat="identity")+
  facet_grid(~condition)+
  geom_vline(aes(xintercept =  1/3),linetype = "dashed")+
  labs(x = "proportion of no-responses by condition", y = "cue")


## Which participants are not responding to cues outside of the generally
## "hard" cues. 
response_d <- d %>% 
  group_by(condition) %>%
  mutate(low.response = ifelse(cue %in% cue_no_response_density$cue[cue_no_response_density$is.disp == TRUE], 
                               TRUE,
                               FALSE)) %>%
  group_by(participant) %>%
  mutate(prop_noresp = sum(((response == ""|is.na(response)) & low.response == FALSE))/n())
  


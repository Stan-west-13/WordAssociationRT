load("psychling/responses_metadata_2025-02-27.Rdata")
library(dplyr)
d <- combined_meta %>%
  select(cue,condition,response, AoA_Kup_lem, Lg10WF, Lg10CD) %>%
  unique() %>%
  mutate(AoA_inv = max(AoA_Kup_lem, na.rm =T) - AoA_Kup_lem ) %>%
  group_by(cue,response,condition) %>%
  mutate(comp = sum(AoA_inv,Lg10CD,Lg10WF, na.rm = T)) %>%
  filter(condition == "child") %>%
  ungroup() %>%
  select(cue, comp) %>%
  unique()
  

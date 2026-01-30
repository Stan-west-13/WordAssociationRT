library(tidyverse)
library(readxl)

# Unzip data folder
unzip("data_WMRT.zip")

tta001 <- read_xlsx('data_WMRT/TTA_001/TTA_001_LOAD_2025-09-25.xlsx')
tta001 <- tta001 %>% 
  mutate(condition_b = tta001[36,2], .after = condition)

# Makes a list of all the files we want in our metadata df
files <- list.files("data_WMRT", pattern = '.xlsx', recursive = T,full.names = TRUE)
files <- str_subset(files, 'PRAC', negate = T)

# Takes each file from files, reads it in, creates between subjects condition column, and cleans up df a bit
metadata <- map_dfr(files, function(x){
  d <- read_xlsx(x, col_types ="text" ,trim_ws = TRUE)
  d <- d %>% 
    mutate(condition_b = d[[36,2]], .after = pp)
  d <- d[-c(33:41),]
  d <- d %>%
    select(participant = pp, condition_b, condition_w = condition, trial_type = TT, cue, response = input_textbox.text_raw, square_resp = yesno_resp.keys_raw,
           cue_rt = key_space.rt_mean, resp_rt = input_key.rt_mean, square_rt = yesno_resp.rt_mean) %>%
    mutate(response = gsub(response, pattern = "'", replacement = ""),
          response = trimws(gsub(response, pattern = "\\\\n", replacement = "")),
          square_resp = gsub(square_resp, pattern = "'", replacement = ""))
  d <- d %>% 
    mutate(accuracy = ifelse((trial_type == "match" & square_resp == "j") | (trial_type == 'mismatch' & square_resp == 'f'), 1, 0),
           .after = response) %>% 
    mutate(across(c(participant:trial_type, accuracy, square_resp), as.factor)) %>% 
    mutate(across(c(cue_rt:square_rt), as.numeric))
  
}) #map_dfr reads in the tables then it stacks them by rows
















library(readr)
library(tidyverse)
source("R/Load_Helpers.R")

## Load data
d <- load_most_recent_by_mtime("data", "TTA")

## Load psycholinguistics
sub <- read.csv("psychling/SUBTLEXusfrequencyabove1.csv")
sub_map <- read_rds("psychling/subtlex 1.rds")
aoa <- read.csv("psychling/AoA_51715_words.csv")
aoa_map <- read_rds("psychling/kuperman 1.rds")
familiarity <- read_xlsx("psychling/13428_2018_1077_MOESM2_ESM.xlsx")[,-6]
rsp_map <- read_rds("psychling/response_map 2.rds")

## Combine psychling

## Join previously mapped responses with psycholinguistics 
rsp_map_joined <- rsp_map %>%
  left_join(select(sub_map,-word), by = c("subtlex_id" = "id")) %>%
  left_join(select(aoa_map,-word), by = c("kuperman_id" = "id")) %>%
  left_join(select(aoa, Word,Nphon,Nsyll), by = c("response" = "Word"))

## Combine mapped responses with unmapped psycholinguistic databases
all_psychling <- full_join(sub,aoa) %>%
  select(response = Word,Lg10WF, Lg10CD,aoa = AoA_Kup_lem,Nphon,Nsyll) %>%
  rbind(select(rsp_map_joined, response, Lg10CD,Lg10WF, aoa, Nphon,Nsyll)) %>%
  unique() %>%
  rowwise() %>%
  mutate(sum_na = sum(is.na(c_across(c(Lg10WF,Lg10CD,aoa))))) %>%
  group_by(response) %>%
  slice_min(sum_na, n = 1) %>% ## keep the mapping with least amount of missing values
  filter(!(response == "idk")) %>%
  select(-sum_na)


response_psychling <- d %>%
  left_join(all_psychling) %>%
  mutate(nchar = nchar(response)) %>% 
  group_by(cue) %>%
  filter(!response == cue)
saveRDS(response_psychling,file = paste0("data/TTA_response_mapped_meta-",Sys.Date(),".rds"))


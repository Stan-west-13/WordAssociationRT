library(RSQLite)
library(dplyr)
library(tidyverse)
library(readxl)
load("psychling/responses_metadata_2025-04-26_reformatted.Rdata")
psychling <- read_xlsx("psychling/13428_2018_1077_MOESM2_ESM.xlsx")
dbListTables(con)

con <- dbConnect(RSQLite::SQLite(), "Word-AssociationRT_mapped.db")

response_maps <- dbGetQuery(conn = con,"SELECT *
                             FROM response_map")

cues_responses <- dbGetQuery(conn = con,"SELECT *
                             FROM cues_responses")

cues <- dbGetQuery(conn = con,"SELECT *
                             FROM cues")

responses <- dbGetQuery(conn = con,"SELECT *
                             FROM responses")

kup <- dbGetQuery(conn = con,"SELECT *
                             FROM kuperman")

sub <- dbGetQuery(conn = con,"SELECT *
                             FROM subtlex")


response_revisions <- cues_responses %>%
  left_join(cues, by = c("cue_id" = "id")) %>%
  left_join(responses, by = c("response_id" = "id")) %>%
  left_join(response_maps %>% mutate(cue_response_id = as.integer(cue_response_id)), by = c("id" = "cue_response_id")) %>%
  select(-id,-id.y, -cue_id.x,-cue_id.y,-response_id) %>%
  left_join(kup, by = c("kuperman_id" = "id")) %>%
  left_join(sub %>% select(-word), by = c("subtlex_id" = "id")) %>%
  select(cue,response,revision,aoa,Lg10WF,Lg10CD) %>%
  unique()

response_psychling <- response_revisions %>%
  select(response,revision,aoa,Lg10WF,Lg10CD) %>%
  unique() %>%
  filter(!is.na(aoa) & !is.na(Lg10WF) & !is.na(Lg10CD)) %>%
  left_join(unique(select(combined_meta, response,Nletters,Nphon, Nsyll)), by = c("revision" = "response" )) %>%
  mutate(response = ifelse(is.na(revision),response,revision)) %>%
  unique()

response_psychling$response <- ifelse(is.na(response_psychling$revision),response_psychling$response,response_psychling$revision)

combined_meta_mapped <- combined_meta %>%
  select(-Lg10WF,-Lg10CD) %>%
  left_join(response_psychling %>% select(response,
                                          revision,
                                          aoa,
                                          Lg10WF,
                                          Lg10CD), by = "response") %>%
  select(participant,cue,strength_strat,type,response,revision,aoa,Lg10WF,Lg10CD,Nletters,Nphon,Nsyll,rt,condition,
         runningclock,cue_onset,cue_offset,key,response.start,response.stop,
         FREQcount,CDcount,FREQlow,Cdlow,SUBTLWF,SUBTLCD,Alternative.spelling,Freq_pm,Dom_PoS_SUBTLEX,
         Lemma_highest_PoS, AoA_Kup, Perc_known,Concreteness_mean,AoAinv_mean,Pknown_mean,nlettersinv_mean,R1_max,
         date,expName,psychopyVersion,frameRate,expStart,Nobs,Prevalence,FreqZipfUS)

combined_meta_mapped[!is.na(combined_meta_mapped$revision)]


combined_meta_mapped %>%
  select(cue,response,revision,aoa,AoA_Kup_lem,Lg10WF.x,Lg10CD.x,Lg10WF.y,Lg10CD.y) %>%
  View()




            
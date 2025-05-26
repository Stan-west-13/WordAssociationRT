library(RSQLite)
library(dplyr)
library(tidyverse)
library(readxl)

map_revisions_psychling <- function(orig_df, revision_df){
  for (i in 1:nrow(orig_df)){
    resp <- orig_df$response[i]
    if (is.na(orig_df$aoa[i]) & !is.na(orig_df$revision[i])){
      orig_df$aoa[i] <- unique(revision_df$aoa[revision_df$response == resp])
    }
    if (is.na(orig_df$Lg10CD[i]) & !is.na(orig_df$revision[i])){
      orig_df$Lg10CD[i] <- unique(revision_df$Lg10CD[revision_df$response == resp])
    }
    if (is.na(orig_df$Lg10WF[i]) & !is.na(orig_df$revision[i])){
      orig_df$Lg10WF[i] <- unique(revision_df$Lg10WF[revision_df$response == resp])
    }
    if (!is.na(orig_df$revision[i])){
      orig_df$Nletters[i] <- str_count(orig_df$revision[i])
    }
    else if (is.na(orig_df$revision[i])){
      orig_df$Nletters[i] <- str_count(orig_df$response[i])
    }
    if (is.na(orig_df$Nphon[i]) & !is.na(orig_df$revision[i])){
      orig_df$Nphon[i] <- unique(revision_df$Nphon[revision_df$response == resp])
    }
    if (is.na(orig_df$Nsyll[i]) & !is.na(orig_df$revision[i])){
      orig_df$Nsyll[i] <- unique(revision_df$Nsyll[revision_df$response == resp])
    }
    else
      orig_df$aoa[i] <- orig_df$aoa[i]
      orig_df$Lg10WF[i] <- orig_df$Lg10WF[i]
      orig_df$Lg10CD[i] <- orig_df$Lg10CD[i]
      orig_df$Nphon[i] <- orig_df$Nphon[i]
      orig_df$Nsyll[i] <- orig_df$Nsyll[i]
  }
  return(orig_df)
}

## Load in data##################################################
load("psychling/responses_metadata_2025-04-26_reformatted.Rdata")
combined_meta$aoa <- combined_meta$AoA_Kup_lem
psychling <- read.csv("psychling/AoA_51715_words.csv")


#################################################################

## Conncect to database and pull relevant tables ################
con <- dbConnect(RSQLite::SQLite(), "Word-AssociationRT_mapped.db")
dbListTables(con)

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
#############################################

## Disconnect 
dbDisconnect(conn = con)
#################

## Join cues, responses, revisions, and mapped psycholinguistic variabiles
response_revisions <- cues_responses %>%
  left_join(cues, by = c("cue_id" = "id")) %>%
  left_join(responses, by = c("response_id" = "id")) %>%
  left_join(response_maps %>% mutate(cue_response_id = as.integer(cue_response_id)), by = c("id" = "cue_response_id")) %>%
  select(-id,-id.y, -cue_id.x,-cue_id.y,-response_id) %>%
  left_join(kup, by = c("kuperman_id" = "id")) %>%
  left_join(sub %>% select(-word), by = c("subtlex_id" = "id")) %>%
  select(cue,response,revision,aoa,Lg10WF,Lg10CD) %>%
  unique()
####################################################################

## Make dataframe of cues,responses, and revisions to translate to original dataframe
response_revisions_only <- response_revisions %>%
  filter(!is.na(revision), !revision == "") %>%
  left_join(psychling %>% select(Word,Nletters,Nphon,Nsyll), by = c("revision" = "Word"))
################################################################################

## Join revisions to original dataframe ############################
combined_meta_revisions <- combined_meta %>%
  left_join(select(response_revisions_only,cue,response,revision), by = c("cue","response"))
#############################################################################


## Run function to replace aoa, Lg10WF, and Lg10CD with values from revised and mapped
## dataframe.
meta_mapped <- map_revisions_psychling(combined_meta_revisions,response_revisions_only)
##############################################################

## Reorganize and select relevant columns
combined_meta_mapped <- meta_mapped %>%
  select(participant,cue,strength_strat,type,response,revision,aoa,Lg10WF,Lg10CD,Nletters,Nphon,Nsyll,rt,condition,
         runningclock,cue_onset,cue_offset,key,response.start,response.stop,
         FREQcount,CDcount,FREQlow,Cdlow,SUBTLWF,SUBTLCD,Alternative.spelling,Freq_pm,Dom_PoS_SUBTLEX,
         Lemma_highest_PoS, AoA_Kup, Perc_known,Concreteness_mean,AoAinv_mean,Pknown_mean,nlettersinv_mean,R1_max,
         date,expName,psychopyVersion,frameRate,expStart,Nobs,Prevalence,FreqZipfUS)

 
## Save out data #####################
write_rds(combined_meta_mapped, file = paste0("psychling/mapped_response_metadata_",Sys.Date(),".rds"))
###################




            
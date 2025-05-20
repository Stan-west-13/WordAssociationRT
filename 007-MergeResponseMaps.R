library(RSQLite)
library(dplyr)
library(tidyverse)
load("psychling/responses_metadata_2025-04-26_reformatted.Rdata")

dbListTables(con)

con <- dbConnect(RSQLite::SQLite(), "Word-AssociationRT.db")

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
  left_join(sub %>% select(-word), by = c("subtlex_id" = "id"))

            
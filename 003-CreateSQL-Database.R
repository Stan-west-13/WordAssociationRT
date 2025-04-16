library(DBI)
library(RSQLite)
library(dplyr)
library(lubridate)
library(purrr)
library(readr)
library(stringr)
library(tidyr)

## Load data
load("psychling/responses_metadata_2025-03-28.Rdata")
source("R/dbStatementHelpers.R")
source("R/dbGetHelpers.R")

## Create studies table
studies <- data.frame(id = c(1,2,3), 
                      label = c("Adult_Association1", "Adult_Association2", "WordAssociationRT"), 
                      description = c("Movers and stayers in RSA were assessed 
                                       by collecting word associations from 60 
                                       cues with high and low probability of 
                                       having strong context effects. Collection 1",
                                      "Movers and stayers in RSA were assessed 
                                       by collecting word associations from 60 
                                       cues with high and low probability of 
                                       having strong context effects. Collection 2",
                                      "Word associations were collected for 64 cues
                                       that elicit child-oriened and 
                                       non-child-oriented resoponses in SWOW.
                                       Response times were collected to see how changing
                                       task instructions (context or no context) effects
                                       associative behavior"))





## Create psychling tables
kuperman_table <- read_csv("psychling/AoA_51715_words.csv") %>%
  mutate(
    id = seq_len(n()),
    word = tolower(str_trim(Word))
  ) %>%
  select(id, word, aoa = AoA_Kup_lem) %>%
  drop_na()

subtlex_table <- read_csv("psychling/SUBTLEXusfrequencyabove1.csv") %>%
  mutate(
    id = seq_len(n()),
    word = tolower(str_trim(Word))
  ) %>%
  select(id, word, Lg10WF, Lg10CD) %>%
  drop_na()

## Formatting metadata
metadata <- combined_meta %>%
  select(PPID = participant, condition, cue, strength_strat, type, response, rt = key_resp_cue.rt, 25:49) %>%
  mutate(rt_mili = rt * 1000, .after = rt) %>%
  group_by(PPID) %>%
  mutate(cue_order = seq_len(n()), .after = cue) %>%
  ungroup()


## Cue table
cue_table <- metadata %>%
  select(cue) %>%
  distinct() %>%
  arrange(cue) %>%
  mutate(id = seq_len(n())) %>%
  select(id,cue)

## Response behavior table setup
resp_behavior_table <- metadata %>%
  filter(!response == "") %>%
  select(subject_id = PPID,
         cue_order,
         cue,
         response) %>%
  mutate(
    id = 1:nrow(.)
  ) %>%
  left_join(cue_table %>% select(cue, cue_id = id),
            by = "cue") %>%
  select(-cue)

## Responses table
responses_table <- resp_behavior_table %>%
  filter(!response == "") %>%
  select(response) %>%
  distinct() %>%
  arrange(response) %>%
  mutate(id = seq_len(n())) %>%
  select(id, response)

## Response behavior table setup finish
response_behavior_table <- resp_behavior_table %>%
  left_join(responses_table %>% rename(response_id = id), by = "response") %>%
  select(id, subject_id , cue_order, cue_id, response_id, -response) %>%
  filter(!is.na(response_id))

## Load existing response maps and ids and add study ID
conn_adult_collection1 <- dbConnect(RSQLite::SQLite(),"databases/semantic_association_validation-crc.db")
adult_collection1_rm <- dbGetQuery(conn_adult_collection1, "SELECT * FROM response_map")
adult_collection1_respid <- dbGetQuery(conn_adult_collection1, "SELECT * FROM responses") %>%
  mutate(study_id = 1,.after = id)
dbDisconnect(conn_adult_collection1)

conn_adult_collection2 <- dbConnect(RSQLite::SQLite(),"databases/semantic_association_validation-crc_2.db")
adult_collection2_rm <- dbGetQuery(conn_adult_collection2, "SELECT * FROM response_map")
adult_collection2_respid <- dbGetQuery(conn_adult_collection2, "SELECT * FROM responses") %>%
  mutate(study_id = 2, .after = id)
dbDisconnect(conn_adult_collection2)




## Join responses from second adult associaton collection into the first and
## give new responses new unique ids.
studies_response <- rbind(adult_collection1_respid, adult_collection2_respid) %>%
  mutate(response_id = id,
         id = seq.int(n())) %>%
  select(id, study_id, response_id)













## Cues and resposnes table
cues_responses_table <- response_behavior_table %>%
  select(cue_id, response_id) %>%
  distinct() %>%
  arrange(cue_id, response_id) %>%
  mutate(id = seq_len(n())) %>%
  select(id, cue_id, response_id)


## Response mapping table
response_map_table <- cues_responses_table %>%
  left_join(
    responses_table %>% rename(response_id = id),
    by = "response_id"
  ) %>%
  left_join(
    kuperman_table %>% select(response = word, kuperman_id = id),
    by = "response"
  ) %>%
  left_join(
    subtlex_table %>% select(response = word, subtlex_id = id),
    by = "response"
  ) %>%
  select(-cue_id) %>%
  left_join(
    cue_table %>% select(cue_id = id, response = cue),
    by = "response"
  ) %>%
  filter(!(is.na(kuperman_id) & is.na(subtlex_id))) %>%
  rename(cue_response_id = id) %>%
  mutate(revision = NA, researcher_id = NA, timestamp = now(), id = seq_len(n())) %>%
  select(id, cue_response_id, kuperman_id, subtlex_id, cue_id, revision, researcher_id, timestamp, -response)


## PP table
subjects_table <- metadata %>%
  select(
    subject_id = PPID,
    condition
  ) %>%
  unique()

words_meta_table <- kuperman_table %>%
  select(word, kuperman_id = id) %>%
  full_join(subtlex_table %>% select(word, subtlex_id = id), by = "word") %>%
  full_join(cue_table %>% select(word = cue, cue_id = id), by = "word")



# Build database ----
con <- dbConnect(RSQLite::SQLite(), "databases/Word-AssociationRT.db")
dbExecute(con, "PRAGMA foreign_keys = ON;")
sql_schema <- read_sql_schema("databases/Word-AssociationRT.sql")
dbExecuteList(con, sql_schema)

dbWriteTable(con, "cues", cue_table, append = TRUE)
dbWriteTable(con, "kuperman", kuperman_table, append = TRUE)
dbWriteTable(con, "subtlex", subtlex_table, append = TRUE)
dbWriteTable(con, "subjects", subjects_table, append = TRUE)
dbWriteTable(con, "responses", responses_table, append = TRUE)
dbWriteTable(con, "response_behaviors", response_behavior_table, append = TRUE)
dbWriteTable(con, "cues_responses", cues_responses_table, append = TRUE)
dbWriteTable(con, "response_map", response_map_table, append = TRUE)
dbWriteTable(con, "words_meta", words_meta_table, append = TRUE)

dbDisconnect(con)







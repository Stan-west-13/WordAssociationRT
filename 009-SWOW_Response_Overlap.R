###########################################################

## Load in necessary data and packages
library(tidyverse)
library(ez)
library(pastecs)
library(psych)
library(rstatix)

load("psychling/SWOW_WordAssociations.Rdata")
d <- readRDS("psychling/mapped_response_metadata_2025-05-25.rds")

filter_participants <- d %>%
  filter(!participant %in% c("TTA_067","TTA_068")) %>%
  mutate(rt = rt *1000) %>% 
  filter(rt > 250) |>
  filter(!Nletters == 0) |>
  group_by(participant) |>
  mutate(z_rt_pp = (rt - mean(rt))/sd(rt))|>
  filter(z_rt_pp < 2.5) |>
  ungroup() |>
  mutate(
    participant = as.factor(participant),
    condition = factor(condition, c("child", "peer", "short", "creative")),
    cue = factor(cue),
    response = str_trim(response)
  )

filter_participants_corrected <- filter_participants %>%
  mutate(corrected_response = ifelse((is.na(revision) != TRUE) & revision != "", revision, response),
         .after = revision) %>% 
  filter(response != "") %>%
  filter(response != "notsure", response != "idk")

########################################################

## Df for TTA 
tta_df <- filter_participants_corrected %>%
  select(condition, cue, response, revision, corrected_response) %>%
  filter(!is.na(response)) %>% 
  group_by(cue, response) %>% 
  mutate(frequency_response = n()) %>%
  group_by(cue) %>% 
  mutate(max_response = ifelse(frequency_response == max(frequency_response), TRUE, FALSE)) %>% 
  group_by(cue, corrected_response) %>% 
  mutate(frequency_corrected = n()) %>% 
  group_by(cue) %>% 
  mutate(max_corrected = ifelse(frequency_corrected == max(frequency_corrected), TRUE, FALSE)) %>%
  ungroup() %>% 
  #distinct() %>% 
  arrange(cue, desc(frequency_response), desc(frequency_corrected))

tta_dic <- tta_df %>% 
  select(c(cue, corrected_response)) %>% 
  rename(c_response = corrected_response)
  #distinct()

## Renaming cues, omitting rows with NA, filtering for only cues we used
x <- levels(tta_dic$cue)
x <- as.factor(x)

swow_df <- SWOW_WordAssociations %>% 
  rename(
    cue = CUE,
    resp_id = RESP_ID,
    response = TARGET
  ) %>%
  select(c(cue,response)) %>% 
  na.omit() %>% 
  filter(cue %in% x) %>% 
  distinct() %>% 
  arrange(cue, response)

## Now, I want to compare cue-responses pairs. If the cue-response pair in our data set occurs in swow data set, 
## give it a 0. If not, give it a 1.

overlap_df <- tta_dic %>% 
  group_by(cue) %>% 
  mutate(overlap = ifelse((c_response %in% swow_df$response[swow_df$cue %in% cue]), "yes", "no"))

## Showing how many cue-response pairs overlapped (1) or not (0)
plot <- overlap_df %>% 
  count(overlap)

## Plotting how many cue-response pairs are overlapping or not for each cue
plot$overlap <- as.factor(plot$overlap)
ggplot(plot, aes(x = overlap, y = n, fill = overlap)) + 
  geom_bar(stat = 'identity') +
  facet_wrap(~cue)

## Cue-response pair not overlapping by cue
ggplot(plot, aes(x = n, y = cue, fill = overlap)) + 
  geom_col(position = 'stack')





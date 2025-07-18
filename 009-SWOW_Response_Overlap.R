###########################################################

## Load in necessary data and packages
library(tidyverse)
library(ez)
library(pastecs)
library(psych)
library(rstatix)
library(textstem)

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

tta_dic_L <- tta_dic %>% 
  mutate(L_response = tolower(c_response)) %>% 
  mutate(L_response = lemmatize_words(L_response))
  
## Renaming cols, omitting rows with NA, filtering for only cues we used
x <- tta_dic$cue
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

## Making responses all lowercase and lemmatizing words
swow_df_L <- swow_df %>% 
  mutate(response = tolower(response)) %>% 
  mutate(response = lemmatize_words(response))

## Now, I want to compare cue-responses pairs. If the cue-response pair in our data set occurs in swow data set, 
## give it a 'yes'. If not, give it a 'no'.
overlap_df <- tta_dic %>% 
  group_by(cue) %>% 
  mutate(overlap = ifelse((c_response %in% swow_df$response[swow_df$cue %in% cue]), 1, 0)) %>% 
  mutate(proportion = sum(overlap == 0)/n(),
         no_overlap = sum(overlap == 0),
         total = n())

overlap_df_L <- tta_dic_L %>% 
  group_by(cue) %>% 
  mutate(overlap = ifelse((L_response %in% swow_df_L$response[swow_df_L$cue %in% cue]), 1, 0)) %>% 
  mutate(proportion = sum(overlap == 0)/n(),
         no_overlap = sum(overlap == 0),
         total = n())

ifelse((x = (overlap_df != overlap_df_L)), print(x), 'no')

## Looking at corrections made
tta_check <- tta_dic_L %>% 
  filter(!c_response == L_response)

################### PLOTTING ###############

## Showing how many cue-response pairs overlapped (yes) or not (no)
plot <- overlap_df %>% 
  count(overlap) %>% 
  rename(count = n)

plot_L <- overlap_df_L %>% 
  count(overlap) %>% 
  rename(count = n)

## Plotting how many cue-response pairs are overlapping or not for each cue
plot$overlap <- as.factor(plot$overlap)
ggplot(plot, aes(x = overlap, y = count, fill = overlap)) + 
  geom_bar(stat = 'identity') +
  facet_wrap(~cue)

plot_L$overlap <- as.factor(plot_L$overlap)
ggplot(plot_L, aes(x = overlap, y = count, fill = overlap)) + 
  geom_bar(stat = 'identity') +
  facet_wrap(~cue)

## Cue-response pair not overlapping by cue
ggplot(plot, aes(x = count, y = cue, fill = overlap)) + 
  geom_col(position = 'stack')

ggplot(plot_L, aes(x = count, y = cue, fill = overlap)) + 
  geom_col(position = 'stack')

#############PROPORTIONS###############

z_pro <- overlap_df %>% 
  distinct(cue,proportion) %>% 
  ungroup()%>% 
  mutate(z_score = (proportion - mean(proportion))/sd(proportion)) %>% 
  filter(z_score >= 2)

z_pro_L <- overlap_df_L %>% 
  distinct(cue,proportion) %>% 
  ungroup()%>% 
  mutate(z_score = (proportion - mean(proportion))/sd(proportion)) %>% 
  filter(z_score >= 1.5)

list <- overlap_df_L %>% 
  filter(cue %in% c('abhor', "brimstone", "crusader", "detest", "forum", "overdone", "repression")) %>% 
  distinct(cue,L_response)

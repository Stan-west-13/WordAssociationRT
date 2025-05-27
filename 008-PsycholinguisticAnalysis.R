d <- readRDS("psychling/mapped_response_metadata_2025-05-25.rds") 
library(dplyr)
library(tidyr)
library(ggplot2)
library(ez)
library(purrr)
library(lme4)
library(lmerTest)
library(statmod)
library(fitdistrplus)
library(stringr)
## Remove outliers and convert rt to miliseconds ####################
filter_participants <- d %>%
  filter(!participant %in% c("TTA_067","TTA_068")) %>%
  mutate(rt = rt *1000)


## Filter out responses faster than 250ms and responses that are more than 
## 2.5 standard deviations away from participant mean. Also get rid of non-responses ########################
filter_participants_psychling <- filter_participants |>
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


## Plotting measures
plot_df <- filter_participants_psychling %>%
  pivot_longer(cols = c("aoa", "Lg10WF", "Lg10CD","Nletters","Nsyll"),
               names_to = "measure",
               values_to = "value") %>% 
  relocate(c(measure, value), .after = condition)

ggplot(plot_df, aes(x = condition, y = value))+
  geom_bar(stat = "summary", fun = "mean")+
  facet_wrap(~measure,scales = 'free')

## Looking at descriptive stats by condition for each measure
plot_df %>% 
  group_by(condition, measure) %>% 
  get_summary_stats(value, type = "common")

## ANOVA
### since we don't have multiple responses, do I use a between subjects ANOVA

#average over cues in each condition, then within cue between conditions ANOVA; pairwise differences between conditions in context of ANOVA 

temp <- filter_participants_psychling %>% 
  drop_na(Nletters)

aoa_model <- ezANOVA(data = temp,
                     dv = aoa,
                     wid = participant,
                     between = condition,
                     detailed = TRUE)
# error with cells having NA when aggregated to a mean
aoa_model

pairwise.t.test(temp$aoa, temp$condition, paried = FALSE)


cd_model <- ezANOVA(data = temp,
                     dv = Lg10CD,
                     wid = participant,
                     between = condition,
                     detailed = TRUE)
# error with cells having NA when aggregated to a mean
cd_model

pairwise.t.test(temp$Lg10CD, temp$condition, paried = FALSE)


wf_model <- ezANOVA(data = temp,
                     dv = Lg10WF,
                     wid = participant,
                     between = condition,
                     detailed = TRUE)
# error with cells having NA when aggregated to a mean
wf_model

pairwise.t.test(temp$Lg10WF, temp$condition, paried = FALSE)


#------------

## Number of letters 
nletters_model <- ezANOVA(data = temp,
                     dv = Nletters,
                     wid = participant,
                     between = condition,
                     detailed = TRUE)
# error with cells having NA when aggregated to a mean
nletters_model

pairwise.t.test(temp$Nletters, temp$condition, paried = FALSE)


#------------

nsyll_model <- ezANOVA(data = temp,
                     dv = Nsyll,
                     wid = participant,
                     between = condition,
                     detailed = TRUE)
# error with cells having NA when aggregated to a mean
nsyll_model

pairwise.t.test(temp$Nsyll, temp$condition, paried = FALSE)

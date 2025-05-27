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
               values_to = "value")

ggplot(plot_df, aes(x = condition, y = value))+
  geom_bar(stat = "summary", fun = "mean")+
  facet_wrap(~measure,scales = 'free')


## Psychling analyses
cue_averages <- filter_participants_psychling %>%
  group_by(cue, condition) %>%
  summarize(across(c(Lg10WF, Lg10CD, aoa, Nletters, Nsyll), function(x) mean(x, na.rm = TRUE))) %>%
  ungroup() 



dependent_variables <- c("Lg10WF", "Lg10CD", "aoa", "Nletters", "Nsyll")
names(dependent_variables) <- dependent_variables
posthoc_contrasts <- list(pc = c("child", "peer"),
                          cs = c("child", "short"),
                          ps = c("peer", "short"), 
                          crc = c("creative", "child"),
                          crp = c("creative","peer"),
                          crs = c("creative", "short"))

anovas <- map(dependent_variables, function(dv) {
  list(
    full_model = eval(substitute(
      ezANOVA(
        data = cue_averages,
        dv = .dv,
        wid = cue,
        between = condition
      ), list(.dv = dv))),
    posthocs = map(posthoc_contrasts, function(ph_contr, dv) {
      eval(substitute(ezANOVA(
        data = cue_averages %>%
          filter(condition %in% ph_contr) %>%
          droplevels(),
        dv = .dv,
        wid = cue,
        between = condition
      ), list(.dv = dv)))
    }, dv = dv)
  )
})


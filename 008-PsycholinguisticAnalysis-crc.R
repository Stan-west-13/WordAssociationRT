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

m <- filter_participants |>
  filter(condition %in% c("peer", "short", "child")) |>
  mutate(condition = factor(condition, levels = c("child", "short", "peer"))) |>
  lm(Nletters ~ condition, data = _)

davg <- filter_participants |>
  filter(condition %in% c("peer", "short", "child")) |>
  filter(!is.na(response)) |>
  mutate(
    condition = factor(condition, levels = c("child", "short", "peer")),
    cue = as.factor(cue),
    revision = if_else(is.na(revision), response, revision),
    Nletters = nchar(revision)
  ) |>
  group_by(cue, condition, type) |>
  summarize(aoa = mean(aoa, na.rm = TRUE))

a <- aov(aoa ~ condition * type + Error(cue), data = davg)

summary(a)

davg |>
  group_by(condition, type) |>
  summarize(s = sd(aoa), aoa = mean(aoa)) |>
  ggplot(aes(x = condition, y = aoa, color = type)) +
  geom_pointrange(aes(ymin = aoa - s, ymax = aoa +s), position = position_dodge(.7))


coef(a)

dd <- filter_participants |>
  filter(condition %in% c("peer", "short", "child")) |>
  mutate(condition = factor(condition, levels = c("child", "short", "peer")))


cc <- split(dd, dd$condition) |>
  map(~ sort(unique(.x$cue)))

all.equal(cc[[1]], cc[[3]])
  
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


## Analysis

cue_averages <- filter_participants_psychling %>%
  group_by(cue, condition) %>%
  summarize(across(c(Lg10WF, Lg10CD, aoa, Nletters, Nsyll), function(x) mean(x, na.rm = TRUE))) %>%
  ungroup()

davg <- filter_participants |>
  filter(condition %in% c("peer", "short", "child")) |>
  filter(!is.na(response)) |>
  mutate(
    condition = factor(condition, levels = c("child", "short", "peer")),
    cue = as.factor(cue),
    type = as.factor(type),
    revision = if_else(is.na(revision), response, revision),
    Nletters = nchar(revision)
  ) |>
  group_by(cue, condition, type) |>
  summarize(across(c(Lg10WF, Lg10CD, aoa, Nletters, Nsyll), function(x) mean(x, na.rm = TRUE))) |>
  ungroup()

dependent_variables <- c("Lg10WF", "Lg10CD", "aoa", "Nletters", "Nsyll")
names(dependent_variables) <- dependent_variables
posthoc_contrasts <- list(pc = c("child", "peer"),
                          cs = c("child", "short"),
                          ps = c("peer", "short"))






anovas <- map(dependent_variables, function(dv, cue_averages) {
  list(
    full_model = eval(substitute(
      ezANOVA(
        data = cue_averages,
        dv = .dv,
        wid = cue,
        within = condition,
        between = type
      ), list(.dv = dv))),
    posthocs = map(posthoc_contrasts, function(ph_contr, dv) {
      eval(substitute(ezANOVA(
        data = cue_averages %>%
          filter(condition %in% ph_contr) %>%
          droplevels(),
        dv = .dv,
        wid = cue,
        within = condition,
        between = type
      ), list(.dv = dv)))
    }, dv = dv)
  )
}, cue_averages = davg)

dplot <- filter_participants |>
  filter(condition %in% c("peer", "short", "child")) |>
  filter(!is.na(response)) |>
  mutate(
    condition = factor(condition, levels = c("child", "short", "peer")),
    cue = as.factor(cue),
    type = as.factor(type),
    revision = if_else(is.na(revision), response, revision),
    Nletters = nchar(revision)
  ) |>
  group_by(cue, condition, type) |>
  summarize(across(c(Lg10WF, Lg10CD, aoa, Nletters, Nsyll), function(x) mean(x, na.rm = TRUE))) |>
  pivot_longer(cols = Lg10WF:Nsyll, names_to = "metric", values_to = "value") |>
  group_by(metric, type, cue) |>
  summarize(
    pc = mean(value[condition == "peer"]) - mean(value[condition == "child"]),
    ps = mean(value[condition == "peer"]) - mean(value[condition == "short"]),
    sc = mean(value[condition == "short"]) - mean(value[condition == "child"])
  ) |>
  group_by(metric, type) |>
  summarize(across(c(pc, ps, sc), list(m= ~ mean(.x, na.rm = T), s = ~ sd(.x, na.rm = T), se =  ~ sd(.x, na.rm = T) / sqrt(60)))) |>
  pivot_longer(cols = pc_m:sc_se, names_to = c("condition_contrast", "stat"), names_sep = "_", values_to = "value") |>
  pivot_wider(id_cols = c(metric, type, condition_contrast), names_from = stat, values_from = value) |>
  ungroup()

dplot |>
  filter(metric != "Nsyll") |>
  droplevels() |>
  ggplot(aes(x = condition_contrast, y = m, color = type)) +
  geom_pointrange(aes(ymin = m - (se*2), ymax = m + (se*2)), position = position_dodge(.7)) +
  facet_wrap(vars(metric))

dplotBar <- filter_participants |>
  filter(condition %in% c("peer", "short", "child")) |>
  filter(!is.na(response)) |>
  mutate(
    condition = factor(condition, levels = c("child", "short", "peer")),
    cue = as.factor(cue),
    type = as.factor(type),
    revision = if_else(is.na(revision), response, revision),
    Nletters = nchar(revision)
  ) |>
  group_by(cue, condition, type) |>
  summarize(across(c(Lg10WF, Lg10CD, aoa, Nletters, Nsyll), function(x) mean(x, na.rm = TRUE))) |>
  pivot_longer(cols = Lg10WF:Nsyll, names_to = "metric", values_to = "value") |>
  group_by(type, condition, metric) |>
  summarize(across(c(value), list(m= ~ mean(.x, na.rm = T), s = ~ sd(.x, na.rm = T), se =  ~ sd(.x, na.rm = T) / sqrt(60)))) |>
  pivot_longer(cols = pc_m:sc_se, names_to = c("condition_contrast", "stat"), names_sep = "_", values_to = "value") |>
  pivot_wider(id_cols = c(metric, type, condition_contrast), names_from = stat, values_from = value) |>
  ungroup()

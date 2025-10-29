load("psychling/responses_metadata_2025-04-26_reformatted.Rdata") 
library(dplyr)
library(tidyr)
library(ggplot2)
library(ez)
library(purrr)
library(lme4)
library(lmerTest)
library(statmod)
library(fitdistrplus)

## Remove outliers and convert rt to miliseconds ####################
filter_participants <- combined_meta %>%
  filter(!participant %in% c("TTA_067","TTA_068")) %>%
  mutate(rt = rt *1000)


## Filter out responses faster than 250ms and responses that are more than 
## 2.5 standard deviations away from participant mean ########################
glmer_df <- filter_participants |>
  dplyr::select(participant,condition,rt,cue) |>
  filter(rt > 250) |>
  group_by(participant) |>
  mutate(z_rt_pp = (rt - mean(rt))/sd(rt))|>
  filter(z_rt_pp < 2.5) |>
  ungroup() |>
  mutate(
    participant = as.factor(participant),
    condition = factor(condition, c("child", "peer", "short", "creative")),
    cue = factor(cue)
  ) |>
  left_join(combined_meta %>% dplyr::select(cue,type,strength_strat) %>% unique, by = "cue")
  

## Fit linear mixed model with fixed effect of condition and random intercepts for cues ###
glmer_fit <- glmer(
  rt ~ condition + (1 | cue),
  data = glmer_df,
  family = inverse.gaussian("identity")
)

summary(glmer_fit)



## group means for plot may 2025
glmer_df %>%
  group_by(condition) %>%
  get_summary_stats(rt, type = c('mean_sd')) %>%
  View()


## Fit model with association strength and word-type (co or non-co) strata
glmer_df_type <- glmer_df %>%
  left_join(combined_meta %>% select(cue, type,strength_strat) %>% unique(), by = "cue")

glmer_fit_wordtype <- glmer(
  rt ~ condition * type * strength_strat + (strength_strat:type|participant) + (1 | cue),
  data = glmer_df_type,
  family = inverse.gaussian("identity")
)
  

summary(glmer_fit_wordtype)


## Contrasts #######################################
contrasts(glmer_df$condition)
model.matrix(glmer_fit)


## Plotting model fit ##############################
q <- list(
  invgauss = fitdist(glmer_df$rt, distr = "invgauss",
                     start = list(mean=700, dispersion=300, shape=1)),
  gamma = fitdist(glmer_df$rt, dist = "gamma"),
  normal = fitdist(glmer_df$rt, dist = "norm")
)
ix <- seq(250, 10000, length.out = 1000)

plot(density(glmer_df$rt), lwd = 3, main = NA)
points(
  ix,
  dinvgauss(
    ix,
    mean = q$invgauss$estimate["mean"],
    shape = q$invgauss$estimate["shape"],
    dispersion = q$invgauss$estimate["dispersion"]
  ),
  col = "red", type = "l", lwd = 3
)
points(
  ix,
  dgamma(
    ix,
    shape = q$gamma$estimate["shape"],
    rate = q$gamma$estimate["rate"]
  ),
  col = "blue", type = "l", lwd = 3
)
points(
  ix,
  dnorm(
    ix,
    mean = q$normal$estimate["mean"],
    sd = q$normal$estimate["sd"]
  ),
  col = "green", type = "l", lwd = 3
)
legend(
  "topright",
  legend = c("estimated density", "inverse Gaussian", "gamma", "normal"),
  col = c("black", "red", "blue", "green"),
  lwd = 3
)


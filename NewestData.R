load("psychling/responses_metadata_2025-03-28_reformatted.Rdata")
library(dplyr)
library(tidyr)
library(ggplot2)
library(ez)
library(purrr)
library(lme4)
library(lmerTest)
library(statmod)

## No paggplot2## No participants are 3SDs outside of their condition's mean. But,
## some participants may be driving condition means up. 
combined_meta %>%
  group_by(condition) %>%
  reframe(participant = participant,
            rt = rt,
            m_cond = mean(rt),
            sd_cond = sd(rt)) %>%
  ungroup() %>%
  group_by(participant) %>%
  mutate(m_pp = mean(rt),
         z_pp_cond = (mean(rt)-m_cond)/sd_cond) %>%
  group_by(condition) %>%
  reframe(sum(z_pp_cond >= 3))

## Look at extreme responses across all participants.
filter_responses <- combined_meta %>%
  mutate(rt = rt *1000) %>%
  filter(rt > 150) %>%
  mutate(z_rt = (rt - mean(rt))/sd(rt),.after = rt,
         is.extreme = ifelse(z_rt >= 3, TRUE,FALSE))
##  67 and 68 are especially bad
filter_responses %>%
  group_by(participant) %>%
  summarize(sum(is.extreme)) %>%
  View()

filter_participants <- combined_meta %>%
  filter(!participant %in% c("TTA_067","TTA_068")) %>%
  mutate(rt = rt *1000) %>%
  filter(rt > 150) 


splt_cond <- split(filter_participants,filter_participants$condition)
keep_pps <- map_dfr(splt_cond, function(x){
  sample(x$participant, size = 30)
}) %>%
  pivot_longer(cols = everything(),
               names_to = "condition",
               values_to = "participant")

## ANOVA Response time
anova_df <- filter_participants %>%
  select(participant,condition,rt,cue) %>%
  filter(participant %in% keep_pps$participant) %>%
  group_by(participant,condition)%>%
  summarize(med_rt = median(rt)) %>%
  group_by(condition) |>
  mutate(med_rt_z = (med_rt - mean(med_rt)) / sd(med_rt))

  filter(med_rt < 2000)

  glmer_df <- filter_participants %>%
    select(participant,condition,rt,cue) %>%
    filter(rt > 250) |>
    group_by(participant) %>%
    mutate(z_rt_pp = (rt - mean(rt))/sd(rt)) %>%
    filter(z_rt_pp < 2.5) |>
    ungroup() |>
    mutate(
      participant = as.factor(participant),
      condition = factor(condition, c("child", "peer", "short", "creative")),
      cue = factor(cue)
    )
  
    group_by(participant,condition)%>%
    summarize(med_rt = median(rt)) %>%
    group_by(condition) |>
    mutate(med_rt_z = (med_rt - mean(med_rt)) / sd(med_rt))
  
anova_df <- filter_participants %>%
  select(participant,condition,rt,cue) %>%
  filter(participant %in% keep_pps$participant) %>%
  group_by(participant,condition)%>%
  summarize(med_rt = median(rt)) %>%
  filter(med_rt > 2000) %>%
  group_by(condition) %>%
  count()

m <- ezANOVA(anova_df,
             dv = rt,
             wid = participant,
             within = .(cue),
             between = condition,)


ggplot(anova_df |> filter(condition == "child"), aes(x = condition, y = rt, color = participant))+
  geom_boxplot()
  geom_point(position = "jitter")


str(anova_df)


## Use this going forward.
glmer_fit <- glmer(
  rt ~ condition + (1 | cue),
  data = glmer_df,
  family = inverse.gaussian("identity")
)

summary(glmer_fit)


glmer_df_type <- glmer_df %>%
  left_join(combined_meta %>% select(cue, type,strength_strat) %>% unique(), by = "cue")

glmer_fit_wordtype <- glmer(
  rt ~ condition * type * strength_strat + (strength_strat:type|participant) + (1 | cue),
  data = glmer_df_type,
  family = inverse.gaussian("identity")
)
  

summary(glmer_fit_wordtype)


library(fitdistrplus)

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

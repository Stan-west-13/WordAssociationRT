library(dplyr)
library(tidyr)
library(ggplot2)
library(ez)
library(purrr)
library(lme4)
library(lmerTest)
library(statmod)
library(fitdistrplus)
library(rstatix)
library(codingMatrices)
library(ggsignif)
source("R/Load_Helpers.R")

## Load data
d <- load_most_recent_by_mtime("data", "TTA_response_mapped_meta-")


## Filter out responses faster than 250ms and responses that are more than 
## 2 standard deviations away from participant mean, and participants
## 2 standard deviations away from condition grand means ########################
filter_participants <- d %>%
  filter(cue_rt_mili > 250,!participant == "TTA_067",!participant == "TTA_068") |>
  filter(!nchar == 0) |>
  group_by(participant) |>
  mutate(z_rt_pp = (cue_rt_mili - mean(cue_rt_mili))/sd(cue_rt_mili))|>
  filter(z_rt_pp < 2) |>
  ungroup() |>
  mutate(
    participant = as.factor(participant),
    condition = factor(condition, c("child", "peer", "short", "creative")),
    condition_diff = condition,
    cue = factor(cue)
  )

saveRDS(filter_participants, file = paste0("data/TTA_response_mapped_meta_filtered-",Sys.Date(),".rds"))

## Add column for difference contrasted conditions
contrasts(filter_participants$condition_diff) <- code_diff(4)


## Fit linear mixed model with fixed effect of condition and random intercepts for cues and participant ###
glmer_fit <- glmer(
  cue_rt_mili ~ condition + (1 | cue) + (1|participant),
  data = filter_participants,
  family = inverse.gaussian("identity")
)

summary(glmer_fit)

## Fit linear mixed model above, but with difference contrasts.
glmer_fit_diff <- glmer(
  cue_rt_mili ~ condition_diff + (1| cue) + (1|participant),
  data = filter_participants,
  family = inverse.gaussian("identity")
)

summary(glmer_fit_diff)

## group means and sd
filter_participants %>%
  group_by(condition) %>%
  get_summary_stats(cue_rt_mili, type = c('mean_sd')) 


## Plot averages with standard error bars
plot_glmer <- filter_participants %>%
  group_by(condition) %>%
  summarize(mean = mean(cue_rt_mili),
            n = length(cue_rt_mili),
            se = sd(cue_rt_mili)/sqrt(length(cue_rt_mili))) %>%
  mutate(condition = factor(condition, 
                            levels = c("peer",
                                       "child",
                                       "short",
                                       "creative")))
## Wide plot
ggplot(plot_glmer, aes(x = condition, y = mean,fill=condition))+
  geom_col()+
  geom_errorbar(aes(ymin = mean-se, ymax =mean+se),width = 0.1, linewidth = 0.75)+
  scale_fill_manual(values = c("#AEAEAE","#f0b400", "#9C8BFF", "#4A248E"))+
  coord_cartesian(ylim = c(0, 2750))+
  theme_bw(base_size = 32)+
  geom_text(stat = "summary",fun = "mean",vjust = 12, aes(label = round(after_stat(y),2)),
            position = position_dodge(0.9),size = 8)+
  labs(y = "mean response time (ms)",
       title = "Condition Response Times"
       )+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = "#FCFBFF"))+
  ggsignif::geom_signif(stat="signif",
                        position = "identity",
                        comparisons = list(c("peer","child"),
                                           c("child","short"),
                                           c("child","creative")),
                        annotations = c("***"),
                        textsize = 7,
                        tip_length = .1,
                        size = 1,
                        y_position = c(2100,2300,2600))
## Save plot
ggsave(filename = 'Figures/rt_plot_condition.png' ,width = 9.5, height = 7.5, dpi = 600, units = "in", device='png')


## Long plot
ggplot(plot_glmer, aes(x = condition, y = mean,fill=condition))+
  geom_col()+
  geom_errorbar(aes(ymin = mean-se, ymax =mean+se),width = 0.1, linewidth = 0.75)+
  scale_fill_manual(values = c("#AEAEAE","#f0b400", "#9C8BFF", "#4A248E"))+
  coord_cartesian(ylim = c(0, 2750))+
  theme_bw(base_size = 32)+
  labs(y = "mean response time (ms)",
       title = "Condition Response Times"
  )+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.background = element_blank(),
        panel.background = element_blank())+
  ggsignif::geom_signif(stat="signif",
                        position = "identity",
                        comparisons = list(c("peer","child"),
                                           c("child","short"),
                                           c("child","creative")),
                        annotations = c("***"),
                        textsize = 7,
                        tip_length = .1,
                        size = 1,
                        y_position = c(2100,2300,2600))

ggsave(filename = 'Figures/rt_plot_condition_lg.png' ,width = 9.27, height = 10.42, dpi = 600, units = "in", device='png')


## Confidence interval plot
ggplot(plot_glmer, aes(x = condition, y = mean,fill=condition))+
  geom_col()+
  geom_errorbar(aes(ymin = mean-1.96*se, ymax =mean+1.96*se),width = 0.1, linewidth = 0.75)+
  scale_fill_manual(values = c("#AEAEAE","#f0b400", "#9C8BFF", "#4A248E"))+
  coord_cartesian(ylim = c(0, 2750))+
  theme_bw(base_size = 32)+
  labs(y = "mean response time (ms)",
       title = "Condition Response Times"
  )+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.background = element_blank(),
        panel.background = element_blank())+
  ggsignif::geom_signif(stat="signif",
                        position = "identity",
                        comparisons = list(c("peer","child"),
                                           c("child","short"),
                                           c("child","creative")),
                        annotations = c("***"),
                        textsize = 7,
                        tip_length = .1,
                        size = 1,
                        y_position = c(2100,2300,2600))

ggsave(filename = 'Figures/rt_plot_condition_lg_confint.png' ,width = 9.27, height = 10.42, dpi = 600, units = "in", device='png')

## Contrasts #######################################
contrasts(filter_participants$condition)
model.matrix(glmer_fit)


## Plotting model fit ##############################
q <- list(
  invgauss = fitdist(filter_participants$cue_rt_mili, distr = "invgauss",
                     start = list(mean=700, dispersion=300, shape=1)),
  #gamma = fitdist(filter_participants$cue_rt_mili, dist = "gamma"),
  normal = fitdist(filter_participants$cue_rt_mili, dist = "norm")
)
ix <- seq(250, 10000, length.out = 1000)

plot(density(filter_participants$cue_rt_mili), lwd = 3, main = NA)
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
# points(
#   ix,
#   dgamma(
#     ix,
#     shape = q$gamma$estimate["shape"],
#     rate = q$gamma$estimate["rate"]
#   ),
#   col = "blue", type = "l", lwd = 3
# )
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


## Factorizing conditions into rule vs context. 
plot_df <- filter_participants %>%
  mutate(plot_fac = ifelse(condition == "child" | condition == "peer", "context", "rule"))

ggplot(plot_df ,aes(x = plot_fac, y = cue_rt_mili, fill = condition))+
  stat_summary(position = "dodge", geom = "bar", fun = "mean") +
  labs(title = "Response Time by Context Condition",
       y = "response time",
       x = "condition")+
  theme_bw()+
  theme(legend.position = "none")
  

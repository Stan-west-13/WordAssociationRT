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
library(rstatix)
library(ggsignif)
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
  left_join(combined_meta %>%  dplyr::select(cue,type,strength_strat) %>% unique, by = "cue") %>%
  mutate(type = as.factor(type),
         strength_strat = as.factor(strength_strat))

  

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

summary_rt <- glmer_df %>% 
  group_by(condition) %>% 
  summarize(
    m = mean(rt),
    s = sd(rt),
    n = n(),
    se = s/sqrt(n())
  ) %>% 
  View()


## Fit model with association strength and word-type (co or non-co) strata

glmer_fit_wordtype <- glmer(
  rt ~ condition * type * strength_strat  + (1 | cue),
  data = glmer_df,
  family = inverse.gaussian("identity")
)
  
summary(glmer_fit_wordtype)


plot_glmer <- glmer_df %>%
  group_by(condition) %>%
  summarize(mean = mean(rt),
            n = length(rt),
            se = sd(rt)/sqrt(length(rt))) %>%
  mutate(condition = factor(condition, 
                            levels = c("peer",
                                       "child",
                                       "short",
                                       "creative")))

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

ggsave(filename = 'Figures/rt_plot_condition.png' ,width = 9.5, height = 7.5, dpi = 600, units = "in", device='png')




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

ggsave(filename = 'Figures/rt_plot_condition_lg.png' ,width = 9.27, height = 11.42, dpi = 600, units = "in", device='png')






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

ggsave(filename = 'Figures/rt_plot_condition_lg_confint.png' ,width = 9.27, height = 11.42, dpi = 600, units = "in", device='png')

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



plot_df <- glmer_df %>%
  mutate(plot_fac = ifelse(condition == "child" | condition == "peer", "context", "rule"))

ggplot(plot_df ,aes(x = plot_fac, y = rt, fill = condition))+
  stat_summary(position = "dodge", geom = "bar", fun = "mean") +
  labs(title = "Response Time by Context Condition",
       y = "response time",
       x = "condition")+
  theme_bw()+
  theme(legend.position = "none")
  

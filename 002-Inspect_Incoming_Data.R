load("psychling/responses_metadata_2025-03-28.Rdata")
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(ez)

library(pastecs); library(psych); library(tidyverse); library(rstatix)


## Running count by condition

combined_meta %>%
  select(participant, condition) %>%
  unique() %>%
  group_by(condition) %>%
  summarize(n())


## Check completness
## By participant
unknown_plot <- combined_meta %>%
   group_by(participant) %>%
   summarise(unknown = sum(response == "")) %>%
   arrange(desc(unknown))
ggplot(unknown_plot,aes(x = unknown)) +
   geom_histogram() +
   labs(y = "Participant Count", x = "# unknown")
#ggsave(filename = "Figures/Unknown_cues.png")

## By condition
unknown_plot_by_cond <-combined_meta %>%
   group_by(condition) %>%
   summarise(unknown = sum(response == "")) %>%
   arrange(desc(unknown))

ggplot(unknown_plot_by_cond, aes(x = condition, y = unknown, fill = condition))+
   geom_col()
#ggsave("Figures/unknown_by_condition.png")

## By word-type
unknown_wordtype <- combined_meta %>%
   group_by(strength_strat, type,condition) %>%
   summarize(unknown = sum(response == "")) %>%
   arrange(desc(unknown))

ggplot(unknown_wordtype, aes(x = strength_strat, y = unknown, fill = type))+
   geom_col(position = "dodge")+
   facet_wrap(~condition)
#ggsave('Figures/unknown_by_wordtypecond.png')

## By word
unknown_words <- combined_meta %>%
   group_by(type, cue, strength_strat) %>%
   summarize(unknown = sum(response == "")) %>%
   filter(unknown > 0) %>%
   arrange(desc(unknown))

ggplot(unknown_words, aes(x = unknown, y = cue, fill = type))+
   geom_col(position = 'dodge',aes( alpha= strength_strat))+
   scale_alpha_manual(values=c(1, 0.45))

#ggsave("Figures/unknown_words.png")

## Plot rt

## Remove participants with high means
combined_meta_rm_pp <- combined_meta %>%

## Filter responses quicker than 150 ms and 3 stadard deviaions from participant means
meta_z <- combined_meta %>%
   filter(!response == "") %>%
   mutate(rt_mili = key_resp_cue.rt * 1000) %>%
   filter(rt_mili > 150) %>%
   group_by(participant) %>%
   mutate(rt_z = (rt_mili - mean(rt_mili))/sd(rt_mili)) %>%
   filter(rt_z < 3) %>%
   ungroup()

text_df <- combined_meta %>%
   filter(!response == "") %>%
   mutate(rt_mili = key_resp_cue.rt * 1000) %>%
   filter(rt_mili > 150) %>%
   group_by(strength_strat,type,condition) %>%
   mutate(rt_z = (rt_mili - mean(rt_mili))/sd(rt_mili)) %>%
   filter(rt_z < 1) %>%
   summarize(rt_mili = mean(rt_mili)) %>%
   unique() %>%
   ungroup()

ggplot(meta_z, aes(x = condition, y = rt_mili, fill = condition)) +
    geom_violin() +
    geom_boxplot(fill = "grey", width = 0.15)+
    facet_wrap(~type + strength_strat)
ggsave("Figures/rt_dist_condition_type.png")
   
        
ggplot(meta_z, aes(x = condition, y = rt_mili, fill = condition)) +
    geom_violin() +
    geom_boxplot(fill = "grey", width = 0.15)
#ggsave("Figures/rt_dist_condition.png")

ggplot(meta_z, aes(x = condition, y = rt_mili, fill = condition))+
   geom_bar(stat = "summary",fun = "mean")+
   facet_wrap(~strength_strat+type, scales = "fixed")+
   geom_text(data = text_df, aes(label = round(rt_mili,2),vjust =0))+
   scale_y_continuous(breaks = seq(0,2500,250))
#ggsave("Figures/rt_bar_means_condition_type.png")


meta_z %>%
   group_by(condition, strength_strat, type) %>%
   summarize(mean_rt = mean(rt_mili))


## Long_psycholing
meta_long <- pivot_longer(combined_meta,
                          cols = c("Lg10WF","AoA_Kup_lem","Nphon","Nsyll","Nletters"),
                          names_to = "metric",
                          values_to = "value")


text_psycholing <- meta_long %>% 
   group_by(metric, condition) %>%
   summarize(value = mean(value,na.rm = T))


ggplot(meta_long, aes(x = condition, y = value, color = condition))+
   geom_violin()+
   geom_boxplot(fill = "grey", width = 0.25)+
   facet_wrap(~metric, scales = 'free')
#ggsave("Figures/psycholing_dist_condition_type.png")

ggplot(meta_long, aes(x = condition, y = value, fill = condition))+
   geom_bar(stat = "summary", fun = "mean")+
   facet_wrap(~metric, scales = 'free')+
   geom_text(data = text_psycholing, aes(label = round(value, 2)),vjust=0)
#ggsave("Figures/psycholing_bar_mean_condition.png")



meta_long %>%
   group_by(condition, metric) %>%
   summarize(m = mean(value, na.rm = T))


m <- ezANOVA(data = combined_meta,
             dv = key_resp_cue.rt,
             within = c(type,strength_strat,cue),
             between = condition,
             wid = participant)

##---------------------------
## Analysis RT
##---------------------------

## Descriptives for RT

describe(combined_meta$rt)

rt_raw_dis <- ggplot(combined_meta, aes(x=rt)) + geom_histogram()
rt_raw_dis

## Mixed ANOVA (condition and cue) on RT



## Psycholing characteristics of responses by condition




## Notes from meeting: 

# 4 levels (conditions) between-subjects on RT 
# cues are within

#cue by condition mixed anova on RT

#looking at how people responses look (are people generally making responses 
#that are child oreinted in child context and it is meaningfully different frmo other conditions)

# and sepearte anovas for psychling factors



library(dplyr)
library(tidyr)
library(ggplot2)
library(ez)
library(purrr)
library(lme4)
library(rstatix)
library(lmerTest)
library(statmod)
library(fitdistrplus)
library(codingMatrices)
library(stringr)
source("R/Load_Helpers.R")

z <- function(x){
  return((x - mean(x,na.rm = T))/sd(x,na.rm = T))
}


d <- load_most_recent_by_mtime("data","filtered")  %>%
  filter(!participant == "TTA_061",!participant == "TTA_100",!condition == "creative") %>%
  mutate(nchar = log10(nchar),
         wf_z = z(Lg10WF),
         aoa_z = z(aoa),
         wl_z = z(nchar),
         cd_z = z(Lg10CD)) %>%
  mutate(condition = relevel(condition,ref = "peer")) 
## Plotting measures
plot_df <- d %>%
  pivot_longer(cols = c("aoa", "Lg10WF", "Lg10CD","nchar","Nsyll"),
               names_to = "measure",
               values_to = "value") %>% 
  relocate(c(measure, value), .after = condition)

ggplot(plot_df, aes(x = condition, y = value, fill = type))+
  geom_bar(stat = "summary", fun = "mean", position = "dodge")+
  facet_wrap(~measure,scales = 'free')

## Looking at descriptive stats by condition for each measure
plot_df %>% 
  group_by(condition, measure) %>% 
  get_summary_stats(value, type = "common")



## Analysis

## Compute averages
cue_averages <- d %>%
  group_by(cue, condition) %>%
  summarize(across(c(Lg10WF, Lg10CD, aoa, nchar, Nsyll), function(x) mean(x, na.rm = TRUE))) %>%
  ungroup()

## Setup mapping lists
davg <- d |>
  filter(condition %in% c("peer", "short", "child")) |>
  filter(!is.na(response)) |>
  mutate(
    condition = factor(condition, levels = c("child", "short", "peer")),
    cue = as.factor(cue),
  ) |>
  group_by(cue, condition) |>
  summarize(across(c(Lg10WF, Lg10CD, aoa, nchar, Nsyll), function(x) mean(x, na.rm = TRUE))) |>
  ungroup()

dependent_variables <- c("Lg10WF", "Lg10CD", "aoa", "nchar", "Nsyll")
names(dependent_variables) <- dependent_variables
posthoc_contrasts <- list(pc = c("child", "peer"),
                          cs = c("child", "short"),
                          ps = c("peer", "short"))
## Run ANOVAs
anovas <- map(dependent_variables, function(dv,cue_averages) {
  list(
    full_model = eval(substitute(
      ezANOVA(
        data = cue_averages,
        dv = .dv,
        wid = cue,
        within = condition
      ), list(.dv = dv))),
    posthocs = map(posthoc_contrasts, function(ph_contr, dv) {
      eval(substitute(ezANOVA(
        data = cue_averages %>%
          filter(condition %in% ph_contr) %>%
          droplevels(),
        dv = .dv,
        wid = cue,
        within = condition
      ), list(.dv = dv)))
    }, dv = dv)
  )
},cue_averages = davg)

## Averages by metric wide formatted
d_avg_met <- d |>
  dplyr::select(condition, cue, aoa, Lg10WF, Lg10CD, nchar) |>
  tidyr::pivot_longer(cols = aoa:nchar, names_to = "metric", values_to = "value") |>
  dplyr::group_by(condition, cue, metric) |>
  dplyr::summarize(m = mean(value, na.rm = TRUE)) |>
  tidyr::pivot_wider(id_cols = c(metric, cue), names_from = condition, values_from = m)

## Grouping by metric for t-tests
d_avg_grouped <- group_by(d_avg_met, metric)

## Run t-tests over contrasts
ttests <- d_avg_grouped |>
  group_split() |>
  map(~{
    list(
      child_peer = t.test(.x$child, .x$peer, paired = TRUE),
      child_short = t.test(.x$child, .x$short, paired = TRUE),
      peer_short = t.test(.x$peer, .x$short, paired = TRUE)
    )
  })
## Give names to t-test list
names(ttests) <- group_keys(d_avg_grouped)$metric

## Linear mixed models 
## Split dataframe along measure and normalized vs non-normalized dv
d_long_filt_normalized <- d %>%
  dplyr::select(participant,
         cue,
         condition,
         type,
         aoa_z,
         wf_z,
         cd_z,
         wl_z) %>%
  pivot_longer(cols = ends_with("_z"),
               names_to = "measure",
               values_to = "value") %>%
  drop_na() %>%
  mutate(type = as.factor(type))

d_long_filt_nonnormalized <- d %>%
  dplyr::select(participant,
         cue,
         condition,
         type,
         aoa,
         Lg10WF,
         Lg10CD,
         nchar) %>%
  pivot_longer(cols = c("aoa",starts_with("Lg"),"nchar"),
               names_to = "measure",
               values_to = "value") %>%
  drop_na() %>%
  mutate(type = as.factor(type))

lst_mods <- list(normalized = d_long_filt_normalized, nonnormal = d_long_filt_nonnormalized)

## Split into lists for mapping analysis
d_split <- map(lst_mods, function(x){
  split(x,x$measure)
})


## Run LMER, plot interaction plots and bar plots.
mods <- imap(d_split, function(y,name){
  map(y, function(x){
    sum_stats <- x %>%
<<<<<<< HEAD
      group_by(condition) %>%
=======
      group_by(condition,type) %>%
>>>>>>> 499c0bd660053c2f49d26169e10802308c3e80f5
      get_summary_stats(value, type = c("mean_ci"))
    ## random intercepts for participants and cue
    m_lmer <- lmer(value ~ condition*type + (1|cue) + (1|participant), data = x) 
    print(paste("############## Model output for ", unique(x$measure),name,"########################"))
    print(summary(m_lmer))
    
    contrasts(x$condition) <- code_diff(4)
    m_lmer_diff <- lmer(value ~ condition*type  + (1|cue) + (1|participant), data = x) 
    print(paste("############## Model output for ", unique(x$measure),name," DIFF ","########################"))
    print(summary(m_lmer_diff))
  
    
    g <- ggplot(sum_stats, aes(x = condition, y = mean, fill = type))+
      stat_summary(fun = "identity", geom = "col", position = "dodge")+
<<<<<<< HEAD
      geom_errorbar(aes(ymin = mean -ci, ymax = mean+ci,width = 0.2), position = "dodge")+
=======
      geom_errorbar(aes(ymin = mean -ci, ymax = mean+ci,width = 0.2), position = position_dodge(0.9))+
>>>>>>> 499c0bd660053c2f49d26169e10802308c3e80f5
      ggtitle(paste0("Barplot by Context ",unique(x$measure)))+
      theme_classic()
    
    return(list(model = m_lmer,model_diff = m_lmer_diff,plot(g)))
  })
})





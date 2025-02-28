library(dplyr)
library(tidyr)
library(purrr)
library(readxl)
library(ggplot2)
library(geomtextpath)
## Load functions
combine_files <- function(files){
   dataf <- data.frame()
   for (file in files){
    file_csv <- list.files(file)
    df <- read.csv(paste0(file,"/",file_csv[1]))[-1,]
    dataf <- rbind(dataf,df)
   }
   return(dataf)
}

## Unzip data
unzip("data.zip")
sub <- read.csv("psychling/SUBTLEXusfrequencyabove1.csv")
aoa <- read.csv("psychling/AoA_51715_words.csv")
words <- read.csv("psychling/stim_64_NNVB_wcuss.csv")[,-1]
familiarity <- read_xlsx("psychling/13428_2018_1077_MOESM2_ESM.xlsx")[,-6]
## Combine data over participants
pp_files <- list.files("data",full.names = TRUE)
combined <- combine_files(pp_files)[,-25]

combined_meta <- combined %>%
   left_join(sub, by = c("response" = "Word")) %>%
   left_join(aoa, by = c("response" = "Word")) %>%
   left_join(words, by = "cue") %>%
   left_join(familiarity, by = c("cue" = "Word"))
save(combined_meta, file = paste0("psychling/","responses_metadata_",Sys.Date(),".Rdata"))

## Running count
combined_meta %>%
   select(condition,participant ) %>%
   unique() %>%
   group_by(condition) %>%
   summarize(n = n())


## Z-score response time based on participant mean and sd
combined_meta <- combined_meta %>%
   group_by(participant) %>%
   mutate(rt_mili = key_resp_cue.rt * 1000,
          sd_rt_mili = sd(rt_mili),
          z_rt = (rt_mili - mean(rt_mili))/sd(rt_mili)) %>%
   ungroup() %>%
   mutate(missing = ifelse(response == "", TRUE,FALSE))

## Plot distribution of missing values across response times.
## Majority of missing responses centered around the mean response times for
## individuals. 
ggplot(combined_meta %>% filter(missing) , aes(x = z_rt, fill = missing))+
   geom_density(alpha = 0.4) +
   facet_wrap(~condition)


x <- combined_meta %>%
   filter(missing) %>%
   group_by(cue) %>%
   mutate(sum_missing  = sum(missing),
          mean_rt_z = mean(z_rt)) %>%
   arrange(mean_rt_z) %>%
   select(cue, sum_missing, mean_rt_z,z_rt, Prevalence) %>%
   unique() %>%
   pivot_longer(cols = c("z_rt", "Prevalence"),
                names_to ="metric",
                values_to = "value")
View(x)

nms <- x %>%
   select(cue, sum_missing) %>%
   unique()
## By responses missing more than once, what are the distribution
## of participant-wise response times?
ggplot(x %>% filter(sum_missing >2), aes(x = value, color = metric)) +
   geom_density(stat="density") +
   #geom_textdensity(aes(label = sum_missing),hjust = "ymax",vjust = -0.5)+
   facet_wrap(~cue)+
   scale_x_continuous(breaks = seq(-1,4,by = 1))
   


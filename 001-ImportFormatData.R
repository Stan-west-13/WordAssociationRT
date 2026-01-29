library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readxl)
library(ggplot2)
library(geomtextpath)
## Load functions
combine_files <- function(files){
   dataf <- data.frame() #creates empty data frame we will iteratively add to
   
   for (file in files){ #iterates over each participant file in the participant data folder 
     if (any(str_detect(list.files(file), ".xlsx"))){ #if a participant file already has a .xlsx file...
       file_xl <- list.files(file, pattern = "*.xlsx") #'*.xlsx' identifies files with .xlsx and adds them to list (but I do have a question about \* since it says it finds it 0 or more times) 
       df <- read_xlsx(paste0(file,"/",file_xl[1]))[-1,] #this uses paste0 to get the path of the file we are on, the [1] indexes the 1st item in the vector, and [-1,] removes the first row
       dataf <- rbind(dataf,df) #add rows of df to data frame
     }
      else {
        file_csv <- list.files(file,pattern = "*.csv")
        df <- read.csv(paste0(file,"/",file_csv[1]))[-1,-25]
        dataf <- rbind(dataf,df) #add rows of df to data frame
      }
     
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
combined <- combine_files(pp_files)

combined_meta <- combined %>%
   left_join(sub, by = c("response" = "Word")) %>%
   left_join(aoa, by = c("response" = "Word")) %>%
   left_join(words, by = "cue") %>%
   left_join(familiarity, by = c("cue" = "Word"))


load("psychling/responses_metadata_2025-03-28.Rdata")

## Code for renaming and removing columns
combined_meta <- combined_meta %>%
  subset(select = -c(trials.thisRepN, trials.thisTrialN, trials.thisN, 
                     trials.thisIndex, thisRow.t, notes, key_resp_cue.started, 
                     cue_pres_trial.started)) %>%
  rename(c(cue_offset = textPrompt.stopped, key = key_resp_cue.keys, rt = key_resp_cue.rt)) %>%
  relocate("participant") %>%
  relocate(c("date", "expName", "psychopyVersion", "frameRate", "expStart"), .after = "Pknown")

save(combined_meta, file = paste0("psychling/","responses_metadata_",Sys.Date(),"_reformatted.Rdata"))

names(combined_meta)



## Running count
combined_meta %>%
   select(condition,participant ) %>%
   unique() %>%
   group_by(condition) %>%
   summarize(n = n())


## Z-score response time based on participant mean and sd
combined_meta <- combined_meta %>%
   group_by(participant) %>%
   mutate(rt_mili = rt * 1000,
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
   


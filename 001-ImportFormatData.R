library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(readxl)
library(ggplot2)
library(geomtextpath)
source("R/Load_Helpers.R")
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

## Choose data directory - OneDrive: Word Association RT study/data
data_source <- choose_directory()

## Combine data over participants
pp_files <- list.files(data_source,full.names = TRUE)
stim <- read.csv("psychling/stim_64_NNVB_wcuss.csv")
combined <- combine_files(pp_files) %>%
  select(participant, date, condition, cue, response, cue_onset,cue_offset = textPrompt.stopped, cue_rt = key_resp_cue.rt, response.start, response.stop ) %>%
  mutate(cue_rt_mili = cue_rt * 1000) %>%
  left_join(select(stim, cue, strength_strat,type), by = "cue")

saveRDS(combined, file = paste0("data/TTA_combined_responses-",Sys.Date(),".rds"))

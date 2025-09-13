library(tidyverse)
library(purrr)

# Only run once:

# Read in list of valid combinations of 3 squares presented out of 8
og_combos_df <- read.csv("010-Excel_Sheet_Generate/exp_combos.csv")

# Read in csv of cue words and extract one cues for now
stim <- read.csv("010-Excel_Sheet_Generate/stim_64_NNVB.csv")|>
  select(cue,strength_strat,type)


# Create data frame of cues, conditions, and probability
cue_splits <- split(stim,list(stim$strength_strat,stim$type))

trial_type_probs_L <- data.frame(TT = c("match", "mismatch"), prob = c(0.5,0.5))
trial_type_probs_NL <- data.frame(TT = c("match", "mismatch"), prob = c(0.5,0.5))

## Create two stim sheets for each participant.
x <- map(c(1), function(x){ ## change values in c() for more participants
  combos <- og_combos_df ## load in load combos
  cues <- cue_splits ## load in cues split by association strength and type
  df <- data.frame(pp = rep(x, each = 64), sq1 = NA,sq2=NA,sq3=NA,sq4=NA,sq5=NA,sq6=NA,sq7=NA,sq8=NA) ## initailize dataframe with the participant id
  df$condition <- rep(c("load","no_load"), each = 32) ## add condition column
  df$TT <- 0 ## set a placeholder for trial type to use with the while loop
  df_split <- split(df,df$condition) ## split into load and no load
  df_split$load$cue <- c(sample(cues$high.co$cue,8),
                         sample(cues$low.co$cue,8),
                         sample(cues$high.nonco$cue,8),
                         sample(cues$low.nonco$cue,8)) ## sample 8 cues for load condition from each cue type.
  cues_nonselect <- map(cues, function(x){
    d <- x |>
      filter(!cue %in% df_split$load$cue)
    return(d)
  }) ## remove already sampled cues for no load condition
  df_split$no_load$cue <- c(cues_nonselect$high.co$cue,
                            cues_nonselect$low.co$cue,
                            cues_nonselect$high.nonco$cue,
                            cues_nonselect$low.nonco$cue) ## populate no load cues
  
  while(any(rle(df_split$load$TT)$lengths > 3) |any(rle(df_split$no_load$TT)$lengths > 3)){ ## This is saying while there are any runs > 3 in the condition column for a participant, keep running the for loop.
    for (i in 1:nrow(df_split$load)){
      df_split$load$TT[i] <- sample(trial_type_probs_L$TT, size = 1, prob = trial_type_probs_L$prob) ## add trial type
      df_split$load[i,2:9] <- slice_sample(combos[!combos %in% df_split$load[,2:9]], n = 1,replace = FALSE) ## add box locations
        if (df_split$load$TT[i] == "match"){ ## if that trial type is match,
          trial_type_probs_L$prob[trial_type_probs_L$TT == "match"] <- trial_type_probs_L$prob[trial_type_probs_L$TT == "match"] - (1/32) # subtract 1/32 from the match probability 
        }else{
          trial_type_probs_L$prob[trial_type_probs_L$TT == "mismatch"] <- trial_type_probs_L$prob[trial_type_probs_L$TT == "mismatch"] - (1/32) # otherwise subtract it from the mismatch prob
        }
    }
    trial_type_probs_NL$prob <- 0.5 ## set probability back to 0.5
    trial_type_probs_L$prob <- 0.5
    for (i in 1:nrow(df_split$no_load)){ ## add in trial type info for no load
      df_split$no_load$TT[i] <- sample(trial_type_probs_L$TT, size = 1, prob = trial_type_probs_L$prob)
      df_split$no_load[i,2:9] <- rep(1,8)
      if (df_split$no_load$TT[i] == "match"){ ## if that trial type is match,
        trial_type_probs_NL$prob[trial_type_probs_NL$TT == "match"] <- trial_type_probs_NL$prob[trial_type_probs_NL$TT == "match"] - (1/32) # subtract 1/32 from the match probability 
      }else{
        trial_type_probs_NL$prob[trial_type_probs_NL$TT == "mismatch"] <- trial_type_probs_NL$prob[trial_type_probs_NL$TT == "mismatch"] - (1/32) # otherwise subtract it from the mismatch prob
      }
    }
    trial_type_probs_NL$prob <- 0.5
    trial_type_probs_L$prob <- 0.5
  }
  
  dir.create(paste0("stim_files/TTA_",sprintf("%03d",unique(df_split$no_load$pp))))
  write.csv(df_split$load, paste0("stim_files/TTA_",sprintf("%03d",unique(df_split$load$pp)),"/TTA_",sprintf("%03d",unique(df_split$load$pp)),"_trial_list_L.csv" ),row.names = FALSE)
  write.csv(df_split$no_load, paste0("stim_files/TTA_",sprintf("%03d",unique(df_split$no_load$pp)),"/TTA_",sprintf("%03d",unique(df_split$no_load$pp)),"_trial_list_NL.csv" ),row.names = FALSE)## Add N and NL to end of trial list for load and no load.
  return(df_split)
})

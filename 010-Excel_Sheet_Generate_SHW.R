library(tidyverse)
library(readxl)
library(purrr)
library(progressr)
library(parallel)
##################################################################################################

# Only run once:

# Read in list of valid combinations of 3 squares presented out of 8
og_combos_df <- read_xlsx("010-Excel_Sheet_Generate/for_generating_sq_combo.xlsx")

# Read in csv of cue words and extract one cues for now
stim <- read.csv("010-Excel_Sheet_Generate/stim_64_NNVB.csv") |>
  select(cue)


# Create data frame of cues, conditions, and probability
cue_combos <- expand.grid(cue = stim$cue, condition = c("load","no_load"), 
                          trial_type = c("match","mismatch"),prob = 1 ) %>% 
  filter(!c(condition == "no_load" & trial_type == "mismatch")) %>% 
  arrange(cue)


trial_type_probs <- data.frame(TT = c("match", "mismatch"), prob = c(0.5,0.5))
condition_probs <- data.frame(condition = c("load","no_load"), prob = c(0.5,0.5))

## Map is like a loop, this is itterating over 20 integers (participants) and running the
## code defined in the anonymous function:
map(seq.int(1,2,1), function(x){
  df <- data.frame(pp = rep(x, 64)) ## initailize dataframe with the participant id
  df$cue <- sample(stim$cue, 64, replace = FALSE) ## randomly sample the 64 cues without replacement
  df$condition <- 0 ## set a placeholder for condition to use with the while loop
  while(any(rle(df$condition)$lengths > 3)){ ## This is saying while there are any runs > 3 in the condition column for a participant, keep running the for loop.
    for (i in 1:nrow(df)){
      df$condition[i] <- sample(condition_probs$condition,size = 1, prob = condition_probs$prob) # sample a condition one at a time with probabilities sampled from the dataframes on lines 23 & 24
        if (df$condition[i] == "load"){
          condition_probs$prob[condition_probs$condition == "load"] <- condition_probs$prob[condition_probs$condition == "load"] - (1/64) # if the current iteration is load, subtract 1/64
          df$TT[i]  <- sample(trial_type_probs$TT, size = 1, prob = trial_type_probs$prob) ## also sample a trial type for load
            if (df$TT[i] == "match"){ ## if that trial type is match,
              trial_type_probs$prob[trial_type_probs$TT == "match"] <- trial_type_probs$prob[trial_type_probs$TT == "match"] - (1/32) # subtract 1/32 from the match probability 
            }else{
              trial_type_probs$prob[trial_type_probs$TT == "mismatch"] <- trial_type_probs$prob[trial_type_probs$TT == "mismatch"] - (1/32) # otherwise subtract it from the mismatch prob
            }
        } else{
          condition_probs$prob[condition_probs$condition == "no_load"] <- condition_probs$prob[condition_probs$condition == "no_load"] - (1/64) ## where condition is no_load, subtract 1/64
          df$TT[i] <- "match" ## and assign it "match" because no_load only gets match
        }
      
    }
  condition_probs$prob <- 0.5 ## Finally reset the probabilities in case the for loop has to run multiple times for a participant
  trial_type_probs$prob <- 0.5
  }
  write.csv(df, paste0("test_psychopy/TTA_",sprintf("%03d",unique(df$pp)),"_square_combinations.csv" ),row.names = FALSE)
  return(df)
})


## Check that all lists have expected condition and trial type distributions

map(x,function(x){
  x |>
    group_by(condition, TT) |>
    summarize(n = length(cue))
})






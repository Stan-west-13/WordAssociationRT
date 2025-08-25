library(tidyverse)
library(readxl)
library(purrr)
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

map(seq.int(1,20,1), function(x){
  df <- data.frame(pp = rep(x, 64))
  df$cue <- sample(stim$cue, 64, replace = FALSE)
  df$condition <- 0
  while(any(rle(df$condition)$lengths > 3)){
    for (i in 1:nrow(df)){
      df$condition[i] <- sample(condition_probs$condition,size = 1, prob = condition_probs$prob)
        if (df$condition[i] == "load"){
          condition_probs$prob[condition_probs$condition == "load"] <- condition_probs$prob[condition_probs$condition == "load"] - (1/64)
          df$TT[i]  <- sample(trial_type_probs$TT, size = 1, prob = trial_type_probs$prob)
            if (df$TT[i] == "match"){
              trial_type_probs$prob[trial_type_probs$TT == "match"] <- trial_type_probs$prob[trial_type_probs$TT == "match"] - (1/32)
            }else{
              trial_type_probs$prob[trial_type_probs$TT == "mismatch"] <- trial_type_probs$prob[trial_type_probs$TT == "mismatch"] - (1/32)
            }
        } else{
          condition_probs$prob[condition_probs$condition == "no_load"] <- condition_probs$prob[condition_probs$condition == "no_load"] - (1/64)
          df$TT[i] <- "match"
        }
      
    }
  condition_probs$prob <- 0.5
  trial_type_probs$prob <- 0.5
  }
  
  return(df)
})


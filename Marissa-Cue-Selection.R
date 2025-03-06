library(dplyr)
library(tidyr)
load("psychling/responses_formatted_1032023.Rdata")

full_df_rev_coded <- full_df %>%
  mutate(condition = factor(COND_ID, levels = c(1,2,3), labels = c("peer", "child_o", "short")),.after = response) %>%
  rename(word_length = wl) %>%
  select(-COND_ID) %>%
  mutate(aoa_inv = max(aoa, na.rm = T) - aoa,
         word_length_inv = max(word_length) - word_length, .after=syll)

## Objectives are to: 
# 1. Compute an average score by cue and condition for reverse-coded word length, reverse-coded aoa, word frequency, and contextual diversity. 
##   AoA and word length have already been reverse coded. 

# 2. Compute a composite "child-orientedness" score for each cue and condition by summing across the averaged psycholinguistic variables.

# 3. See which cues show the largest composite score differences between the child-oriented and peer-oriented conditions.

## Good functions for summarizing statistics by groups and conditions is pairing the group_by() function with summarize() or mutate().



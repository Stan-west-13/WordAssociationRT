library(dplyr)
load("psychling/responses_formatted_1032023.Rdata")

full_df_rev_coded <- full_df %>%
  mutate(condition = factor(COND_ID, levels = c(1,2,3), labels = c("peer", "child-o", "short")),.after = response) %>%
  rename(word_length = wl) %>%
  select(-COND_ID) %>%
  mutate(aoa_inv = max(aoa, na.rm = T) - aoa,
         word_length_inv = max(word_length) - word_length, .after=syll)

## Objectives are to: 
# 1. Compute an average score by cue for reverse-coded word length, reverse-coded aoa, word frequency, and contextual diversity. 
##   AoA and word length have already been reverse coded. 
# 2. Compute a composite "child-orientedness" score for each cue by summing across the averaged psycholinguistic variables.


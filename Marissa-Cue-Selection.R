library(dplyr)
library(tidyr)
load("psychling/responses_formatted_1032023.Rdata")

# reverse coded data frame

full_df_rev_coded <- full_df %>%
  mutate(
    condition = factor(COND_ID, levels = c(1,2,3), labels = c("peer", "child_o", "short")),
    .after = response
    ) %>%
  rename(word_length = wl) %>%
  select(-COND_ID) %>%
  mutate(aoa_inv = max(aoa, na.rm = T) - aoa,
         word_length_inv = max(word_length) - word_length,
         .after=syll
         )

# Composite scores method:

## Objectives are to: 
# 1. Compute an average score by cue and condition for reverse-coded word length, 
##   reverse-coded aoa, word frequency, and contextual diversity. 
##   AoA and word length have already been reverse coded. 

# 2. Compute a composite "child-orientedness" score for each cue and condition 
##   by summing across the averaged psycholinguistic variables.

# 3. See which cues show the largest average composite score differences
##   between the child-oriented and peer-oriented conditions.

## Good dplyr functions for summarizing statistics by groups and conditions 
## is pairing the group_by() function with summarize() or mutate().

## Example data: Randomly sampled cues, responses, and psycholingusitic variables 
ex_data <- data.frame(
  response = sample(full_df_rev_coded$response, 100), 
  cue = sample(full_df_rev_coded$cue, 100), 
  aoa = rnorm(100, m = 4, sd = 2),
  wf = rnorm(100, m = 0.35, sd = 0.10)
) %>%
  mutate(condition = rep(c("child","peer"),each = 50))

## Compute summary stats over conditions:
ex_data %>%
  group_by(condition) %>%
  summarize(
    mean_aoa = mean(aoa, na.rm = T)
    )

## Compute summary stats over cue and condition
ex_data %>%
  group_by(condition, cue) %>%
  summarize(mean_aoa = mean(aoa, na.rm = T))

## With mutate: Notice R adds a column to the existing dataframe instead of just 
## keeping the new column
ex_data %>%
  group_by(condition, cue) %>%
  mutate(mean_aoa = mean(aoa, na.rm = T))



# Difference scores method:


# Data frame of averages by cue and condition

## across() applies the same function to multiple columns at once
## \(x) is the same as function(x)
## In this case, we are computing the mean of each psycholinguistic characteristic in the across() function

df_avg <- full_df_rev_coded |>
  group_by(condition, cue) |>
  summarize(
    n_responses = n(),
    n_unique_responses = n_distinct(response),
    across(
      c(aoa, Lg10WF, Lg10CD, word_length, syll), 
      \(x) mean(x, na.rm = TRUE))
  ) 


diff_table <- df_avg |>
  ## pivot_longer() and pivot_wider() makes it easier to compute the difference scores first because
  ## you can give it the two condition names to subtract across the columns rather than working
  ## with the rows
  pivot_longer(cols = aoa:syll, 
               names_to = "metric",
               values_to = "value"
  ) |>
  pivot_wider(
    id_cols = c(cue, metric),
    names_from = condition,
    values_from = value
  ) |>
  
## Difference scores
  ## We want a negative aoa, word length, and syllable score because that means that the child condition
  ## had lower aoa values, shorter words, and fewer syllables
  mutate(diff = child_o - peer) |>
 
  ## Pivoted back to wide to compare specific psycholinguistic differences between words
   pivot_wider(
    id_cols = cue,
    names_from = metric,
    values_from = diff
  )

write_csv(diff_table, file = "diff_table.csv")


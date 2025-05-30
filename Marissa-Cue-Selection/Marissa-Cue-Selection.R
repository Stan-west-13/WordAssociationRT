library(dplyr)
library(tidyr)
library(ggplot2)
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

## plotting aoa diff scores

library(ggplot2)
library(ggrepel)

ggplot(data = diff_table, aes(x = cue, y = aoa)) +
  geom_point() + 
  geom_text_repel(aes(label = cue), max.overlaps = 30,
                  position = position_nudge_repel(x = .05, y = .02)) +
    theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


# Difference scores for CDI data from Cox & Haebig (unsimplified)
assoc_resp_stats <- readRDS("assoc_resp_stats.rds")

## average scores for AoA of lemma, word frequency, and length by letters, phonemes, and syllables
assoc_resp_avg <- assoc_resp_stats %>%
  group_by(COND, CUE) %>%
  summarize(
    n_responses = n(),
    n_unique = n_distinct(RESPONSE),
    across(
      c(AoA_Kup_lem, Lg10WF, Nletters, Nphon, Nsyll),
      \(x) mean(x, na.rm = TRUE)
    )
  )

full_diff_table <- assoc_resp_avg %>%
  pivot_longer(cols = Nletters:AoA_Kup_lem,
               names_to = "metric",
               values_to = "value") %>%
  pivot_wider(
    id_cols = c(CUE,metric),
    names_from = COND,
    values_from = value
  ) %>%
  mutate(diff = child - adult) %>%
  pivot_wider(
    id_cols = CUE,
    names_from = metric,
    values_from = diff
  )

write.csv(full_diff_table, file = "full_diff.csv")

ggplot(full_diff_table, aes(x = Nletters, y = AoA_Kup_lem)) +
  geom_point(aes(color = Lg10WF))

exclude_words <- c("all gone", "bring", "gas station", "grrr", "hafta/have to",
                   "on top of", "thank you", "wanna/want to", "your")

full <- full_diff_table %>% filter(Nletters < -.35, Lg10WF > .1, AoA_Kup_lem < -0.35,
                                   !CUE %in% exclude_words)
print(full)

## Response averages by condition over all responses and cues
keep_cues <- droplevels(full$CUE[1:60])

resp_avg <- assoc_resp_stats %>%
  filter(CUE %in% keep_cues) %>%
  group_by(COND, CUE) %>%
  summarize(
    n_responses = n(),
    n_unique = n_distinct(RESPONSE),
    across(
      c(AoA_Kup_lem, Lg10WF, Nletters),
      \(x) mean(x, na.rm = TRUE)
    )
  )

cond_avg <- resp_avg %>%
  mutate(COND = recode(COND, "adult" = "classical")) %>%
  group_by(COND) %>%
  summarize(
    across(
      c(AoA_Kup_lem, Lg10WF, Nletters),
      \(x) mean(x)
    )
  )

#add white noise
white_noise <- cond_avg %>%
  filter(COND == "classical") %>%
  mutate(COND = "white noise")
cond_avg <- bind_rows(cond_avg, white_noise)

#bar plot
AoA_plot <- ggplot(cond_avg, aes(x = COND, y = AoA_Kup_lem, fill = COND)) +
  geom_bar(stat = "identity") +
  scale_fill_grey() +
  labs(title = "AoA", x = "Context", y = "Year of Acquisition") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


WF_plot <- ggplot(cond_avg, aes(x = COND, y = Lg10WF, fill = COND)) +
  geom_bar(stat = "identity") +
  scale_fill_grey() +
  labs(title = "Lg10WF", x = "Context", y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

Length_plot <- ggplot(cond_avg, aes(x = COND, y = Nletters, fill = COND)) +
  geom_bar(stat = "identity") +
  scale_fill_grey() +
  labs(title = "Letters", x = "Context", y = "Length") +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggpubr)
ggarrange(AoA_plot, Length_plot, WF_plot, ncol = 3, nrow = 1, labels = c("A", "B", "C"))

# Simplified diff scores
assoc_resp_simple <- readRDS("assoc-response-stats-simple.rds")

#avg scores for nlength, AoA_Kup_lemma, and frequency
simple_avg <- assoc_resp_simple %>%
  group_by(COND, CUE) %>%
  summarize(
     n_responses = sum(n),
     n_unique = n(),
        across(
          c(Nletters, Lg10WF, AoA_Kup_lem),
          \(x) mean(x, na.rm = TRUE)
        )
  )

simple_diff_table <- simple_avg %>%
  pivot_longer(cols = Nletters:AoA_Kup_lem,
               names_to = "metric",
               values_to = "value") %>%
  pivot_wider(
    id_cols = c(CUE,metric),
    names_from = COND,
    values_from = value
  ) %>%
  mutate(diff = child - adult) %>%
  pivot_wider(
    id_cols = CUE,
    names_from = metric,
    values_from = diff
  )

write.csv(simple_diff_table, file = "simple_diff.csv")

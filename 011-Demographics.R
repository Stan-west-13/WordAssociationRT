library(readxl)
library(purrr)
library(ggplot2)
library(ez)
library(pastecs)
library(psych)
library(tidyverse)
library(rstatix)

temp_df <- read_xlsx('Demographics_TTA.xlsx')

################################################
# Making new data frame to factorize columns
################################################

## n participants per group:
demo_df <- temp_df %>%
  mutate_at(c('subject_id', 'condition', 'education', 'gender', 'income', 'race', 'ethnicity', 
              'parent', 'toddler_interaction', 'toddler_int_frequency'), as.factor)


demo_summary <- demo_df %>% 
  group_by(condition) %>% 
  summarize(n = n(),
            mean_age = mean(age, na.rm = T),
            sd_age = sd(age, na.rm = T),
            min_age = min(age, na.rm = T),
            max_age = max(age, na.rm = T),
            NA_values = sum(if_any(everything(), is.na)))

x <- demo_df %>% 
  group_by(condition, education) %>% 
  summarize(n_edu = n())

y <- demo_df %>% 
  group_by(condition) %>% 
  summarise(n_associates = sum(education == 'associates')/ n(),
            n_highschool = sum(education == 'high school graduate')/ n(),
            )


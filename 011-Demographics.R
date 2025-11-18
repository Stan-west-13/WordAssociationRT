library(readxl)
library(purrr)
library(ggplot2)
library(ez)
library(pastecs)
library(psych)
library(tidyverse)
library(rstatix)
library(flextable)

temp_df <- read_xlsx('Demographics_TTA.xlsx')

#########################
# Making new data frame
#########################

demo_df <- temp_df %>%
  filter(!subject_id %in% c("TTA_067","TTA_068")) %>% 
  mutate_at(c('subject_id', 'condition', 'education', 'gender', 'income', 'race', 'ethnicity', 
              'parent', 'toddler_interaction', 'toddler_int_frequency'), as.factor)


demo_summary <- demo_df %>% 
  group_by(condition) %>% 
  summarize(n = n(),
            n_male = length(which(gender == 'male')),
            mean_age = mean(age, na.rm = T),
            sd_age = sd(age, na.rm = T),
            min_age = min(age, na.rm = T),
            max_age = max(age, na.rm = T),
            NA_age = sum(if_any(age, is.na)),
            weekly_int = sum(toddler_int_frequency %in% c('daily', 'weekly', 'few/week'))/ n(),
            less_int = sum(toddler_int_frequency %in% c('monthly', 'rare', 'never'))/ n()
            ) 
demo_summary$mean_age <- round(demo_summary$mean_age, 2)
demo_summary$sd_age <- round(demo_summary$sd_age, 2)
demo_summary$weekly_int <- round(demo_summary$weekly_int, 2)
demo_summary$less_int <- round(demo_summary$less_int, 2)

##########################
# Making table for poster
##########################

demo_table <- flextable(data = head(demo_summary),
                        col_keys = c('condition','col1', 'n', 'col2',
                                     'mean_age', 'sd_age', 'min_age', 'max_age', 'col3',
                                     'weekly_int')) |> 
                          width(j = 'col1', width = .2) |>
                          empty_blanks()

demo_table <- demo_table %>% 
  add_header_row(., values = c('','', 'Age', 'Toddler Interaction'), 
                 colwidths = c(2, 2, 5, 1)) %>% 
  align(align = 'center', part = 'all') %>% 
  set_header_labels(., n = 'n(male)', mean_age = 'mean', sd_age = 'SD', min_age = 'min',
                    max_age = 'max', NA_age = 'NA', weekly_int = '≥ Weekly') %>% 
  bg(bg = '#FCFBFF', part = 'all') %>% 
  autofit(., add_w = 0, add_h = 0)
  
save_as_docx(demo_table, path = "demographics_table.docx")










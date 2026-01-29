library(tidyverse)
library(readxl)

# Unzip data folder
unzip("data_WMRT.zip")

# List out all the participant folders (later moving to bottom after function is figured out)
ppfiles <- list.files('data_WMRT', full.names = T)

# This test function shows me that 
extract_csv <- function(datafile) {
  x <- ''
  
  for (file in datafile) {
    get_csv <- list.files(file, '.csv')
    for (csv in get_csv) {
      new <- paste0(file, "/", csv)
      new <- str_subset(new, 'trialstest', negate = T)
      x <- c(x, new)
    }
  }
  return(x)
}

extract_csv(ppfiles)


# Make function to extract the .csv files wanted from each participant file
extract_csv <- function(datafile) {
  df <- data.frame()
  x <- ''
  
  for (file in datafile) {
    get_csv <- list.files(file, '.csv')
    for (csv in get_csv) {
      new <- paste0(file, "/", csv) #the only problem left here is getting rid of the trialtest.csv files
      new <- str_subset(new, 'trialstest', negate = T)
      new <- read.csv(paste0(new))
      df <- rbind(df,new)
    }
  }
  return(df)
}

extracted_csv <- extract_csv(ppfiles)

# make if else loop
extract_csv <- function(datafile) {
  df <- data.frame()
  x <- ''
  
  for (file in datafile) {
    get_csv <- list.files(file, '.csv')
    for (csv in get_csv) {
      if (str_detect(csv, 'trialstest', negate = T))
        new <- paste0(file, "/", csv)
        x <- c(x, new)
    }
  }
  return(x)
}

extracted_csv <- extract_csv(ppfiles)

extract_csv <- function(datafile) {
  df <- data.frame()
  x <- ''
  
  for (file in datafile) {
    get_csv <- list.files(file, '.csv')
    for (csv in get_csv) {
      if (str_detect(csv, 'trialstest', negate = T))
        new <- paste0(file, "/", csv)
        new_df <- read.csv(paste0(new))
        df <- rbind(df,new_df)
    }
  }
  return(df)
}

extracted_csv <- extract_csv(ppfiles)


# Excel extract

extract_xlsx <- function(datafile) {
  x <- ''
  
  for (file in datafile) {
    get_xlsx <- list.files(file, '.xlsx')
    contains(get_xlsx, )
    x <- c(x, new)
    }
  }
  return(x)
}

extract_csv(ppfiles)





files <- list.files("data_WMRT", pattern = '.xlsx', recursive = T,full.names = TRUE)
files <- str_subset(files, 'PRAC', negate = T)

x <- map_dfr(files, function(x){
  d <- read_xlsx(x, col_types ="text" ,trim_ws = TRUE)[-c(33:41),]
  d <- d %>% 
    select(participant = pp, condition, trial_type = TT, cue, response = input_textbox.text_raw, cue_rt = key_space.rt_mean, resp_rt = input_key.rt_mean, 
            square_resp = yesno_resp.keys_raw, square_rt = yesno_resp.rt_mean) %>% 
    mutate(response = gsub(response, pattern = "'", replacement = ""),
           response = trimws(gsub(response, pattern = "[\\r\\n]", replacement = "")))
}) #map_dfr reads in the tables then it stacks them by rows


# make square response a factor then work on correct response column, then look at accuracy














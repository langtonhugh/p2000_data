# Load packages.
library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(lubridate)

# Load data. This has been handled and saved in p2000_html_handling_use_this.R script.
mydata <- readRDS("data/new_scrapes_20230208_20231009.rds")

# Initial cleaning.
source_clean_df <- mydata %>% 
  rename(code = X1, times = X2, dates = X3, sign = X4, info = X5) %>% 
  drop_na(times) %>%  
  distinct(times, dates, info, .keep_all = TRUE) %>% 
  mutate(service = case_when(str_detect(string = info, pattern = "Ambulance" ) ~ "Ambulance", # imperfect.
                             str_detect(string = info, pattern = "Politie"   ) ~ "Police",
                             str_detect(string = info, pattern = "Brandweer" ) ~ "Fire",
                             str_detect(string = info, pattern = "MKA"       ) ~ "Ambulance"),
         priority = str_remove_all(string = str_extract_all(
           info, "^.{3}"), pattern = " ")
         ) 

# Create a complete date variable to mimic GMS data.
source_clean_df <- source_clean_df %>% 
  mutate(timestamp = dmy_hms(paste(dates, times))) %>% 
  drop_na(timestamp) 

# Confirm dates for subset.
min(source_clean_df$timestamp)
max(source_clean_df$timestamp)

# Subset.
subset_df <- source_clean_df %>% 
  mutate(month_var = month(timestamp)) %>% 
  filter(month_var == 6)

# Save for demo at RUM-UMC.
write_csv(subset_df, "data/p2000_july2023.csv")

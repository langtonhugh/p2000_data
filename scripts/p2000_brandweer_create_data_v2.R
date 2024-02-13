# =======================================
# Loading in real P2000 data and creating
# fake 'real' GMS data.
# =======================================

# Load packages.
library(readr)
library(dplyr)
library(stringr)
library(forcats)
library(tidyr)
library(lubridate)

# Turn off scientific notation.
options(scipen=9999999)

# Load data. This has been handled and saved in p2000_html_handling_use_this.R script.
mydata_1 <- readRDS("data/mydata_20211130_20220524.rds")
mydata_2 <- readRDS("data/new_scrapes_20220524_20230208.rds")
mydata_3 <- readRDS("data/new_scrapes_20230208_20231009.rds")

# Bind rows.
mydata <- bind_rows(mydata_1, mydata_2, mydata_3)

# Remove old to save memory if needed.
rm(mydata_1, mydata_2, mydata_3)

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
  drop_na(timestamp) # only 40 didn't parse.

# Clean the data in a way which kind of mimics GMS data for fire only, but we 
# also keep the info variable so NIPV see the raw info.
source_gms_struc_df <- source_clean_df %>% 
  filter(service  == "Fire") %>% 
  mutate(priority = if_else(str_detect(priority, "1"), "prio1", priority),
         priority = if_else(str_detect(priority, "2"), "prio2", priority)) %>% 
  filter(priority %in% c("prio1", "prio2")) %>% 
  select(code, timestamp, sign, info, service, priority)

# Load in the CAP code data.
cap_df <- read_delim("data/capcodelijst_source_in_header.csv", skip = 1, delim = ";") %>% 
  mutate(code = as.character(code))

# Get a table of what ones we have. First-two-digits ID is not that simple...
fire_cap_lookup_df <- cap_df %>% 
  filter(cap_service == "Brandweer") %>% 
  mutate(region_code = str_extract_all(code, "^.{2}")) %>% 
  group_by(region_code, region) %>% 
  tally() %>% 
  ungroup() %>% 
  arrange(region_code)

# Join cap codes with the scraped data, create label for the data 'source'
# and then drop missing regions.
fire_clean_df <- source_gms_struc_df %>% 
  left_join(cap_df, relationship = "many-to-many") %>% 
  mutate(data_source = "p2000")

# How many are missing on the 'region'? 
sum(is.na(fire_clean_df$region)) / nrow(fire_clean_df) # 25%

# Drop missings on region.
# !!! note this decision !!! you may decide to keep them !!!
fire_no_miss_clean_df <- fire_clean_df %>%
  filter(!is.na(region) )

# Select only variables that are in inzet-level GMS data and rename to
# something similar.
fire_no_miss_clean_df <- fire_no_miss_clean_df %>% 
  select(code, timestamp, service, priority, region, data_source) %>% 
  rename(roep_nummer             = code,
         brand_melding_timestamp = timestamp,
         dienst                  = service,
         prio                    = priority,
         regio                   = region)

# Mimic this data for 'fake' real GMS data.
fake_gms_df <- fire_no_miss_clean_df %>% 
  slice_sample(prop = 0.75, replace = FALSE) %>% 
  mutate(data_source = "fake gms")
  
# Save each data separately.
write_csv(x = fire_no_miss_clean_df, file = "data/p2000_demo_data.csv")
write_csv(x = fake_gms_df          , file = "data/fake_gms_demo_data.csv")

# End.
# ==============================================
# Handling and visuals for Brandweer comparison.
# ==============================================

# To get the latest changes, please run the p2000_brandweer_create_data_v2.R
# script first! Then proceed with this script.

# Load packages.
library(dplyr)
library(readr)
library(ggplot2)
library(forcats)
library(lubridate)

# Turn off scientific notation.
options(scipen=9999999)

# Set ggplot2 theme.
theme_set(theme_bw())

# Load in the p2000 data. Created by SL using p2000_brandweer_create_data_v2.R.
p2000_df <- read_csv("data/p2000_demo_data.csv")

# Load in the GMS data. For now, this is fake data created by SL in the
# p2000_brandweer_create_data_v2.R script. 
gms_df   <- read_csv("data/fake_gms_demo_data.csv")

# You can proceed with the fake data to see the demo comparison, but later on,
# replace the above gms_df with the *real dispatch-level GMS data*. The data
# should be inzet-level data for prio1 or prio2 incidents. If you 
# wrangle the real data to mimic the structure of this fake data, the rest of
# the script should run without any further edits.

# We need a day-date to filter the gms data.
p2000_df <- p2000_df %>% 
  mutate(full_date = date(brand_melding_timestamp))

# Filter the GMS data according to days present in the p2000 data. In this
# demo, the fake GMS data is just a random sample of the p2000 data, so the
# filter probably does nothing. The real thing will drop much more though
# (as it should!).
gms_sub_df <- gms_df %>% 
  mutate(full_date = date(brand_melding_timestamp)) %>% 
  filter(full_date %in% unique(p2000_df$full_date))

# Bind them together to make handling easier.
complete_df <- bind_rows(p2000_df, gms_sub_df)

# Remove the old ones to save memory if needed.
rm(gms_df, gms_sub_df, p2000_df)

# Create (potentially) useful variables.
clean_df <- complete_df %>% 
  mutate(date_time_r  = round_date(brand_melding_timestamp, "hour"),
         hour_of_day  = hour(date_time_r),
         week_day     = wday(full_date , abbr = FALSE, label = TRUE),
         hour_of_week = hour_of_day + (24 * (wday(full_date, week_start = 1)-1)), # Create an hour of the week (0-167 hours).
         month_naam   = month(full_date, abbr = FALSE, label = TRUE),
         week_year    = week(full_date),
         week_day_f   = fct_relevel(week_day,
                          "maandag"  , "dinsdag" , "woensdag",
                          "donderdag", "vrijdag" , "zaterdag", "zondag")
         )

# To speed things up and try things out, use random sample. 
# Otherwise, comment this out.
clean_df <- clean_df %>%
  sample_n(size = 100000) %>%
  ungroup()

# Nationwide hourly frequencies by hour (mainly to show study period and gaps).
clean_df %>% 
  group_by(date_time_r, data_source) %>% 
  summarise(counts = n()) %>% 
  ungroup() %>% 
  ggplot() +
  geom_line(mapping = aes(x = date_time_r, y = counts, colour = data_source)) +
  facet_wrap(~data_source, scales = "free", ncol = 1) +
  theme(legend.position = "none")

# Save.
ggsave(filename = "visuals/hourly_counts_study_period.png",
       height = 10, width = 16, unit = "cm")

# Check regional frequencies.
clean_df %>% 
  ggplot() +
  geom_bar(mapping = aes(x = regio, fill = data_source), position = "dodge") + 
  coord_flip() +
  theme(legend.position = "bottom")

# Save.
ggsave(filename = "visuals/regional_frequencies.png",
       height = 16, width = 12, unit = "cm")

# Check priority frequencies by region.
clean_df %>% 
  ggplot() +
  geom_bar(mapping = aes(x = prio, fill = data_source), position = "dodge") +
  facet_wrap(~regio) +
  theme(legend.position = "bottom")

# Save.
ggsave(filename = "visuals/priority_frequencies_region.png",
       height = 26, width = 28, unit = "cm")

# Regional frequency counts.
regional_hour_freq_df <- clean_df %>%
  group_by(regio, data_source, hour_of_day) %>%
  tally() %>% 
  ungroup()

# Plot.
ggplot(data = regional_hour_freq_df) +
  geom_line(mapping = aes(x = hour_of_day, y = n, group = data_source, colour = data_source)) +
  facet_wrap(~regio, scales = "free") +
  labs(colour = NULL) +
  theme(
    legend.position = "bottom"
  )

# Save.
ggsave(filename = "visuals/regional_hourly_frequencies.png",
       height = 26, width = 28, unit = "cm")

# Distributions plots of hours of day.
hourly_counts_df <- clean_df %>% 
  group_by(hour_of_day, data_source, full_date) %>% 
  summarise(counts = n()) %>% 
  ungroup() 

# # I've commented this out because the boxplots below are maybe better alternative.
# # Plot hours of the day means with raw distribution around it.
# hourly_counts_df %>%
#   filter(counts < 50) %>% # Comment out or change (are you using the sample?).
#   ggplot(mapping = aes(x = hour_of_day, y = counts, group = 1, col = data_source),
#          shape = 21, fill = "black", colour = NA) +
#   geom_point(alpha = 0.1) +
#   stat_summary(fun = mean, geom = "line" , col = "black", group = 1, linewidth = 1) +
#   facet_wrap(~data_source, scales = "free_y", nrow = 1) +
#   theme(legend.position = "none")
# 
# # Save.
# ggsave(filename = "visuals/hours_mean_distribution.png",
#        height = 26, width = 28, unit = "cm")

# Plot distribution.
hourly_counts_df %>% 
  filter(counts < 50) %>% # Comment out or change (are you using the sample?).
  ggplot(mapping = aes(x = hour_of_day, y = counts, group = hour_of_day, fill = data_source)) +
  geom_boxplot(outlier.shape = NA) + # this removes outlier points.
  stat_summary(fun = mean, geom = "point", group = 1) +
  facet_wrap(~data_source, scales = "fixed", nrow = 1) +
  theme(legend.position = "none")

# Save.
ggsave(filename = "visuals/hours_mean_boxplots.png",
       height = 26, width = 28, unit = "cm")

# Create hour of week distributions.
hour_week_counts_df <- clean_df %>% 
  group_by(hour_of_week, full_date, data_source) %>% 
  summarise(counts = n()) %>% 
  ungroup()

# Plot distribution.
hour_week_counts_df %>% 
  filter(counts < 50) %>% # Comment out or change (are you using the sample?).
  ggplot(mapping = aes(x = hour_of_week, y = counts, group = hour_of_week, colour = data_source)) +
  geom_point(alpha = 0.1) +
  stat_summary(fun = "mean", geom = "line", col = "black", group = 1) +
  facet_wrap(~data_source, nrow = 2, scales = "free_y") +
  theme(legend.position = "none")

# Save.
ggsave(filename = "visuals/hours_week_mean_disributions.png",
       height = 12, width = 18, unit = "cm")


# End.



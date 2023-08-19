# Produce long-term annual summaries for table in chapter.
# KMH
#
# The station_summary_table at the end of the script was exported into
# the figures folder as 'Annual_Met_Summaries_Table.png'



# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(gt)

# Read in the data from the CSV file
data <- read_csv("./data/processed_data/met_hourly_gap_filled.csv")

# Convert 'dt' column to a proper datetime format
data$dt <- as.POSIXct(data$dt)

# Extract year from 'dt' column
data$year <- lubridate::year(data$dt)





# Calculate annual means for variables considering flag variables
annual_means <- data %>%
  group_by(sta, year) %>%
  summarize(
    airt_mean = mean(airt[!airt_miss & !no_record_flag], na.rm = TRUE),
    minair_mean = mean(minair[!minair_miss & !no_record_flag], na.rm = TRUE),
    minair_min = min(minair[!minair_miss & !no_record_flag], na.rm = TRUE),
    maxair_mean = mean(maxair[!maxair_miss & !no_record_flag], na.rm = TRUE),
    maxair_max = max(maxair[!maxair_miss & !no_record_flag], na.rm = TRUE),
    rh_mean = mean(rh[!rh_miss & !no_record_flag], na.rm = TRUE)
  )

# Calculate sum of 'ppt' for each year and station
annual_ppt_sum <- data %>%
  filter(!ppt_miss & !no_record_flag) %>%
  group_by(sta, year) %>%
  summarize(ppt_sum = sum(ppt, na.rm = TRUE))

# Join the annual means and annual ppt sum data frames
final_data <- left_join(annual_means, annual_ppt_sum, by = c("sta", "year"))




# Create a table for annual 'ppt' sums
ppt_table <- final_data %>%
  gt() %>%
  tab_header(title = "Annual Precipitation Sum") %>%
  cols_label(year = "Year", sta = "Station", ppt_sum = "Total Precipitation")

# Print the table
print(ppt_table)



# Summarize variables across all years by station
station_summary <- final_data %>%
  group_by(sta) %>%
  summarize(
    airt_mean_ = round(mean(airt_mean, na.rm = TRUE), 1),
    airt_sd = round(sd(airt_mean, na.rm = TRUE), 1),
    minair_min = round(min(minair_min, na.rm = TRUE), 1),
    minair_mean_ = round(mean(minair_mean, na.rm = TRUE), 1),
    minair_sd = round(sd(minair_mean, na.rm = TRUE), 1),
    maxair_max = round(max(maxair_max, na.rm = TRUE), 1),
    maxair_mean_ = round(mean(maxair_mean, na.rm = TRUE), 1),
    maxair_sd = round(sd(maxair_mean, na.rm = TRUE), 1),
    rh_mean_ = round(mean(rh_mean, na.rm = TRUE), 1),
    rh_sd = round(sd(rh_mean, na.rm = TRUE), 1),
    ppt_mean = round(mean(ppt_sum, na.rm = TRUE), 1),
    ppt_sd = round(sd(ppt_sum, na.rm = TRUE), 1)
  )

# Create a table for station summary
station_summary_table <- station_summary %>%
  gt() %>%
  tab_header(title = "Annual Meteorological Means and Standard Deviations") %>%
  cols_label(sta = "Station", 
             airt_mean_ = "Air Temperature Mean (C)",
             airt_sd = "Air Temperature SD",
             minair_mean_ = "Minimum Air Temperature Mean (C)",
             minair_sd = "Minimum Air Temperature SD",
             minair_min = "Record Minimum Air Temperature (C)",
             maxair_mean_ = "Maximum Air Temperature Mean (C)",
             maxair_sd = "Maximum Air Temperature SD",
             maxair_max = "Record Maximum Air Temperature (C)",
             rh_mean_ = "Relative Humidity Mean (%)",
             rh_sd = "Relative Humidity SD",
             ppt_mean = "Precipitation Mean (mm)",
             ppt_sd = "Precipitation SD")

# Print the station summary table
print(station_summary_table)




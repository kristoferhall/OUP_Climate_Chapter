# KM Hall
#
# Purpose:
# 1. Create all hourly records for stations of interest. Some stations have missing records,
#    and this step will add a record for all records from when the station initially went online
#    until the end of 2021.
# 2. Make flags that identify records that weren't present in the raw data
# 3. Anomalize data by station and variable of interest



library(tidyverse)
library(lubridate)
library(anomalize)
library(rlang)

# file containing functions for this script
source("./R/met_anomalize_hourly_functions.R")


file_input <- "./data/raw_data/sev_hrly_met_40_42_49_50.csv"
files_out_path <- "./data/processed_data/"      # folder where any processed data will be written

met <- read_csv(file_input) %>% 
  mutate(StationID = as.factor(StationID)) %>% 
  select(StationID:Hour, Temp_C, Max_Temp_C, Min_Temp_C, Precipitation, Relative_Humidity,
         Solar_Radiation, Min_Solar_Radiation, Max_Solar_Radiation) %>% 
  rename(sta = StationID,
         dt = Date_Time,
         date = Date,
         year = Year,
         month = Month,
         day_of_month = Day_of_Month,
         day_of_year = Julian_Day,
         hour = Hour,
         airt = Temp_C,
         maxair = Max_Temp_C,
         minair = Min_Temp_C,
         ppt = Precipitation,
         rh = Relative_Humidity,
         sol = Solar_Radiation,
         minsol = Min_Solar_Radiation,
         maxsol = Max_Solar_Radiation)

# create a long version of the data
met_l <- met %>% 
  pivot_longer(airt:maxsol, names_to = "variable", values_to = "value")



# initial inspection of the data ------------------------------------------------------------

summary(met)

# inspect each station and variable - uncomment to run because thegraphs take a while to run
# sta_hrly_all_vars_graph(data=met_l, station="40")
# sta_hrly_all_vars_graph(data=met_l, station="42")
# sta_hrly_all_vars_graph(data=met_l, station="49")
# sta_hrly_all_vars_graph(data=met_l, station="50")
# 
# hrly_var_graph(data=met,  var_name = "airt")
# hrly_var_graph(data=met,  var_name = "maxair")
# hrly_var_graph(data=met,  var_name = "minair")
# hrly_var_graph(data=met,  var_name = "sol")
# hrly_var_graph(data=met,  var_name = "minsol")
# hrly_var_graph(data=met,  var_name = "maxsol")
# hrly_var_graph(data=met,  var_name = "ppt")
# hrly_var_graph(data=met,  var_name = "rh")


# create flags for missing data and missing records ------------------------


# missing data flags for each met variable for records that are in the raw data.
#   will join back in with other data later on
met_data_miss_flags <- met %>% 
  mutate(airt_miss = ifelse(is.na(airt), TRUE, FALSE),
         maxair_miss = ifelse(is.na(maxair), TRUE, FALSE),
         minair_miss = ifelse(is.na(minair), TRUE, FALSE),
         ppt_miss = ifelse(is.na(ppt), TRUE, FALSE),
         rh_miss = ifelse(is.na(rh), TRUE, FALSE),
         sol_miss = ifelse(is.na(sol), TRUE, FALSE),
         minsol_miss = ifelse(is.na(minsol), TRUE, FALSE),
         maxsol_miss = ifelse(is.na(maxsol), TRUE, FALSE)) %>% 
  select(sta, dt, airt_miss:maxsol_miss)


# subsetting data for each station
m40 <- subset_stations(met, "40")
m42 <- subset_stations(met, "42")
m49 <- subset_stations(met, "49")
m50 <- subset_stations(met, "50")



# make data with no record flags (nr = no record) - this data has a record for every
# hour since a station came online, even if the record was missing in the raw data
m40_nr <- sta_all_dts_w_flag(m40)
m42_nr <- sta_all_dts_w_flag(m42)
m49_nr <- sta_all_dts_w_flag(m49)
m50_nr <- sta_all_dts_w_flag(m50)


# provides info on how the no record flags are setting for each station
no_rec_flag_count(m40_nr)
no_rec_flag_count(m42_nr)
no_rec_flag_count(m49_nr)
no_rec_flag_count(m50_nr)


# graphs show where no record flags are setting in the time series
no_rec_graph(m40_nr)
no_rec_graph(m42_nr)
no_rec_graph(m49_nr)
no_rec_graph(m50_nr)





# hourly gap-filling  --------------------------------------------------------------------

# after calculating variable means for day of year and hour of day, joins those to
# the data and uses the means to gap-fill missing data

m40_gf <- var_hrly_means(m40_nr)
m42_gf <- var_hrly_means(m42_nr)
m49_gf <- var_hrly_means(m49_nr)
m50_gf <- var_hrly_means(m50_nr)

m_all_gf <- rbind(m40_gf, m42_gf, m49_gf, m50_gf)

m_all_gf_l <- m_all_gf %>% 
  pivot_longer(airt:maxsol, names_to = "variable", values_to = "value")
  



# testing gf with graph - uncomment to run
# gf_graph(m_all_gf_l, "40")
# gf_graph(m_all_gf_l, "42")
# gf_graph(m_all_gf_l, "49")
# gf_graph(m_all_gf_l, "50")




# anomalize the data for each variable -----------------------------------------

airt_an <- var_anomolize(m_all_gf, "airt")
maxair_an <- var_anomolize(m_all_gf, "maxair")
minair_an <- var_anomolize(m_all_gf, "minair")
ppt_an <- var_anomolize(m_all_gf, "ppt")
rh_an <- var_anomolize(m_all_gf, "rh")
sol_an <- m_all_gf %>% 
  filter(sta != "49") %>% 
  var_anomolize(., "sol")
maxsol_an <- m_all_gf %>% 
  filter(sta != "49") %>% 
  var_anomolize(., "maxsol")
minsol_an <- m_all_gf %>% 
  filter(sta != "49") %>% 
  var_anomolize(., "minsol")



show_anomalies_graph(airt_an)
show_anomalies_graph(minair_an)
show_anomalies_graph(maxair_an)
show_anomalies_graph(ppt_an)
show_anomalies_graph(rh_an)
show_anomalies_graph(sol_an)
show_anomalies_graph(minsol_an)
show_anomalies_graph(maxsol_an)


show_anomalies_stats(airt_an)
show_anomalies_stats(minair_an)
show_anomalies_stats(maxair_an)
show_anomalies_stats(ppt_an)
show_anomalies_stats(rh_an)
show_anomalies_stats(sol_an)
show_anomalies_stats(minsol_an)
show_anomalies_stats(maxsol_an)


# join data missing flags with m_all_gf before writing to file ------------
m_all_gf_flags <- m_all_gf %>% 
  left_join(met_data_miss_flags)

# NOTE: The data missing flags (e.g. - airt_miss) are TRUE if the value
# was missing in the raw data, FALSE if present in the raw data, and
# NA if the record didn't exist in the raw data and was added.
#
# Both TRUE and NA data missing flags will be gap filled. There will not
# be missing values, except for the solar variables for station 49 because
# that station has never had solar sensors.



# summary(m_all_gf_flags)



# write out files ----------------------------------------------------

# not going to use anomalize flags. going to just keep the gap-filled
# hourly data to use in summarizing future data by day, month, year, etc.
#
# there aren't enough true anomalies to have much affect on less granular
# data summaries



write_csv(m_all_gf_flags, paste0(files_out_path, "met_hourly_gap_filled.csv"))









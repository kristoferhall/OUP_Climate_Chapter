# This script produces a table for the chapter providing basic info about each station.
# The chapter text contians more descriptive info about each station.
#
# KMH
#
#
# 
# The script was used to create the 'met_station_info_table.png' in the figures folder.


library(tidyverse)
library(gt)

station_id <- c("40", "49", "50", "42")
station_name <- c("Deep Well", "Five Points", "Blue Grama", "Cerro Montoso")
lat <- c(34.3592, 34.335, 34.335, 34.3685)
lon <- c(-106.691, -106.729, -106.632, -106.535)
elev <- c(1600, 1610, 1669, 1971)
estab <- c(1988, 1989, 1999, 2002)


# used to determine year established
# df <- read_csv("./data/processed_data/met_hourly_gap_filled.csv")
# 
# df |> 
#   mutate(year = year(dt)) |> 
#   group_by(sta) |> 
#   summarize(established_year = min(year))
  
station_info <- tibble(station_id = station_id,
                       station_name = station_name,
                       year_established = estab,
                       latitude = lat,
                       longitude = lon,
                       elevation = elev)


station_info_table <- station_info |> 
  gt() |> 
  tab_header(title = "SEV Meteorological Station Information") |> 
  cols_label(station_id = "Station ID",
             station_name = "Station Name",
             year_established = "Year Established",
             latitude = "Latitude (deg)",
             longitude = "Longitude (deg)",
             elevation = "Elevation (m)")
  
print(station_info_table)
  
  
# creates the met_station_info_table.png in figures
  
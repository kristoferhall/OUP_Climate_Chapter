# KM Hall
#
#
# This script provides a more careful look at the raw met data prior to gap 
# filling in order to add more careful filtering of outliers.
#
# This came up because it was noted that there were some -40 minair values
# showing up during monsoon season in the met_basic_analyses.Rmd report. 
# It prompted this deeper dive into the raw data prior to any gap filling.


library(tidyverse)
library(lubridate)

library(anomalize)
library(tibbletime)


# load met hourly raw (mhr) for stations on interest (40, 42, 49, 50)
mhr <- read_csv("./data/raw_data/sev_hrly_met_40_42_49_50.csv") %>% 
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






summary(mhr)


# hourly temps grouped by month of year ----------------------------------
ggplot(mhr, aes(x = dt, y = airt, color = sta)) +
  geom_point() +
  facet_grid(month(dt) ~ sta)

# My initial impression, just doing this one graph, is that you first
# need to start at at least a daily, if not monthly, scale to identify
# values that seem unlikely. Then, you can get back to the hourly scale to
# fix outliers. So, it is necessary to jump between levels, I think.
#
# You can definitely see some obvious issues in the data, but I suspect
# there is a lot hidden at the hourly scale that needs to be identified 
# and addressed first at the daily or monthly level.





# roll-up into daily and monthly for some vars of interest ----------------

# raw met daily summaries
mdr <- mhr %>% 
  group_by(sta, date) %>% 
  summarize(airt = (max(maxair, na.rm = TRUE) + min(minair, na.rm = TRUE)) / 2,
            minair = min(minair, na.rm = TRUE),
            maxair = max(maxair, na.rm = TRUE),
            ppt = sum(ppt, na.rm = TRUE),
            rh = mean(rh, na.rm = TRUE)) %>% 
  mutate(doy = yday(date),
         airt = ifelse(airt == -Inf, NA, airt),
         minair = ifelse(minair == Inf, NA, minair),
         maxair = ifelse(maxair == -Inf, NA, maxair))

summary(mdr)


ggplot(mdr, aes(x = date, y = airt, color = sta)) +
  geom_point() +
  facet_grid(month(date) ~ sta)


mmr <- mdr %>%
  mutate(date = ymd(paste0(year(date), "-", month(date), "-01"))) %>% 
  group_by(sta, date) %>% 
  summarize(airt = mean(airt, na.rm = TRUE),
            minair = min(minair, na.rm = TRUE),
            maxair = max(maxair, na.rm = TRUE),
            ppt = sum(ppt, na.rm = TRUE),
            rh = mean(rh, na.rm = TRUE)) %>%
  mutate(airt = ifelse(airt == -Inf, NA, airt),
         minair = ifelse(minair == Inf, NA, minair),
         maxair = ifelse(maxair == -Inf, NA, maxair)) %>% 
  ungroup()

summary(mmr)

ggplot(mmr, aes(x = date, y = airt, color = sta)) +
  geom_point() +
  facet_grid(month(date) ~ sta, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  labs(title = "Mean Monthly Air Temperature (C)")

ggplot(mmr, aes(x = date, y = minair, color = sta)) +
  geom_point() +
  facet_grid(month(date) ~ sta, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)) +
  labs(title = "Mean Minimum Air Temperature (C)")



# 
# # try anomalize pkg at the monthly scale - might have to actually break
# # it up further by month...we'll see...
# 
# mmr %>% 
#   glimpse()
# 
# m_minair_anomalized <- mmr %>% 
#   mutate(date = mdate) %>%
#   time_decompose(minair, merge = TRUE) %>% 
#   anomalize(remainder) %>% 
#   time_recompose()
# 
# class(mmr)
# 
# 
# # Have to fill NAs in temp to something for anomalize to work because
# # it requires a complete time series.
# #
# # So, going to use the overall monthly means for all stations to fill
# fill_values <- mmr %>% 
#   mutate(month = month(date)) %>% 
#   group_by(sta, month) %>% 
#   summarize(airt_fill = mean(airt, na.rm = TRUE),
#             minair_fill = mean(minair, na.rm = TRUE),
#             maxair_fill = mean(maxair, na.rm = TRUE)) %>% 
#   ungroup()
# 
# mmr_fill <- mmr %>% 
#   mutate(month = month(date)) %>% 
#   left_join(fill_values) %>% 
#   mutate(airt = ifelse(is.na(airt), airt_fill, airt),
#          minair = ifelse(is.na(minair), minair_fill, minair),
#          maxair = ifelse(is.na(maxair), maxair_fill, maxair))
# 
# 
# # plot data versus monthly means
# mmr_fill %>% 
#   ggplot(aes(x = date)) +
#   geom_point(aes(y = airt), color = "firebrick", size = 1.2) +
#   geom_line(aes(y = airt_fill), color = "goldenrod", size = 0.5) +
#   facet_wrap(~ sta) +
#   theme_minimal() +
#   facet_grid(month(date) ~ sta, scales = "free_y")
# 
# 
# var_anomolize <- function(data, var_to_anomalize, date_var) {
#   # runs anomalize on a variable for all stations
#   
#   data %>% 
#     select(sta, {{ date_var }}, {{ var_to_anomalize }}) %>% 
#     arrange(sta, {{ date_var }}) %>% 
#     group_by(sta) %>% 
#     time_decompose({{ var_to_anomalize }}, merge = TRUE) %>% 
#     anomalize(remainder) %>% 
#     time_recompose()
# }
# 
# 
# 
# var_anomolize(mmr, "ppt", "date")




# function to plot monthly data with all stations combined to visualize potential data problems
by_month_all_sta <- function(data, var, title) {
  ggplot(data, aes(x = year(date), y = {{ var }}, color = sta)) +
    geom_line() +
    facet_wrap(~ month(date), scales = "free_y") +
    ggtitle({{ title }})
}



# airt monthly graphs to ID potential outliers ----

by_month_all_sta(mmr, airt, "Monthly Mean Air Temperature (C)")  
# Possible issue for sta 50 in 09-2012 and 10-2012

mmr %>% filter(date == "2012-10-01")
# this is the same month were you have a monthly minair of -40. Need to look at this at daily and hourly scales

mmr %>% filter(date == "2012-09-01")
# same issue in 09.




# minair monthly graphs to ID potential outliers ----

by_month_all_sta(mmr, minair, "Monthly Minimum Air Temperature (C)")  
# Several possible issues:
# - 49 in 01-2003 - has a -40 value
# - 42 in 05-2017 - has a -40 value
# - 50 in 09-2012 and 09-2020 - has a -40 value
# - 50 in 10-2012 and 10-2020 - has a -40 value

mmr %>% filter(date == "2003-01-01")
# actually -39.7 for the value at 49

mmr %>% filter(date == "2017-05-01")
# value of -40 at 42

mmr %>% filter(date == "2012-09-01" | date == "2012-10-01")
# value of -40 at 50 in both Sept. and Oct.

mmr %>% filter(date == "2020-09-01" | date == "2020-10-01")
# value of -40 at 50 in both Sept. and Oct.

# need to look at the above at daily and hourly scales





# maxair monthly graphs to ID potential outliers ----

by_month_all_sta(mmr, maxair, "Monthly Maximum Air Temperature (C)")  
# Possible issues:
# - 49 in 2003-01 - value > 40
# - 50 in 2003-02 - value < 10
# - 49 in 2000-07 - value > 44
# - 32 in 2004-10 - value < 15 (but this may be ok...because it matched general pattern but colder in mountains)





# ppt monthly graphs to ID potential outliers ----

by_month_all_sta(mmr, ppt, "Monthly Total Precipitation (mm)")  
# not easy to discern any obvious outliers
# check 42 in July of 1997 and 1998, but likely ok.







# focus in on the potential issues at daily and hourly scales ----



# station 50 airt and minair in 2012-09 and 2012-10:

# daily
mdr %>% 
  filter(month(date) %in% c('9', '10') & year(date) == '2012' & sta == '50') %>% 
  select(sta:maxair) %>% 
  pivot_longer(airt:maxair) %>% 
  ggplot(aes(x = date, y = value, color = name)) +
  geom_line() +
  geom_point(color = 'red') +
  facet_wrap(~ name, ncol = 1)

  # -40s for minair(filter in met processing program cuts off at -40) don't make sense in Sept. and Oct., 
  # but the patterns generally match airt
  #
  # It seems a little hard to believe that mean airt is < 0 for several days in Sept. and Oct. 

# hourly
mhr %>% 
  filter(month(date) %in% c('9', '10') & year(date) == '2012' & sta == '50') %>% 
  select(sta:minair) %>% 
  pivot_longer(airt:minair) %>% 
  ggplot(aes(x = dt, y = value, color = name)) +
  geom_line() +
  geom_point(color = 'red', size = 0.2) +
  facet_wrap(~ name, ncol = 1)

  # Yeah, so given the pattern of the hourly graph for the temp vars, I suspect the temp sensor
  # must have been flaky

# looking at hourly during this time for all stations
mhr %>% 
  filter(month(date) %in% c('9', '10') & year(date) == '2012') %>% 
  select(sta:minair) %>% 
  pivot_longer(airt:minair) %>% 
  ggplot(aes(x = dt, y = value, color = name)) +
  geom_line() +
  facet_grid(name ~ sta)

  # That lends support to the flaky sensor idea. Could filter sta 50 so that any variable less than 
  # the minimum of the other stations is made NA as a solution to the problem. It looks like 50 was 
  # still collecting decent data for many of the hourly records during the time period.







  
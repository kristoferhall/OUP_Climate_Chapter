# KM Hall
#
# Meteorology basic analyses for OUP climate chapter.
#
# This script does not use specialized time series R packages to look at some of the
# questions of interest to be addressed in the climate chapter.



library(tidyverse)
library(lubridate)

path_to_files <- "./data/processed_data/"



# yearly data ------------------------------------------------

y <- read_csv(paste0(path_to_files, "met_yearly_gap_filled.csv")) %>% 
  mutate(sta = as.factor(sta))

summary(y)


# airt
ggplot(y, aes(x = year, y = airt, color = sta)) +
  geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(~ sta) +
  theme(legend.position="none") +
  ggtitle("Yearly Mean Air Temperature (C)")

y %>% 
  ggplot(aes(x = sta, y = airt, color = sta)) +
    geom_boxplot() +
  labs(title = "Mean Annual Air Temperature by Station",
       x = "Station",
       y = "Air Temp (C) ") +
  theme(legend.position="none")

y %>% 
  ggplot() +
  geom_histogram(aes(x = airt, fill = sta), bins = 30) +
  facet_wrap(~ sta) +
  theme(legend.position="none") +
  labs(title = "Distribution of Mean Annual Air Temperature",
       x = "Air Temp (C)",
       y = "Count")



# minair
ggplot(y, aes(x = year, y = minair, color = sta)) +
  geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(~ sta) +
  theme(legend.position="none") +
  ggtitle("Yearly Minimum Air Temperature (C)")

y %>% 
  ggplot(aes(x = sta, y = minair, color = sta)) +
  geom_boxplot() +
  labs(title = "Minimul Annual Air Temperature by Station",
       x = "Station",
       y = "Minimum Air Temp (C) ") +
  theme(legend.position="none")

y %>% 
  ggplot() +
  geom_histogram(aes(x = minair, fill = sta), bins = 30) +
  facet_wrap(~ sta) +
  theme(legend.position="none") +
  labs(title = "Distribution of Minimum Annual Air Temperature",
       x = "Air Temp (C)",
       y = "Count")





# maxair
ggplot(y, aes(x = year, y = maxair, color = sta)) +
  geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(~ sta) +
  theme(legend.position="none") +
  ggtitle("Yearly Maximum Air Temperature (C)")

y %>% 
  ggplot(aes(x = sta, y = maxair, color = sta)) +
  geom_boxplot() +
  labs(title = "Maximum Annual Air Temperature by Station",
       x = "Station",
       y = "Maximum Air Temp (C) ") +
  theme(legend.position="none")

y %>% 
  ggplot() +
  geom_histogram(aes(x = maxair, fill = sta), bins = 30) +
  facet_wrap(~ sta) +
  theme(legend.position="none") +
  labs(title = "Distribution of Maximum Annual Air Temperature",
       x = "Maximum Air Temp (C)",
       y = "Count")




# rh 
ggplot(y, aes(x = year, y = rh, color = sta)) +
  geom_line() +
  geom_smooth(method = "lm", color = "lightblue") +
  facet_wrap(~ sta) +
  theme(legend.position="none") +
  ggtitle("Annual Mean Relative Humidity")


y %>% 
  ggplot(aes(x = sta, y = rh, color = sta)) +
  geom_boxplot() +
  labs(title = "Mean Annual Relative Humidity by Station",
       x = "Station",
       y = "Relative Humidity (%) ") +
  theme(legend.position="none")


y %>% 
  ggplot() +
  geom_histogram(aes(x = rh, fill = sta), bins = 30) +
  facet_wrap(~ sta) +
  theme(legend.position="none") +
  labs(title = "Distribution of Mean Annual Relative Humidity",
       x = "Relative Humidity (%)",
       y = "Count")




# ppt
ggplot(y, aes(x = year, y = ppt, color = sta)) +
  geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(~ sta) +
  theme(legend.position="none") +
  ggtitle("Yearly Total Precipitation (mm)")

y %>% 
  ggplot(aes(x = sta, y = ppt, color = sta)) +
  geom_boxplot() +
  labs(title = "Total Annual Precipitation by Station",
       x = "Station",
       y = "Precipitation (mm)") +
  theme(legend.position="none")


y %>% 
  ggplot() +
  geom_histogram(aes(x = ppt, fill = sta), bins = 30) +
  facet_wrap(~ sta) +
  theme(legend.position="none") +
  labs(title = "Distribution of Total Annual Precipitation",
       x = "Precipitation (mm)",
       y = "Count")





# observe temp and ppt variables by station
y_met_vars_by_sta <- function(data, station) {
  data %>% 
    filter(sta == station) %>% 
    select(sta:ppt) %>% 
    pivot_longer(airt:ppt) %>% 
    ggplot(., aes(x = year, y = value, color = name)) +
    geom_line() +
    geom_smooth(method = "lm") +
    facet_wrap(~ name, scales = "free_y") +
    theme(legend.position="none") +
    ggtitle(paste0("Station ", {{ station }}))
}


y_met_vars_by_sta(y, "40")
y_met_vars_by_sta(y, "42")
y_met_vars_by_sta(y, "49")
y_met_vars_by_sta(y, "50")





# monthly data -------------------------------------------------------

m <- read_csv(paste0(path_to_files, "met_monthly_gap_filled.csv")) %>% 
  mutate(sta = as.factor(sta))

summary(m)


# airt
ggplot(m, aes(x = date, y = airt, color = sta)) +
  geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(~ sta) +
  theme(legend.position="none") +
  ggtitle("Monthly Mean Air Temperature (C)")


m %>% 
  ggplot(aes(x = sta, y = airt, color = sta)) +
  geom_boxplot() +
  labs(title = "Mean Monthly Air Temperature by Station \n(months combined)",
       x = "Station",
       y = "Air Temp (C) ")  +
  theme(legend.position="none")

m %>% 
  ggplot(aes(x = sta, y = airt, color = sta)) +
  geom_boxplot() +
  labs(title = "Mean Monthly Air Temperature by month",
       x = "Station",
       y = "Air Temp (C) ") +
  facet_wrap(~ month(date)) +
  theme(legend.position="none")

m %>% 
  group_by(month(date)) %>% 
  ggplot(aes(x = as.factor(month(date)), y = airt, color = sta)) +
  geom_boxplot() +
  labs(title = "Mean Monthly Air Temperature \nby Station and Month of Year",
       x = "Station",
       y = "Air Temp (C) ") +
  theme(legend.position="none") +
  facet_wrap(~ sta)


m %>% 
  ggplot() +
  geom_histogram(aes(x = airt, fill = sta), bins = 75) +
  facet_wrap(~ sta) +
  theme(legend.position="none") +
  labs(title = "Distribution of Mean Monthly Air Temperature",
       x = "Air Temp (C)",
       y = "Count")



# minair
ggplot(m, aes(x = date, y = minair, color = sta)) +
  geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(~ sta) +
  theme(legend.position="none") +
  ggtitle("Monthly Minimum Air Temperature (C)")


m %>% 
  ggplot(aes(x = sta, y = minair, color = sta)) +
  geom_boxplot() +
  labs(title = "Minimum Monthly Air Temperature by Station \n(months combined)",
       x = "Station",
       y = "Minimum Air Temp (C) ")  +
  theme(legend.position="none")

m %>% 
  ggplot(aes(x = sta, y = airt, color = sta)) +
  geom_boxplot() +
  labs(title = "Minimum Monthly Air Temperature by month",
       x = "Station",
       y = "MinimumAir Temp (C) ") +
  facet_wrap(~ month(date)) +
  theme(legend.position="none")

m %>% 
  group_by(month(date)) %>% 
  ggplot(aes(x = as.factor(month(date)), y = minair, color = sta)) +
  geom_boxplot() +
  labs(title = "Minimum Monthly Air Temperature \nby Station and Month of Year",
       x = "Station",
       y = "Minimum Air Temp (C) ") +
  theme(legend.position="none") +
  facet_wrap(~ sta)


m %>% 
  ggplot() +
  geom_histogram(aes(x = minair, fill = sta), bins = 75) +
  facet_wrap(~ sta) +
  theme(legend.position="none") +
  labs(title = "Distribution of Minimum Monthly Air Temperature",
       x = "Air Temp (C)",
       y = "Count")




# maxair
ggplot(m, aes(x = date, y = maxair, color = sta)) +
  geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(~ sta) +
  theme(legend.position="none") +
  ggtitle("Monthly Maximum Air Temperature (C)")

m %>% 
  ggplot(aes(x = sta, y = maxair, color = sta)) +
  geom_boxplot() +
  labs(title = "Maximum Monthly Air Temperature by Station \n(months combined)",
       x = "Station",
       y = "Maximum Air Temp (C) ")  +
  theme(legend.position="none")

m %>% 
  ggplot(aes(x = sta, y = maxair, color = sta)) +
  geom_boxplot() +
  labs(title = "Maximum Monthly Air Temperature by month",
       x = "Station",
       y = "Maximum Air Temp (C) ") +
  facet_wrap(~ month(date)) +
  theme(legend.position="none")

m %>% 
  group_by(month(date)) %>% 
  ggplot(aes(x = as.factor(month(date)), y = maxair, color = sta)) +
  geom_boxplot() +
  labs(title = "Maximum Monthly Air Temperature \nby Station and Month of Year",
       x = "Station",
       y = "Maximum Air Temp (C) ") +
  theme(legend.position="none") +
  facet_wrap(~ sta)


m %>% 
  ggplot() +
  geom_histogram(aes(x = maxair, fill = sta), bins = 75) +
  facet_wrap(~ sta) +
  theme(legend.position="none") +
  labs(title = "Distribution of Maximum Monthly Air Temperature",
       x = "Air Temp (C)",
       y = "Count")



# rh 
ggplot(m, aes(x = date, y = rh, color = sta)) +
  geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(~ sta) +
  theme(legend.position="none") +
  ggtitle("Mean Relative Humidity")

m %>% 
  ggplot(aes(x = sta, y = rh, color = sta)) +
  geom_boxplot() +
  labs(title = "Mean Relative Humidity by Station \n(months combined)",
       x = "Station",
       y = "Relative Humidity (%)")  +
  theme(legend.position="none")

m %>% 
  ggplot(aes(x = sta, y = rh, color = sta)) +
  geom_boxplot() +
  labs(title = "Monthly Mean Relative Humidity by month",
       x = "Station",
       y = "Relative Humidity (%)") +
  facet_wrap(~ month(date)) +
  theme(legend.position="none")

m %>% 
  group_by(month(date)) %>% 
  ggplot(aes(x = as.factor(month(date)), y = rh, color = sta)) +
  geom_boxplot() +
  labs(title = "Monthly Mean Relative Humidity \nby Station and Month of Year",
       x = "Station",
       y = "Relative Humidity (%)") +
  theme(legend.position="none") +
  facet_wrap(~ sta)


m %>% 
  ggplot() +
  geom_histogram(aes(x = rh, fill = sta), bins = 75) +
  facet_wrap(~ sta) +
  theme(legend.position="none") +
  labs(title = "Distribution of Monthly Mean Relative Humidity",
       x = "Relative Humidity (%",
       y = "Count")



# ppt
ggplot(m, aes(x = date, y = ppt, color = sta)) +
  geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(~ sta) +
  theme(legend.position="none") +
  ggtitle("Monthly Total Precipitation (mm)")

m %>% 
  ggplot(aes(x = sta, y = ppt, color = sta)) +
  geom_boxplot() +
  labs(title = "Monthly Total Precipitation by Station \n(months combined)",
       x = "Station",
       y = "Relative Humidity (%)")  +
  theme(legend.position="none")

m %>% 
  ggplot(aes(x = sta, y = ppt, color = sta)) +
  geom_boxplot() +
  labs(title = "Monthly Total Precipitation by month",
       x = "Station",
       y = "Precip (mm)") +
  facet_wrap(~ month(date)) +
  theme(legend.position="none")

m %>% 
  group_by(month(date)) %>% 
  ggplot(aes(x = as.factor(month(date)), y = ppt, color = sta)) +
  geom_boxplot() +
  labs(title = "Monthly Total Precipitation \nby Station and Month of Year",
       x = "Station",
       y = "Precip (mm)") +
  theme(legend.position="none") +
  facet_wrap(~ sta)


m %>% 
  ggplot() +
  geom_histogram(aes(x = ppt, fill = sta), bins = 75) +
  facet_wrap(~ sta) +
  theme(legend.position="none") +
  labs(title = "Distribution of Monthly Total Precipitation",
       x = "Precip (mm)",
       y = "Count")








# observe temp and ppt variables by station
m_met_vars_by_sta <- function(data, station) {
  data %>% 
    filter(sta == station) %>% 
    select(sta:ppt) %>% 
    pivot_longer(airt:ppt) %>% 
    ggplot(., aes(x = date, y = value, color = name)) +
    geom_line() +
    geom_smooth(method = "lm") +
    facet_wrap(~ name, scales = "free_y") +
    theme(legend.position="none") +
    ggtitle(paste0("Station ", {{ station }}, " - Monthly"))
}

m_met_vars_by_sta(m, "40")
m_met_vars_by_sta(m, "42")
m_met_vars_by_sta(m, "49")
m_met_vars_by_sta(m, "50")



# by month of year all stations 
by_month_all_sta <- function(data, var, title) {
  ggplot(data, aes(x = year, y = {{ var }}, color = sta)) +
    geom_line() +
    facet_wrap(~ month, scales = "free_y") +
    ggtitle({{ title }})
}

by_month_all_sta(m, airt, "Monthly Mean Air Temperature (C)")  
by_month_all_sta(m, maxair, "Monthly Maximum Air Temperature (C)")  
by_month_all_sta(m, minair, "Monthly Minimum Air Temperature (C)")  
by_month_all_sta(m, ppt, "Monthly Total Precipitation (mm)")  



# by month of year for individual stations and their vars with lm
by_month_lm <- function(data, var, station, title) {
  m %>% 
    filter(sta == station) %>% 
    ggplot(., aes(x = year, y = {{ var }})) +
    geom_line() +
    geom_smooth(method = "lm") +
    facet_wrap(~ month, scales = "free_y") +
    ggtitle({{ title }})
}

by_month_lm(m, airt, "40", "Station 40 Monthly Mean Air Temperature (C)")
by_month_lm(m, airt, "42", "Station 42 Monthly Mean Air Temperature (C)")
by_month_lm(m, airt, "49", "Station 49 Monthly Mean Air Temperature (C)")
by_month_lm(m, airt, "50", "Station 50 Monthly Mean Air Temperature (C)")

by_month_lm(m, minair, "40", "Station 40 Monthly Minimum Air Temperature (C)")
by_month_lm(m, minair, "42", "Station 42 Monthly Minimum Air Temperature (C)")
by_month_lm(m, minair, "49", "Station 49 Monthly Minimum Air Temperature (C)")
by_month_lm(m, minair, "50", "Station 50 Monthly Minimum Air Temperature (C)")

by_month_lm(m, maxair, "40", "Station 40 Monthly Maximum Air Temperature (C)")
by_month_lm(m, maxair, "42", "Station 42 Monthly Maximum Air Temperature (C)")
by_month_lm(m, maxair, "49", "Station 49 Monthly Maximum Air Temperature (C)")
by_month_lm(m, maxair, "50", "Station 50 Monthly Maximum Air Temperature (C)")

by_month_lm(m, rh, "40", "Station 40 Monthly Mean Relative Humidity (%)")
by_month_lm(m, rh, "42", "Station 42 Monthly Mean Relative Humidity (%)")
by_month_lm(m, rh, "49", "Station 49 Monthly Mean Relative Humidity (%)")
by_month_lm(m, rh, "50", "Station 50 Monthly Mean Relative Humidity (%)")

by_month_lm(m, ppt, "40", "Station 40 Monthly Precipitation (mm)")
by_month_lm(m, ppt, "42", "Station 42 Monthly Precipitation (mm)")
by_month_lm(m, ppt, "49", "Station 49 Monthly Precipitation (mm)")
by_month_lm(m, ppt, "50", "Station 50 Monthly Precipitation (mm)")




# daily data ----------------------------------------------------
d <- read_csv(paste0(path_to_files, "met_daily_gap_filled.csv")) %>% 
  mutate(sta = as.factor(sta))

summary(d)

# airt
ggplot(d, aes(x = date, y = airt, color = sta)) +
  geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(~ sta) +
  theme(legend.position="none") +
  ggtitle("Daily Mean Air Temperature (C)")


# minair
ggplot(d, aes(x = date, y = minair, color = sta)) +
  geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(~ sta) +
  theme(legend.position="none") +
  ggtitle("Daily Minimum Air Temperature (C)")


# maxair
ggplot(d, aes(x = date, y = maxair, color = sta)) +
  geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(~ sta) +
  theme(legend.position="none") +
  ggtitle("Daily Maximum Air Temperature (C)")

# rh
ggplot(d, aes(x = date, y = rh, color = sta)) +
  geom_line() +
  geom_smooth(method = "lm", color = "black") +
  facet_wrap(~ sta) +
  theme(legend.position="none") +
  ggtitle("Daily Mean Relative Humidity (%)")


# ppt
ggplot(d, aes(x = date, y = ppt, color = sta)) +
  geom_point(size = 0.2) +
  facet_wrap(~ sta) +
  theme(legend.position="none") +
  ggtitle("Daily Total Precipitation (mm)")



# look at data during the monsoon period -
# defined in Petrie et al. (2014) as DOY 181-273. 

d_monsoon <- d %>% 
  mutate(DOY = yday(date)) %>% 
  filter(DOY >= 181 & DOY <= 273)


# to aid in plotting monsoon data, taking the daily data, summarizing
# it by year, and then plotting
y_monsoon <- d_monsoon %>% 
  mutate(year = year(date)) %>% 
  group_by(sta, year) %>% 
  summarize(airt = mean(airt),
            minair = min(minair),
            maxair = max(maxair),
            ppt = sum(ppt))


y_monsoon %>% 
  ggplot(., aes(x = year, y = airt, color = sta)) +
  geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(~ sta) + 
  ggtitle("Monsoon Mean Air Temperature (C)")
  
y_monsoon %>% 
  ggplot(., aes(x = year, y = minair, color = sta)) +
  geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(~ sta) + 
  ggtitle("Monsoon Minimum Air Temperature (C)")

y_monsoon %>% 
  ggplot(., aes(x = year, y = maxair, color = sta)) +
  geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(~ sta) + 
  ggtitle("Monsoon Maximum Air Temperature (C)")

y_monsoon %>% 
  ggplot(., aes(x = year, y = ppt, color = sta)) +
  geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(~ sta) + 
  ggtitle("Monsoon Total Precipitation (mm)")





# solar data and nighttime temps ------------------------------

# h <- read_csv(paste0(path_to_files, "met_hourly_gap_filled.csv"))

# From Tomer:
# It is better to adjust for nighttime hours by light. 
# Night can be defined as the time when incoming SW radiation 
# is very low - so less than about 50 W/m^2. 
# If you have your data and SWin in the same file, you could do 
# it easily. If not, since this is predictable - only a function of 
# sun and location, I can use our long time SWin data and tell you 
# when the night starts and ends. I can do it for every month in an 
# accuracy of 30 minutes or even create 365 values for start and end 
# times for each day of the year. 
  
# Looking to convert, but SEV data is labelled as units of 
# joulePerCentimeterSquared.
#
# Online, most conversions show joules per second per centimeter squared to
# Watts per meter squared. 
#
# Not sure how hourly data is presented and whether it shows an 
# accumulation over the hour or something else...


# trying a conversion to see whether I can make sense of the data
# h_sol <- h %>% 
#   select(sta:minair, sol:maxsol) %>% 
#   mutate(sol_W_per_sq_m = )















# KM Hall
#
# Analyzing SEV met data using tidyquant 


library(tidyverse)
library(tidyquant)


path_to_files <- "./data/processed_data/"



# Yearly Data ----  

y <- read_csv(paste0(path_to_files, "met_yearly_gap_filled.csv")) %>% 
  mutate(sta = as.factor(sta))

summary(y)


# barchart from tidyquant
y %>% 
  ggplot(aes(x = year, y = airt)) +
  geom_barchart(aes(open = airt, high = maxair, low = minair, close = airt)) +
  labs(title = "Annual Air Temperature (C)",
       y = "Air Temp (C)",
       x = "Year") +
  facet_wrap(~ sta)


# candlestick from tidyquant
y %>% 
  ggplot(aes(x = year, y = airt)) +
  geom_candlestick(aes(open = airt, high = maxair, low = minair, close = airt)) +
  labs(title = "Annual Air Temperature (C)",
       y = "Air Temp (C)",
       x = "Year") +
  facet_wrap(~ sta) +
  theme_tq()



# might be more interesting with monthly data ----
m <- read_csv(paste0(path_to_files, "met_monthly_gap_filled.csv")) %>% 
  mutate(sta = as.factor(sta))

summary(m)


# barchart from tidyquant
end = max(m$date)
start = end - weeks(156)
m %>% 
  filter(date >= start - days(2 * 36)) %>% 
  ggplot(aes(x = date, y = airt, open = airt, high = maxair, low = minair, close = airt)) +
  geom_point() +
  labs(title = "Monthly Air Temperature (C)",
       y = "Air Temp (C)",
       x = "Date") +
  facet_wrap(~ sta) +
  geom_ma(ma_fun = SMA, n = 8, linetype = 5) +
  geom_ma(ma_fun = SMA, n = 36, color = "aquamarine") +
  geom_bbands(ma_fun = SMA, sd = 2, n = 20)







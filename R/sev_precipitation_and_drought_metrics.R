# KM Hall
#
# Looking at continuous dry days, precipitation events, and their extremes
#
# Metrics based on Knapp et al. (2015) in Global Change Biology



library(tidyverse)
library(ggtext)
library(lubridate)

# set threshold for the minimum size of an "event" -------------------

# Knapp et al. (2015) GCB classify an event as >= 0.3 mm of ppt
threshold <- 0.3



# load data -----------------------------------------------------------

# only keeping data from 1990 forward because of some ppt data issues early on
# that I'm not totally comfortable with
met <- read_csv("data/processed_data/met_daily_gap_filled.csv") %>% 
  select(sta, date, ppt) %>% 
  mutate(sta = as.factor(sta)) %>% 
  filter(year(date) >= 1990)





# initial graphical look at precipitation --------------------------------------

# daily
ggplot(met, aes(x = date, y = ppt, color = sta)) +
  geom_line(size = 0.2) +
  facet_wrap(~ sta) +
  labs(title = "SEV Daily Precipitation",
       x = "Date",
       y = "Precipitation (mm)") +
  theme(legend.position = "none")


# monthly
met %>% 
  mutate(month_date = ymd(paste0(year(date), "-", month(date), "-01"))) %>% 
  group_by(sta, month_date) %>% 
  summarize(ppt_mthly = sum(ppt, na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot(., aes(x = month_date, y = ppt_mthly, color = sta)) +
  geom_line(size = 0.4) +
  facet_wrap(~ sta) +
  labs(title = "SEV Monthly Precipitation",
       x = "Date",
       y = "Precipitation (mm)") +
  theme(legend.position = "none")


# yearly 
met %>% 
  mutate(year = year(date)) %>% 
  group_by(sta, year) %>% 
  summarize(ppt = sum(ppt, na.rm = TRUE)) %>% 
  ungroup() %>% 
  ggplot(., aes(x = year, y = ppt, color = sta)) +
  geom_line(size = 0.4) +
  facet_wrap(~ sta) +
  labs(title = "SEV Yearly Precipitation",
       x = "Date",
       y = "Precipitation (mm)") +
  theme(legend.position = "none")






# calculate mean annual precipitation (MAP) and look at yearly variability  ----------

# annual precip
met_annual_ppt <- met %>% 
  mutate(year = year(date)) %>% 
  group_by(sta, year) %>% 
  summarize(ppt = sum(ppt, na.rm = TRUE)) %>% 
  ungroup()


# calculate MAP accross all years for each station
met_map <- met_annual_ppt %>% 
  group_by(sta) %>% 
  summarize(map = mean(ppt, na.rm = TRUE)) %>% 
  ungroup()


# left join met_map to met_annual_ppt and calculate diff from MAP then graph
met_annual_ppt <- met_annual_ppt %>% 
  left_join(met_map) %>% 
  mutate(diff_from_map = ppt - map)

met_annual_ppt %>% 
  filter(year >= 1990) %>% 
  ggplot(aes(x = year, y = diff_from_map, color = sta)) + 
  geom_hline(yintercept = 0, color = "black", size = 0.3) +
  geom_line(size = 0.7) +
  facet_wrap(~ sta) +
  labs(title = "Difference from MAP",
       x = "Year",
       y = "Difference from MAP (mm)") +
  theme(legend.position = "none")
# there aren't any multi-year runs of above/below MAP, but they are all short time-series





# Classify ppt events -------------------------------------------------------

# Knapp et al. (2015) use >= 0.3 mm to classify a daily precipitation "event".
# Anything less that 0.3 is not an event, and end up being classified as 0.
#
# I am going to keep events < 0.3 mm and use a separate var to be able to see
# how much of a diff it makes in our system.

# using 'threshold' defined at top of script - allows you to play around with different
# thresholds, if needed
events <- met %>% 
  mutate(is_event = ifelse(ppt >= threshold, TRUE, FALSE),
         ppt_no_small = ifelse(ppt < threshold, 0, ppt),
         small_diff = ppt - ppt_no_small)


# look at how much of an effect small ppt events make
events %>% 
  mutate(year = year(date)) %>% 
  group_by(sta, year) %>% 
  summarize(small_diffs_total = sum(small_diff)) %>% 
  ggplot(aes(x = year, y = small_diffs_total, col = sta)) +
  geom_line(size = 0.5) +
  facet_wrap(~ sta) +
  labs(title = "Annual sum of small ppt events of < 0.3 mm",
       x = "Year",
       y = "Total of small events (mm)") +
  theme(legend.position = "none")
# Probably insignificant, but could depend when the small events happen in the year
# or what time of day.
# I don't know whether 0.3 is a good threshold or not. From what think I've heard, 
# biocrusts may respond to 0.2 (?) mm events? But probably not much, and there aren't
# that many small events in any given year.
  



# Classify Extreme Events ------------------------------------------------

# Knapp et al. (2015) classify an extreme event as a daily ppt amount exceeding
# the 99th %-ile of historic record (granted they use a 100 year record, but I
# only have a few decades for each met station)

extreme_event <- events %>% 
  group_by(sta) %>% 
  summarize(extreme_threshold = quantile(ppt_no_small, 0.99))


# add flag to events
events <- events %>% 
  left_join(extreme_event) %>% 
  mutate(is_extreme = ifelse(ppt_no_small > extreme_threshold, TRUE, FALSE))





# Number of events and extreme events per month and per year -------------------------------------

events_per_month <- events %>% 
  mutate(month_date = ymd(paste0(year(date), "-", month(date), "-01"))) %>% 
  group_by(sta, month_date) %>% 
  summarize(number_of_events = sum(is_event),
            number_of_extreme_events = sum(is_extreme)) %>% 
  ungroup()


events_per_year <- events_per_month %>% 
  mutate(year = year(month_date)) %>% 
  group_by(sta, year) %>% 
  summarize(number_of_events = sum(number_of_events),
            number_of_extreme_events = sum(number_of_extreme_events)) %>% 
  ungroup()


# graphs of # events/month
events_per_month %>% 
  ggplot(aes(x = month_date, y = number_of_events, color = sta)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ sta) +
  labs(title = "Number of Monthly Precipitation Events",
       x = "Date",
       y = "Number of Events") +
  theme(legend.position = "none")

events_per_month %>% 
  ggplot(aes(x = month_date, y = number_of_events, color = sta)) +
  geom_point(size = 0.2) +
  facet_wrap(~ sta) +
  labs(title = "Number of Monthly Precipitation Events",
       x = "Date",
       y = "Number of Events") +
  theme(legend.position = "none")

events_per_month %>% 
  ggplot(aes(x = month_date, y = number_of_extreme_events, color = sta)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ sta) +
  labs(title = "Number of EXTREME Monthly Precipitation Events",
       x = "Date",
       y = "Number of Extreme Events") +
  theme(legend.position = "none")

events_per_month %>% 
  ggplot(aes(x = month_date, y = number_of_extreme_events, color = sta)) +
  geom_point(size = 0.2) +
  facet_wrap(~ sta) +
  labs(title = "Number of EXTREME Monthly Precipitation Events",
       x = "Date",
       y = "Number of Extreme Events") +
  theme(legend.position = "none")

# graphs of # events/year
events_per_year %>% 
  ggplot(aes(x = year, y = number_of_events, color = sta)) +
  geom_line(size = 0.5) +
  facet_wrap(~ sta) +
  labs(title = "Number of Annual Precipitation Events",
       x = "Year",
       y = "Number of Events") +
  theme(legend.position = "none")

events_per_year %>% 
  ggplot(aes(x = year, y = number_of_extreme_events, color = sta)) +
  geom_line(size = 0.5) +
  facet_wrap(~ sta) +
  labs(title = "Number of EXTREME Annual Precipitation Events",
       x = "Date",
       y = "Number of Extreme Events") +
  theme(legend.position = "none")



# graphs of events by month of year
events_per_month %>% 
  mutate(month = month(month_date)) %>% 
  ggplot(aes(x = month_date, y = number_of_events, color = sta)) +
  geom_line(size = 0.2) +
  facet_wrap(~ month) +
  labs(title = "Number of Precipitation Events by Month",
       x = "Year",
       y = "Number of Monthly Events")

events_per_month %>% 
  mutate(month = month(month_date)) %>% 
  ggplot(aes(x = month_date, y = number_of_extreme_events, color = sta)) +
  geom_line(size = 0.2) +
  facet_wrap(~ month) +
  labs(title = "Number of EXTREME Precipitation Events by Month",
       x = "Year",
       y = "Number of Extreme Monthly Events")
# These are kind of cool! Could clean them up more and separate by station




























  
  
  
  









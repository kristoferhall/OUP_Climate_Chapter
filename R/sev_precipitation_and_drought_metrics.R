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
  geom_smooth(method = "lm", se = FALSE, size = 0.6) +
  facet_wrap(~ sta) +
  labs(title = "Number of Annual Precipitation Events",
       x = "Year",
       y = "Number of Events") +
  theme(legend.position = "none")

events_per_year %>% 
  ggplot(aes(x = year, y = number_of_extreme_events, color = sta)) +
  geom_line(size = 0.5) +
  geom_smooth(method = "lm", se = FALSE, size = 0.6) +
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
  geom_smooth(method = "lm", se = FALSE, size = 0.2) +
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





# Calculate Consecutive Dry Days ------------------------------------------------

# Knapp et al. (2015) define the following:
#  - Consecutive Dry Days (CDD) = the average number of days in the dry period
#       between precipitation events
#  - Extreme CDDs = the number of dry periods that exceeded in length (in days) 
#       the 95th %-ile of all dry periods in the historic record.



# I generally think this approach is pretty decent. It has 2 small flaws (that I know of):
#  1. It misses 0 ppt days at the beginning of the record up until the first
#     event at the start of the data. e.g. - if the first event is on Jan. 15 at the beginning, 
#     it misses a CDD event of 14 days. This is only an issue at the start, and is quite minor.
#  2. It doesn't take into account transitions between years. Therefore, you have
#     to classify a CDD event that spans years into the year of the event start or end.


cdd <- met %>%
  select(sta, date, ppt) %>%
  group_by(sta) %>% 
  filter(ppt >= threshold) %>%
  mutate(prev_date = lag(date, n=1)) %>%
  drop_na() %>%
  mutate(cdd = as.numeric(date - prev_date) - 1,
         year = year(date),
         start_date = prev_date + 1,
         end_date = date) 



cdd %>% 
  ggplot(aes(x = end_date, y = cdd, color = sta)) +
  geom_point(size = 0.5) +
  facet_wrap(~ sta) +
  labs(title = "Consecutive Dry Days (CDD) without Precipitation",
       x = "Date for End of CDD",
       y = "CDDs")

cdd %>% 
  ggplot(aes(x = cdd, fill = sta)) +
  geom_histogram() +
  facet_wrap(~ sta)
# highly right skewed




# annual mean CDD length
cdd_yrly_mean <- cdd %>% 
  group_by(sta, year) %>% 
  summarize(CDD_mean = mean(cdd)) %>% 
  ungroup()

cdd_yrly_mean %>% 
  ggplot(aes(x = year, y = CDD_mean, col = sta)) +
  geom_line(size = 0.5) +
  geom_smooth(method = "lm", se = FALSE, size = 0.4) +
  facet_wrap(~ sta) +
  labs(title = "Annual Average Length of Consecutive Dry Days",
       x = "Year",
       y = "Mean CDD Length")

# not as easy to calculate about monthly CDDs because of how often they might span months...

# this is a crude monthly CDD length - using end date to classify into a month
cdd_mthly_mean <- cdd %>% 
  mutate(month_date = ymd(paste0(year(end_date), "-", month(end_date), "-01"))) %>% 
  group_by(sta, month_date) %>% 
  summarize(CDD_mean = mean(cdd)) %>% 
  ungroup()

cdd_mthly_mean %>% 
  mutate(month = month(month_date)) %>% 
  ggplot(aes(x = month_date, y = CDD_mean, col = sta)) +
  geom_line() +
  facet_wrap(~ month)
# this doesn't really work, because it can pick up long periods that span more than
# a month
#
# TODO: Look at how to handle this - like a month start and month end and
#     then whether cdd start and end dates span it. But that doesn't get at the 
#     whole picture very well...


# mean and 95th %-ile CDD length over entire time span
cdd_mean <- cdd %>% 
  group_by(sta) %>% 
  summarize(CDD_mean = mean(cdd),
            CDD_95   = quantile(cdd, 0.95))


# add extreme cdd flag to cdd
cdd <- cdd %>% 
  left_join(cdd_mean) %>% 
  mutate(is_extreme_cdd = ifelse(cdd > CDD_95, TRUE, FALSE))


# cdd extremes by year
cdd_yrly_extremes <- cdd %>% 
  group_by(sta, year) %>% 
  summarize(number_of_extreme_cdds = sum(is_extreme_cdd))

ggplot(cdd_yrly_extremes, aes(x = year, y = number_of_extreme_cdds, col = sta)) +
  geom_line(size = 0.5) +
  facet_wrap(~ sta) +
  labs(title = "Number of Annual Extreme Consecutive Dry Day Periods",
       x = "Year",
       y = "Number of Extreme CDDs")





# Define extreme wet and dry years ----------------------------------------------

# Knapp et al. (2015) define the following:
#  - extreme wet year - ppt > 90th %-ile
#  - extreme dry year - ppt < 10th %-ile
#  - average year - ppt > 45th and < 55th %-ile
#
# I am adding:
#  - above average - >= 55th and <= 90th
#  - below average - <= 45th and >= 10th


wet_dry_years <- met_annual_ppt %>% 
  group_by(sta) %>% 
  summarize(extreme_wet = quantile(ppt, .9),
            extreme_dry = quantile(ppt, .1),
            avg_lo = quantile(ppt, .45),
            avg_hi = quantile(ppt, .55))



met_annual_ppt_classified <- met_annual_ppt %>% 
  left_join(wet_dry_years) %>% 
  mutate(year_type = ifelse(ppt > extreme_wet, 'extreme_wet',
                            ifelse(ppt < extreme_dry, 'extreme_dry',
                                   ifelse((ppt > avg_lo) & (ppt < avg_hi), 'average',
                                          ifelse((ppt >= extreme_dry) & (ppt <= avg_lo), 'below_average', 'above_average'))))) %>%
  left_join(events_per_year) %>% 
  left_join(cdd_yrly_mean) %>% 
  left_join(cdd_yrly_extremes) 



# met 40 as an initial look 
met_annual_ppt_classified %>% 
  filter(sta == '40' & year_type %in% c("extreme_wet", "average", "extreme_dry")) %>% 
  ggplot(aes(x = year, y = number_of_extreme_cdds)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ year_type)


met_annual_ppt_classified %>% 
  filter(sta == '40' & year_type %in% c("extreme_wet", "average", "extreme_dry")) %>% 
  ggplot(aes(x = year, y = CDD_mean)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ year_type)

met_annual_ppt_classified %>% 
  filter(sta == '40' & year_type %in% c("extreme_wet", "average", "extreme_dry")) %>% 
  ggplot(aes(x = year, y = number_of_extreme_events)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ year_type)

met_annual_ppt_classified %>% 
  filter(sta == '40' & year_type %in% c("extreme_wet", "average", "extreme_dry")) %>% 
  ggplot(aes(x = year, y = number_of_events)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ year_type)





met_annual_ppt_classified %>% 
  ggplot(aes(x = year, y = CDD_mean, col = year_type)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  facet_wrap(~ sta)

met_annual_ppt_classified %>% 
  ggplot(aes(x = year, y = number_of_extreme_cdds, col = year_type)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  facet_wrap(~ sta)

met_annual_ppt_classified %>% 
  ggplot(aes(x = year, y = number_of_events, col = year_type)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  facet_wrap(~ sta)

met_annual_ppt_classified %>% 
  ggplot(aes(x = year, y = number_of_extreme_events, col = year_type)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size = 0.5) +
  facet_wrap(~ sta)




met_annual_ppt_classified %>% 
  mutate(decade = ifelse(year < 2000, "1990-1999",
                         ifelse(year >= 2000 & year < 2010, "2000-2009", 
                                ifelse(year >= 2010 & year < 2020, "2010-2019", "2020-2021")))) %>% 
  ggplot() +
  geom_bar(aes(decade, fill = year_type)) +
  facet_wrap(~ sta)

met_annual_ppt_classified %>% 
  mutate(year_type_factor = factor(year_type,
                                      levels = c("extreme_dry",
                                                 "below_average",
                                                 "average",
                                                 "above_average",
                                                 "extreme_wet"))) %>% 
  mutate(decade = ifelse(year < 2000, "1990-1999",
                         ifelse(year >= 2000 & year < 2010, "2000-2009", 
                                ifelse(year >= 2010 & year < 2020, "2010-2019", "2020-2021")))) %>% 
  ggplot() +
  geom_bar(aes(decade, fill = year_type_factor)) +
  facet_wrap(~ sta) +
  labs(x = "Period",
       y = "Count")


met_annual_ppt_classified %>% 
  mutate(year_type_factor = factor(ifelse(year_type == "extreme_dry", 0,
                                          ifelse(year_type == "below_average", 1,
                                                 ifelse(year_type == "average", 2,
                                                        ifelse(year_type == "above_average", 3, 4)))))) %>%
  ggplot(aes(x = year, y = year_type_factor, color = year_type)) +
  geom_point() +
  facet_wrap(~ sta) +
  labs(x = "Period",
       y = "Count")






















  
  
  
  









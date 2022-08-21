# KM Hall
#
# OUP Climate - The initial gap method for precipitation was to use 0 fill on hourly data. 
# This program considers how often there is precipitation > 0 on both sides of an NA record in order
# to guage whether 0 fill is the best method. The reason for gap filling is that most
# time-series analyses require complete records without any NAs. 



library(tidyverse)
library(lubridate)

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


str(mhr)



# initial inspection of precipitation data -----------------------

ggplot(mhr, aes(x = dt, y = ppt, color = sta)) +
  geom_point(size=0.1, alpha=0.5) +
  facet_wrap(~ sta)

mhr %>% 
  mutate(missing_data = is.na(ppt)) %>% 
  ggplot(., aes(x = dt, y = ppt, color = missing_data)) +
  geom_point(size=0.4) +
  facet_wrap(~ sta)

# the plots don't yield insight with so many data points



# get records with ppt NA -----------------------

ppt_na <- mhr %>%
  filter(is.na(ppt)) %>% 
  select(sta, dt, ppt) 

# there are 3672 records with missing ppt 



# see whether it is possible to pinpoint times for missing records to some extent -----------

ppt_m_na <- ppt_na %>% 
  mutate(month_date = ymd(paste0(year(dt), "-", month(dt), "-01"))) %>% 
  group_by(sta, month_date) %>% 
  summarize(na_per_month = sum(is.na(ppt)))

ggplot(ppt_m_na, aes(x=month_date, y=na_per_month, color=sta)) +
  geom_line(alpha = 0.6) +
  facet_wrap(~ sta)

# Most of the NAs are are for sta 40 prior to 1990 and for 42 prior to 1993. These are early on
# in the met stations' collection time periods, although there are some additional missing values.
# later on in their time series. sta 49 has almost no NAs. 50 has a few, but relatively insignificant.
#
#
# My initial impression is that it might be best to not use precip data for 40 and 42 until after
# the missing data issues resolved in the early years of data collection. Will consult others
# for further thoughts. 





# looking at hour previous and post to NAs --------------------------

# doing this to get a better sense of how often a value is just missing for an hour

ppt_pre_post <- ppt_na %>% 
  mutate(pre_hour = dt - dhours(1),
         post_hour = dt + dhours(1))

# joining data for 1 hour before and 1 hour after NA records
ppt_h_pre <- mhr %>% 
  select(sta, dt, ppt) %>% 
  mutate(pre_hour = dt,
         ppt_pre = ppt) %>% 
  select(-ppt, -dt)

ppt_h_post <- mhr %>% 
  select(sta, dt, ppt) %>% 
  mutate(post_hour = dt,
         ppt_post = ppt) %>% 
  select(-ppt, -dt)


ppt_pre_post <- ppt_pre_post %>% 
  left_join(ppt_h_pre)

ppt_pre_post <- ppt_pre_post %>% 
  left_join(ppt_h_post)

# making some flags for comparison

ppt_pre_post <- ppt_pre_post %>% 
  mutate(pre_miss = ifelse(is.na(ppt_pre), TRUE, FALSE),
         post_miss = ifelse(is.na(ppt_post), TRUE, FALSE),
         both_miss = ifelse(pre_miss == TRUE & post_miss == TRUE, TRUE, FALSE),
         both_present = ifelse(pre_miss == FALSE & post_miss == FALSE, TRUE, FALSE))


# see how often flags set TRUE
ppt_pre_post %>% 
  group_by(sta) %>% 
  summarize(n_rec = n(),
            both_miss_total = sum(both_miss),
            both_present_total = sum(both_present),      
            both_miss_percent = sum(both_miss) / n_rec * 100,
            both_present_percent = sum(both_present) / n_rec * 100)



# # A tibble: 4 × 6
# sta   n_rec both_miss_total both_present_total both_miss_percent both_present_percent
# <fct> <int>           <int>              <int>             <dbl>                <dbl>
#   1 40     2775            2750                  7              99.1                0.252
# 2 42      761             742                  6              97.5                0.788
# 3 49       16              16                  0             100                  0    
# 4 50      120             105                  3              87.5                2.5  



# see how often there is precip in pre or post hour when both records are present
ppt_pre_post %>% 
  filter(both_present == TRUE & ppt_pre > 0 & ppt_post > 0)


# There are no occurances with the above criteria. No reason to gap fill with anything other than
# a 0 value for data. 
#
# Need to consider whether or not to delete early records for 40 and 42 given how many missing
# values there are early on. 



# looking at the data problems early on for 40 and 42 ------------------  

early <- mhr %>% 
  filter(sta %in% c("40", "42") & year(dt) < '1993') %>% 
  select(sta, dt, ppt)
  
View(early %>% 
  filter(!is.na(ppt)))

early %>% 
  filter(!is.na(ppt)) %>% 
  ggplot(aes(x=dt, y=ppt)) +
  geom_point(size=0.4) +
  facet_wrap(~sta)

early %>% 
  mutate(ppt_miss = ifelse(is.na(ppt), TRUE, FALSE)) %>% 
  ggplot(aes(x=dt, y=ppt_miss, color=ppt_miss)) +
  geom_point(size=0.4, alpha=0.5) +
  facet_wrap(~sta)

# see how gap filling with 0 affects results
early_month <- early %>% 
  mutate(ppt_gf = ifelse(is.na(ppt), 0, ppt),
         year = year(dt),
         month = month(dt)) %>% 
  group_by(sta, year, month) %>% 
  summarize(ppt_no_gf = sum(ppt, na.rm = TRUE),
            ppt_0_gf = sum(ppt_gf, na.rm = TRUE),
            diff = ppt_no_gf - ppt_0_gf,
            n_miss = sum(is.na(ppt), na.rm = TRUE))

# zero filling does not affect ppt early on for 40 and 42. 



# looking at how 0 gf affects ppt more broadly -----------------------

ppt_h <- mhr %>% 
  select(sta, dt, ppt) %>% 
  mutate(ppt_gf = ifelse(is.na(ppt), 0, ppt))

ppt_m <- ppt_h %>% 
  mutate(year = year(dt),
         month = month(dt)) %>% 
  group_by(sta, year, month) %>% 
  summarize(ppt_no_gf = sum(ppt, na.rm = TRUE),
            ppt_0_gf = sum(ppt_gf, na.rm = TRUE),
            diff = ppt_no_gf - ppt_0_gf,
            n_miss = sum(is.na(ppt), na.rm = TRUE)) %>% 
  mutate(date = paste0(year, "-", month, "-01"))


ppt_m %>% filter(diff > 0)

# 0 gap fill has no affect on ppt

ppt_m %>% 
  group_by(sta, year) %>% 
  summarize(ppt = sum(ppt_0_gf)) %>% 
  ggplot(aes(x = year, y = ppt, color = sta)) +
  geom_line() +
  facet_wrap(~ sta)




# Final impressions: 0 gap filling is appropriate and has negligible effect
# on ppt








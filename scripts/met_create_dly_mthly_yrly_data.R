# KM Hall
#
# Create met data sets rolled up to daily, monthly and yearly levels



library(tidyverse)
library(lubridate)

# load met hourly data
met_h <- read_csv("./data/processed_data/met_hourly_gap_filled.csv") %>% 
  mutate(sta = as.factor(sta))


summary(met_h)
names(met_h)


# daily from hourly ----------------------------------------------
met_d <- met_h %>% 
  select(sta, dt, airt:maxsol, no_record_flag, airt_miss:maxsol_miss) %>% 
  mutate(date = as.Date(dt)) %>%        # need to do this because date is missing for missing records
  group_by(sta, date) %>% 
  summarize(airt = (max(maxair, na.rm = TRUE) + min(minair, na.rm = TRUE)) / 2,     # standard way to calculate mean temp....
            maxair = max(maxair, na.rm = TRUE), 
            minair = min(minair, na.rm = TRUE),
            ppt = sum(ppt, na.rm = TRUE),
            rh = mean(rh, na.rm = TRUE),
            sol_total = sum(sol, na.rm = TRUE),
            sol_mean = mean(sol, na.rm = TRUE),
            minsol = min(minsol, na.rm = TRUE),
            maxsol = max(maxsol, na.rm = TRUE),
            no_record_flag = sum(no_record_flag),
            airt_miss = sum(airt_miss, na.rm = TRUE),
            maxair_miss = sum(maxair_miss, na.rm = TRUE),
            minair_miss = sum(minair_miss, na.rm = TRUE),
            ppt_miss = sum(ppt_miss, na.rm = TRUE),
            rh_miss = sum(rh_miss, na.rm = TRUE),
            sol_miss = sum(sol_miss, na.rm = TRUE),
            minsol_miss = sum(minsol_miss, na.rm = TRUE),
            maxsol_miss = sum(maxsol_miss, na.rm = TRUE),
            number_hrly_records = n()) %>% 
  ungroup() %>% 
  mutate(sol_total = ifelse(sta == '49', NA, sol_total),
         sol_mean = ifelse(sta == '49', NA, sol_mean),
         minsol = ifelse(sta == '49', NA, minsol),
         maxsol = ifelse(sta == '49', NA, maxsol))
            

summary(met_d)
names(met_d)

# monthly from daily -----------------------------------------
met_m <- met_d %>% 
  select(sta, date, airt:maxsol) %>% 
  mutate(month = month(date),
         year = year(date),
         date = ymd(paste0(year, "-", month, "-01"))) %>%        # need to do this because date is missing for missing records
  group_by(sta, year, month, date) %>% 
  summarize(airt = mean(airt, na.rm = TRUE),
            maxair = max(maxair, na.rm = TRUE), 
            minair = min(minair, na.rm = TRUE),
            ppt = sum(ppt, na.rm = TRUE),
            rh = mean(rh, na.rm = TRUE),
            sol_total = sum(sol_total, na.rm = TRUE),
            sol_mean = mean(sol_mean, na.rm = TRUE),
            minsol = min(minsol, na.rm = TRUE),
            maxsol = max(maxsol, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(sol_total = ifelse(sta == '49', NA, sol_total),
         sol_mean = ifelse(sta == '49', NA, sol_mean),
         minsol = ifelse(sta == '49', NA, minsol),
         maxsol = ifelse(sta == '49', NA, maxsol))


# yearly from monthly ----------------------------------------
met_y<- met_m %>% 
  select(sta, year, airt:maxsol) %>% 
  group_by(sta, year) %>% 
  summarize(airt = mean(airt, na.rm = TRUE),
            maxair = max(maxair, na.rm = TRUE), 
            minair = min(minair, na.rm = TRUE),
            ppt = sum(ppt, na.rm = TRUE),
            rh = mean(rh, na.rm = TRUE),
            sol_total = sum(sol_total, na.rm = TRUE),
            sol_mean = sum(sol_mean, na.rm = TRUE),
            minsol = min(minsol, na.rm = TRUE),
            maxsol = max(maxsol, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(sol_total = ifelse(sta == '49', NA, sol_total),
         sol_mean = ifelse(sta == '49', NA, sol_mean),
         minsol = ifelse(sta == '49', NA, minsol),
         maxsol = ifelse(sta == '49', NA, maxsol))



# TODO: look up proper way to summarize solar vars at daily,
# monthly, and yearly scales


# write out files ------------------------------------

write_csv(met_d, "./data/processed_data/met_daily_gap_filled.csv")

write_csv(met_m, "./data/processed_data/met_monthly_gap_filled.csv")

write_csv(met_y, "./data/processed_data/met_yearly_gap_filled.csv")


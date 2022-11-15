# KM Hall
#
# SEV Vapor Pressure Deficit (VPD)
#



library(tidyverse)
library(lubridate)


# load hourly data
met_h <- read_csv("./data/processed_data/met_hourly_gap_filled.csv", guess_max = 1000000) %>% 
  mutate(sta = as.factor(sta))

glimpse(met_h)
summary(met_h)

# correct date variable and filter for >= 1990
met_h <- met_h %>% 
  mutate(date = as_date(dt),
         year = year(dt),
         month = month(dt)) %>% 
  filter(year(dt) >= 1990)

summary(met_h)


###############################################################################
# Source of equations is: https://en.wikipedia.org/wiki/Vapour-pressure_deficit
#  WILL want a more official source
###############################################################################

# # constants -----------------------------------------
# 
# A <- -1.0440397e4
# B <- -11.29465
# C <- -2.7022355e-2
# D <- 1.289036e-5
# E <- -2.4780681e-9
# F_ <- 6.5459673
# 
# # compute air temperature in the Rankine scale
# met_h <- met_h %>% 
#   mutate(T_Rankine = ((airt * (9/5)) + 32) + 459.67)
# 
# 
# # calculate hourly VP saturated and VP deficit
# met_h_vpd <- met_h %>% 
#   mutate(vp_sat = exp((A/T_Rankine) + B + (C * T_Rankine) + (D * T_Rankine^2) + (E * T_Rankine^3) + (F_ * log(T_Rankine))),
#          vpd = vp_sat * (1 - (rh/100))) %>% 
#   select(sta:airt, rh, T_Rankine:vpd)
# 
# 
# 
# ############################################################
# # Second method for VPD calculation
# # calculate hourly VP saturated and VP deficit
# #
# # source: doi:10.3390/rs9030294
# met_h_vpd <- met_h %>% 
#   mutate(vp_sat = 0.611 * exp(17.27 * (airt / (airt + 237.3))),
#          vpd = vp_sat - (vp_sat * (rh/100))) %>% 
#   select(sta:airt, rh, vp_sat:vpd)


#############################################################
# Third method for VPD calculation
# 
# from Alesia
# source: http://cronklab.wikidot.com/calculation-of-vapour-pressure-deficit
met_h_vpd <- met_h %>% 
  mutate(vp_sat = 610.7 * 10^((7.5 * airt)/(237.3 + airt)),
         vpd = (1-(rh/100)) * vp_sat) %>% 
  select(sta:airt, rh, vp_sat:vpd)


# daily -------------------------------------------
met_d_vpd <- met_h_vpd %>% 
  group_by(sta, date) %>% 
  summarize(vpd = mean(vpd)) %>% 
  ungroup()

met_d_vpd %>% 
  ggplot(aes(x = date, y = vpd, color = sta)) +
  geom_line() +
  facet_wrap(~ sta)


# monthly ----------------------------------------
met_m_vpd <- met_d_vpd %>% 
  mutate(month_date = ymd(paste0(year(date), "-", month(date), "-01"))) %>% 
  group_by(sta, month_date) %>% 
  summarize(vpd = mean(vpd)) %>% 
  ungroup()


met_m_vpd %>% 
  ggplot(aes(x = month_date, y = vpd, color = sta)) +
  geom_line() +
  facet_wrap(~ sta)



# yearly ----------------------------------------
met_y_vpd <- met_m_vpd %>% 
  mutate(year = year(month_date)) %>% 
  group_by(sta, year) %>% 
  summarize(vpd = mean(vpd)) %>% 
  ungroup()

met_y_vpd %>% 
  ggplot(aes(x = year, y = vpd, color = sta)) +
  geom_smooth(method = 'lm', se = FALSE, size = 0.6) +
  geom_line() +
  facet_wrap(~ sta)



# by month of year -----------------------------

# by month of year for individual stations and their vars with lm
by_month_lm <- function(data, var, station, title) {
  data %>% 
    filter(sta == station) %>% 
    ggplot(., aes(x = year, y = {{ var }})) +
    geom_line() +
    geom_smooth(method = "lm") +
    facet_wrap(~ month, scales = "free_y") +
    ggtitle({{ title }}) +
    labs(subtitle = "Y-axis is free")
}


met_m_vpd <- met_m_vpd %>% 
  mutate(year = year(month_date),
         month = month(month_date))
  

by_month_lm(met_m_vpd, vpd, "40", "Station 40 Monthly Mean Vapor Pressure Deficit (Pa)")
by_month_lm(met_m_vpd, vpd, "42", "Station 42 Monthly Mean Vapor Pressure Deficit (Pa)")
by_month_lm(met_m_vpd, vpd, "49", "Station 49 Monthly Mean Vapor Pressure Deficit (Pa)")
by_month_lm(met_m_vpd, vpd, "50", "Station 50 Monthly Mean Vapor Pressure Deficit (Pa)")







  
  
  
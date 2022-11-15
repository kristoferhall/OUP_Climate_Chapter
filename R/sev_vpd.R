# KM Hall
#
# SEV Vapor Pressure Deficit (VPD)
#



library(tidyverse)
library(lubridate)
library(nlme)
library(gridExtra)

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
         vpd = (1-(rh/100)) * vp_sat / 1000) %>% 
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
  facet_wrap(~ sta) +
  scale_color_viridis_d(option = "cividis") +
  labs(title = "Annual Mean Vapor Pressure",
       x = "Year",
       y = "VPD (kPa)") +
  theme_minimal()



# by month of year -----------------------------

# by month of year for individual stations and their vars with lm
by_month_lm <- function(data, var, station, title) {
  data %>% 
    filter(sta == station) %>% 
    ggplot(., aes(x = year, y = {{ var }})) +
    geom_line(alpha = 0.6) +
    geom_smooth(method = lm, se = FALSE, size = 0.5, color = "green") +
    geom_smooth(method = loess, se = FALSE, size = 0.5) +
    facet_wrap(~ month, scales = "free_y") +
    ggtitle({{ title }}) +
    labs(subtitle = "Y-axis is free") +
    theme_minimal()
}


met_m_vpd <- met_m_vpd %>% 
  mutate(year = year(month_date),
         month = month(month_date))
  

(m40_m_vpd <- by_month_lm(met_m_vpd, vpd, "40", "Station 40 Monthly Mean Vapor Pressure Deficit (kPa)"))
(m42_m_vpd <- by_month_lm(met_m_vpd, vpd, "42", "Station 42 Monthly Mean Vapor Pressure Deficit (kPa)"))
(m49_m_vpd <- by_month_lm(met_m_vpd, vpd, "49", "Station 49 Monthly Mean Vapor Pressure Deficit (kPa)"))
(m50_m_vpd <- by_month_lm(met_m_vpd, vpd, "50", "Station 50 Monthly Mean Vapor Pressure Deficit (kPa)"))


# saves previous graph
ggsave(filename = paste0("figures/m40_bymonth_vpd", ".jpg"),
       plot = m40_m_vpd,
       dpi = 300,
       width = 10,
       height = 4)

# saves previous graph
ggsave(filename = paste0("figures/m42_bymonth_vpd", ".jpg"),
       plot = m42_m_vpd,
       dpi = 300,
       width = 10,
       height = 4)

# saves previous graph
ggsave(filename = paste0("figures/m49_bymonth_vpd", ".jpg"),
       plot = m49_m_vpd,
       dpi = 300,
       width = 10,
       height = 4)

# saves previous graph
ggsave(filename = paste0("figures/m50_bymonth_vpd", ".jpg"),
       plot = m50_m_vpd,
       dpi = 300,
       width = 10,
       height = 4)






# yearly model

# base model function
base_nlme_model <- function(data, var) {
  # data = dataframe
  # var = var to model
  
  model_specification = as.formula(paste0(var, " ~ time"))
  
  gls(model = model_specification,
      data = data,
      method = "ML",
      na.action = na.omit)
}

# function for plotting graphs with best nlme model results ------------------------------------------------

plot_yearly_results <- function(data, var, coeffs, title, y_axis_label) {
  # data = dataset
  # var = variable (not in quotes)
  # coeffs = object containing coefficients and p-values of best model
  # title = title for graph (in quotes)
  # y_axis_label = label for y-axis (in quotes)
  ggplot(data, aes(x = year, y = {{ var }})) +
    geom_line(color = "burlywood") +
    geom_smooth(method = "lm", color = "black", size = 0.6, se = FALSE) +
    geom_smooth(method = "loess", color = "lightgrey", size = 0.4, se = FALSE) +
    labs(title = title,
         x = "Year",
         y = y_axis_label,
         caption = paste("slope = ", round(coeffs[2,1], 3), "\n(p-value = ", round(coeffs[2,2], 4), ")"))
}

m40_bm <- met_y_vpd %>% 
  filter(sta == "40") %>% 
  mutate(time = as.numeric(as.factor(year))) %>% 
  base_nlme_model(., var = "vpd")

summary(m40_bm)
m40_coeffs <- summary(m40_bm)$tTable[,c(1, 4)]

(m40_plot <- plot_yearly_results(met_y_vpd %>% filter(sta == "40"), vpd, m40_coeffs, "Met 40 - Annual Mean VPD", "VPD (kPa)"))


m42_bm <- met_y_vpd %>% 
  filter(sta == "42") %>% 
  mutate(time = as.numeric(as.factor(year))) %>% 
  base_nlme_model(., var = "vpd")

summary(m42_bm)
m42_coeffs <- summary(m42_bm)$tTable[,c(1, 4)]

(m42_plot <- plot_yearly_results(met_y_vpd %>% filter(sta == "42"), vpd, m42_coeffs, "Met 42 - Annual Mean VPD", "VPD (kPa)"))


m49_bm <- met_y_vpd %>% 
  filter(sta == "49") %>% 
  mutate(time = as.numeric(as.factor(year))) %>% 
  base_nlme_model(., var = "vpd")

summary(m49_bm)
m49_coeffs <- summary(m49_bm)$tTable[,c(1, 4)]

(m49_plot <- plot_yearly_results(met_y_vpd %>% filter(sta == "49"), vpd, m49_coeffs, "Met 49 - Annual Mean VPD", "VPD (kPa)"))


m50_bm <- met_y_vpd %>% 
  filter(sta == "50") %>% 
  mutate(time = as.numeric(as.factor(year))) %>% 
  base_nlme_model(., var = "vpd")

summary(m50_bm)
m50_coeffs <- summary(m50_bm)$tTable[,c(1, 4)]

(m50_plot <- plot_yearly_results(met_y_vpd %>% filter(sta == "50"), vpd, m50_coeffs, "Met 50 - Annual Mean VPD", "VPD (kPa)"))

  
(m_annual_vpd <- grid.arrange(m40_plot, m42_plot, m49_plot, m50_plot, ncol=2))



# saves previous graph
ggsave(filename = paste0("figures/Annual_Mean_VPD_all_stations", ".jpg"),
       plot = m_annual_vpd,
       dpi = 300,
       width = 10,
       height = 4)
  
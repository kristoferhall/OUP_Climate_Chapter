# KM Hall
#
# Detecting heat wave events 
#



# Using the heatwaveR package - https://robwschlegel.github.io/heatwaveR/index.html
library(heatwaveR)
library(tidyverse)
library(lubridate)
library(nlme)
library(ggthemes)

path_to_file <- "data/processed_data/met_daily_gap_filled.csv"

met <- read_csv(path_to_file) 

summary(met)
str(met)

# subset data for each met station ---------  

table(met$sta)

m40 <- met %>% filter(sta == 40)
m42 <- met %>% filter(sta == 42)
m49 <- met %>% filter(sta == 49)
m50 <- met %>% filter(sta == 50)



# create some functions ----------

# function to select station and variable and add 'time' var
prepare_data_for_nlme <- function(data) {
  # data = dataframe to use
  data %>% 
    mutate(time = as.numeric(as.factor(year)))
}

# base nlme model function
base_nlme_model <- function(data, var) {
  # data = dataframe
  # var = var to model
  
  model_specification = as.formula(paste0(var, " ~ time"))
  
  gls(model = model_specification,
      data = data,
      method = "ML",
      na.action = na.omit)
}

# # linear model function
# linear_model <- function(data, var) {
#   # data = dataframe
#   # var = var to model
#   
#   var_scaled <- scale({{ var }})
#   
#   # model_specification = as.formula(paste0(var, " ~ year"))
#   
#   # model_specification = as.formula(scale(var) ~ scale(year))
#   
#   
#   lm(formula = var_scaled ~ scale(year),
#       data = data)
# }



# conduct heat wave analysis ----------------

# Using the CTX90pct method with a 15-day window with a threshold
# of a hot day being above the 90th percentile for a given calendar
# day calculated across the entire times series. (see Perkins and Alexander)
#
# A heat wave event occurs when Tmax is greater than the 90th percentile 
# historical climatology for 3 or more consecutive days.


# NOTE: USER - change the station to conduct the heat wave analysis on another station.
#      After station is reassigned, the rest of the code runs without needed modification.
#      Except for maybe some commented out code at the end, but check that out. 
# station <- m40
station <- m42
# station <- m49
# station <- m50


station_id <- as.numeric(station %>% select(sta) %>% unique())

station_color <- ifelse(station_id == 40, "grey60",
                        ifelse(station_id == 42, "darkslategrey",
                               ifelse(station_id == 49, "darkolivegreen", "deepskyblue4")))


start_end <- station %>% 
  summarize(start_clim_period = min(date),
            end_clim_period = max(date))


# calculate station historical climatology baseline
station_hx <- ts2clm(
  station,
  x = date,
  y = maxair,
  climatologyPeriod = c(start_end$start_clim_period, start_end$end_clim_period),
  windowHalfWidth = 7,        # 15-day window
  pctile = 90,          # threshold percentile for detection of events
)

# detect heatwaves    (can also be used for cold-spells)
station_hw <- detect_event(station_hx,
                           x = date, 
                           y = maxair,
                           minDuration = 3    # an event is >= 3 days in duration
)



station_climatology <- station_hw$climatology

station_event <- station_hw$event

station_yearly_metrics <- block_average(station_hw,
                                        x = date,
                                        y = maxair,
                                        report = "full")

# monsoon subset - doy 181-273
# station_climatology <- station_climatology %>% 
#   filter(doy >= 181 & doy <= 273)
# station_event <- station_event %>% 
#   filter(yday(date_start) >= 181 & yday(date_start) <= 273)


# number of events per year
station_yearly_metrics %>% 
  ggplot(aes(x = year, y = count)) +
  geom_line(size = 0.7, color = station_color) +
  geom_point(size = 0.7, color = "black") +
  labs(x = "Year",
       y = "Maximum daily temperature (C)")


station_yearly_metrics <- prepare_data_for_nlme(station_yearly_metrics)  






# run model on number of annual heat wave events over time

# using nlme
# m.number_events <- base_nlme_model(station_yearly_metrics, "count")
# m.number_events_coeffs <- summary(m.number_events)$tTable[, c(1, 4)]

# linear model
lm.number_events <- lm(count ~ year, data = station_yearly_metrics)
lm.sum_number <- summary(lm.number_events)
lm.number_events_coeffs <- list(round(lm.number_events$coefficients[2], 2), round(lm.sum_number$coefficients[2, 4], 3))



# graph of number of annual heat waves
station_yearly_metrics %>% 
  ggplot(aes(x = year, y = count)) + 
  geom_point(size = 2, color = station_color) + 
  geom_smooth(method = "lm", se = FALSE, size = 0.5, color = station_color) +
  labs(x = "Year",
       y = "Number of heat waves") +
  annotate("text", 
           x = 2016, 
           y = 9, 
           label = paste("slope = ", lm.number_events_coeffs[[1]], "\nP = ", lm.number_events_coeffs[[2]]),
           size = 4,
           color = station_color) +
  # theme(axis.text = element_text(size = 12),
  #       axis.title = element_text(size = 14)) +
  theme_tufte(base_size = 14, base_family = "sans") +
  ylim(0, 10)






# saves previous graph
# ggsave(filename = paste0("figures/hw_event_count_", station_id, ".jpg"),
#        dpi = 300,
#        width = 10,
#        height = 4)







# run model on duration of heat wave events over time

# nlme model
# m.duration_events <- base_nlme_model(station_yearly_metrics, "duration")
# m.duration_events_coeffs <- summary(m.duration_events)$tTable[, c(1, 4)]

# linear model
lm.duration_events <- lm(duration ~ year, data = station_yearly_metrics)
lm.sum_duration <- summary(lm.duration_events)
lm.duration_events_coeffs <- list(round(lm.duration_events$coefficients[2], 2), round(lm.sum_duration$coefficients[2, 4], 3))


# duration of events
station_yearly_metrics %>% 
  ggplot(aes(x = year, y = duration)) + 
  geom_point(size = 2, color = station_color) + 
  geom_smooth(method = "lm", se = FALSE, size = 0.5, color = station_color) +
  labs(x = "Year",
       y = "Duration of heat wave events (days)") +
  theme_minimal() + 
  annotate("text", 
           x = 2016, 
           y = 9, 
           label = paste("slope = ", lm.duration_events_coeffs[[1]], "\nP = ", lm.duration_events_coeffs[[2]]),
           size = 4,
           color = station_color) +
  # theme(axis.text = element_text(size = 12),
  #       axis.title = element_text(size = 14))
  theme_tufte(base_size = 14, base_family = "sans") +
  ylim(0, 10)




# saves previous graph
# ggsave(filename = paste0("figures/hw_duration_", station_id, ".jpg"),
#        dpi = 300,
#        width = 10,
#        height = 4)






# run model on intensity_mean of heat wave events over time

# nlme model
# m.intensity_mean_events <- base_nlme_model(station_yearly_metrics, "intensity_mean")
# m.intensity_mean_events_coeffs <- summary(m.intensity_mean_events)$tTable[, c(1, 4)]

# linear model
lm.intensity_mean_events <- lm(intensity_mean ~ year, data = station_yearly_metrics)
lm.sum_intensity_mean <- summary(lm.intensity_mean_events)
lm.intensity_mean_events_coeffs <- list(round(lm.intensity_mean_events$coefficients[2], 2), round(lm.sum_intensity_mean$coefficients[2, 4], 3))


# intensity_mean of events
station_yearly_metrics %>% 
  ggplot(aes(x = year, y = intensity_mean)) + 
  geom_point(size = 2, color = station_color) + 
  geom_smooth(method = "lm", se = FALSE, size = 0.5, color = station_color) +
  labs(x = "Year",
       y = "Average intensity of heat wave events") +
  theme_minimal() + 
  annotate("text", 
           x = 2016, 
           y = 9, 
           label = paste("slope = ", lm.intensity_mean_events_coeffs[[1]], "\nP = ", lm.intensity_mean_events_coeffs[[2]]),
           size = 4,
           color = station_color) +
  # theme(axis.text = element_text(size = 12),
  #       axis.title = element_text(size = 14))
  theme_tufte(base_size = 14, base_family = "sans") +
  ylim(0, 10)




# saves previous graph
# ggsave(filename = paste0("figures/hw_intensity_mean_", station_id, ".jpg"),
#        dpi = 300,
#        width = 10,
#        height = 4)







# run model on intensity_max of heat wave events over time

# nlme model
# m.intensity_max_events <- base_nlme_model(station_yearly_metrics, "intensity_max")
# m.intensity_max_events_coeffs <- summary(m.intensity_max_events)$tTable[, c(1, 4)]

# linear model
lm.intensity_max_events <- lm(intensity_max ~ year, data = station_yearly_metrics)
lm.sum_intensity_max <- summary(lm.intensity_max_events)
lm.intensity_max_events_coeffs <- list(round(lm.intensity_max_events$coefficients[2], 2), round(lm.sum_intensity_max$coefficients[2, 4], 3))


# intensity_max of events
station_yearly_metrics %>% 
  ggplot(aes(x = year, y = intensity_max)) + 
  geom_point(size = 2, color = station_color) + 
  geom_smooth(method = "lm", se = FALSE, size = 0.5, color = station_color) +
  labs(x = "Year",
       y = "Maximum intensity of heat wave events") +
  theme_minimal() + 
  annotate("text", 
           x = 2016, 
           y = 9, 
           label = paste("slope = ", lm.intensity_max_events_coeffs[[1]], "\nP = ", lm.intensity_max_events_coeffs[[2]]),
           size = 4,
           color = station_color) +
  # theme(axis.text = element_text(size = 12),
  #       axis.title = element_text(size = 14))
  theme_tufte(base_size = 14, base_family = "sans") +
  ylim(0, 10)




# saves previous graph
# ggsave(filename = paste0("figures/hw_intensity_max_", station_id, ".jpg"),
#        dpi = 300,
#        width = 10,
#        height = 4)








# run model on intensity_cumulative of heat wave events over time

# nlme model
# m.intensity_cumulative <- base_nlme_model(station_yearly_metrics, "intensity_cumulative")
# m.intensity_cumulative_coeffs <- summary(m.intensity_cumulative)$tTable[, c(1, 4)]

# linear model
lm.intensity_cumulative <- lm(intensity_cumulative ~ year, data = station_yearly_metrics)
lm.sum_intensity_cumulative <- summary(lm.intensity_cumulative)
lm.intensity_cumulative_coeffs <- list(round(lm.intensity_cumulative$coefficients[2], 2), round(lm.sum_intensity_cumulative$coefficients[2, 4], 3))


# intensity_cumulative of events
station_yearly_metrics %>% 
  ggplot(aes(x = year, y = intensity_cumulative)) + 
  geom_point(size = 2, color = station_color) + 
  geom_smooth(method = "lm", se = FALSE, size = 0.5, color = station_color) +
  labs(x = "Year",
       y = "Maximum intensity of heat wave events") +
  theme_minimal() + 
  annotate("text", 
           x = 2016, 
           y = 18, 
           label = paste("slope = ", lm.intensity_cumulative_coeffs[[1]], "\nP = ", lm.intensity_cumulative_coeffs[[2]]),
           size = 4,
           color = station_color) +
  # theme(axis.text = element_text(size = 12),
  #       axis.title = element_text(size = 14))
  theme_tufte(base_size = 14, base_family = "sans") 




# saves previous graph
# ggsave(filename = paste0("figures/hw_intensity_cumulative_", station_id, ".jpg"),
#        dpi = 300,
#        width = 10,
#        height = 4)



station_event %>% 
  ggplot(aes(x = date_peak, y = duration)) +
  geom_line() +
  geom_smooth(method=lm)

station_event %>% 
  ggplot(aes(x = date_peak, y = intensity_cumulative)) +
  geom_line() +
  geom_smooth(method=lm)

station_event %>% 
  ggplot(aes(x = date_peak, y = intensity_cumulative_relThresh)) +
  geom_line() +
  geom_smooth(method=lm)

station_event %>% 
  ggplot(aes(x = date_peak, y = intensity_mean_relThresh)) +
  geom_line() +
  geom_smooth(method=lm)


station_event %>% 
  mutate(year = year(date_start)) %>% 
  group_by(year) %>% 
  summarize(mean_event_duration = mean(duration)) %>% 
  ggplot(aes(x = year, y = mean_event_duration)) +
  geom_line() +
  geom_smooth(method = lm, se = FALSE )

station_event %>% 
  mutate(year = year(date_start)) %>% 
  group_by(year) %>% 
  summarize(total_days_above_threshold = sum(duration)) %>% 
  ggplot(aes(x = year, y = total_days_above_threshold)) +
  geom_line() +
  geom_smooth(method = lm, se = FALSE )


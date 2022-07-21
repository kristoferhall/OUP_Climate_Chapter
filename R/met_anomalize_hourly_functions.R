# KM Hall
#
# Functions associated with the anomalize_hourly_met_data.R script



sta_hrly_all_vars_graph <- function(data_long, station) {
  # function graphs hourly data for all variables for a given station.
  #
  # data needs to be in long format
  
  data_long %>% 
    filter(sta == {{ station }}) %>% 
    ggplot(., aes(x = dt, y = value, color = variable)) +
    geom_point(size = 0.4, alpha = 0.2) +
    facet_wrap(~ variable, scales = "free_y") +
    ggtitle(paste0("Variables for Met ", {{ station }}))
}





hrly_var_graph <- function(data, var_name) {
  # function graphs hourly data for all stations for a given variable.
  #
  # data needs to be in wide format
  data %>% 
    ggplot(., aes(x = dt, y = .data[[var_name]], color = sta)) +
    geom_point(size = 0.4, alpha = 0.2) +
    facet_wrap(~ sta) +
    ggtitle(paste0(var_name, " by Met Station"))
}




subset_stations <- function(data, station) {
  data %>% 
    filter(sta == {{ station }})
}



sta_all_dts_w_flag <- function(data) {
  # creates records for all hourly records that are possible from the time
  # a station went online until the last record in the data.
  #
  # data should already be subset by station
  
  # gets min and max dt in data
  min_dt <- min(data$dt, na.rm = TRUE)
  max_dt <- max(data$dt, na.rm = TRUE)
  
  # gets sta value
  station <- unique(data$sta)
  
  # creates a comprehensive list of hourly records, joins to data, and creates
  # a no_record_flag set to TRUE for missing records
  dt <- seq(min_dt, max_dt, by = "hour")
  all_dt <- tibble(sta = station, 
                   dt = dt)
  
  all_dt_with_data <- all_dt %>% 
    left_join(data) %>% 
    mutate(no_record_flag = ifelse(is.na(date), TRUE, FALSE))
  
  return(all_dt_with_data)
  
}






no_rec_flag_count <- function(data) {
  # counts no_record_flags and provides percent of total rows that have flag
  
  total_rows <- nrow(data)
  
  data %>% 
    group_by(no_record_flag) %>% 
    summarize(flag_count = n(),
              percent = n() / total_rows * 100) %>% 
    ungroup()
}






no_rec_graph <- function(data) {
  # graphs no_record_flag 
  
  data %>% 
    ggplot(., aes(x = dt, y = no_record_flag, color = no_record_flag)) +
    geom_point(size = 0.2)
}






var_hrly_means <- function(data) {
  # Function calculates means by day of year and hour of day for all vars. Then
  # it uses the var mean if the var value is missing. Gap fills missing data
  # with the mean value of that variable for the day of year and hour of day.
  
  
  data_temp <- data %>% 
    mutate(day_of_year = ifelse(is.na(day_of_year), yday(dt), day_of_year),
           hour = ifelse(is.na(hour), hour(dt), hour)) 
  
  data_means <- data_temp %>% 
    group_by(day_of_year, hour) %>% 
    summarize(airt_mean = mean(airt, na.rm = TRUE),
              maxair_mean = mean(maxair, na.rm = TRUE),
              minair_mean = mean(minair, na.rm = TRUE),
              ppt_mean = 0,     # just assume a missed hour has 0 ppt
              rh_mean = mean(rh, na.rm = TRUE),
              sol_mean = mean(sol, na.rm = TRUE),
              minsol_mean = mean(minsol, na.rm = TRUE),
              maxsol_mean = mean(maxsol, na.rm = TRUE)) %>% 
    ungroup()
  
  data_final <- data_temp %>% 
    left_join(data_means) %>% 
    mutate(airt = ifelse(is.na(airt), airt_mean, airt),
           maxair = ifelse(is.na(maxair), maxair_mean, maxair),
           minair = ifelse(is.na(minair), minair_mean, minair),
           ppt = ifelse(is.na(ppt), ppt_mean, ppt),
           rh = ifelse(is.na(rh), rh_mean, rh),
           sol = ifelse(is.na(sol), sol_mean, sol),
           minsol = ifelse(is.na(minsol), minsol_mean, minsol),
           maxsol = ifelse(is.na(maxsol), maxsol_mean, maxsol))
  
  return(data_final)
  
}







gf_graph <- function(data_long, station) {
  # takes a long, gap-filled data set and returns info for each var for a station
  # color coded by no_record_flag
  
  data_long %>% 
    filter(sta == {{ station }}) %>% 
    ggplot(., aes(x = dt, y = value, color = no_record_flag)) +
    geom_point(size = 0.6, alpha = 0.6) +
    facet_wrap(~ variable, scales = "free_y")
}





var_anomolize <- function(data, var_to_anomalize) {
  # runs anomalize on a variable for all stations
  
  data %>% 
    select(sta, dt, {{ var_to_anomalize }}) %>% 
    arrange(sta, dt) %>% 
    group_by(sta) %>% 
    time_decompose({{ var_to_anomalize }}, merge = TRUE) %>% 
    anomalize(remainder) %>% 
    time_recompose()
}




show_anomalies_graph <- function(data) {
  # graphs anomalies using anomalize package
  
  data %>% 
    plot_anomalies(alpha_dots = 0.1)
}




show_anomalies_stats <- function(data) {
  # shows flag count on anomalies 
  
  data %>% 
    group_by(sta, anomaly) %>% 
    summarize(anom_flag_count = n())
}











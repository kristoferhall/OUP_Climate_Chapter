# KM Hall
# 
# SEV SPEI calculations


library(tidyverse)
library(lubridate)
library(SPEI)



# load monthly met data ----------------------------
met <- read_csv("./data/processed_data/met_monthly_gap_filled.csv") %>% 
  mutate(sta = as.factor(sta)) %>% 
  filter(date >= "1990-01-01") %>% 
  arrange(sta, year, month)

summary(met)
glimpse(met)




# determine the reference period to use for SPEI -----------------------

# Note: going to use a defined reference period that is from the time
# the youngest met station went online through 2020.

# met %>% 
#   group_by(sta) %>% 
#   summarize(first_date = min(date),
#             last_date = max(date))

# 2002-01-01 is when met 50 went online -
#
# reference period: 2002-01-01 through 2020-12-31

# TODO: should do this at some point... spei function needs to be ts object
#    to use reference periods

# ref_start <- "c(2002, 1)"
# ref_end <- "c(2020, 12)"



# calculate SPEI ------------------------------------------


# calculate PET with Thornthwaite Method to derive water balance

thornthwaite_by_sta <- function(data, station) {
  calc_for_sta <- data %>% 
    filter(sta == station) %>% 
    mutate(pet = thornthwaite(Tave  = airt,
                              lat   = 34.3),
           balance = ppt - pet)
  
  return(calc_for_sta)
}




met_40 <- thornthwaite_by_sta(met, "40")
met_42 <- thornthwaite_by_sta(met, "42")
met_49 <- thornthwaite_by_sta(met, "49")
met_50 <- thornthwaite_by_sta(met, "50")



# wrapping spei calculations into custom function for ease
spei_by_sta <- function(data) {
  spei_calc <- data %>% 
    mutate(spei_12 = as.vector(spei(scale = 12, na.rm = TRUE, data = .$balance)$fitted),         # 12 month rolling - see documentation
           spei_6  = as.vector(spei(scale =  6, na.rm = TRUE, data = .$balance)$fitted),         # 6 month rolling
           spei_1  = as.vector(spei(scale =  1, na.rm = TRUE, data = .$balance)$fitted)) 
  
  return(spei_calc)
}



met_40_spei <- spei_by_sta(met_40)
met_42_spei <- spei_by_sta(met_42)
met_49_spei <- spei_by_sta(met_49)
met_50_spei <- spei_by_sta(met_50)



met_spei <- rbind(met_40_spei, met_42_spei, met_49_spei, met_50_spei)

names(met_spei)





# take a look at pet, balance, and spei ------------------------------

basic_plot_by_sta <- function(data, variable_to_plot, variable_name_quotes) {
  data %>% 
    ggplot(aes(x = date, y = {{ variable_to_plot }}, color = sta)) +
    geom_line() +
    geom_smooth(method = "lm", size = 0.8) +
    facet_wrap(~ sta) +
    scale_color_viridis_d() +
    theme_minimal() +
    labs(title = variable_name_quotes)
}




basic_plot_by_sta(met_spei, pet, "pet")
basic_plot_by_sta(met_spei, balance, "balance")
basic_plot_by_sta(met_spei, spei_12, "spei_12")
basic_plot_by_sta(met_spei, spei_6, "spei_6")
basic_plot_by_sta(met_spei, spei_1, "spei_1")


met_spei %>% 
  filter(date >= "2014-01-01") %>% 
  ggplot(aes(x = date, y = spei_12, color = sta)) +
  geom_line() +
  facet_wrap(~ sta) +
  scale_color_viridis_d() +
  theme_minimal()




# testing
thornth_40 <- thornthwaite(met_40$airt, 34.3)
thornth_50 <- thornthwaite(met_50$airt, 34.3)

# 12 month integration
spei_40_12 <- spei(met_40$ppt - thornth_40, scale = 12)
plot(spei_40_12)

basic_plot_by_sta(met_spei, spei_12, "spei_12")

# 6 month int
spei_40_6 <- spei(met_40$ppt - thornth_40, scale = 6)
spei_50_6 <- spei(met_50$ppt - thornth_50, scale = 6)

plot(spei_40_6)
plot(spei_50_6)

basic_plot_by_sta(met_spei, spei_6, "spei_6")





# by month of year
met_spei %>% 
  ggplot(aes(x = year, y = spei_6, color = sta)) +
  geom_line(alpha = 0.6) +
  geom_smooth(method = "lm", size = 0.3, se = FALSE) +
  facet_wrap(~ month) +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "SPEI 6-month integration by month of year")



met_spei %>% 
  filter(sta == "40") %>% 
  ggplot(aes(x = year, y = spei_6, color = sta)) +
  geom_line() +
  geom_smooth(method = "lm", size = 0.3, se = FALSE) +
  facet_wrap(~ month) +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "Station 40 SPEI 6-month integration by month of year")

met_spei %>% 
  filter(sta == "42") %>% 
  ggplot(aes(x = year, y = spei_6, color = sta)) +
  geom_line() +
  geom_smooth(method = "lm", size = 0.3, se = FALSE) +
  facet_wrap(~ month) +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "Station 42 SPEI 6-month integration by month of year")

met_spei %>% 
  filter(sta == "49") %>% 
  ggplot(aes(x = year, y = spei_6, color = sta)) +
  geom_line() +
  geom_smooth(method = "lm", size = 0.3, se = FALSE) +
  facet_wrap(~ month) +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "Station 49 SPEI 6-month integration by month of year")

met_spei %>% 
  filter(sta == "50") %>% 
  ggplot(aes(x = year, y = spei_6, color = sta)) +
  geom_line() +
  geom_smooth(method = "lm", size = 0.3, se = FALSE) +
  facet_wrap(~ month) +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "Station 50 SPEI 6-month integration by month of year")




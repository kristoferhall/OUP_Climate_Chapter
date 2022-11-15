# KM Hall
# 
# SEV SPEI calculations


library(tidyverse)
library(lubridate)
library(SPEI)
library(nlme)
library(gridExtra)


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
  ggplot(aes(x = date, y = spei_6, color = sta)) +
  geom_line(alpha = 0.6) +
  geom_smooth(method = "lm", size = 0.3, se = FALSE) +
  facet_wrap(~ month) +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "SPEI 6-month integration by month of year")



met_spei %>% 
  filter(sta == "40") %>% 
  ggplot(aes(x = date, y = spei_6, color = sta)) +
  geom_line() +
  geom_smooth(method = "lm", size = 0.3, se = FALSE) +
  facet_wrap(~ month) +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "Station 40 SPEI 6-month integration by month of year")

met_spei %>% 
  filter(sta == "42") %>% 
  ggplot(aes(x = date, y = spei_6, color = sta)) +
  geom_line() +
  geom_smooth(method = "lm", size = 0.3, se = FALSE) +
  facet_wrap(~ month) +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "Station 42 SPEI 6-month integration by month of year")

met_spei %>% 
  filter(sta == "49") %>% 
  ggplot(aes(x = date, y = spei_6, color = sta)) +
  geom_line() +
  geom_smooth(method = "lm", size = 0.3, se = FALSE) +
  facet_wrap(~ month) +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "Station 49 SPEI 6-month integration by month of year")

met_spei %>% 
  filter(sta == "50") %>% 
  ggplot(aes(x = date, y = spei_6, color = sta)) +
  geom_line() +
  geom_smooth(method = "lm", size = 0.3, se = FALSE) +
  facet_wrap(~ month) +
  scale_color_viridis_d() +
  theme_minimal() +
  labs(title = "Station 50 SPEI 6-month integration by month of year")






# run basic linear model

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
  ggplot(data, aes(x = date, y = {{ var }})) +
    geom_hline(yintercept = 0, size = 0.2, color = "black") +
    geom_line(color = "burlywood") +
    geom_smooth(method = "lm", color = "brown", size = 0.4, se = FALSE) +
    geom_smooth(method = "loess", color = "grey", size = 0.4, se = FALSE) +
    labs(title = title,
         x = "Year",
         y = y_axis_label,
         caption = paste("slope = ", round(coeffs[2,1], 3), "\n(p-value = ", round(coeffs[2,2], 4), ")")) +
    theme_minimal() 
}

m40_bm <- met_spei %>% 
  filter(sta == "40" & !is.na(spei_6)) %>%
  mutate(time = as.numeric(as.factor(year))) %>% 
  base_nlme_model(., var = "spei_6")

summary(m40_bm)
m40_coeffs <- summary(m40_bm)$tTable[,c(1, 4)]

(m40_plot <- plot_yearly_results(met_spei %>% filter(sta == "40"), spei_6, m40_coeffs, "Met 40 - SPEI - 6-month Integration", "SPEI"))


m42_bm <- met_spei %>% 
  filter(sta == "42") %>% 
  mutate(time = as.numeric(as.factor(year))) %>% 
  base_nlme_model(., var = "spei_6")

summary(m42_bm)
m42_coeffs <- summary(m42_bm)$tTable[,c(1, 4)]

(m42_plot <- plot_yearly_results(met_spei %>% filter(sta == "42"), spei_6, m42_coeffs, "Met 42 - SPEI - 6-month Integration", "SPEI"))


m49_bm <- met_spei %>% 
  filter(sta == "49") %>% 
  mutate(time = as.numeric(as.factor(year))) %>% 
  base_nlme_model(., var = "spei_6")

summary(m49_bm)
m49_coeffs <- summary(m49_bm)$tTable[,c(1, 4)]

(m49_plot <- plot_yearly_results(met_spei %>% filter(sta == "49"), spei_6, m49_coeffs, "Met 49 - SPEI - 6-month Integration", "SPEI"))


m50_bm <- met_spei %>% 
  filter(sta == "50") %>% 
  mutate(time = as.numeric(as.factor(year))) %>% 
  base_nlme_model(., var = "spei_6")

summary(m50_bm)
m50_coeffs <- summary(m50_bm)$tTable[,c(1, 4)]

(m50_plot <- plot_yearly_results(met_spei %>% filter(sta == "50"), spei_6, m50_coeffs, "Met 50 - SPEI - 6-month Integration", "SPEI"))


grid.arrange(m40_plot, m42_plot, m49_plot, m50_plot, ncol=2)









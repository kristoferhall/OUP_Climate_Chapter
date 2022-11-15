# KM Hall
#
#
# Annual nlme models for met data based on results of met_basic_analyses.Rmd
#
# Monthly nlme models are in met_monthly_nlme_models.R


library(tidyverse)
library(lubridate)
library(nlme)
library(MuMIn)
library(gridExtra)


# Load yearly data ---------------------------------------------

path_to_files <- "./data/processed_data/"

# load and only keep vars of interest
y <- read_csv(paste0(path_to_files, "met_yearly_gap_filled.csv")) %>% 
  mutate(sta = as.factor(sta)) %>% 
  select(sta:ppt)

str(y)
summary(y)



# prepare data to run models - add 'time' var ---------------------

# function to select station and variable and add 'time' var
prepare_data_for_nlme <- function(data, sta) {
  # data = dataframe to use
  # sta = met station (in quotes)
  data %>% 
    filter(sta == {{ sta }}) %>% 
    mutate(time = as.numeric(as.factor(year)))
}


m40 <- prepare_data_for_nlme(y, "40")
m42 <- prepare_data_for_nlme(y, "42")
m49 <- prepare_data_for_nlme(y, "49")
m50 <- prepare_data_for_nlme(y, "50")

mall <- rbind(m40, m42, m49, m50)


# function for plotting preliminary yearly graph -----------------------------------------------------------

plot_yearly_prelim <- function(data, var, title, y_axis_label) {
  # data = dataset
  # var = variable (not in quotes)
  # title = title for graph (in quotes)
  # y_axis_label = label for y-axis (in quotes)
  ggplot(data, aes(x = year, y = {{ var }}, col = sta)) +
    geom_line(color = "burlywood") +
    geom_smooth(method = "lm", color = "black", size = 0.6, se = FALSE) +
    facet_wrap(~ sta) +
    labs(title = title,
         x = "Year",
         y = y_axis_label)
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
    labs(title = title,
         x = "Year",
         y = y_axis_label,
         caption = paste("slope = ", round(coeffs[2,1], 3), "\n(p-value = ", round(coeffs[2,2], 4), ")"))
}


# functions to run nlme models ----------------------------------------------------------

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



# AR1 model function
AR1_nlme_model <- function(data, var) {
  # data = dataframe
  # var = var to model
  
  model_specification = as.formula(paste0(var, " ~ time"))
  
  gls(model = model_specification,
      data = data,
      correlation = corARMA(form = ~ time, p = 1),
      method = "ML",
      na.action = na.omit)
}


# AR2 model function
AR2_nlme_model <- function(data, var) {
  # data = dataframe
  # var = var to model
  
  model_specification = as.formula(paste0(var, " ~ time"))
  
  gls(model = model_specification,
      data = data,
      correlation = corARMA(form = ~ time, p = 2),
      method = "ML",
      na.action = na.omit)
}


# all stations with interaction - sta is already a factor
# base model function
base_sta_int_nlme_model <- function(data, var) {
  # data = dataframe
  # var = var to model
  
  model_specification = as.formula(paste0(var, " ~ time*sta"))
  
  gls(model = model_specification,
      data = data,
      method = "ML",
      na.action = na.omit)
}


# airt -------------------------------------------------------


plot_yearly_prelim(y, airt, "Annual Mean Air Temperature", "Temperature (C)")





# Met 40:

m40_mbase <- base_nlme_model(m40, "airt")
m40_mAR1 <- AR1_nlme_model(m40, "airt")
m40_mAR2 <- AR2_nlme_model(m40, "airt")


AICc(m40_mbase, m40_mAR1, m40_mAR2)
# m40_mbase is best model

summary(m40_mbase)
m40_coeffs <- summary(m40_mbase)$tTable[,c(1, 4)]


(m40_plot <- plot_yearly_results(m40, airt, m40_coeffs, "Met 40 - Deep Well \nAnnual Mean Air Temperature", "Temperature (C)"))



# Met 42:

m42_mbase <- base_nlme_model(m42, "airt")
m42_mAR1 <- AR1_nlme_model(m42, "airt")
m42_mAR2 <- AR2_nlme_model(m42, "airt")

AICc(m42_mbase, m42_mAR1, m42_mAR2)
# m42_mbase is best model 

summary(m42_mbase)
m42_coeffs <- summary(m42_mbase)$tTable[,c(1, 4)]

(m42_plot <- plot_yearly_results(m42, airt, m42_coeffs, "Met 42 - Cerro Montoso \nAnnual Mean Air Temperature", "Temperature (C)"))



# Met 49:

m49_mbase <- base_nlme_model(m49, "airt")
m49_mAR1 <- AR1_nlme_model(m49, "airt")
m49_mAR2 <- AR2_nlme_model(m49, "airt")

AICc(m49_mbase, m49_mAR1, m49_mAR2)
# m49_mbase is best model

summary(m49_mbase)
m49_coeffs <- summary(m49_mbase)$tTable[,c(1, 4)]

(m49_plot <- plot_yearly_results(m49, airt, m49_coeffs, "Met 49 - Five Points \nAnnual Mean Air Temperature", "Temperature (C)"))



# Met 50:

m50_mbase <- base_nlme_model(m50, "airt")
m50_mAR1 <- AR1_nlme_model(m50, "airt")
m50_mAR2 <- AR2_nlme_model(m50, "airt")

AICc(m50_mbase, m50_mAR1, m50_mAR2)
# m50_mbase is best model

summary(m50_mbase)
m50_coeffs <- summary(m50_mbase)$tTable[,c(1, 4)]


(m50_plot <- plot_yearly_results(m50, airt, m50_coeffs, "Met 50 - Blue Grama \nAnnual Mean Air Temperature", "Temperature (C)"))



# airt results 
(m_airt <- grid.arrange(m40_plot, m42_plot, m49_plot, m50_plot, ncol=2, nrow=2))


# # saves previous graph
# ggsave(filename = paste0("figures/Annual_Mean_Temp_all_stations", ".jpg"),
#        plot = m_airt,
#        dpi = 300,
#        width = 10,
#        height = 4)


# base model with sta interaction
mall_base_sta_int <- base_sta_int_nlme_model(mall, "airt")

summary(mall_base_sta_int)
summary(mall_base_sta_int)$tTable

plot(mall_base_sta_int$fitted, mall_base_sta_int$residuals)

mall_base_fit_resid <- tibble(fitted = mall_base_sta_int$fitted, 
                              residual = mall_base_sta_int$residuals)

mall_base_fit_resid %>% 
  ggplot(aes(x = fitted, y = residual)) +
  geom_point() 



# minair -------------------------------------------------------


plot_yearly_prelim(y, minair, "Annual Minimum Air Temperature", "Temperature (C)")


# Met 40:

m40_mbase <- base_nlme_model(m40, "minair")
m40_mAR1 <- AR1_nlme_model(m40, "minair")
m40_mAR2 <- AR2_nlme_model(m40, "minair")

AICc(m40_mbase, m40_mAR1, m40_mAR2)
# m40_mbase is best model

summary(m40_mbase)
m40_coeffs <- summary(m40_mbase)$tTable[,c(1, 4)]


(m40_plot <- plot_yearly_results(m40, minair, m40_coeffs, "Met 40 - Deep Well \nAnnual Minimum Air Temperature", "Temperature (C)"))




# Met 42:

m42_mbase <- base_nlme_model(m42, "minair")
m42_mAR1 <- AR1_nlme_model(m42, "minair")
m42_mAR2 <- AR2_nlme_model(m42, "minair")

AICc(m42_mbase, m42_mAR1, m42_mAR2)
# m42_mbase is best model 

summary(m42_mbase)
m42_coeffs <- summary(m42_mbase)$tTable[,c(1, 4)]

(m42_plot <- plot_yearly_results(m42, minair, m42_coeffs, "Met 42 - Cerro Montoso \nAnnual Minimum Air Temperature", "Temperature (C)"))



# Met 49:

m49_mbase <- base_nlme_model(m49, "minair")
m49_mAR1 <- AR1_nlme_model(m49, "minair")
m49_mAR2 <- AR2_nlme_model(m49, "minair")

AICc(m49_mbase, m49_mAR1, m49_mAR2)
# m49_mbase is best model

summary(m49_mbase)
m49_coeffs <- summary(m49_mbase)$tTable[,c(1, 4)]

(m49_plot <- plot_yearly_results(m49, minair, m49_coeffs, "Met 49 - Five Points \nAnnual Minimum Air Temperature", "Temperature (C)"))



# Met 50:

m50_mbase <- base_nlme_model(m50, "minair")
m50_mAR1 <- AR1_nlme_model(m50, "minair")
m50_mAR2 <- AR2_nlme_model(m50, "minair")

AICc(m50_mbase, m50_mAR1, m50_mAR2)
# m50_mbase is best model

summary(m50_mbase)
m50_coeffs <- summary(m50_mbase)$tTable[,c(1, 4)]


(m50_plot <- plot_yearly_results(m50, minair, m50_coeffs, "Met 50 - Blue Grama \nAnnual Minimum Air Temperature", "Temperature (C)"))



# minair results 
grid.arrange(m40_plot, m42_plot, m49_plot, m50_plot, ncol=2)



# base model with sta interaction
mall_base_sta_int <- base_sta_int_nlme_model(mall, "minair")

summary(mall_base_sta_int)
summary(mall_base_sta_int)$tTable



# maxair -------------------------------------------------------


plot_yearly_prelim(y, maxair, "Annual Maximum Air Temperature", "Temperature (C)")


# Met 40:

m40_mbase <- base_nlme_model(m40, "maxair")
m40_mAR1 <- AR1_nlme_model(m40, "maxair")
m40_mAR2 <- AR2_nlme_model(m40, "maxair")

AICc(m40_mbase, m40_mAR1, m40_mAR2)
# m40_mAR1 is best model ****

summary(m40_mAR1)
m40_coeffs <- summary(m40_mAR1)$tTable[,c(1, 4)]


(m40_plot <- plot_yearly_results(m40, maxair, m40_coeffs, "Met 40 - Deep Well \nAnnual Maximum Air Temperature", "Temperature (C)"))




# Met 42:

m42_mbase <- base_nlme_model(m42, "maxair")
m42_mAR1 <- AR1_nlme_model(m42, "maxair")
m42_mAR2 <- AR2_nlme_model(m42, "maxair")

AICc(m42_mbase, m42_mAR1, m42_mAR2)
# m42_mAR1 is best model 

summary(m42_mAR1)
m42_coeffs <- summary(m42_mAR1)$tTable[,c(1, 4)]

(m42_plot <- plot_yearly_results(m42, maxair, m42_coeffs, "Met 42 - Cerro Montoso \nAnnual Maximum Air Temperature", "Temperature (C)"))




# Met 49:

m49_mbase <- base_nlme_model(m49, "maxair") 
m49_mAR1 <- AR1_nlme_model(m49, "maxair")
m49_mAR2 <- AR2_nlme_model(m49, "maxair")

AICc(m49_mbase, m49_mAR1, m49_mAR2)
# m49_mbase is best model

summary(m49_mbase)
m49_coeffs <- summary(m49_mbase)$tTable[,c(1, 4)]

(m49_plot <- plot_yearly_results(m49, maxair, m49_coeffs, "Met 49 - Five Points \nAnnual Maximum Air Temperature", "Temperature (C)"))



# Met 50:

m50_mbase <- base_nlme_model(m50, "maxair")
m50_mAR1 <- AR1_nlme_model(m50, "maxair")
m50_mAR2 <- AR2_nlme_model(m50, "maxair")

AICc(m50_mbase, m50_mAR1, m50_mAR2)
# m50_mAR1 is best model

summary(m50_mAR1)
m50_coeffs <- summary(m50_mAR1)$tTable[,c(1, 4)]


(m50_plot <- plot_yearly_results(m50, maxair, m50_coeffs, "Met 50 - Blue Grama \nAnnual Maximum Air Temperature", "Temperature (C)"))



# maxair results 
grid.arrange(m40_plot, m42_plot, m49_plot, m50_plot, ncol=2)


# base model with sta interaction
mall_base_sta_int <- base_sta_int_nlme_model(mall, "maxair")

summary(mall_base_sta_int)
summary(mall_base_sta_int)$tTable





# ppt -------------------------------------------------------


plot_yearly_prelim(y, ppt, "Annual Total Precipitation", "Precipitation (mm)")


# Met 40:

m40_mbase <- base_nlme_model(m40, "ppt")
m40_mAR1 <- AR1_nlme_model(m40, "ppt")
m40_mAR2 <- AR2_nlme_model(m40, "ppt")

AICc(m40_mbase, m40_mAR1, m40_mAR2)
# m40_mbase is best model

summary(m40_mbase)
m40_coeffs <- summary(m40_mbase)$tTable[,c(1, 4)]


(m40_plot <- plot_yearly_results(m40, ppt, m40_coeffs, "Met 40 - Deep Well \nAnnual Total Precipitation", "Precipitation (mm)"))




# Met 42:

m42_mbase <- base_nlme_model(m42, "ppt")
m42_mAR1 <- AR1_nlme_model(m42, "ppt")
m42_mAR2 <- AR2_nlme_model(m42, "ppt")

AICc(m42_mbase, m42_mAR1, m42_mAR2)
# m42_mbase is best model 

summary(m42_mbase)
m42_coeffs <- summary(m42_mbase)$tTable[,c(1, 4)]

(m42_plot <- plot_yearly_results(m42, ppt, m42_coeffs, "Met 42 - Cerro Montoso \nAnnual Total Precipitation", "Precipitation (mm)"))



# Met 49:

m49_mbase <- base_nlme_model(m49, "ppt")
m49_mAR1 <- AR1_nlme_model(m49, "ppt")
m49_mAR2 <- AR2_nlme_model(m49, "ppt")

AICc(m49_mbase, m49_mAR1, m49_mAR2)
# m49_mbase is best model

summary(m49_mbase)
m49_coeffs <- summary(m49_mbase)$tTable[,c(1, 4)]

(m49_plot <- plot_yearly_results(m49, ppt, m49_coeffs, "Met 49 - Five Points \nAnnual Total Precipitation", "Precipitation (mm)"))



# Met 50:

m50_mbase <- base_nlme_model(m50, "ppt")
m50_mAR1 <- AR1_nlme_model(m50, "ppt")
m50_mAR2 <- AR2_nlme_model(m50, "ppt")

AICc(m50_mbase, m50_mAR1, m50_mAR2)
# m50_mbase is best model

summary(m50_mbase)
m50_coeffs <- summary(m50_mbase)$tTable[,c(1, 4)]


(m50_plot <- plot_yearly_results(m50, ppt, m50_coeffs, "Met 50 - Blue Grama \nAnnual Total Precipitation", "Precipitation (mm)"))


# ppt results 
(m_ppt <- grid.arrange(m40_plot, m42_plot, m49_plot, m50_plot, ncol=2))

# # saves previous graph
# ggsave(filename = paste0("figures/Annual_Mean_Precip_all_stations", ".jpg"),
#        plot = m_ppt,
#        dpi = 300,
#        width = 10,
#        height = 4)



# base model with sta interaction
mall_base_sta_int <- base_sta_int_nlme_model(mall, "ppt")

summary(mall_base_sta_int)
summary(mall_base_sta_int)$tTable





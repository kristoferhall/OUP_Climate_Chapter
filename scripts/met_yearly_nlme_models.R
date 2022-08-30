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




# function for plotting preliminary yearly graph -----------------------------------------------------------

plot_yearly_prelim <- function(data, var, title, y_axis_label) {
  # data = dataset
  # var = variable (not in quotes)
  # title = title for graph (in quotes)
  # y_axis_label = label for y-axis (in quotes)
  ggplot(data, aes(x = year, y = {{ var }})) +
    geom_line(color = "burlywood") +
    geom_smooth(method = "lm", color = "black", size = 0.6) +
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
    geom_smooth(method = "lm", color = "black", size = 0.6) +
    labs(title = title,
         x = "Year",
         y = y_axis_label,
         caption = paste("slope = ", round(coeffs[2,1], 3), "\n(p-value = ", coeffs[2,2], ")"))
}




# airt -------------------------------------------------------


ggplot(y, aes(x = year, y = airt, color = sta)) +
  geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(~ sta) +
  theme(legend.position="none") +
  labs(title = "Yearly Mean Air Temperature (C)",
       y = "Mean Air Temperature (C)",
       x = "Year")

plot_yearly_prelim(m40, airt, "Met 40 - Deep Well - Annual Mean Air Temperature", "Mean Air Temperature (C)")


# Met 40:

m40_mbase <- gls(airt ~ time,
                 data = m40,
                 method = "ML",
                 na.action = na.omit)
m40_mAR1 <- gls(airt ~ time,
            data = m40,
            correlation = corARMA(form = ~ time, p = 1),
            method = "ML",
            na.action = na.omit)
m40_mAR2 <- gls(airt ~ time,
            data = m40,
            correlation = corARMA(form = ~ time, p = 2),
            method = "ML",
            na.action = na.omit)

AICc(m40_mbase, m40_mAR1, m40_mAR2)
# m40_mbase is best model

summary(m40_mbase)
m40_coeffs <- summary(m40_mbase)$tTable[,c(1, 4)]


(m40_plot <- plot_yearly_results(m40, airt, m40_coeffs, "Met 40 - Deep Well - Annual Mean Air Temperature", "Mean Air Temperature (C)"))




# Met 42:

m42_mbase <- gls(airt ~ time,
                 data = m42,
                 method = "ML",
                 na.action = na.omit)
m42_mAR1 <- gls(airt ~ time,
                data = m42,
                correlation = corARMA(form = ~ time, p = 1),
                method = "ML",
                na.action = na.omit)
m42_mAR2 <- gls(airt ~ time,
                data = m42,
                correlation = corARMA(form = ~ time, p = 2),
                method = "ML",
                na.action = na.omit)

AICc(m42_mbase, m42_mAR1, m42_mAR2)
# m42_mbase is best model 

summary(m42_mbase)
m42_coeffs <- summary(m42_mbase)$tTable[,c(1, 4)]

(m42_plot <- plot_yearly_results(m42, airt, m42_coeffs, "Met 42 - Deep Well - Annual Mean Air Temperature", "Mean Air Temperature (C)"))



# Met 49:

m49_mbase <- gls(airt ~ time,
                 data = m49,
                 method = "ML",
                 na.action = na.omit)
m49_mAR1 <- gls(airt ~ time,
                data = m49,
                correlation = corARMA(form = ~ time, p = 1),
                method = "ML",
                na.action = na.omit)
m49_mAR2 <- gls(airt ~ time,
                data = m49,
                correlation = corARMA(form = ~ time, p = 2),
                method = "ML",
                na.action = na.omit)

AICc(m49_mbase, m49_mAR1, m49_mAR2)
# m49_mbase is best model

summary(m49_mbase)
m49_coeffs <- summary(m49_mbase)$tTable[,c(1, 4)]

(m49_plot <- plot_yearly_results(m49, airt, m49_coeffs, "Met 49 - Deep Well - Annual Mean Air Temperature", "Mean Air Temperature (C)"))



# Met 50:

m50_mbase <- gls(airt ~ time,
                 data = m50,
                 method = "ML",
                 na.action = na.omit)
m50_mAR1 <- gls(airt ~ time,
                data = m50,
                correlation = corARMA(form = ~ time, p = 1),
                method = "ML",
                na.action = na.omit)
m50_mAR2 <- gls(airt ~ time,
                data = m50,
                correlation = corARMA(form = ~ time, p = 2),
                method = "ML",
                na.action = na.omit)

AICc(m50_mbase, m50_mAR1, m50_mAR2)
# m50_mbase is best model

summary(m50_mbase)
m50_coeffs <- summary(m50_mbase)$tTable[,c(1, 4)]


(m50_plot <- plot_yearly_results(m50, airt, m50_coeffs, "Met 50 - Deep Well - Annual Mean Air Temperature", "Mean Air Temperature (C)"))



# airt results 
grid.arrange(m40_plot, m42_plot, m49_plot, m50_plot, ncol=2)



























































































































































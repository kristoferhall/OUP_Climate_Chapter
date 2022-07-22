# KM Hall
#
#
# WaveletComp on SEV meteorological data


library(WaveletComp)
library(dplyr)
library(matrixStats)
library(tidyr)
library(ggplot2)
library(lubridate)
library(readr)


# starting with monthly data
path_to_files <- "./data/processed_data/"

m <- read_csv(paste0(path_to_files, "met_monthly_gap_filled.csv")) %>% 
  mutate(sta = as.factor(sta),
         date = ymd(date))



m40 <- m %>% 
  filter(sta == "40") %>% 
  select(date, airt, minair, maxair, rh, ppt) %>% 
  arrange(date)



# airt
my.wt.m40.airt <- analyze.wavelet(m40, "airt", make.pval = TRUE, n.sim = 20)

wt.image(my.wt.m40.airt, 
         main = "Met 40 - Monthly Mean Air Temperature",
         periodlab = "period (monthly)",
         label.time.axis = TRUE,
         show.date = TRUE,
         color.key = "quantile",
         legend.params = list(label.digits = 3, lab = "power", mar = 8),
         plot.contour = TRUE)

maximum.level = 1.001 * max(my.wt.m40.airt$Power.avg)
wt.avg(my.wt.m40.airt, maximum.level = maximum.level)


# maxair
my.wt.m40.maxair <- analyze.wavelet(m40, "maxair", make.pval = TRUE, n.sim = 20)

wt.image(my.wt.m40.maxair, 
         main = "Met 40 - Monthly Maximum Air Temperature",
         periodlab = "period (monthly)",
         label.time.axis = TRUE,
         show.date = TRUE,
         color.key = "quantile",
         legend.params = list(label.digits = 3, lab = "power", mar = 8),
         plot.contour = TRUE)

maximum.level = 1.001 * max(my.wt.m40.maxair$Power.avg)
wt.avg(my.wt.m40.maxair, maximum.level = maximum.level)



# minair
my.wt.m40.minair <- analyze.wavelet(m40, "minair", make.pval = TRUE, n.sim = 20)

wt.image(my.wt.m40.minair, 
         main = "Met 40 - Monthly Minimum Air Temperature",
         periodlab = "period (monthly)",
         label.time.axis = TRUE,
         show.date = TRUE,
         color.key = "quantile",
         legend.params = list(label.digits = 3, lab = "power", mar = 8),
         plot.contour = TRUE)

maximum.level = 1.001 * max(my.wt.m40.minair$Power.avg)
wt.avg(my.wt.m40.minair, maximum.level = maximum.level)


# rh
my.wt.m40.rh <- analyze.wavelet(m40, "rh", make.pval = TRUE, n.sim = 20)

wt.image(my.wt.m40.rh, 
         main = "Met 40 - Monthly Mean Relative Humidity (%)",
         periodlab = "period (monthly)",
         label.time.axis = TRUE,
         show.date = TRUE,
         color.key = "quantile",
         legend.params = list(label.digits = 3, lab = "power", mar = 8),
         plot.contour = TRUE)

maximum.level = 1.001 * max(my.wt.m40.rh$Power.avg)
wt.avg(my.wt.m40.rh, maximum.level = maximum.level)


# ppt
my.wt.m40.ppt <- analyze.wavelet(m40, "ppt", make.pval = TRUE, n.sim = 20)

wt.image(my.wt.m40.ppt, 
         main = "Met 40 - Monthly Total Precipitation (mm)",
         periodlab = "period (monthly)",
         label.time.axis = TRUE,
         show.date = TRUE,
         color.key = "quantile",
         legend.params = list(label.digits = 3, lab = "power", mar = 8),
         plot.contour = TRUE)

maximum.level = 1.001 * max(my.wt.m40.ppt$Power.avg)
wt.avg(my.wt.m40.ppt, maximum.level = maximum.level)






# cross-wavelet for ppt and rh
m_df <- data.frame(m40)

my.wc.ppt_rh <- analyze.coherency(m_df, c("ppt", "rh"), n.sim = 10)


wc.image(my.wc.ppt_rh, main = "cross-wavelet power spectrum, precip and rh",
         legend.params = list(lab = "cross-wavelet power levels"),
         periodlab = "period (monthly)")

wc.image(my.wc.ppt_rh, main = "cross-wavelet power spectrum, precip and rh",
         legend.params = list(lab = "cross-wavelet power levels"),
         periodlab = "period (days)",
         plot.contour = FALSE) # without contour lines


# plot of average coherence:
wc.avg(my.wc.ppt_rh, which.avg = "wc", 
       siglvl = 0.05, sigcol = 'red', 
       legend.coords = "topleft", 
       periodlab = "period (days)")




# cross-wavelet for ppt and airt

my.wc.ppt_airt <- analyze.coherency(m_df, c("ppt", "airt"), n.sim = 10)


wc.image(my.wc.ppt_airt, main = "cross-wavelet power spectrum, precip and airt",
         legend.params = list(lab = "cross-wavelet power levels"),
         periodlab = "period (monthly)")

wc.image(my.wc.ppt_airt, main = "cross-wavelet power spectrum, precip and airt",
         legend.params = list(lab = "cross-wavelet power levels"),
         periodlab = "period (days)",
         plot.contour = FALSE) # without contour lines


# plot of average coherence:
wc.avg(my.wc.ppt_airt, which.avg = "wc", 
       siglvl = 0.05, sigcol = 'red', 
       legend.coords = "topleft", 
       periodlab = "period (days)")























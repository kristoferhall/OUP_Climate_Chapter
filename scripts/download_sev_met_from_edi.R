# KM Hall
# 2022
#
#
# PURPOSE: Download SEV hourly meteorological data from EDI and save raw data.
# 
# This is the first step in the OUP Climate Chapter project
#
# Download SEV-LTER hourly meteorological data from EDI and
# save to file for further analyses for climate book chapter
# for OUP book.
#
# This program serves to officially document what data was obtained
# from EDI. The version of the data is knb-lter-sev.1.15.
# The data package is here: https://portal.edirepository.org/nis/mapbrowse?scope=knb-lter-sev&identifier=1
# DOI:10.6073/pasta/d56307b398e28137dabaa6994f0f5f92
#
# After download, files were modified to be read-only. From ../data/raw_data change permissions
# for user: chmod u-w <file_name.csv>.


library(tidyverse)
library(lubridate)

# folder where the output file will be written to
path_for_data_export <- "~/Documents/SEV/Projects/OUP_Climate_Chapter/data/raw_data/"


# obtain met data from EDI ------------------------------------
m8894 <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-sev.1.15&entityid=87ff32c8e179e69743c6514cbbc8b31c",
                  guess_max = 1000000)

m9599 <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-sev.1.15&entityid=dbe548a38a8dc4c1c829657cee190c4d",
                  guess_max = 1000000)

m0004 <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-sev.1.15&entityid=371109f8068b35cf65edc8ba4237c8bd",
                  guess_max = 1000000)

m0509 <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-sev.1.15&entityid=e326dbe48c0cdc5b91496a469a50e36d",
                  guess_max = 1000000)

m1014 <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-sev.1.15&entityid=011fd6eb9726321cace6c72b50cb8056",
                  guess_max = 1000000)

m1519 <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-sev.1.15&entityid=76922a0b041ac5ab05be6132ff7f90d7",
                  guess_max = 1000000)

m2021 <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=knb-lter-sev.1.15&entityid=bd8e2163c866a22a72136db5b52dd3b9",
                  guess_max = 1000000)




# combine data files and inspect data ----------------------------
m <- rbind(m8894, m9599, m0004, m0509, m1014, m1519, m2021)

m <- m %>% 
  mutate(StationID = as.factor(StationID))

summary(m)

# View(m %>% 
#        group_by(StationID, Year) %>% 
#        summarize(min_dt = min(Date_Time, na.rm = TRUE),
#                  max_dt = max(Date_Time, na.rm = TRUE)))

m %>% 
  group_by(StationID, Date) %>% 
  summarize(n_rec = n()) %>% 
  ggplot(., aes(x=Date, y=n_rec, color=StationID)) +
  geom_jitter(size = 0.2, alpha=.3) +
  facet_wrap(~ StationID)
# NOTE: may want to address dates with fewer than 24 hourly records 
# for individual analyses


# write data to file -------------------------------------
write_csv(m, paste0(path_for_data_export, "sev_hrly_met_all.csv"))


# only for stations of interest for OUP climate chapter
m %>% 
  filter(StationID %in% c("40", "42", "49", "50")) %>% 
  write_csv(., paste0(path_for_data_export, "sev_hrly_met_40_42_49_50.csv"))



# KM Hall
#
# This script creates a daily met dataset for Jenn. It drops
# the solar variables until all the details for those (e.g. - units,
# divide by 100, etc.) are worked out

library(tidyverse)
library(lubridate)


md <- read_csv("./data/processed_data/met_daily_gap_filled.csv") %>% 
  mutate(sta = as.factor(sta))

md4j <- md %>% 
  select(sta:rh, no_record_flag:rh_miss, number_hrly_records)

md4j %>% 
  ggplot(., aes(x = date, y = airt)) +
  geom_point() +
  facet_wrap(~ sta)
md4j %>% 
  ggplot(., aes(x = date, y = minair)) +
  geom_point() +
  facet_wrap(~ sta)
md4j %>% 
  ggplot(., aes(x = date, y = maxair)) +
  geom_point() +
  facet_wrap(~ sta)
md4j %>% 
  ggplot(., aes(x = date, y = ppt)) +
  geom_point() +
  facet_wrap(~ sta)
md4j %>% 
  ggplot(., aes(x = date, y = rh)) +
  geom_point() +
  facet_wrap(~ sta)


summary(md4j)

names(md4j)

write_csv(md4j, paste0("./output/for_jenn/met_daily_gap_filled_to_jenn_", as_date(now()), ".csv"))



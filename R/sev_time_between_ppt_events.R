library(tidyverse)
library(lubridate)
library(ggtext)

station_to_analyze <- "49"
threshold <-  0.3  #0




met <- read_csv("data/processed_data/met_daily_gap_filled.csv") %>% 
  select(sta, date, ppt) %>% 
  mutate(sta = as.factor(sta)) %>% 
  filter(sta == station_to_analyze)


# only obtain records with ppt > threshold (set above)
drought_by_year <- met %>%
  select(date, ppt) %>%
  filter(ppt >= threshold) %>%
  mutate(prev_date = lag(date, n=1)) %>%
  drop_na() %>%
  mutate(drought_length = as.numeric(date - prev_date) - 1,
         year = year(date)) %>%
  select(year, length = drought_length)

# TODO: How to account for the beginning of the year until first rain? Might
#   also be a problem at the end of the year...

drought_by_year %>% 
  ggplot(aes(x = length)) + 
  geom_histogram()

min_year <- as.numeric(drought_by_year %>% select(year) %>% filter(year == min(year)) %>% unique())
max_year <- as.numeric(drought_by_year %>% select(year) %>% filter(year == max(year)) %>% unique())



drought_by_year %>%
  group_by(year) %>%
  summarize(n = n(),
            median = median(length),
            mean = mean(length),
            max = max(length),
            quantile_75th = quantile(length, prob = 0.75)) %>%
  pivot_longer(-year) %>% 
  ggplot(aes(x = year, y = value)) +
  geom_line(color = "tan") +
  geom_smooth(se = FALSE, col = "black", size = 0.5) +
  facet_wrap(~ name, ncol = 1, scale = "free_y") +
  scale_x_continuous(breaks = seq(from = min_year, to = max_year, by = 2)) +
  theme_minimal() 
  







drought_by_year %>%
  group_by(year) %>%
  summarize(n = n(),
            median = median(length),
            mean = mean(length),
            max = max(length),
            quantile_75th = quantile(length, prob = 0.75)) %>%
  ggplot(aes(x = year, y = mean)) +
  geom_line() +
  geom_smooth(se = FALSE, size = 0.7) +
  labs(x = "Year",
       y = "Average number of days\nbetween precipitation events") +
  scale_x_continuous(breaks = seq(min_year, max_year, 2)) +
  theme_classic() +
  theme(plot.title.position = "plot",
        plot.title = element_textbox_simple(size = 18, margin = margin(b=10)))












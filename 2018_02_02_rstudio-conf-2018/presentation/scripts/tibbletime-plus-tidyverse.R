library(tibbletime)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)

stats_raw <- read_csv("data/aggregate/agg_match_stats_0.csv")

stats_time <- stats_raw %>%
  as_tbl_time(date) %>%
  arrange(date)

# Doesn't make a whole lot of sense, lots of spread
stats_time %>%
  collapse_by("hour") %>%
  summarise(num_players = length(unique(player_name))) %>%
  ggplot(aes(x = date, y = num_players)) +
  geom_point()

stats_time %>%
  collapse_by("day") %>%
  summarise(num_players = length(unique(player_name))) %>%
  mutate(is_weekend = lubridate::wday(date, label = TRUE) %in% c("Sat", "Sun")) %>%
  ggplot(aes(x = date, y = num_players, color = is_weekend)) +
  geom_point() 

stats_2017_11 <- stats_time[~"2017-11"]

stats_2017_11 %>%
  filter(team_placement == 1)

stats_2017_11 %>%
  filter(team_placement == 1) %>%
  group_by(match_mode) %>%
  collapse_by("hour") %>%
  distinct(match_id) %>%
  summarise(num_games = n()) %>%
  ggplot(aes(x = date, y = num_games, color = match_mode)) +
  geom_point()

ggplotly()

# Slide on collapse_by()
# - faster than lubridate::floor_index() , base R solution, 
# - more flexible, support for other time index columns, support for a vector of arbitrary dates as breakpoints
# - show slide on arbitrary breakpoints

# Slide on time_formula + filter_time()
  
microbenchmark::microbenchmark(
  stats_2017_11 %>%
    mutate(date = lubridate::ceiling_date(date, "hour")) %>%
    group_by(date),
  stats_2017_11 %>%
    collapse_by("hour"),
  times = 5
)

stats_2017_11 %>%
  collapse_by(as.POSIXct("2017-11-01 00:59:55", "UTC"))



#############

ecommerce <- read_csv("data/e-commerce.csv")

ecommerce <- ecommerce %>%
  mutate(InvoiceDate = lubridate::mdy_hm(InvoiceDate))

ecommerce %>%
  filter(Quantity > 0) %>%
  as_tbl_time(InvoiceDate) %>%
  collapse_by("quarter") %>%
  summarise(profit = sum(UnitPrice * Quantity)) %>%
  ggplot(aes(x = InvoiceDate, y = profit)) +
  geom_point()

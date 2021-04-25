# Title     : Zadanko 1
# Created by: mateu
# Created on: 01.04.2021

library(dplyr)
library(tibble)
library(ggplot2)

# Exercise 1

orange.df <- read.csv("ORANGEPL.mst") # drop redundant ticker column
pekao.df <- read.csv("PEKAO.mst")

prepare_data <- function(df) {
  names(df) <- c('ticker', 'date', 'open', 'high', 'low', 'close', 'vol')

  df %>%
    filter(between(date, 20190700, 20191231)) %>%
    mutate(
      date = as.Date.character(date, format = "%Y%m%d"),
      delta = 100 * (close - lag(close)) / lag(close)
    ) %>%
    filter(row_number() != 1)
}

orange.df <- prepare_data(orange.df)
pekao.df <- prepare_data(pekao.df)

# 2.a) draw delta %

draw_delta <- function(df) {
  ggplot(df, aes(date, delta)) +
    geom_path(colour = "blue") +
    theme_bw() +
    labs(
      title = paste("[2019] Procentowe zmiany cen zamknięcia spółki", df[1, 1]),
      x = "Data",
      y = "Zmiany cen zamknięcia [%]"
    )
}

draw_delta(orange.df)
draw_delta(pekao.df)

# b) histogram + boxplot

draw_histogram <- function(df) {
  ggplot(df, aes(delta)) +
    theme_bw() +
    geom_histogram(aes(y = ..density..), colour = "white", fill = "blue") +
    labs(
      title = paste("[2019] Histogram procentowych zmian zamknięcia spółki", df[1, 1]),
      x = "Zmiany cen zamknięcia [%]",
      y = "Częstość występowania"
    )
}

draw_boxplot <- function(df) {
  ggplot(df, aes(ticker, delta)) +
    theme_bw() +
    geom_boxplot() +
    labs(
      title = paste("[2019] Boxplot procentowych zmian zamknięcia spółki", df[1, 1]),
      x = "",
      y = "Zmiany cen zamknięcia [%]"
    )
}

draw_histogram(orange.df)
draw_boxplot(orange.df)
draw_histogram(pekao.df)
draw_boxplot(pekao.df)

# c) estimated distribution parameters

orange.mean <- mean(orange.df$delta)
orange.sd <- sd(orange.df$delta)
pekao.mean <- mean(pekao.df$delta)
pekao.sd <- sd(pekao.df$delta)

# d) compare model and data

draw_histogram(orange.df) +
  stat_function(fun = dnorm, args = list(mean = orange.mean, sd = orange.sd), colour = "red", size = 1.5)

draw_histogram(pekao.df) +
  stat_function(fun = dnorm, args = list(mean = pekao.mean, sd = pekao.sd), colour = "red", size = 3)

# Exercise 2

orange.df <- arrange(read.csv("ORANGEPL.csv"), date)

# a) transaction volume distribution over 3 phases

open_phase <- orange.df %>%
  filter(time == 090000) %>%
  group_by(date, time, price) %>%
  summarise(volume = sum(volume)) %>%
  filter(row_number() == 1)

continuous_phase <- orange.df %>%
  filter(between(time, 090000, 165100)) %>%
  group_by(date) %>%
  summarise(volume = sum(volume)) %>%
  mutate(volume = volume - open_phase$volume)

close_phase <- orange.df %>%
  filter(time > 165900) %>%
  group_by(date) %>%
  summarise(volume = sum(volume))

phases_combined <- open_phase %>%
  ungroup() %>%
  rename(open_volume = volume) %>%
  mutate(date = as.Date.character(date, format = "%y%m%d")) %>%
  add_column(continuous_volume = continuous_phase$volume) %>%
  add_column(close_volume = close_phase$volume) %>%
  select(-time, -price)

ggplot(phases_combined, aes(date)) +
  geom_line(aes(
    y = 100 * open_volume / (open_volume + continuous_volume + close_volume),
    color = "open phase")) +
  geom_line(aes(
    y = 100 * continuous_volume / (open_volume + continuous_volume + close_volume),
    color = "continuous phase")) +
  geom_line(aes(
    y = 100 * close_volume / (open_volume + continuous_volume + close_volume),
    color = "close phase")) +
  labs(
    title = "Transaction volume of ORANGEPL",
    x = "Day",
    y = "Daily volume contribution"
  )

# c) minimal distinct seconds count in 2-hour interval

compute_minimal_distinct_seconds_count <- function(hour_from, hour_to) {
  orange.df %>%
    filter(between(time, hour_from, hour_to)) %>%
    group_by(date) %>%
    summarise(distinct_seconds = n_distinct(time)) %>%
    ungroup() %>%
    filter(distinct_seconds == min(distinct_seconds)) %>%
    mutate(hour_from = hour_from, hour_to = hour_to)
}

minimal_interval_metadata <- bind_rows(
  compute_minimal_distinct_seconds_count(100000, 115959),
  compute_minimal_distinct_seconds_count(110000, 125959),
  compute_minimal_distinct_seconds_count(120000, 135959),
  compute_minimal_distinct_seconds_count(130000, 145959),
  compute_minimal_distinct_seconds_count(140000, 155959),
) %>%
  filter(distinct_seconds == min(distinct_seconds))

minimal_interval_data <- orange.df %>%
  filter(
    date == minimal_interval_metadata$date
      & between(time, minimal_interval_metadata$hour_from, minimal_interval_metadata$hour_to)
  ) %>%
  select(time) %>%
  distinct() %>%
  mutate(time = as.numeric(as.character(substr(time, 1, 4)))) %>%
  group_by(time) %>%
  summarise(seconds_count = n()) %>%
  add_row(time = 1100:1159, seconds_count = 0) %>%  # filling empty minutes
  add_row(time = 1200:1259, seconds_count = 0) %>%
  arrange(time, desc(seconds_count)) %>%
  group_by(time) %>%
  filter(row_number() == 1) %>% # remove fake 0 values for duplicated times
  ungroup() %>%
  mutate(time = 1:120)

# i illustrate active seconds (transaction-wise)

ggplot(minimal_interval_data) +
  geom_point(aes(x = time, y = seconds_count)) +
  labs(
    title = paste("[", minimal_interval_metadata$date, "]", " Period between ", minimal_interval_metadata$hour_from, " and ", minimal_interval_metadata$hour_to),
    x = "Time [min]",
    y = "Number of active seconds"
  )





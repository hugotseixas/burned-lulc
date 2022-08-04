# HEADER ----------------------------------------------------------------------
#
# Title:
# Description:
#
# Author:         Hugo Tameirao Seixas
# Contact:        hugo.seixas@alumni.usp.br / tameirao.hugo@gmail.com
# Date:           2021-02-06
#
# Notes:
#
# LIBRARIES -------------------------------------------------------------------
#
library(arrow)
library(lubridate)
library(fs)
library(glue)
library(tsibble)
library(feasts)
library(tidyverse)
#
# OPTIONS ---------------------------------------------------------------------
#

#
# READ SAMPLE DATA SET --------------------------------------------------------

# Read data set
ds_sample <- open_dataset("data/tables/sample/")

# Create list of samples
sample_id_list <-
  dir_ls("data/tables/sample/") %>%
  str_extract("(?<=sample_id=).*") %>%
  as.integer()

# PROCESS MONTHLY VALUES ------------------------------------------------------

## Transform to long format ----
# Maybe get median and standard deviation
sample_mvar_long <-
  map_df(
    .x = sample_id_list,
    .f =
      ~ {

        sample <- ds_sample %>%
          filter(sample_id == .x) %>%
          collect() %>%
          group_by(group_name, pair_id, date) %>%
          summarise_at(vars(mon_burn:precip), mean, na.rm = TRUE) %>%
          ungroup()

        sample <- sample %>%
          filter(mon_burn >= 1) %>% # Get the year of the burn
          rename(burn_date = date) %>%
          select(pair_id, burn_date) %>%
          full_join(sample, by = "pair_id") %>% # Regenerate table
          mutate(
            relative_date = lubridate::interval(burn_date, date) %/% months(1)
          ) %>%
          select(-burn_date)

        sample %>%
          select(-mon_burn) %>%
          pivot_longer(et:precip, names_to = "var") %>%
          filter(relative_date >= -84, relative_date <= 84) %>%
          group_by(relative_date, group_name, var) %>%
          summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
          filter(relative_date != 0) %>%
          mutate(period = if_else(relative_date >= 0, "after", "before")) %>%
          group_by(group_name, var, period) %>%
          group_nest() %>%
          mutate(data = map(
            data,
            ~ .x %>%
              mutate(
                date = yearmonth(
                  seq(
                    ymd("2000-01-01"),
                    ymd("2006-12-01"),
                    by = "month"
                  )
                )
              ) %>%
              select(-relative_date)
            )
          ) %>%
          mutate(data = map(data, ~ as_tsibble(.x, index = date))) %>%
          mutate(
            model = map(
              data,
              ~ .x %>%
                model(STL(value ~ season() + trend())) %>%
                components() %>%
                as_tibble()
            )
          ) %>%
          unnest(cols = model) %>%
          mutate(period = factor(period, levels = c("before", "after"))) %>%
          select(-c(data, .model)) %>%
          mutate(sample_id = .x)

      }
  )

diff <- sample_mvar_long %>%
  group_by(group_name, var, period, date) %>%
  summarise(
    mean = mean(trend),
    .groups = "drop"
  ) %>%
  group_by(group_name, var) %>%
  mutate(relative_date = c(-84:-1, 1:84)) %>%
  filter(var %in% c("et", "evi", "lai", "lst", "ndvi")) %>%
  ungroup() %>%
  mutate(
    control = if_else(group_name == "burn_id", 1, 0),
    period = if_else(period == "after", 1, 0),
    date_class = abs(relative_date)
  ) %>%
  group_by(var, date_class) %>%
  group_nest() %>%
  mutate(
    model = map(
      data,
      ~ lm(mean ~ control + period + control * period, data = .)
    ),
    coef_info = map(model, tidy)
  ) %>%
  unnest(cols = coef_info)

diff %>%
  filter(
    term == "control:period",
    var %in% c("et", "evi", "lai", "lst", "ndvi")
  ) %>%
  select(var, date_class, estimate) %>%
  ggplot() +
  facet_wrap( ~ var, ncol = 1, scales = "free") +
  geom_col(aes(x = date_class, y = estimate)) +
  scale_fill_viridis_c() +
  theme_bw()

sample_mvar_long %>%
  group_by(group_name, var, period, date) %>%
  summarise(
    mean = mean(season_year),
    low = quantile(season_year, 0.025),
    high = quantile(season_year, 0.975),
    .groups = "drop"
  ) %>%
  group_by(group_name, var) %>%
  mutate(relative_date = c(-84:-1, 1:84)) %>%
  filter(var %in% c("et", "evi", "lai", "lst", "ndvi")) %>%
  ggplot(aes(x = relative_date, color = group_name, fill = group_name)) +
  facet_grid(var ~ period, scales = "free") +
  geom_line(aes(y = mean)) +
  geom_ribbon(
    aes(ymin = low, ymax = high),
    alpha = 0.3,
    color = "transparent"
  ) +
  scale_color_manual(values = c("#882255", "#117733")) +
  scale_fill_manual(values = c("#882255", "#117733")) +
  theme_bw() +
  theme(
    legend.position = "",
    axis.title = element_blank()
  )

sample_mvar_long %>%
  group_by(group_name, var, period, date) %>%
  summarise(
    mean = mean(trend),
    low = quantile(trend, 0.025),
    high = quantile(trend, 0.975),
    .groups = "drop"
  ) %>%
  group_by(group_name, var) %>%
  mutate(relative_date = c(-84:-1, 1:84)) %>%
  filter(var %in% c("et", "evi", "lai", "lst", "ndvi")) %>%
  ggplot(aes(x = relative_date, color = group_name, fill = group_name)) +
  facet_grid(var ~ period, scales = "free") +
  geom_line(aes(y = mean)) +
  geom_ribbon(
    aes(ymin = low, ymax = high),
    alpha = 0.3,
    color = "transparent"
  ) +
  scale_color_manual(values = c("#882255", "#117733")) +
  scale_fill_manual(values = c("#882255", "#117733")) +
  theme_bw() +
  theme(
    legend.position = "",
    axis.title = element_blank()
  )

sample_mvar_long %>%
  group_by(group_name, var, period, date) %>%
  summarise(
    mean = mean(value),
    low = quantile(value, 0.025),
    high = quantile(value, 0.975),
    .groups = "drop"
  ) %>%
  group_by(group_name, var) %>%
  mutate(relative_date = c(-84:-1, 1:84)) %>%
  filter(var %in% c("et", "evi", "lai", "lst", "ndvi")) %>%
  ggplot(aes(x = relative_date, color = group_name, fill = group_name)) +
  facet_grid(var ~ period, scales = "free") +
  geom_line(aes(y = mean)) +
  geom_ribbon(
    aes(ymin = low, ymax = high),
    alpha = 0.3,
    color = "transparent"
  ) +
  scale_color_manual(values = c("#882255", "#117733")) +
  scale_fill_manual(values = c("#882255", "#117733")) +
  theme_bw() +
  theme(
    legend.position = "",
    axis.title = element_blank()
  )

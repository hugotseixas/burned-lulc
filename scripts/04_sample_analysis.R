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
library(ggridges)
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
  str_extract("(?<=sample_id=).*")

# PROCESS MONTHLY VALUES ------------------------------------------------------

## Transform to long format ----
# Maybe get median and standard deviation
sample_mvar_long <-
  map_df(
    .x = sample_id_list,
    .f =
      ~ {

        ds_sample %>%
          filter(sample_id == .x) %>%
          collect() %>%
          filter(relative_year >= -15 & relative_year <= 15) %>%
          group_by(group_name, id, relative_year) %>%
          summarise_at(vars(et:precip), mean, na.rm = TRUE) %>%
          ungroup() %>%
          pivot_longer(et:precip, names_to = "var", values_to = "value") %>%
          mutate(sample_id = .x)

      }
  )

## Transform to paired observations ----
sample_mvar_paired <-
  map_df(
    .x = sample_id_list,
    .f =
      ~ {

        ds_sample %>%
          filter(sample_id == .x) %>%
          collect() %>%
          group_by(group_name) %>%
          group_map(
            ~ {

              group_name <- str_remove(.y, "_id")

              values_name <- as.character(glue("{group_name}_value"))

              .x %>%
                filter(relative_year >= -15 & relative_year <= 15) %>%
                group_by(pair_id, id, relative_year) %>%
                summarise_at(vars(et:precip), mean, na.rm = TRUE) %>%
                ungroup() %>%
                pivot_longer(
                  et:precip,
                  names_to = "var",
                  values_to = values_name
                ) %>%
                rename(!!sym(glue("{group_name}_id")) := id)

            }
          ) %>%
          reduce(inner_join, by = c("pair_id", "relative_year", "var")) %>%
          mutate(
            diff = control_value - burn_value,
            sample_id = .x
          )

      }
  )

# CREATE PLOTS ----------------------------------------------------------------

## Ridges of median values time series ----
sample_mvar_long %>%
  filter(var != "precip") %>%
  group_by(sample_id, group_name, relative_year, var) %>%
  summarise(value = median(value, na.rm = TRUE), .groups = "drop") %>%
  ggplot() +
  facet_wrap( ~ var, ncol = 2, scales = "free_y") +
  geom_density_ridges(
    stat = "density",
    alpha = 0.8,
    aes(
      x = value,
      y = relative_year,
      group = interaction(relative_year, group_name),
      fill = group_name,
      height = stat(density)
    )
  ) +
  scale_fill_manual(values = c("#CC3311", "#009988")) +
  labs(color = "group") +
  theme_bw() +
  coord_flip() +
  theme(text = element_text(size = 11)) +
  ggsave("figs/median_ridges.png", width = 15, height = 18, units = "cm")

## Median values time series ----
ggplot(sample_mvar_long) +
  facet_wrap( ~ var, ncol = 2, scales = "free_y") +
  annotate(
    "rect",
    xmin = 0, xmax = Inf, ymin = -Inf, ymax = Inf, alpha = 0.2
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.3) +
  stat_summary(
    fun = median,
    na.rm = TRUE,
    geom = "line",
    alpha = 0.1,
    size = 0.3,
    aes(
      x = relative_year,
      y = value,
      group = interaction(sample_id, group_name),
      color = group_name
    )
  ) +
  stat_summary(
    fun = median,
    na.rm = TRUE,
    geom = "line",
    size = 0.8,
    aes(
      x = relative_year,
      y = value,
      group = group_name,
      color = group_name
    )
  ) +
  scale_color_manual(values = c("#CC3311", "#009988")) +
  labs(color = "group") +
  theme_bw() +
  theme(text = element_text(size = 11)) +
  ggsave("figs/median_lines.png", width = 15, height = 18, units = "cm")

## Ridges from differences ----
# Maybe scale variables and remove 0.05 and 0.95 extremes?
sample_mvar_paired %>%
  filter(var != "precip") %>%
  group_by(sample_id, relative_year, var) %>%
  summarise(diff = median(diff, na.rm = TRUE), .groups = "drop") %>%
  ggplot() +
  facet_wrap( ~ var, ncol = 2, scales = "free_y") +
  geom_density_ridges(
    quantile_lines = TRUE,
    quantiles = 2,
    alpha = 0.8,
    scale = 2,
    aes(
      x = diff,
      y = relative_year,
      group = relative_year
    )
  ) +
  theme_bw() +
  coord_flip() +
  theme(
    text = element_text(size = 11),
    legend.position = ""
  ) +
  ggsave("figs/diff_ridges.png", width = 15, height = 18, units = "cm")

## Differences time series ----
ggplot(sample_mvar_paired %>% filter(var != "precip")) +
  facet_wrap( ~ var, ncol = 2, scales = "free_y") +
  annotate(
    "rect",
    xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0, alpha = 0.2
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.3) +
  stat_summary(
    fun = median,
    na.rm = TRUE,
    geom = "line",
    alpha = 0.1,
    size = 0.3,
    aes(
      x = relative_year,
      y = diff,
      group = sample_id
    )
  ) +
  stat_summary(
    fun = median,
    na.rm = TRUE,
    geom = "line",
    size = 0.8,
    aes(
      x = relative_year,
      y = diff,
      group = 1
    )
  ) +
  theme_bw() +
  theme(text = element_text(size = 11)) +
  ggsave("figs/diff_lines.png", width = 15, height = 18, units = "cm")

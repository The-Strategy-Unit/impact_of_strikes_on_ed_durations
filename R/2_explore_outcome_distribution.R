# README

# This script explores the distribution of the model outcome variable
# duration_ed


# 0 set up ----

library("here") 
library("dplyr")
library("purrr") 
library("readr") 
library("tidyr")
library("ggplot2") 
library("janitor")
library("stringr")
library("lubridate")
library("targets")

# 1 load data ----
model_df <- readRDS(here("data", "model_df.RDS"))
model_df_cut <- readRDS(here("data", "model_df_cut.RDS"))


# 2 plot duration ed distribution ----
# 2a all atts
model_df_cut |> 
  group_by(duration_ed) |> 
  summarise(n = n()) |> 
  ggplot() +
  geom_vline(aes(xintercept = 240)) +
  geom_point(aes(x = duration_ed, y = n)) +
  geom_smooth(aes(x = duration_ed, y = n),
              method = "loess",
              span = 0.1)


# 2b by strike ----
model_df_cut |> 
  group_by(duration_ed, jr_docs_strike) |> 
  summarise(n = n()) |> 
  ggplot() +
  geom_vline(aes(xintercept = 240)) +
  geom_point(aes(x = duration_ed, y = n)) +
  geom_smooth(aes(x = duration_ed, y = n),
              method = "loess",
              span = 0.1) +
  facet_wrap(vars(jr_docs_strike),
             scales = "free_y")  +
  scale_x_continuous(limits = c(0, 60*24*2))


# 3 derive 4hr distortion metric

model_df |> 
  group_by(duration_ed) |> 
  summarise(n = n()) |> 
  ggplot() +
  geom_vline(aes(xintercept = 240)) +
  geom_point(aes(x = duration_ed, y = n)) +
  geom_smooth(aes(x = duration_ed, y = n),
              method = "loess",
              span = 0.1) +
  scale_x_continuous(limits = c(220, 260))


interval <- 10


model_df  |> 
  filter(duration_ed >= 4 * 60 - interval) |> 
  filter(duration_ed <= 4 * 60 + interval) |> 
  mutate(pre4hr = ifelse(duration_ed <= 240, 1, 0)) |> 
  mutate(mid_arr_dep_dttm_rd_day = round_date(mid_arr_dep_dttm, "day")) |> 
  group_by(mid_arr_dep_dttm_rd_day, jr_docs_strike) |> 
  summarise(distort4hr = sum(pre4hr) / (n() - sum(pre4hr))) |> 
  ggplot() +
  geom_point(aes(x = mid_arr_dep_dttm_rd_day, y = distort4hr, colour = as.factor(jr_docs_strike))) +
  geom_smooth(aes(x = mid_arr_dep_dttm_rd_day, y = distort4hr, colour = as.factor(jr_docs_strike)),
              method = "loess",
              span = 0.5)
    
  
model_df  |> 
  filter(duration_ed >= 4 * 60 - interval) |> 
  filter(duration_ed <= 4 * 60 + interval) |> 
  mutate(pre4hr = ifelse(duration_ed <= 240, 1, 0)) |> 
  mutate(mid_arr_dep_dttm_rd_day = round_date(mid_arr_dep_dttm, "day")) |> 
  group_by(mid_arr_dep_dttm_rd_day, is_weekend) |> 
  summarise(distort4hr = sum(pre4hr) / (n() - sum(pre4hr))) |> 
  ggplot() +
  geom_point(aes(x = mid_arr_dep_dttm_rd_day, y = distort4hr, colour = as.factor(is_weekend))) +
  geom_smooth(aes(x = mid_arr_dep_dttm_rd_day, y = distort4hr, colour = as.factor(is_weekend)),
              method = "loess",
              span = 0.5)







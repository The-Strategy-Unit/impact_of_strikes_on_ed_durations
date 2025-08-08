# This script counts ho many records were excluded at SQL pre-procesing stage


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


# 1 load files ----

provider_sample_df <- 
  readRDS(here("data", "model_provider_sample_df.RDS")) |> 
  ungroup() |> 
  filter(procode != "RJZ")

ecds_events <- 
  readRDS(here("data", "ecds_events_full_df.RDS")) |> 
  ungroup() |> 
  inner_join(provider_sample_df |> 
               dplyr::select(procode),
             join_by(procode)) |> 
  mutate(arr_date = floor_date(dttm_arr, "days")) |> 
  filter(arr_date >= as_date('2022-08-01'),
         arr_date <= as_date('2024-07-31')) 
  

ecds_events_before_exclusions <- 
  readRDS(here("data", "ecds_events_full_before_exclusions_df.RDS")) |> 
  ungroup()   |> 
  inner_join(provider_sample_df |> 
                           dplyr::select(procode),
                         join_by(procode)) |> 
  mutate(arr_date = floor_date(dttm_arr, "days")) |> 
  filter(arr_date >= as_date('2022-08-01'),
         arr_date <= as_date('2024-07-31')) 


excluded_cases <- nrow(ecds_events_before_exclusions) - nrow(ecds_events)

p_excluded_cases <- excluded_cases / nrow(ecds_events_before_exclusions) 



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

# 1 create table

etr_ods <- read.csv(here("data_raw", "etr.csv"),
                    header = FALSE) |> 
  dplyr::select(procode = V1,
                provider_name = V2) |> 
  mutate(provider_name = str_to_title(provider_name)) |> 
  mutate(provider_name= gsub("Nhs", "NHS", provider_name))


provider_sample_names<- 
  readRDS(here("data", "model_provider_sample_df.RDS")) |> 
  ungroup() |> 
  filter(procode != "RJZ") |> 
  left_join(etr_ods,
            join_by(procode)) |> 
  dplyr::select(procode, provider_name)

# 2 save table ----

write.csv(provider_sample_names,
          here("data_raw", "provider_sample_names.csv"))



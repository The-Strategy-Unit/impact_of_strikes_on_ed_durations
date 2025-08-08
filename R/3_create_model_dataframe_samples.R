# README

## This script create samples of the model_df data.frame of various sizes

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
library("scales")
library("targets")

# 1 load data ----
model_df <- readRDS(here("data", "model_df.RDS")) 

gc()

# 2a create df sample 1m records ----
set.seed(8190)

model_df_cut_1m <- 
  model_df |> 
  slice_sample(n = 1000000) 

gc()



# 2b create df sample 200k records ----
set.seed(71845)

model_df_cut_200k <- 
  model_df |> 
  slice_sample(n = 200000) 
gc()
  


# 2c create df sample 20k records ----
set.seed(234)

model_df_cut_20k <- 
  model_df |> 
  slice_sample(n = 20000) 

gc()

# 2d create df sample 50% sample ----
set.seed(352)

model_df_cut_50pc <- 
  model_df |> 
  slice_sample(prop = 0.5) 

gc()


# 3 save files ----
saveRDS(model_df_cut_1m, here("data", "model_df_cut_1m.RDS"))
# model_df_cut_1m <- readRDS(here("data", "model_df_cut_1m.RDS"))

saveRDS(model_df_cut_200k, here("data", "model_df_cut_200k.RDS"))
# model_df_cut_200k <- readRDS(here("data", "model_df_cut_200k.RDS"))

saveRDS(model_df_cut_20k, here("data", "model_df_cut_20k.RDS"))
# model_df_cut_20k<- readRDS(here("data", "model_df_cut_20k.RDS"))

saveRDS(model_df_cut_50pc, here("data", "model_df_cut_50pc.RDS"))
# model_df_cut_50pc<- readRDS(here("data", "model_df_cut_50pc.RDS"))
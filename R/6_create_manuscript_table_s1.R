# README

# this script generates some information and table that set out how we arrive at 
# the study dataset, it size, and national share, 
# the number of missing-value excludsions,
# and the eprsentativeness of of the dataset

# 0 set-up ----
library("tidyverse")
library("here")

# 1 load data ----

ecds_events_before_exclusions <- 
  readRDS(here("data", "ecds_events_full_before_exclusions_df.RDS")) |> 
  ungroup() 



ecds_events <- 
  readRDS(here("data", "ecds_events_full_df.RDS")) |> 
  ungroup() 

model_df_stage11 <- readRDS(here("data", "model_df_stage11.RDS")) 

model_df_stage12 <- readRDS(here("data", "model_df.RDS")) 

model_df_cut_50pc<- readRDS(here("data", "model_df_cut_50pc.RDS"))


# 2 key figures for manuscript ----

# records
nrow(model_df_stage11)

# share of national records
nrow(model_df_stage11) / nrow(ecds_events)

# records removed due to missing values
nrow(model_df_stage11) - nrow(model_df_stage12)

(nrow(model_df_stage11) - nrow(model_df_stage12)) /nrow(model_df_stage11)

# what proportion of provides included
ecds_events |> 
  group_by(procode) |> 
  summarise(n_all = n()) |> 
  left_join(model_df_stage11 |> 
              group_by(procode) |> 
              summarise(n_analysis_dataset = n()),
            join_by(procode)) |> 
  left_join(model_df_cut_50pc |> 
              group_by(procode) |> 
              summarise(n_model_dataset = n()),
            join_by(procode)) |> 
  arrange(-n_model_dataset, -n_all) |> 
  View()
              
20/127

# how many records excluded before processing


# model dataset
nrow(model_df_cut_50pc)

# 3 manuscript table s1 (dataset representativeness) ----

# total
table_s1_all <-
  ecds_events |> 
  summarise(n = n()) |> 
  mutate(ds = "all_eng") |> 
  bind_rows(model_df_stage11 |> 
              summarise(n = n()) |> 
              mutate(ds = "20_trusts")) |> 
  bind_rows(model_df_cut_50pc |> 
              summarise(n = n()) |> 
              mutate(ds = "mod_50pc")) |> 
  mutate(grouping = 'all',
         group = 'all',
         p = NA_real_)

table_s1_sex <-
  ecds_events |> 
  mutate(sex = case_when(sex == 'm' ~ 'male',
                         sex == 'f' ~ 'female',
                         TRUE ~ "other / not recorded")) |> 
  group_by(sex) |> 
  summarise(n = n()) |> 
  mutate(p = n / sum(n)) |> 
  mutate(ds = "all_eng") |> 
  bind_rows(model_df_stage11 |> 
              mutate(sex = case_when(sex == 'm' ~ 'male',
                                     sex == 'f' ~ 'female',
                                     TRUE ~ "other / not recorded")) |> 
              group_by(sex) |> 
              summarise(n = n()) |> 
              mutate(p = n / sum(n)) |> 
              mutate(ds = "20_trusts")) |> 
  bind_rows(model_df_cut_50pc |> 
              mutate(sex = case_when(sex == 'm' ~ 'male',
                                     TRUE ~ 'female')) |> 
              group_by(sex) |> 
              summarise(n = n()) |> 
              mutate(p = n / sum(n)) |> 
              mutate(ds = "mod_50pc")) |> 
  mutate(grouping = 'sex') |> 
  rename(group = sex) 

table_s1_age <-
  ecds_events |> 
  mutate(age = case_when(age < 18 ~ '0 17 yrs',
                         age < 65 ~ '18 - 64 yrs',
                         age < 120 ~ '65+ yrs',
                         TRUE ~ "other / not recorded")) |> 
  group_by(age) |> 
  summarise(n = n()) |> 
  mutate(p = n / sum(n)) |> 
  mutate(ds = "all_eng") |> 
  bind_rows(model_df_stage11 |> 
              mutate(age = case_when(age < 18 ~ '0 17 yrs',
                                     age < 65 ~ '18 - 64 yrs',
                                     age < 120 ~ '65+ yrs',
                                     TRUE ~ "other / not recorded")) |> 
              group_by(age) |> 
              summarise(n = n()) |> 
              mutate(p = n / sum(n)) |> 
              mutate(ds = "20_trusts")) |> 
  bind_rows(model_df_cut_50pc |> 
              mutate(age = case_when(age < 18 ~ '0 17 yrs',
                                     age < 65 ~ '18 - 64 yrs',
                                     age < 120 ~ '65+ yrs',
                                     TRUE ~ "other / not recorded")) |> 
              group_by(age)  |> 
              summarise(n = n()) |> 
              mutate(p = n / sum(n)) |> 
              mutate(ds = "mod_50pc")) |> 
  mutate(grouping = 'age') |> 
  rename(group = age) 


table_s1_imd <-
  ecds_events |> 
  mutate(imdq = case_when(imd_dec %in% c("1", "2") ~ '1 (most deprived)',
                          imd_dec %in% c("3", "4") ~ '2',
                          imd_dec %in% c("5", "6") ~ '3',
                          imd_dec %in% c("7", "8") ~ '4',
                          imd_dec %in% c("9", "10") ~ '5',
                         TRUE ~ "other / not recorded")) |> 
  group_by(imdq) |> 
  summarise(n = n()) |> 
  mutate(p = n / sum(n)) |> 
  mutate(ds = "all_eng") |> 
  bind_rows(model_df_stage11 |> 
              mutate(imdq = case_when(imd_dec %in% c("1", "2") ~ '1 (most deprived)',
                                      imd_dec %in% c("3", "4") ~ '2',
                                      imd_dec %in% c("5", "6") ~ '3',
                                      imd_dec %in% c("7", "8") ~ '4',
                                      imd_dec %in% c("9", "10") ~ '5',
                                      TRUE ~ "other / not recorded")) |> 
              group_by(imdq) |> 
              summarise(n = n()) |> 
              mutate(p = n / sum(n)) |> 
              mutate(ds = "20_trusts")) |> 
  bind_rows(model_df_cut_50pc |> 
              mutate(imdq = case_when(imd_dec %in% c("1", "2") ~ '1 (most deprived)',
                                      imd_dec %in% c("3", "4") ~ '2',
                                      imd_dec %in% c("5", "6") ~ '3',
                                      imd_dec %in% c("7", "8") ~ '4',
                                      imd_dec %in% c("9", "10") ~ '5',
                                      TRUE ~ "other / not recorded")) |> 
              group_by(imdq) |> 
              summarise(n = n()) |> 
              mutate(p = n / sum(n)) |> 
              mutate(ds = "mod_50pc")) |> 
  mutate(grouping = 'deprivaton quintile') |> 
  rename(group = imdq) 

table_s1_arr_mode <-
  ecds_events |> 
  mutate(arr_mode = case_when(arr_mode == "amb" ~ "by ambulance",
                              arr_mode == "walk_in" ~ "walk in",
                              TRUE ~ "other / not recorded")) |> 
  group_by(arr_mode) |> 
  summarise(n = n()) |> 
  mutate(p = n / sum(n)) |> 
  mutate(ds = "all_eng") |> 
  bind_rows(model_df_stage11 |> 
              mutate(arr_mode = case_when(arr_mode == "amb" ~ "by ambulance",
                                          arr_mode == "walk_in" ~ "walk in",
                                          TRUE ~ "other / not recorded")) |> 
              group_by(arr_mode) |> 
              summarise(n = n()) |> 
              mutate(p = n / sum(n)) |> 
              mutate(ds = "20_trusts")) |> 
  bind_rows(model_df_cut_50pc |> 
              mutate(arr_mode = case_when(arr_mode == "amb" ~ "by ambulance",
                                          arr_mode == "walk_in" ~ "walk in",
                                          TRUE ~ "other / not recorded")) |> 
              group_by(arr_mode) |> 
              summarise(n = n()) |> 
              mutate(p = n / sum(n)) |> 
              mutate(ds = "mod_50pc")) |> 
  mutate(grouping = 'arrival mode') |> 
  rename(group = arr_mode) 

table_s1_acuity <-
  ecds_events |> 
  mutate(acuity = case_when(acuity == 1 ~ "1 Immediate resuscitation",
                            acuity == 2 ~ "2 Very urgent",
                            acuity == 3 ~ "3 Urgent",
                            acuity == 4 ~ "4 Standard",
                            acuity == 5 ~ "5 Non-urgent",
                              TRUE ~ "other / not recorded")) |> 
  group_by(acuity) |> 
  summarise(n = n()) |> 
  mutate(p = n / sum(n)) |> 
  mutate(ds = "all_eng") |> 
  bind_rows(model_df_stage11 |> 
              mutate(acuity = case_when(acuity == 1 ~ "1 Immediate resuscitation",
                                        acuity == 2 ~ "2 Very urgent",
                                        acuity == 3 ~ "3 Urgent",
                                        acuity == 4 ~ "4 Standard",
                                        acuity == 5 ~ "5 Non-urgent",
                                        TRUE ~ "other / not recorded")) |> 
              group_by(acuity) |> 
              summarise(n = n()) |> 
              mutate(p = n / sum(n)) |> 
              mutate(ds = "20_trusts")) |> 
  bind_rows(model_df_cut_50pc |> 
              mutate(acuity = case_when(acuity == 1 ~ "1 Immediate resuscitation",
                                        acuity == 2 ~ "2 Very urgent",
                                        acuity == 3 ~ "3 Urgent",
                                        acuity == 4 ~ "4 Standard",
                                        acuity == 5 ~ "5 Non-urgent",
                                        TRUE ~ "other / not recorded")) |> 
              group_by(acuity) |> 
              summarise(n = n()) |> 
              mutate(p = n / sum(n)) |> 
              mutate(ds = "mod_50pc")) |> 
  mutate(grouping = 'acuity') |> 
  rename(group = acuity) 


table_s1_dur_ed <-
  ecds_events |> 
  mutate(dur_ed = case_when(duration_ed < 60 ~ "< 1 hr",
                            duration_ed < 180 ~ "< 1-3 hrs",
                            duration_ed < 360 ~ "< 4-6 hr",
                            duration_ed <= 4500 ~ "< 7+ hrs",
                            TRUE ~ "other / not recorded")) |> 
  group_by(dur_ed) |> 
  summarise(n = n()) |> 
  mutate(p = n / sum(n)) |> 
  mutate(ds = "all_eng") |> 
  bind_rows(model_df_stage11 |> 
              mutate(dur_ed = case_when(duration_ed < 60 ~ "< 1 hr",
                                        duration_ed < 180 ~ "< 1-3 hrs",
                                        duration_ed < 360 ~ "< 4-6 hr",
                                        duration_ed <= 4500 ~ "< 7+ hrs",
                                        TRUE ~ "other / not recorded")) |> 
              group_by(dur_ed) |> 
              summarise(n = n()) |> 
              mutate(p = n / sum(n)) |> 
              mutate(ds = "20_trusts")) |> 
  bind_rows(model_df_cut_50pc |> 
              mutate(dur_ed = case_when(duration_ed < 60 ~ "< 1 hr",
                                        duration_ed < 180 ~ "< 1-3 hrs",
                                        duration_ed < 360 ~ "< 4-6 hr",
                                        duration_ed <= 4500 ~ "< 7+ hrs",
                                        TRUE ~ "other / not recorded")) |> 
              group_by(dur_ed) |> 
              summarise(n = n()) |> 
              mutate(p = n / sum(n)) |> 
              mutate(ds = "mod_50pc")) |> 
  mutate(grouping = 'duration in ED') |> 
  rename(group = dur_ed) 

manuscript_table_s1 <-
  bind_rows(table_s1_all, 
            table_s1_sex,
            table_s1_age,
            table_s1_imd,
            table_s1_arr_mode,
            table_s1_acuity,
            table_s1_dur_ed) |> 
  pivot_wider(names_from = "ds",
              values_from = c("n", "p"),
              values_fill = 0) 

# 4 save files ----
write.csv(manuscript_table_s1, here("data", "manuscript_table_s1.csv"))



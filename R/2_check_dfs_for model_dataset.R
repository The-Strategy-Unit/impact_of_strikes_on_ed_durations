# This script checks the data frames that will be used to construct the dataset
# for the regression models.  they have been built  from a 
# number of NCDR / UDAL extracts and other data files.


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

# 1 load base files ----

provider_sample_df <- readRDS(here("data", "model_provider_sample_df.RDS")) |> ungroup()

ecds_events <- readRDS(here("data", "ecds_events_df.RDS")) |> ungroup()

ed_type1_activity_prov_arrmode_hour_day <- readRDS(here("data", "ed_type1_act_df.RDS")) |> ungroup()

ed_type3_activity_icb_hour_day <- readRDS(here("data", "ed_type3_act_df.RDS")) |> ungroup()

ed_occ_prov_hour_day <- readRDS(here("data", "ed_occ_df.RDS")) |> ungroup()

apc_occ_prov_pod_day <- readRDS(here("data", "apc_occ_df.RDS")) |> ungroup()

op_activity_prov_day <- readRDS(here("data", "op_act_df.RDS")) |> ungroup()

comm_activity_icb_day <- readRDS(here("data", "comm_act_df.RDS")) |> ungroup()

dids_activity_prov_pod_day <- readRDS(here("data", "dids_act_df.RDS")) |> 
  inner_join(provider_sample_df, 
             join_by(procode)) |> 
  ungroup()

strikes_staff_hour_day <- readRDS(here("data", "strikes_df.RDS")) |> ungroup()

unusal_dates_type_day <- tar_read("unusual_dates_df") |> 
  select(date,
         unusual_day_type = measure) |> 
  ungroup()

gp_activity_icb_sameday_status_day <- readRDS(here("data", "gp_appts_df.RDS")) |> ungroup()


# 2 some checks ----

# correct number of providers in each relevant df
provider_sample_df |> distinct(procode) |> nrow()
ecds_events |> distinct(procode) |> nrow()
ed_type1_activity_prov_arrmode_hour_day |> distinct(procode) |> nrow()
ed_occ_prov_hour_day |> distinct(procode) |> nrow()
apc_occ_prov_pod_day |> distinct(procode) |> nrow()
op_activity_prov_day |> distinct(procode) |> nrow()
dids_activity_prov_pod_day |> distinct(procode) |> nrow()

# all icb_sub_locs in ecds_events_df appear in comm, gp and ed activity dfs
ecds_events |> distinct(sub_icb) |> nrow()

ed_activity_prov_icb_depttype_arrmode_hour_day |> 
  distinct(sub_icb) |> 
  inner_join(ecds_events |> 
               distinct(sub_icb),
             join_by(sub_icb)) |> 
  nrow()


comm_activity_icb_day |> 
  distinct(icbSubLoc) |> 
  inner_join(ecds_events |> 
               distinct(sub_icb),
             join_by(icbSubLoc == sub_icb)) |> 
  nrow()


gp_activity_icb_sameday_status_day |> 
  distinct(icb_sub_loc) |> 
  inner_join(ecds_events |> 
               distinct(sub_icb),
             join_by(icb_sub_loc == sub_icb)) |> 
  nrow()

# trends by prov

ecds_events |> 
  mutate(arr_date = floor_date(dttm_arr, "days")) |> 
  group_by(procode, arr_date) |> 
  summarise(n = n()) |> 
  ggplot() +
  geom_point(aes(x = arr_date, y = n)) +
  facet_wrap(vars(procode))
# may need to drop RJZ - low numbers after Sept 2023
# looks like issues with sex, and EC_AttendanceCategory variables

ed_type1_activity_prov_arrmode_hour_day |> 
  mutate(date = make_date(year = year, month = month, day = day)) |> 
  group_by(procode, date) |> 
  summarise(n = sum(n_att)) |> 
  ggplot() +
  geom_point(aes(x = date, y = n)) +
  facet_wrap(vars(procode))


apc_occ_prov_pod_day |> 
  mutate(date = floor_date(census_dttm, "days")) |> 
  group_by(procode, date, Admission_Method_min) |> 
  summarise(n_occ = mean(ip_occ)) |> 
  group_by(procode, date) |> 
  summarise(n_occ = sum(n_occ)) |> 
  ggplot() +
  geom_point(aes(x = date, y = n_occ)) +
  facet_wrap(vars(procode))
# occ rates look a little odd in the first few months for RYR and RVJ
# could perhaps start the study period in Aug 2022 so that the study period is 
# 24 months

op_activity_prov_day |> 
  group_by(appointment_date, procode) |> 
  summarise(op_atts = sum(op_atts)) |> 
  ggplot() +
  geom_point(aes(x = appointment_date, y = op_atts)) +
  facet_wrap(vars(procode))

dids_activity_prov_pod_day |> 
  group_by(procode, date) |> 
  summarise(img = sum(img)) |> 
  ggplot() +
  geom_point(aes(x = date, y = img)) +
  facet_wrap(vars(procode))
# some big gaps in time series for RJZ - may need to drop this provider
# smaller gaps in  time series for a few other providers
# have checked the raw data and there are date gsps here too, so 
# this is not an artefact of the extraction process / queries.


        
# trends by ICB
ed_type3_activity_icb_hour_day |> 
  inner_join(ecds_events |> distinct(sub_icb),
             join_by(sub_icb == sub_icb)) |>
  mutate(date = make_date(year = year, month = month, day = day)) |> 
  mutate(month = floor_date(date, "months")) |> 
  filter(month >= "2022-04-01") |> 
  filter(month <= "2024-07-01") |> 
  group_by(sub_icb, month) |> 
  summarise(n = sum(n_att)) |> 
  ggplot() +
  geom_point(aes(x = month, y = n)) +
  facet_wrap(vars(sub_icb))
# quite variable between icbs,  some big steps

comm_activity_icb_day |> 
  inner_join(ecds_events |> distinct(sub_icb),
             join_by(icbSubLoc == sub_icb)) |> 
  mutate(month = floor_date(as_date(date), "months")) |> 
  filter(!is.na(icbSubLoc)) |> 
  group_by(icbSubLoc, month) |> 
  summarise(n = sum(comm_contacts)) |> 
  ggplot() +
  geom_point(aes(x = month, y = n)) +
  facet_wrap(vars(icbSubLoc))
# looks a bit chaopopy and variable
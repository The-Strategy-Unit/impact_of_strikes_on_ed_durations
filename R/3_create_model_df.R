# This script assembles from the data frame 
# for the regression models from a 
# number of NCDR / UDAL extracts and other data files.

# note: checks indicate that 
# a. RJZ should be removed from the provider sample due to several, 
#   quite particular, data quality issues
# b. the study period should start on 1st Aug 2022, due to some covariate issues
#     with some providers in the first few months of 2022/23
# see check_dfs_for_model_dataset.R for details


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

# 1 load base files ----

provider_sample_df <- 
  readRDS(here("data", "model_provider_sample_df.RDS")) |> 
  ungroup() |> 
  filter(procode != "RJZ")

ecds_events <- 
  readRDS(here("data", "ecds_events_full_df.RDS")) |> 
  ungroup() 

ed_type1_activity_prov_arrmode_day <- 
  readRDS(here("data", "ed_type1_act_df.RDS")) |> 
  ungroup() |> 
  filter(procode != "RJZ")

ed_type3_activity_icb_day <- 
  readRDS(here("data", "ed_type3_act_df.RDS")) |> 
  ungroup() 

ed_type1_4hr_distortion_prov_day <- 
  readRDS(here("data", "ed_type1_4hr_distortion_df.RDS")) |> 
  ungroup() 

ed_occ_prov_hour_day <- 
  readRDS(here("data", "ed_occ_df.RDS")) |> 
  ungroup() |> 
  filter(procode != "RJZ")

apc_occ_prov_pod_day <- 
  readRDS(here("data", "apc_occ_df.RDS")) |> 
  ungroup() |> 
  filter(procode != "RJZ")

op_activity_prov_day <- 
  readRDS(here("data", "op_act_df.RDS")) |> 
  ungroup() |> 
  filter(procode != "RJZ")

comm_activity_icb_day <- 
  readRDS(here("data", "comm_act_df.RDS")) |> 
  ungroup()

dids_activity_prov_pod_day <- 
  readRDS(here("data", "dids_act_df.RDS")) |> 
  inner_join(provider_sample_df, 
             join_by(procode)) |> 
  ungroup() |> 
  filter(procode != "RJZ")


strikes_staff_hour_day <- 
  readRDS(here("data", "strikes_df.RDS")) |> ungroup()

unusual_dates_type_day <- 
  readRDS(here("data", "unusual_dates_df.RDS")) |> 
  dplyr::select(date,
                unusual_day_type = measure) |> 
  ungroup()

gp_activity_icb_sameday_status_day <- 
  readRDS(here("data", "gp_appts_df.RDS")) |> 
  ungroup()

lkp_invst_type <- readRDS(here("data", "lkp_invst_type.RDS"))

imaging_codes <- lkp_invst_type |> 
  filter(invst_group == 'Imaging') |> 
  dplyr::select(Der_EC_Investigation) |> 
  pull() |> 
  paste(collapse = "|")

haem_codes <- lkp_invst_type |> 
  filter(invst_group == 'Haematology') |> 
  dplyr::select(Der_EC_Investigation) |> 
  pull() |> 
  paste(collapse = "|")

biochem_codes <- lkp_invst_type |> 
  filter(invst_group == 'Biochemistry') |> 
  dplyr::select(Der_EC_Investigation) |> 
  pull() |> 
  paste(collapse = "|")

other_test_codes <- lkp_invst_type |> 
  filter(invst_group == 'Other') |> 
  dplyr::select(Der_EC_Investigation) |> 
  pull() |> 
  paste(collapse = "|")




# 2 assemble model df ----

## 2.0 subset of selected providers ----


model_df_stage0 <- 
  ecds_events |> 
  inner_join(provider_sample_df |> 
               dplyr::select(procode),
             join_by(procode))

## 2.1 add some variables to ecds_events for linking ----

model_df_stage1 <- 
  model_df_stage0 |> 
  mutate(arr_date = floor_date(dttm_arr, "days")) |> 
  filter(arr_date >= as_date('2022-08-01'),
         arr_date <= as_date('2024-07-31')) |> 
  rowwise() |> 
  mutate(mid_arr_dep_dttm = mean(c(dttm_arr, dttm_depart))) |> 
  ungroup() |> 
  mutate(mid_arr_dep_dttm_dow = wday(mid_arr_dep_dttm, week_start = 1),
         mid_arr_dep_dttm_month = month(mid_arr_dep_dttm),
         mid_arr_dep_dttm_year = year(mid_arr_dep_dttm),
         mid_arr_dep_dttm_rd_hr = hour(round_date(mid_arr_dep_dttm, "hour")),
         mid_arr_dep_dttm_rd_half_hr = hour(round_date(mid_arr_dep_dttm - minutes(30), "hour")),
         mid_arr_dep_dttm_rd_day = round_date(mid_arr_dep_dttm, "day"),
         is_mtw = ifelse(mid_arr_dep_dttm_dow <= 3, 1, 0)) |> 
  mutate(n_img_tests = str_count(invst_all, imaging_codes),
         n_haem_tests = str_count(invst_all, haem_codes),
         n_biochem_tests = str_count(invst_all, biochem_codes),
         n_other_tests = str_count(invst_all, other_test_codes)) |> 
  mutate(n_trtmnts = str_count(treat_all, ",") + 1 - str_count(treat_all, "183964008")) |> 
  # assume of na value for invst_all or treat_all then this implies no activity
  mutate(n_img_tests = if_else(is.na(n_img_tests), 0, n_img_tests),
         n_haem_tests = if_else(is.na(n_haem_tests), 0, n_haem_tests),
         n_biochem_tests = if_else(is.na(n_biochem_tests), 0, n_biochem_tests),
         n_other_tests = if_else(is.na(n_other_tests), 0, n_other_tests),
         n_trtmnts = if_else(is.na(n_trtmnts), 0, n_trtmnts)) |> 
  mutate(is_adm = if_else(disdest_grp == "admitted", 1, 0)) |> 
  mutate(is_winter = if_else(month(mid_arr_dep_dttm_rd_day) %in% c(12, 1, 2, 3), 1, 0),
         is_weekend = if_else(wday(mid_arr_dep_dttm_rd_day, week_start = 1) %in% c(6, 7), 1, 0),
         is_night = ifelse(mid_arr_dep_dttm_rd_hr %in% c(20, 21, 22, 23, 0, 1, 2, 3, 4, 5, 6, 7), 1, 0)) |> 
  mutate(sex = as.factor(sex),
         arr_mode = as.factor(arr_mode),
         acuity_desc = as.factor(acuity_desc),
         chief_comp_grp = as.factor(chief_comp_grp),
         imd_dec = as.factor(imd_dec),
         procode = as.factor(procode),
         sub_icb = as.factor(sub_icb),
         mid_arr_dep_dttm_rd_hr_fct = as.factor(mid_arr_dep_dttm_rd_hr),
         mid_arr_dep_dttm_year_fct = as.factor(mid_arr_dep_dttm_year),
         mid_arr_dep_dttm_month_fct = as.factor(mid_arr_dep_dttm_month),
         mid_arr_dep_dttm_dow_fct = as.factor(mid_arr_dep_dttm_dow)) 
# |> 
#   filter(mid_arr_dep_dttm >= as_datetime('2022-08-01 00:00:00'),
#          mid_arr_dep_dttm <= as_datetime('2024-07-31 23:59:59')) 

gc()

## 2.2 join to strikes_df ----
model_df_stage2 <- 
  model_df_stage1 |> 
  # nurse strikes
  left_join(strikes_staff_hour_day |> 
              filter(staff_group == "nurses") |> 
              dplyr::select(-staff_group) |> 
              dplyr::select(date, hour, 
                     nurses_strike_intensity = strike_intensity , 
                     nurses_strike_sequence = strike_sequence),
            join_by(mid_arr_dep_dttm_rd_day == date,
                    mid_arr_dep_dttm_rd_hr == hour)) |> 
    mutate(nurses_strike = if_else(is.na(nurses_strike_sequence), 0, 1),
           nurses_strike_intensity = if_else(is.na(nurses_strike_intensity), 0, nurses_strike_intensity),
           nurses_strike_sequence = if_else(is.na(nurses_strike_sequence), 0, nurses_strike_sequence)) |> 
  # paramedic strikes
  left_join(strikes_staff_hour_day |> 
              filter(staff_group == "paramedics") |> 
              dplyr::select(-staff_group) |> 
              dplyr::select(date, hour, 
                     paramedics_strike_intensity = strike_intensity , 
                     paramedics_strike_sequence = strike_sequence),
            join_by(mid_arr_dep_dttm_rd_day == date,
                    mid_arr_dep_dttm_rd_hr == hour)) |> 
  mutate(paramedics_strike = if_else(is.na(paramedics_strike_sequence), 0, 1),
         paramedics_strike_intensity = if_else(is.na(paramedics_strike_intensity), 0, paramedics_strike_intensity),
         paramedics_strike_sequence = if_else(is.na(paramedics_strike_sequence), 0, paramedics_strike_sequence)) |>
  # jr_docs strikes
  left_join(strikes_staff_hour_day |> 
              filter(staff_group == "jr_docs") |> 
              dplyr::select(-staff_group) |> 
              dplyr::select(date, hour, 
                     jr_docs_strike_intensity = strike_intensity , 
                     jr_docs_strike_sequence = strike_sequence),
            join_by(mid_arr_dep_dttm_rd_day == date,
                    mid_arr_dep_dttm_rd_hr == hour)) |> 
  mutate(jr_docs_strike = if_else(is.na(jr_docs_strike_sequence), 0, 1),
         jr_docs_strike_intensity = if_else(is.na(jr_docs_strike_intensity), 0, jr_docs_strike_intensity),
         jr_docs_strike_sequence = if_else(is.na(jr_docs_strike_sequence), 0, jr_docs_strike_sequence)) |>
  # consultants strikes
  left_join(strikes_staff_hour_day |> 
              filter(staff_group == "consultants") |> 
              dplyr::select(-staff_group) |> 
              dplyr::select(date, hour, 
                     consultants_strike_intensity = strike_intensity , 
                     consultants_strike_sequence = strike_sequence),
            join_by(mid_arr_dep_dttm_rd_day == date,
                    mid_arr_dep_dttm_rd_hr == hour)) |> 
  mutate(consultants_strike = if_else(is.na(consultants_strike_sequence), 0, 1),
         consultants_strike_intensity = if_else(is.na(consultants_strike_intensity), 0, consultants_strike_intensity),
         consultants_strike_sequence = if_else(is.na(consultants_strike_sequence), 0, consultants_strike_sequence)) |> 
  # add google trends index
  left_join(strikes_staff_hour_day |> 
              group_by(date) |> 
              summarise(google_trends_index = mean(google_trends_index)),
            join_by(mid_arr_dep_dttm_rd_day == date)) |> 
  mutate(google_trends_index = if_else(is.na(google_trends_index), 0, google_trends_index)) %>% 
  # create vars to identify when both jr docs and consultants on strike, or one or the other but not both
  mutate(jr_docs_only_strike = if_else(jr_docs_strike == 1 & consultants_strike == 0, 1, 0),
         consultants_only_strike = if_else(consultants_strike == 1 & jr_docs_strike == 0, 1, 0),
         jr_docs_and_cons_strike = if_else(jr_docs_strike == 1 & consultants_strike == 1, 1, 0))
   
gc()


## 2.3 add prov ed activity ----

model_df_stage3 <- 
  model_df_stage2 |>             
  left_join(ed_type1_activity_prov_arrmode_day |> 
              filter(arr_mode == "amb") |> 
              mutate(date = make_date(year = year, month = month, day = day),
                     dow = wday(date, week_start = 1)) |> 
              group_by(procode, dow) |> 
              mutate(prov_day_ed_type1_amb_act_sc = if_else(is.na(n_att) | n_att == 0, 1, n_att / mean(n_att, na.rm = TRUE)), 
                     prov_day_ed_type1_amb_act_z = if_else(is.na(n_att) | n_att == 0, 0, (n_att - mean(n_att, na.rm = TRUE)) / sd(n_att, na.rm = TRUE))) |> 
              ungroup() |> 
              dplyr::select(procode, date, prov_day_ed_type1_amb_act = n_att, prov_day_ed_type1_amb_act_sc,, prov_day_ed_type1_amb_act_z),
            join_by(procode, arr_date == date)) |> 
  left_join(ed_type1_activity_prov_arrmode_day |> 
              filter(arr_mode == "walk_in") |> 
              mutate(date = make_date(year = year, month = month, day = day),
                     dow = wday(date, week_start = 1)) |> 
              group_by(procode, dow) |> 
              mutate(prov_day_ed_type1_walkin_act_sc = if_else(is.na(n_att) | n_att == 0, 1, n_att / mean(n_att, na.rm = TRUE)),
                     prov_day_ed_type1_walkin_act_z = if_else(is.na(n_att) | n_att == 0, 0, (n_att - mean(n_att, na.rm = TRUE)) / sd(n_att, na.rm = TRUE))) |> 
              ungroup() |> 
              dplyr::select(procode, date, prov_day_ed_type1_walkin_act = n_att, prov_day_ed_type1_walkin_act_sc, , prov_day_ed_type1_walkin_act_z),
            join_by(procode, arr_date == date)) 

gc()

## 2.4 add prov ip occupancy ----

model_df_stage4 <- 
  model_df_stage3 |>   
  # elec occ
  left_join(apc_occ_prov_pod_day |> 
              filter(Admission_Method_min %in% c("ElecDC", "ElecOrd", "ElecRegDN")) |> 
              mutate(date = floor_date(census_dttm, "days"),
                     dow = wday(date, week_start = 1)) |> 
              group_by(procode, date, dow, hour) |> 
              summarise(ip_occ = sum(ip_occ)) |> 
              ungroup() |> 
              group_by(procode, dow, hour) |> 
              mutate(mean_ip_occ = mean(ip_occ, na.rm = TRUE),
                     sd_ip_occ = sd(ip_occ, na.rm = TRUE)) |> 
              mutate(prov_hr_elec_occ_sc = if_else(mean_ip_occ == 0, 1, ip_occ / mean_ip_occ),
                     prov_hr_elec_occ_z = if_else(mean_ip_occ == 0, 0, (ip_occ - mean_ip_occ) / sd_ip_occ)) |> 
              ungroup() |> 
              dplyr::select(procode, date, hour, prov_hr_elec_occ = ip_occ, prov_hr_elec_occ_sc, prov_hr_elec_occ_z),
            join_by(procode, 
                    mid_arr_dep_dttm_rd_day == date,
                    mid_arr_dep_dttm_rd_half_hr == hour)) |> 
  # non_elec occ
  left_join(apc_occ_prov_pod_day |> 
              filter(Admission_Method_min  %in% c("Emer", "Mat", "Other")) |> 
              mutate(date = floor_date(census_dttm, "days"),
                     dow = wday(date, week_start = 1)) |> 
              group_by(procode, date, dow, hour) |> 
              summarise(ip_occ = sum(ip_occ)) |> 
              ungroup() |> 
              group_by(procode, dow, hour) |> 
              mutate(mean_ip_occ = mean(ip_occ, na.rm = TRUE)) |> 
              mutate(sd_ip_occ = sd(ip_occ, na.rm = TRUE)) |> 
              mutate(prov_hr_emer_occ_sc = if_else(mean_ip_occ == 0, 1, ip_occ / mean_ip_occ),
                     prov_hr_emer_occ_z = if_else(mean_ip_occ == 0, 0, (ip_occ - mean_ip_occ) / sd_ip_occ)) |> 
              ungroup() |> 
              dplyr::select(procode, date, hour, prov_hr_emer_occ = ip_occ, prov_hr_emer_occ_sc, prov_hr_emer_occ_z),
            join_by(procode, 
                    mid_arr_dep_dttm_rd_day == date,
                    mid_arr_dep_dttm_rd_half_hr == hour))

gc()

## 2.5 add prov op activity ----

model_df_stage5 <- 
  model_df_stage4 |> 
  # op atts
  left_join(op_activity_prov_day |> 
              filter(op_act_type == "OP_att") |> 
              mutate(dow = wday(appointment_date, week_start = 1)) |> 
              group_by(procode, dow) |> 
              mutate(mean_op_atts = mean(op_atts, na.rm = TRUE),
                     sd_op_atts = sd(op_atts, na.rm = TRUE)) |> 
              ungroup() |> 
              mutate(prov_day_op_att_sc = op_atts / mean_op_atts,
                     prov_day_op_att_z = (op_atts - mean_op_atts) / sd_op_atts) |> 
              dplyr::select(procode, date = appointment_date, prov_day_op_att = op_atts, prov_day_op_att_sc, prov_day_op_att_z, mean_op_atts, sd_op_atts),
            join_by(procode,
                    mid_arr_dep_dttm_rd_day == date)) |> 
  mutate(prov_day_op_att_sc = case_when(is.na(prov_day_op_att_sc) & is.na(mean_op_atts) ~ 1,
                                        is.na(prov_day_op_att_sc) & !is.na(mean_op_atts) ~ 0,
                                        TRUE ~ prov_day_op_att_sc),
         prov_day_op_att_z = case_when(is.na(prov_day_op_att_z) & is.na(mean_op_atts) ~ 0,
                                        is.na(prov_day_op_att_z) & !is.na(mean_op_atts) ~ -mean_op_atts / sd_op_atts,
                                        TRUE ~ prov_day_op_att_z),
         prov_day_op_att = if_else(is.na(prov_day_op_att), 0, prov_day_op_att)) |> 
  dplyr::select(-mean_op_atts, -sd_op_atts) |> 
  # op procs
  left_join(op_activity_prov_day |> 
              filter(op_act_type == "OP_proc") |> 
              mutate(dow = wday(appointment_date, week_start = 1)) |> 
              group_by(procode, dow) |> 
              mutate(mean_op_atts = mean(op_atts, na.rm = TRUE),
                     sd_op_atts = sd(op_atts, na.rm = TRUE)) |> 
              ungroup() |> 
              mutate(prov_day_op_proc_sc = op_atts / mean_op_atts,
                     prov_day_op_proc_z = (op_atts - mean_op_atts) / sd_op_atts) |> 
              dplyr::select(procode, date = appointment_date, prov_day_op_proc = op_atts, prov_day_op_proc_sc, prov_day_op_proc_z, mean_op_atts, sd_op_atts),
            join_by(procode,
                    mid_arr_dep_dttm_rd_day == date)) |> 
    mutate(prov_day_op_proc_sc = case_when(is.na(prov_day_op_proc_sc) & is.na(mean_op_atts) ~ 1,
                                          is.na(prov_day_op_proc_sc) & !is.na(mean_op_atts) ~ 0,
                                          TRUE ~ prov_day_op_proc_sc),
           prov_day_op_proc_z = case_when(is.na(prov_day_op_proc_sc) & is.na(mean_op_atts) ~ 0,
                                         is.na(prov_day_op_proc_sc) & !is.na(mean_op_atts) ~ -mean_op_atts / sd_op_atts,
                                         TRUE ~ prov_day_op_proc_z),
           prov_day_op_proc = if_else(is.na(prov_day_op_proc), 0, prov_day_op_proc)) |> 
    dplyr::select(-mean_op_atts, -sd_op_atts)

gc()
  
## 2.6 add prov diag imaging activity ----

# note that some providers don;t submit data for short periods (c 2 weeks)
# need to distinguish this from dates with no scans
# see right joins below

model_df_stage6 <- 
  model_df_stage5 |> 
  # ct scans
  left_join(dids_activity_prov_pod_day |> 
              filter(test_type == 'ct') |> 
              group_by(procode, date) |> 
              summarise(img = sum(img, na.rm = TRUE)) |> 
              ungroup() |> 
              right_join(data.frame(date = seq(as_date("2022-08-01"), as_date("2024-07-31"), by = "days")) |>
                           crossing(provider_sample_df |> distinct(procode) |> filter(procode != "RJZ")),
                         join_by(date, procode)) |> 
              group_by(procode) |> 
              arrange(procode, date) |> 
              mutate(img = if_else(is.na(img) &
                                     !is.na(lag(img, n = 1)) &
                                     !is.na(lead(img, n = 1)), 0, img)) |> 
              ungroup() |> 
              mutate(dow = wday(date, week_start = 1)) |> 
              group_by(procode, dow) |> 
              mutate(mean_img = mean(img, na.rm = TRUE),
                     sd_img = sd(img, na.rm = TRUE)) |> 
              ungroup() |> 
              mutate(prov_day_ct_scan_sc = img / mean_img,
                     prov_day_ct_scan_z = (img - mean_img / sd_img)) |> 
              dplyr::select(procode, date, prov_day_ct_scan = img, prov_day_ct_scan_sc, prov_day_ct_scan_z),
            join_by(procode,
                    mid_arr_dep_dttm_rd_day == date)) |> 
  # mri scans
  left_join(dids_activity_prov_pod_day |> 
              filter(test_type == 'mri') |> 
              group_by(procode, date) |> 
              summarise(img = sum(img, na.rm = TRUE)) |>
              ungroup() |> 
              right_join(data.frame(date = seq(as_date("2022-08-01"), as_date("2024-07-31"), by = "days")) |>
                           crossing(provider_sample_df |> distinct(procode) |> filter(procode != "RJZ")),
                         join_by(date, procode)) |> 
              group_by(procode) |> 
              arrange(procode, date) |> 
              mutate(img = if_else(is.na(img) &
                                     !is.na(lag(img, n = 1)) &
                                     !is.na(lead(img, n = 1)), 0, img)) |> 
              ungroup() |> 
              mutate(dow = wday(date, week_start = 1)) |> 
              group_by(procode, dow) |> 
              mutate(mean_img = mean(img, na.rm = TRUE),
                     sd_img = sd(img, na.rm = TRUE)) |> 
              ungroup() |> 
              mutate(prov_day_mri_scan_sc = img / mean_img,
                     prov_day_mri_scan_z = (img - mean_img / sd_img)) |> 
              dplyr::select(procode, date, prov_day_mri_scan = img, prov_day_mri_scan_sc, prov_day_mri_scan_z),
            join_by(procode,
                    mid_arr_dep_dttm_rd_day == date)) |> 
  # ultrasound scans
  left_join(dids_activity_prov_pod_day |> 
              filter(test_type == 'us') |> 
              group_by(procode, date) |> 
              summarise(img = sum(img, na.rm = TRUE)) |> 
              ungroup() |> 
              right_join(data.frame(date = seq(as_date("2022-08-01"), as_date("2024-07-31"), by = "days")) |>
                           crossing(provider_sample_df |> distinct(procode) |> filter(procode != "RJZ")),
                         join_by(date, procode)) |> 
              group_by(procode) |> 
              arrange(procode, date) |> 
              mutate(img = if_else(is.na(img) &
                                     !is.na(lag(img, n = 1)) &
                                     !is.na(lead(img, n = 1)), 0, img)) |> 
              ungroup() |> 
              mutate(dow = wday(date, week_start = 1)) |> 
              group_by(procode, dow) |> 
              mutate(mean_img = mean(img, na.rm = TRUE),
                     sd_img = sd(img, na.rm = TRUE)) |>
              ungroup() |> 
              mutate(prov_day_us_scan_sc = img / mean_img,
                     prov_day_us_scan_z = (img - mean_img / sd_img)) |> 
              dplyr::select(procode, date, prov_day_us_scan = img, prov_day_us_scan_sc, prov_day_us_scan_z),
            join_by(procode,
                    mid_arr_dep_dttm_rd_day == date)) |> 
  # xray scans
  left_join(dids_activity_prov_pod_day |> 
              filter(test_type == 'xray') |> 
              group_by(procode, date) |> 
              summarise(img = sum(img, na.rm = TRUE)) |> 
              ungroup() |> 
              right_join(data.frame(date = seq(as_date("2022-08-01"), as_date("2024-07-31"), by = "days")) |>
                           crossing(provider_sample_df |> distinct(procode) |> filter(procode != "RJZ")),
                         join_by(date, procode)) |> 
              group_by(procode) |> 
              arrange(procode, date) |> 
              mutate(img = if_else(is.na(img) &
                                     !is.na(lag(img, n = 1)) &
                                     !is.na(lead(img, n = 1)), 0, img)) |> 
              ungroup() |> 
              mutate(dow = wday(date, week_start = 1)) |> 
              group_by(procode, dow) |> 
              mutate(mean_img = mean(img, na.rm = TRUE),
                     sd_img = sd(img, na.rm = TRUE)) |>
              ungroup() |> 
              mutate(prov_day_xray_scan_sc = img / mean_img,
                     prov_day_xray_scan_z = (img - mean_img / sd_img)) |> 
              dplyr::select(procode, date, prov_day_xray_scan = img, prov_day_xray_scan_sc, prov_day_xray_scan_z),
            join_by(procode,
                    mid_arr_dep_dttm_rd_day == date)) 

gc()

## 2.7 add type 3 ed activity ----

model_df_stage7 <- 
  model_df_stage6 |> 
  left_join(ed_type3_activity_icb_day |> 
              mutate(date = make_date(year = year, month = month, day = day)) |> 
              mutate(dow = wday(date, week_start = 1)) |> 
              group_by(sub_icb, dow) |> 
              mutate(mean_att = mean(n_att),
                     sd_att = sd(n_att)) |> 
              ungroup() |> 
              mutate(subicb_ed_type3_act_sc = n_att / mean_att,
                     subicb_ed_type3_act_z = (n_att - mean_att) / sd_att) |> 
              dplyr::select(sub_icb, date, subicb_ed_type3_act = n_att, subicb_ed_type3_act_sc, subicb_ed_type3_act_z, mean_att, sd_att),
            join_by(sub_icb,
                    mid_arr_dep_dttm_rd_day == date)) |> 
  mutate(subicb_ed_type3_act_sc = case_when(is.na(subicb_ed_type3_act_sc) & is.na(mean_att) ~ 1,
                                           is.na(subicb_ed_type3_act_sc) & !is.na(mean_att) ~ 0,
                                           TRUE ~ subicb_ed_type3_act_sc),
         subicb_ed_type3_act_z = case_when(is.na(subicb_ed_type3_act_z) & is.na(mean_att) ~ 0,
                                            is.na(subicb_ed_type3_act_z) & !is.na(mean_att) ~ -mean_att / sd_att,
                                            TRUE ~ subicb_ed_type3_act_z),
         subicb_ed_type3_act = if_else(is.na(subicb_ed_type3_act), 0, subicb_ed_type3_act)) |> 
  dplyr::select(-mean_att, -sd_att)

gc()

## 2.8 add community services activity ----

model_df_stage8 <- 
  model_df_stage7 |> 
  left_join(comm_activity_icb_day |> 
              filter(is_attendance == 1) |>
              mutate(date = as_date(date)) |> 
              mutate(dow = wday(date, week_start = 1)) |> 
              group_by(icbSubLoc, dow) |> 
              mutate(mean_comm_contacts = mean(comm_contacts),
                     sd_comm_contacts = sd(comm_contacts)) |> 
              ungroup() |> 
              mutate(subicb_comm_contacts_sc = comm_contacts / mean_comm_contacts,
                     subicb_comm_contacts_z = (comm_contacts - mean_comm_contacts) / sd_comm_contacts) |> 
              dplyr::select(sub_icb = icbSubLoc, date, subicb_comm_contacts = comm_contacts, subicb_comm_contacts_sc, subicb_comm_contacts_z, mean_comm_contacts, sd_comm_contacts),
            join_by(sub_icb,
                    mid_arr_dep_dttm_rd_day == date)) |> 
  mutate(subicb_comm_contacts_sc = case_when(is.na(subicb_comm_contacts_sc) & is.na(mean_comm_contacts) ~ 1,
                                            is.na(subicb_comm_contacts_sc) & !is.na(mean_comm_contacts) ~ 0,
                                            TRUE ~ subicb_comm_contacts_sc),
         subicb_comm_contacts_z = case_when(is.na(subicb_comm_contacts_z) & is.na(mean_comm_contacts) ~ 0,
                                             is.na(subicb_comm_contacts_z) & !is.na(mean_comm_contacts) ~ -mean_comm_contacts / sd_comm_contacts,
                                             TRUE ~ subicb_comm_contacts_z),
         subicb_comm_contacts = if_else(is.na(subicb_comm_contacts), 0, subicb_comm_contacts)) |> 
  dplyr::select(-mean_comm_contacts, -sd_comm_contacts)

gc()

## 2.9 add gp appt activity ----

# note that we're leaving in DNAs

model_df_stage9 <- 
  # same day appts
  model_df_stage8 |> 
  left_join(gp_activity_icb_sameday_status_day |> 
              filter(same_day == 1) |> 
              group_by(icb_sub_loc, date) |> 
              summarise(gp_appts = sum(gp_appts)) |> 
              ungroup() |> 
              mutate(dow = wday(date, week_start = 1)) |> 
              group_by(icb_sub_loc, dow) |> 
              mutate(mean_gp_appts = mean(gp_appts),
                     sd_gp_appts = sd(gp_appts)) |> 
              ungroup() |> 
              mutate(subicb_gp_appts_sameday_sc = gp_appts / mean_gp_appts,
                     subicb_gp_appts_sameday_z = (gp_appts - mean_gp_appts) / sd_gp_appts) |> 
              dplyr::select(sub_icb = icb_sub_loc, date, subicb_gp_appts_sameday = gp_appts, subicb_gp_appts_sameday_sc, subicb_gp_appts_sameday_z, mean_gp_appts, sd_gp_appts),
            join_by(sub_icb, 
                    mid_arr_dep_dttm_rd_day == date)) |> 
  mutate(subicb_gp_appts_sameday_sc = case_when(is.na(subicb_gp_appts_sameday_sc) & is.na(mean_gp_appts) ~ 1,
                                             is.na(subicb_gp_appts_sameday_sc) & !is.na(mean_gp_appts) ~ 0,
                                             TRUE ~ subicb_gp_appts_sameday_sc),
         subicb_gp_appts_sameday_z = case_when(is.na(subicb_gp_appts_sameday_z) & is.na(mean_gp_appts) ~ 0,
                                                is.na(subicb_gp_appts_sameday_z) & !is.na(mean_gp_appts) ~ -mean_gp_appts / sd_gp_appts,
                                                TRUE ~ subicb_gp_appts_sameday_z),
         subicb_gp_appts_sameday = if_else(is.na(subicb_gp_appts_sameday), 0, subicb_gp_appts_sameday)) |> 
  dplyr::select(-mean_gp_appts, -sd_gp_appts) |> 
  # later day appts
  left_join(gp_activity_icb_sameday_status_day |> 
              filter(same_day == 0) |> 
              group_by(icb_sub_loc, date) |> 
              summarise(gp_appts = sum(gp_appts)) |> 
              ungroup() |> 
              mutate(dow = wday(date, week_start = 1)) |> 
              group_by(icb_sub_loc, dow) |> 
              mutate(mean_gp_appts = mean(gp_appts),
                     sd_gp_appts = sd(gp_appts)) |> 
              ungroup() |> 
              mutate(subicb_gp_appts_prebook_sc = gp_appts / mean_gp_appts,
                     subicb_gp_appts_prebook_z = (gp_appts - mean_gp_appts) / sd_gp_appts) |> 
              dplyr::select(sub_icb = icb_sub_loc, date, subicb_gp_appts_prebook = gp_appts, subicb_gp_appts_prebook_sc, subicb_gp_appts_prebook_z, mean_gp_appts, sd_gp_appts),
            join_by(sub_icb, 
                    mid_arr_dep_dttm_rd_day == date)) |> 
  mutate(subicb_gp_appts_prebook_sc = case_when(is.na(subicb_gp_appts_prebook_sc) & is.na(mean_gp_appts) ~ 1,
                                                is.na(subicb_gp_appts_prebook_sc) & !is.na(mean_gp_appts) ~ 0,
                                                TRUE ~ subicb_gp_appts_prebook_sc),
         subicb_gp_appts_prebook_z = case_when(is.na(subicb_gp_appts_prebook_z) & is.na(mean_gp_appts) ~ 0,
                                                is.na(subicb_gp_appts_prebook_z) & !is.na(mean_gp_appts) ~ -mean_gp_appts / sd_gp_appts,
                                                TRUE ~ subicb_gp_appts_prebook_z),
         subicb_gp_appts_prebook = if_else(is.na(subicb_gp_appts_prebook), 0, subicb_gp_appts_prebook)) |> 
  dplyr::select(-mean_gp_appts, -sd_gp_appts) 

gc()

## 2.10 add ed hr distortion measure ----

model_df_stage10 <- 
  model_df_stage9 |> 
  left_join(ed_type1_4hr_distortion_prov_day |> 
              mutate(date = make_date(year = year, month = month, day = day)) |> 
              dplyr::select(procode, date, distort_4hr),
            join_by(procode,
                     mid_arr_dep_dttm_rd_day == date))
                       


## 2.11 add bank holidays ----
model_df_stage11 <- 
  model_df_stage10 |> 
  left_join(unusual_dates_type_day |> 
              filter(unusual_day_type == "bank holiday") |> 
              mutate(bank_holiday = 1) |> 
              dplyr::select(date, bank_holiday),
            join_by(mid_arr_dep_dttm_rd_day == date)) |> 
  mutate(bank_holiday = if_else(is.na(bank_holiday), 0, bank_holiday))

 gc()

# 3 check missing ----
model_df_stage11 |> 
  summarise_all(~ sum(is.na(.))) |> 
  t()

 

model_df_stage11 |> nrow()
# 3946696 # all_record

model_df_stage11 |> 
  drop_na(c(duration_ed)) |> nrow() 
# 3944680 # excl na duration

model_df_stage11 |> 
  drop_na(c(duration_ed,
            acuity)) |> nrow() 
# 3873396 # excl na acuity

model_df_stage11 |> 
  drop_na(c(duration_ed,
            acuity,
            chief_comp_grp)) |> nrow() 
# 3739378 # excl na chief compl

model_df_stage11 |> 
  drop_na(c(duration_ed,
            acuity,
            chief_comp_grp,
            invst_all)) |> nrow() 
# 3693486 # excl na inv

model_df_stage11 |> 
  drop_na(c(duration_ed,
            acuity,
            chief_comp_grp,
            invst_all,
            treat_all)) |> nrow() 
# 3592473 # acl na trt

model_df_stage11 |> 
  drop_na(c(duration_ed,
            acuity,
            chief_comp_grp,
            invst_all,
            treat_all,
            age)) |> nrow() 
 # 3592380 # exl age > 110

model_df_stage11 |> 
  drop_na(c(duration_ed,
            acuity,
            chief_comp_grp,
            invst_all,
            treat_all,
            age, 
            imd_dec)) |> nrow() 
# 3592174 # exl na imd

model_df_stage11 |> 
  drop_na(c(duration_ed,
            acuity,
            chief_comp_grp,
            invst_all,
            treat_all,
            age, 
            imd_dec,
            prov_hr_elec_occ)) |> nrow() 
# 3588681 # exl na ip occ


model_df_stage11 |> 
  drop_na(c(duration_ed,
            acuity,
            chief_comp_grp,
            invst_all,
            treat_all,
            age, 
            imd_dec,
            prov_hr_elec_occ,
            prov_day_ct_scan_sc,
            prov_day_mri_scan_sc,
            prov_day_us_scan_sc,
            prov_day_xray_scan_sc)) |> nrow() 
# 3405635 # exl na scan

model_df_stage11 |> 
  drop_na(c(duration_ed,
            acuity,
            chief_comp_grp,
            invst_all,
            treat_all,
            age, 
            imd_dec,
            prov_hr_elec_occ,
            prov_day_ct_scan_sc,
            prov_day_mri_scan_sc,
            prov_day_us_scan_sc,
            prov_day_xray_scan_sc,
            distort_4hr)) |> nrow() 
# 3405310 # exl na distort 4hr






model_df_stage12 <-
  model_df_stage11 |> 
  drop_na(c(-att_source_desc,
            -inj_flag,
            -ethnic_grp,
            -chief_comp,
            -chief_comp_desc,

            -prov_day_op_proc_z,

            -prov_hr_elec_occ,
            -prov_hr_elec_occ_z,
            -prov_hr_emer_occ,
            -prov_hr_emer_occ_z,

            -prov_day_ct_scan,
            -prov_day_mri_scan,
            -prov_day_us_scan,
            -prov_day_xray_scan,

            -prov_day_ct_scan_z,
            -prov_day_mri_scan_z,
            -prov_day_us_scan_z,
            -prov_day_xray_scan_z,

            -subicb_gp_appts_prebook_z
            ))


model_df_stage12 |> nrow()
# 3369232


model_df_stage12 |> 
  summarise_all(~ sum(is.na(.))) |> 
  t()

# how many records removed

nrow(model_df_stage11) - nrow(model_df_stage12)
# 577464 records

(nrow(model_df_stage11) - nrow(model_df_stage12)) / nrow(model_df_stage11)        
# 14.6% of records


# 4 save file ----
saveRDS(model_df_stage0, here("data", "model_df_stage0.RDS")) 
saveRDS(model_df_stage11, here("data", "model_df_stage11.RDS")) 

 
saveRDS(model_df_stage12, here("data", "model_df.RDS")) 
 
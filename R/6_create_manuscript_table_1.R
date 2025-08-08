#README
# this file crates table(s) 1 for the manuscript.

# 0 set-up ----

library("tidyverse")
library("here")


# 1 read in dataset ----

study_df <- readRDS(here("data", "model_df_stage11.RDS"))


# 2 create table 1a ---- 
# patient and attendance characteristics by exposure


t1a_total_n <- 
  study_df |> 
  filter(!is.na(duration_ed)) |> 
  mutate(exposure = case_when(jr_docs_only_strike == 1 ~ "resident (jr) Drs",
                              consultants_only_strike == 1 ~ "consultants",
                              jr_docs_and_cons_strike == 1 ~ "resident Drs & consultants",
                              paramedics_strike == 1 ~ "paramedics",
                              nurses_strike == 1 ~ "nurses",
                              TRUE ~ "none")) |> 
  group_by(exposure) |> 
  summarise(n = n()) |> 
  pivot_wider(names_from = "exposure", 
              values_from = 2) |> 
  mutate(label = "total n") |> 
  dplyr::select(label, none, 
         `resident (jr) Drs`,
         `consultants`,
         `resident Drs & consultants`,
         `paramedics`,
         `nurses`)
  
t1a_total_p <- 
  study_df |> 
  filter(!is.na(duration_ed)) |> 
  mutate(exposure = case_when(jr_docs_only_strike == 1 ~ "resident (jr) Drs",
                              consultants_only_strike == 1 ~ "consultants",
                              jr_docs_and_cons_strike == 1 ~ "resident Drs & consultants",
                              paramedics_strike == 1 ~ "paramedics",
                              nurses_strike == 1 ~ "nurses",
                              TRUE ~ "none")) |> 
  group_by(exposure) |> 
  summarise(n = n()) |> 
  mutate(p = n / sum(n)) |> 
  dplyr::select(-n) |> 
  pivot_wider(names_from = "exposure", 
              values_from = 2) |> 
  mutate(label = "total p") |> 
  dplyr::select(label, none, 
         `resident (jr) Drs`,
         `consultants`,
         `resident Drs & consultants`,
         `paramedics`,
         `nurses`)



t1a_mean_dur <-
  study_df |> 
  filter(!is.na(duration_ed)) |> 
  mutate(exposure = case_when(jr_docs_only_strike == 1 ~ "resident (jr) Drs",
                              consultants_only_strike == 1 ~ "consultants",
                              jr_docs_and_cons_strike == 1 ~ "resident Drs & consultants",
                              paramedics_strike == 1 ~ "paramedics",
                              nurses_strike == 1 ~ "nurses",
                              TRUE ~ "none")) |> 
  group_by(exposure) |> 
  summarise(mean_dur = mean(duration_ed)) |> 
  pivot_wider(names_from = "exposure", 
              values_from = 2) |> 
  mutate(label = "mean_dur") |> 
  dplyr::select(label, none, 
         `resident (jr) Drs`,
         `consultants`,
         `resident Drs & consultants`,
         `paramedics`,
         `nurses`)

t1a_female_p <-
  study_df |> 
  filter(!is.na(duration_ed)) |> 
  mutate(exposure = case_when(jr_docs_only_strike == 1 ~ "resident (jr) Drs",
                              consultants_only_strike == 1 ~ "consultants",
                              jr_docs_and_cons_strike == 1 ~ "resident Drs & consultants",
                              paramedics_strike == 1 ~ "paramedics",
                              nurses_strike == 1 ~ "nurses",
                              TRUE ~ "none")) |> 
  group_by(exposure, sex) |> 
  summarise(n = n()) |> 
  group_by(exposure) |> 
  mutate(p = n / sum(n)) |> 
  filter(sex == "f") |> 
  dplyr::select(-n, -sex) |> 
  pivot_wider(names_from = "exposure", 
              values_from = 2) |> 
  mutate(label = "p_female") |> 
  dplyr::select(label, none, 
         `resident (jr) Drs`,
         `consultants`,
         `resident Drs & consultants`,
         `paramedics`,
         `nurses`)


t1a_mean_age <-
  study_df |> 
  filter(!is.na(duration_ed)) |> 
  mutate(exposure = case_when(jr_docs_only_strike == 1 ~ "resident (jr) Drs",
                              consultants_only_strike == 1 ~ "consultants",
                              jr_docs_and_cons_strike == 1 ~ "resident Drs & consultants",
                              paramedics_strike == 1 ~ "paramedics",
                              nurses_strike == 1 ~ "nurses",
                              TRUE ~ "none")) |> 
  group_by(exposure) |> 
  summarise(mean_dur = mean(age, na.rm = TRUE)) |> 
  pivot_wider(names_from = "exposure", 
              values_from = 2) |> 
  mutate(label = "mean_age") |> 
  dplyr::select(label, none, 
         `resident (jr) Drs`,
         `consultants`,
         `resident Drs & consultants`,
         `paramedics`,
         `nurses`)


t1a_most_deprived_p <-
  study_df |> 
  filter(!is.na(duration_ed)) |> 
  mutate(exposure = case_when(jr_docs_only_strike == 1 ~ "resident (jr) Drs",
                              consultants_only_strike == 1 ~ "consultants",
                              jr_docs_and_cons_strike == 1 ~ "resident Drs & consultants",
                              paramedics_strike == 1 ~ "paramedics",
                              nurses_strike == 1 ~ "nurses",
                              TRUE ~ "none")) |> 
  group_by(exposure, imd_dec) |> 
  summarise(n = n()) |> 
  group_by(exposure) |> 
  mutate(p = n / sum(n)) |> 
  filter(imd_dec == "1") |> 
  dplyr::select(-n, -imd_dec) |> 
  pivot_wider(names_from = "exposure", 
              values_from = 2) |> 
  mutate(label = "p_most_dep") |> 
  dplyr::select(label, none, 
         `resident (jr) Drs`,
         `consultants`,
         `resident Drs & consultants`,
         `paramedics`,
         `nurses`)


t1a_chief_comp_p <-
  study_df |> 
  filter(!is.na(duration_ed)) |> 
  mutate(exposure = case_when(jr_docs_only_strike == 1 ~ "resident (jr) Drs",
                              consultants_only_strike == 1 ~ "consultants",
                              jr_docs_and_cons_strike == 1 ~ "resident Drs & consultants",
                              paramedics_strike == 1 ~ "paramedics",
                              nurses_strike == 1 ~ "nurses",
                              TRUE ~ "none")) |> 
  mutate(chief_comp_grp = case_when(is.na(chief_comp_grp) ~ "other / not stated",
                                    chief_comp_grp == "Not applicable to child terms" ~ "other / not stated",
                                    chief_comp_grp == "General / minor / admin" ~ "General",
                                    chief_comp_grp == "ObGyn" ~ "Obstetrics / gynaecology",
                                    TRUE ~ chief_comp_grp)) |> 
  group_by(exposure, chief_comp_grp) |> 
  summarise(n = n()) |> 
  group_by(exposure) |> 
  mutate(p = n / sum(n)) |> 
  dplyr::select(-n) |> 
  pivot_wider(names_from = "exposure", 
              values_from = 3) |> 
  rename(label = chief_comp_grp) |> 
  dplyr::select(label, none, 
         `resident (jr) Drs`,
         `consultants`,
         `resident Drs & consultants`,
         `paramedics`,
         `nurses`)


t1a_amb_conv_p <-
  study_df |> 
  filter(!is.na(duration_ed)) |> 
  mutate(exposure = case_when(jr_docs_only_strike == 1 ~ "resident (jr) Drs",
                              consultants_only_strike == 1 ~ "consultants",
                              jr_docs_and_cons_strike == 1 ~ "resident Drs & consultants",
                              paramedics_strike == 1 ~ "paramedics",
                              nurses_strike == 1 ~ "nurses",
                              TRUE ~ "none")) |> 
  group_by(exposure, arr_mode) |> 
  summarise(n = n()) |> 
  group_by(exposure) |> 
  mutate(p = n / sum(n)) |> 
  filter(arr_mode == "amb") |> 
  dplyr::select(-n, -arr_mode) |> 
  pivot_wider(names_from = "exposure", 
              values_from = 2) |> 
  mutate(label = "p_amb") |> 
  dplyr::select(label, none, 
         `resident (jr) Drs`,
         `consultants`,
         `resident Drs & consultants`,
         `paramedics`,
         `nurses`)


t1a_urgent_p <-
  study_df |> 
  filter(!is.na(duration_ed)) |> 
  mutate(exposure = case_when(jr_docs_only_strike == 1 ~ "resident (jr) Drs",
                              consultants_only_strike == 1 ~ "consultants",
                              jr_docs_and_cons_strike == 1 ~ "resident Drs & consultants",
                              paramedics_strike == 1 ~ "paramedics",
                              nurses_strike == 1 ~ "nurses",
                              TRUE ~ "none")) |> 
  mutate(urgent = if_else(acuity %in% c(1, 2, 3), 1, 0)) |> 
  group_by(exposure, urgent) |> 
  summarise(n = n()) |> 
  group_by(exposure) |> 
  mutate(p = n / sum(n)) |> 
  filter(urgent == 1) |> 
  dplyr::select(-n, -urgent) |> 
  pivot_wider(names_from = "exposure", 
              values_from = 2) |> 
  mutate(label = "p_urgent") |> 
  dplyr::select(label, none, 
         `resident (jr) Drs`,
         `consultants`,
         `resident Drs & consultants`,
         `paramedics`,
         `nurses`)

t1a_ooh_p <-
  study_df |> 
  filter(!is.na(duration_ed)) |> 
  mutate(exposure = case_when(jr_docs_only_strike == 1 ~ "resident (jr) Drs",
                              consultants_only_strike == 1 ~ "consultants",
                              jr_docs_and_cons_strike == 1 ~ "resident Drs & consultants",
                              paramedics_strike == 1 ~ "paramedics",
                              nurses_strike == 1 ~ "nurses",
                              TRUE ~ "none")) |> 
  mutate(ooh = if_else(mid_arr_dep_dttm_rd_hr < 8 | mid_arr_dep_dttm_rd_hr > 20 , 1, 0)) |> 
  group_by(exposure, ooh) |> 
  summarise(n = n()) |> 
  group_by(exposure) |> 
  mutate(p = n / sum(n)) |> 
  filter(ooh == 1) |> 
  dplyr::select(-n, -ooh) |> 
  pivot_wider(names_from = "exposure", 
              values_from = 2) |> 
  mutate(label = "p_ooh") |> 
  dplyr::select(label, none, 
         `resident (jr) Drs`,
         `consultants`,
         `resident Drs & consultants`,
         `paramedics`,
         `nurses`)

t1a_weekend_p <-
  study_df |> 
  filter(!is.na(duration_ed)) |> 
  mutate(exposure = case_when(jr_docs_only_strike == 1 ~ "resident (jr) Drs",
                              consultants_only_strike == 1 ~ "consultants",
                              jr_docs_and_cons_strike == 1 ~ "resident Drs & consultants",
                              paramedics_strike == 1 ~ "paramedics",
                              nurses_strike == 1 ~ "nurses",
                              TRUE ~ "none")) |> 
  group_by(exposure, is_weekend) |> 
  summarise(n = n()) |> 
  group_by(exposure) |> 
  mutate(p = n / sum(n)) |> 
  filter(is_weekend == 1) |> 
  dplyr::select(-n, -is_weekend) |> 
  right_join(data.frame(exposure = c("none", 
                                     "resident (jr) Drs", "consultants", 
                                     "resident Drs & consultants",
                                     "paramedics", "nurses")),
                        join_by(exposure)) |> 
  mutate(p = if_else(is.na(p), 0, p)) |> 
  pivot_wider(names_from = "exposure", 
              values_from = 2,
              values_fill = 0) |> 
  mutate(label = "p_weekend") |> 
  dplyr::select(label, none, 
         `resident (jr) Drs`,
         `consultants`,
         `resident Drs & consultants`,
         `paramedics`,
         `nurses`)

t1a_winter_p <-
  study_df |> 
  filter(!is.na(duration_ed)) |> 
  mutate(exposure = case_when(jr_docs_only_strike == 1 ~ "resident (jr) Drs",
                              consultants_only_strike == 1 ~ "consultants",
                              jr_docs_and_cons_strike == 1 ~ "resident Drs & consultants",
                              paramedics_strike == 1 ~ "paramedics",
                              nurses_strike == 1 ~ "nurses",
                              TRUE ~ "none")) |> 
  group_by(exposure, is_winter) |> 
  summarise(n = n()) |> 
  group_by(exposure) |> 
  mutate(p = n / sum(n)) |> 
  filter(is_winter == 1) |> 
  dplyr::select(-n, -is_winter) |> 
  right_join(data.frame(exposure = c("none", 
                                     "resident (jr) Drs", "consultants", 
                                     "resident Drs & consultants",
                                     "paramedics", "nurses")),
             join_by(exposure)) |> 
  mutate(p = if_else(is.na(p), 0, p)) |> 
  pivot_wider(names_from = "exposure", 
              values_from = 2,
              values_fill = 0) |> 
  mutate(label = "p_winter") |> 
  dplyr::select(label, none, 
         `resident (jr) Drs`,
         `consultants`,
         `resident Drs & consultants`,
         `paramedics`,
         `nurses`)


t1a_bank_hol_p <-
  study_df |> 
  filter(!is.na(duration_ed)) |> 
  mutate(exposure = case_when(jr_docs_only_strike == 1 ~ "resident (jr) Drs",
                              consultants_only_strike == 1 ~ "consultants",
                              jr_docs_and_cons_strike == 1 ~ "resident Drs & consultants",
                              paramedics_strike == 1 ~ "paramedics",
                              nurses_strike == 1 ~ "nurses",
                              TRUE ~ "none")) |> 
  group_by(exposure, bank_holiday) |> 
  summarise(n = n()) |> 
  group_by(exposure) |> 
  mutate(p = n / sum(n)) |> 
  filter(bank_holiday == 1) |> 
  dplyr::select(-n, -bank_holiday) |> 
  right_join(data.frame(exposure = c("none", 
                                     "resident (jr) Drs", "consultants", 
                                     "resident Drs & consultants",
                                     "paramedics", "nurses")),
             join_by(exposure)) |> 
  mutate(p = if_else(is.na(p), 0, p)) |> 
  pivot_wider(names_from = "exposure", 
              values_from = 2,
              values_fill = 0) |> 
  mutate(label = "p_bank_holiday") |> 
  dplyr::select(label, none, 
         `resident (jr) Drs`,
         `consultants`,
         `resident Drs & consultants`,
         `paramedics`,
         `nurses`)


manuscript_table_1a <-
  bind_rows(t1a_total_n,
            t1a_total_p,
            t1a_mean_dur,
            t1a_female_p,
            t1a_mean_age,
            t1a_most_deprived_p,
            t1a_amb_conv_p,
            t1a_urgent_p,
            t1a_chief_comp_p,
            t1a_ooh_p,
            t1a_weekend_p,
            t1a_winter_p,
            t1a_bank_hol_p)


# 2 create table 1b ---- 
# mediators by exposure

manuscript_table_1b <-
  study_df |> 
  filter(!is.na(duration_ed)) |> 
  mutate(exposure = case_when(jr_docs_only_strike == 1 ~ "resident (jr) Drs",
                              consultants_only_strike == 1 ~ "consultants",
                              jr_docs_and_cons_strike == 1 ~ "resident Drs & consultants",
                              paramedics_strike == 1 ~ "paramedics",
                              nurses_strike == 1 ~ "nurses",
                              TRUE ~ "none")) |> 
  group_by(exposure) |> 
  summarise(mean_prov_day_ed_type1_amb_act_sc = mean(prov_day_ed_type1_amb_act_sc, na.rm = TRUE),
            mean_prov_day_ed_type1_walkin_act_sc = mean(prov_day_ed_type1_walkin_act_sc, na.rm = TRUE),
            mean_subicb_ed_type3_act_sc = mean(subicb_ed_type3_act_sc, na.rm = TRUE),
            mean_subicb_comm_contacts_sc = mean(subicb_comm_contacts_sc, na.rm = TRUE),
            mean_subicb_gp_appts_prebook_sc = mean(subicb_gp_appts_prebook_sc, na.rm = TRUE),
            mean_subicb_gp_appts_sameday_sc= mean(subicb_gp_appts_sameday_sc, na.rm = TRUE),
            #mean_google_trends_index = mean(google_trends_index, na.rm = TRUE),
            
            mean_prov_hr_emer_occ_sc= mean(prov_hr_emer_occ_sc, na.rm = TRUE),
            mean_prov_hr_elec_occ_sc= mean(prov_hr_elec_occ_sc, na.rm = TRUE),
            mean_prov_day_op_att_sc= mean(prov_day_op_att_sc, na.rm = TRUE),
            mean_prov_day_op_proc_sc= mean(prov_day_op_proc_sc, na.rm = TRUE),
            mean_prov_day_ct_scan_sc= mean(prov_day_ct_scan_sc, na.rm = TRUE),
            mean_prov_day_mri_scan_sc= mean(prov_day_mri_scan_sc, na.rm = TRUE),
            mean_prov_day_us_scan_sc= mean(prov_day_us_scan_sc, na.rm = TRUE),
            mean_prov_day_xray_scan_sc= mean(prov_day_xray_scan_sc, na.rm = TRUE),,
            
            mean_n_img_tests= mean(n_img_tests, na.rm = TRUE),
            mean_n_haem_tests= mean(n_haem_tests, na.rm = TRUE),
            mean_n_biochem_tests= mean(n_biochem_tests, na.rm = TRUE),
            mean_n_other_tests= mean(n_other_tests, na.rm = TRUE),
            mean_n_trtmnts= mean(n_trtmnts, na.rm = TRUE),
            p_is_adm= mean(is_adm, na.rm = TRUE),
            mean_distort_4hr = mean(distort_4hr, na.rm = TRUE)) |> 
  pivot_longer(cols = 2:22,
               names_to = "metric",
               values_to = "value") |> 
  pivot_wider(names_from = "exposure", 
              values_from = "value") |> 
  dplyr::select(metric, none, 
         `resident (jr) Drs`,
         `consultants`,
         `resident Drs & consultants`,
         `paramedics`,
         `nurses`)

# 3 save files ----
write.csv(manuscript_table_1a, here("manuscript", "manuscript_table_1a.csv"))
write.csv(manuscript_table_1b, here("manuscript", "manuscript_table_1b.csv"))

# README

## need to control for hour better


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

library("lme4")
library("mgcv")
library("broom.mixed")
library("mediation")
library("mma")
library("survival")

# https://bookdown.org/mike/data_analysis/mediation.html

# 1 load data ----
model_df <- readRDS(here("data", "model_df.RDS")) 

gc()

# 2 models ----

# create df sample
model_df_cut <- slice_sample(model_df, n = 10000) |> 
  dplyr::select(-fyear, -dttm_arr, -dttm_depart,
         -att_source_desc, -disdest_grp, -acuity,
         -chief_comp, -chief_comp_desc,
         -inj_flag, -invst_all, -treat_all, 
         -ethnic_grp,
         -p_complete_ed, -p_complete_apc,
         -arr_date,
         -mid_arr_dep_dttm,
         #-mid_arr_dep_dttm_rd_hr,
         -is_night,
         -mid_arr_dep_dttm_rd_half_hr,
         -mid_arr_dep_dttm_rd_day) |> 
  drop_na() |> 
  mutate(arr_mode = as.factor(arr_mode),
         acuity_desc = as.factor(acuity_desc),
         chief_comp_grp = as.factor(chief_comp_grp),
         imd_dec = as.factor(imd_dec),
         procode = as.factor(procode),
         sub_icb = as.factor(sub_icb),
         mid_arr_dep_dttm_rd_hr = as.factor(mid_arr_dep_dttm_rd_hr)) |> ) |> 
  mutate(duration_ed = duration_ed + 1)

saveRDS(model_df_cut, here("data", "model_df_cut.RDS"))

# model_df_cut <- readRDS(here("data", "model_df_cut.RDS"))

rm(model_df)

gc()


set.seed(127854)

# 2.1 linear mixed effects
out_mod_lin_me <- lmer(duration_ed ~ age + sex + imd_dec + 
                     arr_mode + acuity_desc + chief_comp_grp +
                     is_winter + is_weekend + mid_arr_dep_dttm_rd_hr + bank_holiday +
                     nurses_strike + paramedics_strike + 
                     jr_docs_strike + consultants_strike + 
                     jr_docs_strike:consultants_strike +
                       #prov_hr_elec_occ_sc +
                       prov_hr_emer_occ_sc +
                       #subicb_gp_appts_prebook_sc +
                     (1 | procode),
                     #family = gaussian(link = "log"),
                   data = model_df_cut)

AIC(out_mod_lin_me)

med_mod_lin_me <- lmer(prov_hr_emer_occ_sc ~ age + sex + imd_dec + 
                         arr_mode + acuity_desc + chief_comp_grp +
                         is_winter + is_weekend + mid_arr_dep_dttm_rd_hr + bank_holiday +
                         nurses_strike + paramedics_strike + 
                         jr_docs_strike + consultants_strike + 
                         jr_docs_strike:consultants_strike +
                         (1 | procode),
                       data = model_df_cut)

med_out_lin_me <- mediate(model.m = med_mod_lin_me, 
                          model.y = out_mod_lin_me, 
                          treat = "jr_docs_strike", 
                          mediator = "prov_hr_emer_occ_sc", 
                          sims = 10)

summary(med_out_lin_me)
# summary(out_mod_lin_me)
# summary(med_mod_lin_me)



# summary(mod_lin_me)
# 
# broom.mixed::tidy(mod_lin_me) |> 
#   print(n = 50)

AIC(mod_lin_me)

## need to work out how to get p.values out of these glmer models
## perhaps try gam and compare results


model_df_cut_mma <- model_df_cut |> 
  dplyr::select(-nurses_strike_intensity, -nurses_strike_sequence,
                -paramedics_strike_intensity, -paramedics_strike_sequence,
                -jr_docs_strike_intensity, -jr_docs_strike_sequence,
                -consultants_strike_intensity, -consultants_strike_sequence,
                -prov_day_ed_type1_amb_act, -prov_day_ed_type1_walkin_act, 
                -prov_hr_elec_occ, -prov_hr_emer_occ,
                -prov_day_op_att, -prov_day_op_proc,
                -prov_day_ct_scan, -prov_day_mri_scan, 
                -prov_day_us_scan, -prov_day_xray_scan,
                -subicb_ed_type3_act, -subicb_comm_contacts,
                -subicb_gp_appts_sameday, -subicb_gp_appts_prebook)

mma_test <- mma(x = model_df_cut_mma[,c(2:9, 16:18, 38, 19, 20, 22, 23:37)],          
                y = model_df_cut_mma[,1],           
                pred = model_df_cut_mma[,21],         
                #mediator = c(5, 6),                  
                contmed = c(16:30),
                predref = 0,
                n2 = 10)

mma_test




## TRY SURVREG


out_mod_surv <- survreg(Surv(duration_ed) ~ age + sex + imd_dec + 
                          arr_mode + acuity_desc + chief_comp_grp +
                          is_winter + is_weekend + mid_arr_dep_dttm_rd_hr + bank_holiday +
                          nurses_strike + paramedics_strike + 
                          jr_docs_strike + consultants_strike + 
                          jr_docs_strike:consultants_strike +
                          #prov_hr_emer_occ_sc,
                          subicb_comm_contacts_sc,
                        cluster = procode,
                        data = model_df_cut)


med_mod_lin_me <- lmer(subicb_comm_contacts_sc ~ age + sex + imd_dec + 
                         arr_mode + acuity_desc + chief_comp_grp +
                         is_winter + is_weekend + mid_arr_dep_dttm_rd_hr + bank_holiday +
                         nurses_strike + paramedics_strike + 
                         jr_docs_strike + consultants_strike + 
                         jr_docs_strike:consultants_strike +
                         (1 | procode),
                       data = model_df_cut)

med_out_surv <- mediate(med_mod_lin_me, out_mod_surv, 
                          treat = "jr_docs_strike", 
                          mediator = "subicb_comm_contacts_sc", 
                          robustSE = TRUE, sims = 100)


summary(med_out_surv)

# README

# This script measure mediation effects for jr doctor and consultant strikes
# on duration in ED

# model forms established in 
#  initial_mediation_analysis.R
#  test_z_score.RDS


# 16 continuous mediators
# 5  count mediators
# 1  binary mediator


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
library("tidyverse")

library("lme4")
library("broom.mixed")
library("survival")
library("mediation")


# 1 load data ----
model_df <- readRDS(here("data", "model_df_cut_50pc.RDS")) %>% 
  drop_na() |>    
  dplyr::select(
    -fyear, -dttm_arr, -dttm_depart,
                -att_source_desc, -disdest_grp, -acuity,
                -chief_comp, -chief_comp_desc,
                -inj_flag, -invst_all, -treat_all,
                -ethnic_grp,
                -arr_date,
                -prov_day_ed_type1_amb_act,
                -prov_day_ed_type1_amb_act_z,
                -prov_day_ed_type1_walkin_act,
                -prov_day_ed_type1_walkin_act_z,
                -prov_hr_elec_occ,
                -prov_hr_elec_occ_z,
                -prov_hr_emer_occ,
                -prov_hr_emer_occ_z,
                -prov_day_op_att,
                -prov_day_op_att_z,
                -prov_day_op_proc,
                -prov_day_op_proc_z,
                -prov_day_ct_scan,
                -prov_day_ct_scan_z,
                -prov_day_mri_scan,
                -prov_day_mri_scan_z,
                -prov_day_us_scan,
                -prov_day_us_scan_z,
                -prov_day_xray_scan,
                -prov_day_xray_scan_z,
                -subicb_ed_type3_act,
                -subicb_ed_type3_act_z,
                -subicb_comm_contacts,
                -subicb_comm_contacts_z,
                -subicb_gp_appts_sameday,
                -subicb_gp_appts_sameday_z,
                -subicb_gp_appts_prebook,
                -subicb_gp_appts_prebook_z,
                -nurses_strike_intensity,
                -nurses_strike_sequence,
                -paramedics_strike_intensity,
                -paramedics_strike_sequence,
                -jr_docs_strike_intensity,
                -jr_docs_strike_sequence,
                -consultants_strike_intensity,
                -consultants_strike_sequence,
                -jr_docs_strike,
                -consultants_strike) |> 
  mutate(duration_ed = duration_ed + 1)


gc()


# 2 set parameters ----

predictors <- c("nurses_strike", 
                "paramedics_strike",
                "jr_docs_only_strike",
                "consultants_only_strike",
                "jr_docs_and_cons_strike")


count_mediators <- data.frame(mediator = c("n_img_tests", 
                                     "n_haem_tests", 
                                     "n_biochem_tests", 
                                     "n_other_tests",
                                     "n_trtmnts"),
                        type = "count",
                        hypothesis = "hyp3 - change in ED operations")

binary_mediators <- data.frame(mediator = "is_adm",
                       type = "binary",
                       hypothesis = "hyp3 - change in ED operations")
  
continuous_mediators <- data.frame(mediator = c("prov_day_ed_type1_amb_act_sc",
                                    "prov_day_ed_type1_walkin_act_sc",
                                    "subicb_ed_type3_act_sc",
                                    "subicb_comm_contacts_sc",
                                    "subicb_gp_appts_sameday_sc",
                                    "subicb_gp_appts_prebook_sc",
                                    "google_trends_index",
                                    
                                    "prov_hr_elec_occ_sc",
                                    "prov_hr_emer_occ_sc",
                                    "prov_day_op_att_sc",
                                    "prov_day_op_proc_sc",
                                    "prov_day_ct_scan_sc",
                                    "prov_day_mri_scan_sc",
                                    "prov_day_us_scan_sc",
                                    "prov_day_xray_scan_sc",
                                    
                                    "distort_4hr"
                                    ),
                       type = "continuous",
                       hypothesis = c(rep("hyp1 - reduced demand", 7),
                                      rep("hyp2 - resources freed up", 8),
                                      "hyp3 - change in ED operations"))



mediators <- bind_rows(count_mediators, binary_mediators, continuous_mediators)

n_sims <- 200

# 3 build models ----

# 3a unmediated model ----

unmediated_model <- survreg(Surv(duration_ed) ~ age + sex + imd_dec + 
                                   arr_mode + acuity_desc + chief_comp_grp +
                                   is_winter + is_weekend + mid_arr_dep_dttm_rd_hr_fct + 
                                   bank_holiday + 
                                   mid_arr_dep_dttm_year_fct +
                                   nurses_strike + paramedics_strike + 
                                   jr_docs_only_strike + consultants_only_strike + 
                                   jr_docs_and_cons_strike,
                                 cluster = procode, 
                                 dist = "loglogistic",
                                 data = model_df)

saveRDS(unmediated_model, here("data", "unmediated_model.RDS"))
rm(unmediated_model)
gc()

# 3b mediation models

set.seed(437867)

# 3bi continuous mediators ----

continuous_mediation_models_df <- 
  mediators %>% 
  filter(type == "continuous") %>% 
  as_tibble() %>% 
  mutate(outcome_model = purrr::map(mediator, function(x){
    
    outcome_model_formula = formula(gsub("\n", "", 
                                       paste("Surv(duration_ed) ~ age + sex + imd_dec + 
                                                         arr_mode + acuity_desc + chief_comp_grp +
                                                         is_winter + is_weekend + mid_arr_dep_dttm_rd_hr_fct + 
                                                         bank_holiday + 
                                                         mid_arr_dep_dttm_year_fct +
                                                         nurses_strike + paramedics_strike + 
                                                         jr_docs_only_strike + consultants_only_strike + 
                                                         jr_docs_and_cons_strike + ",
                                             x,
                                             sep ="")))
    
    survreg(outcome_model_formula, 
            cluster = procode,
            dist = "loglogistic",
            data = model_df)
  
    
  }, .progress = TRUE)) %>% 
  
  mutate(mediator_model = purrr::map(mediator, function(x){
    
    mediator_model_formula = formula(gsub("\n", "", 
                                         paste(x, " ~ age + sex + imd_dec + 
                                                         arr_mode + acuity_desc + chief_comp_grp +
                                                         is_winter + is_weekend + mid_arr_dep_dttm_rd_hr_fct + 
                                                         bank_holiday + 
                                                         mid_arr_dep_dttm_year_fct +
                                                         nurses_strike + paramedics_strike + 
                                                         jr_docs_only_strike + consultants_only_strike + 
                                                         jr_docs_and_cons_strike + 
                                                         (1 | procode)",
                                               sep ="")))
    
    lmer(mediator_model_formula, 
         data = model_df)
    
  }, .progress = TRUE))

saveRDS(continuous_mediation_models_df, here("data", "continuous_mediation_models_df.RDS"))
rm(continuous_mediation_models_df)
gc()

# 3bii count mediators ----

set.seed(437868)

count_mediation_models_df <- 
  mediators %>% 
  filter(type == "count") %>% 
  as_tibble() %>% 
  
  mutate(outcome_model = purrr::map(mediator, function(x){
    
    outcome_model_formula = formula(gsub("\n", "", 
                                         paste("Surv(duration_ed) ~ age + sex + imd_dec + 
                                                         arr_mode + acuity_desc + chief_comp_grp +
                                                         is_winter + is_weekend + mid_arr_dep_dttm_rd_hr_fct + 
                                                         bank_holiday + 
                                                         mid_arr_dep_dttm_year_fct +
                                                         nurses_strike + paramedics_strike + 
                                                         jr_docs_only_strike + consultants_only_strike + 
                                                         jr_docs_and_cons_strike + ",
                                               x,
                                               sep ="")))
    
    survreg(outcome_model_formula, 
            cluster = procode,
            dist = "loglogistic",
            data = model_df)
    
    
  }, .progress = TRUE)) %>% 
  
  mutate(mediator_model = purrr::map(mediator, function(x){
    
    mediator_model_formula = formula(gsub("\n", "", 
                                          paste(x, " ~ age + sex + imd_dec + 
                                                         arr_mode + acuity_desc + chief_comp_grp +
                                                         is_winter + is_weekend + mid_arr_dep_dttm_rd_hr_fct + 
                                                         bank_holiday + 
                                                         mid_arr_dep_dttm_year_fct +
                                                         nurses_strike + paramedics_strike + 
                                                         jr_docs_only_strike + consultants_only_strike + 
                                                         jr_docs_and_cons_strike + 
                                                         (1 | procode)",
                                                sep ="")))
    
    glmer(mediator_model_formula, 
         data = model_df,
         family = poisson(link = "log"),
         nAGQ = 1)
    
  }, .progress = TRUE))

saveRDS(count_mediation_models_df, here("data", "count_mediation_models_df.RDS"))
rm(count_mediation_models_df)
gc()


# 3biii binary mediators ----

set.seed(437869)

binary_mediation_models_df <- 
  mediators %>% 
  filter(type == "binary") %>% 
  as_tibble() %>% 
  
  mutate(outcome_model = purrr::map(mediator, function(x){
    
    outcome_model_formula = formula(gsub("\n", "", 
                                         paste("Surv(duration_ed) ~ age + sex + imd_dec + 
                                                         arr_mode + acuity_desc + chief_comp_grp +
                                                         is_winter + is_weekend + mid_arr_dep_dttm_rd_hr_fct + 
                                                         bank_holiday + 
                                                         mid_arr_dep_dttm_year_fct +
                                                         nurses_strike + paramedics_strike + 
                                                         jr_docs_only_strike + consultants_only_strike + 
                                                         jr_docs_and_cons_strike + ",
                                               x,
                                               sep ="")))
    
    survreg(outcome_model_formula, 
            cluster = procode,
            dist = "loglogistic",
            data = model_df)
    
    
  }, .progress = TRUE)) %>% 
  
  mutate(mediator_model = purrr::map(mediator, function(x){
    
    mediator_model_formula = formula(gsub("\n", "", 
                                          paste(x, " ~ age + sex + imd_dec + 
                                                         arr_mode + acuity_desc + chief_comp_grp +
                                                         is_winter + is_weekend + mid_arr_dep_dttm_rd_hr_fct + 
                                                         bank_holiday + 
                                                         mid_arr_dep_dttm_year_fct +
                                                         nurses_strike + paramedics_strike + 
                                                         jr_docs_only_strike + consultants_only_strike + 
                                                         jr_docs_and_cons_strike + 
                                                         (1 | procode)",
                                                sep ="")))
    
    glmer(mediator_model_formula, 
          data = model_df,
          family = binomial(link = "logit"),
          nAGQ = 1)
    
  }, .progress = TRUE))

saveRDS(binary_mediation_models_df, here("data", "binary_mediation_models_df.RDS"))
rm(binary_mediation_models_df)
gc()

#rm(model_df)
gc()



# 4 mediation analysis ----


# 4a continuous mediators ----

for (i in (1:nrow(continuous_mediators))) {
  
  print(paste0(i, " of ", nrow(continuous_mediators)))
  
  continuous_mediation_output_df <- 
    readRDS(here("data", "continuous_mediation_models_df.RDS")) %>% 
    slice(i) %>%  
    cross_join(predictors %>% 
                 as_tibble() %>% 
                 rename(predictor = value)) %>% 
    mutate(mediate_output = purrr::pmap(list(mediator_model, outcome_model, predictor, mediator), 
                                        function(x, y, p, m){
                                          
                                          mediate(model.m = x, 
                                                  model.y = y, 
                                                  treat = p, 
                                                  mediator = m, 
                                                  sims = n_sims)
                                          
                                        }, .progress = TRUE)) 
  
  gc()
  
  saveRDS(continuous_mediation_output_df, here("data", paste0("continuous_mediation_output_df_", i, ".RDS")))
  
  rm(continuous_mediation_output_df)
  
  gc()
  
}

# 4b count mediators ----

for (i in (1:nrow(count_mediators))) {
  
  print(paste0(i, " of ", nrow(count_mediators)))
  
  count_mediation_output_df <- 
    readRDS(here("data", "count_mediation_models_df.RDS")) %>% 
    slice(i) %>% 
    cross_join(predictors %>% 
                 as_tibble() %>% 
                 rename(predictor = value)) %>% 
    mutate(mediate_output = purrr::pmap(list(mediator_model, outcome_model, predictor, mediator), 
                                        function(x, y, p, m){
                                          
                                          mediate(model.m = x, 
                                                  model.y = y, 
                                                  treat = p, 
                                                  mediator = m, 
                                                  sims = n_sims)
                                          
                                        }, .progress = TRUE)) 
  
  gc()
  
  saveRDS(count_mediation_output_df, here("data", paste0("count_mediation_output_df_", i, ".RDS")))
  
  rm(count_mediation_output_df)
  
  gc()
  
}



# 4c binary mediators ----


gc()

binary_mediation_output_df <- 
  readRDS(here("data", "binary_mediation_models_df.RDS")) |> 
  cross_join(predictors %>% 
               as_tibble() %>% 
               rename(predictor = value)) %>% 
  mutate(mediate_output = purrr::pmap(list(mediator_model, outcome_model, predictor, mediator), 
                                      function(x, y, p, m){
                                        
                                        mediate(model.m = x, 
                                                model.y = y, 
                                                treat = p, 
                                                mediator = m, 
                                                sims = n_sims)
                                        
                                      }, .progress = TRUE)) 

gc()

saveRDS(binary_mediation_output_df, here("data", "binary_mediation_output_df.RDS"))

rm(binary_mediation_output_df)

gc()


# 5 fully mediated model ----

# all potential mediators
fully_mediated_outcome_model <- survreg(Surv(duration_ed) ~ age + sex + imd_dec + 
                              arr_mode + acuity_desc + chief_comp_grp +
                              is_winter + is_weekend + mid_arr_dep_dttm_rd_hr_fct + 
                              bank_holiday + 
                              mid_arr_dep_dttm_year_fct +
                              nurses_strike + paramedics_strike + 
                              jr_docs_only_strike + consultants_only_strike + 
                              jr_docs_and_cons_strike +
                              prov_day_ed_type1_amb_act_sc + prov_day_ed_type1_walkin_act_sc +
                              subicb_ed_type3_act_sc + subicb_comm_contacts_sc + 
                              subicb_gp_appts_sameday_sc + subicb_gp_appts_prebook_sc +
                              google_trends_index +
                              prov_hr_elec_occ_sc + prov_hr_emer_occ_sc +
                              prov_day_op_att_sc + prov_day_op_proc_sc +
                              prov_day_ct_scan_sc + prov_day_mri_scan_sc + prov_day_us_scan_sc + prov_day_xray_scan_sc +
                              n_img_tests + n_haem_tests + n_biochem_tests + n_other_tests + 
                              n_trtmnts + is_adm + distort_4hr,
                            cluster = procode, 
                            dist = "loglogistic",
                            data = model_df)


saveRDS(fully_mediated_outcome_model, here("data", "fully_mediated_outcome_model.df"))

rm(fully_mediated_outcome_model)

gc()

  
# 6 secondary analysis ----
# unmediated model with 4 and 12hr binary outcome variables

model_df_sec_analysis <- 
  model_df |> 
  mutate(dur_ed_4hr = if_else(duration_ed > 241, 1, 0),
         dur_ed_12hr = if_else(duration_ed > 721, 1, 0))
  


unmediated_model_4hr <- glmer(dur_ed_4hr ~ age + sex + imd_dec + 
                              arr_mode + acuity_desc + chief_comp_grp +
                              is_winter + is_weekend + mid_arr_dep_dttm_rd_hr_fct + 
                              bank_holiday + 
                              mid_arr_dep_dttm_year_fct +
                              nurses_strike + paramedics_strike + 
                              jr_docs_only_strike + consultants_only_strike + 
                              jr_docs_and_cons_strike +
                              (1 | procode),
                              nAGQ = 0,
                              family = binomial,
                              data = model_df_sec_analysis)

saveRDS(unmediated_model_4hr, here("data", "unmediated_model_4hr.RDS"))
rm(unmediated_model_4hr)
gc()

unmediated_model_12hr <- glmer(dur_ed_12hr ~ age + sex + imd_dec + 
                                arr_mode + acuity_desc + chief_comp_grp +
                                is_winter + is_weekend + mid_arr_dep_dttm_rd_hr_fct + 
                                bank_holiday + 
                                mid_arr_dep_dttm_year_fct +
                                nurses_strike + paramedics_strike + 
                                jr_docs_only_strike + consultants_only_strike + 
                                jr_docs_and_cons_strike +
                                (1 | procode),
                               nAGQ = 0,
                               family = binomial,
                               data = model_df_sec_analysis)
saveRDS(unmediated_model_12hr, here("data", "unmediated_model_12hr.RDS"))
rm(unmediated_model_12hr)
gc()
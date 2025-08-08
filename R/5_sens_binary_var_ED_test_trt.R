# README

# This script tests whether treating the 5 mediators that indicate the number
# or diagnostic tests and treatments that a patient receives in ED 
#   # imaging, blood, biochem and other, tests and #treatments
# as binary variables, influences the results

# the theory here is the perhaps some providers only record the first instance
# of a given test, while other record multiple tests
# so this approach would be more conservative, if less specific


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
  mutate(duration_ed = duration_ed + 1) |> 
  mutate(yn_img_tests = ifelse(n_img_tests >= 1, 1, 0),
         yn_haem_tests = ifelse(n_haem_tests >= 1, 1, 0),
         yn_biochem_tests = ifelse(n_biochem_tests >= 1, 1, 0),
         yn_other_tests = ifelse(n_other_tests >= 1, 1, 0),
         yn_trtmnts = ifelse(n_trtmnts >= 1, 1, 0))


gc()


# 2 set parameters ----

predictors <- c("nurses_strike", 
                "paramedics_strike",
                "jr_docs_only_strike",
                "consultants_only_strike",
                "jr_docs_and_cons_strike")




sens_binary_mediators <- data.frame(mediator = c("yn_img_tests", 
                                            "yn_haem_tests", 
                                            "yn_biochem_tests", 
                                            "yn_other_tests",
                                            "yn_trtmnts"),
                       type = "binary",
                       hypothesis = "hyp3 - change in ED operations")
  

n_sims <- 200

# 3 build mediation models ----

set.seed(1289667)

sens_binary_mediation_models_df <- 
  sens_binary_mediators %>% 
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

saveRDS(sens_binary_mediation_models_df, here("data", "sens_binary_mediation_models_df.RDS"))
rm(sens_binary_mediation_models_df)
gc()

#rm(model_df)
gc()



# 4 mediation analysis ----

sens_binary_mediation_output_df <- 
  readRDS(here("data", "sens_binary_mediation_models_df.RDS")) |> 
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

saveRDS(sens_binary_mediation_output_df, here("data", "sens_binary_mediation_output_df.RDS"))
# sens_binary_mediation_output_df <- readRDS(here("data", "sens_binary_mediation_output_df.RDS"))
gc()



# 5 extract values from mediation analysis ----

sens_binary_mediation_output_df_1 <- 
  sens_binary_mediation_output_df |> 
  filter(mediator == "yn_img_tests")

sens_binary_mediation_output_df_2 <- 
  sens_binary_mediation_output_df |> 
  filter(mediator == "yn_haem_tests")

sens_binary_mediation_output_df_3 <- 
  sens_binary_mediation_output_df |> 
  filter(mediator == "yn_biochem_tests")
  
sens_binary_mediation_output_df_4 <- 
    sens_binary_mediation_output_df |> 
  filter(mediator == "yn_other_tests")

sens_binary_mediation_output_df_5 <- 
  sens_binary_mediation_output_df |> 
  filter(mediator == "yn_trtmnts")

saveRDS(sens_binary_mediation_output_df_1, here("data", "sens_binary_mediation_output_df_1.RDS")) 
saveRDS(sens_binary_mediation_output_df_2, here("data", "sens_binary_mediation_output_df_2.RDS")) 
saveRDS(sens_binary_mediation_output_df_3, here("data", "sens_binary_mediation_output_df_3.RDS")) 
saveRDS(sens_binary_mediation_output_df_4, here("data", "sens_binary_mediation_output_df_4.RDS")) 
saveRDS(sens_binary_mediation_output_df_5, here("data", "sens_binary_mediation_output_df_5.RDS")) 

rm(sens_binary_mediation_output_df,
   sens_binary_mediation_output_df_1,
   sens_binary_mediation_output_df_2, 
   sens_binary_mediation_output_df_3, 
   sens_binary_mediation_output_df_4, 
   sens_binary_mediation_output_df_5)


sens_mediation_output_files <- c("sens_binary_mediation_output_df_1.RDS",
                            "sens_binary_mediation_output_df_2.RDS",
                            "sens_binary_mediation_output_df_3.RDS",
                            "sens_binary_mediation_output_df_4.RDS",
                            "sens_binary_mediation_output_df_5.RDS")


sens3_mediation_results_df <- data.frame(mediator = character(),
                                   type = character(),
                                   hypothesis = character(),
                                   predictor = character(),
                                   p_mediated = numeric(),
                                   p_mediated_pv = numeric(),
                                   p_mediated_lcl95 = numeric(),
                                   p_mediated_ucl95 = numeric(),
                                   p_mediated_exposed = numeric(),
                                   p_mediated_exposed_pv = numeric(),
                                   p_mediated_exposed_lcl95 = numeric(),
                                   p_mediated_exposed_ucl95 = numeric(),
                                   p_mediated_unexposed = numeric(),
                                   p_mediated_unexposed_pv = numeric(),
                                   p_mediated_unexposed_lcl95 = numeric(),
                                   p_mediated_unexposed_ucl95 = numeric(),
                                   pred_coef_med_model = numeric(),
                                   pred_coef_se_med_model = numeric(),
                                   med_coef_out_model = numeric(),
                                   med_coef_se_out_model = numeric(),
                                   med_coef_pv_out_model = numeric(),
                                   pred_coef_lcl95_med_model = numeric(),
                                   pred_coef_ucl95_med_model = numeric(),
                                   med_coef_lcl95_out_model = numeric(),
                                   med_coef_ucl95_out_model = numeric())


for (i in (sens_mediation_output_files)) {
  
  print(paste(Sys.time(), i, sep = " - "))
  
  mediation_output_df <- readRDS(here("data", i))
  
  print(paste(Sys.time(), "file in", sep = " - "))
  
  gc()
  
  sens3_mediation_results_df_i <-
    mediation_output_df %>%
    mutate(p_mediated = map_dbl(mediation_output_df$mediate_output, function(df) {df$n.avg}),
           p_mediated_pv = map_dbl(mediation_output_df$mediate_output, function(df) {df$n.avg.p}),
           p_mediated_lcl95 = map_dbl(mediation_output_df$mediate_output, function(df) {df$n.avg.ci[[1]]}),
           p_mediated_ucl95 = map_dbl(mediation_output_df$mediate_output, function(df) {df$n.avg.ci[[2]]}),
           p_mediated_exposed = map_dbl(mediation_output_df$mediate_output, function(df) {df$n0}),
           p_mediated_exposed_pv = map_dbl(mediation_output_df$mediate_output, function(df) {df$n0.p}),
           p_mediated_exposed_lcl95 = map_dbl(mediation_output_df$mediate_output, function(df) {df$n0.ci[[1]]}),
           p_mediated_exposed_ucl95 = map_dbl(mediation_output_df$mediate_output, function(df) {df$n0.ci[[2]]}),
           p_mediated_unexposed = map_dbl(mediation_output_df$mediate_output, function(df) {df$n1}),
           p_mediated_unexposed_pv = map_dbl(mediation_output_df$mediate_output, function(df) {df$n1.p}),
           p_mediated_unexposed_lcl95 = map_dbl(mediation_output_df$mediate_output, function(df) {df$n1.ci[[1]]}),
           p_mediated_unexposed_ucl95 = map_dbl(mediation_output_df$mediate_output, function(df) {df$n1.ci[[2]]})) %>%
    mutate(outcome_model_coefs = map(mediation_output_df$outcome_model, function(model) {tidy(model)}),
           mediator_model_coefs = map(mediation_output_df$mediator_model, function(model) {tidy(model)})) %>%
    mutate(pred_coef_med_model = map2_dbl(mediator_model_coefs, predictor, function(df, p) {df %>% filter(term == p) %>% dplyr::select(estimate)%>% pull()}),
           pred_coef_se_med_model = map2_dbl(mediator_model_coefs, predictor, function(df, p) {df %>% filter(term == p) %>% dplyr::select(std.error)%>% pull()})) %>%
    mutate(med_coef_out_model = map2_dbl(outcome_model_coefs, mediator, function(df, m) {df %>% filter(term == m) %>% dplyr::select(estimate)%>% pull()}),
           med_coef_se_out_model = map2_dbl(outcome_model_coefs, mediator, function(df, m) {df %>% filter(term == m) %>% dplyr::select(std.error)%>% pull()}),
           med_coef_pv_out_model = map2_dbl(outcome_model_coefs, mediator, function(df, m) {df %>% filter(term == m) %>% dplyr::select(p.value)%>% pull()})) %>%
    mutate(pred_coef_lcl95_med_model = pred_coef_med_model + (qnorm(0.025) * pred_coef_se_med_model),
           pred_coef_ucl95_med_model = pred_coef_med_model + (qnorm(0.975) * pred_coef_se_med_model)) %>%
    mutate(med_coef_lcl95_out_model = med_coef_out_model + (qnorm(0.025) * med_coef_se_out_model),
           med_coef_ucl95_out_model = med_coef_out_model + (qnorm(0.975) * med_coef_se_out_model)) %>% 
    dplyr::select(mediator,
                  type,
                  hypothesis,
                  predictor,
                  p_mediated,
                  p_mediated_pv,
                  p_mediated_lcl95 ,
                  p_mediated_ucl95,
                  p_mediated_exposed,
                  p_mediated_exposed_pv,
                  p_mediated_exposed_lcl95 ,
                  p_mediated_exposed_ucl95,
                  p_mediated_unexposed,
                  p_mediated_unexposed_pv,
                  p_mediated_unexposed_lcl95 ,
                  p_mediated_unexposed_ucl95,
                  pred_coef_med_model,
                  pred_coef_se_med_model,
                  med_coef_out_model,
                  med_coef_se_out_model,
                  med_coef_pv_out_model,
                  pred_coef_lcl95_med_model,
                  pred_coef_ucl95_med_model,
                  med_coef_lcl95_out_model,
                  med_coef_ucl95_out_model)
  
  print(paste(Sys.time(), "data extracted", sep = " - "))
  
  rm(mediation_output_df)
  
  gc()
  
  sens3_mediation_results_df <- bind_rows(sens3_mediation_results_df, sens3_mediation_results_df_i)
  
  rm(sens3_mediation_results_df_i)
  
  gc()
  
  print(paste(Sys.time(), "files removed", sep = " - "))
  
}

saveRDS(sens3_mediation_results_df, here("data", "sens3_mediation_results_df.RDS"))
# sens3_mediation_results_df <- readRDS(here("data", "sens3_mediation_results_df.RDS"))

# combine with main mediation results and format
sens3_mediation_results_df <- 
  readRDS(here("data", "sens3_mediation_results_df.RDS")) |> 
  mutate(analysis_type = "sensitivity (binary variables)") |> 
  bind_rows(readRDS(here("data", "mediation_results_df.RDS")) |> 
              filter(mediator %in% c("n_img_tests",
                         "n_haem_tests",
                         "n_biochem_tests",
                         "n_other_tests",
                         "n_trtmnts")) |> 
              mutate(analysis_type = "main (count variables)")) |> 
  mutate(mediator_label = case_when(mediator == "n_img_tests" ~ "# imaging tests (patient)",
                                    mediator == "n_haem_tests" ~ "# blood tests (patient)",
                                    mediator == "n_biochem_tests" ~ "# biochem tests (patient)",
                                    mediator == "n_other_tests" ~ "# other tests (patient)",
                                    mediator == "n_trtmnts" ~ "# treatments (patient)",
                                    
                                    mediator == "yn_img_tests" ~ "yn imaging tests (patient)",
                                    mediator == "yn_haem_tests" ~ "yn blood tests (patient)",
                                    mediator == "yn_biochem_tests" ~ "yn biochem tests (patient)",
                                    mediator == "yn_other_tests" ~ "yn other tests (patient)",
                                    mediator == "yn_trtmnts" ~ "yn treatments (patient)"
                                    )) %>% 
  mutate(mediator_label = factor(mediator_label, levels = c("# treatments (patient)",
                                                            "# other tests (patient)",
                                                            "# biochem tests (patient)",
                                                            "# blood tests (patient)",
                                                            "# imaging tests (patient)",
                                                            
                                                            "yn treatments (patient)",
                                                            "yn other tests (patient)",
                                                            "yn biochem tests (patient)",
                                                            "yn blood tests (patient)",
                                                            "yn imaging tests (patient)"
                                                            ))) |> 
  mutate(hypothesis_label = case_when(substr(hypothesis, 8, nchar(hypothesis)) == "reduced demand" ~ "change in demand",
                                      substr(hypothesis, 8, nchar(hypothesis)) == "resources freed up" ~ "availability of shared resources",
                                      substr(hypothesis, 8, nchar(hypothesis)) == "change in ED operations" ~ "change in ED operations")) |> 
  mutate(hypothesis_label = factor(hypothesis_label, levels = c("change in demand", 
                                                                "availability of shared resources", 
                                                                "change in ED operations"))) |> 
  mutate(exposure_label = case_when(predictor == "jr_docs_only_strike" ~ "resident (jr) doctors",
                                    predictor == "consultants_only_strike" ~ "consultants",
                                    predictor == "jr_docs_and_cons_strike" ~ "resident (jr) doctors & consultants",
                                    predictor == "paramedics_strike" ~ "paramedics",
                                    predictor == "nurses_strike" ~ "nurses")) |> 
  mutate(exposure_label  = factor(exposure_label, levels = c("resident (jr) doctors", 
                                                             "consultants", 
                                                             "resident (jr) doctors & consultants", 
                                                             "paramedics", 
                                                             "nurses"))) |> 
  mutate(pred_coef_sig_dir = case_when(sign(pred_coef_lcl95_med_model) != sign(pred_coef_ucl95_med_model) ~ "not sig",
                             pred_coef_med_model < 0 ~ "sig reduced",
                             pred_coef_med_model > 0 ~ "sig increased")) |> 
  mutate(med_coef_sig_dir = case_when(sign(med_coef_lcl95_out_model) != sign(med_coef_ucl95_out_model) ~ "not sig",
                                       med_coef_out_model < 0 ~ "sig reduced",
                                       med_coef_out_model > 0 ~ "sig increased")) |> 
  mutate(p_med_sig_dir = case_when(sign(p_mediated_lcl95) != sign(p_mediated_ucl95) ~ "not sig",
                                   p_mediated < 0 ~ "sig reduced",
                                   p_mediated > 0 ~ "sig increased"))


sens3_mediation_results_df |> 
  ggplot() +
  geom_vline(aes(xintercept = 0), colour = "grey") +
  geom_segment(aes(x = pred_coef_lcl95_med_model, 
                   xend = pred_coef_ucl95_med_model, 
                   y = mediator_label,
                   yend = mediator_label,
                   colour = pred_coef_sig_dir)) +
  geom_point(aes(x = pred_coef_med_model, 
                 y = mediator_label,
                 colour = pred_coef_sig_dir)) +
  facet_grid(cols = vars(exposure_label),
             rows = vars(analysis_type),
             scales = "free_y",
             space = "free_y") +
  scale_x_continuous(name = "change in mediator",
                     label = percent_format(accuracy = 1)) +
  scale_colour_manual(values = c("grey", "blue", "orange"))
  


sens3_mediation_results_df |>
  filter(predictor == "jr_docs_only_strike") |>  # same for all exposures
  ggplot() +
  geom_vline(aes(xintercept = 0), colour = "grey") +
  geom_segment(aes(x = med_coef_lcl95_out_model, 
                   xend = med_coef_ucl95_out_model, 
                   y = mediator_label,
                   yend = mediator_label,
                   colour = med_coef_sig_dir)) +
  geom_point(aes(x = med_coef_out_model, 
                 y = mediator_label,
                 colour = med_coef_sig_dir)) +
  facet_grid(rows = vars(analysis_type),
             scales = "free_y",
             space = "free_y") +
  scale_x_continuous(name = "impact on outcome",
                     label = percent_format(accuracy = 1)) +
  scale_colour_manual(values = c("grey", "blue", "orange"))


sens3_mediation_results_df |> 
  filter(exposure_label != "nurses") |> 
  ggplot() +
  geom_vline(aes(xintercept = 0), colour = "grey") +
  geom_segment(aes(x = p_mediated_lcl95, 
                   xend = p_mediated_ucl95, 
                   y = mediator_label,
                   yend = mediator_label,
                   colour = p_med_sig_dir)) +
  geom_point(aes(x = p_mediated, 
                 y = mediator_label,
                 colour = p_med_sig_dir)) +
  facet_grid(cols = vars(exposure_label),
             rows = vars(analysis_type),
             scales = "free_y",
             space = "free_y") +
  scale_x_continuous(name = "% mediated",
                     label = percent_format(accuracy = 1)) +
  scale_colour_manual(values = c("grey", "blue", "orange"))



manuscript_table_s4 <- sens3_mediation_results_df |> 
  filter(analysis_type == "sensitivity (binary variables)") |> 
  filter(mediator_label != "yn treatments (patient)") |> 
  dplyr::select(mediator_label,
                exposure_label,
                pred_coef_med_model,
                pred_coef_lcl95_med_model,
                pred_coef_ucl95_med_model,
                med_coef_out_model,
                med_coef_lcl95_out_model,
                med_coef_ucl95_out_model,
                p_mediated,
                p_mediated_lcl95,
                p_mediated_ucl95) |> 
  arrange(exposure_label, rev(mediator_label))

write.csv(manuscript_table_s4, here("manuscript", "manuscript_table_s4.csv"))


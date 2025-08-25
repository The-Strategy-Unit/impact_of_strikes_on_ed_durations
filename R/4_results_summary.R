# README

# This script summarises the results of the mediation analysis


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

library("lme4")
library("mgcv")
library("broom.mixed")
library("mediation")
library("mma")
library("survival")




# 1 read in files ----

#model_df <- readRDS(here("data", "model_df.RDS"))
model_df <- readRDS(here("data", "model_df_cut_50pc.RDS"))
#model_df <- readRDS(here("data", "model_df_cut_1m.RDS"))
#model_df <- readRDS(here("data", "model_cut_200k_df.RDS"))
#model_df <- readRDS(here("data", "model_cut_20k_df.RDS"))

unmediated_model <- readRDS(here("data", "unmediated_model.RDS"))
unmediated_model_4hr <- readRDS(here("data", "unmediated_model_4hr.RDS"))
unmediated_model_12hr <- readRDS(here("data", "unmediated_model_12hr.RDS"))

gc()

# 2 visualise unmediated effects ----

# 2a from model coefs ----

unmediated_model_coefs <-
  tidy(unmediated_model) %>% 
  mutate(sig95 = if_else(p.value <= 0.05, 1, 0)) %>% 
  mutate(lcl95 = estimate + qnorm(0.025) * std.error,
         ucl95 = estimate + qnorm(0.975) * std.error) %>% 
  mutate(sig95dir = case_when(sig95 == 0 ~ "not_sig",
                              ucl95 < 0 ~ "sig_low",
                              TRUE ~ "sig_high")) |> 
  mutate(sig95dir = factor(sig95dir, 
                           levels = c("not_sig", "sig_low", "sig_high"))) |> 
  filter(term %in% c("nurses_strike",
                     "paramedics_strike",
                     "jr_docs_and_cons_strike",
                     "consultants_only_strike",
                     "jr_docs_only_strike")) %>%
  mutate(expected_p_change_duration = exp(estimate) - 1,
         lcl95_p_change_duration = exp(lcl95) - 1,
         ucl95_p_change_duration = exp(ucl95) - 1,) %>%
  mutate(term_label = case_when(term == "nurses_strike" ~ "Nurses",
                                term == "paramedics_strike" ~ "Paramedics",
                                term == "jr_docs_and_cons_strike" ~ "Residents & Consultants",
                                term == "consultants_only_strike" ~ "Consultants",
                                term == "jr_docs_only_strike" ~ "Residents")) %>% 
  mutate(term_label = factor(term_label, levels = c("Nurses",
                                        "Paramedics",
                                        "Residents & Consultants",
                                        "Consultants",
                                        "Residents")))

unmediated_model_coefs
              
# unmediated_model_coefs %>% 
#   ggplot() +
#   geom_col(aes(x = expected_p_change_duration, y = term_label, fill = as.factor(sig95))) +
#   scale_x_continuous(name = "% change in ED duration",
#                      label = percent_format(accuracy = 1)) +
#   scale_y_discrete(name = "") +
#   scale_fill_manual(values = c("grey", "blue")) +
#   theme(legend.position = "none") 
  
unmediated_model_coefs %>% 
  ggplot() +
  geom_vline(aes(xintercept = 0), colour = "grey") +
  geom_segment(aes(x = lcl95_p_change_duration, xend = ucl95_p_change_duration,
                   y = term_label, yend = term_label, 
                   colour = sig95dir)) +
  geom_point(aes(x = expected_p_change_duration, 
                 y = term_label, 
                 colour = sig95dir),
             shape = 21) +
  scale_x_continuous(name = "% change in ED duration",
                     label = percent_format(accuracy = 1)) +
  scale_y_discrete(name = "") +
  scale_colour_manual(values = c("grey", "orange", "blue")) +
  labs(title = "Estimated % change in ED durations on days affected by Industrial Action",
       subtitle = "England | December 2022 - July 2024",
       caption = "derived from unmediated model coeficients") +
  theme(legend.position = "none",
        plot.caption = element_text(face = "italic")) 

ggsave(filename = here("charts", "p_change_unmediated.jpg"),
                       device = "jpg",
                       dpi = 300,
                       units = "cm",
                       width = 22,
                       height = 16)


unmediated_model_coefs %>% 
  ggplot() +
  geom_vline(aes(xintercept = 0), colour = "grey") +
  geom_segment(aes(x = lcl95_p_change_duration, xend = ucl95_p_change_duration,
                   y = term_label, yend = term_label, 
                   colour = sig95dir)) +
  geom_point(aes(x = expected_p_change_duration, 
                 y = term_label, 
                 colour = sig95dir),
             shape = 21) +
  scale_x_continuous(name = "% change in ED duration",
                     label = percent_format(accuracy = 1)) +
  scale_y_discrete(name = "") +
  scale_colour_manual(values = c("grey", "orange", "blue")) +
  labs(#title = "Estimated % change in ED durations on days affected by Industrial Action",
       #subtitle = "England | December 2022 - July 2024",
       caption = "whiskers indicate 95% confidence intervals") +
  theme(legend.position = "none",
        plot.caption = element_text(face = "italic")) 


ggsave(filename = here("manuscript", "figure_1.jpg"),
       device = "jpg",
       dpi = 300,
       units = "cm",
       width = 16,
       height = 12)

# 2b coefs of secondary outcome models ----

unmediated_models_4_12_hr_coefs <-
  tidy(unmediated_model_4hr) %>% 
  mutate(model = "unmed_4hr") |> 
  bind_rows(tidy(unmediated_model_12hr) %>% 
              mutate(model = "unmed_12hr")) |> 
  mutate(sig95 = if_else(p.value <= 0.05, 1, 0)) %>% 
  mutate(OR = exp(estimate),
         OR_lcl95 = exp(estimate + qnorm(0.025) * std.error),
         OR_ucl95 = exp(estimate + qnorm(0.975) * std.error)) %>% 
  filter(term %in% c("nurses_strike",
                     "paramedics_strike",
                     "jr_docs_and_cons_strike",
                     "consultants_only_strike",
                     "jr_docs_only_strike"))  %>%
  mutate(term_label = case_when(term == "nurses_strike" ~ "Nurses",
                                term == "paramedics_strike" ~ "Paramedics",
                                term == "jr_docs_and_cons_strike" ~ "Residents & Consultants",
                                term == "consultants_only_strike" ~ "Consultants",
                                term == "jr_docs_only_strike" ~ "Residents")) %>% 
  mutate(term_label = factor(term_label, levels = c("Nurses",
                                                    "Paramedics",
                                                    "Residents & Consultants",
                                                    "Consultants",
                                                    "Residents"))) |> 
  arrange(model, term_label) |> 
  dplyr::select(term_label, model, OR, OR_lcl95, OR_ucl95, sig95)

write.csv(unmediated_models_4_12_hr_coefs, here("manuscript", "table_s2.csv"))

# 2c using average marginal effects ----

jr_docs_strike_margins_df <- bind_rows(model_df, model_df) %>% 
  mutate(jr_docs_only_strike = rep(c(0, 1), each = nrow(model_df)),
         id = rep(1:nrow(model_df), 2)) 

consultants_strike_margins_df <- bind_rows(model_df, model_df) %>% 
  mutate(consultants_only_strike = rep(c(0, 1), each = nrow(model_df)),
         id = rep(1:nrow(model_df), 2))  

jr_docs_consultants_strike_margins_df <- bind_rows(model_df, model_df) %>% 
  mutate(jr_docs_and_cons_strike = rep(c(0, 1), each = nrow(model_df)),
         id = rep(1:nrow(model_df), 2))   

nurses_strike_margins_df <- bind_rows(model_df, model_df) %>% 
  mutate(nurses_strike = rep(c(0, 1), each = nrow(model_df)),
         id = rep(1:nrow(model_df), 2))  

paramedics_strike_margins_df <- bind_rows(model_df, model_df) %>% 
  mutate(paramedics_strike = rep(c(0, 1), each = nrow(model_df)),
         id = rep(1:nrow(model_df), 2))  




mean_mearginal_effects <-
  jr_docs_strike_margins_df %>% 
  mutate(pred_dur_ed = predict(unmediated_model,
                               newdata = jr_docs_strike_margins_df,
                               type = "response")) %>% 
  dplyr::select(id, jr_docs_only_strike, pred_dur_ed) |> 
  pivot_wider(names_from = "jr_docs_only_strike",
              values_from = "pred_dur_ed",
              names_prefix = "strike") |> 
  mutate(marginal_effect = strike1 - strike0) |> 
  summarise(mean_marginal_effect = mean(marginal_effect),
            lq_marginal_effect = quantile(marginal_effect, probs = 0.25),
            uq_marginal_effect = quantile(marginal_effect, probs = 0.75)) |> 
  mutate(exposure = "jr_docs_only_strike") %>% 
  dplyr::select(exposure, mean_marginal_effect, lq_marginal_effect, uq_marginal_effect) |> 
  bind_rows(consultants_strike_margins_df %>% 
              mutate(pred_dur_ed = predict(unmediated_model,
                                           newdata = consultants_strike_margins_df,
                                           type = "response")) %>% 
              dplyr::select(id, consultants_only_strike, pred_dur_ed) |> 
              pivot_wider(names_from = "consultants_only_strike",
                          values_from = "pred_dur_ed",
                          names_prefix = "strike") |> 
              mutate(marginal_effect = strike1 - strike0) |> 
              summarise(mean_marginal_effect = mean(marginal_effect),
                        lq_marginal_effect = quantile(marginal_effect, probs = 0.25),
                        uq_marginal_effect = quantile(marginal_effect, probs = 0.75)) |> 
              mutate(exposure = "consultants_only_strike") %>% 
              dplyr::select(exposure, mean_marginal_effect, lq_marginal_effect, uq_marginal_effect)) |> 
  bind_rows(jr_docs_consultants_strike_margins_df %>% 
              mutate(pred_dur_ed = predict(unmediated_model,
                                           newdata = jr_docs_consultants_strike_margins_df,
                                           type = "response")) %>% 
              dplyr::select(id, jr_docs_and_cons_strike, pred_dur_ed) |> 
              pivot_wider(names_from = "jr_docs_and_cons_strike",
                          values_from = "pred_dur_ed",
                          names_prefix = "strike") |> 
              mutate(marginal_effect = strike1 - strike0) |> 
              summarise(mean_marginal_effect = mean(marginal_effect),
                        lq_marginal_effect = quantile(marginal_effect, probs = 0.25),
                        uq_marginal_effect = quantile(marginal_effect, probs = 0.75)) |> 
              mutate(exposure = "jr_docs_and_cons_strike") %>% 
              dplyr::select(exposure, mean_marginal_effect, lq_marginal_effect, uq_marginal_effect)) |> 
  bind_rows(paramedics_strike_margins_df %>% 
              mutate(pred_dur_ed = predict(unmediated_model,
                                           newdata = paramedics_strike_margins_df,
                                           type = "response")) %>% 
              dplyr::select(id, paramedics_strike, pred_dur_ed) |> 
              pivot_wider(names_from = "paramedics_strike",
                          values_from = "pred_dur_ed",
                          names_prefix = "strike") |> 
              mutate(marginal_effect = strike1 - strike0) |> 
              summarise(mean_marginal_effect = mean(marginal_effect),
                        lq_marginal_effect = quantile(marginal_effect, probs = 0.25),
                        uq_marginal_effect = quantile(marginal_effect, probs = 0.75)) |> 
              mutate(exposure = "paramedics_strike") %>% 
              dplyr::select(exposure, mean_marginal_effect, lq_marginal_effect, uq_marginal_effect)) |> 
  bind_rows(nurses_strike_margins_df %>% 
              mutate(pred_dur_ed = predict(unmediated_model,
                                           newdata = nurses_strike_margins_df,
                                           type = "response")) %>% 
              dplyr::select(id, nurses_strike, pred_dur_ed) |> 
              pivot_wider(names_from = "nurses_strike",
                          values_from = "pred_dur_ed",
                          names_prefix = "strike") |> 
              mutate(marginal_effect = strike1 - strike0) |> 
              summarise(mean_marginal_effect = mean(marginal_effect),
                        lq_marginal_effect = quantile(marginal_effect, probs = 0.25),
                        uq_marginal_effect = quantile(marginal_effect, probs = 0.75)) |> 
              mutate(exposure = "nurses_strike") %>% 
              dplyr::select(exposure, mean_marginal_effect, lq_marginal_effect, uq_marginal_effect))



mean_mearginal_effects


# 3 mediation results ----
# 3a extract key variable for charts ----

mediation_output_files <- c("continuous_mediation_output_df_1.RDS",
                            "continuous_mediation_output_df_2.RDS",
                            "continuous_mediation_output_df_3.RDS",
                            "continuous_mediation_output_df_4.RDS",
                            "continuous_mediation_output_df_5.RDS",
                            "continuous_mediation_output_df_6.RDS",
                            "continuous_mediation_output_df_7.RDS",
                            "continuous_mediation_output_df_8.RDS",
                            "continuous_mediation_output_df_9.RDS",
                            "continuous_mediation_output_df_10.RDS",
                            "continuous_mediation_output_df_11.RDS",
                            "continuous_mediation_output_df_12.RDS",
                            "continuous_mediation_output_df_13.RDS",
                            "continuous_mediation_output_df_14.RDS",
                            "continuous_mediation_output_df_15.RDS",
                            "continuous_mediation_output_df_16.RDS",
                            "count_mediation_output_df_1.RDS",
                            "count_mediation_output_df_2.RDS",
                            "count_mediation_output_df_3.RDS",
                            "count_mediation_output_df_4.RDS",
                            "count_mediation_output_df_5.RDS",
                            "binary_mediation_output_df.RDS")


mediation_results_df <- data.frame(mediator = character(),
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



for (i in (mediation_output_files)) {
  
  print(paste(Sys.time(), i, sep = " - "))
  
  mediation_output_df <- readRDS(here("data", i))
  
  print(paste(Sys.time(), "file in", sep = " - "))
  
  gc()
  
  mediation_results_df_i <-
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
  
  mediation_results_df <- bind_rows(mediation_results_df, mediation_results_df_i)
  
  rm(mediation_results_df_i)
  
  gc()
  
  print(paste(Sys.time(), "files removed", sep = " - "))
  
}

saveRDS(mediation_results_df, here("data", "mediation_results_df.RDS"))
# mediation_results_df <- readRDS(here("data", "mediation_results_df.RDS"))

mediation_results_df <- readRDS(here("data", "mediation_results_df.RDS")) %>% 
  mutate(mediator_label = case_when(mediator == "subicb_gp_appts_sameday_sc" ~ "same-day GP appts (subICB-day)",
                                    mediator == "subicb_gp_appts_prebook_sc" ~ "pre-booked GP appts (subICB-day)",
                                    mediator == "subicb_comm_contacts_sc" ~ "community contacts (subICB-day)",
                                    mediator == "subicb_ed_type3_act_sc" ~ "MIU attendances (subICB-day)",
                                    mediator == "prov_day_ed_type1_amb_act_sc" ~ "amb. conveyed ED atts (prov-day)",
                                    mediator == "prov_day_ed_type1_walkin_act_sc" ~ "walk-in ED atts (prov-day)",
                                    mediator == "google_trends_index" ~ "Google Trends index (day)",
                                    
                                    mediator == "prov_hr_elec_occ_sc" ~ "elective occupied beds (prov-hr)",
                                    mediator == "prov_hr_emer_occ_sc" ~ "non-elective occupied beds (prov-hr)",
                                    mediator == "prov_day_op_att_sc" ~ "outpatient attendances (prov-day)",
                                    mediator == "prov_day_op_proc_sc" ~ "outpatient procedures (prov-day)",
                                    mediator == "prov_day_ct_scan_sc" ~ "CT scans outside ED (prov-day)",
                                    mediator == "prov_day_mri_scan_sc" ~ "MRI scans outside ED (prov-day)",
                                    mediator == "prov_day_us_scan_sc" ~ "US scans outside ED (prov-day)",
                                    mediator == "prov_day_xray_scan_sc" ~ "x-rays outside ED (prov-day)",
                                    
                                    mediator == "distort_4hr" ~ "4hr distortion effect (prov-day)",
                                    mediator == "n_img_tests" ~ "# imaging tests (patient)",
                                    mediator == "n_haem_tests" ~ "# blood tests (patient)",
                                    mediator == "n_biochem_tests" ~ "# biochem tests (patient)",
                                    mediator == "n_other_tests" ~ "# other tests (patient)",
                                    mediator == "n_trtmnts" ~ "# treatments (patient)",
                                    mediator == "is_adm" ~ "is admitted (patient)")) %>% 
           mutate(mediator_label = factor(mediator_label, levels = c("walk-in ED atts (prov-day)",
                                                               "amb. conveyed ED atts (prov-day)",
                                                               "community contacts (subICB-day)",
                                                               "MIU attendances (subICB-day)",
                                                               "same-day GP appts (subICB-day)",
                                                               "pre-booked GP appts (subICB-day)",
                                                               "Google Trends index (day)",
                                                               
                                                               "x-rays outside ED (prov-day)",
                                                               "US scans outside ED (prov-day)",
                                                               "MRI scans outside ED (prov-day)",
                                                               "CT scans outside ED (prov-day)",
                                                               "outpatient procedures (prov-day)",
                                                               "outpatient attendances (prov-day)",
                                                               "elective occupied beds (prov-hr)",
                                                               "non-elective occupied beds (prov-hr)",
                                                               

                                                               
                                                               "4hr distortion effect (prov-day)",
                                                               "is admitted (patient)",
                                                               "# treatments (patient)",
                                                               "# other tests (patient)",
                                                               "# biochem tests (patient)",
                                                               "# blood tests (patient)",
                                                               "# imaging tests (patient)"))) |> 
  mutate(hypothesis_label = case_when(substr(hypothesis, 8, nchar(hypothesis)) == "reduced demand" ~ "change in demand",
                                      substr(hypothesis, 8, nchar(hypothesis)) == "resources freed up" ~ "availability of shared resources",
                                      substr(hypothesis, 8, nchar(hypothesis)) == "change in ED operations" ~ "change in ED operations")) |> 
  mutate(hypothesis_label = factor(hypothesis_label, levels = c("change in demand", 
                                                                "availability of shared resources", 
                                                                "change in ED operations"))) |> 
  mutate(exposure_label = case_when(predictor == "nurses_strike" ~ "Nurses",
                                predictor == "paramedics_strike" ~ "Paramedics",
                                predictor == "jr_docs_and_cons_strike" ~ "Residents & Consultants",
                                predictor == "consultants_only_strike" ~ "Consultants",
                                predictor == "jr_docs_only_strike" ~ "Residents")) %>% 
  mutate(exposure_label = factor(exposure_label, levels = c("Residents",
                                                            "Consultants",
                                                            "Residents & Consultants",
                                                            "Paramedics",
                                                            "Nurses"))) 


# numbers for  manuscript abstract results text

mediation_results_df |> 
  filter(mediator == "prov_hr_emer_occ_sc") |> 
  filter(predictor != "nurses_strike") |> 
  dplyr::select(predictor,
                p_mediated, p_mediated_lcl95, p_mediated_ucl95)
  

mediation_results_df |> 
  filter(mediator == "prov_day_ed_type1_amb_act_sc" | mediator == "prov_day_ed_type1_walkin_act_sc") |> 
  filter(predictor != "nurses_strike") |> 
  filter(predictor != "consultants_only_strike") |> 
  dplyr::select(predictor, mediator,
                p_mediated, p_mediated_lcl95, p_mediated_ucl95) |> 
  group_by(predictor) |> 
  summarise(p_mediated = sum(p_mediated))


mediation_results_df |> 
  filter(predictor == "jr_docs_only_strike") |> 
  filter(mediator == "n_trtmnts") |> 
  dplyr::select(predictor, mediator,
                pred_coef_med_model, pred_coef_lcl95_med_model, pred_coef_ucl95_med_model) |> 
  mutate(IRR = exp(pred_coef_med_model),
         IRR_lcl95 = exp(pred_coef_lcl95_med_model),
         IRR_ucl95 = exp(pred_coef_ucl95_med_model)) |> 
  dplyr::select(-pred_coef_med_model, -pred_coef_lcl95_med_model, -pred_coef_ucl95_med_model)

mediation_results_df |> 
  filter(predictor == "jr_docs_only_strike") |> 
  filter(mediator == "is_adm") |> 
  dplyr::select(predictor, mediator,
                pred_coef_med_model, pred_coef_lcl95_med_model, pred_coef_ucl95_med_model) |> 
  mutate(OR = exp(pred_coef_med_model),
         OR_lcl95 = exp(pred_coef_lcl95_med_model),
         OR_ucl95 = exp(pred_coef_ucl95_med_model)) |> 
  dplyr::select(-pred_coef_med_model, -pred_coef_lcl95_med_model, -pred_coef_ucl95_med_model)


mediation_results_df |> 
  filter(mediator == "distort_4hr") |> 
  filter(predictor != "nurses_strike") |> 
  filter(predictor != "paramedics_strike") |> 
  dplyr::select(predictor,
                p_mediated, p_mediated_lcl95, p_mediated_ucl95)



# 3b visualise stages of mediation ----

# jr docs strike
mediation_results_df %>% 
  mutate(step = "strike >> mediator") %>% 
  mutate(sig95 = if_else(sign(pred_coef_lcl95_med_model) == sign(pred_coef_ucl95_med_model), 1, 0)) %>% 
  dplyr::select(step, predictor, mediator, mediator_label, hypothesis_label,
                estimate = pred_coef_med_model, 
                lcl95 = pred_coef_lcl95_med_model, 
                ucl95 = pred_coef_ucl95_med_model,
                sig95) %>% 
  bind_rows(mediation_results_df %>% 
              mutate(sig95 = if_else(sign(med_coef_lcl95_out_model) == sign(med_coef_ucl95_out_model), 1, 0)) %>% 
              mutate(step = "mediator >> duration in ED") %>% 
              dplyr::select(step, predictor, mediator, mediator_label, hypothesis_label,
                            estimate = med_coef_out_model, 
                            lcl95 = med_coef_lcl95_out_model, 
                            ucl95 = med_coef_ucl95_out_model,
                            sig95)) %>% 
  bind_rows(mediation_results_df %>% 
              mutate(sig95 = if_else(sign(p_mediated_lcl95) == sign(p_mediated_ucl95), 1, 0)) %>% 
              mutate(step = "% mediated") %>% 
              dplyr::select(step, predictor, mediator, mediator_label, hypothesis_label,
                            estimate = p_mediated, 
                            lcl95 = p_mediated_lcl95, 
                            ucl95 = p_mediated_ucl95,
                            sig95)) %>% 
  mutate(step = factor(step, levels = c("strike >> mediator", "mediator >> duration in ED", "% mediated"))) %>% 
  filter(predictor == 'jr_docs_only_strike') %>%
  mutate(gti_label = if_else(step == "strike >> mediator" & mediator == "google_trends_index", as.character(round(estimate, 1)), NA_character_),
         estimate = if_else(step != "strike >> mediator" | mediator != "google_trends_index", estimate, NA_real_),
         lcl95 = if_else(step != "strike >> mediator" | mediator != "google_trends_index", lcl95, NA_real_),
         ucl95 = if_else(step != "strike >> mediator" | mediator != "google_trends_index", ucl95, NA_real_)) %>% 
  ggplot() +
  geom_vline(aes(xintercept = 0)) +
  geom_text(aes(x = 0, y = mediator_label, label = gti_label), hjust = 0) +
  geom_segment(aes(x = lcl95, xend = ucl95, y = mediator_label, yend = mediator_label, colour = as.factor(sig95))) +
  geom_point(aes(x = estimate, y = mediator_label, colour = as.factor(sig95))) +
  facet_grid(cols = vars(step), 
             rows = vars(hypothesis_label),
             scale = "free",
             space = "free_y") +
  scale_colour_manual(values = c("grey", "black")) +
  scale_x_continuous(name = "",
                     label = label_percent(accuracy = 1)) +
  scale_y_discrete(name = "mediator") +
  theme(legend.position = "none") +
  labs(title = "Mediation of Resident (jr) Doctors strike on ED duration")

ggsave(filename = here("charts", "jr_docs_strike_mediation_results.jpg"),
       device = "jpg",
       dpi = 300,
       units = "cm",
       width = 33.5,
       height = 14.5)



    
# consultants strike
mediation_results_df %>% 
  mutate(step = "strike >> mediator") %>% 
  mutate(sig95 = if_else(sign(pred_coef_lcl95_med_model) == sign(pred_coef_ucl95_med_model), 1, 0)) %>% 
  dplyr::select(step, predictor, mediator, mediator_label, hypothesis_label,
                estimate = pred_coef_med_model, 
                lcl95 = pred_coef_lcl95_med_model, 
                ucl95 = pred_coef_ucl95_med_model,
                sig95) %>% 
  bind_rows(mediation_results_df %>% 
              mutate(sig95 = if_else(sign(med_coef_lcl95_out_model) == sign(med_coef_ucl95_out_model), 1, 0)) %>% 
              mutate(step = "mediator >> duration in ED") %>% 
              dplyr::select(step, predictor, mediator, mediator_label, hypothesis_label,
                            estimate = med_coef_out_model, 
                            lcl95 = med_coef_lcl95_out_model, 
                            ucl95 = med_coef_ucl95_out_model,
                            sig95)) %>% 
  bind_rows(mediation_results_df %>% 
              mutate(sig95 = if_else(sign(p_mediated_lcl95) == sign(p_mediated_ucl95), 1, 0)) %>% 
              mutate(step = "% mediated") %>% 
              dplyr::select(step, predictor, mediator, mediator_label, hypothesis_label,
                            estimate = p_mediated, 
                            lcl95 = p_mediated_lcl95, 
                            ucl95 = p_mediated_ucl95,
                            sig95)) %>% 
  mutate(step = factor(step, levels = c("strike >> mediator", "mediator >> duration in ED", "% mediated"))) %>% 
  filter(predictor == 'consultants_only_strike') %>% 
  mutate(gti_label = if_else(step == "strike >> mediator" & mediator == "google_trends_index", as.character(round(estimate, 1)), NA_character_),
         estimate = if_else(step != "strike >> mediator" | mediator != "google_trends_index", estimate, NA_real_),
         lcl95 = if_else(step != "strike >> mediator" | mediator != "google_trends_index", lcl95, NA_real_),
         ucl95 = if_else(step != "strike >> mediator" | mediator != "google_trends_index", ucl95, NA_real_)) %>% 
  ggplot() +
  geom_vline(aes(xintercept = 0)) +
  geom_text(aes(x = 0, y = mediator_label, label = gti_label), hjust = 0) +
  geom_segment(aes(x = lcl95, xend = ucl95, y = mediator_label, yend = mediator_label, colour = as.factor(sig95))) +
  geom_point(aes(x = estimate, y = mediator_label, colour = as.factor(sig95))) +
  facet_grid(cols = vars(step), 
             rows = vars(hypothesis_label),
             scale = "free",
             space = "free_y") +
  scale_colour_manual(values = c("grey", "black")) +
  scale_x_continuous(name = "",
                     label = label_percent(accuracy = 1)) +
  scale_y_discrete(name = "mediator") +
  theme(legend.position = "none") +
  labs(title = "Mediation of Consultants strike on ED duration")

ggsave(filename = here("charts", "conultants_strike_mediation_results.jpg"),
         device = "jpg",
         dpi = 300,
         units = "cm",
         width = 33.5,
         height = 14.5)  
  
  
  
  
  
# jr docs and consultants strike
mediation_results_df %>% 
  mutate(step = "strike >> mediator") %>% 
  mutate(sig95 = if_else(sign(pred_coef_lcl95_med_model) == sign(pred_coef_ucl95_med_model), 1, 0)) %>% 
  dplyr::select(step, predictor, mediator, mediator_label, hypothesis_label,
                estimate = pred_coef_med_model, 
                lcl95 = pred_coef_lcl95_med_model, 
                ucl95 = pred_coef_ucl95_med_model,
                sig95) %>% 
  bind_rows(mediation_results_df %>% 
              mutate(sig95 = if_else(sign(med_coef_lcl95_out_model) == sign(med_coef_ucl95_out_model), 1, 0)) %>% 
              mutate(step = "mediator >> duration in ED") %>% 
              dplyr::select(step, predictor, mediator, mediator_label, hypothesis_label,
                            estimate = med_coef_out_model, 
                            lcl95 = med_coef_lcl95_out_model, 
                            ucl95 = med_coef_ucl95_out_model,
                            sig95)) %>% 
  bind_rows(mediation_results_df %>% 
              mutate(sig95 = if_else(sign(p_mediated_lcl95) == sign(p_mediated_ucl95), 1, 0)) %>% 
              mutate(step = "% mediated") %>% 
              dplyr::select(step, predictor, mediator, mediator_label, hypothesis_label,
                            estimate = p_mediated, 
                            lcl95 = p_mediated_lcl95, 
                            ucl95 = p_mediated_ucl95,
                            sig95)) %>% 
  mutate(step = factor(step, levels = c("strike >> mediator", "mediator >> duration in ED", "% mediated"))) %>% 
  filter(predictor == 'jr_docs_and_cons_strike') %>% 
  mutate(gti_label = if_else(step == "strike >> mediator" & mediator == "google_trends_index", as.character(round(estimate, 1)), NA_character_),
         estimate = if_else(step != "strike >> mediator" | mediator != "google_trends_index", estimate, NA_real_),
         lcl95 = if_else(step != "strike >> mediator" | mediator != "google_trends_index", lcl95, NA_real_),
         ucl95 = if_else(step != "strike >> mediator" | mediator != "google_trends_index", ucl95, NA_real_)) %>% 
  ggplot() +
  geom_vline(aes(xintercept = 0)) +
  geom_text(aes(x = 0, y = mediator_label, label = gti_label), hjust = 0) +
  geom_segment(aes(x = lcl95, xend = ucl95, y = mediator_label, yend = mediator_label, colour = as.factor(sig95))) +
  geom_point(aes(x = estimate, y = mediator_label, colour = as.factor(sig95))) +
  facet_grid(cols = vars(step), 
             rows = vars(hypothesis_label),
             scale = "free",
             space = "free_y") +
  scale_colour_manual(values = c("grey", "black")) +
  scale_x_continuous(name = "",
                     label = label_percent(accuracy = 1)) +
  scale_y_discrete(name = "mediator") +
  theme(legend.position = "none") +   
  labs(title = "Mediation of Resident (jr) Doctors & Consultants strike on ED duration")

ggsave(filename = here("charts", "jr_docs_and_cons_strike_mediation_results.jpg"),
       device = "jpg",
       dpi = 300,
       units = "cm",
       width = 33.5,
       height = 14.5)  

# Paramedics strike
mediation_results_df %>% 
  mutate(step = "strike >> mediator") %>% 
  mutate(sig95 = if_else(sign(pred_coef_lcl95_med_model) == sign(pred_coef_ucl95_med_model), 1, 0)) %>% 
  dplyr::select(step, predictor, mediator, mediator_label, hypothesis_label,
                estimate = pred_coef_med_model, 
                lcl95 = pred_coef_lcl95_med_model, 
                ucl95 = pred_coef_ucl95_med_model,
                sig95) %>% 
  bind_rows(mediation_results_df %>% 
              mutate(sig95 = if_else(sign(med_coef_lcl95_out_model) == sign(med_coef_ucl95_out_model), 1, 0)) %>% 
              mutate(step = "mediator >> duration in ED") %>% 
              dplyr::select(step, predictor, mediator, mediator_label, hypothesis_label,
                            estimate = med_coef_out_model, 
                            lcl95 = med_coef_lcl95_out_model, 
                            ucl95 = med_coef_ucl95_out_model,
                            sig95)) %>% 
  bind_rows(mediation_results_df %>% 
              mutate(sig95 = if_else(sign(p_mediated_lcl95) == sign(p_mediated_ucl95), 1, 0)) %>% 
              mutate(step = "% mediated") %>% 
              dplyr::select(step, predictor, mediator, mediator_label, hypothesis_label,
                            estimate = p_mediated, 
                            lcl95 = p_mediated_lcl95, 
                            ucl95 = p_mediated_ucl95,
                            sig95)) %>% 
  mutate(step = factor(step, levels = c("strike >> mediator", "mediator >> duration in ED", "% mediated"))) %>% 
  filter(predictor == 'paramedics_strike') %>% 
  mutate(gti_label = if_else(step == "strike >> mediator" & mediator == "google_trends_index", as.character(round(estimate, 1)), NA_character_),
         estimate = if_else(step != "strike >> mediator" | mediator != "google_trends_index", estimate, NA_real_),
         lcl95 = if_else(step != "strike >> mediator" | mediator != "google_trends_index", lcl95, NA_real_),
         ucl95 = if_else(step != "strike >> mediator" | mediator != "google_trends_index", ucl95, NA_real_)) %>% 
  ggplot() +
  geom_vline(aes(xintercept = 0)) +
  geom_text(aes(x = 0, y = mediator_label, label = gti_label), hjust = 0) +
  geom_segment(aes(x = lcl95, xend = ucl95, y = mediator_label, yend = mediator_label, colour = as.factor(sig95))) +
  geom_point(aes(x = estimate, y = mediator_label, colour = as.factor(sig95))) +
  facet_grid(cols = vars(step), 
             rows = vars(hypothesis_label),
             scale = "free",
             space = "free_y") +
  scale_colour_manual(values = c("grey", "black")) +
  scale_x_continuous(name = "",
                     label = label_percent(accuracy = 1)) +
  scale_y_discrete(name = "mediator") +
  theme(legend.position = "none") +   
  labs(title = "Mediation of Paramedics strike on ED duration")

ggsave(filename = here("charts", "paramedics_strike_mediation_results.jpg"),
       device = "jpg",
       dpi = 300,
       units = "cm",
       width = 33.5,
       height = 14.5) 

# nurses strike
mediation_results_df %>% 
  mutate(step = "strike >> mediator") %>% 
  mutate(sig95 = if_else(sign(pred_coef_lcl95_med_model) == sign(pred_coef_ucl95_med_model), 1, 0)) %>% 
  dplyr::select(step, predictor, mediator, mediator_label, hypothesis_label,
                estimate = pred_coef_med_model, 
                lcl95 = pred_coef_lcl95_med_model, 
                ucl95 = pred_coef_ucl95_med_model,
                sig95) %>% 
  bind_rows(mediation_results_df %>% 
              mutate(sig95 = if_else(sign(med_coef_lcl95_out_model) == sign(med_coef_ucl95_out_model), 1, 0)) %>% 
              mutate(step = "mediator >> duration in ED") %>% 
              dplyr::select(step, predictor, mediator, mediator_label, hypothesis_label,
                            estimate = med_coef_out_model, 
                            lcl95 = med_coef_lcl95_out_model, 
                            ucl95 = med_coef_ucl95_out_model,
                            sig95)) %>% 
  bind_rows(mediation_results_df %>% 
              mutate(sig95 = if_else(sign(p_mediated_lcl95) == sign(p_mediated_ucl95), 1, 0)) %>% 
              mutate(step = "% mediated") %>% 
              dplyr::select(step, predictor, mediator, mediator_label, hypothesis_label,
                            estimate = p_mediated, 
                            lcl95 = p_mediated_lcl95, 
                            ucl95 = p_mediated_ucl95,
                            sig95)) %>% 
  mutate(step = factor(step, levels = c("strike >> mediator", "mediator >> duration in ED", "% mediated"))) %>% 
  filter(predictor == 'nurses_strike') %>% 
  mutate(gti_label = if_else(step == "strike >> mediator" & mediator == "google_trends_index", as.character(round(estimate, 1)), NA_character_),
         estimate = if_else(step != "strike >> mediator" | mediator != "google_trends_index", estimate, NA_real_),
         lcl95 = if_else(step != "strike >> mediator" | mediator != "google_trends_index", lcl95, NA_real_),
         ucl95 = if_else(step != "strike >> mediator" | mediator != "google_trends_index", ucl95, NA_real_)) %>% 
  ggplot() +
  geom_vline(aes(xintercept = 0)) +
  geom_text(aes(x = 0, y = mediator_label, label = gti_label), hjust = 0) +
  geom_segment(aes(x = lcl95, xend = ucl95, y = mediator_label, yend = mediator_label, colour = as.factor(sig95))) +
  geom_point(aes(x = estimate, y = mediator_label, colour = as.factor(sig95))) +
  facet_grid(cols = vars(step), 
             rows = vars(hypothesis_label),
             scale = "free",
             space = "free_y") +
  scale_colour_manual(values = c("grey", "black")) +
  scale_x_continuous(name = "",
                     label = label_percent(accuracy = 1)) +
  scale_y_discrete(name = "mediator") +
  theme(legend.position = "none") +  
  labs(title = "Mediation of Nurses strike on ED duration")

ggsave(filename = here("charts", "nurses_strike_mediation_results.jpg"),
       device = "jpg",
       dpi = 300,
       units = "cm",
       width = 33.5,
       height = 14.5) 


# 3bi effect of strike on mediators ----

mediation_results_df |> 
  filter(mediator != "google_trends_index") |> 
  mutate(dir_sig95 = case_when(sign(pred_coef_lcl95_med_model) != sign(pred_coef_ucl95_med_model) ~ "not_sig",
                          pred_coef_med_model > 0 ~ "sig_greater",
                          pred_coef_med_model <= 0 ~ "sig_less")) %>% 
  mutate(dir_sig95 = factor(dir_sig95, levels = c("not_sig", "sig_greater", "sig_less"))) %>% 
  dplyr::select(mediator_label, hypothesis_label,
                exposure_label,
                pred_coef_med_model,
                pred_coef_lcl95_med_model,
                pred_coef_ucl95_med_model,
                dir_sig95) |> 
  ggplot() +
  geom_vline(aes(xintercept = 0),
             colour = "grey") +
  geom_segment(aes(x = pred_coef_lcl95_med_model, xend = pred_coef_ucl95_med_model,
                     y = mediator_label, yend = mediator_label,
                   colour = dir_sig95)) +
  geom_point(aes(x = pred_coef_med_model, y = mediator_label,
                 colour = dir_sig95),
             shape = 21) +
  facet_grid(cols = vars(exposure_label),
             rows = vars(hypothesis_label),
             scales = "free_y",
             space = "free_y",
             labeller = labeller(exposure_label = label_wrap_gen(20))) +
  scale_x_continuous(name = "change in mediator during strike",
                     label = label_percent(accuracy = 1)) +
  scale_y_discrete(name = "mediator") +
  scale_colour_manual(values = c("grey", "blue", "orange")) +
  labs(caption = "whiskers = 95% confidence intervals | blue = increased, orange = decreased, grey = no change at 95% level") +
  theme(legend.position = "none",
        plot.caption = element_text(face = "italic"))
  
  
ggsave(filename = here("manuscript", "figure_2.jpg"),
       device = "jpg",
       dpi = 300,
       units = "cm",
       width = 25,
       height = 15) 





# 3bii impact of 1 SD change in mediator on ED duration ----

# 
# mediation_results_df |> 
#   filter(mediator != "google_trends_index") |> 
#   mutate(dir_sig95 = case_when(sign(med_coef_lcl95_out_model) != sign(med_coef_ucl95_out_model) ~ "not_sig",
#                                med_coef_out_model > 0 ~ "sig_greater",
#                                med_coef_out_model <= 0 ~ "sig_less")) %>% 
#   mutate(dir_sig95 = factor(dir_sig95, levels = c("not_sig", "sig_greater", "sig_less"))) %>% 
#   dplyr::select(mediator_label, hypothesis_label,
#                 #exposure_label,
#                 med_coef_out_model,
#                 med_coef_lcl95_out_model,
#                 med_coef_ucl95_out_model,
#                 dir_sig95) |> 
#   distinct() |> 
#   ggplot() +
#   geom_vline(aes(xintercept = 0),
#              colour = "grey") +
#   geom_segment(aes(x = med_coef_lcl95_out_model, xend = med_coef_ucl95_out_model,
#                    y = mediator_label, yend = mediator_label,
#                    colour = dir_sig95)) +
#   geom_point(aes(x = med_coef_out_model, y = mediator_label,
#                  colour = dir_sig95)) +
#   facet_grid(rows = vars(hypothesis_label),
#              scales = "free_y",
#              space = "free_y",
#              labeller = labeller(exposure_label = label_wrap_gen(20))) +
#   scale_x_continuous(name = "impact of mediator on ED durations",
#                      label = label_percent(accuracy = 1)) +
#   scale_y_discrete(name = "mediator") +
#   scale_colour_manual(values = c("grey", "blue", "orange")) +
#   labs(caption = "whiskers = 95% confidence intervals | blue = increase, orange = decrease, grey = no effect at 95% level") +
#   theme(legend.position = "none",
#         plot.caption = element_text(face = "italic"))
#  
# ggsave(filename = here("manuscript", "figure_3.jpg"),
#        device = "jpg",
#        dpi = 300,
#        units = "cm",
#        width = 15,
#        height = 15)  



mediation_results_df |> 
  filter(mediator != "google_trends_index") |> 
  mutate(dir_sig95 = case_when(sign(med_coef_lcl95_out_model) != sign(med_coef_ucl95_out_model) ~ "not_sig",
                               med_coef_out_model > 0 ~ "sig_greater",
                               med_coef_out_model <= 0 ~ "sig_less")) %>% 
  mutate(dir_sig95 = factor(dir_sig95, levels = c("not_sig", "sig_greater", "sig_less"))) %>% 
  dplyr::select(mediator, 
                mediator_label, hypothesis_label,
                #exposure_label,
                med_coef_out_model,
                med_coef_lcl95_out_model,
                med_coef_ucl95_out_model,
                dir_sig95) |> 
  distinct() |> 
  left_join(model_df |> 
              # filter(jr_docs_only_strike == 0 &
              #          consultants_only_strike == 0 &
              #          jr_docs_and_cons_strike == 0 &
              #          paramedics_strike == 0 &
              #          nurses_strike == 0) |> 
              dplyr::select(subicb_gp_appts_sameday_sc,
                            subicb_gp_appts_prebook_sc,
                            subicb_comm_contacts_sc,
                            subicb_ed_type3_act_sc,
                            prov_day_ed_type1_amb_act_sc,
                            prov_day_ed_type1_walkin_act_sc,
                            
                            prov_hr_elec_occ_sc,
                            prov_hr_emer_occ_sc,
                            prov_day_op_att_sc,
                            prov_day_op_proc_sc,
                            prov_day_ct_scan_sc,
                            prov_day_mri_scan_sc,
                            prov_day_us_scan_sc,
                            prov_day_xray_scan_sc,
                            
                            distort_4hr,
                            n_img_tests,
                            n_haem_tests,
                            n_biochem_tests,
                            n_other_tests,
                            n_trtmnts,
                            is_adm) |> 
              pivot_longer(cols = 1:21,
                           names_to = "mediator",
                           values_to = "value") |> 
              group_by(mediator) |> 
              summarise(mean_val = mean(value),
                        sd_value = sd(value)),
            join_by(mediator))|> 
  mutate(cov_value = sd_value / mean_val) |> 
  mutate(expected_p_change_duration = exp(med_coef_out_model) - 1,
         expected_p_change_duration_lcl95 = exp(med_coef_lcl95_out_model) - 1,
         expected_p_change_duration_ucl95 = exp(med_coef_ucl95_out_model) - 1,
         # std_expected_p_change_duration = expected_p_change_duration * sd_value,
         # std_expected_p_change_duration_lcl95 = expected_p_change_duration_lcl95 * sd_value,
         # std_expected_p_change_duration_ucl95 = expected_p_change_duration_ucl95 * sd_value) |> 
         std_expected_p_change_duration = exp(med_coef_out_model * sd_value) - 1,
         std_expected_p_change_duration_lcl95 = exp(med_coef_lcl95_out_model * sd_value) - 1,
         std_expected_p_change_duration_ucl95 = exp(med_coef_ucl95_out_model * sd_value) - 1) |> 
  
  ggplot() +
  geom_vline(aes(xintercept = 0),
             colour = "grey") +
  geom_segment(aes(x = std_expected_p_change_duration_lcl95, xend = std_expected_p_change_duration_ucl95,
                   y = mediator_label, yend = mediator_label,
                   colour = dir_sig95)) +
  geom_point(aes(x = std_expected_p_change_duration, y = mediator_label,
                 colour = dir_sig95),
             shape = 21) +
  facet_grid(rows = vars(hypothesis_label),
             scales = "free_y",
             space = "free_y",
             labeller = labeller(exposure_label = label_wrap_gen(20))) +
  scale_x_continuous(name = "impact of (1 SD) increase in mediator on ED durations",
                     label = label_percent(accuracy = 1)) +
  scale_y_discrete(name = "mediator") +
  scale_colour_manual(values = c("grey", "blue", "orange")) +
  labs(caption = "whiskers = 95% confidence intervals | blue = increase, orange = decrease, grey = no effect at 95% level") +
  theme(legend.position = "none",
        plot.caption = element_text(face = "italic"))

ggsave(filename = here("manuscript", "figure_3.jpg"),
       device = "jpg",
       dpi = 300,
       units = "cm",
       width = 15,
       height = 15)  




# 3biii proportion of effect mediated ----


#  proportion of effect mediated 
# mean_mearginal_effects %>% 
#   mutate(sig95 = ifelse(exposure %in% c("nurses_strike"), 0, 1)) %>% 
#   mutate(exposure = factor(exposure, levels = c("nurses_strike", 
#                                                 "paramedics_strike",
#                                                 "jr_docs_and_cons_strike",
#                                                 "consultants_only_strike", 
#                                                 "jr_docs_only_strike"))) %>% 
#   mutate(exposure_label = case_when(exposure == "nurses_strike" ~ "nurses",
#                                     exposure == "paramedics_strike" ~ "paramedics",
#                                     exposure == "jr_docs_and_cons_strike" ~ "resident (jr) doctors & consultants",
#                                     exposure == "consultants_only_strike" ~ "consultants",
#                                     exposure == "jr_docs_only_strike" ~ "resident (jr) doctors")) %>% 
#   mutate(exposure_label = factor(exposure_label, levels = c("nurses",
#                                                     "paramedics",
#                                                     "resident (jr) doctors & consultants",
#                                                     "consultants",
#                                                     "resident (jr) doctors"))) %>% 
#   ggplot() +
#   geom_col(aes(x = mean_marginal_effect,
#                y = exposure_label,
#                fill = as.factor(sig95))) +
#   scale_x_continuous(name = "mean marginal change in ED duration (mins)",
#                      label = number_format(accuracy = 1)) +
#   scale_y_discrete(name = "") +
#   scale_fill_manual(values = c("grey", "blue")) +
#   theme(legend.position = "none") +
#   labs(title = "Impact of strikes on ED durations")
# 
# ggsave(filename = here("charts", "mean_marginal_effect.jpg"),
#        device = "jpg",
#        dpi = 300,
#        units = "cm",
#        width = 25,
#        height = 15) 



mins_mediated_by_mediator <- 
  mean_mearginal_effects %>% 
  inner_join(mediation_results_df, 
             join_by(exposure == predictor)) %>% 
  mutate(dir_sig95 = case_when(mean_marginal_effect * p_mediated > 0 & p_mediated_pv <= .05 ~ "increase",
                               mean_marginal_effect * p_mediated < 0 & p_mediated_pv <= .05 ~ "decrease",
                               p_mediated_pv > .05 ~ "no_change")) |> 
  mutate(mins_mediated = mean_marginal_effect * p_mediated,
         mins_mediated_lcl = mean_marginal_effect * p_mediated_ucl95,
         mins_mediated_ucl = mean_marginal_effect * p_mediated_lcl95) %>% 
  filter(mediator != "google_trends_index") |> 
  mutate(mediator_label = case_when(mediator == "subicb_gp_appts_sameday_sc" ~ "same-day GP appts (subICB-day)",
                                    mediator == "subicb_gp_appts_prebook_sc" ~ "pre-booked GP appts (subICB-day)",
                                    mediator == "subicb_comm_contacts_sc" ~ "community contacts (subICB-day)",
                                    mediator == "subicb_ed_type3_act_sc" ~ "MIU attendances (subICB-day)",
                                    mediator == "prov_day_ed_type1_amb_act_sc" ~ "amb. conveyed ED atts (prov-day)",
                                    mediator == "prov_day_ed_type1_walkin_act_sc" ~ "walk-in ED atts (prov-day)",
                                    mediator == "google_trends_index" ~ "Google Trends index (day)",
                                    
                                    mediator == "prov_hr_elec_occ_sc" ~ "elective occupied beds (prov-hr)",
                                    mediator == "prov_hr_emer_occ_sc" ~ "non-elective occupied beds (prov-hr)",
                                    mediator == "prov_day_op_att_sc" ~ "outpatient attendances (prov-day)",
                                    mediator == "prov_day_op_proc_sc" ~ "outpatient procedures (prov-day)",
                                    mediator == "prov_day_ct_scan_sc" ~ "CT scans outside ED (prov-day)",
                                    mediator == "prov_day_mri_scan_sc" ~ "MRI scans outside ED (prov-day)",
                                    mediator == "prov_day_us_scan_sc" ~ "US scans outside ED (prov-day)",
                                    mediator == "prov_day_xray_scan_sc" ~ "x-rays outside ED (prov-day)",
                                    
                                    mediator == "distort_4hr" ~ "4hr distortion effect (prov-day)",
                                    mediator == "n_img_tests" ~ "# imaging tests (patient)",
                                    mediator == "n_haem_tests" ~ "# blood tests (patient)",
                                    mediator == "n_biochem_tests" ~ "# biochem tests (patient)",
                                    mediator == "n_other_tests" ~ "# other tests (patient)",
                                    mediator == "n_trtmnts" ~ "# treatments (patient)",
                                    mediator == "is_adm" ~ "is admitted (patient)")) %>% 
  mutate(mediator_label = factor(mediator_label, levels = c("pre-booked GP appts (subICB-day)",
                                                            "same-day GP appts (subICB-day)",
                                                            "MIU attendances (subICB-day)",
                                                            "community contacts (subICB-day)",
                                                            "amb. conveyed ED atts (prov-day)",
                                                            "walk-in ED atts (prov-day)",

                                                            "non-elective occupied beds (prov-hr)",
                                                            "elective occupied beds (prov-hr)",
                                                            "outpatient attendances (prov-day)",
                                                            "outpatient procedures (prov-day)",
                                                            "CT scans outside ED (prov-day)",
                                                            "MRI scans outside ED (prov-day)",
                                                            "US scans outside ED (prov-day)",
                                                            "x-rays outside ED (prov-day)",
  
                                                            "# imaging tests (patient)",
                                                            "# blood tests (patient)",
                                                            "# biochem tests (patient)",
                                                            "# other tests (patient)",
                                                            "# treatments (patient)",
                                                            "is admitted (patient)",
                                                            "4hr distortion effect (prov-day)"))) |> 
  mutate(hypothesis_label = case_when(substr(hypothesis, 8, nchar(hypothesis)) == "reduced demand" ~ "change in demand",
                                      substr(hypothesis, 8, nchar(hypothesis)) == "resources freed up" ~ "availability of shared resources",
                                      substr(hypothesis, 8, nchar(hypothesis)) == "change in ED operations" ~ "change in ED operations")) |> 
  mutate(hypothesis_label = factor(hypothesis_label, levels = c("change in demand", 
                                                                "availability of shared resources", 
                                                                "change in ED operations"))) |> 
  mutate(exposure_label = case_when(exposure == "nurses_strike" ~ "Nurses",
                                    exposure == "paramedics_strike" ~ "Paramedics",
                                    exposure == "jr_docs_and_cons_strike" ~ "Residents & Consultants",
                                    exposure == "consultants_only_strike" ~ "Consultants",
                                    exposure == "jr_docs_only_strike" ~ "Residents")) %>% 
  mutate(exposure_label = factor(exposure_label, levels = c("Residents",
                                                            "Consultants",
                                                            "Residents & Consultants",
                                                            "Paramedics",
                                                            "Nurses")))  |> 
  dplyr::select(hypothesis_label, mediator_label, exposure_label, 
                mins_mediated, mins_mediated_lcl, mins_mediated_ucl, dir_sig95) |> 
  arrange(exposure_label, hypothesis_label, mediator_label)

mins_mediated_by_mediator

write.csv(mins_mediated_by_mediator, here("manuscript", "table_2.csv"))

mins_mediated_by_mediator |> 
  mutate(mins_mediated_lcl =if_else(dir_sig95 == "no_change", NA_real_,mins_mediated_lcl),
         mins_mediated_ucl =if_else(dir_sig95 == "no_change", NA_real_,mins_mediated_ucl)) |> 
  mutate(exposure_label2 = case_when(exposure_label == "Residents" ~ 
                                      "total effect -37.9 mins",
                                    exposure_label == "Consultants" ~ 
                                      "total effect -16.8 mins",
                                    exposure_label == "Residents & Consultants" ~ 
                                      "total effect +21.4 mins",
                                    exposure_label == "Paramedics" ~ 
                                      "total effect -13.3 mins",
                                    exposure_label == "Nurses" ~ 
                                      "total effect -4.9 mins")) |>
  ggplot() +
  geom_vline(aes(xintercept = 0),
             colour = "grey") +
  geom_segment(aes(x = mins_mediated, xend = 0,
                   y = mediator_label, yend = mediator_label,
                   colour = dir_sig95)) +
  geom_point(aes(x = mins_mediated, y = mediator_label,
                 colour = dir_sig95)) +
  facet_grid(cols = vars(exposure_label, exposure_label2),
             rows = vars(hypothesis_label),
             scales = "free_y",
             space = "free_y",
             labeller = labeller(exposure_label = label_wrap_gen(24))) +
  scale_x_continuous(name = "mediated effect (minutes)",
                     limits = c(-10, 5)) +
  scale_y_discrete(name = "mediator",
                   limits = rev) +
  scale_colour_manual(values = c("orange", "blue", "grey")) +
  labs(caption = "blue = increased ED duration, orange = decreased ED duration, grey = no change at 95% level") +
  theme(legend.position = "none",
        plot.caption = element_text(face = "italic"))


ggsave(filename = here("manuscript", "figure_4_v1.jpg"),
       device = "jpg",
       dpi = 300,
       units = "cm",
       width = 25,
       height = 15) 
  
  
  
mins_mediated_by_mediator |> 
  mutate(mins_mediated_lcl =if_else(dir_sig95 == "no_change", NA_real_,mins_mediated_lcl),
         mins_mediated_ucl =if_else(dir_sig95 == "no_change", NA_real_,mins_mediated_ucl)) |> 
  mutate(exposure_label2 = case_when(exposure_label == "Residents" ~ 
                                       "total effect -37.9 mins",
                                     exposure_label == "Consultants" ~ 
                                       "total effect -16.8 mins",
                                     exposure_label == "Residents & Consultants" ~ 
                                       "total effect +21.4 mins",
                                     exposure_label == "Paramedics" ~ 
                                       "total effect -13.3 mins",
                                     exposure_label == "Nurses" ~ 
                                       "total effect -4.9 mins")) |>
  ggplot() +
  geom_vline(aes(xintercept = 0),
             colour = "grey") +
  geom_segment(aes(x = mins_mediated_lcl, xend = mins_mediated_ucl,
                   y = mediator_label, yend = mediator_label,
                   colour = dir_sig95)) +
  geom_point(aes(x = mins_mediated, y = mediator_label,
                 colour = dir_sig95),
             shape = 21) +
  facet_grid(cols = vars(exposure_label, exposure_label2),
             rows = vars(hypothesis_label),
             scales = "free_y",
             space = "free_y",
             labeller = labeller(exposure_label = label_wrap_gen(20))) +
  scale_x_continuous(name = "mediated effect (minutes)",
                     limits = c(-10, 5)) +
  scale_y_discrete(name = "mediator",
                   limits = rev) +
  scale_colour_manual(values = c("orange", "blue", "grey")) +
  labs(caption = "whiskers = 95% confidence intervals (shown for significant effects only) | blue = increased, orange = decreased, grey = no change at 95% level") +
  theme(legend.position = "none",
        plot.caption = element_text(face = "italic"))


ggsave(filename = here("manuscript", "figure_s2.jpg"),
       device = "jpg",
       dpi = 300,
       units = "cm",
       width = 25,
       height = 15) 
  
# 
# mins_mediated_by_mediator_icl_unobs <- 
#   mean_mearginal_effects %>% 
#   left_join(mins_mediated_by_mediator %>% 
#               group_by(exposure) %>% 
#               summarise(mins_mediated = sum(mins_mediated)),
#             join_by(exposure)) %>% 
#   mutate(mins_mediated = if_else(is.na(mins_mediated), 0, mins_mediated)) %>%  
#   mutate(mins_mediated = mean_marginal_effect - mins_mediated) %>% 
#   mutate(mediator = "other") %>% 
#   mutate(mediator_label = "direct & unobserved mediated effect") %>% 
#   mutate(hypothesis = "none") %>% 
#   dplyr::select(exposure, mediator, mediator_label,hypothesis, mins_mediated) %>% 
#   bind_rows(mins_mediated_by_mediator)
# 
# 
# 
# 
# # all mediators
# mins_mediated_by_mediator_icl_unobs %>% 
#   mutate(status = if_else(mediator == "other", "unobserved", "explained")) %>% 
#   group_by(exposure, status) %>% 
#   summarise(mins_mediated = sum(mins_mediated)) %>% 
#   mutate(exposure = factor(exposure, levels = c("nurses_strike", 
#                                                 "paramedics_strike",
#                                                 "jr_docs_and_cons_strike",
#                                                 "consultants_only_strike", 
#                                                 "jr_docs_only_strike"))) %>% 
#   mutate(exposure_label = case_when(exposure == "nurses_strike" ~ "nurses",
#                                     exposure == "paramedics_strike" ~ "paramedics",
#                                     exposure == "jr_docs_and_cons_strike" ~ "resident (jr) doctors & consultants",
#                                     exposure == "consultants_only_strike" ~ "consultants",
#                                     exposure == "jr_docs_only_strike" ~ "resident (jr) doctors")) %>% 
#   mutate(exposure_label = factor(exposure_label, levels = c("nurses",
#                                                             "paramedics",
#                                                             "resident (jr) doctors & consultants",
#                                                             "consultants",
#                                                             "resident (jr) doctors"))) %>% 
#   mutate(status = factor(status, levels = c("unobserved", 
#                                                 "explained"))) %>% 
#   ggplot() +
#   geom_col(aes(x = mins_mediated,
#                y = exposure_label,
#                fill = status),
#            position = position_stack()) +
#   scale_x_continuous(name = "mean marginal change in ED duration (mins)",
#                      label = number_format(accuracy = 1)) +
#   scale_y_discrete(name = "") +
#   scale_fill_manual(values = c("blue", "red"))  +
#   theme(legend.position = "none") +
#   labs(title = "Proportion of effect explained by all observed mediators")
# 
# ggsave(filename = here("charts", "proportion_mediated_all_hypotheses.jpg"),
#        device = "jpg",
#        dpi = 300,
#        units = "cm",
#        width = 25,
#        height = 15) 
# 
# 
# 
# # hyp1 mediators
# mins_mediated_by_mediator_icl_unobs %>% 
#   mutate(status = if_else(hypothesis == "hyp1 - reduced demand", "explained", "unobserved")) %>% 
#   group_by(exposure, status) %>% 
#   summarise(mins_mediated = sum(mins_mediated)) %>% 
#   mutate(exposure = factor(exposure, levels = c("nurses_strike", 
#                                                 "paramedics_strike",
#                                                 "jr_docs_and_cons_strike",
#                                                 "consultants_only_strike", 
#                                                 "jr_docs_only_strike"))) %>% 
#   mutate(exposure_label = case_when(exposure == "nurses_strike" ~ "nurses",
#                                     exposure == "paramedics_strike" ~ "paramedics",
#                                     exposure == "jr_docs_and_cons_strike" ~ "resident (jr) doctors & consultants",
#                                     exposure == "consultants_only_strike" ~ "consultants",
#                                     exposure == "jr_docs_only_strike" ~ "resident (jr) doctors")) %>% 
#   mutate(exposure_label = factor(exposure_label, levels = c("nurses",
#                                                             "paramedics",
#                                                             "resident (jr) doctors & consultants",
#                                                             "consultants",
#                                                             "resident (jr) doctors"))) %>% 
#   mutate(status = factor(status, levels = c("unobserved", 
#                                             "explained"))) %>% 
#   ggplot() +
#   geom_col(aes(x = mins_mediated,
#                y = exposure_label,
#                fill = status),
#            position = position_stack()) +
#   scale_x_continuous(name = "mean marginal change in ED duration (mins)",
#                      label = number_format(accuracy = 1)) +
#   scale_y_discrete(name = "") +
#   scale_fill_manual(values = c("blue", "orange", "grey", "grey"))  +
#   theme(legend.position = "none") +
#   labs(title = "Proportion of effect explained by hypothesis 1 - reduced demand")
# 
# ggsave(filename = here("charts", "proportion_mediated_hypothesis_1.jpg"),
#        device = "jpg",
#        dpi = 300,
#        units = "cm",
#        width = 25,
#        height = 15) 
# 
# # hyp2 mediators
# mins_mediated_by_mediator_icl_unobs %>% 
#   mutate(status = if_else(hypothesis == "hyp2 - resources freed up", "explained", "unobserved")) %>% 
#   group_by(exposure, status) %>% 
#   summarise(mins_mediated = sum(mins_mediated)) %>% 
#   mutate(exposure = factor(exposure, levels = c("nurses_strike", 
#                                                 "paramedics_strike",
#                                                 "jr_docs_and_cons_strike",
#                                                 "consultants_only_strike", 
#                                                 "jr_docs_only_strike"))) %>% 
#   mutate(exposure_label = case_when(exposure == "nurses_strike" ~ "nurses",
#                                     exposure == "paramedics_strike" ~ "paramedics",
#                                     exposure == "jr_docs_and_cons_strike" ~ "resident (jr) doctors & consultants",
#                                     exposure == "consultants_only_strike" ~ "consultants",
#                                     exposure == "jr_docs_only_strike" ~ "resident (jr) doctors")) %>% 
#   mutate(exposure_label = factor(exposure_label, levels = c("nurses",
#                                                             "paramedics",
#                                                             "resident (jr) doctors & consultants",
#                                                             "consultants",
#                                                             "resident (jr) doctors"))) %>% 
#   mutate(status = factor(status, levels = c("unobserved", 
#                                             "explained"))) %>% 
#   ggplot() +
#   geom_col(aes(x = mins_mediated,
#                y = exposure_label,
#                fill = status),
#            position = position_stack()) +
#   scale_x_continuous(name = "mean marginal change in ED duration (mins)",
#                      label = number_format(accuracy = 1)) +
#   scale_y_discrete(name = "") +
#   scale_fill_manual(values = c("blue", "green", "grey", "grey"))  +
#   theme(legend.position = "none") +
#   labs(title = "Proportion of effect explained by hypothesis 2 - resources freed up")
# 
# ggsave(filename = here("charts", "proportion_mediated_hypothesis_2.jpg"),
#        device = "jpg",
#        dpi = 300,
#        units = "cm",
#        width = 25,
#        height = 15) 
# 
# # hyp3 mediators
# mins_mediated_by_mediator_icl_unobs %>% 
#   mutate(status = if_else(hypothesis == "hyp3 - change in ED operations", "explained", "unobserved")) %>% 
#   group_by(exposure, status) %>% 
#   summarise(mins_mediated = sum(mins_mediated)) %>% 
#   mutate(exposure = factor(exposure, levels = c("nurses_strike", 
#                                                 "paramedics_strike",
#                                                 "jr_docs_and_cons_strike",
#                                                 "consultants_only_strike", 
#                                                 "jr_docs_only_strike"))) %>% 
#   mutate(exposure_label = case_when(exposure == "nurses_strike" ~ "nurses",
#                                     exposure == "paramedics_strike" ~ "paramedics",
#                                     exposure == "jr_docs_and_cons_strike" ~ "resident (jr) doctors & consultants",
#                                     exposure == "consultants_only_strike" ~ "consultants",
#                                     exposure == "jr_docs_only_strike" ~ "resident (jr) doctors")) %>% 
#   mutate(exposure_label = factor(exposure_label, levels = c("nurses",
#                                                             "paramedics",
#                                                             "resident (jr) doctors & consultants",
#                                                             "consultants",
#                                                             "resident (jr) doctors"))) %>% 
#   mutate(status = factor(status, levels = c("unobserved", 
#                                             "explained"))) %>% 
#   ggplot() +
#   geom_col(aes(x = mins_mediated,
#                y = exposure_label,
#                fill = status),
#            position = position_stack()) +
#   scale_x_continuous(name = "mean marginal change in ED duration (mins)",
#                      label = number_format(accuracy = 1)) +
#   scale_y_discrete(name = "") +
#   scale_fill_manual(values = c("blue", "yellow", "grey", "grey"))  +
#   theme(legend.position = "none") +
#   labs(title = "Proportion of effect explained by hypothesis 3 - change in ED operations")
# 
# ggsave(filename = here("charts", "proportion_mediated_hypothesis_3.jpg"),
#        device = "jpg",
#        dpi = 300,
#        units = "cm",
#        width = 25,
#        height = 15) 
# 
# 
# # all hypotheses
# mins_mediated_by_mediator_icl_unobs %>% 
#   mutate(status = case_when(hypothesis == "hyp1 - reduced demand" ~ "hyp1 - reduced demand",
#                               hypothesis == "hyp2 - resources freed up" ~ "hyp2 - resources freed up",
#                               hypothesis == "hyp3 - change in ED operations" ~ "hyp3 - change in ED operations",
#                               hypothesis == "none"  ~ "direct & unobserved mediated effect")) %>%
#   group_by(exposure, status) %>% 
#   summarise(mins_mediated = sum(mins_mediated)) %>% 
#   mutate(status = factor(status, levels = c("direct & unobserved mediated effect", 
#                                                 "hyp1 - reduced demand", 
#                                                 "hyp2 - resources freed up", 
#                                                 "hyp3 - change in ED operations"))) %>%
#   mutate(exposure = factor(exposure, levels = c("nurses_strike", 
#                                                 "paramedics_strike",
#                                                 "jr_docs_and_cons_strike",
#                                                 "consultants_only_strike", 
#                                                 "jr_docs_only_strike"))) %>% 
#   mutate(exposure_label = case_when(exposure == "nurses_strike" ~ "nurses",
#                                     exposure == "paramedics_strike" ~ "paramedics",
#                                     exposure == "jr_docs_and_cons_strike" ~ "resident (jr) doctors & consultants",
#                                     exposure == "consultants_only_strike" ~ "consultants",
#                                     exposure == "jr_docs_only_strike" ~ "resident (jr) doctors")) %>% 
#   mutate(exposure_label = factor(exposure_label, levels = c("nurses",
#                                                             "paramedics",
#                                                             "resident (jr) doctors & consultants",
#                                                             "consultants",
#                                                             "resident (jr) doctors"))) %>% 
#   ggplot() +
#   geom_col(aes(x = mins_mediated,
#                y = exposure_label,
#                fill = status),
#            position = position_stack()) +
#   geom_vline(aes(xintercept = 0)) +
#   scale_x_continuous(name = "mean marginal change in ED duration (mins)",
#                      label = number_format(accuracy = 1)) +
#   scale_y_discrete(name = "") +
#   scale_fill_manual(values = c("blue", "orange", "green", "yellow", "grey"))  +
#   theme(legend.title = element_blank(),
#         legend.position = "bottom") +
#   labs(title = "Proportion of effect explained by hypothesis")
# 
# ggsave(filename = here("charts", "proportion_mediated_hypothesis_123.jpg"),
#        device = "jpg",
#        dpi = 300,
#        units = "cm",
#        width = 25,
#        height = 15) 
# 
# 
# # effect by mediator plot
# mins_mediated_by_mediator_icl_unobs  %>% 
#   group_by(exposure, hypothesis, mediator, mediator_label) %>% 
#   summarise(mins_mediated = sum(mins_mediated)) %>% 
#   mutate(status = case_when(hypothesis == "hyp1 - reduced demand"  ~ "hyp1 - reduced demand",
#                               hypothesis == "hyp2 - resources freed up" ~ "hyp2 - resources freed up",
#                               hypothesis == "hyp3 - change in ED operations"  ~ "hyp3 - change in ED operations",
#                               hypothesis == "none" ~ "unobserved")) %>%
#   mutate(status = factor(status, levels = c("unobserved", 
#                                                 "hyp1 - reduced demand", 
#                                                 "hyp2 - resources freed up", 
#                                                 "hyp3 - change in ED operations"))) %>%
#   mutate(exposure = factor(exposure, levels = c("nurses_strike", 
#                                                 "paramedics_strike",
#                                                 "jr_docs_and_cons_strike",
#                                                 "consultants_only_strike", 
#                                                 "jr_docs_only_strike"))) %>% 
#   mutate(exposure_label = case_when(exposure == "nurses_strike" ~ "nurses",
#                                     exposure == "paramedics_strike" ~ "paramedics",
#                                     exposure == "jr_docs_and_cons_strike" ~ "resident (jr) doctors & consultants",
#                                     exposure == "consultants_only_strike" ~ "consultants",
#                                     exposure == "jr_docs_only_strike" ~ "resident (jr) doctors")) %>% 
#   mutate(exposure_label = factor(exposure_label, levels = c("resident (jr) doctors",
#                                                             "consultants",
#                                                             "resident (jr) doctors & consultants",
#                                                             "paramedics",
#                                                             "nurses"))) %>% 
#   ggplot() +
#   geom_col(aes(x = mins_mediated, 
#                y = mediator_label,
#                fill = status)) +
#   geom_vline(aes(xintercept = 0)) +
#   scale_x_continuous(name = "mean marginal change in ED duration (mins)",
#                      label = number_format(accuracy = 1)) +
#   scale_y_discrete(name = "mediator") +
#   facet_grid(cols = vars(exposure_label), 
#              rows = vars(hypothesis),
#              scale = "free_y",
#              space = "free",
#              labeller = labeller(hypothesis = label_wrap_gen(10),
#                                  exposure_label = label_wrap_gen(15))) +
#   scale_fill_manual(values = c("blue", "orange", "green", "yellow")) +
#   theme(legend.position = "none",
#         strip.text.y = element_text(angle = 0))
# 
# 
# ggsave(filename = here("charts", "proportion_mediated_mediator_hypothesis_123_incl_unobs.jpg"),
#        device = "jpg",
#        dpi = 300,
#        units = "cm",
#        width = 25,
#        height = 15) 
# 
# 
# mins_mediated_by_mediator  %>% 
#   group_by(exposure, hypothesis, mediator, mediator_label) %>% 
#   summarise(mins_mediated = sum(mins_mediated)) %>% 
#   mutate(hypothesis_label = substr(hypothesis, 8, nchar(hypothesis))) |> 
#   mutate(hypothesis_label = factor(hypothesis_label, levels = c("reduced demand", 
#                                                                 "resources freed up", 
#                                                                 "change in ED operations"))) |> 
#   mutate(exposure = factor(exposure, levels = c("nurses_strike", 
#                                                 "paramedics_strike",
#                                                 "jr_docs_and_cons_strike",
#                                                 "consultants_only_strike", 
#                                                 "jr_docs_only_strike"))) %>% 
#   mutate(exposure_label = case_when(exposure == "nurses_strike" ~ "nurses",
#                                     exposure == "paramedics_strike" ~ "paramedics",
#                                     exposure == "jr_docs_and_cons_strike" ~ "resident (jr) doctors & consultants",
#                                     exposure == "consultants_only_strike" ~ "consultants",
#                                     exposure == "jr_docs_only_strike" ~ "resident (jr) doctors")) %>% 
#   mutate(exposure_label = factor(exposure_label, levels = c("resident (jr) doctors",
#                                                             "consultants",
#                                                             "resident (jr) doctors & consultants",
#                                                             "paramedics",
#                                                             "nurses"))) %>% 
#   ggplot() +
#   geom_col(aes(x = mins_mediated, 
#                y = mediator_label,
#                fill = hypothesis_label)) +
#   geom_vline(aes(xintercept = 0)) +
#   scale_x_continuous(name = "mean marginal change in ED duration (mins)",
#                      label = number_format(accuracy = 1)) +
#   scale_y_discrete(name = "mediator") +
#   facet_grid(cols = vars(exposure_label), 
#              rows = vars(hypothesis_label),
#              scale = "free_y",
#              space = "free",
#              labeller = labeller(hypothesis_label = label_wrap_gen(10),
#                                  exposure_label = label_wrap_gen(15))) +
#   scale_fill_manual(values = c("blue", "orange", "green")) +
#   theme(legend.position = "none",
#         strip.text.y = element_text(angle = 0))
# 
# ggsave(filename = here("charts", "proportion_mediated_mediator_hypothesis_123.jpg"),
#        device = "jpg",
#        dpi = 300,
#        units = "cm",
#        width = 25,
#        height = 15) 



# 3c check if p_mediated varies between exposed and unexposed ----
mediation_results_df %>% 
  mutate(status = case_when(hypothesis == "hyp1 - reduced demand"  ~ "hyp1 - reduced demand",
                            hypothesis == "hyp2 - resources freed up" ~ "hyp2 - resources freed up",
                            hypothesis == "hyp3 - change in ED operations"  ~ "hyp3 - change in ED operations",
                            hypothesis == "none" ~ "unobserved")) %>%
  mutate(status = factor(status, levels = c("unobserved", 
                                            "hyp1 - reduced demand", 
                                            "hyp2 - resources freed up", 
                                            "hyp3 - change in ED operations"))) %>%
  mutate(exposure = factor(predictor, levels = c("nurses_strike", 
                                                 "paramedics_strike",
                                                 "jr_docs_and_cons_strike",
                                                 "consultants_only_strike", 
                                                 "jr_docs_only_strike"))) %>% 
  mutate(exposure_label = case_when(exposure == "nurses_strike" ~ "nurses",
                                    exposure == "paramedics_strike" ~ "paramedics",
                                    exposure == "jr_docs_and_cons_strike" ~ "resident (jr) doctors & consultants",
                                    exposure == "consultants_only_strike" ~ "consultants",
                                    exposure == "jr_docs_only_strike" ~ "resident (jr) doctors")) %>% 
  mutate(exposure_label = factor(exposure_label, levels = c("resident (jr) doctors",
                                                            "consultants",
                                                            "resident (jr) doctors & consultants",
                                                            "paramedics",
                                                            "nurses"))) %>% 
  ggplot() +
  geom_point(aes(y = mediator_label, x = p_mediated),
             colour = "black",
             shape = 108) +
  geom_point(aes(y = mediator_label, x = p_mediated_exposed),
             colour = "red",
             shape = 108) +
  geom_point(aes(y = mediator_label, x = p_mediated_unexposed),
             colour = "blue",
             shape = 108) +
  facet_grid(cols = vars(exposure_label), 
             rows = vars(hypothesis),
             scale = "free_y",
             space = "free",
             labeller = labeller(hypothesis = label_wrap_gen(10),
                                 exposure_label = label_wrap_gen(15)))


# # 4 fully mediated model marginal effects ----
# 
# fully_mediated_outcome_model <- readRDS(here("data", "fully_mediated_outcome_model.df"))
# 
# # mean marginal effects of a 10% increase/decrease in each of the mediators
# # grouped by strike / non-strike
# 
# 
# fully_mediated_marginal_df <-
#   model_df %>% 
#   mutate(scenario = "no change") %>% 
#   bind_rows(model_df %>% 
#               mutate(google_trends_index = google_trends_index * 1.1) %>% 
#               mutate(scenario = "google_trends_index")) %>% 
#   bind_rows(model_df %>% 
#               mutate(subicb_gp_appts_prebook_sc =  subicb_gp_appts_prebook_sc * 1.1) %>% 
#               mutate(scenario = "subicb_gp_appts_prebook_sc")) %>% 
#   bind_rows(model_df %>% 
#               mutate(subicb_gp_appts_sameday_sc =  subicb_gp_appts_sameday_sc * 1.1) %>% 
#               mutate(scenario = "subicb_gp_appts_sameday_sc")) %>% 
#   bind_rows(model_df %>% 
#               mutate(subicb_ed_type3_act_sc = subicb_ed_type3_act_sc * 1.1) %>% 
#               mutate(scenario = "subicb_ed_type3_act_sc")) %>% 
#   bind_rows(model_df %>% 
#               mutate(subicb_comm_contacts_sc = subicb_comm_contacts_sc * 1.1) %>% 
#               mutate(scenario = "subicb_comm_contacts_sc")) |> 
#   bind_rows(model_df %>% 
#               mutate(prov_day_ed_type1_amb_act_sc = prov_day_ed_type1_amb_act_sc * 0.9) %>% 
#               mutate(scenario = "prov_day_ed_type1_amb_act_sc")) %>% 
#   bind_rows(model_df %>% 
#               mutate(prov_day_ed_type1_walkin_act_sc = prov_day_ed_type1_walkin_act_sc * 0.9) %>% 
#               mutate(scenario = "prov_day_ed_type1_walkin_act_sc")) %>% 
#   bind_rows(model_df %>% 
#               mutate(prov_hr_emer_occ_sc = prov_hr_emer_occ_sc * 0.9) %>% 
#               mutate(scenario = "prov_hr_emer_occ_sc")) %>% 
#   bind_rows(model_df %>% 
#               mutate(prov_hr_elec_occ_sc = prov_hr_elec_occ_sc * 0.9) %>% 
#               mutate(scenario = "prov_hr_elec_occ_sc")) %>% 
#   bind_rows(model_df %>% 
#               mutate(prov_day_op_proc_sc = prov_day_op_proc_sc * 0.9) %>% 
#               mutate(scenario = "prov_day_op_proc_sc")) %>% 
#   bind_rows(model_df %>% 
#               mutate(prov_day_op_att_sc = prov_day_op_att_sc * 0.9) %>% 
#               mutate(scenario = "prov_day_op_att_sc")) %>% 
#   bind_rows(model_df %>% 
#               mutate(prov_day_ct_scan_sc = prov_day_ct_scan_sc * 0.9) %>% 
#               mutate(scenario = "prov_day_ct_scan_sc")) %>% 
#   bind_rows(model_df %>% 
#               mutate(prov_day_mri_scan_sc = prov_day_mri_scan_sc * 0.9) %>% 
#               mutate(scenario = "prov_day_mri_scan_sc")) %>% 
#   bind_rows(model_df %>% 
#               mutate(prov_day_us_scan_sc = prov_day_us_scan_sc * 0.9) %>% 
#               mutate(scenario = "prov_day_us_scan_sc")) %>% 
#   bind_rows(model_df %>% 
#               mutate(prov_day_xray_scan_sc = prov_day_xray_scan_sc * 0.9) %>% 
#               mutate(scenario = "prov_day_xray_scan_sc")) %>% 
#   bind_rows(model_df %>% 
#               mutate(rnd = runif(nrow(model_df))) |> 
#               mutate(n_img_tests = ifelse(rnd < sum(model_df$n_img_tests) / nrow(model_df[model_df$n_img_tests != 0, ]) / 10
#                                           & n_img_tests > 0, 
#                                           n_img_tests - 1, n_img_tests)) %>% 
#               mutate(scenario = "n_img_tests") |> 
#               dplyr::select(-rnd)) %>% 
#   bind_rows(model_df %>% 
#               mutate(rnd = runif(nrow(model_df))) |> 
#               mutate(n_haem_tests = ifelse(rnd < sum(model_df$n_haem_tests) / nrow(model_df[model_df$n_haem_tests != 0, ]) / 10
#                                            & n_haem_tests > 0, 
#                                            n_haem_tests - 1, n_haem_tests)) %>% 
#               mutate(scenario = "n_haem_tests") |> 
#               dplyr::select(-rnd)) %>% 
#   bind_rows(model_df %>% 
#               mutate(rnd = runif(nrow(model_df))) |> 
#               mutate(n_biochem_tests = ifelse(rnd < sum(model_df$n_biochem_tests) / nrow(model_df[model_df$n_biochem_tests != 0, ]) / 10
#                                               & n_biochem_tests > 0, 
#                                               n_biochem_tests - 1, n_biochem_tests)) %>% 
#               mutate(scenario = "n_biochem_tests") |> 
#               dplyr::select(-rnd)) %>% 
#   bind_rows(model_df %>% 
#               mutate(rnd = runif(nrow(model_df))) |> 
#               mutate(n_other_tests = ifelse(rnd < sum(model_df$n_other_tests) / nrow(model_df[model_df$n_other_tests != 0, ]) / 10
#                                             & n_other_tests > 0, 
#                                             n_other_tests - 1, n_other_tests)) %>% 
#               mutate(scenario = "n_other_tests") |> 
#               dplyr::select(-rnd)) %>% 
#   bind_rows(model_df %>% 
#               mutate(rnd = runif(nrow(model_df))) |> 
#               mutate(n_trtmnts = ifelse(rnd < sum(model_df$n_trtmnts) / nrow(model_df[model_df$n_trtmnts != 0, ]) / 10
#                                         & n_trtmnts > 0, 
#                                         n_trtmnts - 1, n_trtmnts)) %>% 
#               mutate(scenario = "n_trtmnts") |> 
#               dplyr::select(-rnd)) %>% 
#   bind_rows(model_df %>% 
#               mutate(rnd = runif(nrow(model_df))) |> 
#               mutate(is_adm = ifelse(rnd < sum(model_df$is_adm) / nrow(model_df[model_df$is_adm != 0, ]) / 10
#                                      & is_adm > 0, 
#                                      is_adm - 1, is_adm)) %>% 
#               mutate(scenario = "is_adm") |> 
#               dplyr::select(-rnd)) %>% 
#   bind_rows(model_df %>% 
#               mutate(distort_4hr = distort_4hr * 1.1) %>% 
#               mutate(scenario = "distort_4hr")) %>% 
#   mutate(strike = if_else(nurses_strike == 1 |
#                             paramedics_strike == 1 |
#                             jr_docs_only_strike == 1 |
#                             consultants_only_strike == 1 |
#                             jr_docs_and_cons_strike == 1, 1, 0))
# 
# 
# mean_marginal_fully_mediated_effects <-
#   fully_mediated_marginal_df %>% 
#   mutate(pred_dur_ed = predict(fully_mediated_outcome_model,
#                                newdata = fully_mediated_marginal_df,
#                                type = "response")) %>% 
#   group_by(scenario, strike) %>% 
#   summarise(mean_pred_dur_ed = mean(pred_dur_ed)) 
# 
# 
# base_dur <- mean_marginal_fully_mediated_effects %>% 
#   filter(strike == 0) %>% 
#   filter(scenario == "no change") %>% 
#   dplyr::select(mean_pred_dur_ed) %>% 
#   ungroup() %>% 
#   pull()
# 
# mean_marginal_fully_mediated_effects %>% 
#   filter(strike == 0)  %>% 
#   mutate(diff_dur = mean_pred_dur_ed - base_dur) %>%
#   mutate(p_diff_dur = diff_dur / base_dur) %>% 
#   arrange(p_diff_dur) |> 
#   print(n = 30)

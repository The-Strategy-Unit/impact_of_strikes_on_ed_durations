#README

# quick test of whether sc or z score form are better for modeling 
# continuous group level mediators

# results - very little in it - prefer by theory

# set up ----

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


model_df_cut <- readRDS(here("data", "model_df_cut_200k.RDS"))

n_sims <- 100

p <- "jr_docs_strike"

m_sc <- "prov_hr_emer_occ_sc"
  
m_z <- "prov_hr_emer_occ_z"



# visualise the two vars
model_df_cut |> 
  ggplot() +
  geom_point(aes(x = prov_hr_emer_occ_sc, 
             y = prov_hr_emer_occ_z,
             colour = procode))


# sc model
out_formula <- as.formula(
  gsub("\n", "", 
       paste("Surv(duration_ed) ~ age + sex + imd_dec + 
                     arr_mode + acuity_desc + chief_comp_grp +
                     is_winter + is_weekend + mid_arr_dep_dttm_rd_hr_fct + 
                     bank_holiday + 
                     mid_arr_dep_dttm_year_fct +
                     nurses_strike + paramedics_strike + 
                     jr_docs_strike + consultants_strike + 
                     jr_docs_strike:consultants_strike + ",
             m_sc,
             sep ="")))

med_formula <- as.formula(
  gsub("\n", "", 
       paste(m_sc, 
             " ~ age + sex + imd_dec + 
                         arr_mode + acuity_desc + chief_comp_grp + 
                         is_winter + is_weekend + mid_arr_dep_dttm_rd_hr_fct + 
                         bank_holiday + 
                         mid_arr_dep_dttm_year_fct +
                         nurses_strike + paramedics_strike + 
                         jr_docs_strike + consultants_strike + 
                         jr_docs_strike:consultants_strike + 
                         (1 | procode)",
             sep = "")))

out_mod_sc <- survreg(out_formula, 
                   cluster = procode, 
                   dist = "loglogistic",
                   data = model_df_cut)

med_mod_sc <- lmer(med_formula, 
                data = model_df_cut)

med_out_sc <- mediate(model.m = med_mod_sc, 
                   model.y = out_mod_sc, 
                   treat = p, 
                   mediator = m_sc, 
                   sims = n_sims)




# z model
out_formula <- as.formula(
  gsub("\n", "", 
       paste("Surv(duration_ed) ~ age + sex + imd_dec + 
                     arr_mode + acuity_desc + chief_comp_grp +
                     is_winter + is_weekend + mid_arr_dep_dttm_rd_hr_fct + 
                     bank_holiday + 
                     mid_arr_dep_dttm_year_fct +
                     nurses_strike + paramedics_strike + 
                     jr_docs_strike + consultants_strike + 
                     jr_docs_strike:consultants_strike + ",
             m_z,
             sep ="")))

med_formula <- as.formula(
  gsub("\n", "", 
       paste(m_z, 
             " ~ age + sex + imd_dec + 
                         arr_mode + acuity_desc + chief_comp_grp + 
                         is_winter + is_weekend + mid_arr_dep_dttm_rd_hr_fct + 
                         bank_holiday + 
                         mid_arr_dep_dttm_year_fct +
                         nurses_strike + paramedics_strike + 
                         jr_docs_strike + consultants_strike + 
                         jr_docs_strike:consultants_strike + 
                         (1 | procode)",
             sep = "")))

out_mod_z <- survreg(out_formula, 
                      cluster = procode, 
                      dist = "loglogistic",
                      data = model_df_cut)

med_mod_z <- lmer(med_formula, 
                   data = model_df_cut)

med_out_z <- mediate(model.m = med_mod_z, 
                      model.y = out_mod_z, 
                      treat = p, 
                      mediator = m_z, 
                      sims = n_sims)





# compare
summary(med_out_sc)
summary(med_out_z)

AIC(out_mod_sc) <= AIC(out_mod_z)
AIC(med_mod_sc) <= AIC(med_mod_z)

BIC(out_mod_sc) <= BIC(out_mod_z)
BIC(med_mod_sc) <= BIC(med_mod_z)

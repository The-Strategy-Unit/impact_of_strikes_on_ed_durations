#README

# use the multimed function to check whether any evidence of
# causal interaction between specific mediators

# function only works for lm models

# test where most plausible

# 1 prov_day_ed_type1_walkin_act_sc >> prov_day_ed_type1_amb_act_sc
# 2 is_adm >> prov_hr_emer_occ_sc 
# 3 n_img_tests >> is_adm

# result: minimal impact on average ACME (i.e. change within 95% CI)


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


model_df_cut <- readRDS(here("data", "model_df_cut_20k.RDS")) |> 
  filter(sex != "NA") |> 
  as.data.frame()
  




outcome <- "duration_ed"

exposure = "jr_docs_strike"

n_sims <- 100


# test where most plausible

# 1 prov_day_ed_type1_walkin_act_sc >> prov_day_ed_type1_amb_act_sc
# 2 is_adm >> prov_hr_emer_occ_sc 
# 3 n_img_tests >> is_adm

med_main <- "prov_hr_emer_occ_sc"

med_alt <- "is_adm"




Xnames <- c("age", "sex", "imd_dec",
            "arr_mode", "acuity_desc", "chief_comp_grp", 
            "is_winter", "is_weekend", "mid_arr_dep_dttm_rd_hr_fct",
            "bank_holiday",
            "mid_arr_dep_dttm_year_fct",
            "nurses_strike",
            "paramedics_strike",
            "consultants_strike",
            "procode")



out_mod_base_formula <- as.formula(paste(outcome, " ~ ", paste(Xnames, collapse = " + "), " + ", med_main, " + ", exposure ))

med_mod_base_formula <- as.formula(paste(med_main, " ~ ", paste(Xnames, collapse = " + "), " + ", exposure ))

out_mod_base <- lm(out_mod_base_formula,
                     data = model_df_cut)

med_mod_base <- lm(med_mod_base_formula,
                     data = model_df_cut)

med_out_base <- mediate(model.m = med_mod_base, 
                        model.y = out_mod_base, 
                        treat = exposure, 
                        mediator = med_main, 
                        sims = n_sims)

summary(med_out_base)



med_interaction <- multimed(outcome = outcome, 
                     med.main = med_main, 
                     med.alt = med_alt, 
                     treat = exposure,
                     covariates = Xnames,
                     data = model_df_cut, 
                     sims = n_sims)

summary(med_interaction)





# plot(med, type = "point")
# plot(med, type=c("sigma","R2-total"),tgroup=c("treated","control"))



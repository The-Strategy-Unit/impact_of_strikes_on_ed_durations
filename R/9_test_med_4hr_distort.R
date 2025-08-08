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


model_df_cut <- readRDS(here("data", "model_df_cut_200k.RDS"))

n_sims <- 100
mcontinuous <- "distort_4hr"
p <- "jr_docs_strike"





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
             mcontinuous,
             sep ="")))

med_formula <- as.formula(
  gsub("\n", "", 
       paste(mcontinuous, 
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

print(paste("start",Sys.time(), p, mcontinuous, "guassian", sep = " - "))

out_mod <- survreg(out_formula, 
                   cluster = procode, 
                   dist = "loglogistic",
                   data = model_df_cut)

med_mod <- lmer(med_formula, 
                data = model_df_cut)

med_out <- mediate(model.m = med_mod, 
                   model.y = out_mod, 
                   treat = p, 
                   mediator = mcontinuous, 
                   sims = n_sims)


summary(med_out)

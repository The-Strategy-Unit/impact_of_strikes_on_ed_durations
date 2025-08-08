#README

# use the medsens function to check whether any issues
# with the sequential ignorability assumption

# note that in theory there should be no problem with this assumption in our
# study design because there are no plausible causal influence of of model 
# covariates or our mediators on our treatments (strike y/n) 

# but nonetheless the literature suggests we should check how sensitive the 
# results are to violation of this assumption.

# function only works for lm models

# test on jr_docs_strike and prov_hr_emer_occ_sc mediator
# where the mediation effect is clearest

# yes - our results are sensitive to violation of the sequential ignorability
# assumption

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


# read in data ----

model_df_cut <- readRDS(here("data", "model_df_cut_20k.RDS")) |> 
  as.data.frame()


# set up for models ----


outcome <- "duration_ed"

treatment <- "jr_docs_strike"

mediator <- "prov_hr_emer_occ_sc"

n_sims <- 100


out_formula <- as.formula(
  gsub("\n", "", 
       paste("duration_ed ~ age + sex + imd_dec + 
                        arr_mode + acuity_desc + chief_comp_grp + 
                        is_winter + is_weekend + is_night +
                        bank_holiday + 
                        mid_arr_dep_dttm_year_fct +
                        nurses_strike + paramedics_strike + 
                        jr_docs_strike + consultants_strike + ",
             mediator,
             sep ="")))

med_formula <- as.formula(
  gsub("\n", "", 
       paste(mediator, 
             " ~ age + sex + imd_dec + 
                arr_mode + acuity_desc + chief_comp_grp + 
                is_winter + is_weekend + is_night +
                bank_holiday + 
                mid_arr_dep_dttm_year_fct +
                nurses_strike + paramedics_strike + 
                jr_docs_strike + consultants_strike",
             sep = "")))

# run models ----

out_mod <- lm(out_formula, 
                data = model_df_cut)

med_mod <- lm(med_formula, 
                data = model_df_cut)

# run mediation analysis ----

med_out <- mediate(model.m = med_mod, 
                   model.y = out_mod, 
                   treat = treatment, 
                   mediator = mediator,
                   cluster = model_df_cut$procode,
                   sims = n_sims)


summary(med_out)

# run sensitivity analysis ----

medsens <- medsens(med_out, 
        rho.by = 0.1, 
        effect.type = "both", 
        # eps = .Machine$double.eps,
        eps = 0.000000001,
        sims = 10)


summary(medsens)
medsens
plot(medsens, sens.par = "rho")

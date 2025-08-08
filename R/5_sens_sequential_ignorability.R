#README

# use the medsens function to check whether any issues
# with the sequential ignorability assumption

# note that in theory there should be no problem with this assumption in our
# study design because there are no plausible causal influence of of model 
# covariates or our mediators on our treatments (strike y/n) 

# but nonetheless the literature suggests we should check how sensitive the 
# results are to violation of this assumption.

# function only works for lm models
# and very  slow if run on full model df - so run on subset of 200k records
# note that when run on sample of 20k records, the results are almost identical

# test on four combinations of exposure and mediator
# 1 jr_docs_strike and prov_hr_emer_occ_sc mediator
# 2 consultants_only_strike and n_biochem_tests
# 3 jr_docs_and_cons_strike and distort_4hr
# 4 paramedics_strike and prov_day_ed_type1_amb_act_sc


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

# model_df <- readRDS(here("data", "model_df_cut_50pc.RDS")) |>
#   filter(sex != "NA")

model_df <- readRDS(here("data", "model_df_cut_200k.RDS")) |>
  filter(sex != "NA")


# model_df <- readRDS(here("data", "model_df_cut_20k.RDS")) |> 
#   filter(sex != "NA")

# set up for models ----

outcome <- "duration_ed"

expsoure_mediator = data.frame(treatment = c("jr_docs_only_strike", "consultants_only_strike", "jr_docs_and_cons_strike", "paramedics_strike"),
                                mediator = c("prov_hr_emer_occ_sc", "n_biochem_tests", "distort_4hr", "prov_day_ed_type1_amb_act_sc"))

n_sims_med <- 200

n_sims_medsens <- 20

medsens_output <- data.frame(expsoure = character(),
                             mediator = character(),
                             Rho_at_which_ACME = numeric(),
                             Rexp2_MstarRexp2_Ystar = numeric(),
                             Rexp2_MtildeRexp2_Ytilde = numeric())


for (i in 1:nrow(expsoure_mediator)) {

  print(paste("models", expsoure_mediator[[1, 1]], expsoure_mediator[[1, 2]], "models", Sys.time(), sep = "-"))
  
  out_formula <- as.formula(
    gsub("\n", "", 
       paste("duration_ed ~ age + sex + imd_dec + 
                        arr_mode + acuity_desc + chief_comp_grp + 
                        mid_arr_dep_dttm_year_fct +
                        is_winter + is_weekend + is_night +
                        bank_holiday + 
                        jr_docs_only_strike + consultants_only_strike +
                        jr_docs_and_cons_strike +
                        nurses_strike + paramedics_strike + ",
             expsoure_mediator[[i, 2]],
             sep ="")))

  med_formula <- as.formula(
    gsub("\n", "", 
         paste(expsoure_mediator[[i, 2]], 
               " ~ age + sex + imd_dec + 
                          arr_mode + acuity_desc + chief_comp_grp + 
                          mid_arr_dep_dttm_year_fct +
                          is_winter + is_weekend + is_night +
                          bank_holiday + 
                          jr_docs_only_strike + consultants_only_strike +
                          jr_docs_and_cons_strike +
                          nurses_strike + paramedics_strike",
               sep = "")))
  
  # run models ----
  
  out_mod <- lm(out_formula, 
                  data = model_df)
  
  med_mod <- lm(med_formula, 
                  data = model_df)

  # run mediation analysis ----
  
  print(paste("mediation", expsoure_mediator[[1, 1]], expsoure_mediator[[1, 2]], "models", Sys.time(), sep = "-"))
  
  med_out <- mediate(model.m = med_mod, 
                     model.y = out_mod, 
                     treat = expsoure_mediator[[i, 1]], 
                     mediator = expsoure_mediator[[i, 2]],
                     cluster = model_df$procode,
                     sims = n_sims_med)


  summary(med_out)



  # run sensitivity analysis ----
  
  print(paste("sensistivity", expsoure_mediator[[1, 1]], expsoure_mediator[[1, 2]], "models", Sys.time(), sep = "-"))
  
  medsens <- medsens(med_out, 
          rho.by = 0.1, 
          effect.type = "indirect", 
          eps = 0.000000001,
          sims = n_sims_medsens)
  
  medsens_output <- medsens_output |> 
    bind_rows(data.frame(expsoure = expsoure_mediator[[i, 1]],
                               mediator = expsoure_mediator[[i, 2]],
                               Rho_at_which_ACME = medsens$err.cr.d,
                               Rexp2_MstarRexp2_Ystar = medsens$R2star.d.thresh,
                               Rexp2_MtildeRexp2_Ytilde = medsens$R2tilde.d.thresh))
  
  
  
  summary(medsens)

  medsens$err.cr.d
  medsens$R2star.d.thresh
  medsens$R2tilde.d.thresh
  
  plot(medsens, sens.par = "rho")


}

write.csv(medsens_output, here("manuscript", "table_s3.csv"))

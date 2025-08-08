# README

# This script checks whether the survival models work better for our
# unmediated model when:
# 2a we replace strike_sequence for strike (y/n)
# 3b we replace strike_intensity for strike (y/n)
# 3a we fit exponential rather than the (default) Weibull distribution
# 3b we fit lognormal rather than the (default) Weibull distribution
# 3c we fit log-logistic rather than the (default) Weibull distribution
# 4 we fit a cox-ph model rather than a aft (survreg) model 

# NOTE log-logistic strike y/n model performs best 

# Then we check 
# 5 distribution of residuals 
# 6 that the prop hazards assumption hols for key variables


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


# 1 load data ----

model_df_cut <- readRDS(here("data", "model_df_cut.RDS"))


# 2 base weibull strike y/n model ----
unmediated_model_base <- survreg(Surv(duration_ed) ~ age + sex + imd_dec + 
                          arr_mode + acuity_desc + chief_comp_grp +
                          is_winter + is_weekend + mid_arr_dep_dttm_rd_hr_fct + 
                          bank_holiday +
                          mid_arr_dep_dttm_year_fct +
                          nurses_strike + paramedics_strike + 
                          jr_docs_strike + consultants_strike + 
                          jr_docs_strike:consultants_strike,
                        cluster = procode,
                        data = model_df_cut)


# 2a weibull - strike sequence model ----

unmediated_model_seq <- survreg(Surv(duration_ed) ~ age + sex + imd_dec + 
                                   arr_mode + acuity_desc + chief_comp_grp +
                                   is_winter + is_weekend + mid_arr_dep_dttm_rd_hr_fct + 
                                   bank_holiday +
                                   mid_arr_dep_dttm_year_fct +
                                   nurses_strike_sequence + 
                                   paramedics_strike_sequence + 
                                   jr_docs_strike_sequence + 
                                   consultants_strike_sequence + 
                                   jr_docs_strike:consultants_strike,
                                 cluster = procode,
                                 data = model_df_cut)



# 2b weibull - strike intensity model ----

unmediated_model_intens <- survreg(Surv(duration_ed) ~ age + sex + imd_dec + 
                                  arr_mode + acuity_desc + chief_comp_grp +
                                  is_winter + is_weekend + mid_arr_dep_dttm_rd_hr_fct + 
                                  bank_holiday +
                                  mid_arr_dep_dttm_year_fct +
                                  nurses_strike_intensity + 
                                  paramedics_strike_intensity + 
                                  jr_docs_strike_intensity + 
                                  consultants_strike_intensity + 
                                  jr_docs_strike:consultants_strike,
                                cluster = procode,
                                data = model_df_cut)

c(AIC(unmediated_model_base), BIC(unmediated_model_base))
c(AIC(unmediated_model_seq), BIC(unmediated_model_seq))
c(AIC(unmediated_model_intens), BIC(unmediated_model_intens))

mod_coefs <- tidy(unmediated_model_base) |> 
  mutate(base_aft = exp(estimate),
         base_haz_ratio = exp(estimate * -1 / unmediated_model_base$scale),
         base_sig = ifelse(p.value < 0.05, 1, 0)) |> 
  dplyr::select(term, base_aft, base_haz_ratio, base_sig) |> 
  full_join(tidy(unmediated_model_seq) |> 
              mutate(seq_aft = exp(estimate),
                     seq_haz_ratio = exp(estimate * -1 / unmediated_model_seq$scale),
                     seq_sig = ifelse(p.value < 0.05, 1, 0)) |> 
              dplyr::select(term, seq_aft, seq_haz_ratio, seq_sig),
            join_by(term)) |> 
  full_join(tidy(unmediated_model_intens) |> 
              mutate(intens_aft = exp(estimate),
                     intens_haz_ratio = exp(estimate * -1 / unmediated_model_intens$scale),
                     intens_sig = ifelse(p.value < 0.05, 1, 0)) |> 
              dplyr::select(term, intens_aft, intens_haz_ratio, intens_sig),
            join_by(term))



# 3b exponential - strike y/n ----

unmediated_model_exp <- survreg(Surv(duration_ed) ~ age + sex + imd_dec + 
                                   arr_mode + acuity_desc + chief_comp_grp +
                                   is_winter + is_weekend + mid_arr_dep_dttm_rd_hr_fct + 
                                   bank_holiday +
                                   mid_arr_dep_dttm_year_fct +
                                   nurses_strike + paramedics_strike + 
                                   jr_docs_strike + consultants_strike + 
                                   jr_docs_strike:consultants_strike,
                                 cluster = procode,
                                 dist = "exponential",
                                 data = model_df_cut)


# 3b logNormal - strike y/n ----

unmediated_model_lognorm <- survreg(Surv(duration_ed) ~ age + sex + imd_dec + 
                                  arr_mode + acuity_desc + chief_comp_grp +
                                  is_winter + is_weekend + mid_arr_dep_dttm_rd_hr_fct + 
                                  bank_holiday +
                                  mid_arr_dep_dttm_year_fct +
                                  nurses_strike + paramedics_strike + 
                                  jr_docs_strike + consultants_strike + 
                                  jr_docs_strike:consultants_strike,
                                cluster = procode,
                                dist = "lognormal",
                                data = model_df_cut)

# 3c logLogistic - strike y/n ----

unmediated_model_loglog <- survreg(Surv(duration_ed) ~ age + sex + imd_dec + 
                                      arr_mode + acuity_desc + chief_comp_grp +
                                      is_winter + is_weekend + mid_arr_dep_dttm_rd_hr_fct + 
                                      bank_holiday +
                                      mid_arr_dep_dttm_year_fct +
                                      nurses_strike + paramedics_strike + 
                                      jr_docs_strike + consultants_strike + 
                                      jr_docs_strike:consultants_strike,
                                    cluster = procode,
                                    dist = "loglogistic",
                                    data = model_df_cut)


c(AIC(unmediated_model_base), BIC(unmediated_model_base))
c(AIC(unmediated_model_exp), BIC(unmediated_model_exp))
c(AIC(unmediated_model_lognorm), BIC(unmediated_model_lognorm))
c(AIC(unmediated_model_loglog), BIC(unmediated_model_loglog))

mod_coefs <- tidy(unmediated_model_base) |> 
  mutate(base_aft = exp(estimate),
         base_haz_ratio = exp(estimate * -1 / unmediated_model_base$scale),
         base_sig = ifelse(p.value < 0.05, 1, 0)) |> 
  dplyr::select(term, base_aft, base_haz_ratio, base_sig) |> 
  full_join(tidy(unmediated_model_exp) |> 
              mutate(exp_aft = exp(estimate),
                     exp_haz_ratio = exp(estimate * -1 / unmediated_model_exp$scale),
                     exp_sig = ifelse(p.value < 0.05, 1, 0)) |> 
              dplyr::select(term, exp_aft, exp_haz_ratio, exp_sig),
            join_by(term)) |> 
  full_join(tidy(unmediated_model_lognorm) |> 
              mutate(lognorm_aft = exp(estimate),
                     lognorm_haz_ratio = exp(estimate * -1 / unmediated_model_lognorm$scale),
                     lognorm_sig = ifelse(p.value < 0.05, 1, 0)) |> 
              dplyr::select(term, lognorm_aft, lognorm_haz_ratio, lognorm_sig),
            join_by(term)) |> 
  full_join(tidy(unmediated_model_loglog) |> 
              mutate(loglog_aft = exp(estimate),
                     loglog_haz_ratio = exp(estimate * -1 / unmediated_model_loglog$scale),
                     loglog_sig = ifelse(p.value < 0.05, 1, 0)) |> 
              dplyr::select(term, loglog_aft, loglog_haz_ratio, loglog_sig),
            join_by(term))


summary(unmediated_model_loglog)


# 4 cox-ph model ----

unmediated_model_coxph <- coxph(Surv(duration_ed) ~ age + sex + imd_dec + 
                                      arr_mode + acuity_desc + chief_comp_grp +
                                      is_winter + is_weekend + mid_arr_dep_dttm_rd_hr_fct + 
                                      bank_holiday +
                                      mid_arr_dep_dttm_year_fct +
                                      nurses_strike + paramedics_strike + 
                                      jr_docs_strike + consultants_strike + 
                                      jr_docs_strike:consultants_strike,
                                    cluster = procode,
                                    data = model_df_cut)

c(AIC(unmediated_model_base), BIC(unmediated_model_base))
c(AIC(unmediated_model_loglog), BIC(unmediated_model_loglog))
c(AIC(unmediated_model_coxph), BIC(unmediated_model_coxph))


# 5 check residuals ----
plot(residuals(unmediated_model_loglog, type = "deviance") )

ggplot() +
  geom_histogram(aes(x = residuals(unmediated_model_loglog, type = "deviance") ))

qqnorm(y = residuals(unmediated_model_loglog, type = "deviance"))
qqline(y = residuals(unmediated_model_loglog, type = "deviance"))

# fairly normally distributed, some skew, longer tail on left

# 6 check proportional hazards ----

# generally seems Ok

fit <- survfit(Surv(duration_ed) ~ acuity_desc, # try different model variable here 
               data = model_df_cut)

sfit <- summary(fit)

fit_df <- data.frame(n = sfit$n.risk,
                     surv = sfit$surv,
                     time = sfit$time,
                     cumhaz = sfit$cumhaz,
                     strata = sfit$strata)
fit_df |> 
  ggplot() +
  geom_line(aes(x = time, y = surv, group = strata))

fit_df |> 
  ggplot() +
  geom_line(aes(x = time, y = cumhaz, colour = strata))

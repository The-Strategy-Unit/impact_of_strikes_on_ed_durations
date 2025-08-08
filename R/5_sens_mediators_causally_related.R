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
library("broom.mixed")
library("mediation")
library("survival")


# read in data ----

model_df <- readRDS(here("data", "model_df_cut_50pc.RDS")) |>
  filter(sex != "NA") |>
  as.data.frame()


# model_df <- readRDS(here("data", "model_df_cut_20k.RDS")) |>
#   filter(sex != "NA") |>
#   as.data.frame()






outcome <- "duration_ed"

exposure <- "jr_docs_only_strike"


# test where most plausible

# 1 prov_day_ed_type1_walkin_act_sc >> prov_day_ed_type1_amb_act_sc
# 2 is_adm >> prov_hr_emer_occ_sc 
# 3 n_img_tests >> is_adm


med_main <- c("prov_day_ed_type1_walkin_act_sc", "is_adm", "n_img_tests")

med_alt <- c("prov_day_ed_type1_amb_act_sc", "prov_hr_emer_occ_sc", "is_adm")

Xnames <- c("age", "sex", "imd_dec",
            "arr_mode", "acuity_desc", "chief_comp_grp", 
            "is_winter", "is_weekend", "mid_arr_dep_dttm_rd_hr_fct",
            "bank_holiday",
            "mid_arr_dep_dttm_year_fct",
            "nurses_strike",
            "paramedics_strike",
            "consultants_only_strike",
            "jr_docs_and_cons_strike",
            "procode")

# n_sims <- 200
n_sims <- 100


sens_med_interaction_outputs <- data.frame(main_mediator = character(),
                                           alternative_mediator  = character(),
                                           scenario = character(),
                                           acme = numeric(),
                                           acme_lcl95 = numeric(),
                                           acme_ucl95 = numeric())


for (i in (1:3)) {
  
  print(paste(med_main[[i]], med_alt[[i]], Sys.time(), sep = "-"))

  
  out_mod_base_formula <- as.formula(paste(outcome, " ~ ", paste(Xnames, collapse = " + "), " + ", med_main[[i]], " + ", exposure ))
  
  med_mod_base_formula <- as.formula(paste(med_main[[i]], " ~ ", paste(Xnames, collapse = " + "), " + ", exposure ))
  
  out_mod_base <- lm(out_mod_base_formula,
                       data = model_df)
  
  med_mod_base <- lm(med_mod_base_formula,
                       data = model_df)
  
  med_out_base <- mediate(model.m = med_mod_base, 
                          model.y = out_mod_base, 
                          treat = exposure, 
                          mediator = med_main[[i]],
                          sims = n_sims)
  
  
  summary(med_out_base)
  
  med_out_base$d.avg
  med_out_base$d.avg.ci[[1]]
  med_out_base$d.avg.ci[[2]]                      
  
  sens_med_interaction_outputs <- sens_med_interaction_outputs |> 
    bind_rows(data.frame(main_mediator = med_main[[i]],
                         alternative_mediator  = med_alt[[i]],
                         scenario = "without causal interaction with second mediator",
                         acme = med_out_base$d.avg,
                         acme_lcl95 = med_out_base$d.avg.ci[[1]],
                         acme_ucl95 = med_out_base$d.avg.ci[[2]]))
  
  med_interaction <- multimed(outcome = outcome, 
                       med.main = med_main[[i]], 
                       med.alt = med_alt[[i]], 
                       treat = exposure, 
                       covariates = Xnames,
                       data = model_df, 
                       sims = n_sims)
  
  
  
  summary(med_interaction)
  
  med_interaction$d.ave.lb[[1]]
  med_interaction$d.ave.ci[[1,1]]
  med_interaction$d.ave.ci[[2,1]]
  
  
  sens_med_interaction_outputs <- sens_med_interaction_outputs |> 
    bind_rows(data.frame(main_mediator = med_main[[i]],
              alternative_mediator  = med_alt[[i]],
              scenario = "with causal interaction with second mediator",
              acme = med_interaction$d.ave.lb[[1]],
              acme_lcl95 = med_interaction$d.ave.ci[[1,1]],
              acme_ucl95 = med_interaction$d.ave.ci[[2,1]]))
}


saveRDS(sens_med_interaction_outputs, here("data", "sens_med_interaction_outputs.RDS"))
# sens_med_interaction_outputs <- readRDS(here("data", "sens_med_interaction_outputs.RDS"))

sens_med_interaction_outputs |> 
  mutate(y_pos = c(2.9, 3.1, 1.9, 2.1, 0.9, 1.1)) |> 
  mutate(y_label = paste("(1) ", main_mediator, " (2)", alternative_mediator, sep = "")) |> 
  ggplot() +
  geom_vline(aes(xintercept = 0), colour = "grey") +
  geom_segment(aes(x = acme_lcl95, 
                   xend = acme_ucl95, 
                   y = y_pos,
                   yend = y_pos,
                   colour = scenario)) +
  geom_point(aes(x = acme, 
                 y = y_pos, 
                 colour = scenario)) +
  scale_y_continuous(name = "",
                     breaks = 1:3,
                     labels = c("(1) # imagimg tests & (2) is admitted",
                                "(1) is admitted & (2) non-elective bed occ",
                                "(1) walk-in  & (2) amb-conveyed ED atts")) +
  scale_x_continuous(name = "average causal mediated effect of first mediator") +
  scale_colour_discrete(name = "") +
  theme(legend.position = "none") +
  labs(caption = "with (red) and without (blue) causal interaction with second mediator")

ggsave(filename = here("manuscript", "figure_s2.jpg"),
       device = "jpg",
       dpi = 300,
       units = "cm",
       width = 18,
       height = 18)

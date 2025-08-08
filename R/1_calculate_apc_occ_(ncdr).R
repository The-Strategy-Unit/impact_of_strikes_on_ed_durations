## README

## this script counts the number of inpatients in hospital for selected 
## providers, by admission methods for hour of each each day in the    
## study period.

## The script must be run in the NCDR data science environment.

library("gt")
library("DBI")
library("here") 
library("dplyr")
library("purrr") 
library("readr") 
library("tidyr")
library("dbplyr")
library("ggplot2") 
library("janitor")
library("stringr")
library("gtExtras")
library("lubridate")
library("VIM")


# 0. CONNECTIONS ----------------------------------------------------------

con_sandbox_su <- dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "PRODNHSESQL101",
  Database = "NHSE_Sandbox_StrategyUnit",
  Trusted_Connection = "True"
)
con_sus_plus <- dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "PRODNHSESQL101",
  Database = "NHSE_SUSPlus_Live",
  Trusted_Connection = "True"
)
con_reporting <- dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "PRODNHSESQL101",
  Database = "NHSE_SUSPlus_Reporting",
  Trusted_Connection = "True"
)


# 1. GET DATA FROM SQL ----------------------------------------------------------
# create df of provider adm and dis times plus other imputation variables
# for selected providers

model_provider_sample_df <- readRDS(here("data", "model_provider_sample_df.RDS"))

apc_times_imputation_df <- dbGetQuery(
  con_sus_plus,
  "
SELECT  
  apcs.Der_Financial_Year,
  LEFT(apcs.Der_Provider_Code, 3) AS procode,
  CASE
    WHEN LEFT(Admission_Method, 1) = '1' AND Patient_Classification = '1' THEN 'ElecOrd'
    WHEN LEFT(Admission_Method, 1) = '1' AND Patient_Classification = '2' THEN 'ElecDC'
    WHEN LEFT(Admission_Method, 1) = '1' AND Patient_Classification IN ('3', '4') THEN 'ElecRegDN'
    WHEN LEFT(Admission_Method, 1) = '2' THEN 'Emer'
    WHEN LEFT(Admission_Method, 1) = '3' THEN 'Mat'
    ELSE 'Other'
    END as Admission_Method_min,
  Admission_Date,
  Discharge_Date,
  der_spell_los,
  times.Admission_Time,
  times.Discharge_Time

FROM [NHSE_SUSPlus_Live].[dbo].[tbl_Data_SEM_APCS] apcs
  LEFT JOIN
  (
  SELECT DISTINCT
    APCS_Ident_min,
    Admission_Time,
    Discharge_Time
  FROM [NHSE_SUSPlus_Reporting].[Data].[PAT_Intermediate_Table_APC]
  ) times
  ON apcs.APCS_Ident = times.APCS_Ident_min

WHERE 1 = 1
  AND (Discharge_Date >= '2022-04-01' OR Discharge_Date is NULL)
  AND Admission_Date <= '2024-07-31'
	
"
) |>
  tibble() |> 
  inner_join(model_provider_sample_df,
             join_by(procode)) |> 
  # set null discharge times to some date well past 2024-07-31
  mutate(Discharge_Date = if_else(is.na(Discharge_Date), 
                                  ymd("2030-01-01"),
                                  Discharge_Date)) |> 
  mutate(Discharge_Time = if_else(is.na(Discharge_Date), 
                                  "12:00:00",
                                  Discharge_Time)) |>
  # don't care about discharge times if after 2024-07-31
  mutate(Discharge_Time = if_else(Discharge_Date > '2024-07-31' & is.na(Discharge_Time), 
                                  "12:00:00",
                                  Discharge_Time)) |>
  # infer admission date from discharge date and los
  mutate(Admission_Date = if_else(is.na(Admission_Date),
                                  Discharge_Date - days(der_spell_los),
                                  Admission_Date)) |> 
  mutate(admi_wkend = if_else(wday(Admission_Date, week_start = 1) <= 5, 0, 1),
         disc_wkend = if_else(wday(Discharge_Date, week_start = 1) <= 5, 0, 1)) |> 
  mutate(Admission_Date_days = as.integer(ymd(Admission_Date) - ymd("2022-04-01")),
         Discharge_Date_days = as.integer(ymd(Discharge_Date) - ymd("2022-04-01"))) |> 
  mutate(Admission_Time_hms = hms(Admission_Time),
         Discharge_Time_hms = hms(Discharge_Time)) |> 
  mutate(Admission_Time_sec = period_to_seconds(Admission_Time_hms),
         Discharge_Time_sec = period_to_seconds(Discharge_Time_hms)) |> 
  mutate(same_day = ifelse(der_spell_los == 0, 1, 0)) |>    
  mutate(at_dt_status =
           case_when(
             same_day == 1 &
               Admission_Time_sec > Discharge_Time_sec ~ "at_dt_invalid",
             is.na(Admission_Time) &
               !is.na(Discharge_Time) &
               Discharge_Time != "00:00:00" ~ "at_missing_only",
             is.na(Discharge_Time) &
               !is.na(Admission_Time) &
               Admission_Time != "00:00:00" ~ "dt_missing_only",
             is.na(Admission_Time) &
               is.na(Discharge_Time) ~ "at_dt_missing",
             Discharge_Time == "00:00:00" &
               Admission_Time == "00:00:00" ~ "at_dt_00",
             TRUE ~ "at_dt_complete")) |> 
  # set admission and discharge times to NA if invalid so they are picked up by imputation process
  mutate(Admission_Time_sec = ifelse(at_dt_status == "at_dt_invalid", NA, Admission_Time_sec),
         Discharge_Time_sec = ifelse(at_dt_status == "at_dt_invalid", NA, Discharge_Time_sec),
         Admission_Time_sec = ifelse(at_dt_status == "at_dt_00", NA, Admission_Time_sec),
         Discharge_Time_sec = ifelse(at_dt_status == "at_dt_00", NA, Discharge_Time_sec))

# 2. CHECK MISSINGNESS OF TIMES ----------------------------------------------------------

apc_times_imputation_df |> 
  group_by(Admission_Method_min) |> 
  summarise(n = n()) 


apc_times_imputation_df |> 
  group_by(at_dt_status, Admission_Method_min) |> 
  summarise(n = n()) |> 
  ungroup() |> 
  group_by(Admission_Method_min) |> 
  mutate(p = n / sum(n, na.rm = TRUE)) |> 
  select(-p) |> 
  pivot_wider(names_from = "at_dt_status", 
              values_from = "n",
              values_fill = 0)

apc_times_imputation_df |> 
  group_by(at_dt_status) |> 
  summarise(n = n()) |> 
  ungroup() |> 
  mutate(at_dt_valid = if_else(at_dt_status == "at_dt_complete", "yes", "no")) |> 
  group_by(at_dt_valid) |> 
  summarise(n = sum(n)) |> 
  mutate(p = n / sum(n, na.rm = TRUE))


apc_times_imputation_df |> 
  group_by(procode, at_dt_status) |> 
  summarise(n = n()) |> 
  ungroup() |> 
  mutate(at_dt_valid = if_else(at_dt_status == "at_dt_complete", "yes", "no")) |> 
  group_by(procode, at_dt_valid) |> 
  summarise(n = sum(n)) |> 
  mutate(p = n / sum(n, na.rm = TRUE)) |> 
  filter(at_dt_valid == "yes") |> 
  print(n = 21)

# apc_times_imputation_df |> 
#   filter(Admission_Method_min == "Emer") |> 
#   group_by(admi_wkend) |> 
#   summarise(n = n())
# 
# apc_times_imputation_df |> 
#   filter(Admission_Method_min == "Emer") |> 
#   group_by(disc_wkend) |> 
#   summarise(n = n())
  
# apc_times_imputation_df |>
#   filter(Admission_Method_min == "Emer") |>
#   group_by(los) |>
#   summarise(n = n()) |> 
#   arrange(-n)



# 3. IMPUTE MISSING TIMES  ----------------------------------------------------------
## impute using kNN


provider_list <- model_provider_sample_df |> 
  select(procode) |> 
  pull()

admi_meth_list <-
  apc_times_imputation_df |> 
  distinct(Admission_Method_min) |> 
  pull()

wdwe_list <- c(0, 1)



  
start_time  <- Sys.time()
  
set.seed(11)
  
apc_times_imputation_temp_df <- 
  apc_times_imputation_df |> 
  filter(1 == 0) 



for (i in provider_list) {
  
  for (j in admi_meth_list) { 
  
    for (k in wdwe_list) { 
  

      tmp_at_missing_only_nsd <- apc_times_imputation_df |> 
        filter(procode == i,
               Admission_Method_min == j,
               admi_wkend == k) |> 
        filter(same_day == 0) |> 
        filter(at_dt_status %in% c("at_missing_only", 
                                   "at_dt_complete")) |> 
        kNN(variable = "Admission_Time_sec",
            k = 1,
            dist_var = c("Admission_Date_days", "der_spell_los"),
            addRandom = TRUE) |> 
        filter(Admission_Time_sec_imp == TRUE) |> 
        select(-Admission_Time_sec_imp)
      
      
      tmp_at_missing_only_sd <- apc_times_imputation_df |> 
        filter(procode == i,
               Admission_Method_min == j,
               admi_wkend == k) |> 
        filter(same_day == 1) |> 
        filter(at_dt_status %in% c("at_missing_only", 
                                   "at_dt_complete")) |> 
        kNN(variable = "Admission_Time_sec",
            k = 1,
            dist_var = c("Admission_Date_days", "Discharge_Time_sec"),
            addRandom = TRUE) |> 
        filter(Admission_Time_sec_imp == TRUE) |> 
        select(-Admission_Time_sec_imp)    
      
      tmp_dt_missing_only_nsd <- apc_times_imputation_df |>
        filter(procode == i,
               Admission_Method_min == j,
               admi_wkend == k) |> 
        filter(same_day == 0) |> 
        filter(at_dt_status %in% c("dt_missing_only", 
                                   "at_dt_complete")) |> 
        kNN(variable = "Discharge_Time_sec",
            k = 1,
            dist_var = c("Discharge_Date_days", "der_spell_los"),
            addRandom = TRUE) |> 
        filter(Discharge_Time_sec_imp == TRUE) |> 
        select(-Discharge_Time_sec_imp)     
      
      tmp_dt_missing_only_sd <- apc_times_imputation_df |> 
        filter(procode == i,
               Admission_Method_min == j,
               admi_wkend == k) |> 
        filter(same_day == 1) |> 
        filter(at_dt_status %in% c("dt_missing_only", 
                                   "at_dt_complete")) |> 
        kNN(variable = "Discharge_Time_sec",
            k = 1,
            dist_var = c("Discharge_Date_days", "Admission_Time_sec"),
            addRandom = TRUE) |> 
        filter(Discharge_Time_sec_imp == TRUE) |> 
        select(-Discharge_Time_sec_imp)  
      
      tmp_at_dt_missing_nsd <- apc_times_imputation_df |> 
        filter(procode == i,
               Admission_Method_min == j,
               admi_wkend == k) |> 
        filter(same_day == 0) |> 
        filter(at_dt_status %in% c("at_dt_missing", 
                                   "at_dt_invalid",
                                   "at_dt_00",
                                   "at_dt_complete")) |> 
        kNN(variable = "Admission_Time_sec",
            k = 1,
            dist_var = c("Admission_Date_days", "der_spell_los"),
            addRandom = TRUE) |> 
        kNN(variable = "Discharge_Time_sec",
            k = 1,
            dist_var = c("Discharge_Date_days", "der_spell_los"),
            addRandom = TRUE) |> 
        filter(Admission_Time_sec_imp == TRUE | Discharge_Time_sec_imp == TRUE) |> 
        select(-Admission_Time_sec_imp, -Discharge_Time_sec_imp)
      
      
      tmp_at_dt_missing_sd <- apc_times_imputation_df |> 
        filter(procode == i,
               Admission_Method_min == j,
               admi_wkend == k) |> 
        filter(same_day == 1) |> 
        filter(at_dt_status %in% c("at_dt_missing", 
                                   "at_dt_invalid",
                                   "at_dt_00",
                                   "at_dt_complete")) |> 
        kNN(variable = "Admission_Time_sec",
            k = 1,
            dist_var = c("Admission_Date_days"),
            addRandom = TRUE) |> 
        kNN(variable = "Discharge_Time_sec",
            k = 1,
            dist_var = c("Admission_Time_sec"),
            addRandom = TRUE) |> 
        filter(Admission_Time_sec_imp == TRUE | Discharge_Time_sec_imp == TRUE) |> 
        select(-Admission_Time_sec_imp, -Discharge_Time_sec_imp)
      

        
      apc_times_imputation_temp_df <- 
        bind_rows(apc_times_imputation_temp_df, 
                  tmp_at_missing_only_sd,
                  tmp_at_missing_only_nsd,
                  tmp_dt_missing_only_sd,
                  tmp_dt_missing_only_nsd,
                  tmp_at_dt_missing_sd,
                  tmp_at_dt_missing_nsd) |> 
        mutate(Discharge_Time_sec = ifelse(is.na(Discharge_Date), 
                                           NA, 
                                           Discharge_Time_sec))
      
      rm(tmp_at_missing_only_sd,
         tmp_at_missing_only_nsd,
         tmp_dt_missing_only_sd,
         tmp_dt_missing_only_nsd,
         tmp_at_dt_missing_sd,
         tmp_at_dt_missing_nsd)
   
    }
    
  }
  
 }
  
anom <- apc_times_imputation_temp_df |> 
  filter(Admission_Date_days == Discharge_Date_days &
             Admission_Time_sec > Discharge_Time_sec) |> 
  select(at_dt_status, same_day,
           Admission_Date_days, Discharge_Date_days,
           Admission_Time_sec, Discharge_Time_sec) 
  
end_time <- Sys.time()
  
dur <- end_time - start_time
  


# 4. IMPUTATION CHECKS ----------------------------------------------------------

# check no admissions after discharges
apc_times_imputation_temp_df |> 
  filter(Admission_Date_days == Discharge_Date_days &
           Admission_Time_sec > Discharge_Time_sec) |> 
  select(at_dt_status, same_day,
         Admission_Date_days, Discharge_Date_days,
           Admission_Time_sec, Discharge_Time_sec) 

end_time <- Sys.time()

print(end_time - start_time)


# visual imputed values follow similar distribution as non-imputed values

admi_wkend_names = c(`0` = "weekend admission",
                     `1` = "weekday admission")

disc_wkend_names = c(`0` = "weekend discharge",
                     `1` = "weekday discharge")

same_day_names = c(`0` = "overnight spell",
                   `1` = "same day spell")


ggplot() +
  geom_density(data = subset(apc_times_imputation_df, !is.na(admi_wkend)),
               aes(x = Admission_Time_sec),
               colour = 'blue') +
  geom_density(data = subset(apc_times_imputation_temp_df, !is.na(admi_wkend)),
               aes(x = Admission_Time_sec),
               colour = 'red') +
  facet_grid(rows = vars(Admission_Method_min),
             cols = vars(same_day, admi_wkend),
             labeller = labeller(same_day = as_labeller(same_day_names),
                                 admi_wkend = as_labeller(admi_wkend_names))) +
  scale_x_continuous(name = "hour admission (blue known |red imputed)",
                     label = scales::label_number(scale = 1/3600),
                     breaks = c(0, 21600, 43200, 64800, 86400))
  
ggplot() +
  geom_density(data = subset(apc_times_imputation_df, !is.na(disc_wkend)),
               aes(x = Discharge_Time_sec),
               colour = 'blue') +
  geom_density(data = subset(apc_times_imputation_temp_df, !is.na(disc_wkend)),
               aes(x = Discharge_Time_sec),
               colour = 'red') +
  facet_grid(rows = vars(Admission_Method_min),
             cols = vars(same_day, disc_wkend),
             labeller = labeller(same_day = as_labeller(same_day_names),
                                 disc_wkend = as_labeller(disc_wkend_names))) +
  scale_x_continuous(name = "hour of discharge (blue known |red imputed)",
                     label = scales::label_number(scale = 1/3600),
                     breaks = c(0, 21600, 43200, 64800, 86400))


# 5. COMBINE KNOWN AND IMPUTED VALUES ----------------------------------------------------------
apc_times_imputation_df |> 
  filter(at_dt_status != "at_dt_complete") |> 
  group_by(at_dt_status) |> 
  summarise(n= n())

apc_times_imputation_temp_df |> 
  group_by(at_dt_status) |> 
  summarise(n= n())


apc_times_df <-
  apc_times_imputation_temp_df |> 
  mutate(Admission_Time_hms = seconds_to_period(Admission_Time_sec),
         Discharge_Time_hms = seconds_to_period(Discharge_Time_sec)) |>  
  bind_rows(apc_times_imputation_df |> 
  filter(at_dt_status == "at_dt_complete")) |> 
  mutate(Admission_datetime = Admission_Date + Admission_Time_hms,
         Discharge_datetime = Discharge_Date + Discharge_Time_hms) |> 
  select(procode, Admission_datetime, Discharge_datetime, Admission_Method_min, at_dt_status)
  
  





# 6 SAVE FILE ----


saveRDS(apc_times_df, here("data", "apc_times_df.RDS"))

# apc_times_df <- readRDS(here("data", "apc_times_df.RDS"))

rm(apc_times_imputation_df, apc_times_imputation_temp_df)

gc()


apc_times_df |> 
  filter(procode != "RJZ") |> # note - added at later stage to fit with extra removal of RJZ
  mutate(at_dt_valid = if_else(at_dt_status == "at_dt_complete", "yes", "no")) |> 
  group_by(at_dt_valid) |> 
  summarise(n = n()) |> 
  mutate(p = n / sum(n))


# 7 CALCULATE OCCUPANCY ----
# by provider hour and day - at half past the hour

start_datetime <- as_datetime("2022-04-01 00:30:00")
end_datetime <- as_datetime("2024-07-31 23:30:00")


clock <- data.frame(census_dttm = seq(start_datetime, end_datetime, by = "hours"))

apc_occ_df <-
  clock |> 
  crossing(model_provider_sample_df 
           |> select(procode)) |> 
  crossing(apc_times_df |> 
             distinct(Admission_Method_min)) |> 
  left_join(apc_times_df |> 
              mutate(pat = 1),
            join_by(procode,
                    Admission_Method_min,
                    census_dttm >= Admission_datetime,
                    census_dttm <= Discharge_datetime)) |> 
  mutate(pat = if_else(is.na(pat), 0, pat)) |> 
  group_by(census_dttm, procode, Admission_Method_min) |> 
  summarise(ip_occ = sum(pat)) |> 
  arrange(procode, Admission_Method_min, census_dttm) |> 
  mutate(year = year(census_dttm),
         month = month(census_dttm),
         day = day(census_dttm),
         hour = hour(census_dttm))



# visual checks
apc_occ_df |>
  group_by(census_dttm) |> 
  summarise(ip_occ = sum(ip_occ)) |> 
  ggplot() +
  geom_line(aes(x = census_dttm, y = ip_occ))

apc_occ_df |> 
  group_by(census_dttm, Admission_Method_min) |> 
  summarise(ip_occ = sum(ip_occ)) |> 
  ggplot() +
  geom_line(aes(x = census_dttm, y = ip_occ)) +
  facet_wrap(vars(Admission_Method_min))


apc_occ_df |> 
  group_by(census_dttm, procode) |> 
  summarise(ip_occ = sum(ip_occ)) |> 
  ggplot() +
  geom_line(aes(x = census_dttm, y = ip_occ)) +
  facet_wrap(vars(procode))




# 8 SAVE FILE ----


saveRDS(apc_occ_df, here("data", "apc_occ_df.RDS"))

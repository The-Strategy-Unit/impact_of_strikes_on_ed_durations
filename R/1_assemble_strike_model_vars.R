# This script assembles information about strikes for the model df.  
# File structure as follows;

# strikes_df
#   date
#   hour (start of hour)
#   staff_group
#   strike_intensity (number of staff on strike)
#   strike_sequence (order of strike)
#   google_trends_index (a proxy for public awareness)

# wide version of strikes_df also provided, pivoted by staff_group



# 0 load libraries ----

library("here") 
library("dplyr")
library("readr") 
library("tidyr")
library("ggplot2") 
library("stringr")
library("lubridate")
library("targets")


# 1 load data ----

strike_dts <- tar_read("strike_dts") 

staff_on_strike_df <- readr::read_csv(
  here("data_raw", "wforce_absence_bytype_20241126.csv"),
  na = c("", "NA", "NULL")
) |>
  filter(n != 0)

# the following file was obtained from google trends
# https://trends.google.com/trends/explore
# using search term - "nhs strike"
#  country - "United Kingdom" 
# period "4/1/2022 to 7/31/2024"
# categories - "all categories"
# "web search"
# "Numbers represent search interest relative to the highest point on the chart 
#  for the given region and time. A value of 100 is the peak popularity for the 
#  term. A value of 50 means that the term is half as popular. A score of 0 
#  means there was not enough data for this term."

strike_public_awareness_df <- readr::read_csv(
  here("data_raw", "google_trends_index_nhs_strike.csv"),
  skip = 3,
  col_names = c("week_starting", "google_trends_index"),
  col_types = list("D", "n"))

unusual_dates_df <- tar_read(unusual_dates_df)


# 2 assemble strike_df ----

strikes_df <- strike_dts |> 
  mutate(staff_group = case_when(strike_type == "nurses industrial action (hrs/24)" ~ 'nurses',
                                strike_type == "ambulance industrial action (hrs/24)" ~ 'paramedics',
                                strike_type == "jr doctors industrial action (hrs/24)" ~ 'jr_docs',
                                strike_type == "consultants industrial action (hrs/24)" ~ 'consultants',
                                TRUE ~ "check")) |> 
  mutate(strike_start_time = case_when(substr(strike_day_seq, 1, 1) == "1"  & strike_len == 1 ~ 0, 
                                substr(strike_day_seq, 1, 1) != "1" ~ 0, 
                                TRUE  ~ 24 * (1 - strike_len))) |> 
  mutate(strike_end_time = case_when(substr(strike_day_seq, 1, 1) == substr(strike_day_seq, 3, 3) & strike_len == 1 ~ 23, 
                              substr(strike_day_seq, 1, 1) != substr(strike_day_seq, 3, 3) ~ 23,
                              substr(strike_day_seq, 1, 1) == "1" ~ 23,
                              TRUE ~ 24  * strike_len)) |>  
left_join(staff_on_strike_df |> 
            select(detailed_staff_group = staff_type,
                   date = rdate,
                   n_staff = n) |> 
            mutate(staff_group = case_when(detailed_staff_group %in% c("Medical / Dental - Consultant") ~ "consultants",
                                           detailed_staff_group %in% c("Medical / Dental - In Training (e.g. Foundation Y1 & Y2, StRs (incl FTSTAs & LATs), SHOs, SpRs / SpTs / GPRs)") ~ "jr_docs",
                                           detailed_staff_group %in% c("Adult / General",
                                                                       "Children and Young People",
                                                                       "District / Community",
                                                                       "Health Visitors",
                                                                       "Learning disabilities",
                                                                       "Maternity Support Workers",
                                                                       "Mental health",
                                                                       "Midwives",
                                                                       "Nurse Associates",
                                                                       "Nursing auxiliary / Nursing assistant / Healthcare assistant (including Health / Clinical / Nursing Support Worker)",
                                                                       "Other Registered Nurses",
                                                                       "Student Nurses",
                                                                       "Trainee Nurse Associates") ~ "nurses",
                                           detailed_staff_group %in% c("Ambulance Care Assistant",
                                                                       "Ambulance Technician/Associate Practitioner",
                                                                       "Assistant Practitioner, Emergency/Urgent Care Support Worker", 
                                                                       "Consultant Paramedic, Advanced Paramedic, Specialist Paramedic",
                                                                       "Emergency Call Handlers, Emergency Medical Dispatchers",
                                                                       "Non-emergency Call Handlers, Non-emergency Medical Dispatchers",
                                                                       "Paramedic") ~ "paramedics",
                                           TRUE ~ "other")) |> 
            group_by(date, staff_group) |> 
            summarise(strike_intensity = sum(n_staff)),
          join_by(date, staff_group)) |> 
  mutate(strike_intensity = case_when(is.na(strike_intensity) &
                                          substr(strike_day_seq, 1, 1) != "1" &
                                          substr(strike_day_seq, 1, 1) == substr(strike_day_seq, 1, 1) ~ lag(strike_intensity, 1),
                                        is.na(strike_intensity) &
                                          substr(strike_day_seq, 1, 1) == "1" &
                                          substr(strike_day_seq, 1, 1) != substr(strike_day_seq, 3, 3) ~ lead(strike_intensity, 1),
                                        TRUE ~ strike_intensity)) |> 
  left_join(strike_public_awareness_df |> 
              mutate(date = week_starting + days(0)) |> 
              bind_rows(strike_public_awareness_df |> 
                          mutate(date = week_starting + days(1))) |> 
              bind_rows(strike_public_awareness_df |> 
                          mutate(date = week_starting + days(2))) |> 
              bind_rows(strike_public_awareness_df |> 
                          mutate(date = week_starting + days(3))) |> 
              bind_rows(strike_public_awareness_df |> 
                          mutate(date = week_starting + days(4))) |> 
              bind_rows(strike_public_awareness_df |> 
                          mutate(date = week_starting + days(5))) |> 
              bind_rows(strike_public_awareness_df |> 
                          mutate(date = week_starting + days(6))) |> 
              select(-week_starting), 
       join_by(date)) |> 
  select(date, 
         staff_group, 
         strike_start_time,
         strike_end_time,
         strike_intensity,
         strike_sequence = strike_number,
         google_trends_index) |> 
  mutate(reps = strike_end_time - strike_start_time + 1) |> 
  uncount(reps) |> 
  group_by(date, staff_group) |> 
  mutate(id = row_number()) |> 
  mutate(hour = strike_start_time  + id - 1) |> 
  select(-id, -strike_start_time, - strike_end_time )


strikes_wide_df <- strikes_df |>
  pivot_wider(names_from = "staff_group",
              values_from = c("strike_intensity", "strike_sequence"),
              values_fill = 0) |> 
  mutate(strike_nurses = if_else(strike_sequence_nurses == 0, 0, 1),
         strike_paramedics = if_else(strike_sequence_paramedics == 0, 0, 1),
         strike_jr_docs = if_else(strike_sequence_jr_docs == 0, 0, 1),
         strike_consultants = if_else(strike_sequence_consultants == 0, 0, 1)) |> 
  select(date, hour, google_trends_index, 
         strike_nurses, strike_sequence_nurses, strike_intensity_nurses,
         strike_paramedics, strike_sequence_paramedics, strike_intensity_paramedics,
         strike_jr_docs, strike_sequence_jr_docs, strike_intensity_jr_docs,
         strike_consultants, strike_sequence_consultants, strike_intensity_consultants)


# 3 save files

saveRDS(strikes_df, here("data", "strikes_df.RDS"))
saveRDS(strikes_wide_df, here("data", "strikes_wide_df.RDS"))



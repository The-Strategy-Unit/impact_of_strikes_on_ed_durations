# README
# This script creates a data frame containing unusual dates that that might
# impact on the descriptive time series analysis in this project. These include
# 1 bank holidays
# 2 periods immediately before of after industrial action
# 3 dates of cancelled or posponed strikes
# 4 periods covered by small scale strikes (e.g. not national or for small staff groups)
# 5 the COVID-19 pandemic lockdowns
# 6 periods effected by cyber attacks
# over the period since 1/1/2021

# set up ----
# library("lubridate")
# library("testthat")
# library("ggplot2")
# library("janitor")
# library("stringr")
# library("readxl")
# library("tibble")
# library("dplyr")
# library("purrr")
# library("readr")
# library("tidyr")
# library("here")
# all called by deps.R

assemble_unusual_dates <- function(strike_dates_df) {
  # 1 bank holidays
  # source: https://www.gov.uk/bank-holidays
  # download calendar (ics) file
  download.file(
    url = "https://www.gov.uk/bank-holidays/england-and-wales.ics",
    destfile = here("data_raw", "england-and-wales.ics")
  )

  bank_holidays <- read.delim(here("data_raw", "england-and-wales.ics")) |>
    filter(substr(BEGIN.VCALENDAR, 1, 8) == "DTSTART;") |>
    mutate(date_raw = substr(
      BEGIN.VCALENDAR,
      nchar(BEGIN.VCALENDAR) - 7,
      nchar(BEGIN.VCALENDAR)
    )) |>
    mutate(date = as_date(date_raw)) |>
    filter(year(date) >= 2021) |>
    filter(year(date) <= 2024) |>
    mutate(
      measure = "bank holiday",
      value = 1
    ) |>
    select(measure, date, value)

  # 2 periods immediately before of after industrial action
  # create tables of dates up to a week ahead of or after a strike date
  # use strikes_dates_df from _targets folder - input to function
  prior_strike_dates <- strike_dates_df |>
    select(date) |>
    mutate(date = as_date(date)) |>
    mutate(
      prior1 = date - days(1),
      prior2 = date - days(2)
    ) |>
    select(-date) |>
    pivot_longer(
      cols = 1:2,
      names_to = "measure",
      values_to = "date"
    ) |>
    mutate(
      measure = "before strike",
      value = 1
    ) |>
    dplyr::select(measure, date, value) |>
    distinct()

  post_strike_dates <- strike_dates_df |>
    select(date) |>
    mutate(date = as_date(date)) |>
    mutate(
      post1 = date + days(1),
      post2 = date + days(2)
    ) |>
    select(-date) |>
    pivot_longer(
      cols = 1:2,
      names_to = "measure",
      values_to = "date"
    ) |>
    mutate(
      measure = "after strike",
      value = 1
    ) |>
    select(measure, date, value) |>
    distinct()

  # 3 dates of cancelled or postponed strikes
  # The source of information used is
  # https://en.wikipedia.org/wiki/2022%E2%80%93present_National_Health_Service_strikes#:~:text=On%2010%20November%2C%20nurses%20and,UK%20cost%20of%20living%20crisis
  # or https://www.gmb.org.uk/news/ambulance-strikes-suspended-government-talks
  # or references listed in these article
  

  cancelled_strikes_dates <- tribble(
    ~measure, ~date, ~value,
    "cancelled ambulance strike", "2023-03-23", 1,
    "cancelled ambulance strike", "2022-12-28", 1,
    "cancelled ambulance strike", "2023-03-06", 1,
    "cancelled ambulance strike", "2023-03-06", 1,
    "nurse strike cut short", "2023-05-02", 1, # https://www.nursingtimes.net/workforce/rcn-strike-in-pictures-nurses-walk-out-for-third-time-02-05-2023/#:~:text=The%20strike%20was%20supposed%20to,six%2Dmonth%20strike%20ballot%20period.
  ) |>
    mutate(date = as_date(date))

  # 4 periods covered by small scale strikes
  # source  IA sITREPs
  # and https://www.bbc.co.uk/news/health-64229958
  
  
  small_scale_strikes_dates <- tribble(
    ~measure, ~date, ~value,
    "sub-national ambulance strike", "2023-01-24", 1,
    "physio strike", "2023-01-26", 1,
    "physio strike", "2023-02-09", 1,
    "strike GMB staff at Mersey Care", "2023-02-13", 1,
    "nurse strike Unite members some trusts", "2023-05-03", 1,
    "ambulance strike SEAS", "2023-05-09", 1,
    "radiographers strike", "2023-07-25", 1,
    "radiographers strike", "2023-07-26", 1,
    
    
  ) |>
    mutate(date = as_date(date)) 
  
  
  
  
  # 5 COVID lockdown periods
  # from 1/1/2021
  # sources: https://www.instituteforgovernment.org.uk/data-visualisation/timeline-coronavirus-lockdowns
  # and https://en.wikipedia.org/wiki/COVID-19_lockdown_in_the_United_Kingdom

  covid_lockdown_dates <- tibble(
    measure = "COVID-19 lockdown",
    date = seq(ymd("2021-01-04"), ymd("2021-05-17"), by = "1 day"),
    value = 1
  )

  # 6 periods effected by cyber attacks
  # sources:
  # https://www.england.nhs.uk/2024/06/synnovis-cyber-attack-statement-from-nhs-england/
  # https://www.england.nhs.uk/2017/05/cyber-attack-updated-statement/
  # https://www.digitalhealth.net/2022/10/mental-health-trusts-still-unable-to-access-patient-records-after-cyber-attack/
  # need to check for more dates
  # at present these are just the attack dates - not the effected dates

  cyber_attack_dates <- tibble(
    measure = "cyber attack",
    date = c("2024-06-03", "2024-05-15", "2022-08-04"),
    value = 1
  ) |>
    mutate(date = as_date(date))

  # X combine into single df
  unusual_dates_df <- bind_rows(
    bank_holidays,
    prior_strike_dates,
    post_strike_dates,
    cancelled_strikes_dates,
    small_scale_strikes_dates,
    covid_lockdown_dates,
    cyber_attack_dates
  ) |> distinct()
}

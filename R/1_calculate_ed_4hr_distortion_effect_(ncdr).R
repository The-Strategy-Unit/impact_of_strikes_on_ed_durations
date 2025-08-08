# README 

## This script calculates a measure of operational distortion associated with the 4 hr ED target ED activity 

## The metric compares the number of ED departures just before (10- mins) and just after (+10 mins) 4 hours 

## If there is no thresholds effect (i.e. operational practice not distorted to achieve the 4 hr target) 
## then we might expect these two quantities to be similar, with the former is slightly larger than the 
## latter because the frequency of departures tends to decline with time in ED.

## The metric can take values from 0 to 1, where the expected value without distortion would be expected 
## to fall just below 0.5.  

## If however, the value is large (i.e. approaching 1) then this would indicate more distortion.

## The metric is calculated by day for each type 1 ED provider.

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

model_provider_sample_df <- readRDS(here("data", "model_provider_sample_df.RDS"))

ed_type1_4hr_distortion_df <- dbGetQuery(
  con_sus_plus,
  "SELECT 
    LEFT(Der_Provider_Code, 3) AS procode,
    DATEPART(YEAR, Der_EC_Departure_Date_Time) AS year,
    DATEPART(MONTH, Der_EC_Departure_Date_Time) AS month,
    DATEPART(DAY, Der_EC_Departure_Date_Time) AS day,
    COUNT(*) AS distort_4hr_denom,
    CAST(SUM(CASE WHEN Der_EC_Duration > (60 * 4) - 10 AND Der_EC_Duration <= (60 * 4) THEN 1 ELSE 0 END) AS FLOAT) / COUNT(*) AS distort_4hr
    
    
    FROM NHSE_SUSPlus_Live.dbo.tbl_Data_SUS_EC
    
    WHERE 1=1
    AND EC_Department_Type IN ('01')
    AND Der_EC_Departure_Date_Time > '2022-03-31 23:59:59'
    AND Der_EC_Arrival_Date_Time <= '2024-08-01 00:00:00'
    AND Der_Dupe_Flag = 0
    AND Der_EC_Duration > (60 * 4) - 10
    AND Der_EC_Duration <= (60 * 4) + 10
    
    
    GROUP BY 
    LEFT(Der_Provider_Code, 3),
    DATEPART(YEAR, Der_EC_Departure_Date_Time),
    DATEPART(MONTH, Der_EC_Departure_Date_Time),
    DATEPART(DAY, Der_EC_Departure_Date_Time)
    
    order by 
    LEFT(Der_Provider_Code, 3),
    DATEPART(YEAR, Der_EC_Departure_Date_Time),
    DATEPART(MONTH, Der_EC_Departure_Date_Time),
    DATEPART(DAY, Der_EC_Departure_Date_Time)
  ")  |> 
  inner_join(model_provider_sample_df,
             join_by(procode))


# 2. SAVE FILES ----

saveRDS(ed_type1_4hr_distortion_df, here("data", 'ed_type1_4hr_distortion_df.RDS'))

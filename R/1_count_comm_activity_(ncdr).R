## README

## this script counts community services activity levels , by attendnace status and icb 
## for each day in the study period.

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
con_community <- dbConnect(
  odbc::odbc(),
  Driver = "SQL Server",
  Server = "PRODNHSESQL101",
  Database = "NHSE_DataLanding_Community",
  Trusted_Connection = "True"
)



# 1. GET DATA FROM SQL ----------------------------------------------------------

latest_mpi <- dbGetQuery(
  con_community,
  "SELECT person_id, 
        MAX(Effective_From) AS max_Effective_From
      
    FROM [NHSE_DataLanding_Community].[csds].[PublishCYP001MPI]
  
    WHERE OrgIDSubICBLocResidence IS NOT NULL
      
    GROUP BY person_id")


person_icb <- dbGetQuery(
  con_community,
  "SELECT DISTINCT
      person_id, 
      Effective_From,
      OrgIDSubICBLocResidence
        
    FROM [NHSE_DataLanding_Community].[csds].[PublishCYP001MPI]
  
  ")
  



unique_contacts_df <- dbGetQuery(
  con_community,
  "SELECT 
      Person_ID,
      Contact_Date as 'date',
      CASE
        WHEN AttendanceStatus IN ('5', '6') THEN 1
        ELSE 0
        END AS 'is_attendance'
  
  FROM
  
    (SELECT Person_ID,
        CareContactID,
        Contact_Date,
        CASE
          WHEN YEAR(Contact_Date) < 2023 THEN AttendOrNot
          ELSE AttendanceStatus
          END AS 'AttendanceStatus',

        ROW_NUMBER() OVER (
          PARTITION BY Person_ID,
          CareContactID,
          OrgID_Provider
          ORDER BY OrgID_Provider) AS row_number
        
      FROM NHSE_DataLanding_Community.CSDS.PublishCYP201CareContact
  
      WHERE Contact_Date >= '2022-04-01'
          AND Contact_Date <= '2024-07-31') contacts
  
  WHERE contacts.row_number = 1
  
  ")



gc()

latest_person_icb <- 
  latest_mpi |> 
  inner_join(person_icb,
             join_by(person_id == person_id,
                     max_Effective_From == Effective_From)) |>
  group_by(person_id) |> 
  summarise(icbSubLoc = last(OrgIDSubICBLocResidence))



comm_act_df <- unique_contacts_df |> 
  left_join(latest_person_icb,
            join_by(Person_ID == person_id)) |> 
  group_by(date, icbSubLoc, is_attendance) |> 
  summarise(comm_contacts = n()) |> 
  ungroup()




# 2. SAVE FILE ----

saveRDS(comm_act_df, here("data", 'comm_act_df.RDS'))
# comm_act_df <- readRDS(here("data", 'comm_act_df.RDS'))

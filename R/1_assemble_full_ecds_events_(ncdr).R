# README 

# This script extracts event level data from ECDS about A&E attendances for 
# a selected sample of providers

# The extract will form the base of the data frame for the hypothesis testing models.,
# and will be combined with other data about e.g. apc occupancy, gp appts, etc.

# This script must be run in the NCDR data science environment


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

# 1. DRAW DATA FROM SQL

ecds_events_full_df <- dbGetQuery(
  con_sus_plus,
        "
      -- README
      -- From record-level in EC dataset within NHSE_SUSPlus_Live db in NCDR. 
      -- Creates a table containing a subset of records and variables
      -- that may be useful for the modelling element of the industrial 
      -- action project. This table includes all providers. We will use 
      -- this table to assess provider data quality and then re-query this 
      -- table (with chosen providers) to create extract 1.
      
      SELECT 
      
      /* TIME-RELATED VARIABLES */
      
      	ec.Der_Financial_Year as fyear, 
          Der_EC_Arrival_Date_Time AS dttm_arr,
          Der_EC_Departure_Date_Time AS dttm_depart,
          CASE
              WHEN Der_EC_Duration < 0 THEN NULL -- 4320 mins = 72 hours = 3 days
              WHEN Der_EC_Duration > 4320 THEN NULL
              ELSE Der_EC_Duration
          END AS duration_ed,
      
      /* ARRIVAL/DISCHARGE VARIABLES */
          ref_attsrc.AttendanceSourceDescription AS att_source_desc,
          CASE
              WHEN ref_arr_mode.ArrivalModeKey IS NULL THEN NULL
              WHEN ref_arr_mode.ArrivalModeKey IN (3, 4, 5, 6, 9) THEN 'amb'
              WHEN ref_arr_mode.ArrivalModeKey IN (1, 2, 7, 8) THEN 'walk_in'
              WHEN ref_arr_mode.ArrivalModeKey IS NULL THEN 'walk_in'
              ELSE CAST(ref_arr_mode.ArrivalModeKey AS VARCHAR(2))
          END AS arr_mode,
          -- see ECDS_Group1 field, tab 26.4, ECDS_ETOS_v4.0.7
          CASE
              WHEN Discharge_Destination_SNOMED_CT = '306689006' THEN 'discharged'
              WHEN Discharge_Destination_SNOMED_CT = '306691003' THEN 'discharged'
              WHEN Discharge_Destination_SNOMED_CT = '306694006' THEN 'discharged'
              WHEN Discharge_Destination_SNOMED_CT = '306705005' THEN 'discharged'
              WHEN Discharge_Destination_SNOMED_CT = '50861005' THEN 'discharged'
              WHEN Discharge_Destination_SNOMED_CT = '1066331000000109' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066341000000100' THEN 'ambulatory'
              WHEN Discharge_Destination_SNOMED_CT = '1066351000000102' THEN 'ambulatory'
              WHEN Discharge_Destination_SNOMED_CT = '306706006' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1874161000000104' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066361000000104' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066371000000106' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066381000000108' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066391000000105' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066401000000108' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '19712007' THEN 'transfer'
              WHEN Discharge_Destination_SNOMED_CT = '183919006' THEN 'transfer'
              WHEN Discharge_Destination_SNOMED_CT = '305398007' THEN 'died'
              ELSE Discharge_Destination_SNOMED_CT
          END AS disdest_grp,
      
      
      /* CARE-RELATED (INVEST-DIAG-TREAT) VARIABLES */
      
          ref_acuity.AcuityID AS acuity, -- 5 ACUITY LEVELS
          ref_acuity.AcuityDescription AS acuity_desc,
          ec.EC_Chief_Complaint_SNOMED_CT AS chief_comp, -- 149 DISTINCT CHIEF COMPLAINTS
          ref_chief_comp.ChiefComplaintDescription AS chief_comp_desc,
          ref_chief_comp_grp.ChiefComplaintGrouping AS chief_comp_grp, -- 15 CHIEF COMPLAINT GROUPS
          Clinical_Chief_Complaint_Injury_Related AS inj_flag,
          Der_EC_Investigation_All AS invst_all,
      	Der_EC_Treatment_All AS treat_all,
         
      /* DEMOGRAPHIC VARIABLES */
      
          CASE
              WHEN Sex = '1' THEN 'm'
              WHEN Sex = '2' THEN 'f'
              WHEN Sex IN ('0', '9', 'X') THEN 'NA'
              ELSE Sex
          END AS sex,
          CASE
              WHEN Age_at_Arrival IS NULL THEN NULL
              WHEN Age_at_Arrival >= 120 THEN NULL
              ELSE CAST(Age_at_Arrival AS int)
          END AS age,
          -- CASE
          --     WHEN Age_at_Arrival < 10 THEN '00-09'
          --     WHEN Age_at_Arrival >= 10
          --     AND Age_at_Arrival < 20 THEN '10-19'
          --     WHEN Age_at_Arrival >= 20
          --     AND Age_at_Arrival < 30 THEN '20-29'
          --     WHEN Age_at_Arrival >= 30
          --     AND Age_at_Arrival < 40 THEN '30-39'
          --     WHEN Age_at_Arrival >= 40
          --     AND Age_at_Arrival < 50 THEN '40-49'
          --     WHEN Age_at_Arrival >= 50
          --     AND Age_at_Arrival < 60 THEN '50-59'
          --     WHEN Age_at_Arrival >= 60
          --     AND Age_at_Arrival < 70 THEN '60-69'
          --     WHEN Age_at_Arrival >= 70
          --     AND Age_at_Arrival < 80 THEN '70-79'
          --     WHEN Age_at_Arrival >= 80
          --     AND Age_at_Arrival < 90 THEN '80-89'
          --     WHEN Age_at_Arrival >= 90
          --     AND Age_at_Arrival < 110 THEN '90+'
          --     WHEN Age_At_Arrival >= 110 THEN 'NA'
          -- END AS age_grp,
          Index_Of_Multiple_Deprivation_Decile AS imd_dec,
          Ethnic_Category AS ethnic_grp,
      
      /* GEOGRAPHICAL VARIABLES */
      
          LEFT(Der_Provider_Code, 3) AS procode,
          Der_Postcode_CCG_Code AS sub_icb
      
      FROM NHSE_SUSPlus_Live.dbo.tbl_Data_SUS_EC ec 
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Arrival_Mode] ref_arr_mode ON ec.EC_Arrival_Mode_SNOMED_CT = ref_arr_mode.ArrivalModeCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Attendance_Source] ref_attsrc ON ec.EC_Attendance_Source_SNOMED_CT = ref_attsrc.AttendanceSourceCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Acuity] ref_acuity ON ec.EC_Acuity_SNOMED_CT = ref_acuity.AcuityCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Chief_Complaint] ref_chief_comp ON ec.EC_Chief_Complaint_SNOMED_CT = ref_chief_comp.ChiefComplaintCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Chief_Complaint_Group] ref_chief_comp_grp ON ec.EC_Chief_Complaint_SNOMED_CT = ref_chief_comp_grp.ChiefComplaintCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Discharge_Status] ref_dis_stat ON ec.EC_Discharge_Status_SNOMED_CT = ref_dis_stat.DischargeStatusCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Discharge_Destination] ref_dis_dest ON ec.Discharge_Destination_SNOMED_CT = ref_dis_dest.DischargeDestinationCode
      
      WHERE 
          EC_Department_Type IN ('01')
          AND -- ATTENDANCE CATEGORY IS AN UNPLANNED FIRST (NOT FOLLOW UP / UNKNOWN):
          EC_AttendanceCategory = '1'
          AND -- NOT BROUGHT IN DEAD OR DIED DURING ATTENDANCE:
            (
            NOT (Der_AEA_Patient_Group = '70' OR Discharge_Destination_SNOMED_CT = '305398007') -- died
            )
          AND -- DURING ATTENDANCE DID NOT LEAVE / UNKNOWN DISPOSAL / NOT STREAMED PATIENTS (GENERALLY)
            (
            DischargeStatusDescription = 'Treatment completed (situation)'
            OR DischargeStatusDescription = 'Streamed to emergency department following initial assessment (situation)'
            )
          AND Der_Dupe_Flag = 0
          AND LEFT(Der_Postcode_Dist_Unitary_Auth, 1) = 'E'
          AND LEFT(Der_Provider_Code, 1) = 'R'
          AND (Der_EC_Departure_Date_Time > '2022-07-31 23:59:59' OR Der_EC_Departure_Date_Time IS NULL)
          AND Der_EC_Arrival_Date_Time < '2024-08-01 00:00:00'
      ") 

gc()



# 3. SAVE FILE

saveRDS(ecds_events_full_df, here("data", "ecds_events_full_df"))


#  Before excluded cases (for manuscript)

ecds_events_full_before_exclusions_df <- dbGetQuery(
  con_sus_plus,
  "
      -- README
      -- From record-level in EC dataset within NHSE_SUSPlus_Live db in NCDR. 
      -- Creates a table containing a subset of records and variables
      -- that may be useful for the modelling element of the industrial 
      -- action project. This table includes all providers. We will use 
      -- this table to assess provider data quality and then re-query this 
      -- table (with chosen providers) to create extract 1.
      
      SELECT 
      
      /* TIME-RELATED VARIABLES */
      
      	ec.Der_Financial_Year as fyear, 
          Der_EC_Arrival_Date_Time AS dttm_arr,
          Der_EC_Departure_Date_Time AS dttm_depart,
          CASE
              WHEN Der_EC_Duration < 0 THEN NULL -- 4320 mins = 72 hours = 3 days
              WHEN Der_EC_Duration > 4320 THEN NULL
              ELSE Der_EC_Duration
          END AS duration_ed,
      
      /* ARRIVAL/DISCHARGE VARIABLES */
          ref_attsrc.AttendanceSourceDescription AS att_source_desc,
          CASE
              WHEN ref_arr_mode.ArrivalModeKey IS NULL THEN NULL
              WHEN ref_arr_mode.ArrivalModeKey IN (3, 4, 5, 6, 9) THEN 'amb'
              WHEN ref_arr_mode.ArrivalModeKey IN (1, 2, 7, 8) THEN 'walk_in'
              WHEN ref_arr_mode.ArrivalModeKey IS NULL THEN 'walk_in'
              ELSE CAST(ref_arr_mode.ArrivalModeKey AS VARCHAR(2))
          END AS arr_mode,
          -- see ECDS_Group1 field, tab 26.4, ECDS_ETOS_v4.0.7
          CASE
              WHEN Discharge_Destination_SNOMED_CT = '306689006' THEN 'discharged'
              WHEN Discharge_Destination_SNOMED_CT = '306691003' THEN 'discharged'
              WHEN Discharge_Destination_SNOMED_CT = '306694006' THEN 'discharged'
              WHEN Discharge_Destination_SNOMED_CT = '306705005' THEN 'discharged'
              WHEN Discharge_Destination_SNOMED_CT = '50861005' THEN 'discharged'
              WHEN Discharge_Destination_SNOMED_CT = '1066331000000109' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066341000000100' THEN 'ambulatory'
              WHEN Discharge_Destination_SNOMED_CT = '1066351000000102' THEN 'ambulatory'
              WHEN Discharge_Destination_SNOMED_CT = '306706006' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1874161000000104' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066361000000104' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066371000000106' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066381000000108' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066391000000105' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066401000000108' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '19712007' THEN 'transfer'
              WHEN Discharge_Destination_SNOMED_CT = '183919006' THEN 'transfer'
              WHEN Discharge_Destination_SNOMED_CT = '305398007' THEN 'died'
              ELSE Discharge_Destination_SNOMED_CT
          END AS disdest_grp,
      
      
      /* CARE-RELATED (INVEST-DIAG-TREAT) VARIABLES */
      
          ref_acuity.AcuityID AS acuity, -- 5 ACUITY LEVELS
          ref_acuity.AcuityDescription AS acuity_desc,
          ec.EC_Chief_Complaint_SNOMED_CT AS chief_comp, -- 149 DISTINCT CHIEF COMPLAINTS
          ref_chief_comp.ChiefComplaintDescription AS chief_comp_desc,
          ref_chief_comp_grp.ChiefComplaintGrouping AS chief_comp_grp, -- 15 CHIEF COMPLAINT GROUPS
          Clinical_Chief_Complaint_Injury_Related AS inj_flag,
          Der_EC_Investigation_All AS invst_all,
      	Der_EC_Treatment_All AS treat_all,
         
      /* DEMOGRAPHIC VARIABLES */
      
          CASE
              WHEN Sex = '1' THEN 'm'
              WHEN Sex = '2' THEN 'f'
              WHEN Sex IN ('0', '9', 'X') THEN 'NA'
              ELSE Sex
          END AS sex,
          CASE
              WHEN Age_at_Arrival IS NULL THEN NULL
              WHEN Age_at_Arrival >= 120 THEN NULL
              ELSE CAST(Age_at_Arrival AS int)
          END AS age,
          -- CASE
          --     WHEN Age_at_Arrival < 10 THEN '00-09'
          --     WHEN Age_at_Arrival >= 10
          --     AND Age_at_Arrival < 20 THEN '10-19'
          --     WHEN Age_at_Arrival >= 20
          --     AND Age_at_Arrival < 30 THEN '20-29'
          --     WHEN Age_at_Arrival >= 30
          --     AND Age_at_Arrival < 40 THEN '30-39'
          --     WHEN Age_at_Arrival >= 40
          --     AND Age_at_Arrival < 50 THEN '40-49'
          --     WHEN Age_at_Arrival >= 50
          --     AND Age_at_Arrival < 60 THEN '50-59'
          --     WHEN Age_at_Arrival >= 60
          --     AND Age_at_Arrival < 70 THEN '60-69'
          --     WHEN Age_at_Arrival >= 70
          --     AND Age_at_Arrival < 80 THEN '70-79'
          --     WHEN Age_at_Arrival >= 80
          --     AND Age_at_Arrival < 90 THEN '80-89'
          --     WHEN Age_at_Arrival >= 90
          --     AND Age_at_Arrival < 110 THEN '90+'
          --     WHEN Age_At_Arrival >= 110 THEN 'NA'
          -- END AS age_grp,
          Index_Of_Multiple_Deprivation_Decile AS imd_dec,
          Ethnic_Category AS ethnic_grp,
      
      /* GEOGRAPHICAL VARIABLES */
      
          LEFT(Der_Provider_Code, 3) AS procode,
          Der_Postcode_CCG_Code AS sub_icb
      
      FROM NHSE_SUSPlus_Live.dbo.tbl_Data_SUS_EC ec 
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Arrival_Mode] ref_arr_mode ON ec.EC_Arrival_Mode_SNOMED_CT = ref_arr_mode.ArrivalModeCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Attendance_Source] ref_attsrc ON ec.EC_Attendance_Source_SNOMED_CT = ref_attsrc.AttendanceSourceCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Acuity] ref_acuity ON ec.EC_Acuity_SNOMED_CT = ref_acuity.AcuityCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Chief_Complaint] ref_chief_comp ON ec.EC_Chief_Complaint_SNOMED_CT = ref_chief_comp.ChiefComplaintCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Chief_Complaint_Group] ref_chief_comp_grp ON ec.EC_Chief_Complaint_SNOMED_CT = ref_chief_comp_grp.ChiefComplaintCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Discharge_Status] ref_dis_stat ON ec.EC_Discharge_Status_SNOMED_CT = ref_dis_stat.DischargeStatusCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Discharge_Destination] ref_dis_dest ON ec.Discharge_Destination_SNOMED_CT = ref_dis_dest.DischargeDestinationCode
      
      WHERE 
          EC_Department_Type IN ('01')
          --AND -- ATTENDANCE CATEGORY IS AN UNPLANNED FIRST (NOT FOLLOW UP / UNKNOWN):
          --EC_AttendanceCategory = '1'
          --AND -- NOT BROUGHT IN DEAD OR DIED DURING ATTENDANCE:
          --  (
          --  NOT (Der_AEA_Patient_Group = '70' OR Discharge_Destination_SNOMED_CT = '305398007') -- died
          --  )
          --AND -- DURING ATTENDANCE DID NOT LEAVE / UNKNOWN DISPOSAL / NOT STREAMED PATIENTS (GENERALLY)
          --  (
          --  DischargeStatusDescription = 'Treatment completed (situation)'
          --  OR DischargeStatusDescription = 'Streamed to emergency department following initial assessment (situation)'
          --  )
          AND Der_Dupe_Flag = 0
          --AND LEFT(Der_Postcode_Dist_Unitary_Auth, 1) = 'E'
          AND LEFT(Der_Provider_Code, 1) = 'R'
          AND (Der_EC_Departure_Date_Time > '2022-07-31 23:59:59' OR Der_EC_Departure_Date_Time IS NULL)
          AND Der_EC_Arrival_Date_Time < '2024-08-01 00:00:00'
      ") 

gc()

saveRDS(ecds_events_full_before_exclusions_df, here("data", "ecds_events_full_before_exclusions_df.RDS"))


# counting other specific exclusions
# resident outside England

ecds_events_full_outside_England_df <- dbGetQuery(
  con_sus_plus,
  "
      -- README
      -- From record-level in EC dataset within NHSE_SUSPlus_Live db in NCDR. 
      -- Creates a table containing a subset of records and variables
      -- that may be useful for the modelling element of the industrial 
      -- action project. This table includes all providers. We will use 
      -- this table to assess provider data quality and then re-query this 
      -- table (with chosen providers) to create extract 1.
      
      SELECT 
      
      /* TIME-RELATED VARIABLES */
      
      	ec.Der_Financial_Year as fyear, 
          Der_EC_Arrival_Date_Time AS dttm_arr,
          Der_EC_Departure_Date_Time AS dttm_depart,
          CASE
              WHEN Der_EC_Duration < 0 THEN NULL -- 4320 mins = 72 hours = 3 days
              WHEN Der_EC_Duration > 4320 THEN NULL
              ELSE Der_EC_Duration
          END AS duration_ed,
      
      /* ARRIVAL/DISCHARGE VARIABLES */
          ref_attsrc.AttendanceSourceDescription AS att_source_desc,
          CASE
              WHEN ref_arr_mode.ArrivalModeKey IS NULL THEN NULL
              WHEN ref_arr_mode.ArrivalModeKey IN (3, 4, 5, 6, 9) THEN 'amb'
              WHEN ref_arr_mode.ArrivalModeKey IN (1, 2, 7, 8) THEN 'walk_in'
              WHEN ref_arr_mode.ArrivalModeKey IS NULL THEN 'walk_in'
              ELSE CAST(ref_arr_mode.ArrivalModeKey AS VARCHAR(2))
          END AS arr_mode,
          -- see ECDS_Group1 field, tab 26.4, ECDS_ETOS_v4.0.7
          CASE
              WHEN Discharge_Destination_SNOMED_CT = '306689006' THEN 'discharged'
              WHEN Discharge_Destination_SNOMED_CT = '306691003' THEN 'discharged'
              WHEN Discharge_Destination_SNOMED_CT = '306694006' THEN 'discharged'
              WHEN Discharge_Destination_SNOMED_CT = '306705005' THEN 'discharged'
              WHEN Discharge_Destination_SNOMED_CT = '50861005' THEN 'discharged'
              WHEN Discharge_Destination_SNOMED_CT = '1066331000000109' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066341000000100' THEN 'ambulatory'
              WHEN Discharge_Destination_SNOMED_CT = '1066351000000102' THEN 'ambulatory'
              WHEN Discharge_Destination_SNOMED_CT = '306706006' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1874161000000104' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066361000000104' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066371000000106' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066381000000108' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066391000000105' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066401000000108' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '19712007' THEN 'transfer'
              WHEN Discharge_Destination_SNOMED_CT = '183919006' THEN 'transfer'
              WHEN Discharge_Destination_SNOMED_CT = '305398007' THEN 'died'
              ELSE Discharge_Destination_SNOMED_CT
          END AS disdest_grp,
      
      
      /* CARE-RELATED (INVEST-DIAG-TREAT) VARIABLES */
      
          ref_acuity.AcuityID AS acuity, -- 5 ACUITY LEVELS
          ref_acuity.AcuityDescription AS acuity_desc,
          ec.EC_Chief_Complaint_SNOMED_CT AS chief_comp, -- 149 DISTINCT CHIEF COMPLAINTS
          ref_chief_comp.ChiefComplaintDescription AS chief_comp_desc,
          ref_chief_comp_grp.ChiefComplaintGrouping AS chief_comp_grp, -- 15 CHIEF COMPLAINT GROUPS
          Clinical_Chief_Complaint_Injury_Related AS inj_flag,
          Der_EC_Investigation_All AS invst_all,
      	Der_EC_Treatment_All AS treat_all,
         
      /* DEMOGRAPHIC VARIABLES */
      
          CASE
              WHEN Sex = '1' THEN 'm'
              WHEN Sex = '2' THEN 'f'
              WHEN Sex IN ('0', '9', 'X') THEN 'NA'
              ELSE Sex
          END AS sex,
          CASE
              WHEN Age_at_Arrival IS NULL THEN NULL
              WHEN Age_at_Arrival >= 120 THEN NULL
              ELSE CAST(Age_at_Arrival AS int)
          END AS age,
          -- CASE
          --     WHEN Age_at_Arrival < 10 THEN '00-09'
          --     WHEN Age_at_Arrival >= 10
          --     AND Age_at_Arrival < 20 THEN '10-19'
          --     WHEN Age_at_Arrival >= 20
          --     AND Age_at_Arrival < 30 THEN '20-29'
          --     WHEN Age_at_Arrival >= 30
          --     AND Age_at_Arrival < 40 THEN '30-39'
          --     WHEN Age_at_Arrival >= 40
          --     AND Age_at_Arrival < 50 THEN '40-49'
          --     WHEN Age_at_Arrival >= 50
          --     AND Age_at_Arrival < 60 THEN '50-59'
          --     WHEN Age_at_Arrival >= 60
          --     AND Age_at_Arrival < 70 THEN '60-69'
          --     WHEN Age_at_Arrival >= 70
          --     AND Age_at_Arrival < 80 THEN '70-79'
          --     WHEN Age_at_Arrival >= 80
          --     AND Age_at_Arrival < 90 THEN '80-89'
          --     WHEN Age_at_Arrival >= 90
          --     AND Age_at_Arrival < 110 THEN '90+'
          --     WHEN Age_At_Arrival >= 110 THEN 'NA'
          -- END AS age_grp,
          Index_Of_Multiple_Deprivation_Decile AS imd_dec,
          Ethnic_Category AS ethnic_grp,
      
      /* GEOGRAPHICAL VARIABLES */
      
          LEFT(Der_Provider_Code, 3) AS procode,
          Der_Postcode_CCG_Code AS sub_icb
      
      FROM NHSE_SUSPlus_Live.dbo.tbl_Data_SUS_EC ec 
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Arrival_Mode] ref_arr_mode ON ec.EC_Arrival_Mode_SNOMED_CT = ref_arr_mode.ArrivalModeCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Attendance_Source] ref_attsrc ON ec.EC_Attendance_Source_SNOMED_CT = ref_attsrc.AttendanceSourceCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Acuity] ref_acuity ON ec.EC_Acuity_SNOMED_CT = ref_acuity.AcuityCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Chief_Complaint] ref_chief_comp ON ec.EC_Chief_Complaint_SNOMED_CT = ref_chief_comp.ChiefComplaintCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Chief_Complaint_Group] ref_chief_comp_grp ON ec.EC_Chief_Complaint_SNOMED_CT = ref_chief_comp_grp.ChiefComplaintCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Discharge_Status] ref_dis_stat ON ec.EC_Discharge_Status_SNOMED_CT = ref_dis_stat.DischargeStatusCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Discharge_Destination] ref_dis_dest ON ec.Discharge_Destination_SNOMED_CT = ref_dis_dest.DischargeDestinationCode
      
      WHERE 
          EC_Department_Type IN ('01')
          AND -- ATTENDANCE CATEGORY IS AN UNPLANNED FIRST (NOT FOLLOW UP / UNKNOWN):
          EC_AttendanceCategory = '1'
          AND -- NOT BROUGHT IN DEAD OR DIED DURING ATTENDANCE:
            (
            NOT (Der_AEA_Patient_Group = '70' OR Discharge_Destination_SNOMED_CT = '305398007') -- died
            )
          AND -- DURING ATTENDANCE DID NOT LEAVE / UNKNOWN DISPOSAL / NOT STREAMED PATIENTS (GENERALLY)
            (
            DischargeStatusDescription = 'Treatment completed (situation)'
            OR DischargeStatusDescription = 'Streamed to emergency department following initial assessment (situation)'
            )
          AND Der_Dupe_Flag = 0
          AND LEFT(Der_Postcode_Dist_Unitary_Auth, 1) <> 'E'
          AND LEFT(Der_Provider_Code, 1) = 'R'
          AND (Der_EC_Departure_Date_Time > '2022-07-31 23:59:59' OR Der_EC_Departure_Date_Time IS NULL)
          AND Der_EC_Arrival_Date_Time < '2024-08-01 00:00:00'
      ") 

gc()


# did or doa

ecds_events_full_did_doa_df <- dbGetQuery(
  con_sus_plus,
  "
      -- README
      -- From record-level in EC dataset within NHSE_SUSPlus_Live db in NCDR. 
      -- Creates a table containing a subset of records and variables
      -- that may be useful for the modelling element of the industrial 
      -- action project. This table includes all providers. We will use 
      -- this table to assess provider data quality and then re-query this 
      -- table (with chosen providers) to create extract 1.
      
      SELECT 
      
      /* TIME-RELATED VARIABLES */
      
      	ec.Der_Financial_Year as fyear, 
          Der_EC_Arrival_Date_Time AS dttm_arr,
          Der_EC_Departure_Date_Time AS dttm_depart,
          CASE
              WHEN Der_EC_Duration < 0 THEN NULL -- 4320 mins = 72 hours = 3 days
              WHEN Der_EC_Duration > 4320 THEN NULL
              ELSE Der_EC_Duration
          END AS duration_ed,
      
      /* ARRIVAL/DISCHARGE VARIABLES */
          ref_attsrc.AttendanceSourceDescription AS att_source_desc,
          CASE
              WHEN ref_arr_mode.ArrivalModeKey IS NULL THEN NULL
              WHEN ref_arr_mode.ArrivalModeKey IN (3, 4, 5, 6, 9) THEN 'amb'
              WHEN ref_arr_mode.ArrivalModeKey IN (1, 2, 7, 8) THEN 'walk_in'
              WHEN ref_arr_mode.ArrivalModeKey IS NULL THEN 'walk_in'
              ELSE CAST(ref_arr_mode.ArrivalModeKey AS VARCHAR(2))
          END AS arr_mode,
          -- see ECDS_Group1 field, tab 26.4, ECDS_ETOS_v4.0.7
          CASE
              WHEN Discharge_Destination_SNOMED_CT = '306689006' THEN 'discharged'
              WHEN Discharge_Destination_SNOMED_CT = '306691003' THEN 'discharged'
              WHEN Discharge_Destination_SNOMED_CT = '306694006' THEN 'discharged'
              WHEN Discharge_Destination_SNOMED_CT = '306705005' THEN 'discharged'
              WHEN Discharge_Destination_SNOMED_CT = '50861005' THEN 'discharged'
              WHEN Discharge_Destination_SNOMED_CT = '1066331000000109' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066341000000100' THEN 'ambulatory'
              WHEN Discharge_Destination_SNOMED_CT = '1066351000000102' THEN 'ambulatory'
              WHEN Discharge_Destination_SNOMED_CT = '306706006' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1874161000000104' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066361000000104' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066371000000106' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066381000000108' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066391000000105' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066401000000108' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '19712007' THEN 'transfer'
              WHEN Discharge_Destination_SNOMED_CT = '183919006' THEN 'transfer'
              WHEN Discharge_Destination_SNOMED_CT = '305398007' THEN 'died'
              ELSE Discharge_Destination_SNOMED_CT
          END AS disdest_grp,
      
      
      /* CARE-RELATED (INVEST-DIAG-TREAT) VARIABLES */
      
          ref_acuity.AcuityID AS acuity, -- 5 ACUITY LEVELS
          ref_acuity.AcuityDescription AS acuity_desc,
          ec.EC_Chief_Complaint_SNOMED_CT AS chief_comp, -- 149 DISTINCT CHIEF COMPLAINTS
          ref_chief_comp.ChiefComplaintDescription AS chief_comp_desc,
          ref_chief_comp_grp.ChiefComplaintGrouping AS chief_comp_grp, -- 15 CHIEF COMPLAINT GROUPS
          Clinical_Chief_Complaint_Injury_Related AS inj_flag,
          Der_EC_Investigation_All AS invst_all,
      	Der_EC_Treatment_All AS treat_all,
         
      /* DEMOGRAPHIC VARIABLES */
      
          CASE
              WHEN Sex = '1' THEN 'm'
              WHEN Sex = '2' THEN 'f'
              WHEN Sex IN ('0', '9', 'X') THEN 'NA'
              ELSE Sex
          END AS sex,
          CASE
              WHEN Age_at_Arrival IS NULL THEN NULL
              WHEN Age_at_Arrival >= 120 THEN NULL
              ELSE CAST(Age_at_Arrival AS int)
          END AS age,
          -- CASE
          --     WHEN Age_at_Arrival < 10 THEN '00-09'
          --     WHEN Age_at_Arrival >= 10
          --     AND Age_at_Arrival < 20 THEN '10-19'
          --     WHEN Age_at_Arrival >= 20
          --     AND Age_at_Arrival < 30 THEN '20-29'
          --     WHEN Age_at_Arrival >= 30
          --     AND Age_at_Arrival < 40 THEN '30-39'
          --     WHEN Age_at_Arrival >= 40
          --     AND Age_at_Arrival < 50 THEN '40-49'
          --     WHEN Age_at_Arrival >= 50
          --     AND Age_at_Arrival < 60 THEN '50-59'
          --     WHEN Age_at_Arrival >= 60
          --     AND Age_at_Arrival < 70 THEN '60-69'
          --     WHEN Age_at_Arrival >= 70
          --     AND Age_at_Arrival < 80 THEN '70-79'
          --     WHEN Age_at_Arrival >= 80
          --     AND Age_at_Arrival < 90 THEN '80-89'
          --     WHEN Age_at_Arrival >= 90
          --     AND Age_at_Arrival < 110 THEN '90+'
          --     WHEN Age_At_Arrival >= 110 THEN 'NA'
          -- END AS age_grp,
          Index_Of_Multiple_Deprivation_Decile AS imd_dec,
          Ethnic_Category AS ethnic_grp,
      
      /* GEOGRAPHICAL VARIABLES */
      
          LEFT(Der_Provider_Code, 3) AS procode,
          Der_Postcode_CCG_Code AS sub_icb
      
      FROM NHSE_SUSPlus_Live.dbo.tbl_Data_SUS_EC ec 
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Arrival_Mode] ref_arr_mode ON ec.EC_Arrival_Mode_SNOMED_CT = ref_arr_mode.ArrivalModeCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Attendance_Source] ref_attsrc ON ec.EC_Attendance_Source_SNOMED_CT = ref_attsrc.AttendanceSourceCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Acuity] ref_acuity ON ec.EC_Acuity_SNOMED_CT = ref_acuity.AcuityCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Chief_Complaint] ref_chief_comp ON ec.EC_Chief_Complaint_SNOMED_CT = ref_chief_comp.ChiefComplaintCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Chief_Complaint_Group] ref_chief_comp_grp ON ec.EC_Chief_Complaint_SNOMED_CT = ref_chief_comp_grp.ChiefComplaintCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Discharge_Status] ref_dis_stat ON ec.EC_Discharge_Status_SNOMED_CT = ref_dis_stat.DischargeStatusCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Discharge_Destination] ref_dis_dest ON ec.Discharge_Destination_SNOMED_CT = ref_dis_dest.DischargeDestinationCode
      
      WHERE 
          EC_Department_Type IN ('01')
          AND -- ATTENDANCE CATEGORY IS AN UNPLANNED FIRST (NOT FOLLOW UP / UNKNOWN):
          EC_AttendanceCategory = '1'
          AND -- NOT BROUGHT IN DEAD OR DIED DURING ATTENDANCE:
            (
            (Der_AEA_Patient_Group = '70' OR Discharge_Destination_SNOMED_CT = '305398007') -- died
            )
          AND -- DURING ATTENDANCE DID NOT LEAVE / UNKNOWN DISPOSAL / NOT STREAMED PATIENTS (GENERALLY)
            (
            DischargeStatusDescription = 'Treatment completed (situation)'
            OR DischargeStatusDescription = 'Streamed to emergency department following initial assessment (situation)'
            )
          AND Der_Dupe_Flag = 0
          AND LEFT(Der_Postcode_Dist_Unitary_Auth, 1) = 'E'
          AND LEFT(Der_Provider_Code, 1) = 'R'
          AND (Der_EC_Departure_Date_Time > '2022-07-31 23:59:59' OR Der_EC_Departure_Date_Time IS NULL)
          AND Der_EC_Arrival_Date_Time < '2024-08-01 00:00:00'
      ") 

gc()


# left before treatment

ecds_events_full_left_df <- dbGetQuery(
  con_sus_plus,
  "
      -- README
      -- From record-level in EC dataset within NHSE_SUSPlus_Live db in NCDR. 
      -- Creates a table containing a subset of records and variables
      -- that may be useful for the modelling element of the industrial 
      -- action project. This table includes all providers. We will use 
      -- this table to assess provider data quality and then re-query this 
      -- table (with chosen providers) to create extract 1.
      
      SELECT 
      
      /* TIME-RELATED VARIABLES */
      
      	ec.Der_Financial_Year as fyear, 
          Der_EC_Arrival_Date_Time AS dttm_arr,
          Der_EC_Departure_Date_Time AS dttm_depart,
          CASE
              WHEN Der_EC_Duration < 0 THEN NULL -- 4320 mins = 72 hours = 3 days
              WHEN Der_EC_Duration > 4320 THEN NULL
              ELSE Der_EC_Duration
          END AS duration_ed,
      
      /* ARRIVAL/DISCHARGE VARIABLES */
          ref_attsrc.AttendanceSourceDescription AS att_source_desc,
          CASE
              WHEN ref_arr_mode.ArrivalModeKey IS NULL THEN NULL
              WHEN ref_arr_mode.ArrivalModeKey IN (3, 4, 5, 6, 9) THEN 'amb'
              WHEN ref_arr_mode.ArrivalModeKey IN (1, 2, 7, 8) THEN 'walk_in'
              WHEN ref_arr_mode.ArrivalModeKey IS NULL THEN 'walk_in'
              ELSE CAST(ref_arr_mode.ArrivalModeKey AS VARCHAR(2))
          END AS arr_mode,
          -- see ECDS_Group1 field, tab 26.4, ECDS_ETOS_v4.0.7
          CASE
              WHEN Discharge_Destination_SNOMED_CT = '306689006' THEN 'discharged'
              WHEN Discharge_Destination_SNOMED_CT = '306691003' THEN 'discharged'
              WHEN Discharge_Destination_SNOMED_CT = '306694006' THEN 'discharged'
              WHEN Discharge_Destination_SNOMED_CT = '306705005' THEN 'discharged'
              WHEN Discharge_Destination_SNOMED_CT = '50861005' THEN 'discharged'
              WHEN Discharge_Destination_SNOMED_CT = '1066331000000109' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066341000000100' THEN 'ambulatory'
              WHEN Discharge_Destination_SNOMED_CT = '1066351000000102' THEN 'ambulatory'
              WHEN Discharge_Destination_SNOMED_CT = '306706006' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1874161000000104' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066361000000104' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066371000000106' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066381000000108' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066391000000105' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '1066401000000108' THEN 'admitted'
              WHEN Discharge_Destination_SNOMED_CT = '19712007' THEN 'transfer'
              WHEN Discharge_Destination_SNOMED_CT = '183919006' THEN 'transfer'
              WHEN Discharge_Destination_SNOMED_CT = '305398007' THEN 'died'
              ELSE Discharge_Destination_SNOMED_CT
          END AS disdest_grp,
      
      
      /* CARE-RELATED (INVEST-DIAG-TREAT) VARIABLES */
      
          ref_acuity.AcuityID AS acuity, -- 5 ACUITY LEVELS
          ref_acuity.AcuityDescription AS acuity_desc,
          ec.EC_Chief_Complaint_SNOMED_CT AS chief_comp, -- 149 DISTINCT CHIEF COMPLAINTS
          ref_chief_comp.ChiefComplaintDescription AS chief_comp_desc,
          ref_chief_comp_grp.ChiefComplaintGrouping AS chief_comp_grp, -- 15 CHIEF COMPLAINT GROUPS
          Clinical_Chief_Complaint_Injury_Related AS inj_flag,
          Der_EC_Investigation_All AS invst_all,
      	Der_EC_Treatment_All AS treat_all,
         
      /* DEMOGRAPHIC VARIABLES */
      
          CASE
              WHEN Sex = '1' THEN 'm'
              WHEN Sex = '2' THEN 'f'
              WHEN Sex IN ('0', '9', 'X') THEN 'NA'
              ELSE Sex
          END AS sex,
          CASE
              WHEN Age_at_Arrival IS NULL THEN NULL
              WHEN Age_at_Arrival >= 120 THEN NULL
              ELSE CAST(Age_at_Arrival AS int)
          END AS age,
          -- CASE
          --     WHEN Age_at_Arrival < 10 THEN '00-09'
          --     WHEN Age_at_Arrival >= 10
          --     AND Age_at_Arrival < 20 THEN '10-19'
          --     WHEN Age_at_Arrival >= 20
          --     AND Age_at_Arrival < 30 THEN '20-29'
          --     WHEN Age_at_Arrival >= 30
          --     AND Age_at_Arrival < 40 THEN '30-39'
          --     WHEN Age_at_Arrival >= 40
          --     AND Age_at_Arrival < 50 THEN '40-49'
          --     WHEN Age_at_Arrival >= 50
          --     AND Age_at_Arrival < 60 THEN '50-59'
          --     WHEN Age_at_Arrival >= 60
          --     AND Age_at_Arrival < 70 THEN '60-69'
          --     WHEN Age_at_Arrival >= 70
          --     AND Age_at_Arrival < 80 THEN '70-79'
          --     WHEN Age_at_Arrival >= 80
          --     AND Age_at_Arrival < 90 THEN '80-89'
          --     WHEN Age_at_Arrival >= 90
          --     AND Age_at_Arrival < 110 THEN '90+'
          --     WHEN Age_At_Arrival >= 110 THEN 'NA'
          -- END AS age_grp,
          Index_Of_Multiple_Deprivation_Decile AS imd_dec,
          Ethnic_Category AS ethnic_grp,
      
      /* GEOGRAPHICAL VARIABLES */
      
          LEFT(Der_Provider_Code, 3) AS procode,
          Der_Postcode_CCG_Code AS sub_icb
      
      FROM NHSE_SUSPlus_Live.dbo.tbl_Data_SUS_EC ec 
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Arrival_Mode] ref_arr_mode ON ec.EC_Arrival_Mode_SNOMED_CT = ref_arr_mode.ArrivalModeCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Attendance_Source] ref_attsrc ON ec.EC_Attendance_Source_SNOMED_CT = ref_attsrc.AttendanceSourceCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Acuity] ref_acuity ON ec.EC_Acuity_SNOMED_CT = ref_acuity.AcuityCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Chief_Complaint] ref_chief_comp ON ec.EC_Chief_Complaint_SNOMED_CT = ref_chief_comp.ChiefComplaintCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Chief_Complaint_Group] ref_chief_comp_grp ON ec.EC_Chief_Complaint_SNOMED_CT = ref_chief_comp_grp.ChiefComplaintCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Discharge_Status] ref_dis_stat ON ec.EC_Discharge_Status_SNOMED_CT = ref_dis_stat.DischargeStatusCode
          LEFT OUTER JOIN [NHSE_Reference].[dbo].[tbl_Ref_DataDic_ECDS_Discharge_Destination] ref_dis_dest ON ec.Discharge_Destination_SNOMED_CT = ref_dis_dest.DischargeDestinationCode
      
      WHERE 
          EC_Department_Type IN ('01')
          AND -- ATTENDANCE CATEGORY IS AN UNPLANNED FIRST (NOT FOLLOW UP / UNKNOWN):
          EC_AttendanceCategory = '1'
          AND -- NOT BROUGHT IN DEAD OR DIED DURING ATTENDANCE:
            (
            NOT (Der_AEA_Patient_Group = '70' OR Discharge_Destination_SNOMED_CT = '305398007') -- died
            )
          AND -- DURING ATTENDANCE DID NOT LEAVE / UNKNOWN DISPOSAL / NOT STREAMED PATIENTS (GENERALLY)
            (
            NOT (DischargeStatusDescription = 'Treatment completed (situation)'
            OR DischargeStatusDescription = 'Streamed to emergency department following initial assessment (situation)')
            )
          AND Der_Dupe_Flag = 0
          AND LEFT(Der_Postcode_Dist_Unitary_Auth, 1) = 'E'
          AND LEFT(Der_Provider_Code, 1) = 'R'
          AND (Der_EC_Departure_Date_Time > '2022-07-31 23:59:59' OR Der_EC_Departure_Date_Time IS NULL)
          AND Der_EC_Arrival_Date_Time < '2024-08-01 00:00:00'
      ") 

gc()



100 * nrow(ecds_events_full_outside_England_df) / (nrow(ecds_events_full_outside_England_df) + nrow(ecds_events_full_df))
100 * nrow(ecds_events_full_did_doa_df) / (nrow(ecds_events_full_did_doa_df) + nrow(ecds_events_full_df))
100 * nrow(ecds_events_full_left_df) / (nrow(ecds_events_full_left_df) + nrow(ecds_events_full_df))

# README

# This script counts diagnostic imaging data by POD, modality, and ICB, 
# for each day in the study period.

# This script should be run in the UDAL environment

library("odbc")
library("DBI")
library("tidyverse")
library("here")



con_udal <- dbConnect(
  odbc::odbc(),
  #UID = rstudioapi::askForPassword("swyatt@udal.nhs.uk"),
  Driver = "ODBC Driver 17 for SQL Server",
  Server = "udalsyndataprod.sql.azuresynapse.net",
  Database = "UDAL_Warehouse",
  Authentication = "ActiveDirectoryInteractive"
)

# 1. GET DATA FROM SQL --------

dids_act_df <- DBI::dbGetQuery(
  con_udal,
  "SELECT 
DidDate3 AS date,
IcProvOrgcode AS procode,
CASE
	WHEN IcModalityDesc = 'Plain radiography (procedure)' THEN 'xray'
	WHEN IcModalityDesc = 'Magnetic resonance imaging (procedure)' THEN 'mri'
	WHEN IcModalityDesc = 'Diagnostic ultrasonography (procedure)' THEN 'us'
	WHEN IcModalityDesc = 'Computerized axial tomography (procedure)' THEN 'ct'
	ELSE 'CHECK'
	END AS test_type,
CASE
	WHEN IcPatsourceDesc IN ('Admitted Patient Care - Day case (this Health Care Provider)', 
							 'Admitted Patient Care - Inpatient (this Health Care Provider)')  THEN 'IP'
	WHEN IcPatsourceDesc = 'Outpatient (this Health Care Provider)' THEN 'OP'
	WHEN IcPatsourceDesc = 'GP Direct Access' THEN 'GPDA'
	ELSE 'CHECK'
	END AS POD,
COUNT(*) as img

FROM [MESH_DIDS].[DIDS]

WHERE
DidDate3 >= '2022-04-01'
AND DidDate3 <= '2024-07-31'
AND IcModalityDesc IN ('Plain radiography (procedure)', 
						'Magnetic resonance imaging (procedure)', 
						'Diagnostic ultrasonography (procedure)', 
						'Computerized axial tomography (procedure)')
AND IcPatsourceDesc IN ('Admitted Patient Care - Day case (this Health Care Provider)', 
						'Admitted Patient Care - Inpatient (this Health Care Provider)', 
						'Outpatient (this Health Care Provider)', 
						'GP Direct Access')

GROUP BY 
DidDate3,
IcProvOrgcode,
CASE
	WHEN IcModalityDesc = 'Plain radiography (procedure)' THEN 'xray'
	WHEN IcModalityDesc = 'Magnetic resonance imaging (procedure)' THEN 'mri'
	WHEN IcModalityDesc = 'Diagnostic ultrasonography (procedure)' THEN 'us'
	WHEN IcModalityDesc = 'Computerized axial tomography (procedure)' THEN 'ct'
	ELSE 'CHECK'
	END,
CASE
	WHEN IcPatsourceDesc IN ('Admitted Patient Care - Day case (this Health Care Provider)', 
							 'Admitted Patient Care - Inpatient (this Health Care Provider)')  THEN 'IP'
	WHEN IcPatsourceDesc = 'Outpatient (this Health Care Provider)' THEN 'OP'
	WHEN IcPatsourceDesc = 'GP Direct Access' THEN 'GPDA'
	ELSE 'CHECK'
	END")

# 2. SAVE FILE -------

saveRDS(dids_act_df, here("data", "dids_act_df.RDS"))



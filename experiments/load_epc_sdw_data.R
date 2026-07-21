
# Load required libraries, install if missing
if (!requireNamespace("DBI", quietly = TRUE)) install.packages("DBI")
if (!requireNamespace("odbc", quietly = TRUE)) install.packages("odbc")
library(DBI)
library(odbc)

# The newest (best) available SQL driver available on the computer will be selected
# Ideally you should use an ODBC Driver (17 or 18), and DTS can help you install it
pick_sql_driver <- function() {
  drv_names <- odbc::odbcListDrivers()$name
  for (d in c("ODBC Driver 18 for SQL Server", "ODBC Driver 17 for SQL Server", "SQL Server")) {
    if (d %in% drv_names) return(d)
  }
  stop("No Microsoft SQL Server ODBC driver found. Install Driver 18 or 17.")
}

# connection details for the REF database on the SDW SQL server
connect_to_REF_db <- function(server = "az-z-prod-sql03.ecdcdmz.europa.eu", database = "REF") {
  drv <- pick_sql_driver()
  
  # Connection details (no encryption, The SDW server doesn't use it)
  args <- list(
    drv = odbc::odbc(),
    Driver = drv,
    Server = server,
    Database = database,
    Trusted_Connection = "Yes",
    Encrypt = "no"
  )
  
  # # Add MARS only for modern SQL Server ODBC drivers
  # if (drv %in% c("ODBC Driver 18 for SQL Server", "ODBC Driver 17 for SQL Server")) {
  #   args$MARS_Connection <- "Yes"
  # }
  
  do.call(DBI::dbConnect, args)
}

connection <- connect_to_REF_db()

# List of Subject Codes, Disease Codes, and Health Topic Codes
Subject_Disease_HealtTopic_sql_code <- "
SELECT SDHT.[SubjectCode]
      ,SDHT.[HealthTopicCode]
      ,SDHT.[DiseaseCode]
      ,SDHT.[DiseaseProgrammeCode]
  FROM [REF].[ref].[dSubjectTodDiseaseTodHealthTopic] SDHT 
  LEFT JOIN [REF].[ref].[dSubject] S ON S.SubjectCode = SDHT.SubjectCode
  WHERE S.Metadata = 1
  ORDER BY DiseaseProgrammeCode, DiseaseCode, HealthTopicCode;"

Subject_Disease_HealtTopic_data <- dbGetQuery(connection, Subject_Disease_HealtTopic_sql_code)



# Use @EpiPulseCasesFormat = 1 to get the data in the EPC Metadata format, 0 for SDW internal format.
# @SplitDates = 1 will create extra variables such as DateUsedForStatisticsMonth, DateUsedForStatisticsYear, etc.
# @DataFrom and @DataTo control the period for which data will be loaded
# @TopCount controls how much data will be retrieved; NULL means all data; @TopCount = 1000 means only the first (random) 1000 rows;
# @DataAsOf can take you back in time. Use NULL for the latest data. Use @DataAsOf = '2025-07-01' to load what was in the database on the first of July
MEAS_sql_code <- "
EXEC epipulse.GetRecordsDataDW
    @SubjectCode = N'MEAS',
    @DiseaseCode = N'MEAS',
    @DataFrom = '2024-01-01',
    @DataTo = '2025-01-01',
    @EpiPulseCasesFormat = 1,
    @SplitDates = 1,
    @TopCount = NULL;
"

MEAS_data <- dbGetQuery(connection, MEAS_sql_code)

CAMP_ISO_sql_code <- "
SET NOCOUNT ON
EXEC epipulse.GetRecordsDataDW
    @SubjectCode = N'CAMPISO',
    @DiseaseCode = N'CAMP',
    @HealthTopicCode = N'ISO',
    @DataFrom = '2024-01-01',
    @DataTo = '2025-01-01',
    @EpiPulseCasesFormat = 1,
    @SplitDates = 1,
    @TopCount = NULL;
"

CAMP_ISO_data <- dbGetQuery(connection, CAMP_ISO_sql_code)

CAMP_ISO_AST_sql_code <- "
EXEC epipulse.GetRecordsDataDW
    @SubjectCode = N'CAMPISO$AST',
    @DiseaseCode = N'CAMP',
    @HealthTopicCode = N'ISO',
    @DataFrom = '2024-01-01',
    @DataTo = '2025-01-01',
    @EpiPulseCasesFormat = 1,
    @SplitDates = 1,
    @TopCount = NULL;
"

CAMP_ISO_AST_data <- dbGetQuery(connection, CAMP_ISO_AST_sql_code)


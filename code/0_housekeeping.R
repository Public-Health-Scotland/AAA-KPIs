#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 0_houskeeping.R
# Karen Hotopp
# April 2023
# 
# Define housekeeping variables used by subsequent scripts
# 
# Written/run on R Posit PWB
# R version 4.1.2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Ubiquitous Variables
yymm <- 202303



## Ubiquitous Pathways
extract_path <- paste0("/PHI_conf/AAA/Topics/Screening/extracts/", yymm,
                       "/output/aaa_extract_", yymm, ".rds")

exclusions_path <- paste0("/PHI_conf/AAA/Topics/Screening/extracts/", yymm, 
                          "/output/aaa_exclusions_", yymm, ".rds")

temp_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", yymm,
                    "/temp")

gpd_lookups <- glue("/conf/linkage/output/lookups/Unicode")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 1_1_processing_for_KPI_11-13.R
cutoff_date <- dmy("31-03-1958")  ## How is this defined??


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2_2_kpi_1_1-1_3_uptake_coverage.R
year1_start <- dmy("01-04-1956")
year1_end <- dmy("31-03-1957")

year2_start <- dmy("01-04-1957")
year2_end <- dmy("31-03-1958")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 3_2_kpi_1_4_surveillance.R
# Define dates
prev_year <- "2020/21" # year preceding current_year
current_year <- "2021/22" # last year of full data (previous to current analysis)
current_year_start <- "2021-03-01" # 1st March of prev_year (date extract pulled)
next_year_start <- "2022-03-01" # 1st March of current_year (date extract pulled)
financial_year_due <- "2022/23" # current data being analyzed
financial_quarters <- c("2021/22_4", "2022/23_1","2022/23_2","2022/23_3")
last_date <- "2023-03-01" # current extract

###
## Data year runs from 1Mar to 28/29Feb for surveillance
## financial_year_due is the current KPI period 
##   Example: Running scripts for the data covering 1Apr22-31Mar23, 
##   financial_year_due = 2022/23
## current_year is the last full data year in financial year terms
##   Example: 2021/22 (1Apr21-31Mar22)
## current_year_start is the 1st March prior to the start of the current year
##   Example: when current_year = 2021/22, current_year_start = 2021-03-01
## financial_year_due is year due to be screened for 12-month surveillance
##   Example: when current_year = 2021/22, financial_year_due = 2022/23
## financial_quarters is year due to be screened for 3-month surveillance (runs Jan-Dec)
##   Example: when current_year = 2021/22, 
##   financial_quarters = 2021/22_4, 2022/23_1, 2022/23_2, 2022/23_3
## last_date is ****
##   Example: when current_year = 2021/22, last_date = 2023-04-01
###

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 6_2_Supplementary_Surveillance.R
# Define dates
next_year <- "2022/23"
cut_off_date <- "2023-03-31"













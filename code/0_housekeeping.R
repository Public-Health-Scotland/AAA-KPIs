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
yymm <- 202309
season <- "autumn"
# Years needed from the historical extract for current KPI report
kpi_report_years <- c("2020/21", "2021/22", "2022/23") 

# hbres_list
hb_list <- tibble(hbres = c("Scotland","Ayrshire & Arran","Borders",
                            "Dumfries & Galloway", "Fife", "Forth Valley", 
                            "Grampian", "Greater Glasgow & Clyde", "Highland", 
                            "Lanarkshire", "Lothian", "Orkney",
                            "Shetland", "Tayside","Western Isles"))


## Ubiquitous Pathways
extract_path <- paste0("/PHI_conf/AAA/Topics/Screening/extracts/", yymm,
                       "/output/aaa_extract_", yymm, ".rds")

exclusions_path <- paste0("/PHI_conf/AAA/Topics/Screening/extracts/", yymm, 
                          "/output/aaa_exclusions_", yymm, ".rds")

temp_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", yymm,
                    "/temp")

hist_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/historical")

gpd_lookups <- paste0("/conf/linkage/output/lookups/Unicode") ## useful or remove?

simd_path <- paste0("/conf/linkage/output/lookups/Unicode/Deprivation",
                    "/postcode_2023_1_simd2020v2.rds")

output_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", yymm,
                      "/output")

##!! Dates should only be updated once a year in May!! 
# May KPI report is a partial-year progress report of data that is reported in 
# the September KPI report as a full year.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 1_1_processing_for_KPI_11-13.R
# Individuals born after this date are not eligible for the program yet 
# (Have not turned 65 years old in time for this KPI round)
# This will match `year2_end` variable in next script
cutoff_date <- dmy("31-03-1958")  


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2_2_kpi_1_1-1_3_uptake_coverage.R
# Year 1 is the cohort currently being analyzed for reporting (complete), 
# while year 2 looks into the future to give a snapshot of progress for the  
# cohort reported on next cycle.
year1 <- "2022/23"
year2 <- "2023/24"

year1_start <- dmy("01-04-1956") # cohort year being analyzed
year1_end <- dmy("31-03-1957")

year2_start <- dmy("01-04-1957") # cohort year still active
year2_end <- dmy("31-03-1958")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3_2_kpi_1_4_surveillance.R & 4_2_kpi_1_4_surveillance_assess_recovery.R

# Dates for KPI 1.4 should only be updated once a year in the May report as this 
# is a partial report that is completed and published in the September report.

# # May Cut-Off Dates
# ## Cut off dates for the spring report generate a 10-month view to
# # account for missing data
# cut_off_12m <- "2022-01-31" # This is the end of the 10th month of the last
#          # financial year (i.e., if run for May 23, this is end of January 22)
# cut_off_3m <- "2022-10-31" # This is the end of the 10th month of the last
#          # calendar year (i.e., if run for May 23, this is end of October 22)


# September Cut-Off Dates
cut_off_12m <- "2022-03-31" # This is the end of the financial year 2 years ago
           # (i.e., if run for September 23, this is end of March 22)
cut_off_3m <- "2022-12-31" # This is the end of the last calendar year
           # (i.e., if run for September 23, this is end of December 22)


## September 2023
# Define dates
prev_year <- "2020/21"# year preceding current_year
current_year <- "2021/22" # last year of full data (previous to current analysis)
current_year_start <- "2021-03-01" # 1st March of prev_year 
next_year_start <- "2022-03-01" # 1st March of current_year 
financial_year_due <- "2022/23" # current data being analyzed
financial_quarters <- c("2021/22_4", "2022/23_1","2022/23_2","2022/23_3")
#last_date <- "2023-03-01" # current extract (May)
last_date <- "2023-09-01" # current extract (September)



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
## last_date is date of the extract (either May or September)
##   Example: when current_year = 2021/22, last_date = 2023-03-01
###


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 6_2_Supplementary_Surveillance.R
# Define dates
next_year <- "2022/23"
date_cut_off <- "2023-03-31"













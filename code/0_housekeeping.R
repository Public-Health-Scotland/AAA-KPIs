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

season <- "spring" # options are "spring" or "autumn"

# Years needed from the historical extract for current KPI report
# This includes the three most recent *complete* years of data (including 
# current year of analysis) and the year in current screening process)
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
cutoff_date <- "31-03-1958"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2_2_kpi_1_1-1_3_uptake_coverage.R
# Year 1 is the cohort currently being analyzed for reporting (complete), 
# while year 2 looks into the future to give a snapshot of progress for the  
# cohort reported on next cycle.
year1 <- "2022/23"
year2 <- "2023/24"

year1_start <- "01-04-1956" # cohort year being analyzed
year1_end <- "31-03-1957"

year2_start <- "01-04-1957" # cohort year still active
year2_end <- "31-03-1958"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# KPI_1_4.R

# Define dates
financial_year_due <- "2022/23" # current data being analyzed


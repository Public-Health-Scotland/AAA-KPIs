#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 0_houskeeping.R
# Karen Hotopp
# April 2023
# 
# Define housekeeping variables used by subsequent scripts
# 
# Written/run on R Posit WB
# R version 4.1.2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Ubiquitous Variables
yymm <- 202209

season <- "autumn" # options are "spring" or "autumn"

# Years needed from the historical extract for current KPI report
# This includes the three most recent *complete* years of data (including 
# current year of analysis) and the year in current screening process)
kpi_report_years <- c("2019/20", "2020/21", "2021/22") 

# hbres_list
hb_list <- tibble::tibble(hbres = c("Scotland","Ayrshire & Arran","Borders",
                                    "Dumfries & Galloway", "Fife", "Forth Valley", 
                                    "Grampian", "Greater Glasgow & Clyde", "Highland", 
                                    "Lanarkshire", "Lothian", "Orkney",
                                    "Shetland", "Tayside","Western Isles"))

# financial year list (list of FYs covered by program from start)
fy_list <- c("2012/13", "2013/14", "2014/15", "2015/16", "2016/17", "2017/18", 
             "2018/19", "2019/20", "2020/21", "2021/22", "2022/23")
fy_tibble <- tibble::tibble(financial_year = c(fy_list))


## Ubiquitous Pathways
extract_path <- paste0("/PHI_conf/AAA/Topics/Screening/extracts/", yymm,
                       "/output/aaa_extract_", yymm, ".rds")

exclusions_path <- paste0("/PHI_conf/AAA/Topics/Screening/extracts/", yymm, 
                          "/output/aaa_exclusions_", yymm, ".rds")

temp_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", yymm,
                    "/temp")

hist_path <- "/PHI_conf/AAA/Topics/Screening/KPI/historical"

simd_path <- paste0("/conf/linkage/output/lookups/Unicode/Deprivation",
                    "/postcode_2023_1_simd2020v2.rds")

output_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", yymm,
                      "/output")

##!! Dates should only be updated once a year in the spring!! 
# Spring KPI report is a partial-year progress report of data that is reported 
# in the autumn KPI report as a full year.


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 1_2_processing_for_KPI_11-13.R
# Individuals born after this date are not eligible for the program yet 
# (Have not turned 65 years old in time for this KPI round)
# This will match `year2_end` variable in next script
cutoff_date <- "31-03-1958"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2_2_kpi_1_1-1_3_uptake_coverage.R
# Year 1 is the cohort currently being analyzed for reporting (complete), 
# while year 2 looks into the future to give a snapshot of progress for the  
# cohort reported on next cycle.
year1 <- kpi_report_years[3]
year2 <- "2023/24"

year1_start <- "01-04-1956" # cohort year being analyzed
year1_end <- "31-03-1957"

year2_start <- "01-04-1957" # cohort year still active
year2_end <- "31-03-1958"


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# KPI_1_4.R
## Moved to KPI 1.4 script
# # Define dates
# financial_year_due <- kpi_report_years[3] # current data being analyzed


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4_3_kpi_2.R
# Dates of first and last financial year ##!! Just start and end dates of current FY?
start_date <- "2022-04-01"
end_date <- "2023-03-31" 

# Table 4 variables
finyear_minus_3 <- "2019/20" # Why does this year need to be segregated out? Not used for report.

end_current <- as.Date(end_date)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Theme 4 scripts
cut_off_date <- as.Date("2023-03-31") # How is this date defined?? 
#potential match: 6_2_Suppl_Surve.R










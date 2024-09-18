#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 0_houskeeping.R
# Karen Hotopp & Aoife McCarthy
# April 2024
# 
# Define housekeeping variables used by subsequent scripts
# 
# Written/run/revised on R Posit WB
# R version 4.1.2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(lubridate)

## Ubiquitous Variables~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### UPDATE THESE IN SPRING AND AUTUMN RUNS ###

# year (YYYY) and month (MM) at time of running
# e.g. autumn (usually september) YYYY09, spring (usually march) YYYY03
yymm <- 202403

season <- "spring" # options are "spring" or "autumn"

# date that most recent extract was downloaded
# format: date + month e.g. 1 September
extract_date <- "7 March"

# month in which QPMG is planned
qpmg_month <- "April"


### UPDATE THESE IN THE SPRING ONLY ###

# Years included in KPI report: 3 most recent financial years
# Should be the financial year ending in current calendar year, and the two years prior
# CHECK: final 2 digits of third FY should match those of yymm above
kpi_report_years <- c("2021/22", "2022/23", "2023/24") 

# financial year list (list of FYs covered by program from start)
# Need to add on FY ending in current calendar year
fy_list <- c("2012/13", "2013/14", "2014/15", "2015/16", "2016/17", "2017/18", 
             "2018/19", "2019/20", "2020/21", "2021/22", "2022/23", "2023/24")
fy_tibble <- tibble::tibble(financial_year = c(fy_list))





### ONLY UPDATE THESE IF FILEPATHS/HEALTH BOARD NAMES CHANGE ###

# hbres_list
hb_list <- c("Scotland","Ayrshire & Arran","Borders", "Dumfries & Galloway", 
             "Fife", "Forth Valley", "Grampian", "Greater Glasgow & Clyde", 
             "Highland", "Lanarkshire", "Lothian", "Orkney",
             "Shetland", "Tayside", "Western Isles")
hb_tibble <- tibble::tibble(hbres = c(hb_list))


## Ubiquitous Pathways
extract_path <- paste0("/PHI_conf/AAA/Topics/Screening/extracts/", yymm,
                       "/output/aaa_extract_", yymm, ".rds")

exclusions_path <- paste0("/PHI_conf/AAA/Topics/Screening/extracts/", yymm, 
                          "/output/aaa_exclusions_", yymm, ".rds")

temp_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", yymm,
                    "/temp")

hist_path <- "/PHI_conf/AAA/Topics/Screening/KPI/historical"

simd_path <- paste0("/conf/linkage/output/lookups/Unicode/Deprivation",
                    "/postcode_2024_2_simd2020v2.rds")

output_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", yymm,
                      "/output")


## Script-specific Variables~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### UPDATED IN SPRING ONLY BUT CALCULATED AUTOMATICALLY ###

# temporary variables to calculate below dates
current_fy <- kpi_report_years[3]
fy_start <- paste0("01-04-", substr(current_fy, 1,4))
fy_end <- paste0("31-03-20", substr(current_fy, 6, 7))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 1_2_processing_for_KPI_11-13.R
# Individuals born after this date are not eligible for the program yet 
# (Have not turned 65 years old in time for this KPI round)
# This will match `year2_end` variable in next script
cutoff_date <- format((dmy(fy_end) - years(65)), "%d-%m-%Y")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2_2_kpi_1_1-1_3_uptake_coverage.R
# Year 1 is the cohort currently being analyzed for reporting (complete), 
# while year 2 looks into the future to give a snapshot of progress for the  
# cohort reported on next cycle.
year1 <- kpi_report_years[3]

# automatically calculate next FY string
parts <- strsplit(year1, "/")[[1]]
modified_parts <- as.character(as.numeric(parts) + 1)
year2 <- paste(modified_parts, collapse = "/")

rm(parts, modified_parts) # tidy

# DOBs for people turning 66 in kpi_report_years[3]
year1_start <- format((dmy(fy_start) - years(66)), "%d-%m-%Y")
year1_end <- format((dmy(fy_end) - years(66)), "%d-%m-%Y")

# DOBs for people turning 66 in next FY (current FY at autumn run)
year2_start <- format((dmy(fy_start) - years(65)), "%d-%m-%Y")
year2_end <- format((dmy(fy_end) - years(65)), "%d-%m-%Y")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# KPI_1_4.R
## Moved to KPI 1.4 script


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4_3_kpi_2.R
# Dates of first and last financial year ##!! Just start and end dates of current FY?
start_date <- paste0(substr(current_fy, 1, 4), "-04-01")
end_date <- paste0("20", substr(current_fy, 6, 7), "-03-31") 

# # Table 4 variables
# finyear_minus_3 <- "2019/20" # Why does this year need to be segregated out? Not used for report.

end_current <- as.Date(end_date)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Theme 4 scripts
cut_off_date <- end_current # How is this date defined?? 
#potential match: 6_2_Suppl_Surve.R


# remove temporary variables
rm(current_fy, fy_start, fy_end)


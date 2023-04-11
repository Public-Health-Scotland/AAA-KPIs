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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 1_1_processing_for_KPI_11-13.R
cutoff_date <- dmy("31-03-1958")  ## How is this defined??


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2_2_kpi_1_1-1_3_uptake_coverage.R
year1_start <- dmy("01-04-1956")
year1_end <- dmy("31-03-1957")

year2_start <- dmy("01-04-1957")
year2_end <- dmy("31-03-1958")















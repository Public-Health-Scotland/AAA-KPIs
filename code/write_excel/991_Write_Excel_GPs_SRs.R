#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 991_Write_Excel_GPs_SRs.R
# Aoife McCarthy
# October 2024
# 
# Create KPI 1.2a by GP Practice + SR counts Excels for individual Health Boards
# 
# Written/run/revised on R Posit WB
# R version 4.1.2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 1: Housekeeping ----
library(dplyr)
library(readr) #
library(readxl)
library(tidylog) #
library(openxlsx) #
library(forcats)
library(phsaaa) # devtools::install_github("aoifem01/phsaaa")

rm(list=ls())
gc()

# source vars
source(here::here("code", "0_housekeeping.R"))

year_xx <- year(cut_off_date)
year_ww <- year_xx - 1

current_gp_data_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", yymm, 
                               "/data/gp_coverage_", substr(kpi_report_years[3], 3, 4),
                               substr(kpi_report_years[3], 6, 7), ".rds")

prev_gp_data_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", yymm - 100,
                            "/data/gp_coverage_", substr(kpi_report_years[2], 3, 4),
                            substr(kpi_report_years[2], 6, 7), ".rds")

sr_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", yymm, 
                  "/data/self_referrals_", substr(kpi_report_years[3], 3, 4),
                  substr(kpi_report_years[3], 6, 7), ".rds")

# 2: Data import ----

# GP coverage data for current FY and previous FY
gp_current <- read_rds(current_gp_data_path) |> 
  rename(cohort_year_xx = cohort_year1,
         test_year_xx = test_a_year1,
         percent_year_xx = percent_a_year1)

gp_prev <- read_rds(prev_gp_data_path) |> 
  rename(cohort_year_ww = cohort_year1,
         test_year_ww = test_a_year1,
         percent_year_ww = percent_a_year1)

gp_data <- gp_prev |> 
  full_join(gp_current) |> 
  mutate(across(contains(c("cohort", "test")), \(x) replace_na(x, 0))) |> 
  mutate(across(contains("percent"), \(x) replace(x, is.nan(x), NA)))

# self-referral data just for current FY
sr_data <- read_rds(sr_path)

# 3: Create function to loop through list of health boards ----

# things it needs

## don't use a template - make a list (ooh fancy!) of text elements that are written in each time
## e.g. header, col names, footnotes, etc etc - should be easy enough

## need it to have conditional formatting on uptake % cols - this might be tricky, but hopefully not

## need it to be flexible in terms of WHERE footnotes/borders are written as each HB has different # of rows

## need to it have HB-specific wb name, rows should automatically go in when written

## need it to have FY-specific col heads and footnotes (like other Excel wbs)


# think it's gonna be easy!


         
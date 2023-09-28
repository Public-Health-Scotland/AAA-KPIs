# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 999_Write_Excel_2.R
# 
# Karen Hotopp
# Sept 2023
# 
# Write out to AAA Excel workbook 2: Invitation and Attendance
# 
# Written/run on Posit WB, R 4.1.2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Notes:
# This script calls in the 3_invite_attend_yyyymm.rds file create in the
# 2_2_kpi_1_1-1_3_uptake_coverage.R script and transforms the data to print 
# directly into the theme 2 Excel file for the MEG.


#### 1: Housekeeping ----
library(dplyr)
library(readr)
library(tidyr)
library(openxlsx)


rm(list=ls())
gc()


## Values
source(here::here("code/0_housekeeping.R"))

rm(hb_list, exclusions_path, extract_path, hist_path, cutoff_date,
   prev_year, current_year, current_year_start, next_year_start,
   financial_year_due, financial_quarters, last_date)



## File paths
template_path <- paste0("/PHI_conf/AAA/Topics/Screening/templates")


### 2: Import data ----
theme2 <- read_rds(paste0(temp_path, "/3_invite_attend_", yymm, ".rds"))

table(theme2$kpi, theme2$fin_year) 
# should be 3 most recent complete years + incomplete/active year


## KPI 1.1 year1 ----
## Data for three most recent complete years and extended coverage to 1 Sept
kpi_1.1 <- theme2 |> 
  filter(kpi %in% c("KPI 1.1", "KPI 1.1 Sept coverage"),
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_"),) |> 
  select(hbres, FY_kpi_group, value)

# Pivot wider to match Excel output
kpi_1.1 <- kpi_1.1 |> 
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.1 year2 ----
## Data for currently active year and extended coverage to 1 Sept
kpi_1.1_y2 <- theme2 |> 
  filter(kpi %in% c("KPI 1.1", "KPI 1.1 Sept coverage"),
         fin_year == year2) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_"),) |> 
  select(hbres, FY_kpi_group, value)

# Pivot wider to match Excel output
kpi_1.1_y2 <- kpi_1.1_y2 |> 
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.2a year1 ----
## Data for currently active year and extended coverage to 1 Sept
kpi_1.2a <- theme2 |> 
  filter(kpi %in% c("KPI 1.2a", "KPI 1.2a Sept coverage"),
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_"),) |>
  select(hbres, FY_kpi_group, value)

# Pivot wider to match Excel output
kpi_1.2a <- kpi_1.2a |>
  pivot_wider(names_from = FY_kpi_group, values_from = value)

# Extract extended coverage to 1 Sept
kpi_1.2a_sept <- kpi_1.2a |>
  select(contains("_cohort"), contains("Sept coverage"))

kpi_1.2a_sept <- kpi_1.2a_sept[ , c(1, 4, 5, 2, 6, 7, 3, 8, 9)] 

# Remove extra historical coverage to 1 Sept data
kpi_1.2a <- kpi_1.2a[, -c(8:11)]

## KPI 1.2a year2 ----
## Data for currently active year and extended coverage to 1 Sept
kpi_1.2a_y2 <- theme2 |> 
  filter(kpi %in% c("KPI 1.2a", "KPI 1.2a Sept coverage"),
         fin_year == year2) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_"),) |> 
  select(hbres, FY_kpi_group, value)

# Pivot wider to match Excel output
kpi_1.2a_y2 <- kpi_1.2a_y2 |> 
  pivot_wider(names_from = FY_kpi_group, values_from = value)


##!! Put something in here for above plus SIMD!



## KPI 1.2b year1 ----
## Data for currently active year and extended coverage to 1 Sept
kpi_1.2b <- theme2 |> 
  filter(kpi == "KPI 1.2b",
         fin_year  %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_"),) |> 
  select(hbres, FY_kpi_group, value)

# Pivot wider to match Excel output
kpi_1.2b <- kpi_1.2b |> 
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.2b year2 ----
## Data for currently active year and extended coverage to 1 Sept
kpi_1.2b_y2 <- theme2 |> 
  filter(kpi == "KPI 1.2b",
         fin_year == year2) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_"),) |> 
  select(hbres, FY_kpi_group, value)

# Pivot wider to match Excel output
kpi_1.2b_y2 <- kpi_1.2b_y2 |> 
  pivot_wider(names_from = FY_kpi_group, values_from = value)












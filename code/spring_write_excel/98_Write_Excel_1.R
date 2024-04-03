# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 98_Write_Excel_1.R
# 
# Karen Hotopp
# February 2024
# 
# DESCRIPTION
# 
# Written/run on Posit WB, R 4.1.2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Notes:
# 


#### 1: Housekeeping ----
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(openxlsx)


rm(list=ls())
gc()

## Values
source(here::here("code/0_housekeeping.R"))

rm (exclusions_path, extract_path, hist_path, output_path, simd_path,
    fy_list, hb_list, fy_tibble, hb_tibble, season,
    cut_off_date, cutoff_date, end_current, end_date, start_date,
    year1_end, year1_start, year2_end, year2_start, year1, year2, yymm)


## File paths
template_path <- paste0("/PHI_conf/AAA/Topics/Screening/templates")


### 2: Import and format data ----
## KPI 1
kpi_1 <- read_rds(paste0(temp_path, "/2_1_invite_attend_", yymm, ".rds")) |> 
  filter(fin_year %in% c(kpi_report_years),
         kpi %in% c("KPI 1.1", "KPI 1.2a", "KPI 1.3a Scotland SIMD",
                    "KPI 1.4a", "KPI 1.4b"),
         hbres == "Scotland",
         !simd %in% c("Total", "Unknown"),
         str_ends(group, "_p")) |> 
  # match Excel tables
  pivot_wider(names_from = fin_year, values_from = value)

## KPI 2
kpi_2 <- read_rds(paste0(temp_path, "/3_1_kpi_2_", yymm, ".rds"))|> 
  filter(fin_year %in% c(kpi_report_years),
         kpi %in% c("KPI 2.1a", "KPI 2.1b", "KPI 2.2"),
         hbres == "Scotland",
         str_ends(group, "_p")) |> 
  # match Excel tables
  pivot_wider(names_from = fin_year, values_from = value)

## KPI 3
kpi_3 <- read_rds(paste0(temp_path, "/4_1_kpi_3_", yymm, ".rds")) |> 
  filter(financial_year %in% c(kpi_report_years),
         kpi %in% c("KPI 3.1 Residence", "KPI 3.2 Residence", 
                    "KPI 3.2 Surgery"),  # Should this last one stay in??
         health_board == "Scotland",
         str_ends(group, "_p")) |> 
  # match Excel tables
  pivot_wider(names_from = financial_year, values_from = value)

## KPI 4
kpi_4 <- read_rds(paste0(temp_path, "/4_2_kpi_4_", yymm, ".rds")) |> 
  filter(kpi %in% c("KPI 4.1", "KPI 4.2"),
         str_ends(group, "_p")) |> 
  mutate(year = str_sub(financial_year, 11)) |> 
  filter(year %in% c(kpi_report_years)) |> 
  select(-year) |> 
  # match Excel tables
  pivot_wider(names_from = financial_year, values_from = value)


## Save out files to use in publication
write_rds(kpi_1, paste0(temp_path, "/6_kpi_1_", yymm, ".rds"))
write_rds(kpi_2, paste0(temp_path, "/6_kpi_2_", yymm, ".rds"))
write_rds(kpi_3, paste0(temp_path, "/6_kpi_3_", yymm, ".rds"))
write_rds(kpi_4, paste0(temp_path, "/6_kpi_4_", yymm, ".rds"))


### 3: Output to Excel ----





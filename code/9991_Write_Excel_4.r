# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 9991_Write_Excel_4.R
# 
# Karen Hotopp
# Oct 2023
# 
# Write out to AAA Excel workbook 4: Referral Treatment and Outcomes
# 
# Written/run on Posit WB, R 4.1.2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Notes:
# This script calls in the 5_referral_outcomes_yyyymm.rds file create in the
# 8_4_KPI_3.1-3.2.R script and transforms the data to print 
# directly into the theme 4 Excel file for the MEG.


#### 1: Housekeeping ----
library(dplyr)
library(readr)
library(tidyr)
library(openxlsx)


rm(list=ls())
gc()


## Values
source(here::here("code/0_housekeeping_theme_4.R"))

rm(hb_list, cut_off_date, exclusions_path, extract_path, hist_path,
   kpi_report_years)


## File paths
template_path <- paste0("/PHI_conf/AAA/Topics/Screening/templates")


### 2: Import data ----
theme4 <- read_rds(paste0(temp_path, "/5_referral_outcomes_", yymm, ".rds"))

table(theme4$kpi, theme4$financial_year) 


### 3: Format data ----
## KPI 3.1 ----
kpi_3_1 <- theme4 |> 
  filter(kpi %in% c("KPI 3.1 Residence")) |> 
  mutate(FY_kpi_group = paste(financial_year, kpi, group, sep = "_")) |> 
  select(health_board, FY_kpi_group, value)

# Match Excel tables
kpi_3_1 <- kpi_3_1 |> 
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 3.2 HB of Residence ----
kpi_3_2_res <- theme4 |> 
  filter(kpi %in% c("KPI 3.2 Residence")) |> 
  mutate(FY_kpi_group = paste(financial_year, kpi, group, sep = "_")) |> 
  select(health_board, FY_kpi_group, value)

# Match Excel tables
kpi_3_2_res <- kpi_3_2_res |> 
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 3.2 HB of Surgery ----
kpi_3_2_surg <- theme4 |> 
  filter(kpi %in% c("KPI 3.2 Surgery")) |> 
  mutate(FY_kpi_group = paste(financial_year, kpi, group, sep = "_")) |> 
  select(health_board, FY_kpi_group, value)

# Match Excel tables
kpi_3_2_surg <- kpi_3_2_surg |> 
  pivot_wider(names_from = FY_kpi_group, values_from = value)








### 4: Write to Excel (openxlsx) ----
### Setup workbook ---
today <- paste0("Workbook created ", Sys.Date())

wb <- loadWorkbook(paste0(template_path, "/4 Referral Treatment and Outcomes_",
                          season, ".xlsx"))

## Table of Contents ---
writeData(wb, sheet = "Table of Contents", today, startRow = 6)

## KPI 3.1 ---
writeData(wb, sheet = "KPI 3.1", kpi_3_1, startRow = 7, colNames = FALSE)

## KPI 3.2 HB Residence ---
writeData(wb, sheet = "KPI 3.2 HB Residence", kpi_3_2_res, startRow = 7, 
          colNames = FALSE)

## KPI 3.2 HB Surgery ---
writeData(wb, sheet = "KPI 3.2 HB Surgery", kpi_3_2_surg, startRow = 7, 
          colNames = FALSE)





## Save ----
saveWorkbook(wb, paste0(output_path, "/4 Referral Treatment and Outcomes_", 
                        yymm, ".xlsx"), overwrite = TRUE)

















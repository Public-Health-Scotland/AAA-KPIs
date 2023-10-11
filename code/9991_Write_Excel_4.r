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
# This script calls in the 5_referral_outcomes_yyyymm.rds, 6_kpi_4_yyyymm.rds,
# 7_kpi_4_HB_yyyymm.rds, and 8_kpi_4_mortality_yyyymm.rds files create in the
# 8_4_KPI_3.R and 9_4_kpi_4.R scripts and transforms the data to print 
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

rm(hb_list, fy_list, cut_off_date, exclusions_path, extract_path,
   kpi_report_years, cut_off_date_1, cut_off_date_3, cut_off_date_5)


## File paths
template_path <- paste0("/PHI_conf/AAA/Topics/Screening/templates")


### 2: Import data ----
# KPI 3.1 and 3.2
theme4_3 <- read_rds(paste0(temp_path, "/5_referral_outcomes_", yymm, ".rds"))

table(theme4_3$kpi, theme4_3$financial_year) 

# KPI 4.1 and 4.2
theme4_4 <- read_rds(paste0(temp_path, "/6_kpi_4_", yymm, ".rds"))

table(theme4_4$kpi, theme4_4$surgery_type) 

theme4_4_hb <- read_rds(paste0(temp_path, "/7_kpi_4_HB_", yymm, ".rds"))

table(theme4_4_hb$kpi, theme4_4_hb$surgery_type) 

theme4_4_mort <- read_rds(paste0(temp_path, "/8_kpi_4_mortality_", yymm, ".rds"))

table(theme4_4_mort$kpi, theme4_4_mort$surg_method) 


### 3: Format data ----
## KPI 3.1 ----
kpi_3_1 <- theme4_3 |> 
  filter(kpi %in% c("KPI 3.1 Residence")) |> 
  mutate(FY_kpi_group = paste(financial_year, kpi, group, sep = "_")) |> 
  select(health_board, FY_kpi_group, value)

# Match Excel tables
kpi_3_1 <- kpi_3_1 |> 
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 3.2 HB of Residence ----
kpi_3_2_res <- theme4_3 |> 
  filter(kpi %in% c("KPI 3.2 Residence")) |> 
  mutate(FY_kpi_group = paste(financial_year, kpi, group, sep = "_")) |> 
  select(health_board, FY_kpi_group, value)

# Match Excel tables
kpi_3_2_res <- kpi_3_2_res |> 
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 3.2 HB of Surgery ----
kpi_3_2_surg <- theme4_3 |> 
  filter(kpi %in% c("KPI 3.2 Surgery")) |> 
  mutate(FY_kpi_group = paste(financial_year, kpi, group, sep = "_")) |> 
  select(health_board, FY_kpi_group, value)

# Match Excel tables
kpi_3_2_surg <- kpi_3_2_surg |> 
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 4.1 ----
kpi_4_1 <- theme4_4 |> 
  filter(kpi %in% c("KPI 4.1")) |>
  pivot_wider(names_from = group, values_from = value) |> 
  select(financial_year, surgeries_n, deaths_n, deaths_p)

kpi_4_1 = tail(kpi_4_1, n = 1)

## KPI 4.2 ----
kpi_4_2 <- theme4_4 |> 
  filter(kpi %in% c("KPI 4.2")) |>
  pivot_wider(names_from = group, values_from = value) |> 
  select(financial_year, surgeries_n, deaths_n, deaths_p)

kpi_4_2 = tail(kpi_4_2, n = 1)

## KPI 4.1 Additional A ----
kpi_4_1_add_A <- theme4_4 |> 
  filter(kpi %in% c("KPI 4.1 Additional A")) |>
  pivot_wider(names_from = group, values_from = value) |> 
  select(procedures_n, deaths)

## KPI 4.2 Additional A ----
kpi_4_2_add_A <- theme4_4 |> 
  filter(kpi %in% c("KPI 4.2 Additional A")) |>
  pivot_wider(names_from = group, values_from = value) |> 
  select(procedures_n, deaths)

## NEED TO RETHINK THIS!! IS HB OF SURGERY ACTUALLY NEEDED??
# ## KPI 4.1 Additional B: Surgery HB ----
kpi_4_1_add_B <- theme4_4_hb |>
  filter(kpi == "KPI 4.1 Add B: Surgery") #|>
  select(procedures_n, deaths)
## HOW TO REMOVE COLUMNS BASED ON ROW VALUE??
# Want to remove columns where cumulative = NA
  
  # 
# ## KPI 4.2 Additional B: Screening HB ----
# kpi_4_2_add_B <- theme4_4_hb |> 
#   filter(kpi %in% c("KPI 4.2 Add B: Screen")) |>
#   pivot_wider(names_from = group, values_from = value) |> 
#   select(procedures_n, deaths)









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

















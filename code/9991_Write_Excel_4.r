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

rm(hb_list, fy_list, cut_off_date, extract_path, kpi_report_years)
   

## File paths
template_path <- paste0("/PHI_conf/AAA/Topics/Screening/templates")


### 2: Import data ----
# KPI 3.1 and 3.2
theme4_3 <- read_rds(paste0(temp_path, "/5_referral_outcomes_", yymm, ".rds"))

table(theme4_3$kpi, theme4_3$financial_year) 

# KPI 4.1 and 4.2
theme4_4 <- read_rds(paste0(temp_path, "/6_kpi_4_", yymm, ".rds"))

table(theme4_4$kpi, theme4_4$surg_method) # GO BACK AND CHANGE IN 9_4_kpi_4.R script

theme4_4_hb <- read_rds(paste0(temp_path, "/7_kpi_4_HB_", yymm, ".rds"))

table(theme4_4_hb$kpi, theme4_4_hb$surg_method) 

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
#!! Row needs added in Excel template manually!!
kpi_4_1_add_A <- theme4_4 |> 
  filter(kpi %in% c("KPI 4.1 Additional A")) |>
  pivot_wider(names_from = group, values_from = value) |> 
  select(procedures_n, deaths)

## KPI 4.2 Additional A ----
#!! Row needs added in Excel template manually!!
kpi_4_2_add_A <- theme4_4 |> 
  filter(kpi %in% c("KPI 4.2 Additional A")) |>
  pivot_wider(names_from = group, values_from = value) |> 
  select(procedures_n, deaths)

## KPI 4.1 Additional B: Screening HB ----
#!! Row needs added in Excel template manually!!
kpi_4_1_add_B <- theme4_4_hb |>
  filter(kpi == "KPI 4.1 Add B: Screen") |> 
  select(-c(kpi, surg_method, financial_year)) |> 
  # remove columns where value is all NA
  select_if(function(col) !all(is.na(col)))

## KPI 4.2 Additional B: Screening HB ----
#!! Row needs added in Excel template manually!!
kpi_4_2_add_B <- theme4_4_hb |>
  filter(kpi %in% c("KPI 4.2 Add B: Screen")) |>
  select(-c(kpi, surg_method, financial_year)) |> 
  # remove columns where value is all NA
  select_if(function(col) !all(is.na(col)))

## KPI 4.1 Additional C: Surgery HB ----
#!! Row needs added in Excel template manually!!
kpi_4_1_add_C <- theme4_4_hb |>
  filter(kpi == "KPI 4.1 Add C: Surgery") |> 
  select(-c(kpi, surg_method, financial_year)) |> 
  # remove columns where value is all NA
  select_if(function(col) !all(is.na(col)))

## KPI 4.2 Additional C: Surgery HB ----
#!! Row needs added in Excel template manually!!
kpi_4_2_add_C <- theme4_4_hb |>
  filter(kpi == "KPI 4.2 Add C: Surgery") |>
  select(-c(kpi, surg_method, financial_year)) |> 
  # remove columns where value is all NA
  select_if(function(col) !all(is.na(col)))

## KPI 4: 1-Year Mortality Rates ----
kpi_4_1yr <- theme4_4 |>
  filter(kpi %in% c("KPI 4.1 1yr Rate", "KPI 4.2 1yr Rate")) |>
  pivot_wider(names_from = c(surg_method, group), values_from = value)

open_1yr <- kpi_4_1yr |> 
  filter(kpi == "KPI 4.1 1yr Rate") |>
  select(financial_year, Open_surgeries_n, Open_deaths_n, Open_deaths_p)

evar_1yr <- kpi_4_1yr |> 
  filter(kpi == "KPI 4.2 1yr Rate") |> 
  select(financial_year, EVAR_surgeries_n, EVAR_deaths_n, EVAR_deaths_p)

kpi_4_1yr <- left_join(open_1yr, evar_1yr)
kpi_4_1yr = tail(kpi_4_1yr, n = 1)

rm(open_1yr, evar_1yr)

## KPI 4: 1, 3, 5-Year Mortality Rates ----
kpi_4_cum_mort <- theme4_4_mort |>
  filter(kpi == "KPI 4 Cumulative") |>
  select(-c(kpi, health_board))



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

## KPI 4.1 ---
writeData(wb, sheet = "KPI 4.1", kpi_4_1, startRow = 7, colNames = FALSE)

## KPI 4.2 ---
writeData(wb, sheet = "KPI 4.2", kpi_4_2, startRow = 7, colNames = FALSE)

## KPI 4.1 Additional ---
writeData(wb, sheet = "KPI 4.1 Additional", kpi_4_1_add_A, startRow = 7, 
          startCol = 2, colNames = FALSE)
writeData(wb, sheet = "KPI 4.1 Additional", kpi_4_1_add_B, startRow = 24, 
          startCol = 2)
writeData(wb, sheet = "KPI 4.1 Additional", kpi_4_1_add_C, startRow = 42, 
          startCol = 2)

## KPI 4.2 Additional ---
writeData(wb, sheet = "KPI 4.2 Additional", kpi_4_2_add_A, startRow = 7, 
          startCol = 2, colNames = FALSE)
writeData(wb, sheet = "KPI 4.2 Additional", kpi_4_2_add_B, startRow = 24, 
          startCol = 2)
writeData(wb, sheet = "KPI 4.2 Additional", kpi_4_2_add_C, startRow = 42, 
          startCol = 2)

## KPI 4 1-year Mortality ---
writeData(wb, sheet = "1-year mortality rates", kpi_4_1yr, startRow = 8, 
          colNames = FALSE)

## KPI 4 1,3,5-year Cumulative Mortality ---
writeData(wb, sheet = "1, 3, 5-year mortality", kpi_4_cum_mort, startRow = 8, 
          colNames = FALSE)




## Save ----
saveWorkbook(wb, paste0(output_path, "/4 Referral Treatment and Outcomes_", 
                        yymm, ".xlsx"), overwrite = TRUE)

















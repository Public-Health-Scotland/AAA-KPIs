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
# This script calls in the multiple RDS files create in the 8_4_KPI_3.R, 
# 9_4_kpi_4.R, 91_4_vascular_referrals.R, and 96_4_unfit_for_surgery.R scripts 
# and transforms the data to print directly into the theme 4 Excel file for 
# both the spring and autumn MEGs.

##!! Need to add in spring to sources file! This should mostly be notes about
## provisional data and information on title tab.

#### 1: Housekeeping ----
library(dplyr)
library(readr)
library(tidyr)
library(reporter)
library(openxlsx)


rm(list=ls())
gc()


## Values
source(here::here("code/0_housekeeping.R"))

rm (exclusions_path, extract_path, hist_path, simd_path,
    fy_list, hb_list, fy_tibble, hb_tibble,
     cutoff_date, end_current, end_date, start_date,
    year1_end, year1_start, year2_end, year2_start, year1, year2)
   

## File paths
template_path <- paste0("/PHI_conf/AAA/Topics/Screening/templates")


### 2: Import data ----
# KPI 3.1 and 3.2
theme4_3 <- read_rds(paste0(temp_path, "/4_1_kpi_3_", yymm, ".rds"))
table(theme4_3$kpi, theme4_3$financial_year) 

# KPI 4.1 and 4.2
theme4_4 <- read_rds(paste0(temp_path, "/4_2_kpi_4_", yymm, ".rds"))
table(theme4_4$kpi, theme4_4$surg_method) # GO BACK AND CHANGE IN 9_4_kpi_4.R script

theme4_4_hb <- read_rds(paste0(temp_path, "/4_3_kpi_4_HB_", yymm, ".rds"))
table(theme4_4_hb$kpi, theme4_4_hb$surg_method) 

theme4_4_mort <- read_rds(paste0(temp_path, "/4_4_kpi_4_mortality_", yymm, ".rds"))
table(theme4_4_mort$kpi, theme4_4_mort$surg_method) 

# Vascular Referrals
theme4_referral <- read_rds(paste0(temp_path, "/4_5_vasc_referrals_", yymm, ".rds"))

theme4_outcomes <- read_rds(paste0(temp_path, "/4_6_vasc_outcomes_", yymm, ".rds"))

theme4_repairs <- read_rds(paste0(temp_path, "/4_7_vasc_ref_repairs_", yymm, ".rds"))

# Unfit for Surgery
theme4_unfit <- read_rds(paste0(temp_path, "/4_8_unfit_for_surgery_", yymm, ".rds"))

theme4_unfit_followup <- read_rds(paste0(temp_path, "/4_9_unfit_follow-up_", 
                                         yymm, ".rds"))

theme4_unfit_deaths <- read_rds(paste0(temp_path, "/4_91_unfit_deaths_cause_", 
                                       yymm, ".rds"))


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

kpi_4_1 <- tail(kpi_4_1, n = 1)

## KPI 4.2 ----
kpi_4_2 <- theme4_4 |> 
  filter(kpi %in% c("KPI 4.2")) |>
  pivot_wider(names_from = group, values_from = value) |> 
  select(financial_year, surgeries_n, deaths_n, deaths_p)

kpi_4_2 <- tail(kpi_4_2, n = 1)

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

kpi_4_1yr <- left_join(open_1yr, evar_1yr) |> 
  # remove last row, as this will not have complete data 
  filter(row_number() <= n()-1) 
kpi_4_1yr_tail = tail(kpi_4_1yr, n = 1) 

rm(open_1yr, evar_1yr)

## KPI 4: 1, 3, 5-Year Mortality Rates ----
kpi_4_cum_mort <- theme4_4_mort |>
  filter(kpi == "KPI 4 Cumulative") |>
  select(-c(kpi, health_board))

## Vascular Referrals: Table 7 ----
vasc_refs <- select(theme4_referral, -source_ref_to_vasc)

## Vascular Referrals: Outcomes ----
vasc_outcomes1 <- theme4_outcomes |> 
  filter(result_size == "large" & outcome_type == "Total") |> 
  select(-c(result_size, outcome_type, result_outcome))

vasc_outcomes2 <- theme4_outcomes |> 
  filter(result_size == "large" & (outcome_type == "Total: final outcome" | 
                                     outcome_type == "final outcome")) |> 
  select(-c(result_size, outcome_type, result_outcome))

vasc_outcomes3 <- theme4_outcomes |> 
  filter(result_size == "large" & (outcome_type == "Total: non-final outcome" | 
                                     outcome_type == "non-final outcome")) |> 
  select(-c(result_size, outcome_type, result_outcome))

vasc_outcomes4 <- theme4_outcomes |> 
  filter(result_size == "large" & outcome_type == "Total: no outcome recorded") |> 
  select(-c(result_size, outcome_type, result_outcome))

vasc_outcomes5 <- theme4_outcomes |> 
  filter(result_size == "small" & outcome_type == "Total") |> 
  select(-c(result_size, outcome_type, result_outcome))

vasc_outcomes6 <- theme4_outcomes |> 
  filter(result_size == "small" & (outcome_type == "Total: final outcome" | 
                                     outcome_type == "final outcome")) |> 
  select(-c(result_size, outcome_type, result_outcome))

vasc_outcomes7 <- theme4_outcomes |> 
  filter(result_size == "small" & (outcome_type == "Total: non-final outcome" | 
                                     outcome_type == "non-final outcome")) |> 
  select(-c(result_size, outcome_type, result_outcome))

## Vascular Referrals: AAA Repairs ----
aaa_repairs <- theme4_repairs |> 
  mutate(fy_surgery = if_else(is.na(fy_surgery), "Cumulative", fy_surgery),
         surg_method = if_else(is.na(surg_method), "Total AAA repairs", 
                               surg_method)) |> 
  pivot_wider(names_from = c(fy_surgery, surg_method), values_from = n)

## Unfit for Surgery ----
unfit_surgery <- theme4_unfit |> 
  pivot_wider(names_from = financial_year, values_from = c(cohort_n:unfit_p)) |> 
  select(hbres, ends_with(kpi_report_years[1]), ends_with(kpi_report_years[2]), 
         ends_with(kpi_report_years[3]), ends_with("Cumulative"))

## Unfit for Surgery Follow-up ----
unfit_followup <- theme4_unfit_followup 
# (no reformatting)

## Unfit Follow-up Deaths ----
unfit_deaths1 <- theme4_unfit_deaths |> 
  filter(section == "Totals") |> 
  select(category, count_n)

unfit_deaths2 <- theme4_unfit_deaths |> 
  filter(section == "All causes") |> 
  select(category, count_n)

unfit_deaths3 <- theme4_unfit_deaths |> 
  filter(section == "AAA-related causes") |> 
  select(category, count_n)


### 4: Write to Excel (openxlsx) ----
### Setup workbook ---
today <- paste0("Workbook created ", Sys.Date())

wb <- loadWorkbook(paste0(template_path, "/4_Referral Treatment and Outcomes_",
                          season, ".xlsx"))
## Source notes script
source(here::here(paste0("code/", season, "_write_excel/95_Source_Excel_4.R")))

rm(list=ls(pattern = "theme4_"))


## Table of Contents ---
writeData(wb, sheet = "Table of Contents", today, startRow = 6)
showGridLines(wb, "Table of Contents", showGridLines = FALSE)

## KPI 3.1 ---
writeData(wb, sheet = "KPI 3.1", kpi_3_1, startRow = 7, colNames = FALSE)
showGridLines(wb, "KPI 3.1", showGridLines = FALSE)

## KPI 3.2 HB Residence ---
writeData(wb, sheet = "KPI 3.2 HB Residence", kpi_3_2_res, startRow = 7, 
          colNames = FALSE)
showGridLines(wb, "KPI 3.2 HB Residence", showGridLines = FALSE)

## KPI 3.2 HB Surgery ---
writeData(wb, sheet = "KPI 3.2 HB Surgery", kpi_3_2_surg, startRow = 7, 
          colNames = FALSE)
showGridLines(wb,  "KPI 3.2 HB Surgery", showGridLines = FALSE)

## KPI 4.1 ---
writeData(wb, sheet = "KPI 4.1", kpi_4_1, startRow = 7, colNames = FALSE)
showGridLines(wb, "KPI 4.1", showGridLines = FALSE)

## KPI 4.2 ---
writeData(wb, sheet = "KPI 4.2", kpi_4_2, startRow = 7, colNames = FALSE)
showGridLines(wb, "KPI 4.2", showGridLines = FALSE)

## KPI 4.1 Additional ---
## UPDATE/CHECK LINES EACH RUN ##
writeData(wb, sheet = "KPI 4.1 Additional", kpi_4_1_add_A, startRow = 7, 
          startCol = 2, colNames = FALSE)
writeData(wb, sheet = "KPI 4.1 Additional", kpi_4_1_add_B, startRow = 25, 
          startCol = 2)
writeData(wb, sheet = "KPI 4.1 Additional", kpi_4_1_add_C, startRow = 44, 
          startCol = 2)
showGridLines(wb, "KPI 4.1 Additional", showGridLines = FALSE)

## KPI 4.2 Additional ---
## UPDATE/CHECK LINES EACH RUN ##
writeData(wb, sheet = "KPI 4.2 Additional", kpi_4_2_add_A, startRow = 7, 
          startCol = 2, colNames = FALSE)
writeData(wb, sheet = "KPI 4.2 Additional", kpi_4_2_add_B, startRow = 25, 
          startCol = 2)
writeData(wb, sheet = "KPI 4.2 Additional", kpi_4_2_add_C, startRow = 44, 
          startCol = 2)
showGridLines(wb, "KPI 4.2 Additional", showGridLines = FALSE)

## Table 7: Vascular Referrals ---
writeData(wb, sheet = "7) Vascular referrals", vasc_refs, startRow = 7,
          startCol = 2, colNames = FALSE)
showGridLines(wb, "7) Vascular referrals", showGridLines = FALSE)

## Vascular Referral Outcomes ---
writeData(wb, sheet = "Vascular KPIs background", vasc_outcomes1, startRow = 5, 
          startCol = 2, colNames = FALSE)
writeData(wb, sheet = "Vascular KPIs background", vasc_outcomes2, startRow = 7, 
          startCol = 2, colNames = FALSE)
writeData(wb, sheet = "Vascular KPIs background", vasc_outcomes3, startRow =21, 
          startCol = 2, colNames = FALSE)
writeData(wb, sheet = "Vascular KPIs background", vasc_outcomes4, startRow = 29, 
          startCol = 2, colNames = FALSE)
writeData(wb, sheet = "Vascular KPIs background", vasc_outcomes5, startRow = 31, 
          startCol = 2, colNames = FALSE)
writeData(wb, sheet = "Vascular KPIs background", vasc_outcomes6, startRow = 33, 
          startCol = 2, colNames = FALSE)
writeData(wb, sheet = "Vascular KPIs background", vasc_outcomes7, startRow = 38, 
          startCol = 2, colNames = FALSE)
showGridLines(wb, "Vascular KPIs background", showGridLines = FALSE)

## KPI 4 1,3,5-year Cumulative Mortality ---
writeData(wb, sheet = "1, 3, 5-year mortality", kpi_4_cum_mort, startRow = 8, 
          colNames = FALSE)
showGridLines(wb, "1, 3, 5-year mortality", showGridLines = FALSE)

## KPI 4 1-year Mortality ---
writeData(wb, sheet = "1-year mortality rates", kpi_4_1yr_tail, startRow = 8, 
          colNames = FALSE)
showGridLines(wb, "1-year mortality rates", showGridLines = FALSE)

## AAA Repairs ---
writeData(wb, sheet = "AAA Repairs", aaa_repairs, startRow = 7, 
          colNames = FALSE)
showGridLines(wb, "AAA Repairs", showGridLines = FALSE)

## Unfit for Surgery ---
writeData(wb, sheet = "Unfit for surgery", unfit_surgery, startRow = 7, 
          colNames = FALSE)
showGridLines(wb, "Unfit for surgery", showGridLines = FALSE)

## Unfit for Surgery Follow-up ---
writeData(wb, sheet = "Unfit for surgery follow-up", unfit_followup, 
          startRow = 7, colNames = FALSE)
showGridLines(wb, "Unfit for surgery follow-up", showGridLines = FALSE)

## Unfit Follow-up Deaths ---
writeData(wb, sheet = "Unfit follow-up deaths by cause", unfit_deaths1, 
          startRow = 5, colNames = FALSE)
writeData(wb, sheet = "Unfit follow-up deaths by cause", unfit_deaths2, 
          startRow = 10, colNames = FALSE)
writeData(wb, sheet = "Unfit follow-up deaths by cause", unfit_deaths3, 
          startRow = 23, colNames = FALSE)
showGridLines(wb, "Unfit follow-up deaths by cause", showGridLines = FALSE)

## Save ----
saveWorkbook(wb, paste0(output_path, "/4_Referral Treatment and Outcomes_", 
                        yymm, ".xlsx"), overwrite = TRUE)


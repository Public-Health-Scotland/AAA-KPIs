# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 03_Write_Excel_4.R
# 
# Karen Hotopp & Aoife McCarthy
# Oct 2023
# 
# Write out to AAA Excel workbook 4: Referral Treatment and Outcomes
# 
# Written/run on Posit WB, R 4.1.2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Notes:
# This script calls in the multiple RDS files create in the 05_4_kpi_3.R, 
# 06_4_kpi_4.R, 07_4_vascular_referrals.R, and 08_4_unfit_for_surgery.R scripts 
# and transforms the data to print directly into the theme 4 Excel file for 
# both the spring and autumn QPMGs.

# 1: Housekeeping ----
library(dplyr)
library(readr)
library(tidyr)
library(reporter)
library(openxlsx)
library(janitor)
library(phsaaa) # devtools::install_github("Public-Health-Scotland/phsaaa")


rm(list=ls())
gc()


## Values
source(here::here("code", "00_housekeeping.R"))

rm (exclusions_path, extract_path, hist_path, simd_path,
    fy_list, fy_tibble, hb_tibble,
     cutoff_date, end_current, end_date, start_date,
    year1_end, year1_start, year2_end, year2_start, year1, year2)
   

## File paths
template_path <- paste0("/PHI_conf/AAA/Topics/Screening/templates")


# 2: Import data ----
# KPI 3.1 and 3.2
theme4_3 <- read_rds(paste0(temp_path, "/4_1_kpi_3_", yymm, ".rds"))

table(theme4_3$kpi, theme4_3$fin_year) 

if(season == "spring") {
  p_note_3.1_data <- read_rds(paste0(temp_path, "/4_provisional_note_3.1.rds")) |> 
    pivot_wider(names_from = pending_appt, values_from = n) |> 
    mutate(`Referral date and outpatient date` = `Total referrals` - `Referral date, no outpatient date`)
}

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


# 3: Format data ----
## KPI 3.1 ----
kpi_3_1 <- theme4_3 |> 
  filter(kpi %in% c("KPI 3.1 Residence")) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value)

# Match Excel tables
kpi_3_1 <- kpi_3_1 |> 
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 3.2 HB of Residence ----
kpi_3_2_res <- theme4_3 |> 
  filter(kpi %in% c("KPI 3.2 Residence")) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value)

# Match Excel tables
kpi_3_2_res <- kpi_3_2_res |> 
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 3.2 HB of Surgery ----
kpi_3_2_surg <- theme4_3 |> 
  filter(kpi %in% c("KPI 3.2 Surgery"),
         !hbres == "Dumfries & Galloway") |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value)

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
kpi_4_1_add_A <- theme4_4 |> 
  filter(kpi %in% c("KPI 4.1 Additional A")) |>
  pivot_wider(names_from = group, values_from = value) |> 
  select(financial_year, procedures_n, deaths) |> 
  mutate(financial_year = paste0("Year ending 31 March ", financial_year))

## KPI 4.2 Additional A ----
kpi_4_2_add_A <- theme4_4 |> 
  filter(kpi %in% c("KPI 4.2 Additional A")) |>
  pivot_wider(names_from = group, values_from = value) |> 
  select(financial_year, procedures_n, deaths) |> 
  mutate(financial_year = paste0("Year ending 31 March ", financial_year))

## KPI 4.1 Additional B: Screening HB ----
kpi_4_1_add_B <- theme4_4_hb |>
  filter(kpi == "KPI 4.1 Add B: Screen") |> 
  select(-c(kpi, surg_method)) |> 
  # remove columns where value is all NA
  select_if(function(col) !all(is.na(col))) |> 
  mutate(financial_year = paste0("Year ending 31 March ", financial_year))

## KPI 4.2 Additional B: Screening HB ----
kpi_4_2_add_B <- theme4_4_hb |>
  filter(kpi %in% c("KPI 4.2 Add B: Screen")) |>
  select(-c(kpi, surg_method)) |> 
  # remove columns where value is all NA
  select_if(function(col) !all(is.na(col))) |> 
  mutate(financial_year = paste0("Year ending 31 March ", financial_year))

## KPI 4.1 Additional C: Surgery HB ----
kpi_4_1_add_C <- theme4_4_hb |>
  filter(kpi == "KPI 4.1 Add C: Surgery") |> 
  select(-c(kpi, surg_method)) |> 
  # remove columns where value is all NA
  select_if(function(col) !all(is.na(col))) |> 
  mutate(financial_year = paste0("Year ending 31 March ", financial_year))

## KPI 4.2 Additional C: Surgery HB ----
kpi_4_2_add_C <- theme4_4_hb |>
  filter(kpi == "KPI 4.2 Add C: Surgery") |>
  select(-c(kpi, surg_method)) |> 
  # remove columns where value is all NA
  select_if(function(col) !all(is.na(col))) |> 
  mutate(financial_year = paste0("Year ending 31 March ", financial_year))

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
data_vasc_outcomes <- theme4_outcomes # used in write_vasc_background function

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
         ends_with(kpi_report_years[3]), ends_with("Cumulative")) |> 
  mutate(hbres = forcats::fct_relevel(hbres, hb_list)) |> 
  arrange(hbres)

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


# 4: Write to Excel (openxlsx) ----
## Setup workbook ----

wb <- loadWorkbook(paste0(template_path, "/4_Referral Treatment and Outcomes_",
                          season, ".xlsx"))

## Source notes script
source(here::here("code", "src", "Source_Excel_4.R"))
source(here::here("code", "src", "Source_Excel_functions.R"))

rm(list=ls(pattern = "theme4_"))


## Table of Contents ----
# notes
writeData(wb, sheet = "Table of Contents", pub_year, 
          startRow = 3, startCol = 1)
writeData(wb, sheet = "Table of Contents", qpmg_review, 
          startRow = 4, startCol = 1)
addStyle(wb, sheet = "Table of Contents", styles$black_bold_nowrap_12,
         rows = 4, cols = 1)
writeData(wb, sheet = "Table of Contents", today, 
          startRow = 6)
addStyle(wb, sheet = "Table of Contents", styles$black_nowrap_12,
         rows = c(3, 6), cols = 1, gridExpand = T)
writeData(wb, sheet = "Table of Contents", tab_vasc_desc, 
          startRow = 21, startCol = 2)
addStyle(wb, sheet = "Table of Contents", styles$blue_border_underline_12,
         rows = 21, cols = 2)
writeData(wb, sheet = "Table of Contents", note_toc, 
          startRow = 29, startCol = 1)
addStyle(wb, sheet = "Table of Contents", styles$red_bold_12,
         rows = 29, cols = 1)
showGridLines(wb, "Table of Contents", showGridLines = FALSE)

## KPI 3.1 ----
# notes
writeData(wb, sheet = "KPI 3.1", screened_year_vvr, 
          startRow = 4, startCol = 2)
writeData(wb, sheet = "KPI 3.1", screened_year_wwr, 
          startRow = 4, startCol = 5)
writeData(wb, sheet = "KPI 3.1", screened_year_xx, 
          startRow = 4, startCol = 8)
addStyle(wb, sheet = "KPI 3.1", styles$black_border_centre_12,
          rows = 4, cols = 2:10, gridExpand = T)
writeData(wb, sheet = "KPI 3.1", kpi_3.1_revised, 
          startRow = 32)
addStyle(wb, sheet = "KPI 3.1", style = styles$orange_11, 
         rows = 32, cols = 1)
if (season == "spring") {
  writeData(wb, sheet = "KPI 3.1", kpi_3.1_prov, 
            startRow = 34)
  addStyle(wb, sheet = "KPI 3.1", style = styles$black_11, 
           rows = 34, cols = 1)
}
# data
writeData(wb, sheet = "KPI 3.1", kpi_3_1, 
          startRow = 7, colNames = FALSE)
showGridLines(wb, "KPI 3.1", showGridLines = FALSE)

## KPI 3.2 HB Residence ----
# notes
writeData(wb, sheet = "KPI 3.2 HB Residence", screened_year_vvr, 
          startRow = 4, startCol = 2)
writeData(wb, sheet = "KPI 3.2 HB Residence", screened_year_wwr, 
          startRow = 4, startCol = 5)
writeData(wb, sheet = "KPI 3.2 HB Residence", screened_year_xx, 
          startRow = 4, startCol = 8)
addStyle(wb, sheet = "KPI 3.2 HB Residence", styles$black_border_centre_12,
          rows = 4, cols = 2:10, gridExpand = T)
writeData(wb, sheet = "KPI 3.2 HB Residence", kpi_3.2_revised, 
          startRow = 33)
addStyle(wb, sheet = "KPI 3.2 HB Residence", styles$orange_11, 
         rows = 33, cols = 1)
if (season == "spring") {
  writeData(wb, sheet = "KPI 3.2 HB Residence", kpi_3.2_prov, 
            startRow = 35)
  addStyle(wb, sheet = "KPI 3.2 HB Residence", styles$orange_11, 
           rows = 35, cols = 1)
}
# data
writeData(wb, sheet = "KPI 3.2 HB Residence", kpi_3_2_res, 
          startRow = 7, colNames = FALSE)
showGridLines(wb, "KPI 3.2 HB Residence", showGridLines = FALSE)

## KPI 3.2 HB Surgery ----
# notes
writeData(wb, sheet = "KPI 3.2 HB Surgery", screened_year_vvr, 
          startRow = 4, startCol = 2)
writeData(wb, sheet = "KPI 3.2 HB Surgery", screened_year_wwr, 
          startRow = 4, startCol = 5)
writeData(wb, sheet = "KPI 3.2 HB Surgery", screened_year_xx, 
          startRow = 4, startCol = 8)
addStyle(wb, sheet = "KPI 3.2 HB Surgery", styles$black_border_centre_12,
         rows = 4, cols = 2:10, gridExpand = T)
writeData(wb, sheet = "KPI 3.2 HB Surgery", kpi_3.2_revised, 
          startRow = 25)
addStyle(wb,sheet =  "KPI 3.2 HB Surgery", style = styles$orange_11, 
         rows = 25, cols = 1)
if (season == "spring") {
  writeData(wb, sheet = "KPI 3.2 HB Surgery", kpi_3.2_prov, 
            startRow = 27)
  addStyle(wb,sheet =  "KPI 3.2 HB Surgery", style = styles$orange_11, 
           rows = 27, cols = 1)
}
# data
writeData(wb, sheet = "KPI 3.2 HB Surgery", kpi_3_2_surg, 
          startRow = 7, colNames = FALSE)
showGridLines(wb,  "KPI 3.2 HB Surgery", showGridLines = FALSE)

## KPI 4.1 ----
# notes
writeData(wb, sheet = "KPI 4.1", kpi_4_1_note, 
          startRow = 16, startCol = 1)
addStyle(wb, sheet = "KPI 4.1", styles$black_11, 
          rows = 16, cols = 1)
if (season == "spring") {
  writeData(wb, sheet = "KPI 4.1", kpi_4_prov, 
            startRow = 18, startCol = 1)
  addStyle(wb, sheet = "KPI 4.1", style = styles$black_11, 
           rows = 18, cols = 1)
}
# data
writeData(wb, sheet = "KPI 4.1", kpi_4_1, 
          startRow = 7, colNames = FALSE)
addStyle(wb, sheet = "KPI 4.1", styles$black_border_centre_12,
         rows = 7, cols = 1)
showGridLines(wb, "KPI 4.1", showGridLines = FALSE)

## KPI 4.2 ----
# notes
writeData(wb, sheet = "KPI 4.2", kpi_4_2_note, 
          startRow = 16, startCol = 1)
addStyle(wb, sheet = "KPI 4.2", styles$black_11, 
         rows = 16, cols = 1)
if (season == "spring") {
  writeData(wb, sheet = "KPI 4.2", kpi_4_prov, 
            startRow = 18, startCol = 1)
  addStyle(wb, sheet = "KPI 4.2", styles$black_11, 
           rows = 18, cols = 1)
}
# data
writeData(wb, sheet = "KPI 4.2", kpi_4_2, 
          startRow = 7, colNames = FALSE)
addStyle(wb, sheet = "KPI 4.2", styles$black_border_centre_12,
         rows = 7, cols = 1)
showGridLines(wb, "KPI 4.2", showGridLines = FALSE)

## KPI 4.1 Additional ----
write_kpi4_add(wb, "KPI 4.1 Additional", season, kpi_report_years, 
                 kpi_4_1_add_A, kpi_4_1_add_B, kpi_4_1_add_C, kpi_4_prov)

## KPI 4.2 Additional ----
write_kpi4_add(wb, "KPI 4.2 Additional", season, kpi_report_years, 
               kpi_4_2_add_A, kpi_4_2_add_B, kpi_4_2_add_C, kpi_4_prov)

## Table 7: Vascular Referrals ----
# notes
writeData(wb, sheet = "7) Vascular referrals", screened_year_vv, 
          startRow = 5, startCol = 2)
writeData(wb, sheet = "7) Vascular referrals", screened_year_ww, 
          startRow = 5, startCol = 4)
writeData(wb, sheet = "7) Vascular referrals", screened_year_xx, 
          startRow = 5, startCol = 6)
writeData(wb, sheet = "7) Vascular referrals", screened_cum, 
          startRow = 5, startCol = 8)
addStyle(wb, sheet = "7) Vascular referrals", styles$black_border_thin_centre_12,
          rows = 5, cols = 2:9, gridExpand = T)
if (season == "spring") {
  writeData(wb, sheet = "7) Vascular referrals", table_7_prov, 
            startRow = 15)
  addStyle(wb, sheet = "7) Vascular referrals", styles$black_nowrap_11,
           rows = 15, cols = 1)
}
# data
writeData(wb, sheet = "7) Vascular referrals", vasc_refs, 
          startRow = 7, startCol = 2, colNames = FALSE)
showGridLines(wb, "7) Vascular referrals", showGridLines = FALSE)

## Vascular Referral Outcomes ----
# notes
writeData(wb, sheet = "Vascular KPIs background", vasc_outcome_title, 
          startRow = 2, startCol = 1)
addStyle(wb, sheet = "Vascular KPIs background", styles$black_bold_nowrap_18, 
         rows = 2, cols = 1)
write_vasc_background(wb, "Vascular KPIs background", season, kpi_report_years, 
                      data_vasc_outcomes, vasc_outcome_prov)

## KPI 4 1,3,5-year Cumulative Mortality ----
# notes
writeData(wb, sheet = "1, 3, 5-year mortality", mort_year_cum1, 
          startRow = 5, startCol = 2)
writeData(wb, sheet = "1, 3, 5-year mortality", mort_year_cum3, 
          startRow = 5, startCol = 5)
writeData(wb, sheet = "1, 3, 5-year mortality", mort_year_cum5, 
          startRow = 5, startCol = 8)
addStyle(wb, sheet = "1, 3, 5-year mortality", styles$black_border_centre_12,
         rows = 5, cols = 2:10, gridExpand = T)
writeData(wb, sheet = "1, 3, 5-year mortality", mort_135_note, 
          startRow = 12, startCol = 1)
writeData(wb, sheet = "1, 3, 5-year mortality", mort_135_prov, 
          startRow = 14, startCol = 1)
addStyle(wb, sheet = "1, 3, 5-year mortality", styles$black_11,
         rows = 12:14, cols = 1, gridExpand = T)
# data
writeData(wb, sheet = "1, 3, 5-year mortality", kpi_4_cum_mort, 
          startRow = 8, colNames = FALSE)
showGridLines(wb, "1, 3, 5-year mortality", showGridLines = FALSE)

## KPI 4 1-year Mortality ----
# notes
writeData(wb, sheet = "1-year mortality rates", operations_title, 
          startRow = 3,  startCol = 1)
addStyle(wb, sheet = "1-year mortality rates", styles$black_bold_nowrap_14,
         rows = 3, cols = 1)
writeData(wb, sheet = "1-year mortality rates", mort_1_note, 
          startRow = 11, startCol = 1)
writeData(wb, sheet = "1-year mortality rates", mort_1_prov,
          startRow = 15, startCol = 1)
addStyle(wb, sheet = "1-year mortality rates", styles$black_11,
         rows = c(11, 15), cols = 1)
# data
writeData(wb, sheet = "1-year mortality rates", kpi_4_1yr_tail, 
          startRow = 8, colNames = FALSE)
addStyle(wb, sheet = "1-year mortality rates", createStyle(fontColour = "#000000"),
         rows = 8, cols = 1, stack = T)
showGridLines(wb, "1-year mortality rates", showGridLines = FALSE)

## AAA Repairs ----
# notes
writeData(wb, sheet = "AAA Repairs", ending_year_vv, 
          startRow = 5, startCol = 2)
writeData(wb, sheet = "AAA Repairs", ending_year_ww, 
          startRow = 5, startCol = 5)
writeData(wb, sheet = "AAA Repairs", ending_year_xx, 
          startRow = 5, startCol = 8)
writeData(wb, sheet = "AAA Repairs", screened_cum, 
          startRow = 5, startCol = 11)
addStyle(wb, sheet = "AAA Repairs", styles$black_border_centre_12,
         rows = 5, cols = 2:13, gridExpand = T)
if (season == "spring") {
  writeData(wb, sheet = "AAA Repairs", kpi_4_prov, 
            startRow = 26, startCol = 1)
  addStyle(wb, sheet = "AAA Repairs", styles$orange_11,
           rows = 26, cols = 1)
}
# data
writeData(wb, sheet = "AAA Repairs", aaa_repairs, 
          startRow = 7, colNames = FALSE)
showGridLines(wb, "AAA Repairs", showGridLines = FALSE)

## Unfit for Surgery ----
# notes
writeData(wb, sheet = "Unfit for surgery", refer_year_vv, 
          startRow = 4, startCol = 2)
writeData(wb, sheet = "Unfit for surgery", refer_year_ww, 
          startRow = 4, startCol = 5)
writeData(wb, sheet = "Unfit for surgery", refer_year_xx, 
          startRow = 4, startCol = 8)
writeData(wb, sheet = "Unfit for surgery", refer_year_cum, 
          startRow = 4, startCol = 11)
addStyle(wb, sheet = "Unfit for surgery", styles$black_border_centre_12,
         rows = 4, cols = 2:13, gridExpand = T)
if (season == "spring") {
  writeData(wb, sheet = "Unfit for surgery", unfit_prov, 
            startRow = 27, startCol = 1)
  addStyle(wb, sheet = "Unfit for surgery", styles$orange_11,
           rows = 27, cols = 1)
}
# data
writeData(wb, sheet = "Unfit for surgery", unfit_surgery, 
          startRow = 7,  colNames = FALSE)
showGridLines(wb, "Unfit for surgery", showGridLines = FALSE)

## Unfit for Surgery Follow-up ----
# notes
writeData(wb, sheet = "Unfit for surgery follow-up", unfit_year_cum1, 
          startRow = 4, startCol = 2)
writeData(wb, sheet = "Unfit for surgery follow-up", unfit_year_cum3, 
          startRow = 4, startCol = 5)
writeData(wb, sheet = "Unfit for surgery follow-up", unfit_year_cum5, 
          startRow = 4, startCol = 8)
addStyle(wb, sheet = "Unfit for surgery follow-up", styles$black_border_centre_12,
         rows = 4, cols = 2:10, gridExpand = T)
writeData(wb, sheet = "Unfit for surgery follow-up", mort_135_note, 
          startRow = 24, startCol = 1)
writeData(wb, sheet = "Unfit for surgery follow-up", mort_135_prov, 
          startRow = 26, startCol = 1)
addStyle(wb, sheet = "Unfit for surgery follow-up", styles$black_11,
         rows = 24:26, cols = 1, gridExpand = T)
# data
writeData(wb, sheet = "Unfit for surgery follow-up", unfit_followup, 
          startRow = 7, colNames = FALSE)
showGridLines(wb, "Unfit for surgery follow-up", showGridLines = FALSE)

## Unfit Follow-up Deaths ----
# notes
writeData(wb, sheet = "Unfit follow-up deaths by cause", refer_year_cum, 
          startRow = 4, startCol = 2)
writeData(wb, sheet = "Unfit follow-up deaths by cause", refer_year_cum, 
          startRow = 9, startCol = 2)
writeData(wb, sheet = "Unfit follow-up deaths by cause", refer_year_cum, 
          startRow = 22, startCol = 2)
addStyle(wb, sheet = "Unfit follow-up deaths by cause", styles$black_border_thin_centre_12,
         rows = c(4, 9, 22), cols = 2)
if (season == "spring") {
  writeData(wb, sheet = "Unfit follow-up deaths by cause", unfit_deaths_prov, 
            startRow = 33, startCol = 1)
  addStyle(wb, sheet = "Unfit follow-up deaths by cause", styles$black_11,
           rows = 33, cols = 1)
}
# data
writeData(wb, sheet = "Unfit follow-up deaths by cause", unfit_deaths1, 
          startRow = 5, colNames = FALSE)
writeData(wb, sheet = "Unfit follow-up deaths by cause", unfit_deaths2, 
          startRow = 10, colNames = FALSE)
writeData(wb, sheet = "Unfit follow-up deaths by cause", unfit_deaths3, 
          startRow = 23, colNames = FALSE)
showGridLines(wb, "Unfit follow-up deaths by cause", showGridLines = FALSE)

# 5: Save output ----
query_saveWorkbook(wb, paste0(output_path, "/4_Referral Treatment and Outcomes_",
                              yymm, ".xlsx"))




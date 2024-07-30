# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 93_Write_Excel_2.R
# 
# Karen Hotopp & Aoife McCarthy
# Sept 2023
# 
# Write out to AAA Excel workbook 2: Invitation and Attendance
# 
# Written/run on Posit WB, R 4.1.2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Notes:
# This script calls in the 3_invite_attend_yyyymm.rds file create in the
# 2_2_kpi_1_1-1_3_uptake_coverage.R script and transforms the data to print 
# directly into the theme 2 Excel file for the QPMG.

# 1: Housekeeping ----
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(openxlsx)
library(lubridate)
library(forcats)
library(phsaaa) # devtools::install_github("aoifem01/phsaaa")

rm(list=ls())
gc()

## Values
source(here::here("code/0_housekeeping.R"))

rm (exclusions_path, extract_path, hist_path, simd_path, fy_tibble, 
    hb_tibble, cutoff_date, end_current, end_date, start_date,
    year1_end, year1_start, year2_end, year2_start, year1)

year_xx <- year(cut_off_date)
year_ww <- year_xx - 1
year_vv <- year_xx - 2
year_uu <- year_xx - 3
year_yy <- year_xx + 1

## File paths
template_path <- paste0("/PHI_conf/AAA/Topics/Screening/templates")


### 2: Import and format data ----

theme2 <- read_rds(paste0(temp_path, "/2_1_invite_attend_", yymm, ".rds")) |> 
  mutate(simd = case_when(simd == "1" ~ "1 (most deprived)",
                          simd == "5" ~ "5 (least deprived)",
                          TRUE ~ simd))

table(theme2$kpi, theme2$fin_year) 
# should be 3 most recent complete years + incomplete/active year

theme2_t6 <- read_rds(paste0(temp_path, "/2_2_Table_6_", yymm, ".rds"))
table(theme2_t6$kpi, theme2_t6$fin_year) 

theme2_dna <- read_rds(paste0(temp_path, "/2_3_dna_exclusions_", yymm, ".rds"))
table(theme2_dna$kpi, theme2_dna$fin_year) 


# 3: Format data ----
## KPI 1.1 year1 ----
## Data for three most recent complete years and extended coverage to 1 Sept
kpi_1.1 <- theme2 |> 
  filter(kpi %in% c("KPI 1.1", "KPI 1.1 Sept coverage"),
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.1 year2 ----
kpi_1.1_y2 <- phsaaa::eval_seasonal_diff(
  season,
  {## Data for currently active year only
    theme2 |> 
      filter(kpi %in% c("KPI 1.1"),
             fin_year == year2) |> 
      mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
      select(hbres, FY_kpi_group, value) |>
      # match Excel output
      pivot_wider(names_from = FY_kpi_group, values_from = value)},
  {# Data for currently active year and extended coverage to 1 Sept
    theme2 |>
      filter(kpi %in% c("KPI 1.1", "KPI 1.1 Sept coverage"),
             fin_year == year2) |>
      mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
      select(hbres, FY_kpi_group, value) |>
      # match Excel output
      pivot_wider(names_from = FY_kpi_group, values_from = value)}
)

## KPI 1.1 Scotland SIMD ----
## Data for three most recent complete years and extended coverage to 1 Sept
kpi_1.1_simd <- theme2 |> 
  filter(kpi %in% c("KPI 1.1 Scotland SIMD", "KPI 1.1 Scotland SIMD Sept coverage"),
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, simd, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.2a year1 & coverage to Sept ----
## Data for currently active year and extended coverage to 1 Sept
kpi_1.2a <- theme2 |> 
  filter(kpi %in% c("KPI 1.2a", "KPI 1.2a Sept coverage"),
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

# Extract extended coverage to 1 Sept
kpi_1.2a_sept <- kpi_1.2a |>
  select(hbres, contains("_cohort"), contains("Sept coverage"))

kpi_1.2a_sept <- kpi_1.2a_sept[ , c(1, 2, 5, 6, 3, 7, 8, 4, 9, 10)] 

# Remove extra historical coverage to 1 Sept data
kpi_1.2a <- kpi_1.2a[, -c(8:11)]

## KPI 1.2a year2 ----

kpi_1.2a_y2 <- phsaaa::eval_seasonal_diff(
  season,
  {## Data for currently active year only
    theme2 |> 
      filter(kpi %in% c("KPI 1.2a"),
             fin_year == year2) |> 
      mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
      select(hbres, FY_kpi_group, value) |>
      # match Excel output
      pivot_wider(names_from = FY_kpi_group, values_from = value)},
  {theme2 |>
      filter(kpi %in% c("KPI 1.2a", "KPI 1.2a Sept coverage"),
             fin_year == year2) |>
      mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
      select(hbres, FY_kpi_group, value) |>
      # match Excel output
      pivot_wider(names_from = FY_kpi_group, values_from = value)}
)

## KPI 1.2b year1 ----
## Data for currently active year and extended coverage to 1 Sept
kpi_1.2b <- theme2 |> 
  filter(kpi == "KPI 1.2b",
         fin_year  %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.2b year2 ----
## Data for currently active year and extended coverage to 1 Sept
kpi_1.2b_y2 <- theme2 |> 
  filter(kpi == "KPI 1.2b",
         fin_year == year2) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.3a year1  by Scotland SIMD & coverage to Sept ----
## Data for currently active year and extended coverage to 1 Sept
kpi_1.3a <- theme2 |> 
  filter(kpi %in% c("KPI 1.3a Scotland SIMD", "KPI 1.3a Sept coverage"),
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, simd, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

# Extract extended coverage to 1 Sept
kpi_1.3a_sept <- kpi_1.3a |>
  select(hbres, simd, contains("_cohort"), contains("Sept coverage"))

kpi_1.3a_sept <- kpi_1.3a_sept[ , c(1, 2, 3, 6, 7, 4, 8, 9, 5, 10, 11)] 

# Remove extra historical coverage to 1 Sept data
kpi_1.3a <- kpi_1.3a[, -c(9:12)]

## KPI 1.3a year1 by HB SIMD ----
## Data for currently active year by HB SIMD
kpi_1.3a_hb <- theme2 |> 
  filter(kpi == "KPI 1.3a HB SIMD",
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, simd, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.3a year2 ----
kpi_1.3a_y2 <- phsaaa::eval_seasonal_diff(
  season,
  {## Data for currently active year only
    theme2 |> 
      filter(kpi %in% c("KPI 1.3a Scotland SIMD"),
             fin_year == year2,
             hbres ==  "Scotland") |> 
      mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
      select(hbres, simd, FY_kpi_group, value) |>
      # match Excel output
      pivot_wider(names_from = FY_kpi_group, values_from = value)},
  {# Data for currently active year and extended coverage to 1 Sept
    theme2 |>
      filter(kpi %in% c("KPI 1.3a Scotland SIMD", "KPI 1.3a Sept coverage"),
             fin_year == year2,
             hbres ==  "Scotland") |>
      mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
      select(hbres, simd, FY_kpi_group, value) |>
      # match Excel output
      pivot_wider(names_from = FY_kpi_group, values_from = value)}
)

kpi_1.3a_y2 <- select(kpi_1.3a_y2, -c(hbres, simd)) # to match Excel table

## KPI 1.3b year1 by Scotland SIMD ----
## Data for currently active year by Scotland SIMD
kpi_1.3b <- theme2 |> 
  filter(kpi == "KPI 1.3b Scotland SIMD",
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, simd, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.3b year1 by HB SIMD ----
## Data for currently active year by HB SIMD
kpi_1.3b_hb <- theme2 |> 
  filter(kpi == "KPI 1.3b HB SIMD",
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, simd, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.4a ----
## Data for three most recent complete years (and extended coverage to 1 Sept)
kpi_1.4a <- theme2 |> 
  filter(kpi == "KPI 1.4a",
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.4b ----
## Data for three most recent complete years (and extended coverage to 1 Sept)
kpi_1.4b <- theme2 |> 
  filter(kpi == "KPI 1.4b",
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value) |> 
  mutate(hbres = forcats::fct_relevel(hbres, hb_list)) |> 
  arrange(hbres)

## Table 6: Surveillance ----
## Data for three most recent complete years
t6_surveill <- theme2_t6 |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, surveillance_interval, sep = "_")) |>
  select(hbres, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## DNA Exclusions ----
## Data for all years
dna_exclude <- theme2_dna |> 
  filter(fin_year %in% c(fy_list)) |> 
  # remove the last two numbers and / from the financial year
  mutate(year = str_remove(fin_year, "[:digit:][:digit:][:punct:]")) |>
  select(pat_inelig, year, count) |>
  # match Excel output
  pivot_wider(names_from = year, values_from = count)


## Prisons ----


##!! KPI2 1.2a & 1.2b Prisons extract to go here


# 4: Write to Excel (openxlsx) ----
## Setup workbook ----
wb <- loadWorkbook(paste0(template_path, "/2_Invitation and Attendance_",
                          season, ".xlsx"))

source(here::here(paste0("code/write_excel/93_Source_Excel_2.R")))

## Table of Contents ----
writeData(wb, sheet = "Table of Contents", pub_year, 
          startRow = 3)
addStyle(wb, "Table of Contents", styles$black_nowrap_12, 
         rows = 3, cols = 1:2, gridExpand = TRUE)
writeData(wb, sheet = "Table of Contents", qpmg_note, 
          startRow = 4)
addStyle(wb, "Table of Contents", styles$black_bold_12, 
         rows = 4, cols = 1)
writeData(wb, sheet = "Table of Contents", today, 
          startRow = 6)
addStyle(wb, "Table of Contents", styles$black_nowrap_12, 
         rows = 6, cols = 1)
writeData(wb, sheet = "Table of Contents", tab_1.1_add, 
          startRow = 12)
writeData(wb, sheet = "Table of Contents", tab_1.1_add_desc, 
          startRow = 12, startCol = 2)
writeData(wb, sheet = "Table of Contents", tab_1.2a_add, 
          startRow = line_no_tab_1.2a_add)
writeData(wb, sheet = "Table of Contents", tab_1.2_add_desc, 
          startRow = line_no_tab_1.2a_add, startCol = 2)
writeData(wb, sheet = "Table of Contents", tab_1.2b_add, 
          startRow = line_no_tab_1.2b_add)
writeData(wb, sheet = "Table of Contents", tab_1.2b_add_desc, 
          startRow = line_no_tab_1.2b_add, startCol = 2)
addStyle(wb, "Table of Contents", styles$black_border_12, 
         rows = 11:24, cols = 1, gridExpand = TRUE)
addStyle(wb, "Table of Contents", styles$blue_border_underline_12, 
         rows = 11:24, cols = 2, gridExpand = TRUE)

writeData(wb, sheet = "Table of Contents", note_toc,
          startRow = line_no_note_toc)
addStyle(wb, "Table of Contents", styles$red_bold_12, 
         rows = line_no_note_toc, cols = 1)

showGridLines(wb, "Table of Contents", showGridLines = FALSE)

# options("openxlsx.dateFormat" = "dd/mm/yyyy")

## KPI 1.1 ----
# notes
writeData(wb, sheet = "KPI 1.1", turn66_year_vv,
          startRow = 4, startCol = 2)
writeData(wb, sheet = "KPI 1.1", turn66_year_ww,
          startRow = 4, startCol = 5)
writeData(wb, sheet = "KPI 1.1", turn66_year_xx,
          startRow = 4, startCol = 8)
addStyle(wb, "KPI 1.1", styles$black_border_centre_12, 
         rows = 4, cols = 2:12, gridExpand = TRUE)
writeData(wb, sheet = "KPI 1.1", kpi_1.1_head_mgmt,
          startRow = 5, startCol = 11)
addStyle(wb, "KPI 1.1", styles$blue_border_centre_12, 
         rows = 5, cols = 11)
if (season == "spring") {
  writeData(wb, "KPI 1.1", kpi_1.1_notep, 
            startRow = 29)
  addStyle(wb, "KPI 1.1", styles$black_11, 
           rows = 29, cols = 1)
  }
writeData(wb, sheet = "KPI 1.1", kpi_1.1_note2, 
          startRow = 31)
addStyle(wb, "KPI 1.1", styles$black_11, 
         rows = 31, cols = 1)
showGridLines(wb, "KPI 1.1", showGridLines = FALSE)
# data
writeData(wb, sheet = "KPI 1.1", kpi_1.1, 
          startRow = 7, colNames = FALSE)

## KPI 1.1 Additional (20XX-YY) ----
# notes
writeData(wb, sheet =  "KPI 1.1 Additional (20XX-YY)", add_cohort_note, 
          startRow = 3)
addStyle(wb, "KPI 1.1 Additional (20XX-YY)", styles$black_bold_14, 
         rows = 3, cols = 1)
writeData(wb, sheet =  "KPI 1.1 Additional (20XX-YY)", add_performance_note, 
          startRow = 4)
addStyle(wb, "KPI 1.1 Additional (20XX-YY)", styles$red_bold_12, 
         rows = 4, cols = 1)
writeData(wb, sheet = "KPI 1.1 Additional (20XX-YY)", turn66_year_yy, 
          startRow = 6, startCol = 2)
addStyle(wb, "KPI 1.1 Additional (20XX-YY)", styles$black_border_centre_12,
         rows = 6, cols = 2:4, gridExpand = TRUE)
if (season == "autumn") {
  writeData(wb, sheet =  "KPI 1.1 Additional (20XX-YY)", kpi_1.1_head_mgmt,
            startRow = 7, startCol = 5)
  addStyle(wb, "KPI 1.1 Additional (20XX-YY)", styles$blue_border_centre_12,
           rows = 7, cols = 5)
}
writeData(wb, sheet = "KPI 1.1 Additional (20XX-YY)", kpi_1.1_add_note1,
          startRow = 31)
addStyle(wb, "KPI 1.1 Additional (20XX-YY)", styles$orange_11,
         rows = 31, cols = 1)
showGridLines(wb, "KPI 1.1 Additional (20XX-YY)", showGridLines = FALSE)
# data
writeData(wb, sheet = "KPI 1.1 Additional (20XX-YY)", kpi_1.1_y2, 
          startRow = 9, colNames = FALSE)
names(wb)[[3]] <- paste0("KPI 1.1 Additional (", year2, ")")

## KPI 1.1 SIMD ----
# notes
writeData(wb, sheet = "KPI 1.1 SIMD", turn66_year_vv, 
          startRow = 4, startCol = 3)
writeData(wb, sheet = "KPI 1.1 SIMD", turn66_year_ww, 
          startRow = 4, startCol = 6)
writeData(wb, sheet = "KPI 1.1 SIMD", turn66_year_xx, 
          startRow = 4, startCol = 9)
addStyle(wb, "KPI 1.1 SIMD", styles$black_border_centre_12,
         rows = 4, cols = 3:13, gridExpand = TRUE)
writeData(wb, sheet =  "KPI 1.1 SIMD", kpi_1.1_head_mgmt,
          startRow = 5, startCol = 12)
addStyle(wb, "KPI 1.1 SIMD", styles$blue_border_centre_12, 
         rows = 5, cols = 12)
if (season == "spring") {
  writeData(wb, sheet = "KPI 1.1 SIMD", kpi_1.1_notep, 
            startRow = 119)
  addStyle(wb, "KPI 1.1 SIMD", styles$black_11,
           rows = 119, cols = 1)
}
writeData(wb, sheet = "KPI 1.1 SIMD", kpi_1.1_note2, 
          startRow = 121)
addStyle(wb, "KPI 1.1 SIMD", styles$black_11,
         rows = 121, cols = 1)
showGridLines(wb, "KPI 1.1 SIMD", showGridLines = FALSE)
# data
writeData(wb, sheet = "KPI 1.1 SIMD", kpi_1.1_simd, 
          startRow = 7, colNames = FALSE)

## KPI 1.2a ----
# notes
writeData(wb, sheet = "KPI 1.2a", turn66_year_vv, 
          startRow = 4, startCol = 2)
writeData(wb, sheet = "KPI 1.2a", turn66_year_ww, 
          startRow = 4, startCol = 5)
writeData(wb, sheet = "KPI 1.2a", turn66_year_xx, 
          startRow = 4, startCol = 8)
addStyle(wb, "KPI 1.2a", styles$black_border_centre_12,
         rows = 4, cols = 2:12, gridExpand = TRUE)
writeData(wb, sheet = "KPI 1.2a", kpi_1.2a_head_mgmt, 
          startRow = 5, startCol = 11)
addStyle(wb, "KPI 1.2a", styles$blue_border_centre_12, 
         rows = 5, cols = 11)
if(season == "spring") {
  writeData(wb, sheet = "KPI 1.2a", prov_data_note, 
            startRow = 30, colNames = FALSE)
  addStyle(wb, "KPI 1.2a", styles$black_11, 
           rows = 30, cols = 1)
}
# data
writeData(wb, sheet = "KPI 1.2a", kpi_1.2a, 
          startRow = 7, colNames = FALSE)
showGridLines(wb, "KPI 1.2a", showGridLines = FALSE)


## KPI 1.2a Coverage by 1 Sept ----
if (season == "autumn") {
  # notes
  writeData(wb, sheet = "Coverage by 1 Sept", turn66_year_vv,
            startRow = 4, startCol = 2)
  writeData(wb, sheet = "Coverage by 1 Sept", turn66_year_ww,
            startRow = 4, startCol = 5)
  writeData(wb, sheet = "Coverage by 1 Sept", turn66_year_xx,
            startRow = 4, startCol = 8)
  addStyle(wb, "Coverage by 1 Sept", styles$black_border_centre_12,
           rows = 4, cols = 2:10, gridExpand = TRUE)
  writeData(wb, sheet = "Coverage by 1 Sept", kpi_1.2a_head_mgmt,
            startRow = 5, startCol = 9)
  addStyle(wb, "Coverage by 1 Sept", styles$blue_border_centre_12,
           rows = 5, cols = 9, gridExpand = TRUE)
  writeData(wb, sheet = "Coverage by 1 Sept", sept_cov_note1,
            startRow = 24, startCol = 1)
  addStyle(wb, "Coverage by 1 Sept", styles$orange_11,
           rows = 24, cols = 1)
  # data
  writeData(wb, sheet = "Coverage by 1 Sept", kpi_1.2a_sept, 
            startRow = 7, colNames = FALSE)
  showGridLines(wb, "Coverage by 1 Sept", showGridLines = FALSE)
}


## KPI 1.2a Additional (20XX-YY) ----
# KPI 1.2a Additional
# notes
writeData(wb, sheet =  "KPI 1.2a Additional (20XX-YY)", add_cohort_note, 
          startRow = 3)
addStyle(wb, "KPI 1.2a Additional (20XX-YY)", styles$black_bold_14, 
         rows = 3, cols = 1)
writeData(wb, sheet =  "KPI 1.2a Additional (20XX-YY)", add_performance_note, 
          startRow = 4)
addStyle(wb, "KPI 1.2a Additional (20XX-YY)", styles$red_bold_12, 
         rows = 4, cols = 1)
writeData(wb, sheet = "KPI 1.2a Additional (20XX-YY)", turn66_year_yy, 
          startRow = 6, startCol = 2)
phsaaa::eval_seasonal_diff(
  season,
  {addStyle(wb, "KPI 1.2a Additional (20XX-YY)", styles$black_border_thin_centre_12, 
            rows = 6, cols = 2:4, gridExpand = TRUE)}, # spring 
  {addStyle(wb, "KPI 1.2a Additional (20XX-YY)", styles$black_border_thin_centre_12, 
            rows = 6, cols = 2:6, gridExpand = TRUE)} # autumn
)

if(season == "autumn"){
  writeData(wb, sheet = "KPI 1.2a Additional (20XX-YY)", kpi_1.2a_head_mgmt, 
            startRow = 7, startCol = 5)
  addStyle(wb, "KPI 1.2a Additional (20XX-YY)", styles$blue_border_centre_12, 
           rows = 7, cols = 5)
}
writeData(wb, sheet =  "KPI 1.2a Additional (20XX-YY)", kpi_1.2a_add_note1, 
          startRow = 26)
addStyle(wb, "KPI 1.2a Additional (20XX-YY)", styles$orange_11,
         rows = 26, cols = 1)
writeData(wb, sheet =  "KPI 1.2a Additional (20XX-YY)", kpi_1.2a_add_note2, 
          startRow = 27)
addStyle(wb, "KPI 1.2a Additional (20XX-YY)", styles$black_11,
         rows = 27, cols = 1)
# data
writeData(wb, sheet = "KPI 1.2a Additional (20XX-YY)", kpi_1.2a_y2, 
          startRow = 9, colNames = FALSE)

# KPI 1.3a Additional
# notes
writeData(wb, sheet = "KPI 1.2a Additional (20XX-YY)", turn66_year_yy,
          startRow = 32, startCol = 2)
phsaaa::eval_seasonal_diff(
  season,
  {addStyle(wb, "KPI 1.2a Additional (20XX-YY)", styles$black_border_thin_centre_12,
            rows = 32, cols = 2:4, gridExpand = TRUE)}, # spring
  {addStyle(wb, "KPI 1.2a Additional (20XX-YY)", styles$black_border_thin_centre_12,
            rows = 32, cols = 2:6, gridExpand = TRUE)} # autumn
)
if (season == "autumn") {
  writeData(wb, sheet = "KPI 1.2a Additional (20XX-YY)", kpi_1.2a_head_mgmt,
            startRow = 33, startCol = 6)
  addStyle(wb, "KPI 1.2a Additional (20XX-YY)", styles$blue_border_centre_12,
           rows = 33, cols = 6)
}

# data
writeData(wb, sheet = "KPI 1.2a Additional (20XX-YY)", kpi_1.3a_y2, 
          startRow = 35, startCol = 2, colNames = FALSE)
showGridLines(wb, "KPI 1.2a Additional (20XX-YY)", showGridLines = FALSE)
names(wb)[[6]] <- paste0("KPI 1.2a Additional (", year2, ")")

## KPI 1.2b ----
# notes
writeData(wb, sheet = "KPI 1.2b", turn66_year_vv, 
          startRow = 4, startCol = 2)
writeData(wb, sheet = "KPI 1.2b", turn66_year_ww, 
          startRow = 4, startCol = 5)
writeData(wb, sheet = "KPI 1.2b", turn66_year_xx, 
          startRow = 4, startCol = 8)
addStyle(wb, "KPI 1.2b", styles$black_border_centre_12,
         rows = 4, cols = 2:10, gridExpand = TRUE)
if(season == "spring") {
  writeData(wb, sheet = "KPI 1.2b", prov_data_note, 
            startRow = 30, colNames = FALSE)
  addStyle(wb, "KPI 1.2b", styles$black_11,
           rows = 30, cols = 1)
}
# data
writeData(wb, sheet = "KPI 1.2b", kpi_1.2b, 
          startRow = 7, colNames = FALSE)
showGridLines(wb, "KPI 1.2b", showGridLines = FALSE)

## KPI 1.2b Additional (20YY-YY) ----
# notes
writeData(wb, sheet =  "KPI 1.2b Additional (20XX-YY)", add_cohort_note, 
          startRow = 3)
addStyle(wb, "KPI 1.2b Additional (20XX-YY)", styles$black_bold_14, 
         rows = 3, cols = 1)
writeData(wb, sheet =  "KPI 1.2b Additional (20XX-YY)", add_performance_note, 
          startRow = 4)
addStyle(wb, "KPI 1.2b Additional (20XX-YY)", styles$red_bold_12, 
         rows = 4, cols = 1)
writeData(wb, sheet = "KPI 1.2b Additional (20XX-YY)", turn66_year_yy, 
          startRow = 6, startCol = 2)
addStyle(wb, "KPI 1.2b Additional (20XX-YY)", styles$black_border_centre_12,
         rows = 6, cols = 2:4, gridExpand = TRUE)
writeData(wb, sheet = "KPI 1.2b Additional (20XX-YY)", kpi_1.2badd_foot, 
          startRow = 31)
addStyle(wb, "KPI 1.2b Additional (20XX-YY)", styles$orange_11, 
         rows = 31, cols = 1)
# data
showGridLines(wb, "KPI 1.2b Additional (20XX-YY)", showGridLines = FALSE)
writeData(wb, sheet = "KPI 1.2b Additional (20XX-YY)", kpi_1.2b_y2, 
          startRow = 9, colNames = FALSE)
names(wb)[[8]] <- paste0("KPI 1.2b Additional (", year2, ")")

## KPI 1.3a ----
# notes
writeData(wb, sheet = "KPI 1.3a", turn66_year_vv, 
          startRow = 4, startCol = 3)
writeData(wb, sheet = "KPI 1.3a", turn66_year_ww, 
          startRow = 4,  startCol = 6)
writeData(wb, sheet = "KPI 1.3a", turn66_year_xx, 
          startRow = 4,  startCol = 9)
addStyle(wb, "KPI 1.3a", styles$black_border_centre_12, 
         rows = 4, cols = 3:13, gridExpand = TRUE)
writeData(wb, sheet = "KPI 1.3a", kpi_1.2a_head_mgmt, 
          startRow = 5, startCol = 12)
addStyle(wb, "KPI 1.3a", styles$blue_border_centre_12, 
         rows = 5, cols = 12)
if (season == "spring") {
  writeData(wb, sheet = "KPI 1.3a", prov_data_note, 
            startRow = 120)
  addStyle(wb, "KPI 1.3a", styles$black_11,
           rows = 120, cols = 1)
}
# data
writeData(wb, sheet = "KPI 1.3a", kpi_1.3a, 
          startRow = 7, colNames = FALSE)
showGridLines(wb, "KPI 1.3a", showGridLines = FALSE)

# autumn only
## KPI 1.3a Coverage by 1 Sept by SIMD ----

if (season == "autumn") {
  # notes
  writeData(wb, sheet = "Coverage by 1 Sept by SIMD", turn66_year_vv,
            startRow = 4, startCol = 3)
  writeData(wb, sheet = "Coverage by 1 Sept by SIMD", turn66_year_ww,
            startRow = 4, startCol = 6)
  writeData(wb, sheet = "Coverage by 1 Sept by SIMD", turn66_year_xx,
            startRow = 4, startCol = 9)
  addStyle(wb, "Coverage by 1 Sept by SIMD", styles$black_border_centre_12,
           rows = 4, cols = 3:11, gridExpand = TRUE)
  # data
  writeData(wb, sheet = "Coverage by 1 Sept by SIMD", kpi_1.3a_sept, 
            startRow = 7, colNames = FALSE)
  showGridLines(wb, "Coverage by 1 Sept by SIMD", showGridLines = FALSE)
}


## KPI 1.3a HB SIMD ----
# notes
writeData(wb, sheet = "KPI 1.3a HB SIMD", turn66_year_vv, 
          startRow = 5, startCol = 3)
writeData(wb, sheet = "KPI 1.3a HB SIMD", turn66_year_ww, 
          startRow = 5, startCol = 6)
writeData(wb, sheet = "KPI 1.3a HB SIMD", turn66_year_xx, 
          startRow = 5, startCol = 9)
addStyle(wb, "KPI 1.3a HB SIMD", styles$black_border_centre_12, 
         rows = 5, cols = 3:11, gridExpand = TRUE)
if (season == "spring") {
  writeData(wb, sheet = "KPI 1.3a HB SIMD", prov_data_note, 
            startRow = 114, colNames = FALSE)
  addStyle(wb, "KPI 1.3a HB SIMD", black_11, 
           rows = 114, cols = 1)
}
# data
writeData(wb, sheet = "KPI 1.3a HB SIMD", kpi_1.3a_hb, 
          startRow = 8, colNames = FALSE)
showGridLines(wb, "KPI 1.3a HB SIMD", showGridLines = FALSE)

## KPI 1.3b ----
writeData(wb, sheet = "KPI 1.3b", turn66_year_vv, 
          startRow = 4, startCol = 3)
writeData(wb, sheet = "KPI 1.3b", turn66_year_ww, 
          startRow = 4, startCol = 6)
writeData(wb, sheet = "KPI 1.3b", turn66_year_xx, 
          startRow = 4, startCol = 9)
addStyle(wb, "KPI 1.3b", styles$black_border_centre_12,
         rows = 4, cols = 3:11, gridExpand = TRUE)
if (season == "spring") {
  writeData(wb, sheet = "KPI 1.3b", prov_data_note, 
            startRow = 120, colNames = FALSE)
  addStyle(wb, "KPI 1.3b", styles$black_11,
           rows = 120, cols = 1)
}
# data
writeData(wb, sheet = "KPI 1.3b", kpi_1.3b, 
          startRow = 7, colNames = FALSE)
showGridLines(wb, "KPI 1.3b", showGridLines = FALSE)


## KPI 1.3b HB SIMD ----
if (season == "autumn") {
  # notes
  writeData(wb, sheet = "KPI 1.3b HB SIMD", turn66_year_vv, 
            startRow = 5, startCol = 3)
  writeData(wb, sheet = "KPI 1.3b HB SIMD", turn66_year_ww, 
            startRow = 5, startCol = 6)
  writeData(wb, sheet = "KPI 1.3b HB SIMD", turn66_year_xx, 
            startRow = 5, startCol = 9)
  addStyle(wb, "KPI 1.3b HB SIMD", styles$black_border_centre_12, 
           rows = 5, cols = 3:11, gridExpand = TRUE)
  # data
  writeData(wb, sheet = "KPI 1.3b HB SIMD", kpi_1.3b_hb, 
            startRow = 8, colNames = FALSE)
  showGridLines(wb, "KPI 1.3b HB SIMD", showGridLines = FALSE)
}


## KPI 1.4a ----
# notes
writeData(wb, sheet = "KPI 1.4a", kpi_1.4a_head1, 
          startRow = 4, startCol = 2)
writeData(wb, sheet = "KPI 1.4a", kpi_1.4a_head2, 
          startRow = 4, startCol = 5)
writeData(wb, sheet = "KPI 1.4a", kpi_1.4a_head3, 
          startRow = 4, startCol = 8)
addStyle(wb, "KPI 1.4a", styles$black_border_centre_12,
         rows = 4, cols = 2:10, gridExpand = TRUE)
if (season == "spring") {
  writeData(wb, sheet = "KPI 1.4a", prov_data_note, 
            startRow = 29, colNames = FALSE)
  writeData(wb, sheet = "KPI 1.4a", kpi_1.4a_note1, 
            startRow = 30, colNames = FALSE)
  addStyle(wb, "KPI 1.4a", styles$black_11,
           rows = 29:30, cols = 1, gridExpand = TRUE)
}
# data
writeData(wb, sheet = "KPI 1.4a", kpi_1.4a, 
          startRow = 7, colNames = FALSE)
showGridLines(wb, "KPI 1.4a", showGridLines = FALSE)


## KPI 1.4b ----
# notes
writeData(wb, sheet = "KPI 1.4b", kpi_1.4b_head1, 
          startRow = 4, startCol = 2)
writeData(wb, sheet = "KPI 1.4b", kpi_1.4b_head2, 
          startRow = 4, startCol = 5)
writeData(wb, sheet = "KPI 1.4b", kpi_1.4b_head3, 
          startRow = 4, startCol = 8)
addStyle(wb, "KPI 1.4b", styles$black_border_centre_12,
         rows = 4, cols = 2:20, gridExpand = TRUE)
if (season == "spring") {
  writeData(wb, sheet = "KPI 1.4b", prov_data_note, 
            startRow = 29, colNames = FALSE)
  writeData(wb, sheet = "KPI 1.4b", kpi_1.4a_note1, 
            startRow = 30, colNames = FALSE)
  addStyle(wb, "KPI 1.4b", styles$black_11,
           rows = 29:30, cols = 1, gridExpand = TRUE)
}
# data
writeData(wb, sheet = "KPI 1.4b", kpi_1.4b, 
          startRow = 7, colNames = FALSE)
showGridLines(wb, "KPI 1.4b", showGridLines = FALSE)


## Table 6: Surveillance----
# notes
writeData(wb, sheet = "6) Surveillance", table6_head1, 
          startRow = 6, startCol = 2)
writeData(wb, sheet = "6) Surveillance", table6_head2, 
          startRow = 6, startCol = 4)
writeData(wb, sheet = "6) Surveillance", table6_head3, 
          startRow = 6, startCol = 6)
addStyle(wb, "6) Surveillance", styles$black_border_centre_12,
         rows = 6, cols = 2:7, gridExpand = TRUE)

if (season == "spring") {
  writeData(wb, sheet = "KPI 1.4b", prov_data_note, 
            startRow = 25, colNames = FALSE)
  addStyle(wb, "6) Surveillance", styles$black_11,
           rows = 25, cols = 1)
}
# data
writeData(wb, sheet = "6) Surveillance", t6_surveill,
          startRow = 8, colNames = FALSE)
showGridLines(wb, "6) Surveillance", showGridLines = FALSE)


## DNA Exclusions ----
# notes
if (season == "spring") {
  writeData(wb, sheet = "DNA Exclusions", dna_note1,
            startRow = 10, colNames = FALSE)
  addStyle(wb, "DNA Exclusions", styles$black_11,
           rows = 10, cols = 1)
}
# data
writeData(wb, sheet = "DNA Exclusions", dna_exclude,
          startRow = 6, colNames = FALSE)
showGridLines(wb, "DNA Exclusions", showGridLines = FALSE)

## Prisons ----
# if (season == "autumn") {
#   writeData(wb, sheet = "KPI 1.2a 1.2b Prisons", kpi_1.2a_prisons,
#             startRow = 7, colNames = FALSE)
#   writeData(wb, sheet = "KPI 1.2a 1.2b Prisons", kpi_1.2b_prisons,
#             startRow = 16, colNames = FALSE)
# }

 
# 5: Save output ----
phsaaa::query_saveWorkbook(wb, paste0(output_path, "/2_Invitation and Attendance_",
                                      yymm, ".xlsx"))


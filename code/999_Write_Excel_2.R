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


### 3: Format data ----
## KPI 1.1 year1 ----
## Data for three most recent complete years and extended coverage to 1 Sept
kpi_1.1 <- theme2 |> 
  filter(kpi %in% c("KPI 1.1", "KPI 1.1 Sept coverage"),
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value)

# Pivot wider to match Excel output
kpi_1.1 <- kpi_1.1 |> 
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.1 year2 ----
## Data for currently active year and extended coverage to 1 Sept
kpi_1.1_y2 <- theme2 |> 
  filter(kpi %in% c("KPI 1.1", "KPI 1.1 Sept coverage"),
         fin_year == year2) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value)

# Pivot wider to match Excel output
kpi_1.1_y2 <- kpi_1.1_y2 |> 
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.2a year1 & coverage to Sept ----
## Data for currently active year and extended coverage to 1 Sept
kpi_1.2a <- theme2 |> 
  filter(kpi %in% c("KPI 1.2a", "KPI 1.2a Sept coverage"),
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, FY_kpi_group, value)

# Pivot wider to match Excel output
kpi_1.2a <- kpi_1.2a |>
  pivot_wider(names_from = FY_kpi_group, values_from = value)

# Extract extended coverage to 1 Sept
kpi_1.2a_sept <- kpi_1.2a |>
  select(hbres, contains("_cohort"), contains("Sept coverage"))

kpi_1.2a_sept <- kpi_1.2a_sept[ , c(1, 2, 5, 6, 3, 7, 8, 4, 9, 10)] 

# Remove extra historical coverage to 1 Sept data
kpi_1.2a <- kpi_1.2a[, -c(8:11)]

## KPI 1.2a year2 ----
## Data for currently active year and extended coverage to 1 Sept
kpi_1.2a_y2 <- theme2 |> 
  filter(kpi %in% c("KPI 1.2a", "KPI 1.2a Sept coverage"),
         fin_year == year2) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value)

# Pivot wider to match Excel output
kpi_1.2a_y2 <- kpi_1.2a_y2 |> 
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.2b year1 ----
## Data for currently active year and extended coverage to 1 Sept
kpi_1.2b <- theme2 |> 
  filter(kpi == "KPI 1.2b",
         fin_year  %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value)

# Pivot wider to match Excel output
kpi_1.2b <- kpi_1.2b |> 
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.2b year2 ----
## Data for currently active year and extended coverage to 1 Sept
kpi_1.2b_y2 <- theme2 |> 
  filter(kpi == "KPI 1.2b",
         fin_year == year2) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value)

# Pivot wider to match Excel output
kpi_1.2b_y2 <- kpi_1.2b_y2 |> 
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.3a year1  by Scotland SIMD & coverage to Sept ----
## Data for currently active year and extended coverage to 1 Sept
kpi_1.3a <- theme2 |> 
  filter(kpi %in% c("KPI 1.3a Scotland SIMD", "KPI 1.3a Sept coverage"),
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, simd, FY_kpi_group, value)

# Pivot wider to match Excel output
kpi_1.3a <- kpi_1.3a |>
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
  select(hbres, simd, FY_kpi_group, value)

# Pivot wider to match Excel output
kpi_1.3a_hb <- kpi_1.3a_hb |>
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.3a year2 ----
## Data for currently active year and extended coverage to 1 Sept
kpi_1.3a_y2 <- theme2 |> 
  filter(kpi %in% c("KPI 1.3a Scotland SIMD", "KPI 1.3a Sept coverage"),
         fin_year == year2,
         hbres ==  "Scotland") |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, simd, FY_kpi_group, value)

# Pivot wider to match Excel output
kpi_1.3a_y2 <- kpi_1.3a_y2 |> 
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.3b year1 by Scotland SIMD ----
## Data for currently active year by Scotland SIMD
kpi_1.3b <- theme2 |> 
  filter(kpi == "KPI 1.3b Scotland SIMD",
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, simd, FY_kpi_group, value)

# Pivot wider to match Excel output
kpi_1.3b <- kpi_1.3b |>
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.3b year1 by HB SIMD ----
## Data for currently active year by HB SIMD
kpi_1.3b_hb <- theme2 |> 
  filter(kpi == "KPI 1.3b HB SIMD",
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, simd, FY_kpi_group, value)

# Pivot wider to match Excel output
kpi_1.3b_hb <- kpi_1.3b_hb |>
  pivot_wider(names_from = FY_kpi_group, values_from = value)

##!! KPIs 1.4a & 1.4b to go here, as well as Table 6) Surveillance, 
##!! DNA Exclusions, and KPI2 1.2a & 1.2b Prisoners extract


### 4: Write to Excel ----
### Setup workbook ---
# ## Styles
# table_style <- createStyle(valign = "Bottom", halign = "Left",
#                            border = "TopBottomLeftRight")

wb <- loadWorkbook(paste0(template_path, "/2_Invitation & Attendance_",
                          season, ".xlsx"))
# options("openxlsx.dateFormat" = "dd/mm/yyyy")
today <- paste0("Workbook created ", Sys.Date())

### Table of Contents ---
writeData(wb, sheet = "Table of Contents", today, startRow = 6)

### KPI 1.1 ---
writeData(wb, sheet = "KPI 1.1", kpi_1.1, startRow = 7, colNames = FALSE)

### KPI 1.1 Additional (20YY-YY) ---
writeData(wb, sheet = "KPI 1.1 Additional (20YY-YY)", 
          kpi_1.1_y2, startRow = 9, colNames = FALSE)



### Save ----
saveWorkbook(wb, paste0(output_path, "/2_Invitation & Attendance_", 
                        yymm, ".xlsx"), overwrite = TRUE)

## Workbook is corrupt... :/
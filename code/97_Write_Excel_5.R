# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 9993_Write_Excel_5.R
# 
# Karen Hotopp
# Oct 2023
# 
# Write out to AAA Excel workbook 5: Results
# 
# Written/run on Posit WB, R 4.1.2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Notes:
# This script calls in the multiple RDS files create in the XXXX script 
# and transforms the data to print directly into the theme 4 Excel file for 
# both the spring and autumn MEGs.


#### 1: Housekeeping ----
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(reporter)
library(openxlsx)


rm(list=ls())
gc()


## Values
source(here::here("code/0_housekeeping.R"))

rm (exclusions_path, extract_path, hist_path, simd_path, 
    fy_list, hb_list, fy_tibble, hb_tibble, kpi_report_years,
    cutoff_date, end_current, end_date, start_date,
    year1_end, year1_start, year2_end, year2_start, year1, year2, yymm)

## Define reporting years
year_xx <- year(cut_off_date)
year_ww <- year_xx - 1
year_vv <- year_xx - 2
year_uu <- year_xx - 3
meg_month <- "December"


## File paths
template_path <- paste0("/PHI_conf/AAA/Topics/Screening/templates")


### 2: Import data ----
# KPI 3.1 and 3.2
theme_5 <- read_rds(paste0(temp_path, "/5_1_results_tables_", yymm, ".rds"))
table(theme_5$table, theme_5$year_screen) 


### 3: Format data ----
## Table 1 ----
# Calculate totals for table_one_data by hbres and result_type
table_one <- theme_5 |> 
  filter(table == "Table 1") |> 
  pivot_wider(names_from = c(year_screen, group), 
              values_from = value) |>  
  select(-c(table, simd2020v2_sc_quintile)) |> 
  mutate_all(~ifelse(is.na(.), 0, .)) |> 
  # move Scotland to top of HB list, then alphabetical order
  arrange(hbres != "Scotland", hbres)

## Table 2 ----
table_two <- theme_5 |> 
  filter(table == "Table 2") |> 
  pivot_wider(names_from = c(year_screen, group), 
              values_from = value) %>% 
  select(-c(table, simd2020v2_sc_quintile)) |> 
  mutate_all(~ifelse(is.na(.), 0, .)) |> 
  # move Scotland to top of HB list, then alphabetical order
  arrange(hbres != "Scotland", hbres)

## Table 3 ----
table_three <- theme_5 |> 
  filter(table == "Table 3") |> 
  pivot_wider(names_from = c(year_screen, group), 
              values_from = value) %>% 
  select(-c(hbres, table))  |> 
  mutate_all(~ifelse(is.na(.), 0, .))%>% 
  arrange(simd2020v2_sc_quintile != "Scotland", simd2020v2_sc_quintile)

## Table 5 ----
table_five <- theme_5 |> 
  filter(table == "Table 5") |> 
  pivot_wider(names_from = c(year_screen, group), 
              values_from = value) %>% 
  select(-c(table, simd2020v2_sc_quintile)) |> 
  mutate_all(~ifelse(is.na(.), 0, .)) %>% 
  arrange(hbres != "Scotland", hbres)



### 4: Write to Excel ----
### Setup workbook ---
## Notes and Headers
today <- paste0("Workbook created ", Sys.Date())
meg_review <- paste0("For review at MEG in ", meg_month, " ", year_xx)

## Seasonal
if (season == "spring") {
  
  # Spring
  pub_year <- paste0("KPI data for year ending 31 March ", year_xx, " is scheduled ",
                     "to be published in March ", year_xx, ". Final data will be ",
                     "produced from data extracted for PHS in September ", year_xx, ".")
  report_type <- "Provisional/partial data"
  report_type_style <- createStyle(fontSize = 12, fontName = "Arial",
                                   textDecoration = "bold", fontColour = "#FF0000")
  note_toc <- paste0("The provisional/partial data for the year ending 31 March ", 
                     year_xx, " are released for data quality assurance and ",
                     "management information purposes and should not be placed in ",
                     "the public domain. The information can be shared locally with ",
                     "those who have a legitimate need to review the data for ",
                     "quality assurance, managerial or operational purposes.")
  result_type <- "Management information -- provisional"
  
} else {
  if (season == "autumn") {
    
    # Autumn
    pub_year <- paste0("KPI data for year ending 31 March ", year_xx, " and some ",
                       "supplementary information are planned for publication in March ", year_xx)
    report_type <- "Due for publication"
    report_type_style <- createStyle(fontSize = 12, fontName = "Arial",
                                     textDecoration = "bold", fontColour = "#000000")
    note_toc <- paste0("The data for the year ending 31 March ", year_xx, 
                       " are released for data quality assurance and management ",
                       "information purposes and should not be placed in the public ",
                       "domain. The information can be shared locally with those who ",
                       "have a legitimate need to review the data for quality ",
                       "assurance, managerial or operational purposes.")
    result_type <- "Due for publication"
    
  } else {
    
    print("Go check your calendar!")
    
  }
}


# Tables 1, 2, & 3
turn66_year_vv <- paste0("Turned 66 in year ending 31 March ", year_vv, '\n',
                         "(became eligible in year ending 31 March ", year_uu, ")")
turn66_year_vv_r <- paste0("Turned 66 in year ending 31 March ", year_vv, {supsc('r')}, '\n',
                         "(became eligible in year ending 31 March ", year_uu, ")")
turn66_year_ww_r <- paste0("Turned 66 in year ending 31 March ", year_ww, {supsc('r')}, '\n',
                         "(became eligible in year ending 31 March ", year_vv, ")")
turn66_year_xx <- paste0("Turned 66 in year ending 31 March ", year_xx, '\n',
                         "(became eligible in year ending 31 March ", year_ww, ")")
turn66_year_cum <- paste0("Cumulative total from implementation to 31 March ", 
                          year_xx, '\n', "(became eligible to 31 March ",
                          year_xx, ")")

# Table 5
screened_year_vv <- paste0("Screened in year ending 31 March ", year_vv)
screened_year_ww <- paste0("Screened in year ending 31 March ", year_ww)
screened_year_xx <- paste0("Screened in year ending 31 March ", year_xx)
screened_year_cum <- paste0("Cumulative total from implementation to 31 March ", 
                            year_xx)

## Load workbook
wb <- loadWorkbook(paste0(template_path, "/5_Results.xlsx"))

rm(theme_5, temp_path)


## Table of Contents ---
writeData(wb, "Table of Contents", pub_year, startRow = 3)
writeData(wb, "Table of Contents", meg_review, startRow = 4)
writeData(wb, "Table of Contents", report_type, startRow = 5)
writeData(wb, "Table of Contents", today, startRow = 6)
writeData(wb, "Table of Contents", note_toc, startRow = 16)

addStyle(wb, "Table of Contents", style = report_type_style, rows = 5, cols = 1)

showGridLines(wb, "Table of Contents", showGridLines = FALSE)

## Table 1 ---
writeData(wb, sheet = "1) Eligible cohort results", table_one, 
          startRow = 9, colNames = FALSE)
writeData(wb, "1) Eligible cohort results", result_type, startRow = 5, 
          startCol = 8)
writeData(wb, "1) Eligible cohort results", turn66_year_vv_r, startRow = 6, 
          startCol = 2)
writeData(wb, "1) Eligible cohort results", turn66_year_ww_r, startRow = 6, 
          startCol = 5)
writeData(wb, "1) Eligible cohort results", turn66_year_xx, startRow = 6, 
          startCol = 8)
writeData(wb, "1) Eligible cohort results", turn66_year_cum, startRow = 6, 
          startCol = 11)

showGridLines(wb, "1) Eligible cohort results", showGridLines = FALSE)

## Table 2 ---
writeData(wb, sheet = "2) Eligible cohort AAA size", table_two, 
          startRow = 10, colNames = FALSE)
writeData(wb, "2) Eligible cohort AAA size", result_type, startRow = 5, 
          startCol = 16)
writeData(wb, "2) Eligible cohort AAA size", turn66_year_vv, startRow = 6, 
          startCol = 2)
writeData(wb, "2) Eligible cohort AAA size", turn66_year_ww_r, startRow = 6, 
          startCol = 9)
writeData(wb, "2) Eligible cohort AAA size", turn66_year_xx, startRow = 6, 
          startCol = 16)
writeData(wb, "2) Eligible cohort AAA size", turn66_year_cum, startRow = 6, 
          startCol = 23)

showGridLines(wb, "2) Eligible cohort AAA size", showGridLines = FALSE)

## Table 3 ---
writeData(wb, sheet = "3) Positive results by SIMD", table_three, 
          startRow = 9, colNames = FALSE)
writeData(wb, "3) Positive results by SIMD", result_type, startRow = 5, 
          startCol = 8)
writeData(wb, "3) Positive results by SIMD", turn66_year_vv_r, startRow = 6, 
          startCol = 2)
writeData(wb, "3) Positive results by SIMD", turn66_year_ww_r, startRow = 6, 
          startCol = 5)
writeData(wb, "3) Positive results by SIMD", turn66_year_xx, startRow = 6, 
          startCol = 8)
writeData(wb, "3) Positive results by SIMD", turn66_year_cum, startRow = 6, 
          startCol = 11)

showGridLines(wb, "3) Positive results by SIMD", showGridLines = FALSE)

## Table 5 ---
writeData(wb, sheet = "5) Self-referral results", table_five, 
          startRow = 9, colNames = FALSE)
writeData(wb, "5) Self-referral results", result_type, startRow = 5, 
          startCol = 8)
writeData(wb, "5) Self-referral results", screened_year_vv, startRow = 6, 
          startCol = 2)
writeData(wb, "5) Self-referral results", screened_year_ww, startRow = 6, 
          startCol = 5)
writeData(wb, "5) Self-referral results", screened_year_xx, startRow = 6, 
          startCol = 8)
writeData(wb, "5) Self-referral results", screened_year_cum, startRow = 6, 
          startCol = 11)

showGridLines(wb, "5) Self-referral results", showGridLines = FALSE)

## Save ----
saveWorkbook(wb, paste0(output_path,
                        "/5_Results for Eligible",
                        "and Self-referrals_", yymm, ".xlsx"), 
             overwrite = TRUE)

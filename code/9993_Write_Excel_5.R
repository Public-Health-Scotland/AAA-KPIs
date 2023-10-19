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
# This script calls in the multiple RDS files create in the XXXX scripts 
# and transforms the data to print directly into the theme 4 Excel file for 
# both the spring and autumn MEGs.


#### 1: Housekeeping ----
library(dplyr)
library(readr)
library(tidyr)
library(openxlsx)


rm(list=ls())
gc()


## Values
source(here::here("code/0_housekeeping_.R"))

rm(hb_list, fy_tibble, fy_list, extract_path)


## File paths
template_path <- paste0("/PHI_conf/AAA/Topics/Screening/templates")


### 2: Import data ----
# KPI 3.1 and 3.2
theme4_3 <- read_rds(paste0(temp_path, "/4_1_kpi_3_", yymm, ".rds"))
table(theme4_3$kpi, theme4_3$financial_year) 

# KPI 4.1 and 4.2
theme4_4 <- read_rds(paste0(temp_path, "/4_2_kpi_4_", yymm, ".rds"))
table(theme4_4$kpi, theme4_4$surg_method) # GO BACK AND CHANGE IN 9_4_kpi_4.R script




### 3: Format data ----
## Table 1 ----
# Calculate totals for table_one_data by hbres and result_type
table_one <- theme_5 |> 
  filter(table = "Table 1") |> 
  pivot_wider(names_from = year_screen, 
                         values_from = c(cohort_n, positive, positive_p), 
                         names_glue = "{year_screen}_{.value}") %>% 
  select(hbres, starts_with(kpi_report_years[1]), 
         starts_with(kpi_report_years[2]), starts_with(kpi_report_years[3]), 
         starts_with("Cumulative")) %>% 
  # move Scotland to top of HB list, then alphabetical order
  arrange(hbres != "Scotland", hbres)

## Table 2 ----
table_two <- theme_5 |> 
  filter(table = "Table 2") |> 
  pivot_wider(names_from = year_screen, 
              values_from = c(cohort_n, small, small_p, medium, 
                              medium_p, large, large_p), 
              names_glue = "{year_screen}_{.value}") %>% 
  select(hbres, starts_with(kpi_report_years[1]), starts_with(kpi_report_years[2]), 
         starts_with(kpi_report_years[3]), starts_with("Cumulative")) %>% 
  # move Scotland to top of HB list, then alphabetical order
  arrange(hbres != "Scotland", hbres)

## Table 3 ----
table_three <- theme_5 |> 
  filter(table = "Table 3") |> 
  pivot_wider(names_from = year_screen, 
              values_from = c(cohort_n, positive_n, positive_p), 
              names_glue = "{year_screen}_{.value}") %>% 
  select(simd2020v2_sc_quintile, starts_with(kpi_report_years[1]), 
         starts_with(kpi_report_years[2]), starts_with(kpi_report_years[3]), 
         starts_with("Cumulative")) %>% 
  arrange(simd2020v2_sc_quintile != "Scotland", simd2020v2_sc_quintile)

## Table 5 ----
table_five <- theme_5 |> 
  filter(table = "Table 5") #|> 
  








### 4: Write to Excel (openxlsx) ----
### Setup workbook ---
today <- paste0("Workbook created ", Sys.Date())

wb <- loadWorkbook(paste0(template_path, "/4_Referral Treatment and Outcomes_",
                          season, ".xlsx"))
## Source notes script
source(here::here("code/9992_Source_Excel_4.R"))

rm(list=ls(pattern = "theme5_"))


## Table of Contents ---
writeData(wb, sheet = "Table of Contents", today, startRow = 6)

## KPI 3.1 ---
writeData(wb, sheet = "KPI 3.1", kpi_3_1, startRow = 7, colNames = FALSE)





## Save ----
saveWorkbook(wb, paste0(output_path, "/4_Referral Treatment and Outcomes_", 
                        yymm, ".xlsx"), overwrite = TRUE)



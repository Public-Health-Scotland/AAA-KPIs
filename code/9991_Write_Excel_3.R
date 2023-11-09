# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 9991_Write_Excel_3.R
# 
# Karen Hotopp
# Nov 2023
# 
# Write out to AAA Excel workbook 3: Quality Assurance
# 
# Written/run on Posit WB, R 4.1.2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Notes:
# This script calls in the RDS file create in the 7_3_KPI_2.R script 
# and transforms the data to print directly into the theme 3 Excel file for 
# the autumn MEG.
# 
# Future work to be done to add spring printing out.


#### 1: Housekeeping ----
library(dplyr)
library(readr)
library(tidyr)
library(openxlsx)
library(reporter)


rm(list=ls())
gc()


## Values
source(here::here("code/0_housekeeping.R"))

rm(hb_list, fy_tibble, fy_list, exclusions_path, extract_path, 
   hist_path, simd_path)

## Define reporting years
year_xx <- year(date_cut_off)
year_ww <- year_xx - 1
year_vv <- year_xx - 2
meg_month <- "December"


## File paths
template_path <- paste0("/PHI_conf/AAA/Topics/Screening/templates")


### 2: Import data ----
# KPI 3.1 and 3.2
theme_3 <- read_rds(paste0(temp_path, "/3_1_kpi_2_", yymm, ".rds"))
table(theme_3$kpi, theme_3$fin_year) 


### 3: Format data ----
## KPI 2.1a ----
kpi_2_1a <- theme_3 |> 
  filter(kpi == "KPI 2.1a") |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hb, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value)


## KPI 2.1b ----
kpi_2_1b <- theme_3 |> 
  filter(kpi == "KPI 2.1b") |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hb, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value)


## KPI 2.2 ----
kpi_2_2 <- theme_3 |> 
  filter(kpi == "KPI 2.2") |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hb, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value)


## KPI 2.2 Additional A ----
kpi_2_2_add_a <- theme_3 |> 
  filter(kpi == "KPI 2.2 Additional A") |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hb, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value)


## KPI 2.2 Additional B ----
kpi_2_2_add_b <- theme_3 |> 
  filter(kpi == "KPI 2.2 Additional B") |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hb, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value)


## Table 4: Eligible, no final result ----
# Top section
table_4_top <- theme_3 |> 
  filter(str_detect(kpi, "Table 4:"),
         fin_year %in% c(kpi_report_years[1:2])) |>
  mutate(hb = fct_relevel(as.factor(hb), "Scotland")) |> 
  arrange(fin_year, hb)  |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hb, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value)

# Bottom section
table_4_bot <- theme_3 |> 
  filter(str_detect(kpi, "Table 4:"),
         fin_year %in% c(kpi_report_years[3])) |>
  mutate(hb = fct_relevel(as.factor(hb), "Scotland")) |> 
  arrange(fin_year, hb)  |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hb, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value)


## QA standard not met Reason ----
# Top section
qa_reason_top <- theme_3 |> 
  filter(kpi == "QA Not Met: Reason",
         fin_year %in% c(kpi_report_years[1:2])) |>
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hb, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value)

# Bottom section
qa_reason_bot <- theme_3 |> 
  filter(kpi == "QA Not Met: Reason",
         fin_year %in% c(kpi_report_years[3])) |>
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hb, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value)


## QA standard not met Detail ----
qa_detail <- theme_3 |> 
  filter(kpi == "QA Not Met: Detail") |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hb, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value) |> 
  select(-hb)


## Batch QA standard not met ----
# Reason -- Scotland
qa_batch_scot <- theme_3 |> 
  filter(kpi == "QA Batch standard not met: Reason",
         hb == "Scotland") |> 
  select(group, fin_year, value) |> 
  # match Excel tables
  pivot_wider(names_from = fin_year, values_from = value)

# Reason -- HBs
qa_batch_hb <- theme_3 |> 
  filter(kpi == "QA Batch standard not met: Reason") |> 
  # move Scotland to end of list
  mutate(hb = fct_relevel(as.factor(hb), "Scotland", after = Inf)) |> 
  arrange(hb, fin_year) |> 
  mutate(FY_group = paste(fin_year, group, sep = "_")) |> 
  select(hb, FY_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_group, values_from = value) %>% 
  #! Do not change the last pipe to |> or next line of code will not work!!
  replace(is.na(.), 0)

# Recall Advice
qa_recall <- theme_3 |> 
  filter(kpi == "QA Batch standard not met: Recall Advice") |> 
  # move Scotland to end of list
  mutate(hb = fct_relevel(as.factor(hb), "Scotland", after = Inf)) |> 
  arrange(hb, fin_year) |> 
  mutate(FY_group = paste(fin_year, group, sep = "_")) |> 
  select(hb, FY_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_group, values_from = value) %>% 
  #! Do not change the last pipe to |> or next line of code will not work!!
  replace(is.na(.), 0)


### 4: Write to Excel (openxlsx) ----
### Setup workbook ---
today <- paste0("Workbook created ", Sys.Date())
meg_review <- paste0("For review at MEG in ", meg_month, " ", year_xx)

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

# KPI 2 & QA
screened_year_vv <- paste0("Screened in year ending 31 March ", year_vv)
screened_year_ww <- paste0("Screened in year ending 31 March ", year_ww)
screened_year_xx <- paste0("Screened in year ending 31 March ", year_xx)

# Table 4
eligible_year_vv <- paste0("Eligible cohort: Turned 66 in year ending 31 March ", 
                           year_vv, {supsc('r')})
eligible_year_ww <- paste0("Eligible cohort: Turned 66 in year ending 31 March ", 
                           year_ww, {supsc('r')})
eligible_year_xx <- paste0("Eligible cohort: Turned 66 in year ending 31 March ", 
                           year_xx, {supsc('r')})


wb <- loadWorkbook(paste0(template_path, "/3 Quality Assurance_",
                          season, ".xlsx"))
## Source notes script
source(here::here("code/9992_Source_Excel_4.R"))


## Table of Contents ---
writeData(wb, sheet = "Table of Contents", today, startRow = 6)

## KPI 2.1a ---
writeData(wb, sheet = "KPI 2.1a", kpi_2_1a, startRow = 7, colNames = FALSE)

## KPI 2.1b ---
writeData(wb, sheet = "KPI 2.1b", kpi_2_1b, startRow = 7, colNames = FALSE)

## KPI 2.2 ---
writeData(wb, sheet = "KPI 2.2", kpi_2_2, startRow = 7, colNames = FALSE)

## KPI 2.2 Additional A ---
writeData(wb, sheet = "KPI 2.2 Additional (A)", kpi_2_2_add_a, startRow = 7, 
          colNames = FALSE)

## KPI 2.2 Additional B ---
writeData(wb, sheet = "KPI 2.2 Additional (B)", kpi_2_2_add_b, startRow = 7, 
          colNames = FALSE)

## Table 4: Eligible, no final result ---
writeData(wb, sheet = "4) Eligible no final result", table_4_top, startRow = 8, 
          colNames = FALSE)
writeData(wb, sheet = "4) Eligible no final result", table_4_bot, startRow = 28, 
          colNames = FALSE)

## QA standard not met reason ---
writeData(wb, sheet = "QA standard not met reason", qa_reason_top, startRow = 8, 
          colNames = FALSE)
writeData(wb, sheet = "QA standard not met reason", qa_reason_bot, startRow = 29, 
          colNames = FALSE)

## QA standard not met detail ---
writeData(wb, sheet = "QA standard not met detail", qa_detail, startRow = 9,
          startCol = 2, colNames = FALSE)

## Batch QA standard not met ---
writeData(wb, sheet = "Batch QA standard not met", qa_batch_scot, startRow = 7, 
          colNames = FALSE)
writeData(wb, sheet = "Batch QA standard not met", qa_batch_hb, startRow = 18, 
          colNames = FALSE)
writeData(wb, sheet = "Batch QA standard not met", qa_recall, startRow = 29, 
          colNames = FALSE)

## Save ----
saveWorkbook(wb, paste0(output_path, "/3_Quality Assurance_", yymm, ".xlsx"), 
             overwrite = TRUE)



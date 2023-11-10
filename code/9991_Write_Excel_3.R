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
library(lubridate)
library(readr)
library(tidyr)
library(stringr)
library(forcats)
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
# Top section
kpi_2_2_add_a_top <- theme_3 |> 
  filter(kpi == "KPI 2.2 Additional A",
         fin_year %in% c(kpi_report_years[1:2])) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hb, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value)

# Bottom section
kpi_2_2_add_a_bot <- theme_3 |> 
  filter(kpi == "KPI 2.2 Additional A",
         fin_year %in% c(kpi_report_years[3])) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hb, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value)


## KPI 2.2 Additional B ----
# Top section
kpi_2_2_add_b_top <- theme_3 |> 
  filter(kpi == "KPI 2.2 Additional B",
         fin_year %in% c(kpi_report_years[1:2])) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hb, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value)

# Bottom section
kpi_2_2_add_b_bot <- theme_3 |> 
  filter(kpi == "KPI 2.2 Additional B",
         fin_year %in% c(kpi_report_years[3])) |> 
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
  mutate(hb = forcats::fct_relevel(as.factor(hb), "Scotland", after = Inf)) |> 
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
  mutate(hb = forcats::fct_relevel(as.factor(hb), "Scotland", after = Inf)) |> 
  arrange(hb, fin_year) |> 
  mutate(FY_group = paste(fin_year, group, sep = "_")) |> 
  select(hb, FY_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_group, values_from = value) %>% 
  #! Do not change the last pipe to |> or next line of code will not work!!
  replace(is.na(.), 0)


## Excel notes for QA standard not met detail ----
qa_detail_1a <- qa_reason_bot[1,2]

qa_detail_1b <- qa_detail[19,5]

#qa_detail_2a <- ## I think this has to come from the main data... do by hand for now.

qa_detail_3 <- left_join(qa_reason_top, qa_reason_bot) |> 
  filter(hb == "Scotland") |> 
  select(hb, ends_with("anatomy_n")) |> 
  mutate(anatomy_sum = sum(c_across(where(is.numeric)))) |> 
  select(anatomy_sum)

 
### 4: Write to Excel ----
### Setup workbook ---
## Notes & Headers
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
                           year_xx)
self_ref_year_xx <- paste0("Self referrals: cumulative period from implementation ",
                           "to 31 March ", year_xx)

# QA standard not met detail notes
qa_detail_note1 <- paste0("1. Up to five detailed reasons for each screen may be ",
                          "recorded where all five positions have been used to ",
                          "identify the relevant cases. This means that the sum ",
                          "of the number of detailed reasons recorded is usually ",
                          "greater than the total number of scans that did not ",
                          "meet the QA standard. For example, there were ",  
                          qa_detail_1a," scans in the year ending 31 March ", year_xx, 
                          " that did not meet the QA standard and the total number ",
                          "of detailed reasons recorded for the same period was ",
                          qa_detail_1b, ".")

# qa_detail_note2 <- paste0("2. Over the 3 years presented, there were a total of ",
#                           qa_detail_2a, " standard not met scans that did not have ",
#                           "any detailed reason recorded: ", qa_detail_2b, 
#                           " of these were image quality, ", qa_detail_2c, 
#                           " calliper placement and ", qa_detail_2d, " angle.")

qa_detail_note3 <- paste0("3. Over the 3 years presented, there were ", qa_detail_3, 
                          " scans with anatomy as the reason the standard was not ", 
                          "met, and in such cases, the recording of a detailed ",
                          "reason is not expected. Anatomy is normally recorded as ",
                          "the reason the standard was not met fail in cases where ", 
                          "the aorta cannot be confidently identified from the image ", 
                          "and therefore no further detail can be added. The normal ",
                          "follow-up for these cases would be to recall the man ",
                          "for screening.")


wb <- loadWorkbook(paste0(template_path, "/3 Quality Assurance_",
                          season, ".xlsx"))

## Table of Contents ---
writeData(wb, "Table of Contents", pub_year, startRow = 3)
writeData(wb, "Table of Contents", meg_review, startRow = 4)
writeData(wb, "Table of Contents", report_type, startRow = 5)
writeData(wb, "Table of Contents", today, startRow = 6)
writeData(wb, "Table of Contents", note_toc, startRow = 23)

addStyle(wb, "Table of Contents", style = report_type_style, rows = 5, cols = 1)

## KPI 2.1a ---
writeData(wb, sheet = "KPI 2.1a", kpi_2_1a, startRow = 7, colNames = FALSE)
writeData(wb, sheet = "KPI 2.1a", screened_year_vv, startRow = 4, startCol = 2)
writeData(wb, sheet = "KPI 2.1a", screened_year_ww, startRow = 4, startCol = 5)
writeData(wb, sheet = "KPI 2.1a", screened_year_xx, startRow = 4, startCol = 8)

## KPI 2.1b ---
writeData(wb, sheet = "KPI 2.1b", kpi_2_1b, startRow = 7, colNames = FALSE)
writeData(wb, sheet = "KPI 2.1b", screened_year_vv, startRow = 4, startCol = 2)
writeData(wb, sheet = "KPI 2.1b", screened_year_ww, startRow = 4, startCol = 5)
writeData(wb, sheet = "KPI 2.1b", screened_year_xx, startRow = 4, startCol = 8)

## KPI 2.2 ---
writeData(wb, sheet = "KPI 2.2", kpi_2_2, startRow = 7, colNames = FALSE)
writeData(wb, sheet = "KPI 2.2", screened_year_vv, startRow = 4, startCol = 2)
writeData(wb, sheet = "KPI 2.2", screened_year_ww, startRow = 4, startCol = 5)
writeData(wb, sheet = "KPI 2.2", screened_year_xx, startRow = 4, startCol = 8)

## KPI 2.2 Additional A ---
writeData(wb, sheet = "KPI 2.2 Additional (A)", kpi_2_2_add_a_top, startRow = 8, 
          colNames = FALSE)
writeData(wb, sheet = "KPI 2.2 Additional (A)", kpi_2_2_add_a_bot, startRow = 29, 
          colNames = FALSE)
writeData(wb, sheet = "KPI 2.2 Additional (A)", screened_year_vv, startRow = 4, 
          startCol = 2)
writeData(wb, sheet = "KPI 2.2 Additional (A)", screened_year_ww, startRow = 4, 
          startCol = 10)
writeData(wb, sheet = "KPI 2.2 Additional (A)", screened_year_xx, startRow = 25, 
          startCol = 2)

## KPI 2.2 Additional B ---
writeData(wb, sheet = "KPI 2.2 Additional (B)", kpi_2_2_add_b_top, startRow = 8, 
          colNames = FALSE)
writeData(wb, sheet = "KPI 2.2 Additional (B)", kpi_2_2_add_b_bot, startRow = 29, 
          colNames = FALSE)
writeData(wb, sheet = "KPI 2.2 Additional (B)", screened_year_vv, startRow = 4, 
          startCol = 2)
writeData(wb, sheet = "KPI 2.2 Additional (B)", screened_year_ww, startRow = 4, 
          startCol = 15)
writeData(wb, sheet = "KPI 2.2 Additional (B)", screened_year_xx, startRow = 25, 
          startCol = 2)

## Table 4: Eligible, no final result ---
writeData(wb, sheet = "4) Eligible no final result", table_4_top, startRow = 8, 
          colNames = FALSE)
writeData(wb, sheet = "4) Eligible no final result", table_4_bot, startRow = 28, 
          colNames = FALSE)
writeData(wb, sheet = "4) Eligible no final result", eligible_year_vv, startRow = 4, 
          startCol = 2)
writeData(wb, sheet = "4) Eligible no final result", eligible_year_ww, startRow = 4, 
          startCol = 9)
writeData(wb, sheet = "4) Eligible no final result", eligible_year_xx, startRow = 24, 
          startCol = 2)
writeData(wb, sheet = "4) Eligible no final result", self_ref_year_xx, startRow = 24, 
          startCol = 9)

## QA standard not met reason ---
writeData(wb, sheet = "QA standard not met reason", qa_reason_top, startRow = 8, 
          colNames = FALSE)
writeData(wb, sheet = "QA standard not met reason", qa_reason_bot, startRow = 29, 
          colNames = FALSE)
writeData(wb, sheet = "QA standard not met reason", screened_year_vv, startRow = 4, 
          startCol = 2)
writeData(wb, sheet = "QA standard not met reason", screened_year_ww, startRow = 4, 
          startCol = 11)
writeData(wb, sheet = "QA standard not met reason", screened_year_xx, startRow = 25, 
          startCol = 2)

## QA standard not met detail ---
writeData(wb, sheet = "QA standard not met detail", qa_detail, startRow = 9,
          startCol = 2, colNames = FALSE)
writeData(wb, sheet = "QA standard not met detail", screened_year_vv, startRow = 4, 
          startCol = 2)
writeData(wb, sheet = "QA standard not met detail", screened_year_ww, startRow = 4, 
          startCol = 4)
writeData(wb, sheet = "QA standard not met detail", screened_year_xx, startRow = 4, 
          startCol = 6)
writeData(wb, sheet = "QA standard not met detail", screened_year_vv, startRow = 7, 
          startCol = 2)
writeData(wb, sheet = "QA standard not met detail", screened_year_ww, startRow = 7, 
          startCol = 4)
writeData(wb, sheet = "QA standard not met detail", screened_year_xx, startRow = 7, 
          startCol = 6)

writeData(wb, sheet = "QA standard not met detail", qa_detail_note1, startRow = 30)
#writeData(wb, sheet = "QA standard not met detail", qa_detail_note2, startRow = 31)
writeData(wb, sheet = "QA standard not met detail", qa_detail_note3, startRow = 32)

## Batch QA standard not met ---
writeData(wb, sheet = "Batch QA standard not met", qa_batch_scot, startRow = 7, 
          colNames = FALSE)
writeData(wb, sheet = "Batch QA standard not met", qa_batch_hb, startRow = 18, 
          colNames = FALSE)
writeData(wb, sheet = "Batch QA standard not met", qa_recall, startRow = 29, 
          colNames = FALSE)
writeData(wb, sheet = "Batch QA standard not met", screened_year_vv, startRow = 5, 
          startCol = 2)
writeData(wb, sheet = "Batch QA standard not met", screened_year_ww, startRow = 5, 
          startCol = 3)
writeData(wb, sheet = "Batch QA standard not met", screened_year_xx, startRow = 5, 
          startCol = 4)
writeData(wb, sheet = "Batch QA standard not met", screened_year_vv, startRow = 14, 
          startCol = 2)
writeData(wb, sheet = "Batch QA standard not met", screened_year_ww, startRow = 14, 
          startCol = 7)
writeData(wb, sheet = "Batch QA standard not met", screened_year_xx, startRow = 14, 
          startCol = 12)
writeData(wb, sheet = "Batch QA standard not met", screened_year_vv, startRow = 25, 
          startCol = 2)
writeData(wb, sheet = "Batch QA standard not met", screened_year_ww, startRow = 25, 
          startCol = 8)
writeData(wb, sheet = "Batch QA standard not met", screened_year_xx, startRow = 25, 
          startCol = 14)

## Save ----
saveWorkbook(wb, paste0(output_path, "/3_Quality Assurance_", yymm, ".xlsx"), 
             overwrite = TRUE)



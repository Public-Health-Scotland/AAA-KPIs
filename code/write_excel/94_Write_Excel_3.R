# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 94_Write_Excel_3.R
# 
# Karen Hotopp & Aoife McCarthy
# Nov 2023
# 
# Write out to AAA Excel workbook 3: Quality Assurance
# 
# Written/run on Posit WB, R 4.1.2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Notes:
# This script calls in the RDS file create in the 4_3_KPI_2.R script 
# and transforms the data to print directly into the theme 3 Excel file for 
# the autumn and spring QPMG.
# 
# Future work to be done to add spring printing out.


# 1: Housekeeping ----
library(dplyr)
library(lubridate)
library(readr)
library(tidyr)
library(stringr)
library(forcats)
library(openxlsx)
library(reporter)
library(phsaaa) # devtools::install_github("aoifem01/phsaaa")


rm(list=ls())
gc()

## Values
source(here::here("code/0_housekeeping.R"))

rm (exclusions_path, extract_path, hist_path, simd_path, fy_list, hb_list,
    fy_tibble, hb_tibble, cutoff_date, end_current, end_date, start_date, 
    year1_end, year1_start, year2_end, year2_start, year1, year2)

## Define reporting years
year_xx <- year(cut_off_date)
year_ww <- year_xx - 1
year_vv <- year_xx - 2
year_yy <- year_xx + 1

## File paths
template_path <- paste0("/PHI_conf/AAA/Topics/Screening/templates")


# 2: Import data ----
# KPI 2.1a/b and 2.2 %>% 
theme_3 <- read_rds(paste0(temp_path, "/3_1_kpi_2_", yymm, ".rds"))
table(theme_3$kpi, theme_3$fin_year) 

# KPI 2.1b by SIMD (new for 202409)
data_kpi_2_1b_simd <- read_rds(paste0(temp_path, "/3_2_kpi_2_1b_simd_", yymm, ".rds")) |> 
  rename(fin_year = financial_year)
table(data_kpi_2_1b_simd$kpi, data_kpi_2_1b_simd$fin_year) 

#KPI 2.1a, 2.1b, + 2.2 device comparison (new for 202409)
kpi_2_dc <- read_rds(paste0(temp_path, "/3_3_kpi_2_dc_", yymm, ".rds"))
kpi_2_dc <- kpi_2_dc |> 
  droplevels() |> 
  mutate(fin_year = as.character(fin_year))
table(kpi_2_dc$kpi, kpi_2_dc$fin_year)

# 3: Format data ----
## KPI 2.1a ----
kpi_2_1a <- theme_3 |> 
  filter(kpi == "KPI 2.1a") |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value)


## KPI 2.1b ----
kpi_2_1b <- theme_3 |> 
  filter(kpi == "KPI 2.1b") |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value)


## KPI 2.2 ----
kpi_2_2 <- theme_3 |> 
  filter(kpi == "KPI 2.2") |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value)


## KPI 2.2 Additional A ----
# Top section
kpi_2_2_add_a_top <- theme_3 |> 
  filter(kpi == "KPI 2.2 Additional A",
         fin_year %in% c(kpi_report_years[1:2])) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value)

# Bottom section
kpi_2_2_add_a_bot <- theme_3 |> 
  filter(kpi == "KPI 2.2 Additional A",
         fin_year %in% c(kpi_report_years[3])) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value)


## KPI 2.2 Additional B ----
# Top section
kpi_2_2_add_b_top <- theme_3 |> 
  filter(kpi == "KPI 2.2 Additional B",
         fin_year %in% c(kpi_report_years[1:2])) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value)

# Bottom section
kpi_2_2_add_b_bot <- theme_3 |> 
  filter(kpi == "KPI 2.2 Additional B",
         fin_year %in% c(kpi_report_years[3])) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value)


## Table 4: Eligible, no final result ----
# Top section
table_4_top <- theme_3 |> 
  filter(str_detect(kpi, "Table 4:"),
         fin_year %in% c(kpi_report_years[1:2])) |>
  mutate(hbres = fct_relevel(as.factor(hbres), "Scotland")) |> 
  arrange(fin_year, hbres)  |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value)

# Bottom section
table_4_bot <- theme_3 |> 
  filter(str_detect(kpi, "Table 4:"),
         fin_year %in% c(kpi_report_years[3])) |>
  mutate(hbres = fct_relevel(as.factor(hbres), "Scotland")) |> 
  arrange(fin_year, hbres)  |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value)


## QA standard not met Reason ----
# Top section
qa_reason_top <- theme_3 |> 
  filter(kpi == "QA Not Met: Reason",
         fin_year %in% c(kpi_report_years[1:2])) |>
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value)

# Bottom section
qa_reason_bot <- theme_3 |> 
  filter(kpi == "QA Not Met: Reason",
         fin_year %in% c(kpi_report_years[3])) |>
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value)


## QA standard not met Detail ----

qa_detail <- theme_3 |> 
  filter(kpi == "QA Not Met: Detail") |> 
  mutate(detail = substr(group, 1, nchar(group)-2),
         group = case_when(str_detect(group, "_n") ~ "n",
                           str_detect(group, "_p") ~ "p")) |>
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(detail, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value) |> 
  select(-detail)


## Batch QA standard not met ----
# Reason -- Scotland
qa_batch_scot <- theme_3 |> 
  filter(kpi == "QA Batch standard not met: Reason",
         hbres == "Scotland") |> 
  select(group, fin_year, value) |> 
  # match Excel tables
  pivot_wider(names_from = fin_year, values_from = value)

# Reason -- HBs
qa_batch_hb <- theme_3 |> 
  filter(kpi == "QA Batch standard not met: Reason") |> 
  arrange(hbres, fin_year) |> 
  mutate(FY_group = paste(fin_year, group, sep = "_")) |> 
  select(hbres, FY_group, value)

# the above retains HBs from historical db which have no values within kpi_report_years
# remove them below
 
qa_batch_hb <- qa_batch_hb %>% 
  group_by(hbres) %>% 
  # sum values, will be 0 if no records present for kpi_report_years
  mutate(count = sum(value),
         hbres= case_when(hbres=="Scotland" ~ "Total", 
                          TRUE ~ hbres)) %>% 
  # remove anything with no records
  filter(count>0) %>% 
  ungroup() %>% 
  select(-count) %>% 
  # match Excel tables
  pivot_wider(names_from = FY_group, values_from = value) %>% 
  # move Total (Scotland) to bottom to match Excel
  mutate(hbres = forcats::fct_relevel(as.factor(hbres), "Total", after = Inf)) |> 
  arrange(hbres) %>%
  #! Do not change the last pipe to |> or next line of code will not work!!
  replace(is.na(.), 0)

# Recall Advice
qa_recall <- theme_3 |> 
  filter(kpi == "QA Batch standard not met: Recall Advice") |> 
  arrange(hbres, fin_year) |> 
  mutate(FY_group = paste(fin_year, group, sep = "_")) |> 
  select(hbres, FY_group, value)

# the above retains HBs from historical db which have no values within kpi_report_years
# remove them below

qa_recall <- qa_recall %>% 
  group_by(hbres) %>% 
  # sum values, will be 0 if no records present for kpi_report_years
  mutate(count = sum(value),
         hbres= case_when(hbres=="Scotland" ~ "Total", 
                          TRUE ~ hbres)) %>% 
  # remove anything with no records
  filter(count>0) %>% 
  ungroup() %>% 
  select(-count) %>% 
  # match Excel tables
  pivot_wider(names_from = FY_group, values_from = value) %>% 
  # move Total (Scotland) to bottom to match Excel
  mutate(hbres = forcats::fct_relevel(as.factor(hbres), "Total", after = Inf)) |> 
  arrange(hbres) %>%
  #! Do not change the last pipe to |> or next line of code will not work!!
  replace(is.na(.), 0)


## KPI 2.1b by SIMD (new for 202409) ----
kpi_2_1b_simd <- data_kpi_2_1b_simd |> 
  filter(kpi == "KPI 2.1b SIMD") |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hb_screen, simd, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value) |> 
  select(-c(hb_screen, simd))

## KPI 2.1a device comparison (new for 202409) ----
kpi_2_1a_dc <- kpi_2_dc |> 
  filter(kpi == "KPI 2.1a dc") |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, device, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 2.1b device comparison (new for 202409) ----
kpi_2_1b_dc <- kpi_2_dc |> 
  filter(kpi == "KPI 2.1b dc") |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, device, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 2.2 device comparison (new for 202409) ----
kpi_2_2_dc <- kpi_2_dc |> 
  filter(kpi == "KPI 2.2 dc") |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, device, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 2.2 Additional A device comparison (new for 202409) ----
kpi_2_2_add_a_dc <- kpi_2_dc |> 
  filter(kpi == "KPI 2.2 Additional A dc") |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, device, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value) |> 
  select(hbres,
         `2023/24_KPI 2.2 Additional A dc_audit_n_old`,
         `2023/24_KPI 2.2 Additional A dc_no_audit_result_n_old`,
         `2023/24_KPI 2.2 Additional A dc_no_audit_result_p_old`,
         `2023/24_KPI 2.2 Additional A dc_audit_n2_old`,
         `2023/24_KPI 2.2 Additional A dc_standard_met_n_old`,
         `2023/24_KPI 2.2 Additional A dc_standard_met_p_old`,
         `2023/24_KPI 2.2 Additional A dc_standard_not_met_n_old`,
         `2023/24_KPI 2.2 Additional A dc_standard_not_met_p_old`,
         `2023/24_KPI 2.2 Additional A dc_audit_n_new`,
         `2023/24_KPI 2.2 Additional A dc_no_audit_result_n_new`,
         `2023/24_KPI 2.2 Additional A dc_no_audit_result_p_new`,
         `2023/24_KPI 2.2 Additional A dc_audit_n2_new`,
         `2023/24_KPI 2.2 Additional A dc_standard_met_n_new`,
         `2023/24_KPI 2.2 Additional A dc_standard_met_p_new`,
         `2023/24_KPI 2.2 Additional A dc_standard_not_met_n_new`,
         `2023/24_KPI 2.2 Additional A dc_standard_not_met_p_new`)

## KPI 2.2 Additional B device comparison (new for 202409) ----
kpi_2_2_add_b_dc <- kpi_2_dc |> 
  filter(kpi == "KPI 2.2 Additional B dc") |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, device, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |> 
  # match Excel tables
  pivot_wider(names_from = FY_kpi_group, values_from = value) |> 
  select(hbres,
         `2023/24_KPI 2.2 Additional B dc_standard_not_met_n_old`,
         `2023/24_KPI 2.2 Additional B dc_imm_recall_n_old`,
         `2023/24_KPI 2.2 Additional B dc_imm_recall_p_old`,
         `2023/24_KPI 2.2 Additional B dc_recall_cc_n_old`,
         `2023/24_KPI 2.2 Additional B dc_recall_cc_p_old`,
         `2023/24_KPI 2.2 Additional B dc_no_recall_sat_interim_n_old`,
         `2023/24_KPI 2.2 Additional B dc_no_recall_sat_interim_p_old`,
         `2023/24_KPI 2.2 Additional B dc_no_recall_refer_vasc_n_old`,
         `2023/24_KPI 2.2 Additional B dc_no_recall_refer_vasc_p_old`,
         `2023/24_KPI 2.2 Additional B dc_no_recall_sec_opin_n_old`,
         `2023/24_KPI 2.2 Additional B dc_no_recall_sec_opin_p_old`,
         `2023/24_KPI 2.2 Additional B dc_no_audit_result_n_old`,
         `2023/24_KPI 2.2 Additional B dc_no_audit_result_p_old`,
         `2023/24_KPI 2.2 Additional B dc_standard_not_met_n_new`,
         `2023/24_KPI 2.2 Additional B dc_imm_recall_n_new`,
         `2023/24_KPI 2.2 Additional B dc_imm_recall_p_new`,
         `2023/24_KPI 2.2 Additional B dc_recall_cc_n_new`,
         `2023/24_KPI 2.2 Additional B dc_recall_cc_p_new`,
         `2023/24_KPI 2.2 Additional B dc_no_recall_sat_interim_n_new`,
         `2023/24_KPI 2.2 Additional B dc_no_recall_sat_interim_p_new`,
         `2023/24_KPI 2.2 Additional B dc_no_recall_refer_vasc_n_new`,
         `2023/24_KPI 2.2 Additional B dc_no_recall_refer_vasc_p_new`,
         `2023/24_KPI 2.2 Additional B dc_no_recall_sec_opin_n_new`,
         `2023/24_KPI 2.2 Additional B dc_no_recall_sec_opin_p_new`,
         `2023/24_KPI 2.2 Additional B dc_no_audit_result_n_new`,
         `2023/24_KPI 2.2 Additional B dc_no_audit_result_p_new`)
 
# 4: Write to Excel ----
## Setup workbook ----
### Notes & Headers ----
today <- paste0("Workbook created ", Sys.Date())
qpmg_review <- paste0("For review at QPMG in ", qpmg_month, " ", year_xx)

pub_year <- phsaaa::eval_seasonal_diff(
  season,
  {paste0("Data for year ending 31 March ", year_xx, " scheduled to ",
          "be published in April ", year_yy, " (final data will be ",
          "produced from data extracted for PHS in September ",
          year_xx, ").")}, #spring
  {paste0("KPI data for year ending 31 March ", year_xx, 
          " and some supplementary information are planned ",
          "for publication in March ", year_yy, ".")} # autumn
)
note_toc <- paste0("The data for the year ending 31 March ", year_xx, 
                   " are released for data quality assurance and management ",
                   "information purposes and should not be placed in the public ",
                   "domain. The information can be shared locally with those who ",
                   "have a legitimate need to review the data for quality ",
                   "assurance, managerial or operational purposes.")


### Styles ----

source(here::here("code/write_excel/99_Source_Excel_Styles.R"))

### KPI 2 & QA ----
screened_year_vv <- paste0("Screened in year ending 31 March ", year_vv)
screened_year_ww <- paste0("Screened in year ending 31 March ", year_ww)
screened_year_xx <- phsaaa::eval_seasonal_diff(
  season,
  {paste0("Screened 1 April ", year_ww, " - ", '\n', "28 February ", year_xx, 
          " (partial data for financial year)")}, # spring
  { paste0("Screened in year ending 31 March ", year_xx) } # autumn
  )

kpi_2_notep <- paste0("p Screened 1 April ", year_ww, " to 28 February ",
                        year_xx, ": provisional rates are presented for the ",
                        "11-month period 1 April ", year_ww, " to 28 February ",
                        year_xx, " as data are not yet available for the full ",
                        "financial year ending 31 March ", year_xx, " from the ",
                        "PHS extract at ", extract_date, " ", year_xx, ". Data ",
                        "for the complete financial year ending 31 March ", year_xx,
                        " will be produced from the PHS data extract at 1 ",
                        "September ", year_xx, ".")


### Table 4 ----
eligible_year_vv <- paste0("Eligible cohort: Turned 66 in year ending 31 March ", 
                           year_vv, {supsc('r')})
eligible_year_ww <- paste0("Eligible cohort: Turned 66 in year ending 31 March ", 
                           year_ww, {supsc('r')})
eligible_year_xx <- paste0("Eligible cohort: Turned 66 in year ending 31 March ", 
                           year_xx)
self_ref_year_xx <- paste0("Self referrals: cumulative period from implementation ",
                           "to 31 March ", year_xx)

### QA standard not met detail standard not met totals from previous tab (reason) ----

std_not_met_y1 <- qa_reason_top %>% filter(hbres=="Scotland") %>% 
  select(contains(kpi_report_years[1]) & contains("Reason_standard_not_met_n")) %>% 
  pull()
         
std_not_met_y2 <- qa_reason_top %>% filter(hbres=="Scotland") %>% 
  select(contains(kpi_report_years[2]) & contains("Reason_standard_not_met_n")) %>% 
  pull()

std_not_met_y3 <- qa_reason_bot %>% filter(hbres=="Scotland") %>% 
  select(contains(kpi_report_years[3]) & contains("Reason_standard_not_met_n")) %>% 
  pull()

### QA standard not met detail notes ----

#qa_detail_2a <- ## I think this has to come from the main data... do by hand for now.
#qa_detail_2b
#qa_detail_2c

# qa_detail_note2 <- paste0("2. Over the 3 years presented, there were a total of ",
#                           qa_detail_2a, " standard not met scans that did not have ",
#                           "any detailed reason recorded: ", qa_detail_2b, 
#                           " of these were image quality, ", qa_detail_2c, 
#                           " calliper placement and ", qa_detail_2d, " angle.")


qa_detail_3 <- left_join(qa_reason_top, qa_reason_bot) |> 
  filter(hbres == "Scotland") |> 
  select(hbres, ends_with("anatomy_n")) |> 
  mutate(anatomy_sum = sum(c_across(where(is.numeric)))) |> 
  select(anatomy_sum)

qa_detail_note3 <- paste0("3. Over the 3 years presented, there were ", qa_detail_3, 
                          " scans with anatomy as the reason the standard was not ", 
                          "met, and in such cases, the recording of a detailed ",
                          "reason is not expected. Anatomy is normally recorded as ",
                          "the reason the standard was not met fail in cases where ", 
                          "the aorta cannot be confidently identified from the image ", 
                          "and therefore no further detail can be added. The normal ",
                          "follow-up for these cases would be to recall the man ",
                          "for screening.")

### workbook ----
# wb <- loadWorkbook(paste0(template_path, "/3_Quality Assurance_",
#                           season, ".xlsx"))
# AMc test path:
wb <- loadWorkbook(paste0(template_path, "/new_templates/3_Quality Assurance_",
                          season, "_new.xlsx"))

## Table of Contents ----
# notes
writeData(wb, "Table of Contents", pub_year, 
          startRow = 3)
addStyle(wb, "Table of Contents", styles$black_nowrap_12,
         rows = 3, cols = 1)
writeData(wb, "Table of Contents", qpmg_review, 
          startRow = 4)
addStyle(wb, "Table of Contents", styles$black_bold_12,
         rows = 4, cols = 1)
writeData(wb, "Table of Contents", today, 
          startRow = 6)
addStyle(wb, "Table of Contents", styles$black_nowrap_12, 
         rows = 6, cols = 1)
writeData(wb, "Table of Contents", note_toc, 
          startRow = 29)
addStyle(wb, "Table of Contents", styles$red_bold_12, 
         rows = 29, cols = 1)

showGridLines(wb, "Table of Contents", showGridLines = FALSE)

## KPI 2.1a ----
# notes
writeData(wb, sheet = "KPI 2.1a", screened_year_vv, 
          startRow = 4, startCol = 2)
writeData(wb, sheet = "KPI 2.1a", screened_year_ww, 
          startRow = 4, startCol = 5)
writeData(wb, sheet = "KPI 2.1a", screened_year_xx, 
          startRow = 4, startCol = 8)
addStyle(wb, "KPI 2.1a", styles$black_border_centre_12, 
         rows = 4, cols = 2:10, gridExpand = TRUE)
if (season == "spring") {
  writeData(wb, sheet = "KPI 2.1a", kpi_2_notep, 
            startRow = 30)
  addStyle(wb, "KPI 2.1a", styles$black_11,
           rows = 30, cols = 1)
}
# data
writeData(wb, sheet = "KPI 2.1a", kpi_2_1a, 
          startRow = 7, colNames = FALSE)
showGridLines(wb, "KPI 2.1a", showGridLines = FALSE)

## KPI 2.1b ----
# data
writeData(wb, sheet = "KPI 2.1b", kpi_2_1b, startRow = 7, colNames = FALSE)
# notes
writeData(wb, sheet = "KPI 2.1b", screened_year_vv, startRow = 4, startCol = 2)
writeData(wb, sheet = "KPI 2.1b", screened_year_ww, startRow = 4, startCol = 5)
writeData(wb, sheet = "KPI 2.1b", screened_year_xx, startRow = 4, startCol = 8)
writeData(wb, sheet = "KPI 2.1b", kpi_2_notep, startRow = 30)
showGridLines(wb, "KPI 2.1b", showGridLines = FALSE)

## KPI 2.1b by SIMD ----
# notes
writeData(wb, sheet = "KPI 2.1b by SIMD", screened_year_vv, 
          startRow = 4, startCol = 3)
writeData(wb, sheet = "KPI 2.1b by SIMD", screened_year_ww, 
          startRow = 4, startCol = 6)
writeData(wb, sheet = "KPI 2.1b by SIMD", screened_year_xx, 
          startRow = 4, startCol = 9)
addStyle(wb, "KPI 2.1b by SIMD", styles$black_border_centre_12,
         rows = 4, cols = 3:11, gridExpand = TRUE)
if (season == "spring") {
  writeData(wb, sheet = "KPI 2.1b by SIMD", kpi_2_notep,
            startRow = 119)
  addStyle(wb, "KPI 2.1b by SIMD", styles$black_11,
           rows = 119, cols = 1)
}
# data
writeData(wb, sheet = "KPI 2.1b by SIMD", kpi_2_1b_simd,
          startRow = 7, startCol = 3, colNames = F)

## KPI 2.2 ----
writeData(wb, sheet = "KPI 2.2", kpi_2_2, startRow = 7, colNames = FALSE)
writeData(wb, sheet = "KPI 2.2", screened_year_vv, startRow = 4, startCol = 2)
writeData(wb, sheet = "KPI 2.2", screened_year_ww, startRow = 4, startCol = 5)
writeData(wb, sheet = "KPI 2.2", screened_year_xx, startRow = 4, startCol = 8)
writeData(wb, sheet = "KPI 2.2", kpi_2_notep, startRow = 30)
showGridLines(wb, "KPI 2.2", showGridLines = FALSE)

## KPI 2.2 Additional A ----
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
showGridLines(wb, "KPI 2.2 Additional (A)", showGridLines = FALSE)

## KPI 2.2 Additional B ----
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
showGridLines(wb, "KPI 2.2 Additional (B)", showGridLines = FALSE)

## Table 4: Eligible, no final result ----
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
showGridLines(wb, "4) Eligible no final result", showGridLines = FALSE)

## QA standard not met reason ----
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
showGridLines(wb, "QA standard not met reason", showGridLines = FALSE)

## QA standard not met detail ----

writeData(wb, sheet = "QA standard not met detail", std_not_met_y1, startRow = 5,
          startCol = 2)
writeData(wb, sheet = "QA standard not met detail", std_not_met_y2, startRow = 5,
          startCol = 4)
writeData(wb, sheet = "QA standard not met detail", std_not_met_y3, startRow = 5,
          startCol = 6)
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

#writeData(wb, sheet = "QA standard not met detail", qa_detail_note1, startRow = 30)
#writeData(wb, sheet = "QA standard not met detail", qa_detail_note2, startRow = 31)
writeData(wb, sheet = "QA standard not met detail", qa_detail_note3, startRow = 32)
showGridLines(wb, "QA standard not met detail", showGridLines = FALSE)

## Batch QA standard not met ----
writeData(wb, sheet = "Batch QA standard not met", qa_batch_scot, startRow = 7, 
          colNames = FALSE)
writeData(wb, sheet = "Batch QA standard not met", qa_batch_hb, startRow = 18, 
          colNames = FALSE)
writeData(wb, sheet = "Batch QA standard not met", qa_recall, startRow = 26, 
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
writeData(wb, sheet = "Batch QA standard not met", screened_year_vv, startRow = 22, 
          startCol = 2)
writeData(wb, sheet = "Batch QA standard not met", screened_year_ww, startRow = 22, 
          startCol = 8)
writeData(wb, sheet = "Batch QA standard not met", screened_year_xx, startRow = 22, 
          startCol = 14)
showGridLines(wb, "Batch QA standard not met", showGridLines = FALSE)

## KPI 2.1a device comparison ----
# notes
# data
writeData(wb, sheet = "KPI 2.1a device", kpi_2_1a_dc,
          startRow = 8, colNames = F)
showGridLines(wb, "KPI 2.1a device", showGridLines = FALSE)

## KPI 2.1b device comparison ----
# notes
# data
writeData(wb, sheet = "KPI 2.1b device", kpi_2_1b_dc,
          startRow = 8, colNames = F)
showGridLines(wb, "KPI 2.1b device", showGridLines = FALSE)

## KPI 2.2 device comparison ----
# notes
# data
writeData(wb, sheet = "KPI 2.2 device", kpi_2_2_dc,
          startRow = 8, colNames = F)
showGridLines(wb, "KPI 2.2 device", showGridLines = FALSE)

## KPI 2.2 Additional (A) device comparison ----
# notes
# data
writeData(wb, sheet = "KPI 2.2 Add (A) device", kpi_2_2_add_a_dc,
          startRow = 9, colNames = F)
showGridLines(wb, "KPI 2.2 Add (A) device", showGridLines = FALSE)

## KPI 2.2 Additional (B) device comparison ----
# notes
# data
writeData(wb, sheet = "KPI 2.2 Add (B) device", kpi_2_2_add_b_dc,
          startRow = 9, colNames = F)
showGridLines(wb, "KPI 2.2 Add (B) device", showGridLines = FALSE)

# 5: Save output ----
# phsaaa::query_saveWorkbook(wb, paste0(output_path, "/3_Quality Assurance_", yymm, ".xlsx"))

# test path:
saveWorkbook(wb, paste0(output_path, "/3_Quality Assurance_", yymm, "XXX.xlsx"),
             overwrite = TRUE)

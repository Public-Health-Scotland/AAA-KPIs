# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 9992_Source_Excel_4.R
# 
# Karen Hotopp
# Oct 2023
# 
# Set up notes for Theme 4 Excel workbook
# 
# Written/run on Posit WB, R 4.1.2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Notes:
# This script automates the titles and notes for each tab of the theme 4 Excel
# workbook for each (spring/autumn) MEG. (Spring to be added in spring 2024!)


#### 1: Housekeeping ----
library(dplyr)
library(lubridate)
library(stringr)


## Define reporting years
year_xx <- year(cut_off_date)
year_ww <- year_xx - 1
year_vv <- year_xx - 2
year_yy <- year_xx + 1
year_3 <- year_xx - 3
year_5 <- year_xx - 5


## KPI 4.1 previous stats
rate_41 <- theme4_4 |> 
  filter(kpi == "KPI 4.1",
         group == "deaths_p") |> 
  filter(str_detect(financial_year, kpi_report_years[1]) | 
           str_detect(financial_year, kpi_report_years[2]))

## KPI 4.2 previous stats
rate_42 <- theme4_4 |> 
  filter(kpi == "KPI 4.2",
         group == "deaths_p") |> 
  filter(str_detect(financial_year, kpi_report_years[1]) | 
           str_detect(financial_year, kpi_report_years[2]))

## KPI 4.1 1yr mortality rates previous stats
rate_41_1yr <- theme4_4 |> 
  filter(kpi == "KPI 4.1 1yr Rate",
         group == "deaths_p") |> 
  filter(str_detect(financial_year, kpi_report_years[1]) | 
           str_detect(financial_year, kpi_report_years[2]))

## KPI 4.2 1yr mortality rates previous stats
rate_42_1yr <- theme4_4 |> 
  filter(kpi == "KPI 4.2 1yr Rate",
         group == "deaths_p") |> 
  filter(str_detect(financial_year, kpi_report_years[1]) | 
           str_detect(financial_year, kpi_report_years[2]))


### Table of Contents ----
pub_year <- paste0("KPI data for year ending 31 March ", year_xx, " and some ",
                   "supplementary information are planned for publication in March ", year_xx)
meg_review <- paste0("For review at MEG in December ", year_xx)
note_toc <- paste0("The data for the year ending 31 March ", year_xx, 
                   " are released for data quality assurance and management ",
                   "information purposes and should not be placed in the public ",
                   "domain. The information can be shared locally with those who ",
                   "have a legitimate need to review the data for quality assurance ",
                   "or for managerial or operational purposes.")

writeData(wb, "Table of Contents", pub_year, startRow = 3, startCol = 1)
writeData(wb, "Table of Contents", meg_review, startRow = 4, startCol = 1)
writeData(wb, "Table of Contents", note_toc, startRow = 29, startCol = 1)


### KPI 3.1 ----
screened_year_vv <- paste0("Screened in year ending 31 March ", year_vv)
screened_year_ww <- paste0("Screened in year ending 31 March ", year_ww)
screened_year_xx <- paste0("Screened in year ending 31 March ", year_xx)

writeData(wb, "KPI 3.1", screened_year_vv, startRow = 4, startCol = 2)
writeData(wb, "KPI 3.1", screened_year_ww, startRow = 4, startCol = 5)
writeData(wb, "KPI 3.1", screened_year_xx, startRow = 4, startCol = 8)


### KPI 3.2 HB Residence ----
screened_year_wwr <- paste0("Screened in year ending 31 March ", year_ww, "r")

writeData(wb, "KPI 3.2 HB Residence", screened_year_vv, startRow = 4, startCol = 2)
writeData(wb, "KPI 3.2 HB Residence", screened_year_wwr, startRow = 4, startCol = 5)
writeData(wb, "KPI 3.2 HB Residence", screened_year_xx, startRow = 4, startCol = 8)


### KPI 3.2 HB Surgery ----
writeData(wb, "KPI 3.2 HB Surgery", screened_year_vv, startRow = 4, startCol = 2)
writeData(wb, "KPI 3.2 HB Surgery", screened_year_wwr, startRow = 4, startCol = 5)
writeData(wb, "KPI 3.2 HB Surgery", screened_year_xx, startRow = 4, startCol = 8)


### KPI 4.1 ----
note_41 <- paste0("1. Due to small numbers, data are reported for five-year ", 
                  "rolling periods and are presented at Scotland level. The 30-day ",
                  "mortality rates for the two previous five-year rolling periods ",
                  "were ", rate_41[1,5], "% (", rate_41[1,3], ") and ", 
                  rate_41[2,5], "% (", rate_41[2,3], ").")

writeData(wb, "KPI 4.1", note_41, startRow = 16, startCol = 1)


### KPI 4.2 ----
note_42 <- paste0("1. Due to small numbers, data are reported for five-year ", 
                  "rolling periods and are presented at Scotland level. The 30-day ",
                  "mortality rates for the two previous five-year rolling periods ",
                  "were ", rate_42[1,5], "% (", rate_42[1,3], ") and ", 
                  rate_42[2,5], "% (", rate_42[2,3], ").")

writeData(wb, "KPI 4.2", note_42, startRow = 16, startCol = 1)


### Table 7) Vascular referrals ----
ending_year_cum <- paste0("Cumulative total from implementation to 31 March ", year_xx)

writeData(wb, "7) Vascular referrals", screened_year_vv, startRow = 5, startCol = 2)
writeData(wb, "7) Vascular referrals", screened_year_ww, startRow = 5, startCol = 4)
writeData(wb, "7) Vascular referrals", screened_year_xx, startRow = 5, startCol = 6)
writeData(wb, "7) Vascular referrals", ending_year_cum, startRow = 5, startCol = 8)


### Vascular KPIs background ----
vasc_outcome_title <- paste0("Vascular KPIs background information: Vascular ",
                             "referral outcomes at 1 March ", year_xx)

writeData(wb, "Vascular KPIs background", vasc_outcome_title, startRow = 2, 
          startCol = 1)


### 1-year mortality rates ----
operations_title <- paste0("Operations in five-year period ", 
                           kpi_4_1yr$financial_year)
note_mort <- paste0("1. Five-year total: Due to small numbers, data are reported ", 
                    "for five-year rolling periods and are presented at Scotland ",
                    "level. The 1-year mortality rates for the two previous ",
                    "five-year rolling periods were: Open surgery ", rate_41_1yr[1,5], 
                    "% (", rate_41_1yr[1,3], ") and ", rate_41_1yr[2,5], "% (", 
                    rate_41_1yr[2,3], "); EVAR ", rate_42_1yr[1,5], "% (", 
                    rate_42_1yr[1,3], ") and ", rate_42_1yr[2,5], "% (", 
                    rate_42_1yr[2,3], ").")

writeData(wb, "1-year mortality rates", operations_title, startRow = 3, 
          startCol = 1)
writeData(wb, "1-year mortality rates", note_mort, startRow = 11, startCol = 1)


### 1,3,5-year mortality rates ----
note_mort_cum1 <- paste0("Cumulative total for operations from implementation ", 
                         "to 31 March ", year_ww, "p")
note_mort_cum3 <- paste0("Cumulative total for operations from implementation ", 
                         "to 31 March ", year_3, "p")
note_mort_cum5 <- paste0("Cumulative total for operations from implementation ", 
                         "to 31 March ", year_5, "p")
note_cum <- paste0("To allow enough time for patient outcomes to be followed ", 
                        "up, the latest years of surgery that can be included at ",
                        "this stage are ", year_ww, " (1-year follow-up), ",
                        year_3, " (3-year follow-up), and ", year_5, 
                        " (5-year follow-up), respectively.")

writeData(wb, "1, 3, 5-year mortality", note_mort_cum1, startRow = 5, startCol = 2)
writeData(wb, "1, 3, 5-year mortality", note_mort_cum3, startRow = 5, startCol = 5)
writeData(wb, "1, 3, 5-year mortality", note_mort_cum5, startRow = 5, startCol = 8)
writeData(wb, "1, 3, 5-year mortality", note_cum, startRow = 12, startCol = 1)


### AAA Repairs ----
ending_year_vv <- paste0("Year ending 31 March ", year_vv)
ending_year_ww <- paste0("Year ending 31 March ", year_ww)
ending_year_xx <- paste0("Year ending 31 March ", year_xx)

writeData(wb, "AAA Repairs", ending_year_vv, startRow = 5, startCol = 2)
writeData(wb, "AAA Repairs", ending_year_ww, startRow = 5, startCol = 5)
writeData(wb, "AAA Repairs", ending_year_xx, startRow = 5, startCol = 8)
writeData(wb, "AAA Repairs", ending_year_cum, startRow = 5, startCol = 11)


### Unfit for surgery ----
refer_year_vv <- paste0("Referrals who were screened in year ending 31 March ", year_vv)
refer_year_ww <- paste0("Referrals who were screened in year ending 31 March ", year_ww)
refer_year_xx <- paste0("Referrals who were screened in year ending 31 March ", year_xx)
refer_year_cum <- paste0("Cumulative total referrals who were screened from ",
                         "implementation to 31 March ", year_xx)

writeData(wb, "Unfit for surgery", refer_year_vv, startRow = 4, startCol = 2)
writeData(wb, "Unfit for surgery", refer_year_ww, startRow = 4, startCol = 5)
writeData(wb, "Unfit for surgery", refer_year_xx, startRow = 4, startCol = 8)
writeData(wb, "Unfit for surgery", refer_year_cum, startRow = 4, startCol = 11)


### Unfit for surgery follow-up ----
note_unfit_cum1 <- paste0("Cumulative total for operations from implementation ", 
                         "to 31 March ", year_ww, "p")
note_unfit_cum3 <- paste0("Cumulative total for operations from implementation ", 
                         "to 31 March ", year_3, "p")
note_unfit_cum5 <- paste0("Cumulative total for operations from implementation ", 
                         "to 31 March ", year_5, "p")

writeData(wb, "Unfit for surgery follow-up", note_unfit_cum1, startRow = 4, startCol = 2)
writeData(wb, "Unfit for surgery follow-up", note_unfit_cum3, startRow = 4, startCol = 5)
writeData(wb, "Unfit for surgery follow-up", note_unfit_cum5, startRow = 4, startCol = 8)
writeData(wb, "Unfit for surgery follow-up", note_cum, startRow = 24, startCol = 1)


## Unfit follow-up deaths by cause ---
note_cause_cum <- paste0("Cumulative total referrals who werescreened from ",  
                         "implementation to 31 March ", year_xx)

writeData(wb, sheet = "Unfit follow-up deaths by cause", note_cause_cum, 
          startRow = 4, startCol = 2)
writeData(wb, sheet = "Unfit follow-up deaths by cause", note_cause_cum, 
          startRow = 9, startCol = 2)
writeData(wb, sheet = "Unfit follow-up deaths by cause", note_cause_cum, 
          startRow = 23, startCol = 2)


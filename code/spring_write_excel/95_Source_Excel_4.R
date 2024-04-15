# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 95_Source_Excel_4.R
# 
# Karen Hotopp & Aoife McCarthy
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

# orange font for notes needing manual input
orange_font <- createStyle(fontSize = 11, fontName = "Arial", 
                           fontColour = "#ff9f00", wrapText = TRUE)

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

## KPI 4.1 & 4.2 1yr mortality rates previous stats
rate_4_1yr <- tail(kpi_4_1yr, n = 3)

rate_4_1yr <- rate_4_1yr |>
  select(financial_year, Open_deaths_p, EVAR_deaths_p)


### Table of Contents ----
pub_year <- paste0("KPI data for year ending 31 March ", year_xx, " and some ",
                   "supplementary information are planned for publication in April ", year_yy)
meg_review <- paste0("For review at MEG in ", meg_month, " ", year_xx)
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
screened_year_vvr <- paste0("Screened in year ending 31 March ", year_vv, {supsc('r')})
screened_year_wwr <- paste0("Screened in year ending 31 March ", year_ww, {supsc('r')})
screened_year_xxp <- paste0("Screened in year ending 31 March ", year_xx, {supsc('p')},
                            '\n', "(Data not complete)")

kpi_3.1_prov <- paste0("p  Provisonal. Data are for the 11-month period 1 April ",
                       year_ww, " to ", extract_date, " ", year_xx,
                       " and do not include data for referrrals with no outpatient ",
                       "date recorded. In the 11-month period, there were a total ",
                       "of {x} vascular referrals. At ", extract_date, " ",
                       year_xx, " (date of extract), {x} of these referrals had ",
                       "an outpatient date recorded and {x} had no outpatient ",
                       "date recorded.")


kpi_3.1_revised <- paste0("r  Revised since published on {publication date [20XX]} ",
                       year_xx, ", due to updates in the data recorded on the ",
                       "Scottish AAA Call Recall System. The Scotland rate for ",
                       "the year ending 31 March ", year_ww, " was previously ",
                       "{x, taken from previous autumn report}%. Figures for all ",
                       "time periods in this table are based on data recorded on ",
                       "the Scottish AAA Call Recall System at ", extract_date,
                       " ", year_xx, " (date of data extraction).")


writeData(wb, "KPI 3.1", screened_year_vvr, startRow = 4, startCol = 2)
writeData(wb, "KPI 3.1", screened_year_wwr, startRow = 4, startCol = 5)
writeData(wb, "KPI 3.1", screened_year_xxp, startRow = 4, startCol = 8)
writeData(wb, "KPI 3.1", kpi_3.1_prov, startRow = 32)
addStyle(wb, "KPI 3.1", style = orange_font, rows = 32, cols = 1)
writeData(wb, "KPI 3.1", kpi_3.1_revised, startRow = 34)
addStyle(wb, "KPI 3.1", style = orange_font, rows = 34, cols = 1)

### KPI 3.2 HB Residence ----
kpi_3.2_hb_note1 <- paste0("r  Revised since published on {publication date year_xx} ",
                           year_xx, " due to updates of the data recorded on the ",
                           "Scottish AAA Call Recall System (Scotland figure was ",
                           "{Scotland % ", year_ww, " taken from previous autumn ",
                           "report}%). The revisions are mainly due to updates in ",
                           "the outcome of men who had a non-final outcome such as '",
                           "referred to other specialty' or 'ongoing assessment by ",
                           "vascular' at the time of the last publication. Some of ",
                           "these men have since been classified as 'appropriate ",
                           "for surgery' or have had surgery and are therefore ",
                           "included in the figures. Figures for all time-periods ",
                           "in this table are based on data recorded on Scottish ",
                           "AAA Call Recall System at ", extract_date, " ", year_xx,
                           " (date of data extraction).")

kpi_3.2_hb_note2 <- paste0("p  Provisional. Data are for men screened from 1 April ",
                          year_ww, " to 31 December ", year_ww, " only to allow ",
                          "for the 8 week target timescale plus a data recording ",
                          "lag. A number of men screened in this period have a ",
                          "non-final outcome or no outcome recorded; therefore, ",
                          "the figures will change when the data becomes more ",
                          "complete. The denominator includes men with a non-final ",
                          "outcome of 'Appropriate for surgery: final outcome pending' ",
                          "with no surgery date recorded.")

writeData(wb, "KPI 3.2 HB Residence", screened_year_vvr, startRow = 4, startCol = 2)
writeData(wb, "KPI 3.2 HB Residence", screened_year_wwr, startRow = 4, startCol = 5)
writeData(wb, "KPI 3.2 HB Residence", screened_year_xxp, startRow = 4, startCol = 8)
writeData(wb, "KPI 3.2 HB Residence", kpi_3.2_hb_note1, startRow = 32)
addStyle(wb, "KPI 3.2 HB Residence", style = orange_font, rows = 32, cols = 1)
writeData(wb, "KPI 3.2 HB Residence", kpi_3.2_hb_note2, startRow = 34)


### KPI 3.2 HB Surgery ----
writeData(wb, "KPI 3.2 HB Surgery", screened_year_vvr, startRow = 4, startCol = 2)
writeData(wb, "KPI 3.2 HB Surgery", screened_year_wwr, startRow = 4, startCol = 5)
writeData(wb, "KPI 3.2 HB Surgery", screened_year_xxp, startRow = 4, startCol = 8)
writeData(wb, "KPI 3.2 HB Surgery", kpi_3.2_hb_note1, startRow = 25)
addStyle(wb, "KPI 3.2 HB Surgery", style = orange_font, rows = 25, cols = 1)
writeData(wb, "KPI 3.2 HB Surgery", kpi_3.2_hb_note2, startRow = 27)

### KPI 4.1 ----
note_41 <- paste0("1. Due to small numbers, data are reported for five-year ", 
                  "rolling periods and are presented at Scotland level. The 30-day ",
                  "mortality rates for the two previous five-year rolling periods ",
                  "were ", rate_41[1,5], "% (", rate_41[1,3], ") and ", 
                  rate_41[2,5], "% (", rate_41[2,3], ").")

kpi_4_note_prov <- paste0("p  Provisional. Data for ", year_ww, "/", substr(year_xx, 3, 4),
                       " are for the 11-month period 1 April ", year_ww, " to 28 ",
                       "February ", year_xx, ". The vascular referral data recorded ",
                       "for this period are incomplete at this stage. At ", extract_date,
                       " ", year_xx, " (date of data extraction), around a third ",
                       "of all vascular referrals in the 11-month period had no ",
                       "vascular outcome data recorded.")

writeData(wb, "KPI 4.1", note_41, startRow = 16, startCol = 1)
writeData(wb, "KPI 4.1", kpi_4_note_prov, startRow = 18, startCol = 1)
addStyle(wb, "KPI 4.1", style = orange_font, rows = 18, cols = 1)

### KPI 4.2 ----
note_42 <- paste0("1. Due to small numbers, data are reported for five-year ", 
                  "rolling periods and are presented at Scotland level. The 30-day ",
                  "mortality rates for the two previous five-year rolling periods ",
                  "were ", rate_42[1,5], "% (", rate_42[1,3], ") and ", 
                  rate_42[2,5], "% (", rate_42[2,3], ").")

writeData(wb, "KPI 4.2", note_42, startRow = 16, startCol = 1)
writeData(wb, "KPI 4.2", kpi_4_note_prov, startRow = 18, startCol = 1)
addStyle(wb, "KPI 4.2", style = orange_font, rows = 18, cols = 1)

### KPI 4.1 additional ----

writeData(wb, "KPI 4.1 Additional", kpi_4_note_prov, startRow = 62, startCol = 1)
addStyle(wb, "KPI 4.1 Additional", style = orange_font, rows = 62, cols = 1)

### KPI 4.2 additional ----

writeData(wb, "KPI 4.2 Additional", kpi_4_note_prov, startRow = 62, startCol = 1)
addStyle(wb, "KPI 4.2 Additional", style = orange_font, rows = 62, cols = 1)

### Table 7) Vascular referrals ----
screened_year_vv <- paste0("Screened in year ending 31 March ", year_vv)
screened_year_ww <- paste0("Screened in year ending 31 March ", year_ww)
screened_year_xx <- paste0("Screened in year ending 31 March ", year_xx)

ending_year_cum <- paste0("Cumulative total from implementation to 31 March ", year_xx)

table_7_prov <- paste0("p  Provisional. Data for year ending 31 March ", year_xx,
                    " are for the 11-month period 1 April ", year_ww, " to 28 ",
                    "February ", year_xx, ".")

writeData(wb, "7) Vascular referrals", screened_year_vv, startRow = 5, startCol = 2)
writeData(wb, "7) Vascular referrals", screened_year_ww, startRow = 5, startCol = 4)
writeData(wb, "7) Vascular referrals", screened_year_xx, startRow = 5, startCol = 6)
writeData(wb, "7) Vascular referrals", ending_year_cum, startRow = 5, startCol = 8)
writeData(wb, "7) Vascular referrals", table_7_prov, startRow = 15)


### Vascular KPIs background ----
vasc_outcome_title <- paste0("Vascular KPIs background information: Vascular ",
                             "referral outcomes at 1 March ", year_xx)
vasc_outcome_prov <- paste0("2. The vascular referral data recorded for the year ",
                         "ending 31 March ", year_xx, " are for the 11-month ",
                         "period 1 April ", year_ww, " to 28 February ", year_xx,
                         ". As shown in the table, the vascular referral data ",
                         "recorded for this period are incomplete at this stage, ",
                         "with around a third of referrals having no outcome data ",
                         "recorded.")


writeData(wb, "Vascular KPIs background", vasc_outcome_title, startRow = 2, 
          startCol = 1)
writeData(wb, "Vascular KPIs background", vasc_outcome_prov, startRow = 43)
addStyle(wb, "Vascular KPIs background", style = orange_font, rows = 43, cols = 1)

### 1-year mortality rates ----
operations_title <- paste0("Operations in five-year period ", 
                           kpi_4_1yr_tail$financial_year)

note_mort <- paste0("1. Five-year total: Due to small numbers, data are reported ", 
                    "for five-year rolling periods and are presented at Scotland ",
                    "level. The 1-year mortality rates for the two previous ",
                    "five-year rolling periods were: Open surgery ", rate_4_1yr[1,2], 
                    "% (", rate_4_1yr[1,1], ") and ", rate_4_1yr[2,2], "% (",
                    rate_4_1yr[2,1], "); EVAR ", rate_4_1yr[1,3], "% (", 
                    rate_4_1yr[1,1], ") and ", rate_4_1yr[2,3], "% (", 
                    rate_4_1yr[2,1], ").")

note_mort2 <- paste0("p Provisional. The number and percentage of deaths within ",
                     "1 year following AAA surgery are provisional for ", 
                     kpi_4_1yr_tail$financial_year, ": the 1-year follow-up period ",
                     "includes death registrations to 31 March ", year_xx, 
                     "; death registrations data for 1 January to 31 March ",
                     year_xx, " are provisional.")

writeData(wb, "1-year mortality rates", operations_title, startRow = 3, 
          startCol = 1)
writeData(wb, "1-year mortality rates", note_mort, startRow = 11, startCol = 1)
writeData(wb, "1-year mortality rates", note_mort2, startRow = 15, startCol = 1)

### 1,3,5-year mortality rates ----
mort_year_cum1 <- paste0("Cumulative total for operations from implementation ", 
                         "to 31 March ", year_ww, {supsc('p')})
mort_year_cum3 <- paste0("Cumulative total for operations from implementation ", 
                         "to 31 March ", year_3, {supsc('p')})
mort_year_cum5 <- paste0("Cumulative total for operations from implementation ", 
                         "to 31 March ", year_5, {supsc('p')})
note_cum <- paste0("To allow enough time for patient outcomes to be followed ", 
                   "up, the latest years of surgery that can be included at ",
                   "this stage are ", year_ww, " (1-year follow-up), ",
                   year_3, " (3-year follow-up), and ", year_5, 
                   " (5-year follow-up), respectively.")
note_prov <- paste0("p Provisional. The number and percentage of deaths ",
                    "following AAA surgery are provisional: the 1, 3 and 5-year ",
                    "follow-up periods include death registrations to 31 March ",
                    year_xx, "; death registrations data for 1 January to 31 March ",
                    year_xx, " are provisional.")

writeData(wb, "1, 3, 5-year mortality", mort_year_cum1, startRow = 5, startCol = 2)
writeData(wb, "1, 3, 5-year mortality", mort_year_cum3, startRow = 5, startCol = 5)
writeData(wb, "1, 3, 5-year mortality", mort_year_cum5, startRow = 5, startCol = 8)
writeData(wb, "1, 3, 5-year mortality", note_cum, startRow = 12, startCol = 1)
writeData(wb, "1, 3, 5-year mortality", note_prov, startRow = 14, startCol = 1)


### AAA Repairs ----
ending_year_vv <- paste0("Year ending 31 March ", year_vv)
ending_year_ww <- paste0("Year ending 31 March ", year_ww)
ending_year_xx <- paste0("Year ending 31 March ", year_xx)

writeData(wb, "AAA Repairs", ending_year_vv, startRow = 5, startCol = 2)
writeData(wb, "AAA Repairs", ending_year_ww, startRow = 5, startCol = 5)
writeData(wb, "AAA Repairs", ending_year_xx, startRow = 5, startCol = 8)
writeData(wb, "AAA Repairs", ending_year_cum, startRow = 5, startCol = 11)
writeData(wb, "AAA Repairs", kpi_4_note_prov, startRow = 26, startCol = 1)

### Unfit for surgery ----
refer_year_vv <- paste0("Referrals who were screened in year ending 31 March ", year_vv)
refer_year_ww <- paste0("Referrals who were screened in year ending 31 March ", year_ww)
refer_year_xx <- paste0("Referrals who were screened in year ending 31 March ", year_xx)
refer_year_cum <- paste0("Cumulative total referrals who were screened from ",
                         "implementation to 31 March ", year_xx)

unfit_p <- paste0("p  Provisional. Data for the year ending 31 March ", year_xx, 
                  " are for the 11-month period 1 April ", year_ww, " to 28 February ",
                  year_xx, ". A number of vascual referrals for this period have ",
                  "a non-final outcome or no outcome recorded at this stage; ",
                  "therefore, the figures will change when the data becomes more complete.")													


writeData(wb, "Unfit for surgery", refer_year_vv, startRow = 4, startCol = 2)
writeData(wb, "Unfit for surgery", refer_year_ww, startRow = 4, startCol = 5)
writeData(wb, "Unfit for surgery", refer_year_xx, startRow = 4, startCol = 8)
writeData(wb, "Unfit for surgery", refer_year_cum, startRow = 4, startCol = 11)
writeData(wb, "Unfit for surgery", unfit_p, startRow = 27, startCol = 1)

### Unfit for surgery follow-up ----
unfit_year_cum1 <- paste0("Cumulative total for operations from implementation ", 
                         "to 31 March ", year_ww, {supsc('p')})
unfit_year_cum3 <- paste0("Cumulative total for operations from implementation ", 
                         "to 31 March ", year_3, {supsc('p')})
unfit_year_cum5 <- paste0("Cumulative total for operations from implementation ", 
                         "to 31 March ", year_5, {supsc('p')})

writeData(wb, "Unfit for surgery follow-up", unfit_year_cum1, startRow = 4, startCol = 2)
writeData(wb, "Unfit for surgery follow-up", unfit_year_cum3, startRow = 4, startCol = 5)
writeData(wb, "Unfit for surgery follow-up", unfit_year_cum5, startRow = 4, startCol = 8)
writeData(wb, "Unfit for surgery follow-up", note_cum, startRow = 24, startCol = 1)
writeData(wb, "Unfit for surgery follow-up", note_cum, startRow = 24, startCol = 1)
writeData(wb, "Unfit for surgery follow-up", note_prov, startRow = 26, startCol = 1)


## Unfit follow-up deaths by cause ---
cause_year_cum <- paste0("Cumulative total referrals who were screened from ",  
                         "implementation to 31 March ", year_xx)

unfit_deaths_prov <- paste0("p  Provisional. Data for the year ending 31 March ",
                            year_xx, " are for the 11-month period 1 April ",
                            year_ww, " to 28 February ", year_xx, ". A number of ",
                            "vascular referrals for this period have a non-final ",
                            "outcome or no outcome recorded at this stage; ",
                            "therefore, the number of men deemed unfit for surgery ",
                            "will change when the data are complete. In addition, ",
                            "death registrations data for 1 January to 31 March ",
                            year_xx, " are provisional.")

writeData(wb, sheet = "Unfit follow-up deaths by cause", cause_year_cum, 
          startRow = 4, startCol = 2)
writeData(wb, sheet = "Unfit follow-up deaths by cause", cause_year_cum, 
          startRow = 9, startCol = 2)
writeData(wb, sheet = "Unfit follow-up deaths by cause", cause_year_cum, 
          startRow = 22, startCol = 2)
writeData(wb, sheet = "Unfit follow-up deaths by cause", unfit_deaths_prov, 
          startRow = 33, startCol = 1)

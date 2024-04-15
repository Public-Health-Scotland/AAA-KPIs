# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 98_Write_Excel_1.R
# 
# Karen Hotopp & Aoife McCarthy
# February 2024
# 
# DESCRIPTION
# 
# Written/run on Posit WB, R 4.1.2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Notes:
# 


#### 1: Housekeeping ----
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(openxlsx)
library(lubridate)


rm(list=ls())
gc()

## Values
source(here::here("code/0_housekeeping.R"))

rm (exclusions_path, extract_path, hist_path, simd_path,
    fy_list, hb_list, fy_tibble, hb_tibble,
    cutoff_date, end_current, end_date, start_date,
    year1_end, year1_start, year2_end, year2_start, year1, year2)

year_xx <- year(cut_off_date)
year_ww <- year_xx - 1
year_vv <- year_xx - 2
year_uu <- year_xx - 3
year_yy <- year_xx + 1

## File paths
template_path <- paste0("/PHI_conf/AAA/Topics/Screening/templates")


### 2: Import and format data ----
## KPI 1
kpi_1 <- read_rds(paste0(temp_path, "/2_1_invite_attend_", yymm, ".rds")) |> 
  filter(fin_year %in% c(kpi_report_years),
         kpi %in% c("KPI 1.1", "KPI 1.2a", "KPI 1.3a Scotland SIMD",
                    "KPI 1.4a", "KPI 1.4b"),
         hbres == "Scotland",
         !simd %in% c("Total", "Unknown"),
         str_ends(group, "_p")) |> 
  # match Excel tables
  pivot_wider(names_from = fin_year, values_from = value) 

## KPI 2
kpi_2 <- read_rds(paste0(temp_path, "/3_1_kpi_2_", yymm, ".rds"))|> 
  filter(fin_year %in% c(kpi_report_years),
         kpi %in% c("KPI 2.1a", "KPI 2.1b", "KPI 2.2"),
         hbres == "Scotland",
         str_ends(group, "_p")) |> 
  # match Excel tables
  pivot_wider(names_from = fin_year, values_from = value) 

## KPI 3
kpi_3 <- read_rds(paste0(temp_path, "/4_1_kpi_3_", yymm, ".rds")) |> 
  filter(financial_year %in% c(kpi_report_years),
         kpi %in% c("KPI 3.1 Residence", "KPI 3.2 Residence", 
                    "KPI 3.2 Surgery"),  # Should this last one stay in??
         health_board == "Scotland",
         str_ends(group, "_p")) |> 
  # match Excel tables
  pivot_wider(names_from = financial_year, values_from = value) 

## KPI 4
kpi_4 <- read_rds(paste0(temp_path, "/4_2_kpi_4_", yymm, ".rds")) |> 
  filter(kpi %in% c("KPI 4.1", "KPI 4.2"),
         str_ends(group, "_p")) |> 
  mutate(year = str_sub(financial_year, 11)) |> 
  filter(year %in% c(kpi_report_years)) |> 
  select(-year) |> 
  # match Excel tables
  pivot_wider(names_from = financial_year, values_from = value)




## Save out files to use in publication
write_rds(kpi_1, paste0(temp_path, "/6_kpi_1_", yymm, ".rds"))
write_rds(kpi_2, paste0(temp_path, "/6_kpi_2_", yymm, ".rds"))
write_rds(kpi_3, paste0(temp_path, "/6_kpi_3_", yymm, ".rds"))
write_rds(kpi_4, paste0(temp_path, "/6_kpi_4_", yymm, ".rds"))


### AMC additions below:

## Format for Excel input
kpi_1 <- kpi_1 %>% select(-c(hbres, kpi, simd, group)) %>% 
  mutate_all(.funs = function(x) paste0(x, "%"))
kpi_2 <- kpi_2 %>% select(-c(hbres, kpi, group)) %>% 
  mutate_all(.funs = function(x) paste0(x, "%"))
kpi_3 <- kpi_3 %>% select(-c(health_board, kpi, group)) %>% 
  mutate_all(.funs = function(x) paste0(x, "%"))
kpi_4 <- kpi_4 %>% select(-c(kpi, surg_method, group)) %>% 
  mutate_all(.funs = function(x) paste0(x, "%"))

### 3: Output to Excel ----

# load workbook
wb <- loadWorkbook(paste0(template_path, "/1_Scotland KPI Summary_", season, ".xlsx"))

# Notes and headers
today <- paste0("Workbook created ", Sys.Date())
meg_review <- paste0("For review at MEG in ", meg_month, " ", year_xx)
data_header <- paste0("Data for year ending 31 March ", year_xx, " scheduled to ",
                      "be published in April ", year_yy, " (final data will be ",
                      "produced from data extracted for PHS in September ",
                      year_xx, ").")

# "data notes" sheet - additional notes
extract_note1 <- paste0("Public Health Scotland (PHS) receives data extracts from ",
                     "the system for the purpose of producing and publishing ",
                     "statistics on the AAA Screening Programme in Scotland. ",
                     "Data for KPIs 1.1 to 2.2b for the years ending 31 March ",
                     year_vv, " and 31 March ", year_ww, " were extracted from ",
                     "Scottish AAA Call-Recall System on ", "[extract date year_vv] ",
                     year_vv, " and ", "[extract date year_ww] ", year_ww,
                     ", respectively. The provisional/partial data for the year ",
                     "ending 31 March ", year_xx, " were extracted on ", 
                     extract_date, " ", year_xx, ". Data for all time ",
                     "periods for the vascular referral KPIs (3.1 to 4.2) were ",
                     "extracted on ", extract_date, " ", year_xx, ".")	

extract_note2 <- paste0("Supplementary tables: for Tables 1 to 5, the data for all ",
                     "time periods were extracted on ", extract_date, " ",
                     year_xx, " so that these cohort-based data include any ",
                     "updates in the initial screening tests and results. For ",
                     "Tables 6 and 7, the data for the year ending 31 March ",
                     year_vv, " were extracted on ", "[extract date year_vv] ",
                     year_vv, " and data for the year ending 31 March ", year_ww,
                     " were extracted on ", "[extract date year_ww] ", year_ww,
                     ". Provisional/partial data for the year and cumulative ",
                     "period ending 31 March ", year_xx, " were extracted on ",
                     extract_date, " ", year_xx, ".")

extract_note3 <- paste0(year_uu, "/", substr(year_vv, 3,4))
extract_note4 <- paste0(year_vv, "/", substr(year_ww, 3,4))
extract_note5 <- paste0(year_ww, "/", substr(year_xx, 3,4))
extract_note6 <- paste0(extract_date, " ", year_xx)

cohort_date1.1 <- as.character(as.numeric(year_uu)-66)
cohort_date1.2 <- as.character(as.numeric(year_uu)-65)
cohort_note1.1 <- paste0("Born 1 April ", cohort_date1.1, " to 31 March ",
                         cohort_date1.2)
cohort_note1.2 <- paste0("Year ending 31 March ", year_uu)
cohort_note1.3 <- paste0("Year ending 31 March ", year_vv)

cohort_date2.1 <- as.character(as.numeric(year_vv)-66)
cohort_date2.2 <- as.character(as.numeric(year_vv)-65)
cohort_note2.1 <- paste0("Born 1 April ", cohort_date2.1, " to 31 March ",
                         cohort_date2.2)
cohort_note2.2 <- paste0("Year ending 31 March ", year_vv)
cohort_note2.3 <- paste0("Year ending 31 March ", year_ww)

cohort_date3.1 <- as.character(as.numeric(year_ww)-66)
cohort_date3.2 <- as.character(as.numeric(year_ww)-65)
cohort_note3.1 <- paste0("Born 1 April ", cohort_date3.1, " to 31 March ",
                         cohort_date3.2)
cohort_note3.2 <- paste0("Year ending 31 March ", year_ww)
cohort_note3.3 <- paste0("Year ending 31 March ", year_xx)

rm(cohort_date1.1, cohort_date1.2, cohort_date2.1, cohort_date2.2, cohort_date3.1,
   cohort_date3.2)

summary_note1 <- paste0("r  Data are revised since published on ",
                        "[publication date [20XX]] ", year_xx, ". For KPI 3.1, ",
                        "the data recorded on vascular referrals screened in ",
                        "the year ending 31 March ", year_ww, " have been updated ",
                        "to reflect the latest available information on these ",
                        "referrals ({x}% when published). For KPI 3.2 Residence, ",
                        "the data recorded on vascular referrals screened in the ",
                        "year ending 31 March ", year_ww, " have been updated to ",
                        "reflect the latest available information on these referrals ",
                        "({x}% when published). For KPI 3.2 Surgery, the data ",
                        "recorded on vascular referrals screened in the year ",
                        "ending 31 March ", year_ww, " have been updated to reflect ",
                        "the latest available information on these referrals ",
                        "({x}% when published).")

year_end_vv <- paste0("Year ending", '\n', "31 March ", year_vv)
year_end_ww <- paste0("Year ending", '\n', "31 March ", year_ww)
year_end_xx <- paste0("Year ending", '\n', "31 March ", year_xx, '\n',
                      "(provisional/partial", '\n', "data)")

# Styles
# bold_red_font <- createStyle(fontSize = 12, fontName = "Arial",
#                             textDecoration = "bold", fontColour = "#FF0000")
bold_black_font <- createStyle(fontSize = 12, fontName = "Arial",
                               textDecoration = "bold", fontColour = "#000000")
# orange font used for provisional notes that need manually updating in final book
orange_font <- createStyle(fontSize = 11, fontName = "Arial",
                           fontColour = "#ff9f00", wrapText = TRUE)
# summary header white font
white_font <- createStyle(fontSize = 12, fontName = "Arial", fgFill = "#462682",
                          fontColour = "#ffffff", wrapText = TRUE,
                          border = c("top", "bottom", "left", "right"),
                          borderStyle = "medium", halign = "center", valign = "bottom",
                          textDecoration = "bold")


# Data Notes
writeData(wb, "Data Notes", data_header, startRow = 2)
writeData(wb, "Data Notes", meg_review, startRow = 3)
addStyle(wb, "Data Notes", bold_black_font, rows = 3, cols = 1)
writeData(wb, "Data Notes", today, startRow = 5)

writeData(wb, "Data Notes", extract_note1, startRow = 19)
addStyle(wb, "Data Notes", orange_font, rows = 19, cols = 1)
writeData(wb, "Data Notes", extract_note2, startRow = 20)
addStyle(wb, "Data Notes", orange_font, rows = 20, cols = 1)

writeData(wb, "Data Notes", extract_note3, startRow = 23)
writeData(wb, "Data Notes", extract_note4, startRow = 24)
writeData(wb, "Data Notes", extract_note5, startRow = 25)
writeData(wb, "Data Notes", extract_note6, startRow = 25, startCol = 2)
addStyle(wb, "Data Notes", orange_font, rows = 25, cols = 2)
writeData(wb, "Data Notes", extract_note6, startRow = 27, startCol = 2)
addStyle(wb, "Data Notes", orange_font, rows = 27, cols = 2)

writeData(wb, "Data Notes", cohort_note1.1, startRow = 39)
writeData(wb, "Data Notes", cohort_note1.2, startRow = 39, startCol = 2)
writeData(wb, "Data Notes", cohort_note1.3, startRow = 39, startCol = 3)
writeData(wb, "Data Notes", cohort_note2.1, startRow = 40)
writeData(wb, "Data Notes", cohort_note2.2, startRow = 40, startCol = 2)
writeData(wb, "Data Notes", cohort_note2.3, startRow = 40, startCol = 3)
writeData(wb, "Data Notes", cohort_note3.1, startRow = 41)
writeData(wb, "Data Notes", cohort_note3.2, startRow = 41, startCol = 2)
writeData(wb, "Data Notes", cohort_note3.3, startRow = 41, startCol = 3)

showGridLines(wb, "Data Notes", showGridLines = FALSE)

# Scotland Summary
writeData(wb, "Scotland Summary", data_header, startRow = 2)
writeData(wb, "Scotland Summary", meg_review, startRow = 3)
addStyle(wb, "Scotland Summary", bold_black_font, rows = 3, cols = 1)
writeData(wb, "Scotland Summary", today, startRow = 5)

writeData(wb, "Scotland Summary", year_end_vv, startRow = 10, startCol = 6)
addStyle(wb, "Scotland Summary", white_font, rows = 10, cols = 6)
writeData(wb, "Scotland Summary", year_end_ww, startRow = 10, startCol = 7)
addStyle(wb, "Scotland Summary", white_font, rows = 10, cols = 7)
writeData(wb, "Scotland Summary", year_end_xx, startRow = 10, startCol = 8)
addStyle(wb, "Scotland Summary", white_font, rows = 10, cols = 8)

writeData(wb, "Scotland Summary", kpi_1, startRow = 12,
          startCol = 6, colNames = FALSE)
writeData(wb, "Scotland Summary", kpi_2, startRow = 22,
          startCol = 6, colNames = FALSE)
writeData(wb, "Scotland Summary", kpi_3, startRow = 26,
          startCol = 6, colNames = FALSE)
# kpi 3 revised superscripts
kpi3_r1 <- kpi_3[1,1] %>% pull() %>% paste0(., {supsc('r')})
kpi3_r2 <- kpi_3[1,2] %>% pull() %>% paste0(., {supsc('r')})
kpi3_r3 <- kpi_3[2,1] %>% pull() %>% paste0(., {supsc('r')})
kpi3_r4 <- kpi_3[2,2] %>% pull() %>% paste0(., {supsc('r')})
kpi3_r5 <- kpi_3[3,1] %>% pull() %>% paste0(., {supsc('r')})
kpi3_r6 <- kpi_3[3,2] %>% pull() %>% paste0(., {supsc('r')})
writeData(wb, "Scotland Summary", kpi3_r1, startRow = 26, startCol = 6)
writeData(wb, "Scotland Summary", kpi3_r2, startRow = 26, startCol = 7)
writeData(wb, "Scotland Summary", kpi3_r3, startRow = 27, startCol = 6)
writeData(wb, "Scotland Summary", kpi3_r4, startRow = 27, startCol = 7)
writeData(wb, "Scotland Summary", kpi3_r5, startRow = 28, startCol = 6)
writeData(wb, "Scotland Summary", kpi3_r6, startRow = 28, startCol = 7)

writeData(wb, "Scotland Summary", kpi_4, startRow = 31,
          startCol = 6, colNames = FALSE)

# 5-year titles for kpi 4

rolling_font <- createStyle(fontSize = 12, fontName = "Arial",
                               textDecoration = "bold", fontColour = "#000000",
                            wrapText = TRUE, border = c("top", "bottom", "left", "right"),
                            borderStyle = "medium", halign = "center", valign = "center")

rolling1 <- paste0("Results for ", year_uu-4, "/", substr(year_vv-4, 3, 4), " - ", 
                   '\n', year_uu, "/", substr(year_vv, 3, 4))
rolling2 <- paste0("Results for ", year_vv-4, "/", substr(year_ww-4, 3, 4), " - ", 
                   '\n', year_vv, "/", substr(year_ww, 3, 4))
rolling3 <- paste0("Results for ", year_ww-4, "/", substr(year_xx-4, 3, 4), " - ", 
                   '\n', year_ww, "/", substr(year_xx, 3, 4))

writeData(wb, "Scotland Summary", rolling1, startRow = 30, startCol = 6)
addStyle(wb, "Scotland Summary", rolling_font, rows =30, cols = 6)
writeData(wb, "Scotland Summary", rolling2, startRow = 30, startCol = 7)
addStyle(wb, "Scotland Summary", rolling_font, rows =30, cols = 7)
writeData(wb, "Scotland Summary", rolling3, startRow = 30, startCol = 8)
addStyle(wb, "Scotland Summary", rolling_font, rows =30, cols = 8)

writeData(wb, "Scotland Summary", summary_note1, startRow = 44)
addStyle(wb, "Scotland Summary", orange_font, rows =44, cols = 1)

showGridLines(wb, "Scotland Summary", showGridLines = FALSE)

# Glossary and Key Terms
showGridLines(wb, "Glossary and Key Terms", showGridLines = FALSE)


## Save ----
saveWorkbook(wb, paste0(output_path,
                        "/1_Scotland KPI Summary_", yymm, ".xlsx"), 
             overwrite = TRUE)

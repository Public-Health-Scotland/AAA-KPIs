###############################################################################
# kpi_3.1_3.2.R
# Calum Purdie
# 17/01/2023
# 
# KPI 3.1 percentage of men with AAA >= 5.5cm seen within two weeks of screening
# KPI 3.2 percentage of men with AAA >= 5.5cm deemed appropriate for 
# intervention/operated on within 8 weeks of screening
#
# Written/run on R Studio Server, R version 3.6.1
# Revised on Posit PWB, R Version 4.1.2
###############################################################################

## Notes: 
## For KPI 3.2, numbers from published data may have minor revisions due 
# to updates of the data recorded on the Scottish AAA Call-Recall System. 
# Therefore, all KPI data is recalculated for each MEG.

## From Sept 2023, KPI 3.2 now includes analysis by HB of surgery (in addition 
# to HB of residence)


### 1: Housekeeping ----
# Load packages
library(dplyr)
library(readr)
library(lubridate)
# library(phsmethods)
library(janitor)
library(tidylog)


rm(list = ls())
gc()


source(here::here("code/0_houskeeping_theme_4.R"))


### 2: Data Manipulation ----
# Keep records where date_referral_true is populated and largest measure is 
# greater than or equal to 5.5, where result_outcome is not "02" and 
# date_screen is less than or equal to cut_off_date
aaa_extract <- read_rds(extract_path) %>% 
  filter(!is.na(date_referral_true) & largest_measure >= 5.5, 
         result_outcome != "02", 
         date_screen <= cut_off_date)


#### 3: KPI 3.1 ----
# Calculate time between date seen in outpatients to date of screening
kpi_3_1 <- aaa_extract %>% 
  mutate(screen_to_screen = time_length(date_screen %--% date_seen_outpatient, 
                                        "days"), 
         seen = case_when(screen_to_screen <= 14 ~ 1, 
                          TRUE ~ 0), 
         screen_to_screen_group = 
           case_when(screen_to_screen >= 0 & screen_to_screen <= 7 ~ 1, 
                     screen_to_screen >= 8 & screen_to_screen <= 14 ~ 2, 
                     screen_to_screen >= 15 & screen_to_screen <= 21 ~ 3, 
                     screen_to_screen >= 22 & screen_to_screen <= 28 ~ 4, 
                     screen_to_screen >= 29 & screen_to_screen <= 42 ~ 5, 
                     screen_to_screen >= 43 ~ 6))

# Check variables
# Do these add value? Do we need them?
kpi_3_1 %>% count(screen_to_screen)
kpi_3_1 %>% count(seen)
kpi_3_1 %>% count(screen_to_screen_group)

# Keep records where result_outcome is "01", "03", "04" or "05" or where
# date_seen_outpatient is populated and screen_to_screen is greater than or equal 
# to zero
kpi_3_1 <- kpi_3_1 %>% 
  filter(result_outcome %in% c("01", "03", "04", "05") | 
           !is.na(date_seen_outpatient) & screen_to_screen >= 0)


### Health Board of Residence ----
# Health Boards
kpi_3_1_hb <- kpi_3_1 %>% 
  group_by(hbres, financial_year) %>% 
  summarise(cohort_n = n(), 
            seen_n = sum(seen)) |> 
  ungroup()
  
# Scotland
kpi_3_1_scot <- kpi_3_1 %>% 
  group_by(financial_year) %>% 
  summarise(cohort_n = n(), 
            seen_n = sum(seen)) %>% 
  mutate(hbres = "Scotland", .before = financial_year)

# Combine
kpi_3_1_res <- bind_rows(kpi_3_1_scot, kpi_3_1_hb) %>% 
  group_by(hbres) %>% 
  mutate(#cum_referrals = sum(cohort), 
         #cum_seen = sum(seen), 
         #pc_cum_seen = round_half_up(cum_seen * 100 / cum_referrals, 1),  
         cover_p = round_half_up(seen_n * 100 / cohort_n, 1)) |> 
  mutate(kpi = "KPI 3.1 Residence", .after = hbres)

#### This next chunk of code adds in FY where missing (produces NAs), but is 
## it better to store without the extra (NA) data produced, as the missing FYs 
## are automatically created when the data is pivoted to match Excel output?
# Ensure every HB is represented every financial year
kpi_3_1_res <- kpi_3_1_res |>
  pivot_longer(!hbres:financial_year, names_to = "group", values_to = "value") |> 
  mutate(financial_year_group = paste(financial_year, group, sep = "_")) |> 
  select(hbres, kpi, financial_year_group, value) |> 
  pivot_wider(names_from = financial_year_group, values_from = value)

kpi_3_1_res <- hb_list |> left_join(kpi_3_1_res, by = "hbres")

kpi_3_1_res <- kpi_3_1_res |> 
  pivot_longer(!hbres:kpi, names_to = "group", values_to = "value") |> 
  mutate(financial_year = group, .after = kpi) |> 
  mutate(financial_year = stringr::str_remove(financial_year, "_cohort_n"),
         financial_year = stringr::str_remove(financial_year, "_seen_n"),
         financial_year = stringr::str_remove(financial_year, "_cover_p"),
         group = case_when(stringr::str_detect(group, "cohort") ~ "cohort_n",
                           stringr::str_detect(group, "seen") ~ "seen_n",
                           stringr::str_detect(group, "cover") ~ "cover_p"))

table(kpi_3_1_res$hbres, kpi_3_1_res$financial_year) # all hbres/FY are 3
# Current run IS saved with this transformation, but to decide how to best store
#### 

## Run the below code if it is decided that above code (creates HB records w NAs) 
## should NOT used (this uses less data storage than above)
# kpi_3_1_res <- kpi_3_1_res |>
#   pivot_longer(!hbres:financial_year, names_to = "group", values_to = "value") 
# 
# table(kpi_3_1_res$hbres, kpi_3_1_res$financial_year) # NOT all hbres/FY are 3










# Tidy environment
rm(kpi_3_1_data, kpi_3_1_hb_totals, kpi_3_1_scot_totals, kpi_3_1)










# ### Step 6: Add historical data ----
# # Historical data from two previous published years needs to be added to 
# # KPI 3.1 and 3.2
# 
# ## Full records (currently only HB Residence; need to add HB Surgery)
# hist_db <- read_rds(paste0(hist_path,"/aaa_kpi_historical_theme_4.rds"))
# # save a backup of hist_db
# write_rds(hist_db, paste0(hist_path, "/aaa_kpi_historical_theme_4_bckp.rds"))
# # change permissions to give the group read/write
# Sys.chmod(paste0(hist_path, "/aaa_kpi_historical_theme_4_bckp.rds"),
#           mode = "664", use_umask = FALSE)
# 
# table(hist_db$fin_year, hist_db$kpi) # should be equal across FYs and match below 
# #         KPI 3.1 HB Residence KPI 3.2 HB Residence
# # 2012/13                   45                   45
# # 2013/14                   45                   45
# # 2014/15                   45                   45
# # 2015/16                   45                   45
# # 2016/17                   45                   45
# # 2017/18                   45                   45
# # 2018/19                   45                   45
# # 2019/20                   45                   45
# # 2020/21                   45                   45
# 
# 
# ## Add new records onto full database
# new_hist_db <- bind_rows(hist_db, kpi_summary)
# 
# ## Check for duplication
# table(new_hist_db$kpi, new_hist_db$fin_year) # current year (year1) should match 
# # previous years, plus 30 records for KPI 1.1 Sept coverage; ignore year2
# 
# 
# ## Current report output ----
# report_db <- new_hist_db |> 
#   filter(fin_year %in% c(kpi_report_years, year2))
# 
# write_rds(report_db, paste0(temp_path, "/3_invite_attend_", yymm, ".rds"))
# #write_csv(report_db, paste0(temp_path, "/3_invite_attend_", yymm, ".csv")) # for checking
# 
# 
# ## New historical database ----
# new_hist_db <- new_hist_db |>
#   mutate(kpi = fct_relevel(kpi, c("KPI 1.1", "KPI 1.2a", "KPI 1.2a Sept coverage", 
#                                   "KPI 1.2b", "KPI 1.3a Scotland SIMD", 
#                                   "KPI 1.3a Sept coverage", "KPI 1.3a HB SIMD", 
#                                   "KPI 1.3b Scotland SIMD", "KPI 1.3b HB SIMD")),
#          fin_year = fct_relevel(fin_year, c("2012/13", "2013/14", "2014/15", 
#                                             "2015/16", "2016/17", "2017/18", 
#                                             "2018/19", "2019/20", "2020/21", 
#                                             "2021/22", "2022/23")),
#          hbres = fct_relevel(hbres, c("Scotland","Ayrshire & Arran","Borders",
#                                       "Dumfries & Galloway", "Fife", "Forth Valley", 
#                                       "Grampian", "Greater Glasgow & Clyde",  
#                                       "Highland", "Lanarkshire", "Lothian", "Orkney",
#                                       "Shetland", "Tayside","Western Isles"))) |> 
#   arrange(kpi, fin_year, hbres)








### 4 KPI 3.2 ----

# Keep records where result_outcome is in specified list or result_outcome is
# "20" and date_surgery is populated
# Calculate time between date seen of surgery to date of screening
# Calculate surgery variable and flag as 1 where screen_to_surgery is less than 
# or equal to 56, 0 elsewhere
# Create groups for screen_to_surgery values

kpi_3_2_data <- aaa_extract %>% 
  filter(result_outcome %in% c("11", "12", "13", "14", "15", "16", "17") | 
           (result_outcome == "20" & surg_method == "03" & !is.na(date_surgery))) %>% 
  mutate(screen_to_surgery = time_length(date_screen %--% date_surgery, 
                                         "days"), 
         surgery = case_when(screen_to_surgery <= 56 ~ 1, 
                             TRUE ~ 0), 
         screen_to_surgery_group = 
           case_when(screen_to_surgery >= 0 & screen_to_surgery <= 14 ~ 1, 
                     screen_to_surgery >= 15 & screen_to_surgery <= 56 ~ 2, 
                     screen_to_surgery >= 57 & screen_to_surgery <= 112 ~ 3, 
                     screen_to_surgery >= 113 & screen_to_surgery <= 224 ~ 4, 
                     screen_to_surgery >= 225 & screen_to_surgery <= 365 ~ 5, 
                     screen_to_surgery >= 366 ~ 6))

# Check variables

kpi_3_2_data %>% count(screen_to_surgery)
kpi_3_2_data %>% count(surgery)
kpi_3_2_data %>% count(screen_to_surgery_group)

# Check result_outcome for records where date_surgery is blank

kpi_3_2_data %>% filter(is.na(date_surgery)) %>% count(result_outcome)

# Keep records where screen_to_surgery is greater than or equal to zero or blank

kpi_3_2_data <- kpi_3_2_data %>% 
  filter(screen_to_surgery >= 0 | is.na(screen_to_surgery))

# This section should be applied to the May MEG run because vascular data for
# the year end is not complete at that stage - it should not be run when
# producing data for the complete year end from the September extract

if (meg_month == "May"){
  
  kpi_3_2_data <- kpi_3_2_data %>% 
    mutate(pending_surgery = 
             case_when(result_outcome == "17" & is.na(date_surgery) ~ 1, 
                       TRUE ~ 0))
  
  kpi_3_2_data %>% count(pending_surgery)
  
}

# Calculate Health Board totals
# Group by hbres and financial_year
# Summarise totals to get cohort and sum surgery column

kpi_3_2_hb_totals <- kpi_3_2_data %>% 
  group_by(hbres, financial_year) %>% 
  summarise(cohort = n(), 
            surgery = sum(surgery))

# Calculate Health Board totals
# Group by financial_year
# Summarise totals to get cohort and sum surgery column

kpi_3_2_scot_totals <- kpi_3_2_data %>% 
  group_by(financial_year) %>% 
  summarise(cohort = n(), 
            surgery = sum(surgery)) %>% 
  mutate(hbres = "Scotland") %>% 
  relocate(hbres, .before = financial_year)

# Bind scot_totals and hb_totals
# Set NA to 0 for numeric columns
# Group by hbres and sum cohort and surgery
# Calculate percentage for cumulative surgeries and percentage seen within eight 
# weeks

kpi_3_2 <- bind_rows(kpi_3_2_scot_totals, kpi_3_2_hb_totals) %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) %>% 
  group_by(hbres) %>% 
  mutate(cum_approp = sum(cohort), 
         cum_surgery = sum(surgery)) %>% 
  mutate(pc_cum_surgery = round_half_up(cum_surgery * 100 / cum_approp, 1), 
         pc_within_eight_weeks = round_half_up(surgery * 100 / cohort, 1))

# Pivot data into wider format, using names from financial_year and values from
# cohort, surgery and pc_within_eight_weeks
# Select column order to match output file specification
# Arrange output file so that Scotland row is first

kpi_3_2_output <- kpi_3_2 %>%
  pivot_wider(names_from = financial_year, 
              values_from = c(cohort, surgery, pc_within_eight_weeks), 
              names_glue = "{financial_year}_{.value}") %>% 
  select(hbres, starts_with("2012/13"), starts_with("2013/14"), 
         starts_with("2014/15"), starts_with("2015/16"), starts_with("2016/17"), 
         starts_with("2017/18"), starts_with("2018/19"), starts_with("2019/20"), 
         starts_with("2020/21"), starts_with("2021/22"), starts_with("2022/23"),
         cum_approp, cum_surgery, pc_cum_surgery) %>% 
  arrange(hbres != "Scotland")

# Select hbres and latest three years columns

kpi_3_2_latest_years <- kpi_3_2_output %>%
  select(hbres, 
         starts_with(year_one), starts_with(year_two), starts_with(year_three))

# Tidy environment

rm(kpi_3_2_data, kpi_3_2_hb_totals, kpi_3_2_scot_totals, kpi_3_2, aaa_extract)




###### REWRITE BELOW FOR KPI 3.2!!!!! *******

### Health Board of Surgery ----
# First, remove records where HB of surgery is NA
kpi_3_1_surg <- kpi_3_1 |> 
  filter(!is.na(hb_surgery)) |> 
  mutate(hb_surgery = case_when(hb_surgery == "A" ~ "Ayrshire & Arran",
                                hb_surgery == "B" ~ "Borders",
                                hb_surgery == "D" ~ "Cumbria",
                                hb_surgery == "F" ~ "Fife",
                                hb_surgery == "G" ~ "Greater Glasgow & Clyde",
                                hb_surgery == "H" ~ "Highland",
                                hb_surgery == "L" ~ "Lanarkshire",
                                hb_surgery == "N" ~ "Grampian",
                                hb_surgery == "R" ~ "Orkney", # Needed?
                                hb_surgery == "S" ~ "Lothian",
                                hb_surgery == "T" ~ "Tayside",
                                hb_surgery == "V" ~ "Forth Valley",
                                hb_surgery == "W" ~ "Western Isles", # Needed?
                                hb_surgery == "Y" ~ "Dumfries & Galloway",
                                hb_surgery == "Z" ~ "Shetland")) # Needed?

table(kpi_3_1_surg$hb_surgery, useNA = "ifany")

cumbria <- kpi_3_1_surg[kpi_3_1_surg$hb_surgery == "Cumbria",]
# all records from (date_screen) 2015-2019
rm(cumbria)

##
no_surg <- filter(kpi_3_1, is.na(hb_surgery))
table(no_surg$date_surgery, no_surg$hbres) # 1 record from FV in 2018 (follow-up??)
rm(no_surg)
##

# Health Boards
kpi_3_1_hb <- kpi_3_1_surg %>% 
  group_by(hb_surgery, financial_year) %>% 
  summarise(cohort_n = n(), 
            seen_n = sum(seen)) |> 
  ungroup()

# Scotland
kpi_3_1_scot <- kpi_3_1_surg %>% # Should these exclude the Cumbria records??
  group_by(financial_year) %>% 
  summarise(cohort_n = n(), 
            seen_n = sum(seen)) %>% 
  mutate(hb_surgery = "Scotland", .before = financial_year)

# Combine
kpi_3_1_surg <- bind_rows(kpi_3_1_scot, kpi_3_1_hb) %>% 
  group_by(hb_surgery) %>% 
  mutate(#cum_referrals = sum(cohort), 
    #cum_seen = sum(seen), 
    #pc_cum_seen = round_half_up(cum_seen * 100 / cum_referrals, 1),  
    cover_p = round_half_up(seen_n * 100 / cohort_n, 1)) |> 
  mutate(kpi = "KPI 3.1 Residence", .after = hb_surgery)

#### This next chunk of code adds in FY where missing (produces NAs), but is 
## it better to store without the extra (NA) data produced, as the missing FYs 
## are automatically created when the data is pivoted to match Excel output?
# Ensure every HB is represented every financial year
kpi_3_1_res <- kpi_3_1_res |>
  pivot_longer(!hbres:financial_year, names_to = "group", values_to = "value") |> 
  mutate(financial_year_group = paste(financial_year, group, sep = "_")) |> 
  select(hbres, kpi, financial_year_group, value) |> 
  pivot_wider(names_from = financial_year_group, values_from = value)

kpi_3_1_res <- hb_list |> left_join(kpi_3_1_res, by = "hbres")

kpi_3_1_res <- kpi_3_1_res |> 
  pivot_longer(!hbres:kpi, names_to = "group", values_to = "value") |> 
  mutate(financial_year = group, .after = kpi) |> 
  mutate(financial_year = stringr::str_remove(financial_year, "_cohort_n"),
         financial_year = stringr::str_remove(financial_year, "_seen_n"),
         financial_year = stringr::str_remove(financial_year, "_cover_p"),
         group = case_when(stringr::str_detect(group, "cohort") ~ "cohort_n",
                           stringr::str_detect(group, "seen") ~ "seen_n",
                           stringr::str_detect(group, "cover") ~ "cover_p"))

table(kpi_3_1_res$hbres, kpi_3_1_res$financial_year) # all hbres/FY are 3
# Current run IS saved with this transformation, but to decide how to best store
#### 

## Run the below code if it is decided that above code (creates HB records w NAs) 
## should NOT used (this uses less data storage than above)
# kpi_3_1_res <- kpi_3_1_res |>
#   pivot_longer(!hbres:financial_year, names_to = "group", values_to = "value") 
# 
# table(kpi_3_1_res$hbres, kpi_3_1_res$financial_year) # NOT all hbres/FY are 3


















# ### 5 Output ----
# 
# # Create workbook
# 
# wb <- createWorkbook()
# 
# # Define a header style for workbook
# 
# hs <- createStyle(halign = "center", valign = "center", 
#                   textDecoration = "bold", border = "TopBottomLeftRight")
# 
# ## 5.1 - Create tab KPI1.4a --
# 
# addWorksheet(wb, sheetName = "KPI3.1", gridLines = FALSE)
# 
# # Add Titles
# writeData(wb, sheet = "KPI3.1", paste0("KPI 3.1: Percentage of men with AAA ≥ 5.5cm seen by vascular specialist within two weeks of screening"),
#           startCol = 1, startRow = 1)
# 
# writeData(wb, sheet = "KPI3.1", kpi_3_1_latest_years, borders = "all", headerStyle = hs, startCol = 1, startRow = 4)
# 
# setColWidths(wb, sheet = "KPI3.1", cols = 1:16, widths = "auto")
# 
# ## 5.2 - Create tab KPI1.4b --
# 
# addWorksheet(wb, sheetName = "KPI3.2", gridLines = FALSE)
# 
# # Add Titles
# writeData(wb, sheet = "KPI3.2", paste0("KPI 3.2: Percentage of men with AAA ≥ 5.5cm deemed appropriate for intervention who were operated on by vascular specialist within eight weeks of screenin"),
#           startCol = 1, startRow = 1)
# 
# writeData(wb, sheet = "KPI3.2", kpi_3_2_latest_years, borders = "all", headerStyle = hs, startCol = 1, startRow = 4)
# 
# setColWidths(wb, sheet = "KPI3.2", cols = 1:16, widths = "auto")
# 
# ## 5.3 - Save Workbook --
# 
# saveWorkbook(wb, file = (paste0("/PHI_conf/AAA/Topics/Screening/KPI", "/", year, 
#                                 month, "/temp/","kpi3_1__3_2.xlsx")) ,overwrite = TRUE)

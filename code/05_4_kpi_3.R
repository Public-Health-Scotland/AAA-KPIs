#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 05_4_kpi_3.R
# Calum Purdie & Karen Hotopp
# 17/01/2023
# 
# KPI 3.1 percentage of men with AAA >= 5.5cm seen within two weeks of screening
# KPI 3.2 percentage of men with AAA >= 5.5cm deemed appropriate for 
# intervention/operated on within 8 weeks of screening
#
# Written/run on R Studio Server, R version 3.6.1
# Revised on Posit WB, R Version 4.1.2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Notes: 
## For KPI 3.2, numbers from published data may have minor revisions due 
# to updates of the data recorded on the Scottish AAA Call-Recall System. 
# Therefore, all KPI data is recalculated for each QPMG.

## From Sept 2023, KPI 3.2 now includes analysis by HB of surgery (in addition 
# to HB of residence) as management information.


# 1: Housekeeping ----
# Load packages
library(dplyr)
library(readr)
library(lubridate)
library(janitor)
library(tidylog)
library(tidyr)
library(phsaaa) # to install: devtools::install_github("Public-Health-Scotland/phsaaa")
library(forcats)

rm(list = ls())
gc()


source(here::here("code/00_housekeeping.R"))

rm (exclusions_path, output_path, simd_path, fy_tibble, 
    qpmg_month, cutoff_date, year1_end, year1_start, year2_end, year2_start, 
    year1, year2, extract_date)


# 2: Data Manipulation ----

# 202409 vascular data update workaround
if(yymm == 202409) {
  extract_path <- "/PHI_conf/AAA/Topics/Screening/extracts/202409/output/aaa_extract_202409_updated_vasc.rds"
}


# Keep records where date_referral_true is populated and largest measure is 
# greater than or equal to 5.5, where result_outcome is not "02" and 
# date_screen is less than or equal to cut_off_date
aaa_extract <- read_rds(extract_path) #%>% 
  filter(!is.na(date_referral_true) & largest_measure >= 5.5, 
         result_outcome != "02", ## AMc note: this removes NAs too
         date_screen <= cut_off_date)


# 3: KPI 3.1 ----
# Calculate time between date seen in outpatients to date of screening
kpi_3_1_base <- aaa_extract %>% 
  mutate(screen_to_seen = time_length(date_screen %--% date_seen_outpatient, 
                                        "days"), 
         seen = case_when(screen_to_seen <= 14 ~ 1, 
                          TRUE ~ 0))


# KPI 3.1 provisional footnote - people referred but not yet seen
# This section should be applied to the spring QPMG run because vascular data for
# the year end is not complete at that stage - autumn run should be complete 
# these numbers get used in the provisional note of kpi 3.1 in theme 4 excel
if (season == "spring"){
  
  pending <- kpi_3_1_base %>% 
    filter(financial_year == kpi_report_years[3]) %>% 
    mutate(pending_appt = case_when(!is.na(date_referral_true) & is.na(date_seen_outpatient) ~ 1, # people referred, but not yet seen
                       TRUE ~ 0)) |> 
    count(pending_appt) |> 
    mutate(pending_appt = case_when(pending_appt == 0 ~ "Total referrals",
                                    pending_appt == 1 ~ "Referral date, no outpatient date",
                                    TRUE ~ "error"))
  
  query_write_rds(pending, paste0(temp_path, "/4_provisional_note_3.1.rds"))
  
}


# Keep records where result_outcome is "01", "03", "04" or "05" or where
# date_seen_outpatient is populated and screen_to_seen is greater than or equal 
# to zero
kpi_3_1_base <- kpi_3_1_base %>% 
  filter(result_outcome %in% c("01", "03", "04", "05") | # reason for not attending
           !is.na(date_seen_outpatient) & screen_to_seen >= 0) |> # evidence of attending
  mutate(financial_year = droplevels(financial_year))


## Health Board of Residence ----
# Health Boards
kpi_3_1_hb <- kpi_3_1_base %>% 
  group_by(hbres, financial_year) %>% 
  summarise(cohort_n = n(), 
            seen_n = sum(seen)) |> 
  ungroup() |> 
  complete(hbres, financial_year) |> # completing table - all HBs for each FY
  mutate_at(vars(cohort_n:seen_n), ~ifelse(is.na(.), 0, .))
  
# Scotland
kpi_3_1_scot <- kpi_3_1_base %>% 
  group_by(financial_year) %>% 
  summarise(cohort_n = n(), 
            seen_n = sum(seen)) %>% 
  mutate(hbres = "Scotland", .before = financial_year) |>
  ungroup()

# Combine
kpi_3_1_res <- bind_rows(kpi_3_1_scot, kpi_3_1_hb) %>% 
  # calculate coverage percentage
  mutate(cover_p = round_half_up(seen_n * 100 / cohort_n, 1)) |> 
  mutate(kpi = "KPI 3.1 Residence", .after = hbres) |>
  # arrange into longer format and order vars
  pivot_longer(!hbres:financial_year, names_to = "group", values_to = "value") |> 
  mutate(group = fct_relevel(group, c("cohort_n", "seen_n", "cover_p"))) |> 
  arrange(hbres, financial_year, group) |> 
  rename(health_board = hbres)

# change NaNs to NAs
kpi_3_1_res$value[is.nan(kpi_3_1_res$value)] <- NA ## AMc note: not sure this is needed?

# check
table(kpi_3_1_res$health_board, kpi_3_1_res$financial_year) # all hbres/FY are 3

rm(kpi_3_1_base, kpi_3_1_hb, kpi_3_1_scot) # tidy


# 4: KPI 3.2 ----
# Keep records where result_outcome is in specified list or result_outcome is
# "20" and date_surgery is populated
# result_outcome:
# 11 - 'Appropriate for Surgery: Patient declined surgery' 
# 12 - 'Appropriate for Surgery: Died before treatment' 
# 13 - 'Appropriate for Surgery: Self-discharge' 
# 14 - 'Appropriate for surgery: Patient deferred surgery' 
# 15 - 'Appropriate for Surgery: AAA repaired and survived 30 days' 
# 16 - 'Appropriate for Surgery: Died within 30 days of treatment' 
# 17 - 'Appropriate for surgery: Final outcome pending' 
# 20 - 'Other final outcome' 
# surg_method:
# 03 - Procedure abandoned

# Calculate surgery variable and flag as 1 where screen_to_surgery is less than 
# or equal to 56 days (8 weeks)
kpi_3_2_base <- aaa_extract %>% 
  filter(result_outcome %in% c("11", "12", "13", "14", "15", "16", "17") | 
           (result_outcome == "20" & surg_method == "03" & !is.na(date_surgery))) %>% 
  mutate(screen_to_surgery = time_length(date_screen %--% date_surgery, "days"), 
         surgery = case_when(screen_to_surgery <= 56 ~ 1, 
                             TRUE ~ 0),
         financial_year = droplevels(financial_year)) 

if(season == "spring") {
  # december 31 filter because of required follow-up time [8 wks] (mentioned in footnotes of excel)
  kpi_3_2_base <- kpi_3_2_base |> 
    filter(date_screen <= dmy(paste("31-12-", substr(start_date, 1, 4))))
}

# Check result_outcome for records where date_surgery is blank ## AMc note: is this needed??
kpi_3_2_base %>% filter(is.na(date_surgery)) %>% count(result_outcome)

# only records where surgery happened after related screening encounter are valid
kpi_3_2_base <- kpi_3_2_base %>% 
  filter(screen_to_surgery >= 0 | is.na(screen_to_surgery))

# This section should be applied to the spring QPMG run because vascular data for
# the year end is not complete at that stage - it should not be run when
# producing data for the complete year end from the September extract
##!! How/Where is this used??
if (season == "spring"){
  
  kpi_3_2_base <- kpi_3_2_base %>% 
    mutate(pending_surgery = 
             case_when(result_outcome == "17" & is.na(date_surgery) ~ 1, 
                       TRUE ~ 0))
  
  kpi_3_2_base %>% count(pending_surgery)
  
}


## Health Board of Residence ----
# Health Boards
kpi_3_2_hb <- kpi_3_2_base %>% 
  group_by(hbres, financial_year) %>% 
  summarise(cohort_n = n(), 
            surgery_n = sum(surgery)) |>
  ungroup() |> 
  complete(hbres, financial_year)

# Scotland
kpi_3_2_scot <- kpi_3_2_base %>% 
  group_by(financial_year) %>% 
  summarise(cohort_n = n(), 
            surgery_n = sum(surgery)) %>% 
  mutate(hbres = "Scotland", .before = financial_year) |>
  ungroup()

# Combine
kpi_3_2_res <- bind_rows(kpi_3_2_scot, kpi_3_2_hb) %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) %>% # AMc note: this is a good way to get rid of NAs!!
  # calculate coverage
  mutate(cover_p = round_half_up(surgery_n * 100 / cohort_n, 1)) |> 
  mutate(kpi = "KPI 3.2 Residence", .after = hbres) |> 
  # arrange into longer format and order vars
  pivot_longer(!hbres:financial_year, names_to = "group", values_to = "value") |> 
  mutate(group = fct_relevel(group, c("cohort_n", "surgery_n", "cover_p"))) |> 
  arrange(hbres, financial_year, group) |> 
  rename(health_board = hbres)

# check all HBs present for each FY
table(kpi_3_2_res$health_board, kpi_3_2_res$financial_year) # all hbres/FY are 3

# change NaNs to NAs
kpi_3_2_res$value[is.nan(kpi_3_2_res$value)] <- NA



## Health Board of Surgery ----
# First, remove records where HB of surgery is NA & note any non-Scot HBs
table(kpi_3_2_base$hb_surgery, useNA = "ifany")

kpi_3_2_surg_base <- kpi_3_2_base |> 
  filter(!is.na(hb_surgery)) |> 
  mutate(hb_surgery = case_when(hb_surgery == "A" ~ "Ayrshire & Arran",
                                hb_surgery == "B" ~ "Borders",
                                hb_surgery == "D" ~ "Cumbria", #**
                                hb_surgery == "F" ~ "Fife",
                                hb_surgery == "G" ~ "Greater Glasgow & Clyde",
                                hb_surgery == "H" ~ "Highland",
                                hb_surgery == "L" ~ "Lanarkshire",
                                hb_surgery == "N" ~ "Grampian",
                                hb_surgery == "R" ~ "Orkney",
                                hb_surgery == "S" ~ "Lothian",
                                hb_surgery == "T" ~ "Tayside",
                                hb_surgery == "V" ~ "Forth Valley",
                                hb_surgery == "W" ~ "Western Isles",
                                hb_surgery == "Y" ~ "Dumfries & Galloway",
                                hb_surgery == "Z" ~ "Shetland"))

table(kpi_3_2_surg_base$hb_surgery, useNA = "ifany")

## Coding Health Board of Surgery note:
# HB of surgery can sometimes be coded to a HB that doesn't actually perform them
# in these instances, PHS will recode the HB Surgery to that in which the surgery
# most likely happened - the HB responsible for surgeries for that area
# Below are the groupings for this (at Spring 2025):

## Grampian group
# Grampian, Orkney, Shetland

## Greater Glasgow & Clyde group
# Greater Glasgow & Clyde, Forth Valley, Western Isles

## Highland group
# Highland

## Lanarkshire group
# Lanarkshire, Ayrshire & Arran, Dumfries & Galloway

## Lothian group
# Lothian, Borders

## Tayside group
# Tayside, Fife


## Investigate anomalies - HB of surgery not in Scotland or not assigned

cumbria <- kpi_3_2_surg_base[kpi_3_2_surg_base$hb_surgery == "Cumbria",]
table(cumbria$date_screen, useNA = "ifany")
# all records from (date_screen) 2015-2019
rm(cumbria)

no_surg <- filter(kpi_3_2_base, is.na(hb_surgery))
table(no_surg$date_surgery, no_surg$hbres) # 1 record from FV in 2018 (follow-up??)
rm(no_surg)

# recode surgery to new hb_surgery_grp variable
kpi_3_2_surg_base <- kpi_3_2_surg_base |> 
  # remove cumbria records
  filter(!hb_surgery == "Cumbria") |> 
  # assign groups
  mutate(hb_surgery_grp = case_when(
    hb_surgery %in% c("Ayrshire & Arran", "Dumfries & Galloway", "Lanarkshire") ~ "Lanarkshire", 
    hb_surgery %in% c("Borders", "Lothian") ~ "Lothian", 
    hb_surgery %in% c("Fife", "Tayside") ~ "Tayside", 
    hb_surgery %in% c("Forth Valley", "Western Isles", "Greater Glasgow & Clyde") ~ "Greater Glasgow & Clyde", 
    hb_surgery %in% c("Orkney", "Shetland", "Grampian") ~ "Grampian", 
    hb_surgery == "Highland" ~ "Highland", 
    .default = NA))
  


## Calculating kpi
# Health Boards
kpi_3_2_hb <- kpi_3_2_surg_base %>% 
  group_by(hb_surgery_grp, financial_year) %>% 
  summarise(cohort_n = n(), 
            surgery_n = sum(surgery)) |> 
  ungroup() |> 
  complete(hb_surgery_grp, financial_year)

# Scotland
kpi_3_2_scot <- kpi_3_2_surg_base %>% 
  group_by(financial_year) %>% 
  summarise(cohort_n = n(), 
            surgery_n = sum(surgery)) %>% 
  mutate(hb_surgery_grp = "Scotland", .before = financial_year) |>
  ungroup()

# Combine
kpi_3_2_surg <- bind_rows(kpi_3_2_scot, kpi_3_2_hb) %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) %>%
  # calculate coverage percentage
  mutate(cover_p = round_half_up(surgery_n * 100 / cohort_n, 1)) |> 
  mutate(kpi = "KPI 3.2 Surgery", .after = hb_surgery_grp) |> 
  # arrange into longer format and order vars
  pivot_longer(!hb_surgery_grp:financial_year, names_to = "group", values_to = "value") |> 
  mutate(hb_surgery_grp = fct_relevel(hb_surgery_grp, c("Scotland", "Grampian", 
                                                        "Greater Glasgow & Clyde",
                                                        "Highland", "Lanarkshire",
                                                        "Lothian", "Tayside")),
         group = fct_relevel(group, c("cohort_n", "surgery_n", "cover_p"))) |> 
  arrange(hb_surgery_grp, financial_year, group) |> 
  rename(health_board = hb_surgery_grp)

# check all HBs represented in all FYs
table(kpi_3_2_surg$health_board, droplevels(kpi_3_2_surg$financial_year)) # all hbres/FY are 3

# change NaNs to NAs
kpi_3_2_res$value[is.nan(kpi_3_2_res$value)] <- NA


###
# Check differences between hbres and hb_surgery
check <- kpi_3_2_base[kpi_3_2_base$financial_year %in% c(kpi_report_years),]
check <- droplevels(check)
table(check$financial_year) # total records by FY
table(check$hbres, check$financial_year) # hbres groups
table(check$hb_surgery, check$financial_year) # hb_surgery groups
table(check$hb_surgery, useNA = "ifany") # are there any NAs?

# Look into NAs and determine result_outcome
# if outcome is one of the reasons for not having surgery listed above, that's ok
# otherwise follow-up with health board
check_2 <- check[is.na(check$hb_surgery),]
table(check_2$result_outcome, check_2$financial_year)

rm(check, check_2)
###

rm(kpi_3_2_base, kpi_3_2_hb, kpi_3_2_scot, kpi_3_2_surg_base) # tidy


# Step 5: Write outputs ----
# Combine KPI 3 subsets
kpi_3 <- bind_rows(kpi_3_1_res, kpi_3_2_res, kpi_3_2_surg)
table(kpi_3$kpi) 

## Historical file
# Create backup of last year's file
hist_db <- read_rds(paste0(hist_path,"/aaa_kpi_historical_theme4.rds"))
table(hist_db$kpi, hist_db$fin_year)

report_db <- kpi_3 |> 
  filter(financial_year %in% c(kpi_report_years)) |> 
  rename(fin_year = financial_year,
         hbres = health_board)|> 
  mutate_all(~replace(., is.nan(.), NA))

# 202409 workaround - doesn't need history building? or at least not yet...
## AMc note: have already built history for this round, may need to replace once
## we have published data/discussed this?
if(!yymm == 202409) {
  # create historical backup + new file with this year's data
  build_history(df_hist = hist_db, 
                df_new = report_db, 
                kpi_number = "3",
                season_var = season,
                fys_in_report = kpi_report_years,
                list_of_fys = fy_list,
                list_of_hbs = hb_list,
                historical_path = hist_path)
}


table(hist_db$fin_year, hist_db$kpi)
#         KPI 3.1 Residence KPI 3.2 Residence KPI 3.2 Surgery
# 2012/13                45                45               9
# 2013/14                45                45              27
# 2014/15                45                45              30
# 2015/16                45                45              30
# 2016/17                45                45              30
# 2017/18                45                45              30
# 2018/19                45                45              30
# 2019/20                45                45              27
# 2020/21                45                45              24
# 2021/22                45                45              24
# 2022/23                45                45              24

## Save report file
if(!yymm == 202409) {
  query_write_rds(report_db, paste0(temp_path, "/4_1_kpi_3_", yymm, ".rds"))
} else if(yymm == 202409) {
  query_write_rds(report_db, paste0(temp_path, "/4_1_kpi_3_", yymm, "_updated_vasc_data.rds"))
}



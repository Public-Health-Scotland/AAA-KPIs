#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# kpi_3_additional_timescales.R
# Calum Purdie & Karen Hotopp
# Adapted by Aoife McCarthy
# 29/01/2025
# 
# KPI 3.1 percentage of men with AAA >= 5.5cm seen within two weeks of screening
# additional col = seen within 4 weeks of screening (audit for Spring 2025 QPMG)
#
# KPI 3.2 percentage of men with AAA >= 5.5cm deemed appropriate for 
# intervention/operated on within 8 weeks of screening
# additional cols = sugery within 3/6/9/12 months of screening (audit for Spring 2025 QPMG)
#
# Written/run on R Studio Server, R version 3.6.1
# Revised on Posit WB, R Version 4.1.2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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
library(openxlsx)
library(stringr)
library(common)

rm(list = ls())
gc()


source(here::here("code/00_housekeeping.R"))

rm (exclusions_path, simd_path, fy_tibble, year1, year2,
    qpmg_month, cutoff_date, year1_end, year1_start, year2_end, year2_start)


template_path <- paste0("/PHI_conf/AAA/Topics/Screening/templates")

# 2: Data Manipulation ----
# Keep records where date_referral_true is populated and largest measure is 
# greater than or equal to 5.5, where result_outcome is not "02" and 
# date_screen is less than or equal to cut_off_date
aaa_extract <- read_rds(extract_path) %>% 
  filter(!is.na(date_referral_true) & largest_measure >= 5.5, 
         result_outcome != "02", 
         date_screen <= cut_off_date)


# 3: KPI 3.1 ----

## AMc note: additional column is "seen within 4 weeks of screening" - requested at Oct 2024 QPMG
# will be management information for now, and will be printed into a separate workbook of audit materials
# after Spring 2025 QPMG we should know whether this is being kept or not

# Calculate time between date seen in outpatients to date of screening
kpi_3_1_base <- aaa_extract %>% 
  mutate(screen_to_seen = time_length(date_screen %--% date_seen_outpatient, 
                                      "days"), 
         seen_2wks = case_when(screen_to_seen <= 14 ~ 1, 
                               TRUE ~ 0),
         #  new var - seen within 4 weeks - audit for Spring KPIs 2025 - management information
         seen_4wks = case_when(screen_to_seen <= 28 ~ 1,
                               .default = 0))

# KPI 3.1 provisional footnote - people referred but not yet seen
# This section should be applied to the spring QPMG run because vascular data for
# the year end is not complete at that stage - it should not be run when
# producing data for the complete year end from the September extract
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
  filter(result_outcome %in% c("01", "03", "04", "05") | 
           !is.na(date_seen_outpatient) & screen_to_seen >= 0) |> 
  mutate(financial_year = droplevels(financial_year))


## Health Board of Residence ----
# Health Boards
kpi_3_1_hb <- kpi_3_1_base %>% 
  group_by(hbres, financial_year) %>% 
  summarise(cohort_n = n(), 
            seen_2wks_n = sum(seen_2wks),
            seen_4wks_n = sum(seen_4wks)) |> 
  ungroup() |> 
  complete(hbres, financial_year) |> # completing table - all HBs for each FY
  mutate_at(vars(cohort_n:seen_4wks_n), ~ifelse(is.na(.), 0, .))

# Scotland
kpi_3_1_scot <- kpi_3_1_base %>% 
  group_by(financial_year) %>% 
  summarise(cohort_n = n(), 
            seen_2wks_n = sum(seen_2wks),
            seen_4wks_n = sum(seen_4wks)) %>% 
  mutate(hbres = "Scotland", .before = financial_year) |>
  ungroup()

# Combine
kpi_3_1 <- bind_rows(kpi_3_1_scot, kpi_3_1_hb) %>% 
  mutate(cover_2wks_p = round_half_up(seen_2wks_n * 100 / cohort_n, 1),
         cover_4wks_p = round_half_up(seen_4wks_n * 100 / cohort_n, 1)) |> 
  mutate(kpi = "KPI 3.1 Residence", .after = hbres) |>
  # arrange into longer format and order vars
  pivot_longer(!hbres:financial_year, names_to = "group", values_to = "value") |> 
  mutate(hbres = fct_relevel(hbres, hb_list),
         group = fct_relevel(group, c("cohort_n", "seen_2wks_n", "cover_2wks_p", "seen_4wks_n", "cover_4wks_p"))) |> 
  arrange(hbres, financial_year, group) |> 
  rename(health_board = hbres)

# change NaNs to NAs
kpi_3_1$value[is.nan(kpi_3_1$value)] <- NA

table(kpi_3_1$health_board, kpi_3_1$financial_year) # all hbres/FY are 3 (5 now since added temp 4wks var)

# Tidy environment
rm(kpi_3_1_base, kpi_3_1_hb, kpi_3_1_scot)

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

## AMc note: additional columns are "surgery within 3m/6m/9m/1y of screening" - requested at Oct 2024 QPMG
# will be management information for now, and will be printed into a separate workbook of audit materials
# after Spring 2025 QPMG we should know whether this is being kept or not

# Calculate surgery variable and flag as 1 where screen_to_surgery is less than 
# or equal to 56 days (8 weeks)
kpi_3_2_base <- aaa_extract %>% 
  filter(result_outcome %in% c("11", "12", "13", "14", "15", "16", "17") | 
           (result_outcome == "20" & surg_method == "03" & !is.na(date_surgery))) %>% 
  mutate(screen_to_surgery = time_length(date_screen %--% date_surgery, "days"), 
         surgery_8wks = case_when(screen_to_surgery <= 56 ~ 1, 
                                  TRUE ~ 0),
         surgery_3m  = case_when(screen_to_surgery <= 90 ~ 1, 
                                  TRUE ~ 0),
         surgery_6m = case_when(screen_to_surgery <= 180 ~ 1, 
                                  TRUE ~ 0),
         surgery_9m = case_when(screen_to_surgery <= 270 ~ 1, 
                                  TRUE ~ 0),
         surgery_1yr = case_when(screen_to_surgery <= 365 ~ 1, 
                                 TRUE ~ 0),
         financial_year = droplevels(financial_year))


if(season == "spring") {
  # december 31 filter because of required follow-up time [8 wks] (mentioned in footnotes of excel)
  kpi_3_2_base <- kpi_3_2_base |> 
    filter(date_screen <= dmy(paste("31-12-", substr(start_date, 1, 4))))
}

# Check variables
kpi_3_2_base %>% count(screen_to_surgery)
kpi_3_2_base %>% count(surgery_8wks)

# Check result_outcome for records where date_surgery is blank
kpi_3_2_base %>% filter(is.na(date_surgery)) %>% count(result_outcome)

kpi_3_2_base <- kpi_3_2_base %>% 
  filter(screen_to_surgery >= 0 | is.na(screen_to_surgery))

# This section should be applied to the spring QPMG run because vascular data for
# the year end is not complete at that stage - it should not be run when
# producing data for the complete year end from the September extract
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
            surgery_8wks_n = sum(surgery_8wks),
            surgery_3m_n = sum(surgery_3m),
            surgery_6m_n = sum(surgery_6m),
            surgery_9m_n = sum(surgery_9m),
            surgery_1yr_n = sum(surgery_1yr)) |>
  ungroup() |> 
  complete(hbres, financial_year)

# Scotland
kpi_3_2_scot <- kpi_3_2_base %>% 
  group_by(financial_year) %>% 
  summarise(cohort_n = n(), 
            surgery_8wks_n = sum(surgery_8wks),
            surgery_3m_n = sum(surgery_3m),
            surgery_6m_n = sum(surgery_6m),
            surgery_9m_n = sum(surgery_9m),
            surgery_1yr_n = sum(surgery_1yr)) %>% 
  mutate(hbres = "Scotland", .before = financial_year) |>
  ungroup()

# Combine
kpi_3_2_res <- bind_rows(kpi_3_2_scot, kpi_3_2_hb) %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) %>% # AMc note: this is a good way to get rid of NAs!!
  # calculate coverage for each timescale
  mutate(cover_8wks_p = round_half_up(surgery_8wks_n * 100 / cohort_n, 1),
         cover_3m_p = round_half_up(surgery_3m_n * 100 / cohort_n, 1),
         cover_6m_p = round_half_up(surgery_6m_n * 100 / cohort_n, 1),
         cover_9m_p = round_half_up(surgery_9m_n * 100 / cohort_n, 1),
         cover_1yr_p = round_half_up(surgery_1yr_n * 100 / cohort_n, 1)) |> 
  mutate(kpi = "KPI 3.2 Residence", .after = hbres) |>
  # arrange into longer format and order vars
  pivot_longer(!hbres:financial_year, names_to = "group", values_to = "value") |> 
  mutate(group = forcats::fct_relevel(group, c("cohort_n", 
                                               "surgery_8wks_n", "cover_8wks_p",
                                               "surgery_3m_n", "cover_3m_p",
                                               "surgery_6m_n", "cover_6m_p",
                                               "surgery_9m_n", "cover_9m_p",
                                               "surgery_1yr_n", "cover_1yr_p"))) |> 
  arrange(hbres, financial_year, group) |> 
  rename(health_board = hbres)

table(kpi_3_2_res$health_board, kpi_3_2_res$financial_year) # all hbres/FY are 3 (should be 11 with new audit vars)

# change NaNs to NAs
kpi_3_2_res$value[is.nan(kpi_3_2_res$value)] <- NA

rm(kpi_3_2_hb, kpi_3_2_scot) # tidy


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
                                hb_surgery == "R" ~ "Orkney", # Needed?
                                hb_surgery == "S" ~ "Lothian",
                                hb_surgery == "T" ~ "Tayside",
                                hb_surgery == "V" ~ "Forth Valley",
                                hb_surgery == "W" ~ "Western Isles", # Needed?
                                hb_surgery == "Y" ~ "Dumfries & Galloway",
                                hb_surgery == "Z" ~ "Shetland")) # Needed?

table(kpi_3_2_surg_base$hb_surgery, useNA = "ifany")

## Investigate HBs where there should not be any surgeries 
##
cumbria <- kpi_3_2_surg_base[kpi_3_2_surg_base$hb_surgery == "Cumbria",]
table(cumbria$date_screen, useNA = "ifany")
# all records from (date_screen) 2015-2019
rm(cumbria)
##

##
borders <- kpi_3_2_surg_base[kpi_3_2_surg_base$hb_surgery == "Borders",]
table(borders$date_screen, useNA = "ifany")
# modern records; re-code as Lothian
rm(borders)
##

##
dag <- kpi_3_2_surg_base[kpi_3_2_surg_base$hb_surgery == "Dumfries & Galloway",]
table(dag$date_screen, useNA = "ifany")
# old record; ignore
rm(dag)
##

##
fv <- kpi_3_2_surg_base[kpi_3_2_surg_base$hb_surgery == "Forth Valley",]
table(fv$date_screen, useNA = "ifany")
# most records from 2018 or before; re-code new dates as GGC
rm(fv)
##

##
no_surg <- filter(kpi_3_2_base, is.na(hb_surgery))
table(no_surg$date_surgery, no_surg$hbres) # 1 record from FV in 2018 (follow-up??)
rm(no_surg)
##

# create new hb_surgery_grp var which recodes non-surgery HBs as their surgery-performing counterpart
kpi_3_2_surg_base <- kpi_3_2_surg_base |> 
  mutate(hb_surgery_grp = case_when(
    hb_surgery %in% c("Ayrshire & Arran", "Dumfries & Galloway", "Lanarkshire") ~ "Lanarkshire", 
    hb_surgery %in% c("Borders", "Lothian") ~ "Lothian", 
    hb_surgery %in% c("Fife", "Tayside") ~ "Tayside", 
    hb_surgery %in% c("Forth Valley", "Western Isles", "Greater Glasgow & Clyde") ~ "Greater Glasgow & Clyde", 
    hb_surgery %in% c("Orkney", "Shetland", "Grampian") ~ "Grampian", 
    hb_surgery == "Highland" ~ "Highland", 
    .default = NA)) 

# Health Boards
kpi_3_2_hb <- kpi_3_2_surg_base %>% 
  filter(!hb_surgery == "Cumbria") |>
  group_by(hb_surgery_grp, financial_year) %>% 
  summarise(cohort_n = n(), 
            surgery_8wks_n = sum(surgery_8wks),
            surgery_3m_n = sum(surgery_3m),
            surgery_6m_n = sum(surgery_6m),
            surgery_9m_n = sum(surgery_9m),
            surgery_1yr_n = sum(surgery_1yr)) |> 
  ungroup() |> 
  complete(hb_surgery_grp, financial_year)

# Scotland
kpi_3_2_scot <- kpi_3_2_surg_base %>% 
  filter(!hb_surgery == "Cumbria") |> 
  group_by(financial_year) %>% 
  summarise(cohort_n = n(), 
            surgery_8wks_n = sum(surgery_8wks),
            surgery_3m_n = sum(surgery_3m),
            surgery_6m_n = sum(surgery_6m),
            surgery_9m_n = sum(surgery_9m),
            surgery_1yr_n = sum(surgery_1yr)) %>% 
  mutate(hb_surgery_grp = "Scotland", .before = financial_year) |>
  ungroup()

# Combine
kpi_3_2_surg <- bind_rows(kpi_3_2_scot, kpi_3_2_hb) %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) %>% 
  # calculate coverage for each timescale
  mutate(cover_8wks_p = round_half_up(surgery_8wks_n * 100 / cohort_n, 1),
         cover_3m_p = round_half_up(surgery_3m_n * 100 / cohort_n, 1),
         cover_6m_p = round_half_up(surgery_6m_n * 100 / cohort_n, 1),
         cover_9m_p = round_half_up(surgery_9m_n * 100 / cohort_n, 1),
         cover_1yr_p = round_half_up(surgery_1yr_n * 100 / cohort_n, 1)) |> 
  mutate(kpi = "KPI 3.2 Surgery", .after = hb_surgery_grp) |>
  # arrange into longer format and order vars
  pivot_longer(!hb_surgery_grp:financial_year, names_to = "group", values_to = "value") |> 
  mutate(group = forcats::fct_relevel(group, c("cohort_n", 
                                               "surgery_8wks_n", "cover_8wks_p",
                                               "surgery_3m_n", "cover_3m_p",
                                               "surgery_6m_n", "cover_6m_p",
                                               "surgery_9m_n", "cover_9m_p",
                                               "surgery_1yr_n", "cover_1yr_p")),
         hb_surgery_grp = forcats::fct_relevel(hb_surgery_grp, 
                                               c("Scotland", "Grampian", 
                                                 "Greater Glasgow & Clyde", 
                                                 "Highland", "Lanarkshire",
                                                 "Lothian", "Tayside"))) |> 
  arrange(hb_surgery_grp, financial_year, group) |> 
  rename(health_board = hb_surgery_grp)


table(kpi_3_2_surg$health_board, droplevels(kpi_3_2_surg$financial_year)) # all hbres/FY are 3 (11 with new audit vars)


###
# Check differences between hbres and hb_surgery
check <- kpi_3_2_base[kpi_3_2_base$financial_year %in% c(kpi_report_years),]
check <- droplevels(check)
table(check$financial_year) # total records by FY
table(check$hbres, check$financial_year) # hbres groups
table(check$hb_surgery, check$financial_year) # hb_surgery groups
table(check$hb_surgery, useNA = "ifany") # are there any NAs?

# Look into NAs and determine result_outcome
check_2 <- check[is.na(check$hb_surgery),]
table(check_2$result_outcome, check_2$financial_year)

rm(check, check_2)
###


# Tidy environment
rm(kpi_3_2_hb, kpi_3_2_scot)


# 5: Save outputs ---------------------------------------------------------

kpi_3_new_timescales <- bind_rows(kpi_3_1, kpi_3_2_res, kpi_3_2_surg)

query_write_rds(kpi_3_new_timescales, paste0(temp_path, "/4_1_kpi_3_", yymm, "_new_timescales.rds"))

rm(aaa_extract, kpi_3_1, kpi_3_2_base, kpi_3_2_res, kpi_3_2_surg, kpi_3_2_surg_base) # tidy

# 6: Prep Excel data ------------------------------------------------------

# function to combine fy and group cols and pivot wider
## AMc note: this could go into a package for sure
pivot_fy_grp_wide <- function(data, fy_col, grp_col, value_col) {
  data |> 
    unite("fy_group", {{ fy_col }}, {{ grp_col }}, sep= "_") |> 
    pivot_wider(names_from = fy_group, values_from = {{ value_col }})
}

# 3.1
excel_3_1 <- kpi_3_new_timescales |> 
  filter(kpi == "KPI 3.1 Residence",
         financial_year %in% kpi_report_years) |> 
  select(-kpi) |> 
  pivot_fy_grp_wide(financial_year, group, value)

# 3.2 Res
excel_3_2_res_top <- kpi_3_new_timescales |> 
  filter(kpi == "KPI 3.2 Residence",
         financial_year %in% kpi_report_years[1:2]) |> 
  select(-kpi) |> 
  pivot_fy_grp_wide(financial_year, group, value)

excel_3_2_res_bottom <- kpi_3_new_timescales |> 
  filter(kpi == "KPI 3.2 Residence",
         financial_year %in% kpi_report_years[3]) |> 
  select(-kpi) |> 
  pivot_fy_grp_wide(financial_year, group, value)

# 3.2 Surg  
excel_3_2_surg_top <- kpi_3_new_timescales |> 
  filter(kpi == "KPI 3.2 Surgery",
         financial_year %in% kpi_report_years[1:2]) |> 
  select(-kpi) |> 
  pivot_fy_grp_wide(financial_year, group, value)

excel_3_2_surg_bottom <- kpi_3_new_timescales |> 
  filter(kpi == "KPI 3.2 Surgery",
         financial_year %in% kpi_report_years[3]) |> 
  select(-kpi) |> 
  pivot_fy_grp_wide(financial_year, group, value)


# 7: Write to Excel -------------------------------------------------------

## housekeeping ----

# Define reporting years
year_xx <- year(cut_off_date)
year_ww <- year_xx - 1
year_vv <- year_xx - 2
year_yy <- year_xx + 1
year_uu <- year_xx - 3
year_ss <- year_xx - 5

# styles
source(here::here("code", "src", "Source_Excel_Styles.R"))

## notes etc ----

### KPI 3.1
screened_year_vvr <- paste0("Screened in year ending 31 March ", year_vv, {supsc('r')})
screened_year_wwr <- paste0("Screened in year ending 31 March ", year_ww, {supsc('r')})
screened_year_xx <- eval_seasonal_diff(
  season,
  {paste0("Screened in year ending 31 March ", year_xx, {supsc('p')},
          '\n', "(Data not complete)")}, # spring
  {paste0("Screened in year ending 31 March ", year_xx)} # autumn
)

# provisional note import and creating
if(season == "spring") { # only possible to create in spring because the data only gets created in spring
  
  p_note_3.1_data <- read_rds(paste0(temp_path, "/4_provisional_note_3.1.rds")) |> 
    pivot_wider(names_from = pending_appt, values_from = n) |> 
    mutate(`Referral date and outpatient date` = `Total referrals` - `Referral date, no outpatient date`)
  
  kpi_3.1_prov <- paste0("p  Provisonal. Data are for the 11-month period 1 April ",
                         year_ww, " to ", extract_date, " ", year_xx,
                         " and do not include data for referrrals with no outpatient ",
                         "date recorded. In the 11-month period, there were a total ",
                         "of ", p_note_3.1_data |> pull(`Total referrals`), 
                         " vascular referrals. At ", extract_date, " ", year_xx, 
                         " (date of extract), ", p_note_3.1_data |> pull(`Referral date and outpatient date`), 
                         " of these referrals had an outpatient date recorded and ",
                         p_note_3.1_data |> pull(`Referral date, no outpatient date`), 
                         " had no outpatient date recorded.")
}

# revised note
kpi_3.1_revised <- paste0("r  Revised since published on {publication date} ",
                          year_xx, ", due to updates in the data recorded on the ",
                          "Scottish AAA Call Recall System. The Scotland rate for ",
                          "the year ending 31 March ", year_ww, " was previously ",
                          "{x, taken from previous autumn report}%. Figures for all ",
                          "time periods in this table are based on data recorded on ",
                          "the Scottish AAA Call Recall System at ", extract_date,
                          " ", year_xx, " (date of data extraction).")

### KPI 3.2 Res and Surg

kpi_3.2_revised <- paste0("r  Revised since published on {publication date year_xx} ",
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

kpi_3.2_prov <- paste0("p  Provisional. Data are for men screened from 1 April ",
                       year_ww, " to 31 December ", year_ww, " only to allow ",
                       "for the 8 week target timescale plus a data recording ",
                       "lag. A number of men screened in this period have a ",
                       "non-final outcome or no outcome recorded; therefore, ",
                       "the figures will change when the data becomes more ",
                       "complete. The denominator includes men with a non-final ",
                       "outcome of 'Appropriate for surgery: final outcome pending' ",
                       "with no surgery date recorded.")

## writing data and styles ----

# wb <- loadWorkbook(paste0(template_path, "/kpi_3_audit_new_timescales_",
#                           season, ".xlsx"))
# temp version
wb <- loadWorkbook(paste0(template_path, "/kpi_3_audit_new_timescales_spring.xlsx"))


### KPI 3.1
# notes
writeData(wb, sheet = "KPI 3.1", screened_year_vvr, 
          startRow = 4, startCol = 2)
writeData(wb, sheet = "KPI 3.1", screened_year_wwr, 
          startRow = 4, startCol = 7)
writeData(wb, sheet = "KPI 3.1", screened_year_xx, 
          startRow = 4, startCol = 12)
addStyle(wb, sheet = "KPI 3.1", styles$black_border_centre_12,
         rows = 4, cols = 2:16, gridExpand = T)
writeData(wb, sheet = "KPI 3.1", kpi_3.1_revised, 
          startRow = 32)
addStyle(wb, sheet = "KPI 3.1", style = styles$orange_11, 
         rows = 32, cols = 1)
if (season == "spring") {
  writeData(wb, sheet = "KPI 3.1", kpi_3.1_prov, 
            startRow = 34)
  addStyle(wb, sheet = "KPI 3.1", style = styles$black_11, 
           rows = 34, cols = 1)
}
# data
writeData(wb, sheet = "KPI 3.1", excel_3_1, 
          startRow = 7, colNames = FALSE)
showGridLines(wb, "KPI 3.1", showGridLines = FALSE)

### KPI 3.2 Res
# notes
writeData(wb, sheet = "KPI 3.2 HB Residence", screened_year_vvr, 
          startRow = 4, startCol = 2)
writeData(wb, sheet = "KPI 3.2 HB Residence", screened_year_wwr, 
          startRow = 4, startCol = 13)
addStyle(wb, sheet = "KPI 3.2 HB Residence", styles$black_border_centre_12,
         rows = 4, cols = 2:23, gridExpand = T)
writeData(wb, sheet = "KPI 3.2 HB Residence", screened_year_xx, 
          startRow = 25, startCol = 2)
addStyle(wb, sheet = "KPI 3.2 HB Residence", styles$black_border_centre_12,
         rows = 25, cols = 2:12, gridExpand = T)
writeData(wb, sheet = "KPI 3.2 HB Residence", kpi_3.2_revised, 
          startRow = 55)
addStyle(wb, sheet = "KPI 3.2 HB Residence", styles$orange_11, 
         rows = 55, cols = 1)
if (season == "spring") {
  writeData(wb, sheet = "KPI 3.2 HB Residence", kpi_3.2_prov, 
            startRow = 57)
  addStyle(wb, sheet = "KPI 3.2 HB Residence", styles$orange_11, 
           rows = 57, cols = 1)
}
# data
writeData(wb, sheet = "KPI 3.2 HB Residence", excel_3_2_res_top, 
          startRow = 7, colNames = F)
writeData(wb, sheet = "KPI 3.2 HB Residence", excel_3_2_res_bottom, 
          startRow = 28, colNames = F)
showGridLines(wb, "KPI 3.2 HB Residence", showGridLines = FALSE)


### KPI 3.2 Surg
# notes
writeData(wb, sheet = "KPI 3.2 HB Surgery", screened_year_vvr, 
          startRow = 4, startCol = 2)
writeData(wb, sheet = "KPI 3.2 HB Surgery", screened_year_wwr, 
          startRow = 4, startCol = 13)
addStyle(wb, sheet = "KPI 3.2 HB Surgery", styles$black_border_centre_12,
         rows = 4, cols = 2:23, gridExpand = T)
writeData(wb, sheet = "KPI 3.2 HB Surgery", screened_year_xx, 
          startRow = 17, startCol = 2)
addStyle(wb, sheet = "KPI 3.2 HB Surgery", styles$black_border_centre_12,
         rows = 17, cols = 2:12, gridExpand = T)
writeData(wb, sheet = "KPI 3.2 HB Surgery", kpi_3.2_revised, 
          startRow = 39)
addStyle(wb, sheet = "KPI 3.2 HB Surgery", styles$orange_11, 
         rows = 39, cols = 1)
if (season == "spring") {
  writeData(wb, sheet = "KPI 3.2 HB Surgery", kpi_3.2_prov, 
            startRow = 41)
  addStyle(wb, sheet = "KPI 3.2 HB Surgery", styles$orange_11, 
           rows = 41, cols = 1)
}
# data
writeData(wb, sheet = "KPI 3.2 HB Surgery", excel_3_2_surg_top, 
          startRow = 7, colNames = F)
writeData(wb, sheet = "KPI 3.2 HB Surgery", excel_3_2_surg_bottom, 
          startRow = 20, colNames = F)
showGridLines(wb, "KPI 3.2 HB Surgery", showGridLines = FALSE)


## save output ----

query_saveWorkbook(wb, paste0(output_path, "/audit_kpi3_extended_timescales.xlsx"))


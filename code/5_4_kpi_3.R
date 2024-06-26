###############################################################################
# 5_4_kpi_3.R
# Calum Purdie & Karen Hotopp
# 17/01/2023
# 
# KPI 3.1 percentage of men with AAA >= 5.5cm seen within two weeks of screening
# KPI 3.2 percentage of men with AAA >= 5.5cm deemed appropriate for 
# intervention/operated on within 8 weeks of screening
#
# Written/run on R Studio Server, R version 3.6.1
# Revised on Posit WB, R Version 4.1.2
###############################################################################

## Notes: 
## For KPI 3.2, numbers from published data may have minor revisions due 
# to updates of the data recorded on the Scottish AAA Call-Recall System. 
# Therefore, all KPI data is recalculated for each QPMG.

## From Sept 2023, KPI 3.2 now includes analysis by HB of surgery (in addition 
# to HB of residence) as management information.


### 1: Housekeeping ----
# Load packages
library(dplyr)
library(readr)
library(lubridate)
library(janitor)
library(tidylog)
library(svDialogs)


rm(list = ls())
gc()


source(here::here("code/0_housekeeping.R"))

rm (exclusions_path, output_path, simd_path, fy_list, hb_list, fy_tibble, 
    qpmg_month, cutoff_date, year1_end, year1_start, year2_end, year2_start, 
    year1, year2, extract_date)


#### 2: Data Manipulation ----
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


# This section should be applied to the spring QPMG run because vascular data for
# the year end is not complete at that stage - it should not be run when
# producing data for the complete year end from the September extract
# these numbers get used in the provisional note of kpi 3.1 in theme 4 excel
if (season == "spring"){
  
  provisional <- kpi_3_1 %>% 
    filter(financial_year == kpi_report_years[3]) %>% 
    mutate(pending_appt = 
             case_when(!is.na(date_referral_true) & is.na(date_seen_outpatient) ~ 1, 
                       TRUE ~ 0))
  
  pending <- provisional %>% count(pending_appt)
  print(pending)
  
  rm(pending, provisional)
  
}
# for the provisional footnote, the total = sum of pending_appt %in% c(0, 1), 
# then separated out into "seen" and "not seen" by these flags


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
  mutate(hbres = "Scotland", .before = financial_year) |>
  ungroup()

# Combine
kpi_3_1_res <- bind_rows(kpi_3_1_scot, kpi_3_1_hb) %>% 
  group_by(hbres) %>% 
  mutate(#cum_referrals = sum(cohort), 
         #cum_seen = sum(seen), 
         #pc_cum_seen = round_half_up(cum_seen * 100 / cum_referrals, 1),  
         cover_p = round_half_up(seen_n * 100 / cohort_n, 1)) |> 
  mutate(kpi = "KPI 3.1 Residence", .after = hbres) |>
  ungroup()

#### This next chunk of code adds in FY where missing (produces NAs), but is 
## it better to store without the extra (NA) data produced, as the missing FYs 
## are automatically created when the data is pivoted to match Excel output?
## This method creates larger files to be stored each year.
# Ensure every HB is represented every financial year
kpi_3_1_res <- kpi_3_1_res |>
  pivot_longer(!hbres:financial_year, names_to = "group", values_to = "value") |> 
  mutate(financial_year_group = paste(financial_year, group, sep = "_")) |> 
  select(hbres, kpi, financial_year_group, value) |> 
  pivot_wider(names_from = financial_year_group, values_from = value)

kpi_3_1_res <- hb_tibble |> left_join(kpi_3_1_res, by = "hbres") 


kpi_3_1_res <- kpi_3_1_res |> 
  pivot_longer(!hbres:kpi, names_to = "group", values_to = "value") |> 
  mutate(financial_year = group, .after = kpi) |> 
  mutate(financial_year = stringr::str_remove(financial_year, "_cohort_n"),
         financial_year = stringr::str_remove(financial_year, "_seen_n"),
         financial_year = stringr::str_remove(financial_year, "_cover_p"),
         group = case_when(stringr::str_detect(group, "cohort") ~ "cohort_n",
                           stringr::str_detect(group, "seen") ~ "seen_n",
                           stringr::str_detect(group, "cover") ~ "cover_p")) |> 
  rename(health_board = hbres)

table(kpi_3_1_res$health_board, kpi_3_1_res$financial_year) # all hbres/FY are 3
# Current run IS saved with this transformation, but to decide how to best store
#### 

## Run the below code if it is decided that above code (creates HB records w NAs) 
## should NOT used (this uses less data storage than above)
## But will need to relevel hbres factors to put Scotland on top.
# kpi_3_1_res <- kpi_3_1_res |>
#   pivot_longer(!hbres:financial_year, names_to = "group", values_to = "value") |>
#   rename(health_board = hbres)
# 
# table(kpi_3_1_res$hbres, droplevels(kpi_3_1_res$financial_year)) # NOT all hbres/FY are 3

kpi_3_1_res <- kpi_3_1_res |> 
  # remove NAs for numerical counts and replace w 0
  # not sure if this needs to be done?
  mutate(value = case_when((group == "cohort_n" | group == "seen_n") & 
                             is.na(value) ~ 0, TRUE ~ value))

# Tidy environment
rm(kpi_3_1, kpi_3_1_hb, kpi_3_1_scot)


#### 4: KPI 3.2 ----
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
kpi_3_2 <- aaa_extract %>% 
  filter(result_outcome %in% c("11", "12", "13", "14", "15", "16", "17") | 
           (result_outcome == "20" & surg_method == "03" & !is.na(date_surgery))) %>% 
  # AMC new: december 31 filter because of follow-up time (mentioned in footnotes of excel)
  filter(date_screen <= dmy(paste("31-12-", substr(start_date, 1, 4)))) %>% 
  mutate(screen_to_surgery = time_length(date_screen %--% date_surgery, 
                                         "days"), 
         surgery = case_when(screen_to_surgery <= 56 ~ 1, 
                             TRUE ~ 0),
         screen_to_surgery_group = 
           case_when(screen_to_surgery >= 0 & screen_to_surgery <= 14 ~ 1, # 2 weeks
                     screen_to_surgery >= 15 & screen_to_surgery <= 56 ~ 2, # 8 weeks
                     screen_to_surgery >= 57 & screen_to_surgery <= 112 ~ 3, # 16 weeks
                     screen_to_surgery >= 113 & screen_to_surgery <= 224 ~ 4, # 32 weeks
                     screen_to_surgery >= 225 & screen_to_surgery <= 365 ~ 5, # 1 year
                     screen_to_surgery >= 366 ~ 6)) # >1 year

# Check variables
# Do these add value? Do we need them?
kpi_3_2 %>% count(screen_to_surgery)
kpi_3_2 %>% count(surgery)
kpi_3_2 %>% count(screen_to_surgery_group)

# Check result_outcome for records where date_surgery is blank
kpi_3_2 %>% filter(is.na(date_surgery)) %>% count(result_outcome)

kpi_3_2 <- kpi_3_2 %>% 
  filter(screen_to_surgery >= 0 | is.na(screen_to_surgery))

# This section should be applied to the spring QPMG run because vascular data for
# the year end is not complete at that stage - it should not be run when
# producing data for the complete year end from the September extract
##!! How/Where is this used??
if (season == "spring"){
  
  kpi_3_2 <- kpi_3_2 %>% 
    mutate(pending_surgery = 
             case_when(result_outcome == "17" & is.na(date_surgery) ~ 1, 
                       TRUE ~ 0))
  
  kpi_3_2 %>% count(pending_surgery)
  
}


### Health Board of Residence ----
# Health Boards
kpi_3_2_hb <- kpi_3_2 %>% 
  group_by(hbres, financial_year) %>% 
  summarise(cohort_n = n(), 
            surgery_n = sum(surgery)) |>
  ungroup()

# Scotland
kpi_3_2_scot <- kpi_3_2 %>% 
  group_by(financial_year) %>% 
  summarise(cohort_n = n(), 
            surgery_n = sum(surgery)) %>% 
  mutate(hbres = "Scotland", .before = financial_year) |>
  ungroup()

# Combine
kpi_3_2_res <- bind_rows(kpi_3_2_scot, kpi_3_2_hb) %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) %>% 
  group_by(hbres) %>% 
  mutate(#cum_approp = sum(cohort), 
         #cum_surgery = sum(surgery),
         #pc_cum_surgery = round_half_up(cum_surgery * 100 / cum_approp, 1), 
         cover_p = round_half_up(surgery_n * 100 / cohort_n, 1)) |> 
  mutate(kpi = "KPI 3.2 Residence", .after = hbres) |>
  ungroup()

#### This next chunk of code adds in FY where missing (produces NAs), but is 
## it better to store without the extra (NA) data produced, as the missing FYs 
## are automatically created when the data is pivoted to match Excel output?
## This method creates larger files to be stored each year.
# Ensure every HB is represented every financial year
kpi_3_2_res <- kpi_3_2_res |>
  pivot_longer(!hbres:financial_year, names_to = "group", values_to = "value") |> 
  mutate(financial_year_group = paste(financial_year, group, sep = "_")) |> 
  select(hbres, kpi, financial_year_group, value) |> 
  pivot_wider(names_from = financial_year_group, values_from = value)

kpi_3_2_res <- hb_tibble |> left_join(kpi_3_2_res, by = "hbres") 


kpi_3_2_res <- kpi_3_2_res |> 
  pivot_longer(!hbres:kpi, names_to = "group", values_to = "value") |> 
  mutate(financial_year = group, .after = kpi) |> 
  mutate(financial_year = stringr::str_remove(financial_year, "_cohort_n"),
         financial_year = stringr::str_remove(financial_year, "_surgery_n"),
         financial_year = stringr::str_remove(financial_year, "_cover_p"),
         group = case_when(stringr::str_detect(group, "cohort") ~ "cohort_n",
                           stringr::str_detect(group, "surgery") ~ "surgery_n",
                           stringr::str_detect(group, "cover") ~ "cover_p")) |> 
  rename(health_board = hbres)

table(kpi_3_2_res$health_board, kpi_3_2_res$financial_year) # all hbres/FY are 3
# Current run IS saved with this transformation, but to decide how to best store
#### 

# ## Run the below code if it is decided that above code (creates HB records w NAs) 
# ## should NOT used (this uses less data storage than above)
# ## But will need to relevel hbres factors to put Scotland on top.
# kpi_3_2_res <- kpi_3_2_res |>
#   pivot_longer(!hbres:financial_year, names_to = "group", values_to = "value") |>
#   rename(health_board = hbres)
# 
# table(kpi_3_2_res$hbres, droplevels(kpi_3_2_res$financial_year)) # NOT all hbres/FY are 3

kpi_3_2_res <- kpi_3_2_res |> 
  # remove NAs for numerical counts and replace w 0
  # leaves _p var as NA, which helps when creating Excel wbs
  mutate(value = case_when((group == "cohort_n" | group == "surgery_n") & 
                             is.na(value) ~ 0, TRUE ~ value))


### Health Board of Surgery ----
# First, remove records where HB of surgery is NA & note any non-Scot HBs
table(kpi_3_2$hb_surgery, useNA = "ifany")

kpi_3_2_surg <- kpi_3_2 |> 
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

table(kpi_3_2_surg$hb_surgery, useNA = "ifany")

##
cumbria <- kpi_3_2_surg[kpi_3_2_surg$hb_surgery == "Cumbria",]
table(cumbria$date_screen, useNA = "ifany")
# all records from (date_screen) 2015-2019
rm(cumbria)
##

##
no_surg <- filter(kpi_3_2, is.na(hb_surgery))
table(no_surg$date_surgery, no_surg$hbres) # 1 record from FV in 2018 (follow-up??)
rm(no_surg)
##

# Health Boards
kpi_3_2_hb <- kpi_3_2_surg %>% 
  group_by(hb_surgery, financial_year) %>% 
  summarise(cohort_n = n(), 
            surgery_n = sum(surgery)) |> 
  ungroup()

# Scotland
kpi_3_2_scot <- kpi_3_2_surg %>% # Should these exclude the Cumbria records??
  group_by(financial_year) %>% 
  summarise(cohort_n = n(), 
            surgery_n = sum(surgery)) %>% 
  mutate(hb_surgery = "Scotland", .before = financial_year) |>
  ungroup()

# Combine
kpi_3_2_surg <- bind_rows(kpi_3_2_scot, kpi_3_2_hb) %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) %>% 
  group_by(hb_surgery) %>% 
  mutate(#cum_referrals = sum(cohort), 
    #cum_seen = sum(seen), 
    #pc_cum_seen = round_half_up(cum_seen * 100 / cum_referrals, 1),  
    cover_p = round_half_up(surgery_n * 100 / cohort_n, 1)) |> 
  mutate(kpi = "KPI 3.2 Surgery", .after = hb_surgery) |>
  ungroup()

# #### This next chunk of code adds in FY where missing (produces NAs), but is 
# ## it better to store without the extra (NA) data produced, as the missing FYs 
# ## are automatically created when the data is pivoted to match Excel output?
# ## Doesn't really work w hb_surgery...
# # Ensure every HB is represented every financial year
# kpi_3_2_surg <- kpi_3_2_surg |>
#   pivot_longer(!hb_surgery:financial_year, names_to = "group", values_to = "value") |> 
#   mutate(financial_year_group = paste(financial_year, group, sep = "_")) |> 
#   select(hb_surgery, kpi, financial_year_group, value) |> 
#   pivot_wider(names_from = financial_year_group, values_from = value)
# 
# kpi_3_2_surg <- hb_list |> left_join(kpi_3_2_surg, c(by = "hb" = "hb_surgery")
# 
# kpi_3_2_surg <- kpi_3_2_surg |>
#   pivot_longer(!hb_surgery:kpi, names_to = "group", values_to = "value") |>
#   mutate(financial_year = group, .after = kpi) |>
#   mutate(financial_year = stringr::str_remove(financial_year, "_cohort_n"),
#          financial_year = stringr::str_remove(financial_year, "_surgery_n"),
#          financial_year = stringr::str_remove(financial_year, "_cover_p"),
#          group = case_when(stringr::str_detect(group, "cohort") ~ "cohort_n",
#                            stringr::str_detect(group, "surgery") ~ "surgery_n",
#                            stringr::str_detect(group, "cover") ~ "cover_p")) |>
#   rename(health_board = hb_surgery)
# 
# table(kpi_3_2_surg$hb_surgery, kpi_3_2_surg$financial_year) # all hbres/FY are 3
# # Current run NOT saved with this transformation, but to decide how to best store
#### 

# Run the below code if it is decided that above code (creates HB records w NAs)
# should NOT used (this uses less data storage than above)
kpi_3_2_surg <- kpi_3_2_surg |>
  pivot_longer(!hb_surgery:financial_year, names_to = "group", 
               values_to = "value") |> 
  rename(health_board = hb_surgery)

table(kpi_3_2_surg$health_board, droplevels(kpi_3_2_surg$financial_year)) # NOT all hbres/FY are 3

kpi_3_2_surg <- kpi_3_2_surg |> 
  # remove NAs for numerical counts and replace w 0
  # not sure if this needs to be done?
  mutate(value = case_when((group == "cohort_n" | group == "surgery_n") & 
                             is.na(value) ~ 0, TRUE ~ value))


###
# Check differences between hbres and hb_surgery
check <- kpi_3_2[kpi_3_2$financial_year %in% c(kpi_report_years),]
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
rm(kpi_3_2, kpi_3_2_hb, kpi_3_2_scot, hb_list)


### Step 5: Write outputs ----
# Combine KPI 3 subsets
kpi_3 <- bind_rows(kpi_3_1_res, kpi_3_2_res, kpi_3_2_surg)
table(kpi_3$kpi) 

## Historical file
# Create backup of last year's file
hist_db <- read_rds(paste0(hist_path,"/aaa_kpi_historical_theme4.rds"))

# temp: renaming "financial_year" to "fin_year" to make below function work
# AMc note: discuss with KH as to whether this can be changed permanently??
hist_db <- hist_db |> 
  rename(fin_year = financial_year)

kpi_3 <- kpi_3 |> 
  rename(fin_year = financial_year)

# create historical backup + new file with this year's data
phsaaa::build_history(hist_db, kpi_3, "3")

table(hist_db$financial_year, hist_db$kpi) 
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
report_db <- kpi_3 |> 
  filter(financial_year %in% c(kpi_report_years))

phsaaa::query_write_rds(report_db, paste0(temp_path, "/4_1_kpi_3_", yymm, ".rds"))

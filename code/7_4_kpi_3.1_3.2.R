###############################################################################
# kpi_3.1_3.2.R
# Calum Purdie
# 17/01/2023
# 
# KPI 3.1 percentage of men with AAA >= 5.5cm seen within two weeks of screening
# KPI 3.2 percentage of men with AAA >= 5.5cm deemed appropriate for 
# intervention/operated on within 8 weeks of screening
#
# Written/run on R Studio Server
# R version 3.6.1
###############################################################################



### 1 Housekeeping ----

# Load packages

library(here)
library(dplyr)
library(readr)
library(lubridate)
library(phsmethods)
library(janitor)
library(tidylog)

# Define date values

year <- 2022
month <- "09"

year_one <- "2019/20"
year_two <- "2020/21"
year_three <- "2021/22"
cut_off_date <- as.Date("2022-03-31")
meg_month <- "Nov"

# Define extract name

extract_name <- paste0("aaa_extract_", year, month, ".rds")

# Define file paths

extracts_path <- paste0("/PHI_conf/AAA/Topics/Screening/extracts", "/", year, 
                        month, "/output")



### 2 Data Manipulation ----

# Read in latest extract
# Remove screenings after cut_off_date
# Keep records where date_referral_true is populated and largest measure is 
# greater than or equal to 5.5, where result_outcome is not "02" and 
# date_screen is less than or equal to cut_off_date

aaa_extract <- read_rds(paste0(extracts_path, "/", extract_name)) %>% 
  filter(!is.na(date_referral_true) & largest_measure >= 5.5, 
         result_outcome != "02", 
         date_screen <= cut_off_date)



### 3 KPI 3.1 ----

# Calculate time between date seen in outpatients to date of screening
# Calculate seen variable and flag as 1 where screen_to_screen is less than or
# equal to 14, 0 elsewhere
# Create groups for screen_to_screen values

kpi_3_1_data <- aaa_extract %>% 
  filter(!is.na(date_referral_true) & largest_measure >= 5.5, 
         result_outcome != "02", 
         date_screen <= cut_off_date) %>% 
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

kpi_3_1_data %>% count(screen_to_screen)
kpi_3_1_data %>% count(seen)
kpi_3_1_data %>% count(screen_to_screen_group)

# Keep records where result_outcome is "01", "03", "04" or "05" or where
# date_seen_outpatient is populated and screen_to_screen is greater than equal 
# to zero

kpi_3_1_data <- kpi_3_1_data %>% 
  filter(result_outcome %in% c("01", "03", "04", "05") | 
           !is.na(date_seen_outpatient) & screen_to_screen >= 0)

# Calculate Health Board totals
# Group by hbres and financial_year
# Summarise totals to get cohort and sum seen column

kpi_3_1_hb_totals <- kpi_3_1_data %>% 
  group_by(hbres, financial_year) %>% 
  summarise(cohort = n(), 
            seen = sum(seen))

# Calculate Health Board totals
# Group by financial_year
# Summarise totals to get cohort and sum seen column

kpi_3_1_scot_totals <- kpi_3_1_data %>% 
  group_by(financial_year) %>% 
  summarise(cohort = n(), 
            seen = sum(seen)) %>% 
  mutate(hbres = "Scotland") %>% 
  relocate(hbres, .before = financial_year)

# Bind scot_totals and hb_totals
# Set NA to 0 for numeric columns
# Group by hbres and sum cohort and seen
# Calculate percentage for cumulative seen and percentage seen within two weeks

kpi_3_1 <- bind_rows(kpi_3_1_scot_totals, kpi_3_1_hb_totals) %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) %>% 
  group_by(hbres) %>% 
  mutate(cum_referrals = sum(cohort), 
         cum_seen = sum(seen), 
         pc_cum_seen = round_half_up(cum_seen * 100 / cum_referrals, 1), 
         pc_within_two_weeks = round_half_up(seen * 100 / cohort, 1))

# Pivot data into wider format, using names from financial_year and values from
# cohort, seen and pc_within_two_weeks
# Select column order to match output file specification
# Arrange output file so that Scotland row is first

kpi_3_1_output <- kpi_3_1 %>%
  pivot_wider(names_from = financial_year, 
              values_from = c(cohort, seen, pc_within_two_weeks), 
              names_glue = "{financial_year}_{.value}") %>% 
  select(hbres, starts_with("2012/13"), starts_with("2013/14"), 
         starts_with("2014/15"), starts_with("2015/16"), starts_with("2016/17"), 
         starts_with("2017/18"), starts_with("2018/19"), starts_with("2019/20"), 
         starts_with("2020/21"), starts_with("2021/22"), 
         cum_referrals, cum_seen, pc_cum_seen) %>% 
  arrange(hbres != "Scotland")

# Select hbres and latest three years columns

kpi_3_1_latest_years <- kpi_3_1_output %>%
  select(hbres, 
         starts_with(year_one), starts_with(year_two), starts_with(year_three))

# Tidy environment

rm(kpi_3_1_data, kpi_3_1_hb_totals, kpi_3_1_scot_totals, kpi_3_1)



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
         starts_with("2020/21"), starts_with("2021/22"), 
         cum_approp, cum_surgery, pc_cum_surgery) %>% 
  arrange(hbres != "Scotland")

# Select hbres and latest three years columns

kpi_3_2_latest_years <- kpi_3_2_output %>%
  select(hbres, 
         starts_with(year_one), starts_with(year_two), starts_with(year_three))

# Tidy environment

rm(kpi_3_2_data, kpi_3_2_hb_totals, kpi_3_2_scot_totals, kpi_3_2, aaa_extract)



### 5 Output ----

# Decide where to save output

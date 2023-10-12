###############################################################################
# vascular_referral_count.R
# Calum Purdie
# 16/02/2023
# 
# Table 7: Vascular referral types
#
# Written/run on Posit Server
# R version 4.1.2
###############################################################################

### 1 Housekeeping ----

# Load packages

# library(dplyr)
# library(readr)
# library(lubridate)
# library(phsmethods)
# library(stringr)
# library(janitor)
# library(haven)
# library(zoo)
# library(tidylog)

rm(list = ls())
gc()


source(here::here("code/0_housekeeping_theme_4.R"))

rm(fy_list, hb_list, output_path)


### 2 Data Extraction ----
# Keep where they were referred to vascular, had a large aneurysm, date_screen
# is less than or equal to cut_off_date and result_outcome is not referred in 
# error
aaa_extract <- read_rds(extract_path) %>% 
  # referred to vascular
  filter(!is.na(date_referral_true) & largest_measure >= 5.5, 
         date_screen <= cut_off_date, 
         result_outcome != "02")

# Count screen_result - they should all be positive
# There shouldn't be any external positive results that have a referral - think 
# it is impossible for system to do this

aaa_extract %>% count(screen_result)

# Define a source_ref_to_vasc variable

aaa_extract <- aaa_extract %>% 
  mutate(source_ref_to_vasc = case_when(
    screen_type %in% c("02", "04") ~ "Surveillance", 
    pat_elig == "03" ~ "Self Referral", 
    screen_type == "01" ~ "Initial Screen", 
    TRUE ~ "Not Assigned"))

# Define year_screen variable
# This uses three defined years and sets other years to Other

aaa_extract <- aaa_extract %>% 
  mutate(year_screen = case_when(
    financial_year %in% c(kpi_report_years) ~ financial_year, 
    TRUE ~ "Other"))


### 3 Create Vascular Referral Output ----
vascular_referral_count <- aaa_extract %>% 
  count(source_ref_to_vasc, year_screen) %>% 
  group_by(source_ref_to_vasc) %>% 
  # create a "Cumulative" level in year_screen (program totals)
  group_modify(~ adorn_totals(.x, name = "Cumulative")) %>% 
  ungroup() %>% 
  # keep only reporting years
  filter(year_screen != "Other") 

vascular_referral_count <- vascular_referral_count %>% 
  group_by(year_screen) %>% 
  # create a source_ref_to_vasc summation (per year)
  group_modify(~ adorn_totals(.x, name = "Total")) %>% 
  mutate(pc = round_half_up(n * 100 / sum(max(n)), 1)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = year_screen, 
              values_from = c(n, pc), 
              names_glue = "{year_screen}_{.value}") %>% 
  # move Total to top of referral source
  arrange(source_ref_to_vasc != "Total", source_ref_to_vasc) %>% 
  select(source_ref_to_vasc, starts_with(kpi_report_years[1]), 
         starts_with(kpi_report_years[2]), starts_with(kpi_report_years[3]), 
         starts_with("Cumulative"))


### 4 Save Output ----
write_csv(vascular_referral_count, paste0(temp_path, "/9_vasc_referral_", 
                                          yymm, ".rds"))

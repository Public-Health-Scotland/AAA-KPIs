###############################################################################
# vascular_referral_count.R
# Calum Purdie
# 16/02/2023
# 
# Save out other final outcome extracts
#
# Written/run on Posit Server
# R version 4.1.2
###############################################################################

### 1 Housekeeping ----

# Load packages

library(here)
library(dplyr)
library(readr)
library(lubridate)
library(phsmethods)
library(stringr)
library(janitor)
library(haven)
library(openxlsx)
library(zoo)
library(tidylog)

# Define date values

year <- 2022
month <- "09"

cut_off_date <- as.Date("2022-03-31")
year_one <- "2019/20"
year_two <- "2020/21"
year_three <- "2021/22"

# Define extract name

extract_name <- paste0("aaa_extract_", year, month, ".rds")

# Define file paths

extracts_path <- paste0("/PHI_conf/AAA/Topics/Screening/extracts", "/", year, 
                        month, "/output")

output_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/temp/4. RTO/")



### 2 Data Extraction ----

# Read in latest extract
# Keep where they were referred to vascular, had a large aneurysm, date_screen
# is less than or equal to cut_off_date and result_outcome is not referred in 
# error

aaa_extract <- read_rds(paste0(extracts_path, "/", extract_name)) %>% 
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
    financial_year %in% c(year_one, year_two, year_three) ~ financial_year, 
    TRUE ~ "Other"))



### 3 Create Vascular Referral Output ----

# Count source_ref_to_vasc and year_screen
# Group by source_ref_to_vasc and add Cumulative totals for each group
# Remove data where year_screen equals "Other"
# Group by year_screen and add Totals for each group
# Calculate percentage for each source_ref_to_vasc in each year
# Pivot data into wider format, taking names from year_screen and values from n
# Add totals for each column
# Arrange data by putting Total row first and then source_ref_to_vasc in 
# alphabetical order
# Select column order

vascular_referral_count <- aaa_extract %>% 
  count(source_ref_to_vasc, year_screen) %>% 
  group_by(source_ref_to_vasc) %>% 
  group_modify(~ adorn_totals(.x, name = "Cumulative")) %>% 
  ungroup() %>% 
  filter(year_screen != "Other") %>% 
  group_by(year_screen) %>% 
  group_modify(~ adorn_totals(.x, name = "Total")) %>% 
  mutate(pc = round_half_up(n * 100 / sum(max(n)), 1)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = year_screen, 
              values_from = c(n, pc), 
              names_glue = "{year_screen}_{.value}") %>% 
  arrange(source_ref_to_vasc != "Total", source_ref_to_vasc) %>% 
  select(source_ref_to_vasc, starts_with(year_one), starts_with(year_two), 
         starts_with(year_three), starts_with("Cumulative"))



### 4 Save Output ----

# Save output

write_xlsx(vascular_referral_count, 
           paste0(output_path, "Table 7 Vascular Referral Source.xlsx"))
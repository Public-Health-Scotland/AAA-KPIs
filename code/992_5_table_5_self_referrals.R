###############################################################################
# table_5_self_referrals.R
# Calum Purdie
# 19/10/2022
# 
# Extracts self referrals data from latest extract and calculates output
# 
# Written/run on Posit Workbench
# R version 4.1.2
###############################################################################



### 1 Housekeeping ----

# Load packages

library(here)
library(dplyr)
library(haven)
library(stringr)
library(readr)
library(phsmethods)
library(janitor)
library(openxlsx)
library(tidylog)

# Define date values

year <- 2023
month <- "03"

# Define years

year_one <- "2020/21"
year_two <- "2021/22"
year_three <- "2022/23"
cut_off_date <- as.Date("2023-03-31")

# Define extract name

extract_name <- paste0("aaa_extract_", year, month, ".rds")

# Define file paths

extracts_path <- paste0("/PHI_conf/AAA/Topics/Screening/extracts/", year, 
                        month, "/output")

output_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", year, month, 
                      "/output/5. Results for Eligible Cohort & Self Referrals", 
                      "_", year, month, ".xlsx")

# Functions

# Define function for calculating rates
# Input dataframe and then an optional year grouping (...)

calculate_rates <- function(data, ...){
  
  # Count number of people tested within data
  
  tested_df <- data %>% 
    count(hbres, ..., name = "tested")
  
  # Filter for positive results excluding those with immediate recall
  # Count number of people positive within data
  
  positive_df <- data %>% 
    filter(screen_result == "01" & followup_recom != "05") %>% 
    count(hbres, ..., name = "positive")
  
  # Combine tested and positive and set NAs to 0
  # Calculate positivity rate
  
  combine_df <- full_join(tested_df, positive_df) %>% 
    mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) %>% 
    mutate(rate = round_half_up(positive / tested * 100, 1))
  
  # Define Scotland totals by year grouping (if applicable)
  # Summarise positive and tested and calculate rate
  # Set hbres value as Scotland
  
  scotland_totals <- combine_df %>% 
    group_by(...) %>% 
    summarise(across(c(positive, tested), ~ sum(.))) %>% 
    mutate(rate = round_half_up(positive / tested * 100, 1), 
           hbres = "Scotland")
  
  # Bind output together
  
  output <- bind_rows(combine_df, scotland_totals)
  
}



### 2 Data Manipulation ----

# Read in latest extract

aaa_extract <- read_rds(paste0(extracts_path, "/", extract_name))

# Remove screenings after cut_off_date
# Filter for self referrals and initial screens (including QA initial screens)
# Filter for tested (+ve, -ve, non-visualisation screen result)

aaa_extract <- aaa_extract %>% 
  filter(date_screen <= cut_off_date, 
         pat_elig == "03" & screen_type %in% c("01", "03"), 
         screen_result %in% c("01", "02", "04"))

# Arrange data by upi and date_screen
# Group by upi and hbres and take the last row for each group
# This gives the most recent screening for each upi
# Calculate screening year and age at screening
# Set year as Other if not in one of the defined years for this analysis

aaa_extract_one_row_per_upi <- aaa_extract  %>% 
  arrange(upi, date_screen) %>% 
  group_by(upi, hbres) %>% 
  slice(n()) %>% 
  ungroup() %>% 
  mutate(year_screen = extract_fin_year(date_screen), 
         age_at_screening = age_calculate(dob, date_screen)) %>% 
  mutate(year_screen = if_else(year_screen %in% c(year_one, year_two, 
                                                  year_three), 
                               year_screen, "Other"))
  


### 3 Calculate Rates ----

# Calculate rates for individual years

individual_years <- calculate_rates(aaa_extract_one_row_per_upi, year_screen)

# Calculate rates for all years and set year as Cumulative

all_years <- calculate_rates(aaa_extract_one_row_per_upi) %>% 
  mutate(year_screen = "Cumulative")

# Combine individual_years and all_years
# Pivot data into wider format, using names from year_screen and values from
# tested, positive and rate
# Select column order to match output file specification
# Arrange output file so that Scotland row is first

output <- bind_rows(individual_years, all_years) %>% 
  pivot_wider(names_from = year_screen, 
              values_from = c(tested, positive, rate), 
              names_glue = "{year_screen}_{.value}") %>% 
  select(hbres, starts_with(year_one), starts_with(year_two), 
         starts_with(year_three), starts_with("Cumulative")) %>% 
  arrange(hbres != "Scotland")



### 4 Output ----

# Load bowel template

bowel_template <- loadWorkbook(output_path)

# Add output to workbook

writeData(bowel_template, sheet = "Self-referral results", output, 
          startCol = 1, startRow = 8, colNames = F)

# Save workbook

saveWorkbook(bowel_template, output_path, overwrite = TRUE)

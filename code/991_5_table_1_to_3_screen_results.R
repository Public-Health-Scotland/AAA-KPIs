###############################################################################
# table_1_to_3_screen_results.R
# Calum Purdie
# 19/10/2022
# 
# Processes extract and generates tables 1 to 3 for results for eligible cohort 
# and self-referrals
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
library(janitor)
library(phsmethods)
library(openxlsx)
library(tidylog)

# Define date values for filepath

year <- 2023
month <- "03"

# Define dob cut-offs for each year

dob_one_start <- as.Date("1954-04-01")
dob_one_end <- as.Date("1955-03-31")
dob_two_start <- as.Date("1955-04-01")
dob_two_end <- as.Date("1956-03-31")
dob_three_start <- as.Date("1956-04-01")
dob_three_end <- as.Date("1957-03-31")

# Define years

year_one <- "2020/21"
year_two <- "2021/22"
year_three <- "2022/23"

# Define extract name

extract_name <- paste0("aaa_extract_", year, month, ".rds")

# Define file paths

extracts_path <- paste0("/PHI_conf/AAA/Topics/Screening/extracts/", year, 
                        month, "/output")

output_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", year, month, 
                      "/output/5. Results for Eligible Cohort & Self Referrals", 
                      "_", year, month, ".xlsx")

# Functions

# Define function for calculating totals for tables
# row_var is the variable to breakdown by in rows (e.g. hbres or SIMD)
# col_var is the variable to spread across columns (e.g. positivity or n tested)

calculate_totals <- function(data, row_var, col_var){
  
  # Count data by row_var, year_screen and col_var
  
  row_col_counts <- data %>% 
    count({{row_var}}, year_screen, {{col_var}}, name = "n")
  
  # Count data by row_var and year_screen
  # Add on a column for col_var and set all values as total
  
  row_counts <- data %>% 
    count({{row_var}}, year_screen, name = "n") %>% 
    mutate({{col_var}} := "total")
  
  # Bind row_col_counts and row_counts together
  
  combined_counts <- bind_rows(row_col_counts, row_counts)
  
  # Define Scotland totals by grouped year_screen and col_var
  # Summarise total n
  # Add on a column for row_var and set all values as Scotland
  
  scot_counts <- combined_counts %>% 
    group_by(year_screen, {{col_var}}) %>% 
    summarise(n = sum(n)) %>% 
    mutate({{row_var}} := "Scotland")
  
  # Bind combined_counts and scot_counts together
  
  counts <- bind_rows(combined_counts, scot_counts)
  
  # Calculate cumulative totals
  # Group by row_var and col_var and summarise total n
  # Add on a column for year_screen and set all values as Cumulative
  
  cumulative_totals <- counts %>%
    group_by({{row_var}}, {{col_var}}) %>%
    summarise(n = sum(n)) %>%
    mutate(year_screen = "Cumulative")
  
  # Bind counts and cumulative_totals together
  
  final_output <- bind_rows(counts, cumulative_totals)
  
}



### 2 Processing ----

# Read in latest extract

aaa_extract <- read_rds(paste0(extracts_path, "/", extract_name))

# Only want initial screens so select if screen type is initial or QA initial
# Include records where men were tested only (ie. result of +ve, -ve or 
# non-visualisation)
# Derive measurements for the PHS screen result categories
# A measurement category is derived for definitive screen results i.e. positive, 
# negative, external postive or external negative results unless the follow up 
# recommendation is immediate recall ('05'). 
# This means a measurement category is not derived for technical fails, non 
# visualisations and immediate recalls. 

aaa_filtered <- aaa_extract %>% 
  filter(screen_type %in% c("01", "03") & 
           screen_result %in% c("01", "02", "04"))

aaa_filtered %>% count(aaa_size_group)

# There are 2 screens assigned to the category very large error - one was 
# investigated in May 2021 and found to be legitimate, recode to large group

aaa_filtered <- aaa_filtered %>% 
  mutate(aaa_size_group = recode(aaa_size_group, "very large error" = "large"))

# Aggregate to one record per UPI
# Initial screens with a result of non-visualisation, positive, negative 
# (including those with immediate recall) are included in this file so take the 
# last screen result based on the screen date.

last_results_initial_screens_output <- aaa_filtered %>% 
  arrange(upi, date_screen) %>% 
  group_by(upi) %>% 
  slice(n()) %>% 
  ungroup() %>% 
  select(upi, date_screen, screen_type, screen_exep, screen_result, 
         followup_recom, aaa_size_group)

# Count screen_result

last_results_initial_screens_output %>% count(screen_result)

# Tidy enviroment

rm(aaa_filtered)



### 3 First Offer ----

# Remove records where no offer has been sent
# These records have a screen type and a screen date because the clinic 
# appointment invitation has been set up but it has not been sent yet 
# e.g. on 1 Sept 2016 extract there are records with a clinic date of October 
# 2016 (invite not sent yet).

first_offer_initial_screens <- aaa_extract %>% 
  filter(screen_type %in% c("01", "03") & !is.na(date_offer_sent)) %>% 
  arrange(upi, date_offer_sent) %>% 
  group_by(upi) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(upi, first_date_offer_sent = date_offer_sent)

# Join first_offer_initial_screens and last_results_initial_screens_output

fo_lr_initial_screens <- first_offer_initial_screens %>% 
  full_join(last_results_initial_screens_output)

fo_lr_initial_screens %>% count(screen_result)

# Tidy environment

rm(aaa_extract, first_offer_initial_screens, 
   last_results_initial_screens_output)



### 4 Join on Invite and Uptake Data ----

# Read in invite and uptake data

invite_and_uptake <- read_rds(paste0("/PHI_conf/AAA/Topics/Screening/KPI/", 
                                     year, month, "/temp/", 
                                     "1_inviteanduptake_initial.rds")) %>% 
  select(upi, ca2019, simd2020v2_sc_quintile, hbres, dob_eligibility, dob)

# Join invite_and_uptake and fo_lr_initial_screens
# Define a result_type, showing positive or negative depending on result
# Define a screening year based on dob
# Filter to keep screen_result 01, 02 or 04

combined_output <- invite_and_uptake %>% 
  left_join(fo_lr_initial_screens) %>% 
  mutate(result_type = if_else(screen_result == "01" & followup_recom != "05", 
                               "positive", "negative"), 
         year_screen = case_when(dob >= dob_one_start & 
                                   dob <= dob_one_end ~ year_one, 
                                 dob >= dob_two_start & 
                                   dob <= dob_two_end ~ year_two, 
                                 dob >= dob_three_start & 
                                   dob <= dob_three_end ~ year_three, 
                                 dob < dob_one_start ~ "older")) %>% 
  filter(screen_result %in% c("01", "02", "04"))

# Tidy environment

rm(fo_lr_initial_screens, invite_and_uptake)



### 5 Table One ----

# Define table_one_data by removing any blank year_screen values
# This removed screenings beyond cut-off point

table_one_data <- combined_output %>% 
  filter(!is.na(year_screen))

table_one_data %>% count(result_type)

# Calculate totals for table_one_data by hbres and result_type
# Remove negative results and older screenings
# Pivot data into wider format, using names from result_type and values from n
# Calculate a percentage positive value
# Pivot data into wider format, using names from year_screen and values from
# total, positive and pc
# Select column order to match output file specification
# Arrange data to put Scotland row first and then alphabetical below

table_one <- calculate_totals(table_one_data, hbres, result_type) %>% 
  filter(result_type != "negative" & year_screen != "older") %>% 
  pivot_wider(names_from = result_type,
              values_from = n) %>% 
  mutate(pc = round_half_up(positive * 100 / total, 1)) %>% 
  pivot_wider(names_from = year_screen, 
              values_from = c(total, positive, pc), 
              names_glue = "{year_screen}_{.value}") %>% 
  select(hbres, starts_with(year_one), starts_with(year_two), 
         starts_with(year_three), 
         starts_with("Cumulative")) %>% 
  arrange(hbres != "Scotland", hbres)



### 6 Table Two ----

# Define table_two_data by keeping posititve results and removing blank years

table_two_data <- combined_output %>% 
  filter(result_type == "positive" & !is.na(year_screen))

table_two_data %>% count(aaa_size_group)

# Calculate totals for table_two_data by hbres and aaa_size_group
# Pivot data into wider format, using names from aaa_size_group and values 
# from n
# Calculate a percentage positive value across large, medium and small columns
# Pivot data into wider format, using names from year_screen and values from
# total, small, medium and large, including pc cols too
# Select column order to match output file specification
# Arrange data to put Scotland row first and then alphabetical below

table_two <- calculate_totals(table_two_data, hbres, aaa_size_group) %>% 
  pivot_wider(names_from = aaa_size_group,
              values_from = n) %>% 
  mutate(across(c(large, medium, small), 
                ~ round_half_up(. * 100 / total, 1), 
                .names = "{col}_pc")) %>% 
  pivot_wider(names_from = year_screen, 
              values_from = c(total, small, small_pc, medium, medium_pc, large, 
                              large_pc), 
              names_glue = "{year_screen}_{.value}") %>% 
  select(hbres, starts_with(year_one), starts_with(year_two), 
         starts_with(year_three), 
         starts_with("Cumulative")) %>% 
  arrange(hbres != "Scotland", hbres)



### 7 Table Three ----

# Define table_three_data by setting simd as a character
# Set NAs as unknown and add most and least to 1 and 5
# Remove rows with blank year_screen

table_three_data <- combined_output %>% 
  mutate(simd2020v2_sc_quintile = 
           case_when(simd2020v2_sc_quintile == 1 ~ "1=most", 
                     simd2020v2_sc_quintile == 5 ~ "5=least",
                     is.na(simd2020v2_sc_quintile) ~ "unknown", 
                     TRUE ~ as.character(simd2020v2_sc_quintile))) %>% 
  filter(!is.na(year_screen))

table_three_data %>% count(simd2020v2_sc_quintile)

# Calculate totals for table_three_data by SIMD and result_type
# Pivot data into wider format, using names from result_type and values from n
# Calculate a percentage positive value
# Pivot data into wider format, using names from year_screen and values from
# total, positive and pc
# Select column order to match output file specification
# Arrange data to put Scotland row first and then numerical below

table_three <- calculate_totals(table_three_data, simd2020v2_sc_quintile, 
                                result_type) %>% 
  filter(result_type != "negative" & year_screen != "older") %>% 
  pivot_wider(names_from = result_type,
              values_from = n) %>% 
  mutate(pc = round_half_up(positive * 100 / total, 1)) %>% 
  pivot_wider(names_from = year_screen, 
              values_from = c(total, positive, pc), 
              names_glue = "{year_screen}_{.value}") %>% 
  select(simd2020v2_sc_quintile, starts_with(year_one), starts_with(year_two), 
         starts_with(year_three), 
         starts_with("Cumulative")) %>% 
  arrange(simd2020v2_sc_quintile != "Scotland", simd2020v2_sc_quintile)



### 8 Output ----

# Load bowel template

bowel_template <- loadWorkbook(output_path)

# Add table one to workbook

writeData(bowel_template, sheet = "Eligible cohort results", table_one, 
          startCol = 1, startRow = 8, colNames = F)

# Add table two to workbook

writeData(bowel_template, sheet = "Eligible cohort AAA size", table_two, 
          startCol = 1, startRow = 9, colNames = F)

# Add table three to workbook

writeData(bowel_template, sheet = "Positive Results by Deprivation", table_three, 
          startCol = 1, startRow = 8, colNames = F)

# Save workbook

saveWorkbook(bowel_template, output_path, overwrite = TRUE)

###############################################################################
# 9_5_screen_result_tables.R
# Calum Purdie & Karen Hotopp
# 19/10/2022
# 
# Processes extract and generates tables 1 to 3 for results for eligible cohort 
# and self-referrals
# 
# Written/run on Posit WB, R 4.1.2
###############################################################################

### 1 Housekeeping ----

# Load packages
library(dplyr)
library(readr)
library(janitor)
library(phsmethods)
library(tidylog)


rm(list = ls())
gc()


source(here::here("code/0_housekeeping.R"))

rm(exclusions_path, cutoff_date, cut_off_12m, cut_off_3m, 
   prev_year, current_year, current_year_start, next_year_start,
   financial_year_due, financial_quarters, last_date, next_year)


# Define dob cut-offs for each year
# WHAT DEFINES THESE YEARS???
dob_one_start <- as.Date("1954-04-01") # 69 years
dob_one_end <- as.Date("1955-03-31")
dob_two_start <- as.Date("1955-04-01") # 68 years
dob_two_end <- as.Date("1956-03-31")
dob_three_start <- as.Date("1956-04-01") # 67 years
dob_three_end <- as.Date("1957-03-31")


## Functions

# Calculating totals for tables 1, 2, 3 ---
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
    mutate({{col_var}} := "cohort_n")
  
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

# Calculating rates for table 5 ---
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


### 2 Processing ----
# Read in latest extract
aaa_extract <- read_rds(extract_path)

aaa_filtered <- aaa_extract %>% 
  # select if screen_type is initial or QA initial
  filter(screen_type %in% c("01", "03") & 
           # keep where men tested (result of +ve, -ve or non-visualisation)
           screen_result %in% c("01", "02", "04"))

## What does this mean? Doesn't seem to be included in analysis...
# Derive measurements for the PHS screen result categories
# A measurement category is derived for definitive screen results i.e. positive, 
# negative, external postive or external negative results unless the follow up 
# recommendation is immediate recall ('05'). 
# This means a measurement category is not derived for technical fails, non 
# visualisations and immediate recalls. 

aaa_filtered %>% count(aaa_size_group)

# There are 2 screens assigned to the category very large error - one was 
# investigated in May 2021 and found to be legitimate, recode to large group
# What about the other one?? These are recoded to "large" for KPI 1, as well, 
# so maybe this should be moved to an initial script? Should this be in quarterly?
aaa_filtered <- aaa_filtered %>% 
  mutate(aaa_size_group = recode(aaa_size_group, "very large error" = "large"))

# Aggregate to one record per UPI
# Initial screens with a result of non-visualisation, positive, negative 
# (including those with immediate recall) are included in this file, so take  
# the last screen result based on the screen date.

last_results <- aaa_filtered %>% 
  arrange(upi, date_screen) %>% 
  group_by(upi) %>% 
  slice(n()) %>% 
  ungroup() %>% 
  select(upi, date_screen, screen_type, screen_exep, screen_result, 
         followup_recom, aaa_size_group)

# Count screen_result
# 01 - Positive AAA ≥ 3.0cm)
# 02 - Negative (AAA < 3.0cm)
# 04 - Non Visualisation
last_results %>% count(screen_result)

# Tidy environment
rm(aaa_filtered)


### 3 First Offer ----
# Remove records where no offer has been sent
# These records have a screen type and a screen date because the clinic 
# appointment invitation has been set up, but it has not been sent yet 
# For example: 1 Sept 2016 extract may have records with a clinic date of October 
# 2016 (invite not sent yet).
first_offer <- aaa_extract %>% 
  # select if screen_type is initial or QA initial
  filter(screen_type %in% c("01", "03") & !is.na(date_offer_sent)) %>% 
  arrange(upi, date_offer_sent) %>% 
  group_by(upi) %>% 
  slice(1) %>% 
  ungroup() %>% 
  select(upi, first_date_offer_sent = date_offer_sent)

# Join first_offer and last_result
fo_lr_initial_screens <- first_offer %>% 
  full_join(last_results)

fo_lr_initial_screens %>% count(screen_result)

# Tidy environment
rm(first_offer, last_results)


### 4 Join on Invite and Uptake Data ----
# Read in invite and uptake data
invite_uptake <- read_rds(paste0(temp_path, 
                                 "/1_inviteanduptake_initial.rds")) %>% 
  select(upi, ca2019, simd2020v2_sc_quintile, hbres, dob_eligibility, dob)

# Join invite_and_uptake and fo_lr_initial_screens
# Define a result_type, showing positive or negative depending on result
# Define a screening year based on dob
# Filter to keep screen_result 01, 02 or 04
combined_output <- invite_uptake %>% 
  left_join(fo_lr_initial_screens) %>% 
  mutate(result_type = if_else(screen_result == "01" & followup_recom != "05", 
                               "positive", "negative"), 
         year_screen = case_when(dob >= dob_one_start & 
                                   dob <= dob_one_end ~ kpi_report_years[1], 
                                 dob >= dob_two_start & 
                                   dob <= dob_two_end ~ kpi_report_years[2], 
                                 dob >= dob_three_start & 
                                   dob <= dob_three_end ~ kpi_report_years[3], 
                                 dob < dob_one_start ~ "older")) %>% 
  filter(screen_result %in% c("01", "02", "04"))

table(combined_output$year_screen, useNA = "ifany")

# Tidy environment
rm(fo_lr_initial_screens, invite_uptake)


### 5 Table One: Eligible cohort Results ----
# Define by removing any blank year_screen values
# This removed screenings beyond cut-off point
table_one_data <- combined_output %>% 
  filter(!is.na(year_screen))

table(table_one_data$year_screen, useNA = "ifany")

table_one_data %>% count(result_type)

# Calculate totals for table_one_data by hbres and result_type
table_1 <- calculate_totals(table_one_data, hbres, result_type) %>% 
  # Remove negative results and older screenings
  filter(result_type != "negative" & year_screen != "older") %>% 
  pivot_wider(names_from = result_type,
              values_from = n) %>% 
  mutate(positive_p = round_half_up(positive * 100 / cohort_n, 1)) %>% 
  select(hbres, year_screen, cohort_n, positive_n = positive, positive_p) |> 
  pivot_longer(!hbres:year_screen, names_to = "group", values_to = "value") |> 
  mutate(table = "Table 1", .after = hbres) |> 
  mutate(simd2020v2_sc_quintile = NA, .after = table)


### 6 Table Two: Eligible Cohort AAA Size ----
# Define by keeping positive results and removing blank years
table_two_data <- combined_output %>% 
  filter(result_type == "positive" & !is.na(year_screen))

table_two_data %>% count(aaa_size_group)

# Calculate totals for table_two_data by hbres and aaa_size_group
table_2 <- calculate_totals(table_two_data, hbres, aaa_size_group) %>% 
  filter(year_screen != "older") |> 
  pivot_wider(names_from = aaa_size_group,
              values_from = n) %>% 
  mutate(across(c(large, medium, small), ~ round_half_up(. * 100 / cohort_n, 1), 
                .names = "{col}_p"))  |> 
  select(hbres, year_screen, cohort_n, 
         small_n = small, small_p,
         medium_n = medium, medium_p,
         large_n = large, large_p) |> 
  pivot_longer(!hbres:year_screen, names_to = "group", values_to = "value") |> 
  mutate(table = "Table 2", .after = hbres) |> 
  mutate(simd2020v2_sc_quintile = NA, .after = table)

  
### 7 Table Three: Positive Results by SIMD ----
# Define table_three_data by setting SIMD as a character and set NAs as unknown 
# Remove rows with blank year_screen
table_three_data <- combined_output %>% 
  mutate(simd2020v2_sc_quintile = 
           case_when(simd2020v2_sc_quintile == 1 ~ "1=most deprived", 
                     simd2020v2_sc_quintile == 5 ~ "5=least deprived",
                     is.na(simd2020v2_sc_quintile) ~ "Unknown", 
                     TRUE ~ as.character(simd2020v2_sc_quintile))) %>% 
  filter(!is.na(year_screen))

table_three_data %>% count(simd2020v2_sc_quintile)

# Calculate totals for table_three_data by SIMD and result_type
table_3 <- calculate_totals(table_three_data, simd2020v2_sc_quintile, 
                            result_type) %>% 
  filter(result_type != "negative" & year_screen != "older") %>% 
  pivot_wider(names_from = result_type,
              values_from = n) %>% 
  mutate(positive_p = round_half_up(positive * 100 / cohort_n, 1))  |> 
  select(simd2020v2_sc_quintile, year_screen, cohort_n, 
         positive_n = positive, positive_p) |> 
  pivot_longer(!simd2020v2_sc_quintile:year_screen, 
               names_to = "group", values_to = "value") |> 
  mutate(hbres = NA, .before = simd2020v2_sc_quintile) |> 
  mutate(table = "Table 3", .after = hbres)


### 8 Table Five: Self-referral Results ----
## A: Reformat extract data
# Remove screenings after cut_off_date
# Filter for self referrals and initial screens (including QA initial screens)
# Filter for tested (+ve, -ve, non-visualisation screen result)
aaa_extract <- aaa_extract %>% 
  filter(date_screen <= date_cut_off, 
         pat_elig == "03" & screen_type %in% c("01", "03"), 
         screen_result %in% c("01", "02", "04"))

# Group by upi and hbres and take the last row for each group
# This gives the most recent screening for each upi
aaa_extract_recent <- aaa_extract  %>% 
  arrange(upi, date_screen) %>% 
  group_by(upi, hbres) %>% 
  slice(n()) %>% 
  ungroup() %>% 
  # financial year of screening
  mutate(year_screen = extract_fin_year(date_screen), 
         age_at_screening = age_calculate(dob, date_screen)) %>% 
  # set year as Other if not in one of the defined years for this analysis
  mutate(year_screen = if_else(year_screen %in% c(kpi_report_years), 
                               year_screen, "Other"))

## B: Calculate Rates ---
# Calculate rates for individual years
individual_years <- calculate_rates(aaa_extract_recent, year_screen)

# Calculate rates for all years and set year as Cumulative
all_years <- calculate_rates(aaa_extract_recent) %>% 
  mutate(year_screen = "Cumulative", .after = hbres)

# Combine to create table 5 excerpt
table_5 <- bind_rows(individual_years, all_years) |> 
  pivot_longer(!hbres:year_screen, names_to = "group", values_to = "value") |> 
  filter(year_screen != "Other") |> 
  mutate(table = "Table 5", .after = hbres) |> 
  mutate(simd2020v2_sc_quintile = NA, .after = table)


### 9 Combine and Save ----
# Combine tables
theme5_tables <- bind_rows(table_1, table_2, table_3, table_5)

# Save
write_rds(theme5_tables, paste0(temp_path, "/5_1_results_tables_", yymm, ".rds"))


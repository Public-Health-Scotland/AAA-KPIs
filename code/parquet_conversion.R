
# first test using kpi 1 processing ---------------------------------------

#~~~~~~~~~~~~~~~~~~~~~~~~~
# 1_2_kpi_1_processing.R
# Angus Morton
# 19/10/2022
#
# Create the invite/uptake file used for producing KPIs 1.1-1.3
#
# Written on R Server (R Version 3.6.1)
# Revised on Posit PWB, R Version 4.1.2
#~~~~~~~~~~~~~~~~~~~~~~~~~

# Invite/uptake file is a patient-level extract with information on invites
# and attendances for each individual. This can then be used to calculate
# figures for the uptake KPIs.


### Step 1: Housekeeping ----
library(readr)
library(dplyr)
library(lubridate)
library(tidylog)
library(svDialogs)

rm(list = ls())
gc()


source(here::here("code/0_housekeeping.R"))

rm (hist_path, output_path, simd_path, extract_date,
    fy_list, hb_list, fy_tibble, hb_tibble, kpi_report_years, season,
    cut_off_date, end_current, end_date, start_date, meg_month,
    year1_end, year1_start, year2_end, year2_start, year1, year2, yymm)


### Step 2: Import data ----
aaa_extract <- read_rds(extract_path)
aaa_exclusions <- read_rds(exclusions_path)



# ### convert to parquet --------------------------------------------------

library(arrow)

pq_path <- "/PHI_conf/AAA/Topics/Git/AMc/parquet_test"

aaa_extract |> 
  write_dataset(path = pq_path, format = "parquet")

aaa_extract_pq <- open_dataset(pq_path)



# Only want initial screens, so select if screen type is initial or QA initial
# Include records where men were tested only (i.e., result of +ve, -ve or
# non-visualisation)
query <- aaa_extract_pq %>%
  filter(screen_type %in% c("01", "03")) 

last_results_initial_screens <- query |> collect()
  
x <- aaa_extract_pq |> 
  filter(hbres == "Western Isles") |> 
  collect()
# don't need to specify the stuff first!!

last_results_initial_screens <- last_results_initial_screens  %>% 
  # very large AAAs will have been investigated previously; update variable
  mutate(aaa_size_group = recode(aaa_size_group,
                                 "very large error" = "large"))
### Step 3: First offer sent ----
# Check for earliest 'date_offer_sent' for each UPI and remove later offers
# Create an object with each UPI and its earliest offer date
first_offer_dates <- last_results_initial_screens %>%
  filter(!is.na(date_offer_sent)) %>%
  group_by(upi) %>%
  # potentially could just do slice here...
  mutate(
    first_offer_flag = if_else(date_offer_sent == min(date_offer_sent), 1, 0)
  ) %>%
  ungroup() %>%
  filter(first_offer_flag == 1) %>%
  select(upi, date_first_offer_sent = date_offer_sent) %>%
  distinct()


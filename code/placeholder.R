#~~~~~~~~~~~~~~~~~~~~~~~~~
# 1_processing_for_KPI_11-13.R
# Angus Morton
# 19/10/2022
#
# Create the invite/uptake file used for producing KPI 1.1-1.3
#
# Written on RServer (R Version 3.6.1)
#~~~~~~~~~~~~~~~~~~~~~~~~~

# invite/uptake file is a patient level extract with information on invites
# and attendances for each individual
 
# Step 1 : Import packages and filepaths

# Step 2 : Import and trim data

# Step 3: Create derived variables

# Step 2: Create a first offer sent file

# STEP 3: Create first screening result file

# STEP 4: Create baseline cohorts file

# STEP 5: Create exclusions files

# STEP 6: Match .





### Step 1 : Import packages and filepaths ----

# This should be the only step which needs edited each time

library(readr)
library(dplyr)
library(tidylog)
library(lubridate)

kpi_month <- "sep22"

cutoff_date <- dmy("31-03-1957")

extract_fpath <- paste0("/PHI_conf/AAA/Topics/Screening/extracts/",
                        "202209/output/")

### Step 2 : Import and trim data ----

aaa_extract <- read_rds(paste0(extract_fpath, "aaa_extract_202209.rds"))

### Step 3 : Create derived variables ----

# Create 'size_group' variable

aaa_extract <- aaa_extract %>%
  mutate(
    size_group = case_when(
      
    )
  )









# # Only want initial screens so select if screen type is initial or QA initial
# # Include records where men were tested only (ie. result of +ve, -ve or 
# # non-visualisation)
# # Derive measurements for the PHS screen result categories
# # A measurement category is derived for definitive screen results i.e. positive, 
# # negative, external postive or external negative results unless the follow up 
# # recommendation is immediate recall ('05'). 
# # This means a measurement category is not derived for technical fails, non 
# # visualisations and immediate recalls. 
# last_results_initial_screens <- aaa_extract %>%
#   filter(screen_type %in% c("01", "03")) %>%
#   filter(screen_result %in% c("01", "02", "04")) %>%
#   mutate(isd_aaa_size = case_when(screen_result %in% c("01", "02", "05", "06") &
#                                     (followup_recom != "05" |
#                                        is.na(followup_recom)) ~ largest_measure)) %>%
#   mutate(isd_aaa_size_group = case_when(isd_aaa_size >= 0 &
#                                           isd_aaa_size <= 2.9 ~ "negative", 
#                                         isd_aaa_size >= 3 &
#                                           isd_aaa_size <= 4.4 ~ "small",
#                                         isd_aaa_size >= 4.5 &
#                                           isd_aaa_size <= 5.4 ~ "medium",
#                                         isd_aaa_size >= 5.5 &
#                                           isd_aaa_size <= 10.5 ~ "large",
#                                         isd_aaa_size >= 10.6 ~
#                                           "very large error"))
# last_results_initial_screens %>% count(isd_aaa_size_group)
# # There is 1 screen assigned to the category very large error - this was 
# # investigated in May 2021 and found to be legitimate,  sorecode to large group
# last_results_initial_screens <- last_results_initial_screens %>%
#   mutate(isd_aaa_size_group = recode(isd_aaa_size_group, 
#                                      "very large error" = "large"))


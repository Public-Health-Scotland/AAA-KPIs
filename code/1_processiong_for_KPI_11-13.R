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

# Only want initial screens so select if screen type is initial or QA initial
# Include records where men were tested only (ie. result of +ve, -ve or
# non-visualisation)
last_results_initial_screens <- aaa_extract %>%
  filter(screen_type %in% c("01", "03"))


### Step 3 : Create derived variables ----

####### Remove once moved to initial processing scripts ####
# # Create 'size_group' variable

# Export very large measurements as they might be errors

# Derive measurements for the PHS screen result categories
# A measurement category is derived for definitive screen results i.e. positive,
# negative, external postive or external negative results unless the follow up
# recommendation is immediate recall ('05').
# This means a measurement category is not derived for technical fails, non
# visualisations and immediate recalls.
last_results_initial_screens <- last_results_initial_screens %>%
  mutate(isd_aaa_size = case_when(screen_result %in% c("01", "02", "05", "06") &
                                    (followup_recom != "05" |
                                       is.na(followup_recom)) ~ largest_measure)) %>%
  mutate(isd_aaa_size_group = case_when(isd_aaa_size >= 0 &
                                          isd_aaa_size <= 2.9 ~ "negative",
                                        isd_aaa_size >= 3 &
                                          isd_aaa_size <= 4.4 ~ "small",
                                        isd_aaa_size >= 4.5 &
                                          isd_aaa_size <= 5.4 ~ "medium",
                                        isd_aaa_size >= 5.5 &
                                          isd_aaa_size <= 10.5 ~ "large",
                                        isd_aaa_size >= 10.6 ~
                                          "very large error"))

# Assume these have been investigated by the checking script

last_results_initial_screens <- last_results_initial_screens %>%
  mutate(isd_aaa_size_group = recode(isd_aaa_size_group,
                                     "very large error" = "large"))
############# End of removed protion #####


# potentially output a file here. (probably not)

### Step 4 : Create a first offer sent object ----
# Check for the earliest 'date_offer_sent' for each upi and filter out 
# later offers.
# Create an object with each upi and its earliest offer date

first_offer_dates <- last_results_initial_screens %>%
  filter(!is.na(date_offer_sent)) %>%
  group_by(upi) %>%
  mutate(
    first_offer_flag = if_else(date_offer_sent == min(date_offer_sent), 1, 0)
    ) %>%
  ungroup() %>%
  filter(first_offer_flag == 1) %>%
  select(upi, date_first_offer_sent = date_offer_sent) %>%
  distinct()
# The above has been checked against the spss with waldo::compare
# and the data is the same


### Step 5 : Create a first screening result object ----


first_result <- last_results_initial_screens %>%
  filter(!is.na(date_offer_sent)) %>%
  filter(screen_result != "03") %>%
  filter(!is.na(screen_result))

# for each UPI. Find the earliest screening result
# the 'FT' prefix stands for 'first tested'

first_result <- first_result %>%
  arrange(upi, date_screen) %>%
  group_by(upi) %>%
  mutate(
    results = n(),
    first_screen_flag = if_else(date_screen == min(date_screen), 1, 0)
  ) %>%
  ungroup() %>%
  filter(first_screen_flag == 1) %>%
  select(upi, results,
         FT_screen_date = date_screen,
         FT_screen_type = screen_type,
         FT_screen_exep = screen_exep,
         FT_screen_result = screen_result,
         isd_aaa_size_group) %>%
  distinct()

# look at duplicate upis
first_result %>% count(upi) %>% arrange(desc(n))
# To deduplicate records with 2 first results on the one day. Choose the one
# with the more important result
# Presumed order of importance
# 01 Positive
# 05 External positive
# 02 Negative
# 06 External negative
# the rest

# deduplicate
first_result <- first_result %>%
  group_by(upi) %>%
  mutate(importance = case_when(
    FT_screen_result == "01" ~ 5,
    FT_screen_result == "05" ~ 4,
    FT_screen_result == "02" ~ 3,
    FT_screen_result == "06" ~ 2,
    TRUE ~ 1
  )) %>%
  filter(importance == max(importance)) %>%
  ungroup() %>%
  distinct()


first_offer_first_result <- first_offer_dates %>%
  left_join(first_result, by = "upi") %>%
  select(upi, date_first_offer_sent, FT_screen_date:FT_screen_result,
         isd_aaa_size_group, results)


### Step 6 : Create a baseline cohort ----

aaa_extract <- aaa_extract %>%
  filter(pat_elig != "03")

aaa_extract <- aaa_extract %>%
  mutate(eligibility_period = case_when(
    between(dob, dmy("01-04-1947"), dmy("31-03-1948")) ~ "Turned 66 in year 201314",
    between(dob, dmy("01-04-1948"), dmy("31-03-1949")) ~ "Turned 66 in year 201415",
    between(dob, dmy("01-04-1949"), dmy("31-03-1950")) ~ "Turned 66 in year 201516",
    between(dob, dmy("01-04-1950"), dmy("31-03-1951")) ~ "Turned 66 in year 201617",
    between(dob, dmy("01-04-1951"), dmy("31-03-1952")) ~ "Turned 66 in year 201718",
    between(dob, dmy("01-04-1952"), dmy("31-03-1953")) ~ "Turned 66 in year 201819",
    between(dob, dmy("01-04-1953"), dmy("31-03-1954")) ~ "Turned 66 in year 201920",
    between(dob, dmy("01-04-1954"), dmy("31-03-1955")) ~ "Turned 66 in year 202021",
    between(dob, dmy("01-04-1955"), dmy("31-03-1956")) ~ "Turned 66 in year 202122",
    between(dob, dmy("01-04-1956"), dmy("31-03-1957")) ~ "Turned 66 in year 202223",
    between(dob, dmy("01-04-1957"), dmy("31-03-1958")) ~ "Turned 66 in year 202324"
  ),
    age65_onstartdate = case_when(
      hbres == "Ayrshire & Arran" &
        between(dob, dmy("01-06-1947"), dmy("31-05-1948")) ~ 1,
      hbres == "Borders" &
        between(dob, dmy("09-08-1946"), dmy("08-08-1947")) ~ 1,
      hbres == "Dumfries & Galloway" &
        between(dob, dmy("24-07-1947"), dmy("23-07-1948")) ~ 1,
      hbres == "Fife" &
        between(dob, dmy("09-01-1947"), dmy("08-01-1948")) ~ 1,
      hbres == "Forth Valley" &
        between(dob, dmy("18-09-1947"), dmy("17-09-1948")) ~ 1,
      hbres == "Grampian" &
        between(dob, dmy("03-10-1946"), dmy("02-10-1947")) ~ 1,
      hbres == "Greater Glasgow & Clyde" &
        between(dob, dmy("06-02-1947"), dmy("05-02-1948")) ~ 1,
      hbres == "Highland" &
        between(dob, dmy("29-06-1946"), dmy("28-06-1947")) ~ 1,
      hbres == "Lanarkshire" &
        between(dob, dmy("01-04-1947"), dmy("31-03-1948")) ~ 1,
      hbres == "Lothian" &
        between(dob, dmy("09-08-1946"), dmy("08-08-1947")) ~ 1,
      hbres == "Orkney" &
        between(dob, dmy("03-10-1946"), dmy("02-10-1947")) ~ 1,
      hbres == "Shetland" &
        between(dob, dmy("03-10-1946"), dmy("02-10-1947")) ~ 1,
      hbres == "Tayside" &
        between(dob, dmy("09-01-1947"), dmy("08-01-1948")) ~ 1,
      hbres == "Western Isles" &
        between(dob, dmy("29-06-1946"), dmy("28-06-1947")) ~ 1
    ))





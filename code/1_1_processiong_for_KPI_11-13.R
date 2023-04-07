#~~~~~~~~~~~~~~~~~~~~~~~~~
# 1_1_processing_for_KPI_11-13.R
# Angus Morton
# 19/10/2022
#
# Create the invite/uptake file used for producing KPIs 1.1-1.3
#
# Written on R Server (R Version 3.6.1)
# Revised on Posit PWB, R Version 4.1.2
#~~~~~~~~~~~~~~~~~~~~~~~~~

# invite/uptake file is a patient-level extract with information on invites
# and attendances for each individual. This can then be used to calculate
# figures for the uptake KPIs.
 
# Step 1 : Housekeeping
# Step 2 : Import data
# Step 3 : First offer sent
# Step 4 : First screening result
# Step 5 : Create a baseline cohort
# Step 6 : Create series of exclusions objects
# Step 7 : Remove exclusions
# Step 8 : Write out


### Step 1: Housekeeping ----

library(readr)
library(dplyr)
library(lubridate)
library(tidylog)


rm(list = ls())
gc()


source(here::here("code/0_housekeeping.R"))


### Step 2: Import data ----

aaa_extract <- read_rds(extract_path)
aaa_exclusions <- read_rds(exclusions_path)

# Only want initial screens, so select if screen type is initial or QA initial
# Include records where men were tested only (ie. result of +ve, -ve or
# non-visualisation)
last_results_initial_screens <- aaa_extract %>%
  filter(screen_type %in% c("01", "03")) %>% 
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


### Step 4: First screening result ----
first_result <- last_results_initial_screens %>%
  filter(!is.na(date_offer_sent)) %>%
  filter(screen_result != "03") %>%
  filter(!is.na(screen_result))

# Find the earliest screening result for each UPI
# The 'FT' prefix stands for 'first tested'

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
         aaa_size_group) %>%
  distinct()

## Duplicate UPIs
first_result %>% count(upi) %>% arrange(desc(n))
# To de-duplicate records with 2 first results on the one day, choose the one
# with the more important result
# Presumed order of importance
# 01 Positive
# 05 External positive
# 02 Negative
# 06 External negative
# the rest

# de-duplicate
first_result <- first_result %>%
  group_by(upi) %>%
  mutate(importance = tidytable::case_when(
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
         aaa_size_group, results)


rm(first_offer_dates, first_result, last_results_initial_screens)


### Step 5: Baseline cohort ----
cohort1 <- aaa_extract %>%
  filter(pat_elig != "03") %>%
  filter(screen_type %in% c("01", "03", NA))


## Sift through the duplicates
# This has been done in spss by sorting on date_offer_sent and then picking
# whatever record happens to be last. Trying to do it slightly
# more methodically here
# There are some records where chi != upi. If the chi != upi ends up being
# a problem then this will need redone

# Remove duplicates that are identical for all relevant variables
cohort1 <- cohort1 %>%
  distinct(upi, postcode, ca2019, simd2020v2_sc_quintile, hbres,
           dob_eligibility, dob, .keep_all = TRUE)


# Create variable called 'keep' to add records to.
# Start with all the non-duplicates, then bring more records in
cohort1 <- cohort1 %>%
  group_by(upi) %>%
  mutate(keep = if_else(n() == 1, 1, 0)) %>%
  # keep only the last offer
  mutate(keep = tidytable::case_when(keep == 1 ~ 1,
                                     date_offer_sent == max(date_offer_sent) ~ 1,
                                     TRUE ~ 0
  )) %>%
  ungroup()

# Remove records where the UPI is accounted for
cohort1 <- cohort1 %>%
  group_by(upi) %>%
  mutate(drop = if_else(any(keep == 1) & keep == 0, 1, 0)
  ) %>%
  ungroup() %>%
  filter(drop == 0)

# Keep only records where there is an offer sent
cohort1 <- cohort1 %>%
  group_by(upi) %>%
  mutate(keep = tidytable::case_when(keep == 1 ~ 1,
                                     !is.na(date_offer_sent) ~ 1,
                                     TRUE ~ 0
    )) %>%
  ungroup()

# Remove records where the UPI is accounted for
cohort1 <- cohort1 %>%
  group_by(upi) %>%
  mutate(drop = if_else(any(keep == 1) & keep == 0, 1, 0)
  ) %>%
  ungroup() %>%
  filter(drop == 0) %>%
  # keep where CHI and UPI match
  mutate(keep = case_when(keep == 1 ~ 1,
                          chi == upi ~ 1,
                          TRUE ~ 0
  ))

# We now have all the CHIs
cohort1 <- filter(cohort1, keep == 1)
# couple of pairs with the same date_offer_sent so get rid of the one
# with the mismatching chi
cohort1 <- cohort1 %>%
  group_by(upi) %>%
  mutate(keep = tidytable::case_when(n() != 1 & chi != upi ~ 0,
                                     TRUE ~ 1
  )) %>%
  ungroup() %>%
  filter(keep == 1) %>% 
  select(upi, postcode, ca2019, simd2020v2_sc_quintile,
         hbres, dob_eligibility, dob) %>%
  filter(dob <= cutoff_date)


# Step 6: Create series of exclusions objects ----
# some of these have '_summary' objects which were previously written to
# excel. They aren't written out from here currently but can add that step
# in if required.

### Already on surveillance prior to national programme ---
prior_sur <- aaa_extract %>%
  filter(screen_type %in% c("01", "02"))

# if upi has a screen_type 01 then take that
prior_sur <- prior_sur %>%
  group_by(upi) %>%
  mutate(keep = if_else(any(screen_type == "01") & 
                          screen_type == "02", 0, 1)
  ) %>%
  ungroup() %>%
  filter(keep == 1) %>%
  distinct(upi, screen_type) %>% 
  filter(screen_type == "02")

### Only an external result ---
external_only <- aaa_extract %>%
  filter(screen_result %in% c("05", "06")) %>%
  distinct(upi, screen_result)

external_only_summary <- external_only %>%
  count(screen_result)

external_only <- select(external_only, upi)

### Deceased ---
deceased <- aaa_exclusions %>%
  filter(pat_inelig %in% c("15","16")) %>%
  mutate(exclflag = tidytable::case_when(
    # remove exclusions that have ended
    !is.na(date_end) ~ 0,
    # keep only exclusions that started before the 66th birthday
    date_start < dob+years(66) ~ 1,
    # account for leap years
    is.na(dob+years(66)) & date_start < dob+days(1)+years(66) ~ 1,
    TRUE ~ 0
  )) %>%
  filter(exclflag == 1) %>%
  distinct(upi, pat_inelig)

deceased_summary <- deceased %>%
  count(pat_inelig)

deceased <- deceased %>% select(upi)

### Prior screen exclusion ---
prior_scr <- aaa_exclusions %>%
  filter(pat_inelig == "21") %>%
  mutate(exclflag = tidytable::case_when(
    # remove exclusions that have ended
    !is.na(date_end) ~ 0,
    # keep only exclusions that started before the 66th birthday
    date_start < dob+years(66) ~ 1,
    # account for leap years
    is.na(dob+years(66)) & date_start < dob+days(1)+years(66) ~ 1,
    TRUE ~ 0
  )) %>%
  filter(exclflag == 1) %>%
  distinct(upi, pat_inelig)

prior_scr_summary <- prior_scr %>%
  count(pat_inelig)

prior_scr <- prior_scr %>% select(upi) %>% arrange(upi)

### Opted out ---
optout <- aaa_exclusions %>%
  filter(pat_inelig == "01") %>% ##!! Why does this not include "02"?
  mutate(optout_length = date_end - date_start)

optout_length_summary <- optout %>%
  filter(!is.na(optout_length)) %>%
  count(optout_length)

optout <- optout %>%
  mutate(exclflag = tidytable::case_when(
    # remove exclusions that have ended
    !is.na(date_end) ~ 0,
    # keep only exclusions that started before the 66th birthday
    date_start < dob+years(66) ~ 1,
    # account for leap years
    is.na(dob+years(66)) & date_start < dob+days(1)+years(66) ~ 1,
    TRUE ~ 0
  )) %>%
  filter(exclflag == 1) %>%
  distinct(upi, pat_inelig)

optout_summary <- optout %>%
  count(pat_inelig)

optout <- optout %>% select(upi) %>% arrange(upi)

### AAA repaired ---
repaired <- aaa_exclusions %>%
  filter(pat_inelig == "04") %>%
  mutate(exlength = date_end - date_start)

repaired <- repaired %>%
  mutate(exclflag = case_when(
    # remove exclusions that have ended
    !is.na(date_end) ~ 0,
    # keep only exclusions that started before the 66th birthday
    date_start < dob+years(66) ~ 1,
    # account for leap years
    is.na(dob+years(66)) & date_start < dob+days(1)+years(66) ~ 1,
    TRUE ~ 0
  )) %>%
  filter(exclflag == 1) %>%
  distinct(upi, pat_inelig)

repaired_summary <- repaired %>%
  count(pat_inelig)

repaired <- repaired %>% select(upi) %>% arrange(upi)

### Under vascular surveillance ---
vasc_sur <- aaa_exclusions %>%
  filter(pat_inelig == "06") %>%
  mutate(exlength = date_end - date_start)

vasc_sur <- vasc_sur %>%
  mutate(exclflag = case_when(
    # remove exclusions that have ended
    !is.na(date_end) ~ 0,
    # keep only exclusions that started before the 66th birthday
    date_start < dob+years(66) ~ 1,
    # account for leap years
    is.na(dob+years(66)) & date_start < dob+days(1)+years(66) ~ 1,
    TRUE ~ 0
  )) %>%
  filter(exclflag == 1) %>%
  distinct(upi, pat_inelig)

vasc_sur_summary <- vasc_sur %>%
  count(pat_inelig)

vasc_sur <- vasc_sur %>% select(upi) %>% arrange(upi)

### Referred ---
referred <- aaa_exclusions %>%
  filter(pat_inelig == "19") %>%
  mutate(exlength = date_end - date_start)

referred <- referred %>%
  mutate(exclflag = case_when(
    # remove exclusions that have ended
    !is.na(date_end) ~ 0,
    # keep only exclusions that started before the 66th birthday
    date_start < dob+years(66) ~ 1,
    # account for leap years
    is.na(dob+years(66)) & date_start < dob+days(1)+years(66) ~ 1,
    TRUE ~ 0
  )) %>%
  filter(exclflag == 1) %>%
  distinct(upi, pat_inelig)

referred_summary <- referred %>%
  count(pat_inelig)

referred <- referred %>% select(upi) %>% arrange(upi)

### Unfit for Scanning ---
unfit <- aaa_exclusions %>%
  filter(pat_inelig == "18") %>%
  mutate(exlength = date_end - date_start)

unfit <- unfit %>%
  mutate(exclflag = case_when(
    # remove exclusions that have ended
    !is.na(date_end) ~ 0,
    # keep only exclusions that started before the 66th birthday
    date_start < dob+years(66) ~ 1,
    # account for leap years
    is.na(dob+years(66)) & date_start < dob+days(1)+years(66) ~ 1,
    TRUE ~ 0
  )) %>%
  filter(exclflag == 1) %>%
  distinct(upi, pat_inelig)

unfit_summary <- unfit %>%
  count(pat_inelig)

unfit <- unfit %>% select(upi) %>% arrange(upi)

### Other exclusion ---
other <- aaa_exclusions %>%
  filter(pat_inelig %in% c("11","12","13","14","17")) %>%
  mutate(exlength = date_end - date_start,
         exlength = as.numeric(exlength)) %>%
  mutate(exlength_group = case_when(
    between(exlength, 0, 7) ~ 1,
    between(exlength, 8, 30) ~ 2,
    between(exlength, 31, 91) ~ 3,
    between(exlength, 92, 183) ~ 4,
    between(exlength, 184, 365) ~ 5,
    exlength > 365 ~ 6
  ))

other_exlength <- other %>%
  filter(!is.na(exlength_group)) %>%
  count(pat_inelig, exlength_group)

other <- other %>%
  mutate(exclflag = case_when(
    # remove exclusions that have ended
    !is.na(date_end) ~ 0,
    # keep only exclusions that started before the 66th birthday
    date_start < dob+years(66) ~ 1,
    # account for leap years
    is.na(dob+years(66)) & date_start < dob+days(1)+years(66) ~ 1,
    TRUE ~ 0
  )) %>%
  filter(exclflag == 1) %>%
  distinct(upi, pat_inelig)

other_summary <- other %>%
  count(pat_inelig)

other <- other %>% distinct(upi) %>% arrange(upi)

### GANA and temporary residents ---
temp_gana <- aaa_exclusions %>%
  filter(pat_inelig %in% c("25","26")) %>%
  mutate(exlength = date_end - date_start) %>%
  distinct(upi) %>%
  arrange(upi)


### Step 7: Remove exclusions ----

cohort1 <- cohort1 %>%
  left_join(first_offer_first_result, by = "upi") %>%
  mutate(
    inoffertested = if_else(upi %in% first_offer_first_result$upi,1,0),
    intempgana = if_else(upi %in% temp_gana$upi,1,0),
    inexternal = if_else(upi %in% external_only$upi,1,0),
    inonlyhavesurv_record = if_else(upi %in% prior_sur$upi,1,0),
    indeceased = if_else(upi %in% deceased$upi,1,0),
    inprior = if_else(upi %in% prior_scr$upi,1,0),
    inoptedout = if_else(upi %in% optout$upi,1,0),
    inrepair = if_else(upi %in% repaired$upi,1,0),
    inundersurv_vas = if_else(upi %in% vasc_sur$upi,1,0),
    inrefvas = if_else(upi %in% referred$upi,1,0),
    inunfit = if_else(upi %in% unfit$upi,1,0),
    inother = if_else(upi %in% other$upi,1,0)
  ) %>%
  mutate(
    inresult = if_else(!is.na(FT_screen_result), 1, 0),
    inoffer = if_else(!is.na(date_first_offer_sent), 1, 0)
  )
  

cohort1 %>% count(inoffertested, inoptedout)


cohort1 <- cohort1 %>%
  mutate(
    intempgana = if_else(inresult == 1, 0, intempgana),
    inonlyhavesurv_record = if_else(inresult == 1, 0, inonlyhavesurv_record),
    indeceased = if_else(inresult == 1, 0, indeceased),
    inprior = if_else(inresult == 1, 0, inprior),
    inrepair = if_else(inresult == 1, 0, inrepair),
    inundersurv_vas = if_else(inresult == 1, 0, inundersurv_vas),
    inrefvas = if_else(inresult == 1, 0, inrefvas),
    inunfit = if_else(inresult == 1, 0, inunfit),
    inother = if_else(inresult == 1, 0, inother)
  )

cohort1 %>% count(inresult, intempgana)
cohort1 %>% count(inresult, inonlyhavesurv_record)
cohort1 %>% count(inresult, indeceased)
cohort1 %>% count(inresult, inprior)
cohort1 %>% count(inresult, inrepair)
cohort1 %>% count(inresult, inundersurv_vas)
cohort1 %>% count(inresult, inrefvas)
cohort1 %>% count(inresult, inunfit)
cohort1 %>% count(inresult, inother)

# remove exclusions
cohort1 <- cohort1 %>%
  filter(intempgana + inonlyhavesurv_record + indeceased + inprior +
         inrepair + inundersurv_vas + inrefvas + inunfit + inother == 0) %>%
  filter(inexternal != 1)


# Step 8: Write out ----

cohort1 <- cohort1 %>%
  select(upi,
         postcode,
         ca2019,
         simd2020v2_sc_quintile,
         hbres,
         dob_eligibility,
         dob,
         date_first_offer_sent,
         screen_date = FT_screen_date,
         screen_type = FT_screen_type,
         screen_exep = FT_screen_exep,
         screen_result = FT_screen_result,
         aaa_size_group,
         results,
         inoffertested,
         inresult,
         inoffer
  )

write_rds(cohort1, paste0(temp_path, "/1_inviteanduptake_initial.rds"))



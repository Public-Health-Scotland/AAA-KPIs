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
# and attendances for each individual. This can then be used to calculate
# figures for the uptake KPIs
 
# Step 1 : Import packages and filepaths
# Step 2 : Import and trim data
# Step 3 : Create derived variables
# Step 4 : Create a 'first offer sent' file
# Step 5 : Create a 'first screening result' object
# Step 6 : Create a baseline cohort
# Step 7 : Create series of exclusions objects
# Step 8 : Remove exclusions
# Step 9 : Write out


### Step 1 : Import packages and filepaths ----

# This should be the only step which needs edited each time

library(readr)
library(dplyr)
library(tidylog)
library(lubridate)

kpi_month <- "sep22"

cutoff_date <- dmy("31-03-1957")

extract_fpath <- paste0("/PHI_conf/AAA/Topics/Screening/extracts/",
                        "202209/output/aaa_extract_202209.rds")

exclusions_fpath <- paste0("/PHI_conf/AAA/Topics/Screening/extracts/",
                           "202209/output/aaa_exclusions_202209.rds")

output_fpath <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/",
                       "temp/KPIs/KPI1.1 - KPI1.3/")


### Step 2 : Import and trim data ----

aaa_extract <- read_rds(extract_fpath)
aaa_exclusions <- read_rds(extract_fpath)


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

cohort1 <- aaa_extract %>%
  filter(pat_elig != "03")

####### Remove once moved to initial processing scripts ####

### There should be a function for the first variable below so it doesn't
### need to be updated with more recent years

cohort1 <- cohort1 %>%
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
        between(dob, dmy("29-06-1946"), dmy("28-06-1947")) ~ 1,
      TRUE ~ 0
    ),
  over65_onstartdate = case_when(
    hbres == "Ayrshire & Arran" & dob < dmy("01-06-1947") ~ 1,
    hbres == "Borders" & dob < dmy("09-08-1946") ~ 1,
    hbres == "Dumfries & Galloway" & dob < dmy("24-07-1947") ~ 1,
    hbres == "Fife" & dob < dmy("09-01-1947") ~ 1,
    hbres == "Forth Valley" & dob < dmy("18-09-1947") ~ 1,
    hbres == "Grampian" & dob < dmy("03-10-1946") ~ 1,
    hbres == "Greater Glasgow & Clyde" & dob < dmy("06-02-1947") ~ 1,
    hbres == "Highland" & dob < dmy("29-06-1946") ~ 1,
    hbres == "Lanarkshire" & dob < dmy("01-04-1947") ~ 1,
    hbres == "Lothian" & dob < dmy("09-08-1946") ~ 1,
    hbres == "Orkney" & dob < dmy("03-10-1946") ~ 1,
    hbres == "Shetland" & dob < dmy("03-10-1946") ~ 1,
    hbres == "Tayside" & dob < dmy("09-01-1947") ~ 1,
    hbres == "Western Isles" & dob < dmy("29-06-1946") ~ 1,
    TRUE ~ 0
  ),
  dob_eligibility = case_when(
    over65_onstartdate == 1 ~ "Over eligible age cohort - age 66plus on start date",
    age65_onstartdate == 1 ~ "Older cohort - age 65 on start date",
    !is.na(eligibility_period) & age65_onstartdate == 0 ~ eligibility_period
  ))
############ End of removed portion ######

cohort1 <- cohort1 %>%
  filter(screen_type %in% c("01","03",NA))

### Sift through the duplicates
# This has been done in spss by sorting on date_offer_sent and then picking
# whatever record happens to be last. Trying to do it slightly
# more methodically here
# There are some records where chi != upi. If the chi != upi ends up being
# a problem then this will need redone

# Some duplicates are identical for all the relevant variables
cohort1 <- cohort1 %>%
  distinct(upi, postcode, ca2019, simd2020v2_sc_quintile, hbres,
           dob_eligibility, dob, .keep_all = TRUE)


# Create variable called 'keep' to add records to.
# Start with all the non duplicates then bring more records in
cohort1 <- cohort1 %>%
  group_by(upi) %>%
  mutate(keep = if_else(n() == 1, 1, 0)) %>%
  ungroup()

# keep only the last offer
cohort1 <- cohort1 %>%
  group_by(upi) %>%
  mutate(
    keep = case_when(
      keep == 1 ~ 1,
      date_offer_sent == max(date_offer_sent) ~ 1,
      TRUE ~ 0
    )) %>%
  ungroup()

# get rid of records where the upi is accounted for
cohort1 <- cohort1 %>%
  group_by(upi) %>%
  mutate(
    drop = if_else(any(keep == 1) & keep == 0, 1, 0)
  ) %>%
  ungroup() %>%
  filter(drop == 0)

# keep only ones where there is an offer sent
cohort1 <- cohort1 %>%
  group_by(upi) %>%
  mutate(
    keep = case_when(
      keep == 1 ~ 1,
      !is.na(date_offer_sent) ~ 1,
      TRUE ~ 0
    )
  ) %>%
  ungroup()

# get rid of records where the upi is accounted for
cohort1 <- cohort1 %>%
  group_by(upi) %>%
  mutate(
    drop = if_else(any(keep == 1) & keep == 0, 1, 0)
  ) %>%
  ungroup() %>%
  filter(drop == 0)

# sort the rest by if chi == upi
cohort1 <- cohort1 %>%
  mutate(
    keep = case_when(
      keep == 1 ~ 1,
      chi == upi ~ 1,
      TRUE ~ 0
    )
  )

# We now have all the chis
cohort1 <- filter(cohort1, keep == 1)
# couple of pairs with the same date_offer_sent so get rid of the one
# with the mismatching chi
cohort1 <- cohort1 %>%
  group_by(upi) %>%
  mutate(
    keep = case_when(
      n() != 1 & chi != upi ~ 0,
      TRUE ~ 1
    )) %>%
  ungroup()

cohort1 <- filter(cohort1, keep == 1)

cohort1 <- cohort1 %>%
  select(upi, postcode, ca2019, simd2020v2_sc_quintile,
         hbres, dob_eligibility, dob)

# trim to cutoff date
cohort1 <- cohort1 %>%
  filter(dob <= cutoff_date)


# Step 7 : Create series of exclusions objects ----
# some of these have '_summary' objects which were previously written to
# excel. They aren't written out from here currently but can add that step
# in if required.

### Already on surveillance prior to national programme
prior_sur <- aaa_extract %>%
  filter(screen_type %in% c("01", "02"))

# if upi has a screen_type 01 then take that
prior_sur <- prior_sur %>%
  group_by(upi) %>%
  mutate(
    keep = if_else(any(screen_type == "01") & 
                     screen_type == "02", 0, 1)
  ) %>%
  ungroup() %>%
  filter(keep == 1) %>%
  distinct(upi, screen_type)
  
prior_sur <- filter(prior_sur, screen_type == "02")

### Only an external result
external_only <- aaa_extract %>%
  filter(screen_result %in% c("05", "06"))

external_only <- external_only %>%
  distinct(upi, screen_result)

external_only_summary <- external_only %>%
  count(screen_result)

external_only <- select(external_only, upi)

### Deceased
deceased <- aaa_exclusions %>%
  filter(pat_inelig %in% c("15","16"))

# Keep only exclusions that started before their 66th birthday
# (with some trickery to get round leap years)
# Don't keep exclusions that have ended
deceased <- deceased %>%
  mutate(exclflag = case_when(
    !is.na(date_end) ~ 0,
    date_start < dob+years(66) ~ 1,
    is.na(dob+years(66)) & date_start < dob+days(1)+years(66) ~ 1,
    TRUE ~ 0
  )) %>%
  filter(exclflag == 1)

deceased <- deceased %>%
  distinct(upi, pat_inelig)

deceased_summary <- deceased %>%
  count(pat_inelig)

deceased <- deceased %>% select(upi)

### Prior screen exclusion
prior_scr <- aaa_exclusions %>%
  filter(pat_inelig == "21")

# Keep only exclusions that started before their 66th birthday
# (with some trickery to get round leap years)
# Don't keep exclusions that have ended
prior_scr <- prior_scr %>%
  mutate(exclflag = case_when(
    !is.na(date_end) ~ 0,
    date_start < dob+years(66) ~ 1,
    is.na(dob+years(66)) & date_start < dob+days(1)+years(66) ~ 1,
    TRUE ~ 0
  )) %>%
  filter(exclflag == 1)

prior_scr <- prior_scr %>%
  distinct(upi, pat_inelig)

prior_scr_summary <- prior_scr %>%
  count(pat_inelig)

prior_scr <- prior_scr %>% select(upi) %>% arrange(upi)

### Opted out
optout <- aaa_exclusions %>%
  filter(pat_inelig == "01") %>%
  mutate(
    optout_length = date_end - date_start
  )

optout_length_summary <- optout %>%
  filter(!is.na(optout_length)) %>%
  count(optout_length)

# Keep only exclusions that started before their 66th birthday
# (with some trickery to get round leap years)
# Don't keep exclusions that have ended
optout <- optout %>%
  mutate(exclflag = case_when(
    !is.na(date_end) ~ 0,
    date_start < dob+years(66) ~ 1,
    is.na(dob+years(66)) & date_start < dob+days(1)+years(66) ~ 1,
    TRUE ~ 0
  )) %>%
  filter(exclflag == 1)

optout <- optout %>%
  distinct(upi, pat_inelig)

optout_summary <- optout %>%
  count(pat_inelig)

optout <- optout %>% select(upi) %>% arrange(upi)

### AAA repaired
repaired <- aaa_exclusions %>%
  filter(pat_inelig == "04") %>%
  mutate(exlength = date_end - date_start)

repaired <- repaired %>%
  mutate(exclflag = case_when(
    !is.na(date_end) ~ 0,
    date_start < dob+years(66) ~ 1,
    is.na(dob+years(66)) & date_start < dob+days(1)+years(66) ~ 1,
    TRUE ~ 0
  )) %>%
  filter(exclflag == 1)

repaired <- repaired %>%
  distinct(upi, pat_inelig)

repaired_summary <- repaired %>%
  count(pat_inelig)

repaired <- repaired %>% select(upi) %>% arrange(upi)

### Under vascular surveillance
vasc_sur <- aaa_exclusions %>%
  filter(pat_inelig == "06") %>%
  mutate(exlength = date_end - date_start)

vasc_sur <- vasc_sur %>%
  mutate(exclflag = case_when(
    !is.na(date_end) ~ 0,
    date_start < dob+years(66) ~ 1,
    is.na(dob+years(66)) & date_start < dob+days(1)+years(66) ~ 1,
    TRUE ~ 0
  )) %>%
  filter(exclflag == 1)

vasc_sur <- vasc_sur %>%
  distinct(upi, pat_inelig)

vasc_sur_summary <- vasc_sur %>%
  count(pat_inelig)

vasc_sur <- vasc_sur %>% select(upi) %>% arrange(upi)

### Referred
referred <- aaa_exclusions %>%
  filter(pat_inelig == "19") %>%
  mutate(exlength = date_end - date_start)

referred <- referred %>%
  mutate(exclflag = case_when(
    !is.na(date_end) ~ 0,
    date_start < dob+years(66) ~ 1,
    is.na(dob+years(66)) & date_start < dob+days(1)+years(66) ~ 1,
    TRUE ~ 0
  )) %>%
  filter(exclflag == 1)

referred <- referred %>%
  distinct(upi, pat_inelig)

referred_summary <- referred %>%
  count(pat_inelig)

referred <- referred %>% select(upi) %>% arrange(upi)

### Unfit
unfit <- aaa_exclusions %>%
  filter(pat_inelig == "18") %>%
  mutate(exlength = date_end - date_start)

unfit <- unfit %>%
  mutate(exclflag = case_when(
    !is.na(date_end) ~ 0,
    date_start < dob+years(66) ~ 1,
    is.na(dob+years(66)) & date_start < dob+days(1)+years(66) ~ 1,
    TRUE ~ 0
  )) %>%
  filter(exclflag == 1)

unfit <- unfit %>%
  distinct(upi, pat_inelig)

unfit_summary <- unfit %>%
  count(pat_inelig)

unfit <- unfit %>% select(upi) %>% arrange(upi)

### Other exclusion
other <- aaa_exclusions %>%
  filter(pat_inelig %in% c("11","12","13","14","17")) %>%
  mutate(exlength = date_end - date_start) %>%
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
    !is.na(date_end) ~ 0,
    date_start < dob+years(66) ~ 1,
    is.na(dob+years(66)) & date_start < dob+days(1)+years(66) ~ 1,
    TRUE ~ 0
  )) %>%
  filter(exclflag == 1)

other <- other %>%
  distinct(upi, pat_inelig)

other_summary <- other %>%
  count(pat_inelig)

other <- other %>% distinct(upi) %>% arrange(upi)

### GANA and temporary residents

temp_gana <- aaa_exclusions %>%
  filter(pat_inelig %in% c("25","26")) %>%
  mutate(exlength = date_end - date_start)

temp_gana <- temp_gana %>%
  distinct(upi) %>%
  arrange(upi)


### Step 8 : Remove exclusions ----

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
         inrepair + inundersurv_vas + inrefvas + inunfit + inother == 0)

cohort1 <- cohort1 %>%
  filter(inexternal != 1)

# Step 9 : Write out ----

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
         isd_aaa_size_group,
         results,
         inoffertested,
         inresult,
         inoffer
  )

write_rds(cohort1, paste0(output_fpath, "inviteanduptake_initial.rds"))




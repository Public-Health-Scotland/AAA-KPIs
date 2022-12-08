#~~~~~~~~~~~~~~~~~~~~~~~~~
# 2_kpi_11_12b_13b_invite_uptake.R
# Angus Morton
# 17/11/2022
#
# Produce KPIs 1.1, 1.2b and 1.3b
#
# Written on RServer (R Version 3.6.1)
#~~~~~~~~~~~~~~~~~~~~~~~~~

# KPI 1.1  : Percentage of eligible population who are sent an initial
#            offer to screening before age 66

# KPI 1.2b : Percentage of men offered screening before age 66 who are
#            tested before age 66 and 3 months 

# KPI 1.3b : Percentage of men offered screening before age 66 who are
#            tested before age 66 and 3 months by Scottish Index of
#            Multiple Deprivation (SIMD) quintile



# Step 1 : Import packages and filepaths
# Step 2 : Import data
# Step 3 : 
# Step x : Write out


### Step 1 : Import packages and filepaths ----

library(readr)
library(dplyr)
library(tidylog)
library(lubridate)

# This should be the only step which needs edited each time

invite_uptake_fpath <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/",
                              "temp/KPIs/KPI1.1 - KPI1.3/",
                              "inviteanduptake_initial.rds")

year1_start <- dmy("01-04-1955")
year1_end <- dmy("31-03-1956")

year2_start <- dmy("01-04-1956")
year2_end <- dmy("31-03-1957")

### Step 2 : Import data ----

invite_uptake <- read_rds(invite_uptake_fpath)


### Step 3 : Create derived variables ----

## for KPI 1.1
# age_at_screen (they want age in months because 66 and 3 months is the
# key age. Don't really like this as ambiguous what a month is)
invite_uptake <- invite_uptake %>%
  mutate(age_screen = interval(dob, screen_date) %/% months(1))

# age_at_offer (they want this in years because 66 is the key age)
invite_uptake <- invite_uptake %>%
  mutate(age_offer = interval(dob, date_first_offer_sent) %/% years(1))


# assign year eligible cohorts
invite_uptake <- invite_uptake %>%
  mutate(cohort_year1 = case_when(
    between(dob, year1_start, year1_end) ~ 1,
    TRUE ~ as.numeric(NA)
  ),
  
  cohort_year2 = case_when(
    between(dob, year2_start, year2_end) ~ 1,
    TRUE ~ as.numeric(NA)
  )
  )

# assign year offer cohorts (offered before 66)
invite_uptake <- invite_uptake %>%
  mutate(offer_year1 = case_when(
    cohort_year1 == 1 & inoffer == 1 & age_offer < 66 ~ 1,
    TRUE ~ as.numeric(NA)
  ),
  
  offer_year2 = case_when(
    cohort_year2 == 1 & inoffer == 1 & age_offer < 66 ~ 1,
    TRUE ~ as.numeric(NA)
  ))

# secondary numerator. For those offered an appointment at any point
invite_uptake <- invite_uptake %>%
  mutate(offer_any_year1 = case_when(
    cohort_year1 == 1 & inoffer == 1 ~ 1,
    TRUE ~ as.numeric(NA)
  ),
  
  offer_any_year2 = case_when(
    cohort_year2 == 1 & inoffer == 1 ~ 1,
    TRUE ~ as.numeric(NA)
  ))

## for KPI 1.2b
# assign year offer cohorts (already have this?)
# offered before age 66 (done?)
# tested before 795 months and offered before 66
invite_uptake <- invite_uptake %>%
  mutate(tested_year1 = case_when(
    cohort_year1 == 1 & inoffer == 1 ~ 1,
    TRUE ~ as.numeric(NA)
  ),
  
  tested_year2 = case_when(
    cohort_year2 == 1 & inoffer == 1 ~ 1,
    TRUE ~ as.numeric(NA)
  ))

### KPI 1.1 ----

# trim date
invite_uptake_1_1 <- invite_uptake %>%
  filter(dob >= dmy("01-04-1948"))

breakdown_1_1 <- invite_uptake_1_1 %>%
  group_by(hbres) %>%
  summarise(
    across(cohort_year1:tested_year2, sum, na.rm = TRUE)
  ) %>%
  ungroup()

scotland_1_1 <- breakdown_1_1 %>%
  summarise(
    across(cohort_year1:tested_year2, sum, na.rm = TRUE)
  ) %>%
  mutate(hbres = "Scotland")

breakdown_1_1 <- bind_rows(breakdown_1_1, scotland_1_1)



# create individual board breakdown
# join on scotland data
# create percentages
# create output tables?

### KPI 1.2b ----

### KPI 1.3b ----





#~~~~~~~~~~~~~~~~~~~~~~~~~
# 2_kpi_1_1-1_3_uptake_coverage.R
# Angus Morton
# 17/11/2022
#
# Produce KPIs 1.1, 1.2a, 1.2b, 1.3a and 1.3b
#
# Written on RServer (R Version 3.6.1)
#~~~~~~~~~~~~~~~~~~~~~~~~~

# KPI 1.1  : Percentage of eligible population who are sent an initial
#            offer to screening before age 66

# KPI 1.2a : Percentage eligible men tested before the age of 66 and
#            3 months

# KPI 1.2b : Percentage of men offered screening before age 66 who are
#            tested before age 66 and 3 months

# KPI 1.3a : Percentage eligible men tested before the age of 66 and
#            3 months by Scottish Index of Multiple Deprivation (SIMD)
#            quintile

# KPI 1.3b : Percentage of men offered screening before age 66 who are
#            tested before age 66 and 3 months by Scottish Index of
#            Multiple Deprivation (SIMD) quintile



# Step 1 : Import packages and filepaths
# Step 2 : Import data
# Step 3 : Create derived variables
# Step 4 : Create summary tables for each kpi
# Step 5 : Write out


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

# Functions

# pass a dataframe and 
make_summary <- function(df, ...) {
  
  
  summary
}


### Step 2 : Import data ----

invite_uptake <- read_rds(invite_uptake_fpath)


### Step 3 : Create derived variables ----

## for KPI 1.1
# age_at_screen (in months because 66 and 3 months is the key age)
invite_uptake <- invite_uptake %>%
  mutate(age_screen = interval(dob, screen_date) %/% months(1))

# age_at_offer (in years because 66 is the key age)
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
  ),
  
  offer_not_assigned = if_else(offer_year1 != 1 &
                                 offer_year2 != 1, 1, 0)
  )

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
    !is.na(screen_result) == 1 & age_screen < 795 &
      age_offer < 66 & cohort_year1 == 1 ~ 1,
    TRUE ~ as.numeric(NA)
  ),
  
  tested_year2 = case_when(
    !is.na(screen_result) == 1 & age_screen < 795 &
      age_offer < 66 & cohort_year2 == 1 ~ 1,
    TRUE ~ as.numeric(NA)
  ),
  
  tested_not_assigned = if_else(tested_year1 != 1 &
                                 tested_year2 != 1, 1, 0)
  )


## for KPI 1.2a
# denominator : cohort who are eligible to be screened (already got)

# numerator : if man has been tested and age screen < 795 months
invite_uptake <- invite_uptake %>%
  mutate(tested2_year1 = case_when(
    !is.na(screen_result) & age_screen < 795 &
      cohort_year1 == 1 ~ 1,
    TRUE ~ as.numeric(NA)
  ),
  
  tested2_year2 = case_when(
    !is.na(screen_result) & age_screen < 795 &
      cohort_year2 == 1 ~ 1,
    TRUE ~ as.numeric(NA)
  ),
  
  tested2_not_assigned = if_else(!is.na(tested2_year1) |
                                   !is.na(tested2_year2) , as.numeric(NA), 1)
  )

# additional : if man has been tested at any time
invite_uptake <- invite_uptake %>%
  mutate(tested2_any_year1 = case_when(
    !is.na(screen_result) & cohort_year1 == 1 ~ 1,
    TRUE ~ as.numeric(NA)
  ),
  
  tested2_any_year2 = case_when(
    !is.na(screen_result) & cohort_year2 == 1 ~ 1,
    TRUE ~ as.numeric(NA)
  ),
  
  tested2_any_not_assigned = if_else(!is.na(tested2_any_year1) |
                                       !is.na(tested2_any_year2) , as.numeric(NA), 1)
  )

# additional : if man has been tested before sep 1 of extract year
#           don't have
# This one seems kind of weird because it overwrites the previous one.
# Going to leave it out for now but might need to come back.
# Actually doesn't seem like it overwrites anything so could just come out?


### Step 4 : Create summary tables for each kpi ----

### KPI 1.1 ----

# trim date
invite_uptake_1_1 <- invite_uptake %>%
  filter(dob >= dmy("01-04-1948"))

# create health board breakdown
breakdown_1_1 <- invite_uptake_1_1 %>%
  group_by(hbres) %>%
  summarise(
    across(cohort_year1:tested_not_assigned, sum, na.rm = TRUE)
  ) %>%
  ungroup()

scotland_1_1 <- breakdown_1_1 %>%
  summarise(
    across(cohort_year1:tested_not_assigned, sum, na.rm = TRUE)
  ) %>%
  mutate(hbres = "Scotland")

breakdown_1_1 <- bind_rows(breakdown_1_1, scotland_1_1)

# create percentages
breakdown_1_1 <- breakdown_1_1 %>%
  mutate(
    percent_year1 = (offer_year1/cohort_year1)*100,
    percent_year2 = (offer_year2/cohort_year2)*100,
    
    percent_any_year1 = (offer_any_year1/cohort_year1)*100,
    percent_any_year2 = (offer_any_year2/cohort_year2)*100
  )

# Output tables (tidy?)
output_a_1_1 <- breakdown_1_1 %>%
  select(hbres, cohort_year1, offer_year1, percent_year1,
         cohort_year2, offer_year2, percent_year2)

output_b_1_1 <- breakdown_1_1 %>%
  select(hbres, cohort_year1, offer_any_year1, percent_any_year1,
         cohort_year2, offer_any_year2, percent_any_year2)


### KPI 1.2b ----

breakdown_1_2 <- invite_uptake %>%
  group_by(hbres) %>%
  summarise(
    across(cohort_year1:tested_not_assigned, sum, na.rm = TRUE)
  ) %>%
  ungroup()

scotland_1_2 <- breakdown_1_2 %>%
  summarise(
    across(cohort_year1:tested_not_assigned, sum, na.rm = TRUE)
  ) %>%
  mutate(hbres = "Scotland")

breakdown_1_2 <- bind_rows(breakdown_1_2, scotland_1_2)

# create percentages
breakdown_1_2 <- breakdown_1_2 %>%
  mutate(
    percent_year1 = (tested_year1/offer_year1)*100,
    percent_year2 = (tested_year2/offer_year2)*100,
    
    p_not_assigned = (tested_not_assigned/offer_not_assigned)*100
  )

# Output tables (tidy?)
output_1_2 <- breakdown_1_2 %>%
  select(hbres, offer_year1, tested_year1, percent_year1,
         offer_year2, tested_year2, percent_year2)


### KPI 1.3b ----

breakdown_1_3 <- invite_uptake %>%
  group_by(hbres, simd2020v2_sc_quintile) %>%
  summarise(
    across(cohort_year1:tested_not_assigned, sum, na.rm = TRUE)
  ) %>%
  ungroup()

breakdown_1_3_tot <- invite_uptake %>%
  group_by(hbres) %>%
  summarise(
    across(cohort_year1:tested_not_assigned, sum, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(simd2020v2_sc_quintile = 0)

scotland_1_3 <- breakdown_1_3 %>%
  group_by(simd2020v2_sc_quintile) %>%
  summarise(
    across(cohort_year1:tested_not_assigned, sum, na.rm = TRUE)
  ) %>%
  mutate(hbres = "Scotland")

scotland_1_3_tot <- breakdown_1_3 %>%
  summarise(
    across(cohort_year1:tested_not_assigned, sum, na.rm = TRUE)
  ) %>%
  mutate(hbres = "Scotland") %>%
  mutate(simd2020v2_sc_quintile = 0)

breakdown_1_3 <- bind_rows(breakdown_1_3, breakdown_1_3_tot) %>%
  arrange(hbres)

scotland_1_3 <- bind_rows(scotland_1_3, scotland_1_3_tot)

breakdown_1_3 <- bind_rows(scotland_1_3, breakdown_1_3) %>%
  select(hbres, everything())

# create percentages
breakdown_1_3 <- breakdown_1_3 %>%
  mutate(
    percent_year1 = (tested_year1/offer_year1)*100,
    percent_year2 = (tested_year2/offer_year2)*100,
    
    p_not_assigned = (tested_not_assigned/offer_not_assigned)*100
  )

# Output tables (tidy?)
output_1_3 <- breakdown_1_3 %>%
  select(hbres, simd2020v2_sc_quintile, offer_year1, tested_year1,
         percent_year1, offer_year2, tested_year2, percent_year2) %>%
  mutate_all(~ifelse(is.nan(.), NA, .))



### KPI 1.2a ----

breakdown_1_2a <- invite_uptake %>%
  group_by(hbres) %>%
  summarise(
    across(cohort_year1:tested2_any_not_assigned, sum, na.rm = TRUE)
  ) %>%
  ungroup()


scotland_1_2a <- breakdown_1_2a %>%
  summarise(
    across(cohort_year1:tested2_any_not_assigned, sum, na.rm = TRUE)
  ) %>%
  mutate(hbres = "Scotland")

breakdown_1_2a <- bind_rows(breakdown_1_2a, scotland_1_2a)

# create percentages
breakdown_1_2a <- breakdown_1_2a %>%
  mutate(
    percent_year1 = (tested2_year1/cohort_year1)*100,
    percent_year2 = (tested2_year2/cohort_year2)*100,
    
    percent_any_year1 = (tested2_any_year1/cohort_year1)*100,
    percent_any_year2 = (tested2_any_year2/cohort_year2)*100
  )

# Output tables (tidy?)
output_a_1_2a <- breakdown_1_2a %>%
  select(hbres, cohort_year1, tested2_year1, percent_year1,
         cohort_year2, tested2_year2, percent_year2)
# Very slight differences presumably to do with slight differences in
# date/age calculations

output_b_1_2a <- breakdown_1_2a %>%
  select(hbres, cohort_year1, tested2_any_year1, percent_any_year1,
         cohort_year2, tested2_any_year2, percent_any_year2)
# Yeah pretty sure it's a date thing since these numbers match
# the previous output

### KPI 1.3a ----

## Coverage by simd
breakdown_1_3a <- invite_uptake %>%
  group_by(hbres, simd2020v2_sc_quintile) %>%
  summarise(
    across(cohort_year1:tested2_any_not_assigned, sum, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(simd2020v2_sc_quintile = as.character(simd2020v2_sc_quintile))


scotland_1_3a <- breakdown_1_3a %>%
  group_by(simd2020v2_sc_quintile) %>%
  summarise(
    across(cohort_year1:tested2_any_not_assigned, sum, na.rm = TRUE)
  ) %>%
  mutate(hbres = "Scotland")%>%
  mutate(simd2020v2_sc_quintile = as.character(simd2020v2_sc_quintile))


tot_1_3a <- breakdown_1_2a %>%
  mutate(simd2020v2_sc_quintile = "Total")

# bind together including non simd totals from previous kpi
breakdown_1_3a <- bind_rows(breakdown_1_3a, scotland_1_3a, tot_1_3a)

# create percentages
breakdown_1_3a <- breakdown_1_3a %>%
  mutate(
    percent_year1 = (tested2_year1/cohort_year1)*100,
    percent_year2 = (tested2_year2/cohort_year2)*100,
    
    percent_any_year1 = (tested2_any_year1/cohort_year1)*100,
    percent_any_year2 = (tested2_any_year2/cohort_year2)*100
  )

# Output tables (tidy?)
output_a_1_3a <- breakdown_1_3a %>%
  select(hbres, simd2020v2_sc_quintile, cohort_year1, tested2_year1, percent_year1,
         cohort_year2, tested2_year2, percent_year2) %>%
  arrange(factor(hbres, levels = "Scotland"), hbres)
# Slight differences due to newer simd version

output_b_1_3a <- breakdown_1_3a %>%
  select(hbres, simd2020v2_sc_quintile, cohort_year1, tested2_any_year1, percent_any_year1,
         cohort_year2, tested2_any_year2, percent_any_year2) %>%
  arrange(factor(hbres, levels = "Scotland"), hbres)
# again slight differences



#~~~~~~~~~~~~~~~~~~~~~~~~~
# 2_2_kpi_1_1-1_3_uptake_coverage.R
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
# Step 4 : Save out basefiles for other scripts
# Step 5 : Create summary tables for each kpi
# Step 6 : Write out


### Step 1 : Import packages and filepaths ----

library(readr)
library(dplyr)
library(lubridate)
library(phsmethods)
library(tidylog)

rm(list = ls())
gc()


source(here::here("code/0_housekeeping.R"))

rm(exclusions_path, extract_path, cutoff_date)


### Step 2 : Import data ----

invite_uptake <- read_rds(paste0(temp_path, "/1_inviteanduptake_initial.rds"))


### Step 3 : Create derived variables ----

## KPI 1.1 ---
invite_uptake <- invite_uptake %>%
  # age_at_screen (in months because 66 and 3 months is the key age)
  mutate(age_screen = age_calculate(dob, screen_date, units = "months")) %>%
  # age_at_offer (in years because 66 is the key age)
  mutate(age_offer = interval(dob, date_first_offer_sent) %/% years(1))


# assign year eligible cohorts
invite_uptake <- invite_uptake %>%
  mutate(cohort_year1 = case_when(between(dob, year1_start, year1_end) ~ 1,
                                  TRUE ~ as.numeric(NA)
  ),
  cohort_year2 = case_when(between(dob, year2_start, year2_end) ~ 1,
                           TRUE ~ as.numeric(NA)
  ))

# assign year offer cohorts (offered before age 66)
invite_uptake <- invite_uptake %>%
  mutate(offer_year1 = case_when(cohort_year1 == 1 & inoffer == 1 & 
                                   age_offer < 66 ~ 1,
                                 TRUE ~ as.numeric(NA)
  ),
  offer_year2 = case_when(cohort_year2 == 1 & inoffer == 1 & 
                            age_offer < 66 ~ 1,
                          TRUE ~ as.numeric(NA)
  ),
  offer_not_assigned = if_else(offer_year1 != 1 &
                                 offer_year2 != 1, 1, 0)
  )

# secondary numerator: those offered an appointment at any point
invite_uptake <- invite_uptake %>%
  mutate(offer_any_year1 = case_when(cohort_year1 == 1 & inoffer == 1 ~ 1,
                                     TRUE ~ as.numeric(NA)
  ),
  offer_any_year2 = case_when(cohort_year2 == 1 & inoffer == 1 ~ 1,
                              TRUE ~ as.numeric(NA)
  ))


## KPI 1.2b ---
# tested before 795 months and offered before age 66
invite_uptake <- invite_uptake %>%
  mutate(tested_year1 = case_when(!is.na(screen_result) == 1 & age_screen < 795 &
                                    age_offer < 66 & cohort_year1 == 1 ~ 1,
                                  TRUE ~ as.numeric(NA)
  ),
  tested_year2 = case_when(!is.na(screen_result) == 1 & age_screen < 795 &
                             age_offer < 66 & cohort_year2 == 1 ~ 1,
                           TRUE ~ as.numeric(NA)
  ),
  tested_not_assigned = if_else(tested_year1 != 1 &
                                  tested_year2 != 1, 1, 0)
  )


## KPI 1.2a ---
# numerator : if patient has been tested and age_screen < 795 months
invite_uptake <- invite_uptake %>%
  mutate(tested2_year1 = case_when(!is.na(screen_result) & age_screen < 795 &
                                     cohort_year1 == 1 ~ 1,
                                   TRUE ~ as.numeric(NA)
  ),
  tested2_year2 = case_when(!is.na(screen_result) & age_screen < 795 &
                              cohort_year2 == 1 ~ 1,
                            TRUE ~ as.numeric(NA)
  ),
  tested2_not_assigned = if_else(!is.na(tested2_year1) |
                                   !is.na(tested2_year2) , as.numeric(NA), 1)
  )

# additional : if patient has been tested at any time
invite_uptake <- invite_uptake %>%
  mutate(tested2_any_year1 = case_when(!is.na(screen_result) & 
                                         cohort_year1 == 1 ~ 1,
                                       TRUE ~ as.numeric(NA)
  ),
  tested2_any_year2 = case_when(!is.na(screen_result) & 
                                  cohort_year2 == 1 ~ 1,
                                TRUE ~ as.numeric(NA)
  ),
  tested2_any_not_assigned = if_else(!is.na(tested2_any_year1) |
                                       !is.na(tested2_any_year2) , 
                                     as.numeric(NA), 1)
  )

# additional : if patient has been tested before Sep 1 of extract year
#           don't have
# This one seems kind of weird because it overwrites the previous one.
# Going to leave it out for now but might need to come back.
# Actually doesn't seem like it overwrites anything so could just come out?


### Step 4 : Save out basefiles for other scripts ----
# These files are the same. I think we only need one in future but
# subsequent scripts currently need both names

#write_rds(invite_uptake, paste0(output_fpath, "inviteanduptake_initial.rds"))
write_rds(invite_uptake, paste0(temp_path, "/1_coverage_basefile.rds"))


### Step 5 : Summary tables ----
### KPI 1.1 ----
# trim date (select men born from 1 April 1948)
# keeps data for eligible cohorts for men turning age 66 from 2014/15
invite_uptake_1_1 <- invite_uptake %>%
  filter(dob >= dmy("01-04-1948"))

# create health board breakdown
breakdown_1_1 <- invite_uptake_1_1 %>%
  group_by(hbres) %>%
  summarise(across(cohort_year1:tested_not_assigned, sum, na.rm = TRUE)
  ) %>%
  ungroup()

scotland_1_1 <- breakdown_1_1 %>%
  summarise(
    across(cohort_year1:tested_not_assigned, sum, na.rm = TRUE)
  ) %>%
  mutate(hbres = "Scotland")

breakdown_1_1 <- bind_rows(scotland_1_1, breakdown_1_1)

# create percentages
breakdown_1_1 <- breakdown_1_1 %>%
  mutate(
    percent_year1 = (offer_year1/cohort_year1)*100,
    percent_year2 = (offer_year2/cohort_year2)*100,
    
    percent_any_year1 = (offer_any_year1/cohort_year1)*100,
    percent_any_year2 = (offer_any_year2/cohort_year2)*100
  )

# Output tables
output_1_1 <- breakdown_1_1 %>%
  select(hbres, cohort_year1, offer_year1, percent_year1, offer_any_year1, 
         percent_any_year1, cohort_year2, offer_year2, percent_year2,
         offer_any_year2, percent_any_year2) #These last 2 only used in fall MEG

# Save
write_rds(output_1_1, paste0(temp_path, "/KPI_1_1.rds"))

rm(invite_uptake_1_1, scotland_1_1, breakdown_1_1)


### KPI 1.2b ----
breakdown_1_2b <- invite_uptake %>%
  group_by(hbres) %>%
  summarise(
    across(cohort_year1:tested_not_assigned, sum, na.rm = TRUE)
  ) %>%
  ungroup()

scotland_1_2b <- breakdown_1_2b %>%
  summarise(
    across(cohort_year1:tested_not_assigned, sum, na.rm = TRUE)
  ) %>%
  mutate(hbres = "Scotland", .before = cohort_year1)

breakdown_1_2b <- bind_rows(scotland_1_2b, breakdown_1_2b)

# create percentages
breakdown_1_2b <- breakdown_1_2b %>%
  mutate(
    percent_year1 = (tested_year1/offer_year1)*100,
    percent_year2 = (tested_year2/offer_year2)*100,
    
    p_not_assigned = (tested_not_assigned/offer_not_assigned)*100
  )

# Output tables
output_1_2b <- breakdown_1_2b %>%
  select(hbres, offer_year1, tested_year1, percent_year1,
         offer_year2, tested_year2, percent_year2)

# Save
write_rds(output_1_2b, paste0(temp_path, "/KPI_1_2b.rds"))

rm(scotland_1_2b, breakdown_1_2b)


### KPI 1.3b ----
breakdown_1_3b <- invite_uptake %>%
  group_by(hbres, simd2020v2_sc_quintile) %>%
  summarise(
    across(cohort_year1:tested_not_assigned, sum, na.rm = TRUE)
  ) %>%
  ungroup()

breakdown_1_3b_tot <- invite_uptake %>%
  group_by(hbres) %>%
  summarise(
    across(cohort_year1:tested_not_assigned, sum, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(simd2020v2_sc_quintile = 0, .after = hbres)

scotland_1_3b <- breakdown_1_3b %>%
  group_by(simd2020v2_sc_quintile) %>%
  summarise(
    across(cohort_year1:tested_not_assigned, sum, na.rm = TRUE)
  ) %>%
  mutate(hbres = "Scotland", .before = simd2020v2_sc_quintile)

scotland_1_3b_tot <- breakdown_1_3b %>%
  summarise(
    across(cohort_year1:tested_not_assigned, sum, na.rm = TRUE)
  ) %>%
  mutate(hbres = "Scotland", .before = cohort_year1) %>%
  mutate(simd2020v2_sc_quintile = 0, .after = hbres)

breakdown_1_3b <- bind_rows(breakdown_1_3b_tot, breakdown_1_3b) %>%
  arrange(hbres)

scotland_1_3b <- bind_rows(scotland_1_3b_tot, scotland_1_3b)

breakdown_1_3b <- bind_rows(scotland_1_3b, breakdown_1_3b) %>%
  select(hbres, everything())

# create percentages
breakdown_1_3b <- breakdown_1_3b %>%
  mutate(
    percent_year1 = (tested_year1/offer_year1)*100,
    percent_year2 = (tested_year2/offer_year2)*100,
    
    p_not_assigned = (tested_not_assigned/offer_not_assigned)*100
  )

# Output tables
output_1_3b <- breakdown_1_3b %>%
  select(hbres, simd2020v2_sc_quintile, offer_year1, tested_year1,
         percent_year1, offer_year2, tested_year2, percent_year2) %>%
  mutate_all(~ifelse(is.nan(.), NA, .))

# Save
write_rds(output_1_3b, paste0(temp_path, "/KPI_1_3b.rds"))

rm(scotland_1_3b, scotland_1_3b_tot, breakdown_1_3b, breakdown_1_3b_tot)


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
  mutate(hbres = "Scotland", .before = cohort_year1)

breakdown_1_2a <- bind_rows(scotland_1_2a, breakdown_1_2a)

# create percentages
breakdown_1_2a <- breakdown_1_2a %>%
  mutate(
    percent_year1 = (tested2_year1/cohort_year1)*100,
    percent_year2 = (tested2_year2/cohort_year2)*100,
    
    percent_any_year1 = (tested2_any_year1/cohort_year1)*100,
    percent_any_year2 = (tested2_any_year2/cohort_year2)*100
  )

# Output tables
output_1_2a <- breakdown_1_2a %>%
  select(hbres, cohort_year1, tested2_year1, percent_year1, tested2_any_year1, 
         percent_any_year1, cohort_year2, tested2_year2, percent_year2, 
         tested2_any_year2, percent_any_year2) #These last 2 only used in fall MEG

# Save
write_rds(output_1_2a, paste0(temp_path, "/KPI_1_2a.rds"))

rm(scotland_1_2a)


### KPI 1.3a ----
## Coverage by simd
####
## Note that data going into KPI 1.3a Additional tab on MEG report is by 
## HB-level SIMD, so need to create new variable
## MOVE THIS TO PREVIOUS SCRIPT!!!
hb_simd <- read_rds(paste0("/conf/linkage/output/lookups/Unicode/Deprivation",
                  "/postcode_2022_2_simd2020v2.rds")) |>
  select(pc8, simd2020v2_hb2019_quintile)

test <- left_join(invite_uptake, hb_simd, by = c("postcode" = "pc8")) |>
  relocate(simd2020v2_hb2019_quintile, .before = hbres)
####

breakdown_1_3a <- test |> #invite_uptake %>%
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
  mutate(hbres = "Scotland", .before = simd2020v2_sc_quintile)%>%
  mutate(simd2020v2_sc_quintile = as.character(simd2020v2_sc_quintile))

tot_1_3a <- breakdown_1_2a %>%
  mutate(simd2020v2_sc_quintile = "Total", .after = hbres)

# bind together including non-simd totals from previous kpi
breakdown_1_3a <- bind_rows(breakdown_1_3a, scotland_1_3a, tot_1_3a)

# create percentages
breakdown_1_3a <- breakdown_1_3a %>%
  mutate(
    percent_year1 = (tested2_year1/cohort_year1)*100,
    percent_year2 = (tested2_year2/cohort_year2)*100,
    
    percent_any_year1 = (tested2_any_year1/cohort_year1)*100,
    percent_any_year2 = (tested2_any_year2/cohort_year2)*100
  )

# Output tables
output_a_1_3a <- breakdown_1_3a %>%
  select(hbres, simd2020v2_sc_quintile, cohort_year1, tested2_year1, 
         percent_year1, cohort_year2, tested2_year2, percent_year2) %>%
  arrange(factor(hbres, levels = "Scotland"), hbres)
# Slight differences due to newer simd version

output_b_1_3a <- breakdown_1_3a %>%
  select(hbres, simd2020v2_sc_quintile, cohort_year1, tested2_any_year1, 
         percent_any_year1,
         cohort_year2, tested2_any_year2, percent_any_year2) %>%
  arrange(factor(hbres, levels = "Scotland"), hbres)
# again slight differences

# Save
write_rds(output_1_3a, paste0(temp_path, "/KPI_1_3a.rds"))

rm(scotland_1_2a)


#~~~~~~~~~~~~~~~~~~~~~~~~~
# 02_2_kpi_1_1-1_3_uptake_coverage.R
# Angus Morton
# 17/11/2022
#
# Produce KPIs 1.1, 1.2a, 1.2b, 1.3a, 1.3a HB-level, 1.3b and 1.3b HB-level
#
# Written on RServer (R Version 3.6.1)
# Revised/Run on Posit PWB (R version 4.1.2)
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
# Step 5 : Create summaries for each kpi
# Step 6 : Add in historical data and save out all files


### Step 1: Import packages and filepaths ----
library(readr)
library(dplyr)
library(lubridate)
library(phsmethods)
library(stringr)
library(forcats)
library(tidylog)
library(phsaaa) # to install: devtools::install_github("Public-Health-Scotland/phsaaa")

rm(list = ls())
gc()


source(here::here("code/00_housekeeping.R"))

rm (exclusions_path, extract_path, output_path, fy_tibble, qpmg_month,
    cut_off_date, cutoff_date, end_current, end_date, start_date, extract_date)

# SIMD levels
simd_level <- tibble(simd = c("Total", "1","2","3", "4", "5", "Unknown"))

### Step 2: Import data ----
invite_uptake <- read_rds(paste0(temp_path, "/1_1_invite_uptake_initial.rds"))

pc_simd <- read_rds(simd_path) |>
  select(pc8, simd2020v2_hb2019_quintile)


### Step 3: Create derived variables ----
## KPI 1.1 ----
## Percentage of eligible population who are sent an initial offer to 
## screening before age 66
invite_uptake <- invite_uptake %>%
  # calculate age at screening (in months as 66 and 3 months is the key age)
  mutate(age_screen = age_calculate(dob, screen_date, units = "months")) %>%
  # calculate age at offer (in years as 66 is the key age)
  mutate(age_offer = interval(dob, date_first_offer_sent) %/% years(1))

# Denominator: eligible population for current analysis year and current active year
invite_uptake <- invite_uptake %>%
  mutate(cohort_year1 = case_when(between(dob, dmy(year1_start), dmy(year1_end)) ~ 1,
                                  TRUE ~ as.numeric(NA)),
         cohort_year2 = case_when(between(dob, dmy(year2_start), dmy(year2_end)) ~ 1,
                                  TRUE ~ as.numeric(NA)))

# Numerator: eligible individuals sent initial offer to screening during current 
# analysis year and current active year (offered before age 66)
invite_uptake <- invite_uptake %>%
  mutate(offer_year1 = case_when(cohort_year1 == 1 & inoffer == 1 & 
                                   age_offer < 66 ~ 1,
                                 TRUE ~ as.numeric(NA)),
         offer_year2 = case_when(cohort_year2 == 1 & inoffer == 1 & 
                                   age_offer < 66 ~ 1,
                                 TRUE ~ as.numeric(NA)),
         offer_not_assigned = if_else(offer_year1 != 1 &
                                        offer_year2 != 1, 1, 0)) ## This feels like a check...
  
# Additional management information (COVID recovery related -- not KPI data)
# Secondary numerator: eligible individuals offered any time before 1 Sept 
# (after age 66)
invite_uptake <- invite_uptake %>%
  mutate(offer_add_year1 = case_when(cohort_year1 == 1 & inoffer == 1 ~ 1,
                                     TRUE ~ as.numeric(NA)),
         offer_add_year2 = case_when(cohort_year2 == 1 & inoffer == 1 ~ 1,
                                     TRUE ~ as.numeric(NA)))


## KPI 1.2a ----
## Percentage of eligible individuals tested before the age of 66 and 3 months
# Denominator: eligible population for current analysis year and current active 
# year (as KPI 1.1)
# Numerator: eligible individuals that have been tested and age_screen < 795 months 
# (66 years + 3 months)
invite_uptake <- invite_uptake %>%
  mutate(test_a_year1 = case_when(!is.na(screen_result) & age_screen < 795 &
                                    cohort_year1 == 1 ~ 1,
                                  TRUE ~ as.numeric(NA)),
         test_a_year2 = case_when(!is.na(screen_result) & age_screen < 795 &
                                    cohort_year2 == 1 ~ 1,
                                  TRUE ~ as.numeric(NA)),
         test_a_not_assigned = if_else(!is.na(test_a_year1) |
                                         !is.na(test_a_year2) , as.numeric(NA), 1))

# Additional management information (COVID recovery related -- not KPI data)
# Secondary numerator: eligible individuals that have been tested any time 
# before 1 Sept (after age 66 and 3 months)
invite_uptake <- invite_uptake %>%
  mutate(test_a_add_year1 = case_when(!is.na(screen_result) & 
                                        cohort_year1 == 1 ~ 1,
                                      TRUE ~ as.numeric(NA)),
         test_a_add_year2 = case_when(!is.na(screen_result) & 
                                        cohort_year2 == 1 ~ 1,
                                      TRUE ~ as.numeric(NA)),
         test_a_add_not_assigned = if_else(!is.na(test_a_add_year1) |
                                             !is.na(test_a_add_year2) , 
                                           as.numeric(NA), 1))

## Coverage by 1 Sept: individuals that have been tested before 1 Sept
# Only used in autumn report
# This is KPI 1.2a additional management information (secondary numerator) for 
# the current year, plus the previous two years and is additional information
# to assist in the assessment of the COVID recovery data (not KPI data)


## KPI 1.2b ----
## Percentage of individuals offered screening before age 66 who are tested before  
## the age of 66 and 3 months
# Numerator: individuals that have been offered screening before age 66 who are 
# tested before 795 months (66 years + 3 months)
invite_uptake <- invite_uptake %>%
  mutate(test_b_year1 = case_when(!is.na(screen_result) == 1 & age_screen < 795 &
                                    age_offer < 66 & cohort_year1 == 1 ~ 1,
                                  TRUE ~ as.numeric(NA)),
         test_b_year2 = case_when(!is.na(screen_result) == 1 & age_screen < 795 &
                                    age_offer < 66 & cohort_year2 == 1 ~ 1,
                                  TRUE ~ as.numeric(NA)),
         test_b_not_assigned = if_else(test_b_year1 != 1 &
                                         test_b_year2 != 1, 1, 0))


## KPI 1.3a ----
# Scotland-weighted SIMD is a variable within invite-uptake dataset; relevant
# derived variables already created with KPI 1.2, but specifics are as follows:

## Percentage of eligible individuals tested before the age of 66 and 3 months by 
## Scotland-weighted SIMD
# Numerator: eligible individuals that have been tested and age_screen < 795 months 
# (66 years + 3 months)
# NOTE: year2 is only used for Scotland totals and is not used for HB splits. 
# This information is added to tab 'KPI 1.2a Additional'. As with 1.2a, the 
# additional figures for individuals tested any time before 1 Sept are produced 
# for the autumn report only.

## Coverage by 1st Sept SIMD: individuals that have been tested before Sept 1 of 
# extract year by Scotland-weighted SIMD
# Only used in autumn report
# This is KPI 1.3a additional management information (secondary numerator) for 
# the current year, plus the previous two years and is additional information
# to assist in the assessment of the COVID recovery data (not KPI data)

## Additional information (Primary audience: local HB coordinators)
# Percentage of eligible individuals tested before the age of 66 and 3 months by 
# health board-weighted SIMD
# Numerator: individuals that have been tested and age_screen < 795 months 
# (66 years + 3 months)
# Add HB-level SIMD
invite_uptake <- invite_uptake %>% 
  left_join(pc_simd, by = c("postcode" = "pc8")) |>
  relocate(simd2020v2_hb2019_quintile, .before = hbres)

rm(pc_simd, simd_path)


## KPI 1.3b ----
# Scotland-weighted SIMD is a variable within invite-uptake dataset; most relevant
# derived variables already created with KPI 1.2, but specifics are as follows:

## Percentage of individuals offered screening before age 66 who are tested before  
## the age of 66 and 3 months by Scotland-weighted SIMD
# Numerator: individuals that have been offered screening before age 66 who are 
# tested before 795 months (66 years + 3 months)

## Additional information (Primary audience: local HB coordinators)
# Percentage of individuals tested before the age of 66 and 3 months by 
# health board-weighted SIMD
# Numerator: individuals that have been tested and age_screen < 795 months 
# (66 years + 3 months)
# HB-level SIMD added as part of KPI 1.3a above



##!! Is it possible to add KPI 1.2a/b prisoners??
## (KPI1.2a/b prisoners is fall QPMG only)




### Step 4: Save out basefiles ----
query_write_rds(invite_uptake, paste0(temp_path, "/1_2_coverage_basefile.rds"))


#invite_uptake <- read_rds(paste0(temp_path, "/1_2_coverage_basefile.rds"))

### Step 5: Summaries ----
## KPI 1.1 ----
kpi_1_1 <- invite_uptake  |> 
  # trim date (select men born from 1 April 1948)    ##!!WHY??
  # keeps data for eligible cohorts for men turning age 66 from 2014/15
  filter(dob >= dmy("01-04-1948")) |> 
  select(hbres, cohort_year1:offer_add_year2) |> 
  group_by(hbres) |> 
  summarise(across(cohort_year1:offer_add_year2, \(x) sum(x, na.rm = TRUE))) %>%
  group_modify(~ janitor::adorn_totals(.x, where = "row", name = "Scotland")) %>% 
  ungroup() |> 
  glimpse() # offer_not_assigned what does this tell us? Why included??

kpi_1_1<- kpi_1_1 |> 
  mutate(coverage_year1 = (offer_year1/cohort_year1)*100,
         coverage_year2 = (offer_year2/cohort_year2)*100,
         coverage_add_year1 = (offer_add_year1/cohort_year1)*100,
         coverage_add_year2 = (offer_add_year2/cohort_year2)*100) |> 
  select(hbres, cohort_year1, offer_year1, coverage_year1, offer_add_year1,
         coverage_add_year1, cohort_year2, offer_year2, coverage_year2,
         offer_add_year2, coverage_add_year2) # these last 2 only used in fall QPMG

# Reformat to match historical data
kpi_1_1 <- kpi_1_1 |> 
  pivot_longer(!hbres, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year) |> 
  mutate(kpi = if_else(str_detect(fin_year, "_add_"), 
                       "KPI 1.1 Sept coverage", "KPI 1.1"), .after = hbres) |> 
  mutate(fin_year = case_when(str_detect(fin_year, "_year1") ~ year1,
                              str_detect(fin_year, "_year2") ~ year2),
         group = case_when(str_detect(group, "cohort") ~ "cohort_n",
                           str_detect(group, "offer") ~ "offer_n",
                           str_detect(group, "coverage") ~ "coverage_p")) |> 
  mutate(simd = NA, .after = fin_year) |> 
  glimpse()

kpi_1_1 <- hb_tibble |> left_join(kpi_1_1, by = "hbres")

## KPI 1.2a ----
kpi_1_2a <- invite_uptake  |> 
  select(hbres, cohort_year1, cohort_year2, test_a_year1:test_a_add_not_assigned) |> 
  group_by(hbres) |> 
  summarise(across(cohort_year1:test_a_add_not_assigned, \(x) sum(x, na.rm = TRUE))) |> 
  group_modify(~ janitor::adorn_totals(.x, where = "row", name = "Scotland")) |>  
  ungroup() |> 
  glimpse() # _not_assigned: what do these rows do??

kpi_1_2a <- kpi_1_2a  |> 
  mutate(coverage_year1 = (test_a_year1/cohort_year1)*100,
         coverage_year2 = (test_a_year2/cohort_year2)*100,
         coverage_add_year1 = (test_a_add_year1/cohort_year1)*100,
         coverage_add_year2 = (test_a_add_year2/cohort_year2)*100) |> 
  select(hbres, cohort_year1, test_a_year1, coverage_year1, test_a_add_year1,
         coverage_add_year1, cohort_year2, test_a_year2, coverage_year2,
         test_a_add_year2, coverage_add_year2) # these last 2 only used in fall QPMG

# Reformat to match historical data
kpi_1_2a <- kpi_1_2a |> 
  pivot_longer(!hbres, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year) |> 
  mutate(kpi = if_else(str_detect(fin_year, "_add_"), 
                       "KPI 1.2a Sept coverage", "KPI 1.2a"), .after = hbres) |> 
  mutate(fin_year = case_when(str_detect(fin_year, "_year1") ~ year1,
                              str_detect(fin_year, "_year2") ~ year2),
         group = case_when(str_detect(group, "cohort") ~ "cohort_n",
                           str_detect(group, "test") ~ "test_n",
                           str_detect(group, "coverage") ~ "coverage_p")) |> 
  mutate(simd = NA, .after = fin_year) |> 
  glimpse()

kpi_1_2a <- hb_tibble |> left_join(kpi_1_2a, by = "hbres")


## KPI 1.2b ----
kpi_1_2b <- invite_uptake |> 
  select(hbres, offer_year1, offer_year2, test_b_year1:test_b_not_assigned) |> 
  group_by(hbres) |> 
  summarise(across(offer_year1:test_b_not_assigned, \(x) sum(x, na.rm = TRUE))) |> 
  group_modify(~ janitor::adorn_totals(.x, where = "row", name = "Scotland")) |>  
  ungroup() |> 
  glimpse() # _not_assigned: what do these rows do??

kpi_1_2b <- kpi_1_2b |> 
  mutate(uptake_year1 = (test_b_year1/offer_year1)*100,
         uptake_year2 = (test_b_year2/offer_year2)*100) |> 
  select(hbres, offer_year1, test_b_year1, uptake_year1,
         offer_year2, test_b_year2, uptake_year2)

# Reformat to match historical data
kpi_1_2b <- kpi_1_2b |> 
  pivot_longer(!hbres, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year) |> 
  mutate(kpi = "KPI 1.2b", .after = hbres) |> 
  mutate(fin_year = case_when(str_detect(fin_year, "_year1") ~ year1,
                              str_detect(fin_year, "_year2") ~ year2),
         group = case_when(str_detect(group, "offer") ~ "offer_n",
                           str_detect(group, "test") ~ "test_n",
                           str_detect(group, "uptake") ~ "uptake_p")) |> 
  mutate(simd = NA, .after = fin_year) |> 
  glimpse()

kpi_1_2b <- hb_tibble |> left_join(kpi_1_2b, by = "hbres")


## KPI 1.3a ----
## Coverage by Scotland-level SIMD
# Health Boards
kpi_1_3a <- invite_uptake  |> 
  select(hbres, simd2020v2_sc_quintile, cohort_year1, cohort_year2, 
         test_a_year1:test_a_add_not_assigned) |> 
  group_by(hbres, simd2020v2_sc_quintile) |> 
  summarise(across(cohort_year1:test_a_add_not_assigned, \(x) sum(x, na.rm = TRUE))) |> 
  group_modify(~ janitor::adorn_totals(.x, where = "row", name = "Total")) |>  
  ungroup() |> 
  glimpse() # _not_assigned: what do these rows do??

# Scotland
kpi_1_3a_scot <- invite_uptake  |> 
  select(simd2020v2_sc_quintile, cohort_year1, cohort_year2, 
         test_a_year1:test_a_add_not_assigned) |> 
  group_by(simd2020v2_sc_quintile) |> 
  summarise(across(cohort_year1:test_a_add_not_assigned, \(x) sum(x, na.rm = TRUE))) |> 
  group_modify(~ janitor::adorn_totals(.x, where = "row", name = "Total")) |>  
  ungroup() |> 
  mutate(hbres = "Scotland", .before = simd2020v2_sc_quintile) |> 
  glimpse() # _not_assigned: what do these rows do??

# Combine & order by SIMD
kpi_1_3a <- bind_rows(kpi_1_3a_scot, kpi_1_3a) |> 
  mutate(simd2020v2_sc_quintile = if_else(is.na(simd2020v2_sc_quintile), 
                                          "Unknown", simd2020v2_sc_quintile))
kpi_1_3a <- simd_level |> left_join(kpi_1_3a, 
                                    by = c("simd" = "simd2020v2_sc_quintile"))

kpi_1_3a <- kpi_1_3a  |> 
  mutate(coverage_year1 = (test_a_year1/cohort_year1)*100,
         coverage_year2 = (test_a_year2/cohort_year2)*100,
         coverage_add_year1 = (test_a_add_year1/cohort_year1)*100,
         coverage_add_year2 = (test_a_add_year2/cohort_year2)*100) |> 
  select(hbres, simd, cohort_year1, test_a_year1, coverage_year1, test_a_add_year1,
         coverage_add_year1, cohort_year2, test_a_year2, coverage_year2,
         test_a_add_year2, coverage_add_year2) # these last 2 only used in fall QPMG

# Reformat to match historical data
kpi_1_3a <- kpi_1_3a |> 
  pivot_longer(!hbres:simd, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year) |> 
  mutate(kpi = if_else(str_detect(fin_year, "_add_"), "KPI 1.3a Sept coverage", 
                       "KPI 1.3a Scotland SIMD"), .after = hbres) |> 
  mutate(fin_year = case_when(str_detect(fin_year, "_year1") ~ year1,
                              str_detect(fin_year, "_year2") ~ year2),
         group = case_when(str_detect(group, "cohort") ~ "cohort_n",
                           str_detect(group, "test") ~ "test_n",
                           str_detect(group, "coverage") ~ "coverage_p")) |> 
  relocate(simd, .after = fin_year) |>
  glimpse()

kpi_1_3a <- hb_tibble |> left_join(kpi_1_3a, by = "hbres")


## KPI 1.3a Health Board ----
## Coverage by HB-level SIMD
## (No need to create Scotland-level)
# Health Boards
kpi_1_3a_hb <- invite_uptake  |> 
  select(hbres, simd2020v2_hb2019_quintile, cohort_year1, test_a_year1, 
         test_a_not_assigned) |> 
  group_by(hbres, simd2020v2_hb2019_quintile) |> 
  summarise(across(cohort_year1:test_a_not_assigned, \(x) sum(x, na.rm = TRUE))) |> 
  group_modify(~ janitor::adorn_totals(.x, where = "row", name = "Total")) |>  
  ungroup() |> 
  glimpse() # _not_assigned: what do these rows do??

# Order by SIMD
kpi_1_3a_hb <- kpi_1_3a_hb |> 
  mutate(simd2020v2_hb2019_quintile = if_else(is.na(simd2020v2_hb2019_quintile), 
                                              "Unknown", simd2020v2_hb2019_quintile))
kpi_1_3a_hb <- simd_level |> left_join(kpi_1_3a_hb, 
                                       by = c("simd" = "simd2020v2_hb2019_quintile"))

kpi_1_3a_hb <- kpi_1_3a_hb |> 
  mutate(coverage_year1 = (test_a_year1/cohort_year1)*100) |> 
  select(hbres, simd, cohort_year1, test_a_year1, coverage_year1)

# Reformat to match historical data
kpi_1_3a_hb <- kpi_1_3a_hb |> 
  pivot_longer(!hbres:simd, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year) |> 
  mutate(kpi = "KPI 1.3a HB SIMD", .after = hbres) |> 
  mutate(fin_year = year1,
         group = case_when(str_detect(group, "cohort") ~ "cohort_n",
                           str_detect(group, "test") ~ "test_n",
                           str_detect(group, "coverage") ~ "coverage_p")) |> 
  relocate(simd, .after = fin_year) |>
  glimpse()

kpi_1_3a_hb <- hb_tibble |> left_join(kpi_1_3a_hb, by = "hbres") |> 
  filter(hbres != "Scotland")


## KPI 1.3b ----
## Coverage by Scotland-level SIMD
# Health Boards
kpi_1_3b <- invite_uptake  |> 
  select(hbres, simd2020v2_sc_quintile, offer_year1, test_b_year1,  
         test_b_not_assigned) |> 
  group_by(hbres, simd2020v2_sc_quintile) |> 
  summarise(across(offer_year1:test_b_not_assigned, \(x) sum(x, na.rm = TRUE))) |> 
  group_modify(~ janitor::adorn_totals(.x, where = "row", name = "Total")) |>  
  ungroup() |> 
  glimpse() # _not_assigned: what do these rows do??

# Scotland
kpi_1_3b_scot <- invite_uptake  |> 
  select(simd2020v2_sc_quintile, offer_year1, test_b_year1, 
         test_b_not_assigned) |> 
  group_by(simd2020v2_sc_quintile) |> 
  summarise(across(offer_year1:test_b_not_assigned, \(x) sum(x, na.rm = TRUE))) |> 
  group_modify(~ janitor::adorn_totals(.x, where = "row", name = "Total")) |>  
  ungroup() |> 
  mutate(hbres = "Scotland", .before = simd2020v2_sc_quintile) |> 
  glimpse() # _not_assigned: what do these rows do??

# Combine & order by SIMD
kpi_1_3b <- bind_rows(kpi_1_3b_scot, kpi_1_3b) |>
  mutate(simd2020v2_sc_quintile = if_else(is.na(simd2020v2_sc_quintile),
                                          "Unknown", simd2020v2_sc_quintile))
kpi_1_3b <- simd_level |> left_join(kpi_1_3b,
                                    by = c("simd" = "simd2020v2_sc_quintile"))

kpi_1_3b <- kpi_1_3b  |> 
  mutate(uptake_year1 = (test_b_year1/offer_year1)*100) |> 
  select(hbres, simd, offer_year1, test_b_year1, uptake_year1)

# Reformat to match historical data
kpi_1_3b <- kpi_1_3b |> 
  pivot_longer(!hbres:simd, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year) |> 
  mutate(kpi = "KPI 1.3b Scotland SIMD", .after = hbres) |> 
  mutate(fin_year = year1,
         group = case_when(str_detect(group, "offer") ~ "offer_n",
                           str_detect(group, "test") ~ "test_n",
                           str_detect(group, "uptake") ~ "uptake_p")) |> 
  relocate(simd, .after = fin_year) |>
  glimpse()

kpi_1_3b <- hb_tibble |> left_join(kpi_1_3b, by = "hbres")


## KPI 1.3b Health Board ----
## Coverage by HB-level SIMD
## (No need to create Scotland-level)
# Health Boards
kpi_1_3b_hb <- invite_uptake  |> 
  select(hbres, simd2020v2_hb2019_quintile, offer_year1, test_b_year1, 
         test_b_not_assigned) |> 
  group_by(hbres, simd2020v2_hb2019_quintile) |> 
  summarise(across(offer_year1:test_b_not_assigned, \(x) sum(x, na.rm = TRUE))) |> 
  group_modify(~ janitor::adorn_totals(.x, where = "row", name = "Total")) |>  
  ungroup() |> 
  glimpse() # _not_assigned: what do these rows do??

# Order by SIMD
kpi_1_3b_hb <- kpi_1_3b_hb |> 
  mutate(simd2020v2_hb2019_quintile = if_else(is.na(simd2020v2_hb2019_quintile), 
                                              "Unknown", simd2020v2_hb2019_quintile))
kpi_1_3b_hb <- simd_level |> left_join(kpi_1_3b_hb, 
                                       by = c("simd" = "simd2020v2_hb2019_quintile"))

kpi_1_3b_hb <- kpi_1_3b_hb  |> 
  mutate(uptake_year1 = (test_b_year1/offer_year1)*100) |> 
  select(hbres, simd, offer_year1, test_b_year1, uptake_year1)

# Reformat to match historical data
kpi_1_3b_hb <- kpi_1_3b_hb |> 
  pivot_longer(!hbres:simd, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year) |> 
  mutate(kpi = "KPI 1.3b HB SIMD", .after = hbres) |> 
  mutate(fin_year = year1,
         group = case_when(str_detect(group, "offer") ~ "offer_n",
                           str_detect(group, "test") ~ "test_n",
                           str_detect(group, "uptake") ~ "uptake_p")) |> 
  relocate(simd, .after = fin_year) |>
  glimpse()

kpi_1_3b_hb <- hb_tibble |> left_join(kpi_1_3b_hb, by = "hbres") |> 
  filter(hbres != "Scotland")

rm(kpi_1_3a_scot, kpi_1_3b_scot, hb_tibble, simd_level)


## Join summaries ----
kpi_summary <- rbind(kpi_1_1, kpi_1_2a, kpi_1_2b, kpi_1_3a,  
                     kpi_1_3a_hb, kpi_1_3b, kpi_1_3b_hb) |> 
  mutate(value = janitor::round_half_up(value, 1))

table(kpi_summary$kpi, kpi_summary$fin_year)

# Change NaNs to NAs
kpi_summary$value[is.nan(kpi_summary$value)] <- NA

rm(kpi_1_1, kpi_1_2a, kpi_1_2b, kpi_1_3a, kpi_1_3a_hb, kpi_1_3b, kpi_1_3b_hb)


### Step 6: Add historical data ----
# Historical data from two previous published years needs to be added to 
# KPI 1.1, 1.2a, 1.2a Coverage by 1 Sept, 1.2b, 1.3a Scotland SIMD, 
# 1.3a Coverage by 1 Sept SIMD, 1.3a HB SIMD, 1.3b Scotland SIMD, and 1.3b HB SIMD

## Full records (currently only from 2020/21; need to add historical)
hist_db <- read_rds(paste0(hist_path,"/aaa_kpi_historical_theme2.rds"))

table(hist_db$kpi, hist_db$fin_year)
table(kpi_summary$kpi, kpi_summary$fin_year)

build_history(df_hist = hist_db, 
              df_new = kpi_summary, 
              kpi_number = "1.1-1.3",
              season_var = season,
              fys_in_report = kpi_report_years,
              list_of_fys = fy_list,
              list_of_hbs = hb_list,
              historical_path = hist_path)

table(hist_db$kpi, hist_db$fin_year) # should be same as when called in 
#                        2019/20 2020/21 2021/22
# KPI 1.1                      0      45      45
# KPI 1.2a                     0      45      45
# KPI 1.2a Sept coverage       0      30      30
# KPI 1.2b                     0      45      45
# KPI 1.3a HB SIMD             0     294     294
# KPI 1.3a Scotland SIMD       0     315     315
# KPI 1.3a Sept coverage       0     210     210
# KPI 1.3b HB SIMD             0     294     294
# KPI 1.3b Scotland SIMD       0     315     315
# KPI 1.4a                    45      45      45
# KPI 1.4b                    45      45      42

## Current report output ----
## Add new records onto full database
report_db <- add_new_rows(hist_db, kpi_summary, fin_year, kpi)

## Check for duplication
viz_kpi_finyear(report_db) # current year (year1) should match 
# previous years, plus 30 records for KPI 1.1 Sept coverage; ignore year2 & KPI 1.4

report_db <- report_db |> 
  filter(fin_year %in% c(kpi_report_years, year2))

query_write_rds(report_db,
                        paste0(temp_path, "/2_1_invite_attend_", yymm, ".rds"))

#write_csv(report_db, paste0(temp_path, "/2_1_invite_attend_", yymm, ".csv")) # for checking

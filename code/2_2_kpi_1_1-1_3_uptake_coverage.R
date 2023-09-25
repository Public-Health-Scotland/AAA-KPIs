#~~~~~~~~~~~~~~~~~~~~~~~~~
# 2_2_kpi_1_1-1_3_uptake_coverage.R
# Angus Morton
# 17/11/2022
#
# Produce KPIs 1.1, 1.2a, 1.2b, 1.3a, 1.3b, 1.3a additional and 1.3b additional
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
# Step 5 : Create summary tables for each kpi
# Step 6 : Write out


### Step 1: Import packages and filepaths ----
library(readr)
library(dplyr)
library(lubridate)
library(phsmethods)
library(tidylog)
library(glue)

rm(list = ls())
gc()


source(here::here("code/0_housekeeping.R"))

rm(exclusions_path, extract_path, cutoff_date, cut_off_12m, cut_off_3m, 
   prev_year, current_year, current_year_start, next_year_start,
   financial_year_due, financial_quarters, last_date, next_year, date_cut_off)


### Step 2: Import data ----
invite_uptake <- read_rds(paste0(temp_path, "/1_inviteanduptake_initial.rds"))

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
  mutate(cohort_year1 = case_when(between(dob, year1_start, year1_end) ~ 1,
                                  TRUE ~ as.numeric(NA)),
         cohort_year2 = case_when(between(dob, year2_start, year2_end) ~ 1,
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


##!! Is it possible to add KPI 1.4a/b here??
##!! What about DNA Exclusions and potentially KPI1.2a/b prisoners??
## (KPI1.2a/b prisoners is fall MEG only)




### Step 4: Save out basefiles ----
write_rds(invite_uptake, paste0(temp_path, "/2_coverage_basefile.rds"))

##!!Is this step needed if data is to be stored as historical file? Is it worth 
## also having a file that is just the given KPI run??
## DON'T DELETE AS FILE MAY BE USED LATER IN PROCESS!!!




### Step 5: Summaries ----
## KPI 1.1 ----
kpi_1_1 <- invite_uptake  |> 
  # trim date (select men born from 1 April 1948)    ##!!WHY??
  # keeps data for eligible cohorts for men turning age 66 from 2014/15
  filter(dob >= dmy("01-04-1948")) |> 
  select(hbres, cohort_year1:offer_add_year2) |> 
  group_by(hbres) |> 
  summarise(across(cohort_year1:offer_add_year2, sum, na.rm = TRUE)) %>%
  group_modify(~ janitor::adorn_totals(.x, where = "row", name = "Scotland")) %>% 
  ungroup() |> 
  glimpse() # offer_not_assigned should be 0 for each HB

kpi_1_1<- kpi_1_1 |> 
  mutate(coverage_year1 = (offer_year1/cohort_year1)*100,
         coverage_year2 = (offer_year2/cohort_year2)*100,
         coverage_add_year1 = (offer_add_year1/cohort_year1)*100,
         coverage_add_year2 = (offer_add_year2/cohort_year2)*100) |> 
  select(hbres, cohort_year1, offer_year1, coverage_year1, offer_add_year1,
         coverage_add_year1, cohort_year2, offer_year2, coverage_year2,
         offer_add_year2, coverage_add_year2) # these last 2 only used in fall MEG

# Reformat to match historical data
kpi_1_1 <- kpi_1_1 |> 
  pivot_longer(!hbres, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year) |> 
  mutate(kpi = if_else(str_detect(fin_year, "_add_"), 
                       "KPI 1.1 additional", "KPI 1.1"), .after = hbres) |> 
  mutate(fin_year = case_when(str_detect(fin_year, "_year1") ~ year1,
                              str_detect(fin_year, "_year2") ~ year2),
         group = case_when(str_detect(group, "cohort") ~ "cohort_n",
                           str_detect(group, "offer") ~ "offer_n",
                           str_detect(group, "coverage") ~ "coverage_p")) |> 
  mutate(simd = NA, .after = fin_year) |> 
  glimpse()

kpi_1_1 <- hb_list |> left_join(kpi_1_1, by = "hbres")


### KPI 1.2a ----
kpi_1_2a <- invite_uptake  |> 
  select(hbres, cohort_year1, cohort_year2, test_a_year1:test_a_add_not_assigned) |> 
  group_by(hbres) |> 
  summarise(across(cohort_year1:test_a_add_not_assigned, sum, na.rm = TRUE)) |> 
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
         test_a_add_year2, coverage_add_year2) # these last 2 only used in fall MEG

# Reformat to match historical data
kpi_1_2a <- kpi_1_2a |> 
  pivot_longer(!hbres, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year) |> 
  mutate(kpi = if_else(str_detect(fin_year, "_add_"), 
                       "KPI 1.2a additional", "KPI 1.2a"), .after = hbres) |> 
  mutate(fin_year = case_when(str_detect(fin_year, "_year1") ~ year1,
                              str_detect(fin_year, "_year2") ~ year2),
         group = case_when(str_detect(group, "cohort") ~ "cohort_n",
                           str_detect(group, "test") ~ "test_n",
                           str_detect(group, "coverage") ~ "coverage_p")) |> 
  mutate(simd = NA, .after = fin_year) |> 
  glimpse()

kpi_1_2a <- hb_list |> left_join(kpi_1_2a, by = "hbres")


### KPI 1.2b ----
kpi_1_2b <- invite_uptake  |> 
  select(hbres, offer_year1, offer_year2, test_b_year1:test_b_not_assigned) |> 
  group_by(hbres) |> 
  summarise(across(offer_year1:test_b_not_assigned, sum, na.rm = TRUE)) |> 
  group_modify(~ janitor::adorn_totals(.x, where = "row", name = "Scotland")) |>  
  ungroup() |> 
  glimpse() # _not_assigned: what do these rows do??

kpi_1_2b <- kpi_1_2b  |> 
  mutate(uptake_year1 = (test_b_year1/offer_year1)*100,
         uptake_year2 = (test_b_year2/offer_year2)*100) |> 
  select(hbres, offer_year1, test_b_year1, uptake_year1,
         offer_year2, test_b_year2, uptake_year2)

# Reformat to match historical data
kpi_1_2b_test <- kpi_1_2b |> 
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

kpi_1_2b <- hb_list |> left_join(kpi_1_2b, by = "hbres")






























#########



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



### KPI 1.3a ----
## Coverage by Scotland-level SIMD
breakdown_1_3a <- invite_uptake %>%
  group_by(hbres, simd2020v2_sc_quintile) %>%
  summarise(
    across(cohort_year1:tested2_any_not_assigned, sum, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(simd2020v2_sc_quintile = as.character(simd2020v2_sc_quintile)) %>% 
  mutate(simd2020v2_sc_quintile = 
           case_when(is.na(simd2020v2_sc_quintile) ~ "Unknown",
                     !is.na(simd2020v2_sc_quintile) ~ simd2020v2_sc_quintile))

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
output_1_3a <- breakdown_1_3a %>%
  select(hbres, simd2020v2_sc_quintile, cohort_year1, tested2_year1, 
         percent_year1, tested2_any_year1, percent_any_year1,
         cohort_year2, tested2_year2, percent_year2, tested2_any_year2, 
         percent_any_year2) %>%
  arrange(factor(simd2020v2_sc_quintile, levels = "Total"), 
          simd2020v2_sc_quintile)%>%
  arrange(factor(hbres, levels = "Scotland"), hbres)
# Slight differences due to newer simd version

# Save
#write_rds(output_1_3a, paste0(temp_path, "/KPI_1_3a.rds"))

rm(scotland_1_3a, breakdown_1_2a, breakdown_1_3a, tot_1_3a)


### KPI 1.3 Additional ----
## Coverage by HB-level SIMD
## Coverage by simd
breakdown_1_3_add <- coverage_by_NHS_Board_SIMD %>%
  group_by(hbres, simd2020v2_hb2019_quintile) %>%
  summarise(
    across(cohort_year1:tested2_any_not_assigned, sum, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(simd2020v2_hb2019_quintile = as.character(simd2020v2_hb2019_quintile)) %>% 
  mutate(simd2020v2_hb2019_quintile = 
           case_when(is.na(simd2020v2_hb2019_quintile) ~ "Unknown",
                     !is.na(simd2020v2_hb2019_quintile) ~ simd2020v2_hb2019_quintile))

breakdown_1_3_tot_add <- coverage_by_NHS_Board_SIMD %>%
  group_by(hbres) %>%
  summarise(
    across(cohort_year1:tested2_any_not_assigned, sum, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(simd2020v2_hb2019_quintile = "Total", .after = hbres)


# bind together including non-simd totals from previous kpi
breakdown_1_3_add <- bind_rows(breakdown_1_3_tot_add, breakdown_1_3_add) %>% 
  arrange(hbres)


### KPI 1.3a ----
# create percentages
breakdown_1_3a_add <- breakdown_1_3_add %>%
  mutate(
    percent_year1 = (tested2_year1/cohort_year1)*100,
    percent_year2 = (tested2_year2/cohort_year2)*100,
    
    percent_any_year1 = (tested2_any_year1/cohort_year1)*100,
    percent_any_year2 = (tested2_any_year2/cohort_year2)*100
  )

# Output tables
output_1_3a_add <- breakdown_1_3a_add %>%
  select(hbres, simd2020v2_hb2019_quintile, cohort_year1, tested2_year1, percent_year1,
         cohort_year2, tested2_year2, percent_year2)

# Save
#write_rds(output_1_3a_add, paste0(temp_path, "/KPI_1_3a_add.rds"))

rm(breakdown_1_3a_add, breakdown_1_3_tot_add)


### KPI 1.3b ----
# create percentages
breakdown_1_3b_add <- breakdown_1_3_add %>%
  mutate(
    percent_year1 = (tested_year1/offer_year1)*100,
    percent_year2 = (tested_year2/offer_year2)*100,
    
    p_not_assigned = (tested_not_assigned/offer_not_assigned)*100
  )

# Output tables
output_1_3b_add <- breakdown_1_3b_add %>%
  select(hbres, simd2020v2_hb2019_quintile, offer_year1, tested_year1,
         percent_year1, offer_year2, tested_year2, percent_year2) %>%
  mutate_all(~ifelse(is.nan(.), NA, .))

# Save
#write_rds(output_1_3b_add, paste0(temp_path, "/KPI_1_3b_add.rds"))

rm(breakdown_1_3b_add)







### Step 6: Add historical data ----
# Historical data from two previous published years needs to be added to 
# KPI 1.1, 1.2a, Coverage by 1 Sept, 1.2b, 1.3a, Coverage by 1 Sept SIMD,
# 1.3a Additional, 1.3b, and 1.3b Additional

## Full records (currently only from 2020/21; need to add historical)
full_db <- read_rds(paste0(hist_path,"/aaa_kpi_historical.rds"))
# save a backup of full_db
write_rds(full_db, paste0(hist_path, "/aaa_kpi_historical_bckp.rds"))
# and change permissions to give the group read/write
Sys.chmod(paste0(hist_path, "/aaa_kpi_historical_bckp.rds"),
          mode = "664", use_umask = FALSE)

# ## current month's records
# current <- read_csv(paste0(r079_path, file_name, YYMM, ".csv"))
# 
# ## Define start date for each month -- this is just 1st of the month
# current %<>%
#   mutate(WorklistDate = dmy(WorklistDate),
#          start_date = floor_date(as_date(WorklistDate), "month"), 
#          .after = WorklistDate) %>% 
#   arrange(BSCName, WorklistDate) %>% 
#   glimpse()
# 
# 
# ## Add new records onto full database
# new_db <- bind_rows(full_db, current) %>% 
#   arrange(WorklistDate, BSCName)
# 
# ## Check for duplication
# table(new_db$start_date) # current month should match `current` obs.
# ggplot(new_db, aes(x = WorklistDate)) +
#   geom_histogram(binwidth = 24)
# 
# ## Check any dates that look odd from visual inspection
# date_check <- new_db %>%
#   count(WorklistDate)
# 
# ggplot(date_check, aes(x = WorklistDate)) +
#   geom_histogram(binwidth = 20)
# 
# 
# write_rds(new_db, paste0(proj_folder, "/Output/SBSS_R079_complete.rds"))
# # and change permissions to give the group read/write
# Sys.chmod(paste0(proj_folder, "/Output/SBSS_R079_complete.rds"),
#           mode = "664", use_umask = FALSE)
















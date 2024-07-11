##########################################################
# 8_4_unfit_for_surgery.R
# Karen Hotopp
# March 2023
# 
# Translation of SPSS file '4. Unfit for surgery'
# Part of Theme 4 for AAA KPIs
# 
# Written/run on R Studio Server, R version 3.6.1
# Revised/Run on Posit WB (R version 4.1.2)
##########################################################


## Notes:
# This script covers three parts of the theme 4 QPMG report:
# - Unfit for Surgery
# - Unfit for Surgery Follow-up
# - Unfit Follow-up Deaths by Cause


#### 1: Housekeeping ####
## Packages
library(odbc)
library(dplyr)
library(readr)
library(lubridate)
library(forcats)
library(janitor)
library(stringr)
library(tidylog)
library(phsaaa) # to install: devtools::install_github("aoifem01/phsaaa")


rm(list = ls())
gc()


source(here::here("code/0_housekeeping.R"))

rm (exclusions_path, hist_path, output_path, simd_path, fy_list, hb_list,
    fy_tibble, season, cutoff_date, end_current, end_date, start_date, 
    year1_end, year1_start, year2_end, year2_start, year1, year2, qpmg_month,
    extract_date)


## Values
# Set cut off date as end of latest financial year.
# Where an interim update is being provided for the QPMG (for instance, in March)
# the final output tables will need to include a footnote to indicate that it 
# represents a partial financial year (e.g. 1 April to end of February).
# Define 1, 3, 5-year periods
cut_off_date_1 <- cut_off_date - years(1)
cut_off_date_3 <- cut_off_date - years(3)
cut_off_date_5 <- cut_off_date - years(5)


#------------------------Unfit for Surgery-------------------------------------#

#### 2: Format Data ####
extract <- read_rds(extract_path) %>% 
  filter(!is.na(date_referral_true),
         date_screen <= cut_off_date) %>% 
  mutate(size_dichot = if_else(largest_measure >= 5.5, ">=5.5cm", "<5.5cm")) %>% 
  glimpse()


table(extract$screen_result)
# 01 (positive) = 898; 02 (negative) = 7    Sep 2022
# 01 (positive) = 1012; 02 (negative) = 7   Mar 2023
# 01 (positive) = 1016; 02 (negative) = 7   Sep 2023
# 01 (positive) = 1148; 02 (negative) = 9   Mar 2023

table(extract$result_outcome, extract$size_dichot)


###
# Quick look at individuals who have been operated on and have result_outcome
# as 'other final'
# 'Other final outcome' reported as a single category in report & in records
check <- extract %>% 
  filter(result_outcome == "20",
         !is.na(date_surgery))
# Two records, from 2012/13 and 1014/15, are where surgery was abandoned
# One record, from 2016/17, is Lothian man, mentioned in 91_4_vascular_referrals.R;
# now that HB of surgery recorded, need to f/up re: result_outcome
rm(check)
###


# Remove where categorized as 'referred in error: appointment with 
# vascular service not required'.
extract <- extract |> 
  filter(result_outcome != "02") %>% 
  mutate(outcome_type = case_when(result_outcome %in%
                                    c('01','03','04','05','06','07','08','11',
                                      '12','13','15','16','20') ~ "final outcome",
                                  result_outcome %in% c('09','10','14','17','18',
                                                        '19') ~ "non-final outcome",
                                  is.na(result_outcome) ~ "no outcome recorded",
                                  TRUE ~ "not categorized")) %>% 
  glimpse()
  

### Unfit for Surgery
unfit_surgery <- extract %>%
  filter(size_dichot == ">=5.5cm",
         outcome_type == "final outcome") %>% 
  # flag anyone unfit for surgery (08)
  mutate(unfit = if_else(result_outcome == "08", 1, 0),
         cohort = 1)

table(unfit_surgery$unfit)
# 128 patients unfit  Sep 2022
# 143 patients unfit  Mar 2023
# 151 patients unfit  Sep 2023


## Patients unfit for surgery by financial year ---
unfit_fy <- unfit_surgery %>% 
  group_by(financial_year, hbres) %>% 
  summarize(cohort_n = sum(cohort), 
            unfit_n = sum(unfit)) %>% 
  # cumulative totals for FYs  
  group_modify(~ adorn_totals(.x, where = "row", name = "Scotland")) |> 
  ungroup() |> 
  mutate(unfit_p = round_half_up(unfit_n * 100/cohort_n, 1)) 

## Should this be written out? And rewritten each year as a new historical file?
unfit_hist <- hb_tibble |> 
  left_join(unfit_fy, by = "hbres")

## Cumulative total from programme implementation ---
unfit_cum <- unfit_surgery %>%
  group_by(hbres) %>%
  summarize(cohort_n = sum(cohort),
            unfit_n = sum(unfit)) %>%
  group_modify(~ adorn_totals(.x, where = "row", name = "Scotland")) |>
  ungroup() |>
  mutate(unfit_p = round_half_up(unfit_n * 100/cohort_n, 1)) |>
  mutate(financial_year = "Cumulative", .before = hbres)

## Current 3-year reporting period ---
unfit_current <- unfit_fy %>% 
  filter(financial_year %in% c(kpi_report_years, "Cumulative"))

unfit_current <- rbind(unfit_current, unfit_cum)

unfit_current <- hb_tibble |> 
  left_join(unfit_current, by = "hbres")

phsaaa::query_write_rds(unfit_current, paste0(temp_path, "/4_8_unfit_for_surgery_", yymm, ".rds"))

rm(unfit_cum, unfit_fy, unfit_hist, unfit_surgery)


#------------------------Unfit for Surgery Follow-up---------------------------#

# Connect to SMRA tables using odbc connection
# The suppressWarnings function prevents your password from appearing in the
# console if the connection is unsuccessful
smra_con <- suppressWarnings(dbConnect(
  odbc(),
  dsn = "SMRA",
  uid = .rs.askForPassword("What is your user ID?"),
  pwd = .rs.askForPassword("What is your LDAP password?")
))


### 3: Data Extraction ----
# Keep where they were referred to vascular, had a large aneurysm and 
# result_outcome is 08 (unfit for surgery)
extract <- read_rds(extract_path) %>% 
  filter(!is.na(date_referral_true) & largest_measure >= 5.5, 
         result_outcome == "08") %>% 
  select(upi, dob, date_screen, hbres, date_surgery, result_outcome, 
         date_death, surg_method, financial_year)

# Read in deaths data
# Select columns and filter for AGE >= 64, DATE_OF_DEATH 2012 onwards and where
# UPI_NUMBER is not empty
deaths <- tbl(smra_con, dbplyr::in_schema("ANALYSIS", "GRO_DEATHS_C")) %>%
  select(YEAR_OF_REGISTRATION, DATE_OF_DEATH, INSTITUTION, 
         UNDERLYING_CAUSE_OF_DEATH, CAUSE_OF_DEATH_CODE_0, 
         CAUSE_OF_DEATH_CODE_1, CAUSE_OF_DEATH_CODE_2, CAUSE_OF_DEATH_CODE_3, 
         CAUSE_OF_DEATH_CODE_4, CAUSE_OF_DEATH_CODE_5, CAUSE_OF_DEATH_CODE_6, 
         CAUSE_OF_DEATH_CODE_7, CAUSE_OF_DEATH_CODE_8, CAUSE_OF_DEATH_CODE_9, 
         AGE, DATE_OF_BIRTH, SEX, PLACE_OF_OCCURRENCE, UPI_NUMBER, LINK_NO, 
         HEALTH_BOARD_OF_OCCURENCE, POSTCODE, HEALTH_BOARD_AREA) %>%
  filter(AGE >= 64 & DATE_OF_DEATH >= to_date("2012-01-01", "YYYY:MM:DD") & 
           UPI_NUMBER != " ") %>%
  rename(UPI = UPI_NUMBER) %>% 
  collect() %>% 
  clean_names()

# Calculate year of death and arrange data by upi and date_of_death
# Group by upi and slice data, taking the last row for each group
# This deduplicates data where someone has two death records
deaths <- deaths %>% 
  mutate(year_of_death = year(date_of_death)) %>% 
  arrange(upi, date_of_death) %>% 
  group_by(upi) %>% 
  slice(n()) %>% 
  ungroup()

# Combine and calculate time between screen and death
extract <- extract %>% 
  left_join(deaths) %>% 
  mutate(screen_to_death = time_length(date_screen %--% date_of_death, 
                                       "days"))

##
# Check if date_death from aaa_extract matches date_of_death from deaths
extract %>% count(date_death == date_of_death)
##


### 4: Unfit for Surgery Follow-up ----
# Flag individuals who had surgery within last 1, 3 or 5 years
# Flag individuals who had surgery within last 1, 3 or 5 years and died within 
# 1, 3 or 5 years, respectively
# CP - THIS CURRENTLY SPECIFIES NUMBER OF DAYS - SHOULD WE CHANGE TO SUBTRACTING YEARS INSTEAD???
mortality <- extract %>% 
  mutate(unfit_1_year = case_when(date_screen <= cut_off_date_1 ~ 1, 
                                  TRUE ~ 0), 
         unfit_3_year = case_when(date_screen < cut_off_date_3 ~ 1, 
                                  TRUE ~ 0), 
         unfit_5_year = case_when(date_screen < cut_off_date_5 ~ 1, 
                                  TRUE ~ 0)) %>% 
  mutate(mort_1_year = 
           case_when(unfit_1_year == 1 & screen_to_death <= 365 ~ 1, # 1 year 
                     TRUE ~ 0), 
         mort_3_year = 
           case_when(unfit_3_year == 1 & screen_to_death <= 1095 ~ 1, # 3 year 
                     TRUE ~ 0), 
         mort_5_year = 
           case_when(unfit_5_year == 1 & screen_to_death <= 1825 ~ 1, # 5 year 
                     TRUE ~ 0))

# Check mortality counts
mortality %>% count(mort_1_year)
mortality %>% count(mort_3_year)
mortality %>% count(mort_5_year)

## Follow-up: 1, 3, 5-year Cumulative Mortalities ---
## Scotland summary
mortality_scotland <- mortality %>% 
  summarise(across(c(unfit_1_year, unfit_3_year, unfit_5_year, 
                     mort_1_year, mort_3_year, mort_5_year), sum)) %>% 
  mutate(rate_1_year = round_half_up(mort_1_year * 100 / unfit_1_year, 1), 
         rate_3_year = round_half_up(mort_3_year * 100 / unfit_3_year, 1), 
         rate_5_year = round_half_up(mort_5_year * 100 / unfit_5_year, 1)) %>% 
  select(ends_with("1_year"), ends_with("3_year"), ends_with("5_year"))

## HB of Residence summary
mortality_hb <- mortality %>% 
  group_by(hbres) %>% 
  summarise(across(c(unfit_1_year, unfit_3_year, unfit_5_year, 
                     mort_1_year, mort_3_year, mort_5_year), sum)) %>% 
  mutate(rate_1_year = round_half_up(mort_1_year * 100 / unfit_1_year, 1), 
         rate_3_year = round_half_up(mort_3_year * 100 / unfit_3_year, 1), 
         rate_5_year = round_half_up(mort_5_year * 100 / unfit_5_year, 1)) %>% 
  ungroup() %>% 
  # add on mortality_scotland
  bind_rows(mortality_scotland) %>% 
  mutate(hbres = case_when(is.na(hbres) ~ "Scotland", 
                           !is.na(hbres) ~ as.character(hbres))) %>% 
  # move Scotland to top of HB list, then alphabetical order
  arrange(hbres != "Scotland", hbres) %>% 
  select(hbres, ends_with("1_year"), ends_with("3_year"), ends_with("5_year"))

mortality_hb <- hb_tibble |> 
  left_join(mortality_hb, by = "hbres") |> 
  mutate_all(~ifelse(is.nan(.), NA, .))

phsaaa::query_write_rds(mortality_hb, paste0(temp_path, "/4_9_unfit_follow-up_", yymm, ".rds"))

#------------------------Unfit Follow-up Deaths by Cause-----------------------#

### 5: Cause of Death ----
# Read in ICD-10 look-up file
# Keep first row for each group
# This de-duplicates where an ICD-10 code has multiple rows
icd_10 <- read_csv(paste0("/conf/linkage/output/lookups/Unicode/", 
                          "National Reference Files/icd10.csv"), 
                   col_select = c("code1", "Description")) %>% 
  clean_names() %>% 
  rename(underlying_cause_of_death = code1) %>% 
  group_by(underlying_cause_of_death) %>% 
  slice(1) %>% 
  ungroup()

# Join mortality and icd_10
# Define category for various ICD-10 codes
# Use str_pad to create ICD-10 code lists by adding a zero to numeric part
# where required
# Also use this for the underlying cause for heart disease - this adds a zero
# at the end of three digit ICD-10 codes (e.g. I70 becomes I700) to ensure we
# capture everything for comparison for full four digit range
cause_of_death <- mortality %>% 
  left_join(icd_10) %>% 
  mutate(category = case_when(
    # Non-AAA categories
    str_sub(underlying_cause_of_death, 1, 3) %in% paste0("C", str_pad(c(0:97), width = 2, side = "left", pad = "0")) ~ "Neoplasms", 
    str_sub(underlying_cause_of_death, 1, 3) %in% paste0("D", str_pad(c(0:48), width = 2, side = "left", pad = "0")) ~ "Neoplasms",   
    str_sub(underlying_cause_of_death, 1, 3) %in% paste0("F0", c(0:3)) ~ "Dementia", 
    str_sub(underlying_cause_of_death, 1, 3) %in% "G30" ~ "Dementia", 
    str_sub(underlying_cause_of_death, 1, 3) %in% paste0("G", c(20, 31:99)) ~ "Diseases of the nervous system", 
    str_sub(str_pad(underlying_cause_of_death, width = 4, side = "right", pad = "0"), 1, 4) %in% paste0("I", str_pad(c(0:712), width = 3, side = "left", pad = "0")) ~ "Heart and circulatory disease (excl. AAA)", 
    str_sub(underlying_cause_of_death, 1, 3) %in% paste0("I", c(72:99)) ~ "Heart and circulatory disease (excl. AAA)", 
    str_sub(underlying_cause_of_death, 1, 3) %in% paste0("J", str_pad(c(0:99), width = 2, side = "left", pad = "0")) ~ "Respiratory disease", 
    str_sub(underlying_cause_of_death, 1, 3) %in% paste0("K", c(70:77)) ~ "Liver disease", 
    # COVID label doesn't seem to be working...
    str_sub(underlying_cause_of_death, 1, 3) %in% "U07" ~ "COVID-19", 
    str_sub(underlying_cause_of_death, 1, 3) %in% paste0("V", str_pad(c(1:98), width = 2, side = "left", pad = "0")) ~ "External causes of morbidity and mortality", 
    str_sub(underlying_cause_of_death, 1, 3) %in% paste0("W", str_pad(c(1:98), width = 2, side = "left", pad = "0")) ~ "External causes of morbidity and mortality", 
    str_sub(underlying_cause_of_death, 1, 3) %in% paste0("X", str_pad(c(1:98), width = 2, side = "left", pad = "0")) ~ "External causes of morbidity and mortality", 
    str_sub(underlying_cause_of_death, 1, 3) %in% paste0("Y", str_pad(c(1:98), width = 2, side = "left", pad = "0")) ~ "External causes of morbidity and mortality", 
    
    # AAA-related categories I71.3 to I71.9 (note there is no I71.7 in the ICD10 classification)
    str_sub(underlying_cause_of_death, 1, 4) %in% "I713" ~ "Ruptured AAA", 
    str_sub(underlying_cause_of_death, 1, 4) %in% "I714" ~ "AAA without mention of rupture", 
    str_sub(underlying_cause_of_death, 1, 4) %in% "I715" ~ "Ruptured TAAA", 
    str_sub(underlying_cause_of_death, 1, 4) %in% "I716" ~ "TAAA without mention of rupture", 
    str_sub(underlying_cause_of_death, 1, 4) %in% "I718" ~ "Ruptured Aortic Aneurysm, unspecified site", 
    str_sub(underlying_cause_of_death, 1, 4) %in% "I719" ~ "Aortic aneurysm of unspecified site without mention of rupture"
  ))

##
check <- cause_of_death |> 
  filter(is.na(category) & !is.na(description))

table(check$description, useNA = "ifany")
# For each NA category, look at description, find appropriate ICD-10 code and 
# update the above R code
rm(check)
##

## Underlying cause of death ---
# AAA-related deaths
aaa_deaths <- cause_of_death %>% 
  filter(!is.na(description)) %>% 
  # keep AAA-related deaths
  filter(underlying_cause_of_death %in% paste0("I71", c(3:6, 8:9))) %>% 
  group_by(category) %>% 
  summarise(count_n = n()) |> 
  group_modify(~ adorn_totals(.x, where = "row", 
                              name = "Total AAA-related deaths")) |>
  ungroup() |> 
  # reorder to match Excel
  mutate(category = fct_relevel(category, c("Ruptured AAA", "Ruptured TAAA",
                                            "AAA without mention of rupture",
                                            "TAAA without mention of rupture",
                                            "Ruptured Aortic Aneurysm, unspecified site",
                                            "Total AAA-related deaths"))) |> 
  arrange(category) |> 
  # add section variable for easier dividing for Excel
  mutate(section = "AAA-related causes")

# Non-AAA related deaths
non_aaa_deaths <- cause_of_death %>% 
  filter(!is.na(description)) %>% 
  # AAA-related deaths categorized together
  mutate(category = if_else(underlying_cause_of_death %in% paste0("I71", c(3:6, 8:9)),
                            "Total AAA-related deaths", category)) %>% 
  # unsure why COVID isn't being correctly labelled above...
  # mutate(category = if_else(is.na(category), "COVID-19", category)) |> 
  group_by(category) %>% 
  summarise(count_n = n()) |> 
  group_modify(~ adorn_totals(.x, where = "row", name = "Total deaths")) |>
  ungroup() |> 
  # add section variable for easier dividing for Excel
  mutate(section = "All causes")

# Total number of deaths
total_deaths <- non_aaa_deaths |> 
  filter(category == "Total deaths") |> 
  mutate(category = if_else(category == "Total deaths", "Deaths (all timescales)",
                            category)) |> 
  # add section variable for easier dividing for Excel
  mutate(section = "Totals")

# Total number unfit for surgery
total_unfit <- unfit_current |> 
  filter(hbres == "Scotland",
         financial_year == "Cumulative") |> 
  select(count_n = unfit_n) |>
  mutate(category = "Unfit for surgery", .before = count_n) |> 
  # add section variable for easier dividing for Excel
  mutate(section = "Totals")
  

### 6: Combine and save ----
all_deaths <- bind_rows(total_unfit, total_deaths, non_aaa_deaths, aaa_deaths)

phsaaa::query_write_rds(all_deaths, paste0(temp_path, "/4_91_unfit_deaths_cause_", yymm, ".rds"))


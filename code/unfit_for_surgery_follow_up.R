###############################################################################
# unfit_for_surgery_follow_up.R
# Calum Purdie
# 15/02/2023
# 
# Unfit for surgery follow-up for those who died and cause of death
#
# Written/run on Posit Server
# R version 4.1.2
###############################################################################

### 1 Housekeeping ----

# Load packages

library(here)
library(odbc)
library(dplyr)
library(readr)
library(lubridate)
library(phsmethods)
library(stringr)
library(janitor)
library(haven)
library(openxlsx)
library(zoo)
library(tidylog)

# Define date values

year <- 2022
month <- "09"

cut_off_date <- as.Date("2022-03-31")
cut_off_date_1 <- cut_off_date - years(1)
cut_off_date_3 <- cut_off_date - years(3)
cut_off_date_5 <- cut_off_date - years(5)

# Define extract name

extract_name <- paste0("aaa_extract_", year, month, ".rds")

# Define file paths

extracts_path <- paste0("/PHI_conf/AAA/Topics/Screening/extracts", "/", year, 
                        month, "/output")

output_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/temp/4. RTO/")

# Connect to SMRA tables using odbc connection
# The suppressWarnings function prevents your password from appearing in the
# console if the connection is unsuccessful

smra_con <- suppressWarnings(dbConnect(
  odbc(),
  dsn = "SMRA",
  uid = .rs.askForPassword("What is your user ID?"),
  pwd = .rs.askForPassword("What is your LDAP password?")
))



### 2 Data Extraction ----

# Read in latest extract
# Keep where they were referred to vascular, had a large aneurysm and 
# result_outcome is 08 - this is people unfit for surgery

aaa_extract <- read_rds(paste0(extracts_path, "/", extract_name)) %>% 
  filter(!is.na(date_referral_true) & largest_measure >= 5.5, 
         result_outcome == "08") %>% 
  select(upi, dob, date_screen, hbres, date_surgery, result_outcome, 
         date_death, surg_method, financial_year)

# Read in deaths data
# Select columns and filter for AGE >= 64, DATE_OF_DEATH 2012 onwards and where
# UPI_NUMBER is not empty
# Collect data and clean names

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

# Join deaths data onto aaa_extract
# Calculate time between surgery and death

aaa_extract <- aaa_extract %>% 
  left_join(deaths) %>% 
  mutate(screen_to_death = time_length(date_screen %--% date_of_death, 
                                        "days"))

# Check to see if date_death from aaa_extract matches date_of_death from deaths

aaa_extract %>% count(date_death == date_of_death)


# Read in icd10 lookup file - keep code1 and Description columns
# Rename cod1 and take first row for each group
# This deduplicates where an ICD-10 code has multiple rows

icd_10 <- read_sav(paste0("/conf/linkage/output/lookups/Unicode/", 
                          "National Reference Files/icd10.sav"), 
                   col_select = c("code1", "Description")) %>% 
  clean_names() %>% 
  rename(underlying_cause_of_death = code1) %>% 
  group_by(underlying_cause_of_death) %>% 
  slice(1) %>% 
  ungroup()
  



### 3 Mortality Output ----

# Create flags for people who had surgery within last 1, 3 or 5 years
# Create flags for people who had surgery within last 1, 3 or 5 years and 
# died within 1, 3 or 5 years respectively
# CP - THIS CURRENTLY SPECIFIES NUMBER OF DAYS - SHOULD WE CHANGE TO SUBTRACTING YEARS INSTEAD???

mortality <- aaa_extract %>% 
  mutate(screen_1_year = case_when(date_screen <= cut_off_date_1 ~ 1, 
                                 TRUE ~ 0), 
         screen_3_year = case_when(date_screen < cut_off_date_3 ~ 1, 
                                 TRUE ~ 0), 
         screen_5_year = case_when(date_screen < cut_off_date_5 ~ 1, 
                                 TRUE ~ 0)) %>% 
  mutate(mort_1_year = 
           case_when(screen_1_year == 1 & screen_to_death <= 365 ~ 1, 
                     TRUE ~ 0), 
         mort_3_year = 
           case_when(screen_3_year == 1 & screen_to_death <= 1095 ~ 1, 
                     TRUE ~ 0), 
         mort_5_year = 
           case_when(screen_5_year == 1 & screen_to_death <= 1825 ~ 1, 
                     TRUE ~ 0))

# Check mortality counts

mortality %>% count(mort_1_year)
mortality %>% count(mort_3_year)
mortality %>% count(mort_5_year)

# Sum mortality and screened within 1, 3, 5 years
# Calculate rates for each year and select column order

mortality_scotland <- mortality %>% 
  summarise(across(c(screen_1_year, screen_3_year, screen_5_year, 
                     mort_1_year, mort_3_year, mort_5_year), sum)) %>% 
  mutate(rate_1_year = round_half_up(mort_1_year * 100 / screen_1_year, 1), 
         rate_3_year = round_half_up(mort_3_year * 100 / screen_3_year, 1), 
         rate_5_year = round_half_up(mort_5_year * 100 / screen_5_year, 1)) %>% 
  select(ends_with("1_year"), ends_with("3_year"), ends_with("5_year"))

# Group by hbres and sum mortality and screened within 1, 3, 5 years
# Calculate rates for each year
# Add on mortality_scotland
# Define hbres as Scotland for blank hbres
# Arrange data by putting Scotland row first and then HB in alphabetical order
# Select columns to redefine order

mortality_hb <- mortality %>% 
  group_by(hbres) %>% 
  summarise(across(c(screen_1_year, mort_1_year, screen_3_year, mort_3_year, 
                     screen_5_year, mort_5_year), sum)) %>% 
  mutate(rate_1_year = round_half_up(mort_1_year * 100 / screen_1_year, 1), 
         rate_3_year = round_half_up(mort_3_year * 100 / screen_3_year, 1), 
         rate_5_year = round_half_up(mort_5_year * 100 / screen_5_year, 1)) %>% 
  bind_rows(mortality_scotland) %>% 
  mutate(hbres = case_when(is.na(hbres) ~ "Scotland", 
                           !is.na(hbres) ~ as.character(hbres))) %>% 
  arrange(hbres != "Scotland", hbres) %>% 
  select(hbres, ends_with("1_year"), ends_with("3_year"), ends_with("5_year"))



### 4 Cause of Death ----

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
    str_sub(underlying_cause_of_death, 1, 3) %in% paste0("G", c(31:99)) ~ "Diseases of the nervous system", 
    str_sub(str_pad(underlying_cause_of_death, width = 4, side = "right", pad = "0"), 1, 4) %in% paste0("I", str_pad(c(0:712), width = 3, side = "left", pad = "0")) ~ "Heart and Circulatory Disease (excl. AAA)", 
    str_sub(underlying_cause_of_death, 1, 3) %in% paste0("I", c(72:99)) ~ "Heart and Circulatory Disease (excl. AAA)", 
    str_sub(underlying_cause_of_death, 1, 3) %in% paste0("J", str_pad(c(0:99), width = 2, side = "left", pad = "0")) ~ "Respiratory disease", 
    str_sub(underlying_cause_of_death, 1, 3) %in% paste0("K", c(70:77)) ~ "Liver disease", 
    str_sub(underlying_cause_of_death, 1, 3) %in% paste0("V", str_pad(c(1:98), width = 2, side = "left", pad = "0")) ~ "External causes of morbidity and mortality", 
    str_sub(underlying_cause_of_death, 1, 3) %in% paste0("W", str_pad(c(1:98), width = 2, side = "left", pad = "0")) ~ "External causes of morbidity and mortality", 
    str_sub(underlying_cause_of_death, 1, 3) %in% paste0("X", str_pad(c(1:98), width = 2, side = "left", pad = "0")) ~ "External causes of morbidity and mortality", 
    str_sub(underlying_cause_of_death, 1, 3) %in% paste0("Y", str_pad(c(1:98), width = 2, side = "left", pad = "0")) ~ "External causes of morbidity and mortality", 
    
    # AAA-related categories I71.3 to I71.9 (note there is no I71.7 in the ICD10 classification)
    str_sub(underlying_cause_of_death, 1, 4) %in% "I713" ~ "Ruptured AAA", 
    str_sub(underlying_cause_of_death, 1, 4) %in% "I714" ~ "AAA without mention of rupture", 
    str_sub(underlying_cause_of_death, 1, 4) %in% "I715" ~ "Ruptured TAAA", 
    str_sub(underlying_cause_of_death, 1, 4) %in% "I716" ~ "TAAA without mention of rupture", 
    str_sub(underlying_cause_of_death, 1, 4) %in% "I718" ~ "Ruptured Aortic Aneurysm unspecified site", 
    str_sub(underlying_cause_of_death, 1, 4) %in% "I719" ~ "Aortic aneurysm of unspecified site without mention of rupture"
  ))

# Group mortality by financial_year and calculate total cohort for each group
# Group mortality by financial_year, description, category and cohort 
# Calculate total deaths for each group

cause_of_death_fy <- cause_of_death %>% 
  group_by(financial_year) %>% 
  mutate(cohort = n()) %>% 
  ungroup() %>% 
  group_by(financial_year, description, category, cohort) %>% 
  summarise(deaths = n())

# Get AAA related deaths
# Exclude where description is blank and take relevant causes of death
# Group by category and sum number of deaths for each

aaa_related_deaths <- cause_of_death %>% 
  filter(!is.na(description)) %>% 
  filter(underlying_cause_of_death %in% paste0("I71", c(3:6, 8:9))) %>% 
  group_by(category) %>% 
  summarise(deaths = n())

# Get non-AAA related deaths
# Exclude where description is blank and take relevant causes of death
# Group by category and sum number of deaths for each

non_aaa_related_deaths <- cause_of_death %>% 
  filter(!is.na(description)) %>% 
  filter(!(underlying_cause_of_death %in% paste0("I71", c(3:6, 8:9)))) %>% 
  group_by(category) %>% 
  summarise(deaths = n())

# Combine all deaths together

all_deaths <- bind_rows(aaa_related_deaths, non_aaa_related_deaths)



### 4 Output ----

# CP - ARE ALL OF THESE ACTUALLY USED SOMEWHERE?

# Save mortality output

write_xlsx(mortality_scotland, 
           paste0(output_path, "unfit_for_surgery_mortality_scotland.xlsx"))

write_xlsx(mortality_hb, 
           paste0(output_path, "unfit_for_surgery_mortality_hb.xlsx"))

# Save cause of death output

write_xlsx(cause_of_death, 
           paste0(output_path, "unfit_for_surgery_causes_of_death.xlsx"))

write_xlsx(cause_of_death_fy, 
           paste0(output_path, "unfit_for_surgery_causes_of_death_by_year.xlsx"))

write_xlsx(aaa_related_deaths, 
           paste0(output_path, "aaa_related_deaths_by_cause.xlsx"))

write_xlsx(non_aaa_related_deaths, 
           paste0(output_path, "non_aaa_related_deaths_by_cause.xlsx"))

write_xlsx(all_deaths, 
           paste0(output_path, "all_deaths_by_cause.xlsx"))

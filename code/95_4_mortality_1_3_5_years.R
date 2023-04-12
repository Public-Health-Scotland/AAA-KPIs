###############################################################################
# mortality_1_3_5_years.R
# Calum Purdie
# 30/01/2023
# 
# 1, 3 and 5 year mortality following endovascular and open AAA surgery
# Cohort is defined using date of surgery
#
# Written/run on R Studio Server
# R version 3.6.1
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
# Keep where they were referred to vascular, had a large aneurysm, date_surgery 
# is less than or equal to cut_off_date and result_outcome is 15 or 16
# 15 or 16 are cases of final outcome 'appropriate for surgery: aaa repaired and 
# survived 30 days' or 'appropriate for surgery: died within 30 days of 
# treatment' in line with as KPI definition document.
# Final outcome pending is not included in this KPI denominator).
# Calculate financial year for surgery date

aaa_extract <- read_rds(paste0(extracts_path, "/", extract_name)) %>% 
  filter(!is.na(date_referral_true) & largest_measure >= 5.5, 
         surg_method %in% c("01", "02"), 
         result_outcome %in% c("15", "16")) %>% 
  select(upi, dob, date_screen, hbres, date_surgery, result_outcome, 
         date_death, surg_method, financial_year) %>% 
  mutate(financial_year_surg = extract_fin_year(date_surgery))

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
  mutate(surgery_to_death = time_length(date_surgery %--% date_of_death, 
                                        "days"))

# Check to see if date_death from aaa_extract matches date_of_death from deaths

aaa_extract %>% count(date_death == date_of_death)



### 3 Mortality Output ----

# Create flags for people who had surgery within last 1, 3 or 5 years
# Create flags for people who had surgery within last 1, 3 or 5 years and 
# died within 1, 3 or 5 years respectively
# CP - THIS CURRENTLY SPECIFIES NUMBER OF DAYS - SHOULD WE CHANGE TO SUBTRACTING YEARS INSTEAD???

mortality <- aaa_extract %>% 
  mutate(surg_1_year = case_when(date_surgery <= cut_off_date_1 ~ 1, 
                                 TRUE ~ 0), 
         surg_3_year = case_when(date_surgery < cut_off_date_3 ~ 1, 
                                 TRUE ~ 0), 
         surg_5_year = case_when(date_surgery < cut_off_date_5 ~ 1, 
                                 TRUE ~ 0)) %>% 
  mutate(mort_1_year = 
           case_when(surg_1_year == 1 & surgery_to_death <= 365 ~ 1, 
                     TRUE ~ 0), 
         mort_3_year = 
           case_when(surg_3_year == 1 & surgery_to_death <= 1095 ~ 1, 
                     TRUE ~ 0), 
         mort_5_year = 
           case_when(surg_5_year == 1 & surgery_to_death <= 1825 ~ 1, 
                     TRUE ~ 0))

# Check mortality counts

mortality %>% count(mort_1_year)
mortality %>% count(mort_3_year)
mortality %>% count(mort_5_year)

# Group by surg_method and sum mortality and surgery within 1, 3, 5 years
# Calculate rates for each year

mortality_scotland <- mortality %>% 
  group_by(surg_method) %>% 
  summarise(across(c(mort_1_year, mort_3_year, mort_5_year, 
                     surg_1_year, surg_3_year, surg_5_year), sum)) %>% 
  mutate(rate_1_year = round_half_up(mort_1_year * 100 / surg_1_year, 1), 
         rate_3_year = round_half_up(mort_3_year * 100 / surg_3_year, 1), 
         rate_5_year = round_half_up(mort_5_year * 100 / surg_5_year, 1))

# Group by surg_method and hbres
# Sum mortality and surgery within 1, 3, 5 years
# Add on mortality_scotland and remove any column names that contain "rate"
# Define hbres as Scotland for blank hbres
# Arrange data by putting Scotland row first and then HB in alphabetical order
# Redefine surg_method as endovascular and open
# Pivot data into wider format, taking names from surg_method
# For values, use any column that has a digit in the column name - this uses
# all of the surg and mort 1, 3, 5 year columns
# Select columns to redefine order

mortality_hb <- mortality %>% 
  group_by(surg_method, hbres) %>% 
  summarise(across(c(surg_1_year, mort_1_year, surg_3_year, mort_3_year, 
                     surg_5_year, mort_5_year), sum)) %>% 
  bind_rows(mortality_scotland) %>% 
  select(-contains("rate")) %>% 
  mutate(hbres = case_when(is.na(hbres) ~ "Scotland", 
                           !is.na(hbres) ~ as.character(hbres))) %>% 
  arrange(hbres != "Scotland", hbres) %>% 
  mutate(surg_method = case_when(surg_method == "01" ~ "endovascular", 
                                 surg_method == "02" ~ "open")) %>% 
  pivot_wider(names_from = surg_method, 
              values_from = matches("[[:digit:]]"), 
              names_glue = "{surg_method}_{.value}") %>% 
  select(hbres, starts_with("open"), starts_with("endovascular"))

# Group mortality by surg_method and financial_year_surg
# Sum mortality and surgery within 1, 3, 5 years
# Arrange data by financial_year_surg

fy_data <- mortality %>% 
  group_by(surg_method, financial_year_surg) %>% 
  summarise(across(c(mort_1_year, mort_3_year, mort_5_year, 
                     surg_1_year, surg_3_year, surg_5_year), sum)) %>% 
  arrange(financial_year_surg)

# Calculate financial year data for open surgeries
# Filter for open surgeries
# Calculate rolling total deaths from open surgeries
# Calculate five-year rolling deaths and surgeries and define rolling year
# Remove rows where rolling_financial_year contains NA
# Select columns and calculate percentage deaths for each year
# CP - INCLUDES 22/23 - IS THIS CORRECT

open_surgery <- fy_data %>%
  filter(surg_method == "02") %>% 
  mutate(deaths = rollapply(mort_1_year, 5, sum, align = "left", fill = NA), 
         surgeries = rollapply(surg_1_year, 5, sum, align = "left", 
                               fill = NA), 
         rolling_financial_year = paste0(financial_year_surg, " - ", 
                                         lead(financial_year_surg, 4))) %>% 
  filter(!(str_detect(rolling_financial_year, "NA"))) %>% 
  select(rolling_financial_year, surgeries, deaths) %>%
  mutate(pc_deaths = round_half_up(deaths * 100 / surgeries, 1)) 

# Calculate financial year data for endovascular
# Filter for endovascular
# Calculate rolling total deaths from evar surgeries
# Calculate five-year rolling deaths and surgeries and define rolling year
# Remove rows where rolling_financial_year contains NA
# Select columns and calculate percentage deaths for each year
# CP - INCLUDES 22/23 - IS THIS CORRECT

evar <- fy_data %>%
  filter(surg_method == "01") %>% 
  mutate(deaths = rollapply(mort_1_year, 5, sum, align = "left", fill = NA), 
         surgeries = rollapply(surg_1_year, 5, sum, align = "left", 
                               fill = NA), 
         rolling_financial_year = paste0(financial_year_surg, " - ", 
                                         lead(financial_year_surg, 4))) %>% 
  filter(!(str_detect(rolling_financial_year, "NA"))) %>% 
  select(rolling_financial_year, surgeries, deaths) %>%
  mutate(pc_deaths = round_half_up(deaths * 100 / surgeries, 1)) 



### 4 Output ----

# Save mortality output

write_xlsx(mortality_scotland, 
           paste0(output_path, "Mortality_Scotland.xlsx"))

write_xlsx(mortality_hb, 
           paste0(output_path, "Mortality_HB.xlsx"))

# Save financial year output

write_xlsx(open_surgery, 
           paste0(output_path, "open_surgeries_rolling_total_deaths.xlsx"))

write_xlsx(evar, 
           paste0(output_path, "evar_surgeries_rolling_total_deaths.xlsx"))
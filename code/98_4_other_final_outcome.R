###############################################################################
# other_final_outcome.R
# Calum Purdie
# 16/02/2023
# 
# Save out other final outcome extracts
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

year <- 2023
month <- "03"

# Define extract name

extract_name <- paste0("aaa_extract_", year, month, ".rds")

# Define file paths

extracts_path <- paste0("/PHI_conf/AAA/Topics/Screening/extracts", "/", year, 
                        month, "/output")

output_path <- paste0("/PHI_conf/AAA/Portfolio/Data/Checks/", 
                      "OtherFinalOutcomeLog/Temp/")

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

aaa_extract <- read_rds(paste0(extracts_path, "/", extract_name)) 

# Check how many had a procedure abandoned and what result outcome recorded was

aaa_extract %>% 
  filter(surg_method == "03") %>% 
  count(result_outcome)

# Filter for other final outcome and select columns

other_final_outcome <- aaa_extract %>% 
  filter(result_outcome == "20") %>% 
  select(upi, chi, dob, date_screen, hbres, date_seen_outpatient, date_surgery, 
         surg_method)

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

other_final_outcome_deaths <- other_final_outcome %>% 
  left_join(deaths) %>% 
  mutate(time_to_death = time_length(date_surgery %--% date_of_death, 
                                     "days"))

# Count time_to_death

other_final_outcome_deaths %>% count(time_to_death)




### 4 Output ----

saveRDS(other_final_outcome, 
        paste0(output_path, "other_final_outcome_", year, month, ".rds"))

saveRDS(other_final_outcome_deaths, 
        paste0(output_path, "other_final_outcome_with_NRS_deaths", year, month, 
               ".rds"))

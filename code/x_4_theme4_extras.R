##########################################################
# x_4_theme4_extras.R
# Karen Hotopp
# 21/10/2022
# 
# Translation of SPSS file '1. Vascular outcomes recorded.sps'
# Part of Theme 4 for AAA KPIs
# Takes the processed BOXI extracts and creates tables detailing
# vascular surgery outcomes:
# - AAA size outcomes
# - Vascular tracker update
# - Deaths pre-surgical assessment
# - MEG letters
# 
# Written/run on R Studio Server
# R version 3.6.1
##########################################################


##!! The portion of this that goes into the MEG workbook has been moved. The 
## below is extra code that may have already been rewritten (to check quarterly
## extract for vascular tracker update) or needs to be checked if it still needs
## to be included. Is the MEG reports the correct place for this? Or are they 
## independent analyses that should sit outside the MEG reporting??



#### 1: Housekeeping ####
## Packages
library(dplyr)
library(tidylog)


rm(list = ls())
gc()


source(here::here("code/0_housekeeping_theme_4.R"))

rm(fy_tibble, hb_list, kpi_report_years, output_path)

resout_list <- tibble(result_outcome = c('99','98','01','03','04','05','06','07',
                                         '08','11','12','13','15','16','20','97',
                                         '09','10','14','17','18','19', '96'))




## GW requested for meeting 23Oct2022
#
# greater_hbs <- vasc %>% 
#   filter(result_size == 1) %>% 
#   mutate(count = 1) %>% 
#   group_by(financial_year, hbres, outcome_type, result_outcome) %>%
#   summarize(cases = sum(count)) %>% 
#   ungroup() %>% 
#   pivot_wider(names_from = financial_year, values_from = cases) %>% 
#   glimpse()
# 
# library(openxlsx)
# header_style <- createStyle(halign = "center", textDecoration = "bold")
# 
# wb <- createWorkbook()
# 
# addWorksheet(wb, "Referrals by HB")
# writeData(wb, "Referrals by HB", greater_hbs, headerStyle = header_style)
# freezePane(wb, "Referrals by HB", firstRow = TRUE)
# saveWorkbook(wb, file = paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209",
#                                "/output/Vascular_referrals_HB.xlsx"), 
#                                overwrite = TRUE)


#### 4: Management Information ####

### A: Vascular Tracker Update ----
# Extract details of non-final outcomes and other final outcomes to provide 
# to NSD. This information is then tied into the vascular referral tracker
# to inform the MEG of their latest status.

track <- vasc %>% 
  filter(result_size == 1) %>% 
  filter(outcome_type == 2 |
           (result_outcome == "20" & date_screen > as.Date("2019-04-01"))) %>% 
  select(outcome_type, financial_year, upi,
         date_screen, hbres, result_outcome) %>% 
  arrange(outcome_type, date_screen)

track_noCHI <- track %>% 
  select(-upi)

write_rds(track, paste0(wd_path, "/temp/4_tracker_CHI.rds"))
write_rds(track_noCHI, paste0(wd_path, "/temp/4_tracker_noCHI.rds"))


### B: Deaths Pre-surgical Assessment ----
# Extract information of patients who died before reaching their 
# surgical assessment

mort <- vasc %>% 
  filter(result_outcome == "07") %>% 
  mutate(screen_death = date_death - date_screen,
         result_outcome = "Died before surgical assessment completed") %>% 
  arrange(date_screen) %>% 
  select(financial_year, upi, hbres, date_screen,
         date_death, result_outcome, screen_death) %>% 
  glimpse()

write_rds(mort, paste0(wd_path, "/temp/4_mortalities_CHI.rds"))


### C: CHIs for MEG Letters ----
# Extract CHI numbers for relevant vascular referrals
# THIS IS RUN AFTER MEG DISCUSSION!

letter <- vasc %>%
  filter(result_size == 1)

##!! Ask MEG what is actually needed in letters 

# # From SPSS:
# # Died before surgical assessment completed.
# filter(result_outcome == "07") #%>% 
# # Appropriate for treatment: died within 30 days of surgery
# filter(result_outcome == "16") #%>% 
# # Non-final outcome in any time period (outcome_type = 2) or 
# # Other final outcome (result outcome = 20) in latest year.
# filter(outcome_type = 2) | (financial_year == "2021/22" & result_outcome == "20")
# 
# # Nov2020 MEG: chi numbers needed for:
# # (a) Other final outcome (result outcome = 20) in 2018/2019 and 2019/20
# # (b) Ongoing assessment by vascular (result outcome=18 ) in 2018/19.
# filter((financial_year %in% c("2018/19", "2019/20") & result_outcome == "20") |
#   (financial_year == "2018/19" & result_outcome == "18"))

write_rds(letter, paste0(wd_path, "/temp/4_letters_CHI.rds"))


## The following code may be useful for looking into vascular referrals and 
## outcomes. The code takes the other final outcomes from vascular surgery
## and matches with NRS deaths records. The quarterly extracts currently produce
## a file covering other final outcomes, but does not cross this with the NRS 
## deaths records.


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

library(odbc)
library(dplyr)
library(readr)
library(lubridate)
library(janitor)
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

aaa_extract <- read_rds(extract_path)

# Filter for other final outcome and select columns
other_final_outcome <- aaa_extract %>% 
  filter(result_outcome == "20") %>% 
  select(upi, chi, dob, date_screen, hbres, date_seen_outpatient, date_surgery, 
         surg_method)

check <- aaa_extract |> 
  # Check how many had a procedure abandoned and what result outcome recorded was
  filter(surg_method == "03") %>% 
  count(result_outcome)


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
saveRDS(other_final_outcome_deaths, 
        paste0(output_path, "other_final_outcome_with_NRS_deaths", year, month, 
               ".rds"))


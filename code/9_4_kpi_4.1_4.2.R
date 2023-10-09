###############################################################################
# kpi_4.1_4.2.R
# Calum Purdie
# 17/01/2023
# 
# KPI 4.1 - 30 day mortality rate following open elective surgery 
# KPI 4.2 - 30 day mortality rate following EVAR elective surgery
#
# Written/run on R Studio Server
# R version 3.6.1
###############################################################################

### 1 Housekeeping ----

# Load packages

library(dplyr)
library(readr)
library(phsmethods)
# library(stringr)
# library(lubridate)
# library(janitor)
# library(zoo)
# library(tidylog)

rm(list = ls())
gc()


source(here::here("code/0_housekeeping_theme_4.R"))

rm(exclusions_path)


# Define date values

# year <- 2023
# month <- "03"

cut_off_date <- as.Date("2023-03-31")


### 2 Data Manipulation ----
# Final outcome pending (19) is not included in this KPI denominator.
# 15 - 'Appropriate for Surgery: AAA repaired and survived 30 days' 
# 16 - 'Appropriate for Surgery: Died within 30 days of treatment' 
aaa_extract <- read_rds(extract_path) %>% 
  # referred to vascular
  filter(!is.na(date_referral_true) & largest_measure >= 5.5, 
         date_surgery <= cut_off_date, 
         result_outcome %in% c("15", "16")) %>% 
  mutate(financial_year_surg = extract_fin_year(date_surgery))


### 3 KPI 4.1 Open surgery ----
kpi_4_1 <- aaa_extract %>% 
  filter(surg_method == "02")

# Check number of days to death is consistent with result outcomes
# Outcome 15 = survived 30 days, outcome 16 = died within 30 days of surgery
kpi_4_1 %>% 
  mutate(surgery_to_death = time_length(date_surgery %--% date_death, 
                                        "days")) %>% 
  count(result_outcome, surgery_to_death)

# calculate open surgeries by financial year
open_surgery <- kpi_4_1 %>%
  group_by(financial_year_surg) %>%
  summarise(procedures_n = n(), 
            deaths_n = sum(result_outcome == "16")) %>% 
  ungroup()

# Calculate deaths from open surgeries by financial year and health board
# Filter where result_outcome is "16" to get deaths
# Group by financial_year and count procedures and sum where result_outcome is
# "16" for deaths
# Count financial_year and hb_screen and set name as deaths

open_surgery_deaths <- kpi_4_1 %>%
  filter(result_outcome == "16") %>%
  count(financial_year_surg, hb_screen, name = "deaths")

# Calculate rolling total deaths from open surgeries
# Calculate five-year rolling deaths and surgeries and define rolling year
# Remove rows where rolling_financial_year contains NA
# Select columns and calculate percentage deaths

open_surgery_totals <- open_surgery %>%
  mutate(deaths = rollapply(deaths, 5, sum, align = "left", fill = NA), 
         surgeries = rollapply(procedures, 5, sum, align = "left", 
                                   fill = NA), 
         rolling_financial_year = paste0(financial_year_surg, " - ", 
                                         lead(financial_year_surg, 4))) %>% 
  filter(!(str_detect(rolling_financial_year, "NA"))) %>% 
  select(rolling_financial_year, surgeries, deaths) %>%
  mutate(pc_deaths = round_half_up(deaths * 100 / surgeries, 1)) 





### 4 KPI 4.2 ----

# Keep evar surgery records

kpi_4_2_data <- aaa_extract %>% 
  filter(surg_method == "01")

# Calculate days from surgery to death
# Check number of days to death is consistent with result outcomes
# Outcome 15 = survived 30 days, outcome 16 = died within 30 days of surgery

kpi_4_2_data %>% 
  mutate(surgery_to_death = time_length(date_surgery %--% date_death, 
                                        "days")) %>% 
  count(result_outcome, surgery_to_death)

# Calculate evar surgeries by financial year
# Group by financial_year and count procedures and sum where result_outcome is
# "16" for deaths

evar_surgery <- kpi_4_2_data %>%
  group_by(financial_year_surg) %>%
  summarise(procedures = n(), 
            deaths = sum(result_outcome == "16")) %>% 
  ungroup()

# Calculate deaths from evar surgeries by financial year and health board
# Filter where result_outcome is "16" to get deaths
# Group by financial_year and count procedures and sum where result_outcome is
# "16" for deaths
# Count financial_year and hb_screen and set name as deaths

evar_surgery_deaths <- kpi_4_2_data %>%
  filter(result_outcome == "16") %>%
  count(financial_year_surg, hb_screen, name = "deaths")

# Calculate rolling total deaths from evar surgeries
# Calculate five-year rolling deaths and surgeries and define rolling year
# Remove rows where rolling_financial_year contains NA
# Select columns and calculate percentage deaths

evar_surgery_totals <- evar_surgery %>%
  mutate(deaths = rollapply(deaths, 5, sum, align = "left", fill = NA), 
         surgeries = rollapply(procedures, 5, sum, align = "left", 
                               fill = NA), 
         rolling_financial_year = paste0(financial_year_surg, " - ", 
                                         lead(financial_year_surg, 4))) %>% 
  filter(!(str_detect(rolling_financial_year, "NA"))) %>% 
  select(rolling_financial_year, surgeries, deaths) %>%
  mutate(pc_deaths = round_half_up(deaths * 100 / surgeries, 1)) 



# ### 5 Output ----
# 
# # CP - SHOULD THESE BE RDS NOW?
# 
# # Save KPI 4.1 output
# 
# write.xlsx(open_surgery, 
#            paste0(output_additional_path, "/1. open surgeries by year.xlsx"))
# 
# write.xlsx(open_surgery_deaths, 
#            paste0(output_additional_path, 
#                   "/2. 30-day deaths by hb and year - open.xlsx"))
# 
# write.xlsx(open_surgery_totals, 
#            paste0(output_additional_path, "/3. open rolling tot deaths.xlsx"))
# 
# # Save KPI 4.2 output
# 
# write.xlsx(evar_surgery, 
#            paste0(output_additional_path, "/4. evar surgeries by year.xlsx"))
# 
# write.xlsx(evar_surgery_deaths, 
#            paste0(output_additional_path, 
#                   "/5. 30-day deaths by hb and year - evar.xlsx"))
# 
# write.xlsx(evar_surgery_totals, 
#            paste0(output_additional_path, "/6. evar rolling tot deaths.xlsx"))

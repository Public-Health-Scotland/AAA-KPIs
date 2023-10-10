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
library(odbc)
library(dplyr)
library(readr)
library(lubridate)
library(phsmethods)
library(stringr)
library(janitor)
library(zoo)
library(tidylog)


source(here::here("code/0_housekeeping_theme_4.R"))


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
# Important bit here is the surg_method filter
aaa_extract <- read_rds(paste0(extract_path)) %>% 
  filter(!is.na(date_referral_true) & largest_measure >= 5.5, 
         date_surgery <= cut_off_date, 
         surg_method %in% c("01", "02"), 
         result_outcome %in% c("15", "16")) %>% 
  select(upi, dob, date_screen, hbres, hb_surgery, date_surgery, 
         result_outcome, date_death, surg_method, financial_year) %>% 
  mutate(financial_year_surg = extract_fin_year(date_surgery))


# Deaths data
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

deaths <- deaths %>% 
  # calculate year of death
  mutate(year_of_death = year(date_of_death)) %>% 
  arrange(upi, date_of_death) %>% 
  group_by(upi) %>% 
  # de-duplicate where individual has 2+ death records (takes last row of group)
  slice(n()) %>% 
  ungroup()

# Join deaths data onto aaa_extract
aaa_extract <- aaa_extract %>% 
  left_join(deaths) %>% 
  mutate(surgery_to_death = time_length(date_surgery %--% date_of_death, 
                                        "days"))

##
# Check: date_death from aaa_extract matches date_of_death from deaths
aaa_extract %>% count(date_death == date_of_death)
##


### 3 CUMULATIVE Mortality Output ----
# Create flags for people who had surgery within last 1, 3 or 5 years
# Create flags for people who had surgery within last 1, 3 or 5 years and 
# died within 1, 3 or 5 years respectively
# CP - THIS CURRENTLY SPECIFIES NUMBER OF DAYS - SHOULD WE CHANGE TO SUBTRACTING YEARS INSTEAD???
mortality <- aaa_extract %>% 
  filter(surg_method %in% c("01", "02")) |> 
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


### Create 1-year Cumulative Mortality Totals ----
fy_mortality <- mortality %>% 
  group_by(surg_method, financial_year_surg) %>% 
  summarise(across(c(mort_1_year, surg_1_year), sum)) %>% 
  arrange(financial_year_surg) |> 
  ungroup()

### Open Surgery ---
open_1yr_mort <- fy_mortality %>%
  filter(surg_method == "02") %>% 
  mutate(deaths_n = rollapply(mort_1_year, 5, sum, align = "left", fill = NA), 
         surgeries_n = rollapply(surg_1_year, 5, sum, align = "left", 
                               fill = NA), 
         rolling_financial_year = paste0(financial_year_surg, " - ", 
                                         lead(financial_year_surg, 4))) %>% 
  filter(!(str_detect(rolling_financial_year, "NA"))) %>% 
  select(rolling_financial_year, surgeries_n, deaths_n) %>%
  mutate(deaths_p = round_half_up(deaths_n * 100 / surgeries_n, 1)) %>%
  mutate(surgery_type = "Open", .before = rolling_financial_year) |> 
  mutate(kpi = "KPI 4.2 1yr Cumulative", .before = surgery_type)

### EVAR Surgery ---
evar_1yr_mort <- fy_mortality %>%
  filter(surg_method == "01") %>% 
  mutate(deaths_n = rollapply(mort_1_year, 5, sum, align = "left", fill = NA), 
         surgeries_n = rollapply(surg_1_year, 5, sum, align = "left", 
                               fill = NA), 
         rolling_financial_year = paste0(financial_year_surg, " - ", 
                                         lead(financial_year_surg, 4))) %>% 
  filter(!(str_detect(rolling_financial_year, "NA"))) %>% 
  select(rolling_financial_year, surgeries_n, deaths_n) %>%
  mutate(deaths_p = round_half_up(deaths_n * 100 / surgeries_n, 1)) %>%
  mutate(surgery_type = "EVAR", .before = rolling_financial_year) |> 
  mutate(kpi = "KPI 4.2 1yr Cumulative", .before = surgery_type)




### 1, 3, 5-year Cumulative Mortalities ----
# Scotland summaries
mortality_scotland <- mortality %>% 
  group_by(surg_method) %>% 
  summarise(across(c(mort_1_year, mort_3_year, mort_5_year, 
                     surg_1_year, surg_3_year, surg_5_year), sum)) %>% 
  mutate(rate_1_year = round_half_up(mort_1_year * 100 / surg_1_year, 1), 
         rate_3_year = round_half_up(mort_3_year * 100 / surg_3_year, 1), 
         rate_5_year = round_half_up(mort_5_year * 100 / surg_5_year, 1)) |> 
  ungroup() |> 
  mutate(hbres = "Scotland", .after = surg_method)


# HB of Residence summary
mortality_hb_res <- mortality %>% 
  group_by(surg_method, hbres) %>% 
  summarise(across(c(surg_1_year, surg_3_year, surg_5_year,  
                     mort_1_year, mort_3_year, mort_5_year), sum)) %>% 
  mutate(rate_1_year = round_half_up(mort_1_year * 100 / surg_1_year, 1), 
         rate_3_year = round_half_up(mort_3_year * 100 / surg_3_year, 1), 
         rate_5_year = round_half_up(mort_5_year * 100 / surg_5_year, 1)) |> 
  ungroup()

# Join
mortality_hb_resa <- mortality_hb |>   
  bind_rows(mortality_scotland) %>% 
  # move Scotland to top of HB list, then alphabetical order
  arrange(hbres != "Scotland", hbres) %>% 
  mutate(surg_method = case_when(surg_method == "01" ~ "EVAR", 
                                 surg_method == "02" ~ "Open surgery"),
         # this doesn't seem to be reordering the surg_method!!!
         surg_method = forcats::fct_relevel(surg_method, c("Open surgery", 
                                                           "EVAR"))) %>% 
  select(surg_method, hbres, ends_with("_1_year"), ends_with("_3_year"), 
         ends_with("_5_year"))


# HB of Surgery summary
mortality_hb_surg <- mortality %>% 
  group_by(surg_method, hb_surgery) %>% 
  summarise(across(c(surg_1_year, surg_3_year, surg_5_year,  
                     mort_1_year, mort_3_year, mort_5_year), sum)) %>% 
  mutate(rate_1_year = round_half_up(mort_1_year * 100 / surg_1_year, 1), 
         rate_3_year = round_half_up(mort_3_year * 100 / surg_3_year, 1), 
         rate_5_year = round_half_up(mort_5_year * 100 / surg_5_year, 1)) |> 
  ungroup()

# Join
mortality_hb_surga <- mortality_hb_surg |>   
  bind_rows(mortality_scotland) %>% 
  # move Scotland to top of HB list, then alphabetical order
  arrange(hbres != "Scotland", hb_surgery) %>% 
  mutate(surg_method = case_when(surg_method == "01" ~ "EVAR", 
                                 surg_method == "02" ~ "Open surgery"),
         # this doesn't seem to be reordering the surg_method!!!
         surg_method = forcats::fct_relevel(surg_method, c("Open surgery", 
                                                           "EVAR"))) %>% 
  select(surg_method, hbres, ends_with("_1_year"), ends_with("_3_year"), 
         ends_with("_5_year"))

##!! Move this to 9_4_kpi_4.1_4.2.R script and check formatting!!

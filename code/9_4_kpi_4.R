###############################################################################
# 9_4_kpi_4.R
# Calum Purdie & Karen Hotopp
# 17/01/2023
# 
# KPI 4.1 - 30-day mortality rate following open elective surgery 
# KPI 4.2 - 30-day mortality rate following EVAR elective surgery
# 1, 3 and 5 year mortality following EVAR and open AAA surgery
#
# Written/run on R Studio Server, R version 3.6.1
# Revised/Run on Posit WB (R version 4.1.2)
###############################################################################

## Notes: 
## From Sept 2023, KPI 4.1 & 4.2 now includes analysis by HB of surgery (in  
# addition to HB of screening)
# Cohort is defined using date of surgery


### 1: Housekeeping ----

# Load packages
library(odbc)
library(dplyr)
library(readr)
library(phsmethods)
library(lubridate)
library(zoo)
library(stringr)
library(janitor)
library(tidylog)

rm(list = ls())
gc()


source(here::here("code/0_housekeeping_theme_4.R"))


# Define cumulative cut-off years
cut_off_date_1 <- cut_off_date - years(1)
cut_off_date_3 <- cut_off_date - years(3)
cut_off_date_5 <- cut_off_date - years(5)


### 2: Import AAA Data ----
# result_outcome:
# 15 - 'Appropriate for Surgery: AAA repaired and survived 30 days' 
# 16 - 'Appropriate for Surgery: Died within 30 days of treatment' 
# Final outcome pending (19) is not included in this KPI denominator.
aaa_extract <- read_rds(extract_path) %>% 
  # referred to vascular
  filter(!is.na(date_referral_true) & largest_measure >= 5.5, 
         date_surgery <= cut_off_date, 
         result_outcome %in% c("15", "16")) %>% 
  select(upi, dob, date_screen, hbres, hb_screen, hb_surgery, date_surgery, 
         result_outcome, date_death, surg_method, financial_year) %>% 
  mutate(financial_year_surg = extract_fin_year(date_surgery)) |> 
  mutate(hb_surgery = case_when(hb_surgery == "A" ~ "Ayrshire & Arran",
                                hb_surgery == "B" ~ "Borders",
                                hb_surgery == "D" ~ "Cumbria", #**
                                hb_surgery == "F" ~ "Fife",
                                hb_surgery == "G" ~ "Greater Glasgow & Clyde",
                                hb_surgery == "H" ~ "Highland",
                                hb_surgery == "L" ~ "Lanarkshire",
                                hb_surgery == "N" ~ "Grampian",
                                hb_surgery == "R" ~ "Orkney", # Needed?
                                hb_surgery == "S" ~ "Lothian",
                                hb_surgery == "T" ~ "Tayside",
                                hb_surgery == "V" ~ "Forth Valley",
                                hb_surgery == "W" ~ "Western Isles", # Needed?
                                hb_surgery == "Y" ~ "Dumfries & Galloway",
                                hb_surgery == "Z" ~ "Shetland")) # Needed?

table(aaa_extract$hb_surgery, useNA = "ifany")

# Check: number of days to death should be consistent with result outcomes
# (where result_outcome = 15, surgery_to_death should be > 30 (or NA) and  
# where result_outcome = 16, surgery_to_death should be 0-30)
aaa_extract %>% 
  mutate(surgery_to_death = time_length(date_surgery %--% date_death, 
                                        "days")) %>% 
  count(result_outcome, surgery_to_death)


### 3: KPI 4.1 Open surgery ----
kpi_4_1 <- aaa_extract %>% 
  filter(surg_method == "02")

## Open surgeries by financial year ---
open_surgery <- kpi_4_1 %>%
  group_by(financial_year_surg) %>%
  summarise(procedures_n = n(), 
            deaths = sum(result_outcome == "16")) %>% 
  ungroup() |> 
  # cumulative totals for procedures and deaths  
  group_modify(~ adorn_totals(.x, where = "row", 
                              name = "Cumulative")) |> 
  mutate(kpi = "KPI 4.1 Additional A", .before = financial_year_surg) |> 
  mutate(surg_method = "Open", .before = financial_year_surg) 
  
## 5-year rolling open surgery mortalities (30-day) ---
open_surgery_roll <- open_surgery %>%
  select(!kpi) |> 
  # calculate rolling totals
  mutate(deaths_n = rollapply(deaths, 5, sum, align = "left", fill = NA), 
         surgeries_n = rollapply(procedures_n, 5, sum, align = "left", 
                               fill = NA), 
         rolling_financial_year = paste0(financial_year_surg, " - ", 
                                         lead(financial_year_surg, 4))) %>% 
  filter(!(str_detect(rolling_financial_year, "NA")),
         !(str_detect(rolling_financial_year, "Cumulative"))) %>% 
  select(surg_method, rolling_financial_year, surgeries_n, deaths_n) |> 
  mutate(kpi = "KPI 4.1", .before = surg_method) %>%
  mutate(deaths_p = round_half_up(deaths_n * 100 / surgeries_n, 1)) 

## Open surgery deaths by financial year and health board of SCREENING ---
open_hb_screen <- kpi_4_1 %>%
  filter(result_outcome == "16") %>%
  count(financial_year_surg, hb_screen, name = "deaths")

open_hb_screen_scot <- kpi_4_1 %>%
  filter(result_outcome == "16") %>%
  count(financial_year_surg, name = "deaths") |> 
  mutate(hb_screen = "Scotland", .after = financial_year_surg)

open_hb_screen <- bind_rows(open_hb_screen, open_hb_screen_scot) |> 
  pivot_wider(names_from = hb_screen, values_from = deaths)
  
open_hb_screen <- fy_list |> left_join(open_hb_screen, 
                                       by = c("financial_year" = 
                                                "financial_year_surg")) |> 
  # cumulative totals for HBs  
  group_modify(~ adorn_totals(.x, where = "row", 
                              name = "Cumulative")) |> 
  mutate(kpi = "KPI 4.1 Add B: Screen", .before = financial_year) |> 
  mutate(surg_method = "Open", .before = financial_year)

## Open surgery deaths by financial year and health board of SURGERY ---
open_hb_surgery <- kpi_4_1 %>%
  filter(result_outcome == "16") %>%
  count(financial_year_surg, hb_surgery, name = "deaths")

open_hb_surgery_scot <- kpi_4_1 %>%
  filter(result_outcome == "16") %>%
  count(financial_year_surg, name = "deaths") |> 
  mutate(hb_surgery = "Scotland", .after = financial_year_surg)

open_hb_surgery <- bind_rows(open_hb_surgery, open_hb_surgery_scot) |> 
  pivot_wider(names_from = hb_surgery, values_from = deaths)

open_hb_surgery <- fy_list |> left_join(open_hb_surgery, 
                                        by = c("financial_year" = 
                                                 "financial_year_surg")) |> 
  # cumulative totals for HBs  
  group_modify(~ adorn_totals(.x, where = "row", 
                              name = "Cumulative")) |> 
  mutate(kpi = "KPI 4.1 Add C: Surgery", .before = financial_year) |> 
  mutate(surg_method = "Open", .before = financial_year)


### 4: KPI 4.2 EVAR Surgery ----
kpi_4_2 <- aaa_extract %>% 
  filter(surg_method == "01")

## EVAR surgeries by financial year ---
evar_surgery <- kpi_4_2 %>%
  group_by(financial_year_surg) %>%
  summarise(procedures_n = n(), 
            deaths = sum(result_outcome == "16")) %>% 
  ungroup() |> 
  # cumulative totals for procedures and deaths  
  group_modify(~ adorn_totals(.x, where = "row", 
                              name = "Cumulative")) |> 
  mutate(kpi = "KPI 4.2 Additional A", .before = financial_year_surg) |> 
  mutate(surg_method = "EVAR", .before = financial_year_surg)

## 5-year rolling EVAR surgery mortalities (30-day) ---
evar_surgery_roll <- evar_surgery %>%
  select(-kpi) |> 
  # calculate rolling totals
  mutate(deaths_n = rollapply(deaths, 5, sum, align = "left", fill = NA), 
         surgeries_n = rollapply(procedures_n, 5, sum, align = "left", 
                               fill = NA), 
         rolling_financial_year = paste0(financial_year_surg, " - ", 
                                         lead(financial_year_surg, 4))) %>% 
  filter(!(str_detect(rolling_financial_year, "NA")),
         !(str_detect(rolling_financial_year, "Cumulative"))) %>% 
  select(surg_method, rolling_financial_year, surgeries_n, deaths_n) |> 
  mutate(kpi = "KPI 4.2", .before = surg_method) %>%
  mutate(deaths_p = round_half_up(deaths_n * 100 / surgeries_n, 1)) 

## EVAR surgery deaths by financial year and health board by SCREENING ---
evar_hb_screen <- kpi_4_2 %>%
  filter(result_outcome == "16") %>%
  count(financial_year_surg, hb_screen, name = "deaths")

evar_hb_screen_scot <- kpi_4_2 %>%
  filter(result_outcome == "16") %>%
  count(financial_year_surg, name = "deaths") |> 
  mutate(hb_screen = "Scotland", .after = financial_year_surg)

evar_hb_screen <- bind_rows(evar_hb_screen, evar_hb_screen_scot) |> 
  pivot_wider(names_from = hb_screen, values_from = deaths)

evar_hb_screen <- fy_list |> left_join(evar_hb_screen, 
                                        by = c("financial_year" = 
                                                 "financial_year_surg")) |> 
  # cumulative totals for HBs  
  group_modify(~ adorn_totals(.x, where = "row", 
                              name = "Cumulative")) |> 
  mutate(kpi = "KPI 4.2 Add B: Screen", .before = financial_year) |> 
  mutate(surg_method = "EVAR", .before = financial_year)

## EVAR surgery deaths by financial year and health board by SURGERY ---
evar_hb_surgery <- kpi_4_2 %>%
  filter(result_outcome == "16") %>%
  count(financial_year_surg, hb_surgery, name = "deaths")

evar_hb_surgery_scot <- kpi_4_2 %>%
  filter(result_outcome == "16") %>%
  count(financial_year_surg, name = "deaths") |> 
  mutate(hb_surgery = "Scotland", .after = financial_year_surg)

evar_hb_surgery <- bind_rows(evar_hb_surgery, evar_hb_surgery_scot) |> 
  pivot_wider(names_from = hb_surgery, values_from = deaths)

evar_hb_surgery <- fy_list |> left_join(evar_hb_surgery, 
                                       by = c("financial_year" = 
                                                "financial_year_surg")) |> 
  # cumulative totals for HBs  
  group_modify(~ adorn_totals(.x, where = "row", 
                              name = "Cumulative")) |> 
  mutate(kpi = "KPI 4.2 Add C: Surgery", .before = financial_year) |> 
  mutate(surg_method = "EVAR", .before = financial_year)

rm(fy_list, hb_list, open_hb_screen_scot, open_hb_surgery_scot, 
   kpi_4_1, kpi_4_2, evar_hb_screen_scot, evar_hb_surgery_scot)


### 5: Import Deaths Data ----
## Call in Deaths data ---
# Connect to SMRA tables using odbc connection
# The suppressWarnings function prevents your password from appearing in the
# console if the connection is unsuccessful
smra_con <- suppressWarnings(dbConnect(
  odbc(),
  dsn = "SMRA",
  uid = .rs.askForPassword("What is your user ID?"),
  pwd = .rs.askForPassword("What is your LDAP password?")))

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
  filter(surg_method %in% c("01", "02")) |> 
  left_join(deaths) %>% 
  mutate(surgery_to_death = time_length(date_surgery %--% date_of_death, 
                                        "days"))

##
# Check: date_death from aaa_extract matches date_of_death from deaths
aaa_extract %>% count(date_death == date_of_death)
##


### 6: Cumulative Mortality by Surgery Type ----
# Flag individuals who had surgery within last 1, 3 or 5 years
# Flag individuals who had surgery within last 1, 3 or 5 years and died within 
# 1, 3 or 5 years, respectively
# CP - THIS CURRENTLY SPECIFIES NUMBER OF DAYS - SHOULD WE CHANGE TO SUBTRACTING YEARS INSTEAD???
mortality <- aaa_extract %>% 
  mutate(surg_1_year = case_when(date_surgery <= cut_off_date_1 ~ 1, 
                                 TRUE ~ 0), 
         surg_3_year = case_when(date_surgery < cut_off_date_3 ~ 1, 
                                 TRUE ~ 0), 
         surg_5_year = case_when(date_surgery < cut_off_date_5 ~ 1, 
                                 TRUE ~ 0)) %>% 
  mutate(mort_1_year = 
           case_when(surg_1_year == 1 & surgery_to_death <= 365 ~ 1, # 1 year
                     TRUE ~ 0), 
         mort_3_year = 
           case_when(surg_3_year == 1 & surgery_to_death <= 1095 ~ 1, # 3 year 
                     TRUE ~ 0), 
         mort_5_year = 
           case_when(surg_5_year == 1 & surgery_to_death <= 1825 ~ 1, # 5 year 
                     TRUE ~ 0))

# Check mortality counts
mortality %>% count(mort_1_year)
mortality %>% count(mort_3_year)
mortality %>% count(mort_5_year)


## 5-year rolling surgery mortalities (1-year rate) ---
fy_mortality <- mortality %>% 
  group_by(surg_method, financial_year_surg) %>% 
  summarise(across(c(mort_1_year, surg_1_year), sum)) %>% 
  arrange(financial_year_surg) |> 
  ungroup()

## Open Surgery
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
  mutate(surg_method = "Open", .before = rolling_financial_year) |> 
  mutate(kpi = "KPI 4.1 1yr Rate", .before = surg_method)

## EVAR Surgery
evar_1yr_mort <- fy_mortality %>%
  filter(surg_method == "01") %>% 
  # calculate rolling totals
  mutate(deaths_n = rollapply(mort_1_year, 5, sum, align = "left", fill = NA), 
         surgeries_n = rollapply(surg_1_year, 5, sum, align = "left", 
                                 fill = NA), 
         rolling_financial_year = paste0(financial_year_surg, " - ", 
                                         lead(financial_year_surg, 4))) %>% 
  filter(!(str_detect(rolling_financial_year, "NA"))) %>% 
  select(rolling_financial_year, surgeries_n, deaths_n) %>%
  mutate(deaths_p = round_half_up(deaths_n * 100 / surgeries_n, 1)) %>%
  mutate(surg_method = "EVAR", .before = rolling_financial_year) |> 
  mutate(kpi = "KPI 4.2 1yr Rate", .before = surg_method)


## 1, 3, 5-year Cumulative Mortalities ---
## Scotland summaries
mortality_scotland <- mortality %>% 
  group_by(surg_method) %>% 
  summarise(across(c(mort_1_year, mort_3_year, mort_5_year, 
                     surg_1_year, surg_3_year, surg_5_year), sum)) %>% 
  mutate(rate_1_year = round_half_up(mort_1_year * 100 / surg_1_year, 1), 
         rate_3_year = round_half_up(mort_3_year * 100 / surg_3_year, 1), 
         rate_5_year = round_half_up(mort_5_year * 100 / surg_5_year, 1)) |> 
  ungroup() |> 
  mutate(hbres = "Scotland", .after = surg_method) |> 
  mutate(kpi = "KPI 4 Cumulative", .before = surg_method)

## HB of Residence summary
mortality_hb_res <- mortality %>% 
  group_by(surg_method, hbres) %>% 
  summarise(across(c(surg_1_year, surg_3_year, surg_5_year,  
                     mort_1_year, mort_3_year, mort_5_year), sum)) %>% 
  mutate(rate_1_year = round_half_up(mort_1_year * 100 / surg_1_year, 1), 
         rate_3_year = round_half_up(mort_3_year * 100 / surg_3_year, 1), 
         rate_5_year = round_half_up(mort_5_year * 100 / surg_5_year, 1)) |> 
  ungroup() |> 
  mutate(kpi = "KPI 4 Cumulative: Residence", .before = surg_method) 
  
# Join
mortality_hb_res <- mortality_hb_res |>   
  bind_rows(mortality_scotland) %>% 
  mutate(surg_method = case_when(surg_method == "01" ~ "EVAR", 
                                 surg_method == "02" ~ "Open surgery"),
         surg_method = forcats::fct_relevel(surg_method, c("Open surgery", 
                                                           "EVAR"))) %>% 
  arrange(surg_method) |> 
  # move Scotland to top of HB list, then alphabetical order
  arrange(hbres != "Scotland", hbres) %>% 
  select(kpi, surg_method, hbres, ends_with("_1_year"), ends_with("_3_year"), 
         ends_with("_5_year")) |> 
  rename(health_board = hbres)

## HB of Surgery summary
mortality_hb_surg <- mortality %>% 
  group_by(surg_method, hb_surgery) %>% 
  summarise(across(c(surg_1_year, surg_3_year, surg_5_year,  
                     mort_1_year, mort_3_year, mort_5_year), sum)) %>% 
  mutate(rate_1_year = round_half_up(mort_1_year * 100 / surg_1_year, 1), 
         rate_3_year = round_half_up(mort_3_year * 100 / surg_3_year, 1), 
         rate_5_year = round_half_up(mort_5_year * 100 / surg_5_year, 1)) |> 
  ungroup()

# No need to add Scotland, as this will be same as for residence

mortality_hb_surg <- mortality_hb_surg |>   
  mutate(surg_method = case_when(surg_method == "01" ~ "EVAR", 
                                 surg_method == "02" ~ "Open surgery"),
         surg_method = forcats::fct_relevel(surg_method, c("Open surgery", 
                                                           "EVAR"))) %>% 
  arrange(hb_surgery, surg_method) %>% 
  select(surg_method, hb_surgery, ends_with("_1_year"), ends_with("_3_year"), 
         ends_with("_5_year")) |> 
  mutate(kpi = "KPI 4 Cumulative: Surgery", .before = surg_method) |> 
  rename(health_board = hb_surgery)


### 7: Combine and Save ----
## KPI 4, KPI 4 additional A, KPI 4 1yr mortality rate
kpi_4_roll <- bind_rows(open_surgery_roll, evar_surgery_roll) |> 
  pivot_longer(!kpi:rolling_financial_year, 
               names_to = "group", values_to = "value") |> 
  rename(financial_year = rolling_financial_year)

kpi_4_surgery <- bind_rows(open_surgery, evar_surgery) |> 
  pivot_longer(!kpi:financial_year_surg, 
               names_to = "group", values_to = "value") |> 
  rename(financial_year = financial_year_surg)

kpi_4_1y_mort <- bind_rows(open_1yr_mort, evar_1yr_mort) |> 
  pivot_longer(!kpi:rolling_financial_year, 
               names_to = "group", values_to = "value") |> 
  rename(financial_year = rolling_financial_year)

kpi_4 <- bind_rows(kpi_4_roll, kpi_4_surgery, kpi_4_1y_mort)

table(kpi_4$kpi, kpi_4$surg_method)

## KPI 4 additional B and C
kpi_4_hb <- bind_rows(open_hb_screen, open_hb_surgery, 
                      evar_hb_screen, evar_hb_surgery) |> 
  relocate(Scotland, .after = last_col())

table(kpi_4_hb$kpi, kpi_4_hb$surg_method)

## Cumulative mortality rates (1, 3, 5-year)
kpi_4_mortality <- bind_rows(mortality_hb_res, mortality_hb_surg)

table(kpi_4_mortality$kpi, kpi_4_mortality$surg_method)


###
write_rds(kpi_4, paste0(temp_path, "/6_kpi_4_", yymm, ".rds"))
write_rds(kpi_4_hb, paste0(temp_path, "/7_kpi_4_HB_", yymm, ".rds"))
write_rds(kpi_4_mortality, paste0(temp_path, "/8_kpi_4_mortality_", yymm, ".rds"))


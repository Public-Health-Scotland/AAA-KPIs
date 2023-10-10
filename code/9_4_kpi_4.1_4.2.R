###############################################################################
# kpi_4.1_4.2.R
# Calum Purdie
# 17/01/2023
# 
# KPI 4.1 - 30 day mortality rate following open elective surgery 
# KPI 4.2 - 30 day mortality rate following EVAR elective surgery
#
# Written/run on R Studio Server, R version 3.6.1
# Revised/Run on Posit WB (R version 4.1.2)
###############################################################################

### 1: Housekeeping ----

# Load packages
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



### 2: Data Manipulation ----
# result_outcome:
# 15 - 'Appropriate for Surgery: AAA repaired and survived 30 days' 
# 16 - 'Appropriate for Surgery: Died within 30 days of treatment' 
# Final outcome pending (19) is not included in this KPI denominator.
aaa_extract <- read_rds(extract_path) %>% 
  # referred to vascular
  filter(!is.na(date_referral_true) & largest_measure >= 5.5, 
         date_surgery <= cut_off_date, 
         result_outcome %in% c("15", "16")) %>% 
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



### 3: KPI 4.1 Open surgery ----
kpi_4_1 <- aaa_extract %>% 
  filter(surg_method == "02")

##
# Check: number of days to death should be consistent with result outcomes
kpi_4_1 %>% 
  mutate(surgery_to_death = time_length(date_surgery %--% date_death, 
                                        "days")) %>% 
  count(result_outcome, surgery_to_death)
##

# Open surgeries by financial year
open_surgery <- kpi_4_1 %>%
  group_by(financial_year_surg) %>%
  summarise(procedures_n = n(), 
            deaths = sum(result_outcome == "16")) %>% 
  ungroup() |> 
  group_modify(~ janitor::adorn_totals(.x, where = "row", 
                                       name = "Cumulative")) |> 
  mutate(kpi = "KPI 4.1 Additional A", .before = financial_year_surg) |> 
  mutate(surgery_type = "Open", .before = financial_year_surg) 
  
# Deaths: 5-year rolling open surgery totals
open_surgery_roll <- open_surgery %>%
  select(!kpi) |> 
  mutate(deaths_n = rollapply(deaths, 5, sum, align = "left", fill = NA), 
         surgeries_n = rollapply(procedures_n, 5, sum, align = "left", 
                               fill = NA), 
         rolling_financial_year = paste0(financial_year_surg, " - ", 
                                         lead(financial_year_surg, 4))) %>% 
  filter(!(str_detect(rolling_financial_year, "NA")),
         !(str_detect(rolling_financial_year, "Cumulative"))) %>% 
  select(surgery_type, rolling_financial_year, surgeries_n, deaths_n) |> 
  mutate(kpi = "KPI 4.1", .before = surgery_type) %>%
  mutate(deaths_p = round_half_up(deaths_n * 100 / surgeries_n, 1)) 

# Deaths: open surgeries by financial year and health board of SCREENING
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
    group_modify(~ janitor::adorn_totals(.x, where = "row", 
                                         name = "Cumulative")) |> 
    mutate(kpi = "KPI 4.1 Add B: Screen", .before = fin_year) |> 
    mutate(surgery_type = "Open", .before = fin_year)
  
# Deaths: open surgeries by financial year and health board of SURGERY
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
  group_modify(~ janitor::adorn_totals(.x, where = "row", 
                                       name = "Cumulative")) |> 
  mutate(kpi = "KPI 4.1 Add B: Surgery", .before = fin_year) |> 
  mutate(surgery_type = "Open", .before = fin_year)


### 4: KPI 4.2 EVAR Surgery ----
kpi_4_2 <- aaa_extract %>% 
  filter(surg_method == "01")

##
# Check: number of days to death should be consistent with result outcomes
kpi_4_2 %>% 
  mutate(surgery_to_death = time_length(date_surgery %--% date_death, 
                                        "days")) %>% 
  count(result_outcome, surgery_to_death)
##

# EVAR surgeries by financial year
evar_surgery <- kpi_4_2 %>%
  group_by(financial_year_surg) %>%
  summarise(procedures_n = n(), 
            deaths = sum(result_outcome == "16")) %>% 
  ungroup() |> 
  group_modify(~ janitor::adorn_totals(.x, where = "row", 
                                       name = "Cumulative")) |> 
  mutate(kpi = "KPI 4.2 Additional A", .before = financial_year_surg) |> 
  mutate(surgery_type = "EVAR", .before = financial_year_surg)

# Deaths: 5-year rolling EVAR surgery totals
evar_surgery_roll <- evar_surgery %>%
  select(-kpi) |> 
  mutate(deaths_n = rollapply(deaths, 5, sum, align = "left", fill = NA), 
         surgeries_n = rollapply(procedures_n, 5, sum, align = "left", 
                               fill = NA), 
         rolling_financial_year = paste0(financial_year_surg, " - ", 
                                         lead(financial_year_surg, 4))) %>% 
  filter(!(str_detect(rolling_financial_year, "NA")),
         !(str_detect(rolling_financial_year, "Cumulative"))) %>% 
  select(surgery_type, rolling_financial_year, surgeries_n, deaths_n) |> 
  mutate(kpi = "KPI 4.2", .before = surgery_type) %>%
  mutate(deaths_p = round_half_up(deaths_n * 100 / surgeries_n, 1)) 

# Deaths: EVAR surgeries by financial year and health board by SCREENING
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
  group_modify(~ janitor::adorn_totals(.x, where = "row", 
                                       name = "Cumulative")) |> 
  mutate(kpi = "KPI 4.2 Add B: Screen", .before = fin_year) |> 
  mutate(surgery_type = "EVAR", .before = fin_year)

# Deaths: EVAR surgeries by financial year and health board by SURGERY
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
  group_modify(~ janitor::adorn_totals(.x, where = "row", 
                                       name = "Cumulative")) |> 
  mutate(kpi = "KPI 4.2 Add B: Surgery", .before = fin_year) |> 
  mutate(surgery_type = "EVAR", .before = fin_year)

rm(open_hb_screen_scot, open_hb_surgery_scot, 
   evar_hb_screen_scot, evar_hb_surgery_scot)


### 4: Combine  and Save ----
kpi_4_roll <- bind_rows(open_surgery_roll, evar_surgery_roll)
kpi_4_surgery <- bind_rows(open_surgery, evar_surgery)
kpi_4_hb <- bind_rows(open_hb_screen, open_hb_surgery, 
                      evar_hb_screen, evar_hb_surgery)

write_rds(kpi_4_roll, paste0(temp_path, "/6_kpi_4.rds"))
write_rds(kpi_4_surgery, paste0(temp_path, "/7_kpi_4_A.rds"))
write_rds(kpi_4_hb, paste0(temp_path, "/8_kpi_4_B.rds"))




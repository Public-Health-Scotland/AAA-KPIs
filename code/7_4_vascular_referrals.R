#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 7_4_vascular_referrals.R
# Calum Purdie & Karen Hotopp & Salomi Barkat
# 16/02/2023
# 
# Vascular referrals, operations, and outcomes
#
# Written/run on Posit Server
# R version 4.1.2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Notes:
# This script covers three parts of the theme 4 QPMG report:
# - Table 7: Vascular Referrals
# - Vascular Referral Outcomes
# - AAA Repair Operations


# 1: Housekeeping ----

# Load packages
library(dplyr)
library(readr)
library(janitor)
library(tidyr)
library(forcats)
library(tidylog)
library(phsaaa) # to install: devtools::install_github("aoifem01/phsaaa")

rm(list = ls())
gc()


source(here::here("code/0_housekeeping.R"))

rm (exclusions_path, hist_path, output_path, simd_path, hb_list, season,
    cutoff_date, end_current, end_date, start_date, qpmg_month, extract_date,
    year1_end, year1_start, year2_end, year2_start, year1, year2)

# list of result outcomes in custom order (99, 98, 97, 96 all newly assigned values)
resout_list <- tibble(result_outcome = c('99','98','01','03','04','05','06','07',
                                         '08','11','12','13','15','16','20','97',
                                         '09','10','14','17','18','19', '96'))
# AMc note: there is no code 2 here - and the order doesn't match the output - definitely needs lookig at

# adding in result outcome detail
resout_list <- resout_list %>% 
  mutate(outcome_type = case_when(result_outcome %in%
                                    c('01','02','03','04','05','06','07','08',
                                      '11','12','13','15','16','20') ~ "final outcome",
                                  result_outcome %in% c('09','10','14','17',
                                                        '18','19') ~ "non-final outcome",
                                  result_outcome == "99" ~ "Total",
                                  result_outcome == "98" ~ "Total: final outcome",
                                  result_outcome == "97" ~ "Total: non-final outcome",
                                  result_outcome == "96" ~ "Total: no outcome recorded"))


# Table 7 Vascular Referrals ----------------------------------------------


## 2: Data Extraction ----
# Keep where they were referred to vascular, had a large aneurysm, date_screen
# is less than or equal to cut_off_date and result_outcome is not referred in 
# error
aaa_extract <- read_rds(extract_path) %>% 
  # referred to vascular
  filter(!is.na(date_referral_true) & largest_measure >= 5.5,
         date_screen <= cut_off_date, 
         result_outcome != "02")

# Count screen_result - they should all be positive
# There shouldn't be any external positive results that have a referral - think 
# it is impossible for system to do this
aaa_extract %>% count(screen_result)

# Define a source_ref_to_vasc variable
aaa_extract <- aaa_extract %>% 
  mutate(source_ref_to_vasc = case_when(
    screen_type %in% c("02", "04") ~ "Surveillance", 
    pat_elig == "03" ~ "Self Referral", 
    screen_type == "01" ~ "Initial Screen", 
    TRUE ~ "Not Assigned"))

# Define year_screen variable
# This uses three defined years and sets other years to Other
aaa_extract <- aaa_extract %>% 
  mutate(year_screen = case_when(
    financial_year %in% c(kpi_report_years) ~ financial_year, 
    TRUE ~ "Other"))


## 3: Create Vascular Referral Output ----
vascular_referral_count <- aaa_extract %>% 
  count(source_ref_to_vasc, year_screen) %>% 
  complete(source_ref_to_vasc, year_screen) |> 
  mutate(n = replace_na(n, 0)) |> 
  group_by(source_ref_to_vasc) %>% 
  # create a "Cumulative" level in year_screen (program totals)
  group_modify(~ adorn_totals(.x, name = "Cumulative")) %>% 
  ungroup() %>% 
  # keep only reporting years
  filter(year_screen != "Other") 

vascular_referral_count <- vascular_referral_count %>% 
  group_by(year_screen) %>% 
  # create a source_ref_to_vasc summation (per year)
  group_modify(~ adorn_totals(.x, name = "Total")) %>% 
  mutate(pc = round_half_up(n * 100 / sum(max(n)), 1)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = year_screen, 
              values_from = c(n, pc), 
              names_glue = "{year_screen}_{.value}") %>% 
  # move Total to top of referral source
  arrange(source_ref_to_vasc != "Total", source_ref_to_vasc) %>% 
  select(source_ref_to_vasc, starts_with(kpi_report_years[1]), 
         starts_with(kpi_report_years[2]), starts_with(kpi_report_years[3]), 
         starts_with("Cumulative")) |> 
  mutate_all(~replace(., is.nan(.), NA))


## 4: Save Output ----
query_write_rds(vascular_referral_count,
                        paste0(temp_path, "/4_5_vasc_referrals_", yymm, ".rds"))

rm(vascular_referral_count, aaa_extract)


# Vascular Referral Outcomes ----------------------------------------------


## 5: Re-call in data ####
vasc <- read_rds(extract_path) %>% 
  select(financial_year, upi, hbres, hb_screen, date_screen, 
         screen_result, largest_measure, aaa_size, 
         date_referral_true, result_outcome, date_death) %>% 
  # only want referrals to vascular
  filter(!is.na(date_referral_true),
         # remove "referred in error: appt w vascular not required"
         result_outcome != "02") %>%  
  filter(date_screen <= cut_off_date) %>% 
  # categorize largest measurement into two bins
  mutate(result_size = if_else(largest_measure >= 5.5, "large", "small")) %>% 
  glimpse()

table(vasc$screen_result)
# 01 - 962, 02 - 01, Mar 2023
# 01 - 978, 02 - 01, Sep 2023
# 01 - 1067, 02 - 01 Mar 2024
# 01 - 1101, 02 - 01 Sep 2024

## Who has negative screen_result?
neg <- vasc[vasc$screen_result == "02",]
neg$aaa_size
# 2.9
rm(neg)

table(vasc$result_size)
# 01 - 957, 02 - 6, Mar 2023
# 01 - 973, 02 - 6, Sep 2023
# large - 1062, small - 6, Mar 2024
# large - 1096, small - 6, Mar 2024

table(vasc$result_outcome, vasc$result_size)



## 6: Reformat data ####
# Divide result_outcome into 'referrals with a final outcome' and 'referrals 
# with a non-final outcome' 
vasc <- vasc |> 
  mutate(outcome_type = case_when(result_outcome %in%  # "02" removed above
                                    c('01','02','03','04','05','06','07','08',
                                      '11','12','13','15','16','20') ~ "final outcome",
                                  result_outcome %in% c('09','10','14','17',
                                                        '18','19') ~ "non-final outcome",
                                  is.na(result_outcome) ~ "no outcome recorded",
                                  TRUE ~ "not categorized")) %>% 
  glimpse()



## Investigate result_outcome
table(vasc$outcome_type, useNA = "ifany")
table(vasc$result_outcome, useNA = "ifany")
# Note: should include "01", "03":"20", though some values may not be 
# represented in data; these are added in below using resout_list

## Create annual (financial year) summaries by referral outcome ----
### Size >= 5.5cm ----
## Referrals with final & non-final outcomes
greater <- vasc %>% 
  filter(result_size == "large") %>% 
  mutate(count = 1) %>% 
  group_by(financial_year, outcome_type, result_outcome) %>%
  summarize(cases = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = financial_year, values_from = cases) %>% 
  arrange(result_outcome) %>% 
  glimpse() 

## Subtotals: final & non-final outcome
greater_subtotal <- vasc %>% 
  filter(result_size == "large",
         outcome_type != "no outcome recorded") %>%
  mutate(count = 1) %>% 
  group_by(financial_year, outcome_type) %>%
  summarize(cases = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = financial_year, values_from = cases) %>% 
  mutate(result_outcome = case_when(outcome_type == "final outcome" ~ "98",
                                    outcome_type == "non-final outcome" ~ "97"),
         .after = outcome_type) %>% 
  glimpse()

## Grand total: final & non-final outcome
greater_grantot <- vasc %>% 
  filter(result_size == "large") %>% 
  mutate(count = 1) %>% 
  group_by(financial_year) %>%
  summarize(cases = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = financial_year, values_from = cases) %>% 
  mutate(outcome_type = "Total",
         result_outcome = "99") %>% 
  relocate(outcome_type:result_outcome) %>% 
  glimpse()  

## Combine into >=5.5cm df
annual_great <- greater_grantot %>% 
  rbind(greater_subtotal, greater) %>% 
  mutate(cumulative = rowSums(pick(where(is.numeric)), na.rm = TRUE))%>% 
  pivot_longer(cols= any_of(fy_list), names_to = "financial_year", values_to = "n")

# ensure all FYs accounted for
annual_great <- fy_tibble %>% 
  left_join(annual_great) %>% 
  pivot_wider(names_from = financial_year, values_from = n) %>% 
  filter(!is.na(result_outcome))

# Organize records by result_outcome
annual_great <- resout_list |> 
  left_join(annual_great, by = "result_outcome") |> 
  rename(outcome_type = outcome_type.x) %>% 
  mutate(result_size = "large") %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) |> 
  select(result_size, outcome_type, result_outcome, all_of(fy_list), cumulative)

### Size < 5.5cm ----

## Referrals with final & non-final outcomes
less <- vasc %>% 
  filter(result_size == "small") %>% 
  mutate(count = 1) %>% 
  group_by(financial_year, outcome_type, result_outcome) %>%
  summarize(cases = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = financial_year, values_from = cases) %>% 
  arrange(result_outcome) %>% 
  glimpse()

## Subtotals: final & non-final outcome
less_subtotal <- vasc %>% 
  filter(result_size == "small",
         outcome_type != "no outcome recorded") %>% 
  mutate(count = 1) %>% 
  group_by(financial_year, outcome_type) %>%
  summarize(cases = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = financial_year, values_from = cases) %>% 
  mutate(result_outcome = case_when(outcome_type == "final outcome" ~ "98",
                                    outcome_type == "non-final outcome" ~ "97")
         , .after = outcome_type) %>%
  glimpse()

## Grand total: final & non-final outcome
less_grantot <- vasc %>% 
  filter(result_size == "small") %>% 
  mutate(count = 1) %>% 
  group_by(financial_year) %>%
  summarize(cases = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = financial_year, values_from = cases) %>% 
  mutate(outcome_type = "Total",
         result_outcome = "99") %>% 
  relocate(outcome_type:result_outcome) %>% 
  glimpse()  

## Combine into <5.5cm df
annual_less <- less_grantot %>%
  rbind(less_subtotal, less) |>  
  mutate(cumulative = rowSums(pick(where(is.numeric)), na.rm = TRUE)) %>% 
  pivot_longer(cols= any_of(fy_list), names_to = "financial_year", values_to = "n")

# ensure all FYs accounted for
annual_less <- fy_tibble %>% 
  left_join(annual_less) %>% 
  pivot_wider(names_from = financial_year, values_from = n) %>% 
  filter(!is.na(result_outcome))

# organise by result outcome detail
annual_less <- resout_list |> 
  left_join(annual_less, by = "result_outcome") |> 
  rename(outcome_type = outcome_type.x) %>% 
  mutate(result_size = "small") %>% 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) |> 
  select(result_size, outcome_type, result_outcome, all_of(fy_list), cumulative)


## Combine annual totals ----
annual <- rbind(annual_great, annual_less) |> 
  mutate_all(~replace(., is.nan(.), NA))

query_write_rds(annual, paste0(temp_path, "/4_6_vasc_outcomes_", yymm, ".rds"))

rm(greater, greater_grantot, greater_subtotal, annual_great, resout_list,
   less, less_grantot, less_subtotal, annual_less, annual, vasc, fy_list)


# AAA Repair Operations ---------------------------------------------------

## 7: Re-call and format Data ####
extract <- read_rds(extract_path) %>% 
  select(hbres, aaa_size, date_referral_true, result_outcome,
         date_surgery, hb_surgery, fy_surgery, surg_method) |> 
  filter(date_surgery <= cut_off_date,
         # select referrals
         !is.na(date_referral_true) & aaa_size >= 5.5) 

table(extract$surg_method, useNA = "ifany")
# 296 EVAR (01), 335 open (02), Mar 2023
# 328 EVAR (01), 372 open (02), Sep 2023
# 369 EVAR (01), 401 open (02), Mar 2024

###
check <- extract[extract$surg_method == "03",]
# Procedure abandoned (03): Historically, there are only two records  
# (Grampian & WIs); abandoned procedures are not reported on, but good to check
# if any more come up!
rm(check)
###

extract <- extract |> 
  filter(surg_method %in% c("01", "02"))

####
## Where surg_method has response, result_outcome should only have: 
## 15 (approp for surgery and survived 30 days) or 
## 16 (approp for surgery and died within 30 days)
table(extract$result_outcome, extract$surg_method)

#   Feb 2023  Sep 2023    Mar 2024 
#     01  02    01  02    01  02
# 15 294 328   326 364    366 391
# 16   1   7     1   8    1  10
# 17                      1   0
# 20   1   0     1   0    1   0

## One record has result_outcome == 20 (other final outcome)
check <- extract[extract$result_outcome == "20",]
# View(check)
## FY 2016/17
## Lothian resident who was eventually operated on in GG&C. 
## Once the board of surgery field is added to vascular module we will be 
## asking board to change result outcome to 15 or 16 (and data will be 
## collated by board of surgery rather than board of residence so it will be 
## counted under GG&C)
rm(check)
####

## Appropriate for surgery records
extract <- extract |> 
  filter(result_outcome %in% c("15", "16"))

table(extract$surg_method)
# 327 EVAR (01), 372 open (02), Sep 2023
# 367 EVAR (01), 401 open (02), Mar 2024
## Check this against the total number of operations as identified in
## KPI 4.1/4.2 Additional A
check <- read_rds(paste0(temp_path, "/4_2_kpi_4_", yymm, ".rds")) |> 
  filter(financial_year == "Cumulative",
         group == "procedures_n")
View(check)
rm(check)


## 8: Create Tables ####
## These tables are used by HBs to look which type of surgical method was used 
# to treat their patients, not identify which HBs use each type of surgery. 
# Therefore, hbres is used instead of hb_surgery to create tables
## First, create table of surgical methods used each year (by FY of surgery) ---
# Surgery method -- Health boards
method <- extract %>% 
  group_by(hbres, fy_surgery, surg_method) %>% 
  count(surg_method) %>% 
  ungroup() |> 
  complete(hbres, fy_surgery, surg_method)

# Surgery method -- Scotland
method_scot <- extract %>% 
  group_by(fy_surgery, surg_method) %>% 
  count(surg_method) %>% 
  ungroup() %>% 
  mutate(hbres = "Scotland", .before = fy_surgery)

# Surgery method -- Combine
method <- rbind(method_scot, method)

## Next, create table of total surgeries performed by hbres (by FY of surgery) ---
# Total repairs -- Health boards
repair <- extract %>% 
  group_by(hbres, fy_surgery) %>% 
  count(hbres) %>% 
  ungroup() %>% 
  complete(hbres, fy_surgery) |> 
  mutate(surg_method = "99", .after = fy_surgery)

# Total repairs -- Scotland
repair_scot <- extract %>% 
  group_by(fy_surgery) %>% 
  count(fy_surgery) %>% 
  ungroup() %>% 
  mutate(hbres = "Scotland", .before = fy_surgery) %>% 
  mutate(surg_method = "99", .after = fy_surgery)

# Total repairs -- combine
repair <- rbind(repair_scot, repair)

rm(method_scot, repair_scot)

## Combine to create table of surgery types and total surgeries by hbres
## Historical surgeries data ---
repairs_all <- rbind(method, repair) %>% 
  mutate(across(where(is.numeric), \(x) replace_na(x, 0))) |> 
  mutate(surg_method = case_when(surg_method == "01" ~ "EVAR",
                                 surg_method == "02" ~ "Open",
                                 surg_method == "99" ~ "Total AAA repairs"),
         surg_method = fct_relevel(surg_method, c("Open", "EVAR",
                                                  "Total AAA repairs"))) %>% 
  arrange(fy_surgery, surg_method) %>% 
  glimpse() 

## Should this be written out? And rewritten each year as a new historical file?
repairs_hist <- hb_tibble |> 
  left_join(repairs_all, by =  "hbres")


## Current 3-year reporting period ---
repairs_current <- repairs_all %>% 
  filter(fy_surgery %in% kpi_report_years) 

# Cumulative totals
repairs_cum <- repairs_all %>% 
  group_by(hbres, surg_method) %>% 
  summarize(n = sum(n, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(fy_surgery = "Cumulative", .after = hbres)

repairs_current <- add_new_rows(
  repairs_current, repairs_cum, fy_surgery, surg_method) |> 
  mutate_all(~replace(., is.nan(.), NA))

query_write_rds(repairs_current, paste0(temp_path, "/4_7_vasc_ref_repairs_", yymm, ".rds"))

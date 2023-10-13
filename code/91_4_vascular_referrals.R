###############################################################################
# vascular_referrals.R
# Calum Purdie & Karen Hotopp
# 16/02/2023
# 
# Table 7: Vascular referral types
# Vascular KPI background information (vascular referrals outcomes)
#
# Written/run on Posit Server
# R version 4.1.2
###############################################################################


## Notes:
# 


### 1: Housekeeping ----

# Load packages

library(dplyr)
library(readr)
library(janitor)
library(tidylog)

rm(list = ls())
gc()


source(here::here("code/0_housekeeping_theme_4.R"))

rm(fy_tibble, hb_list, kpi_report_years, output_path)

resout_list <- tibble(result_outcome = c('99','98','01','03','04','05','06','07',
                                         '08','11','12','13','15','16','20','97',
                                         '09','10','14','17','18','19', '96'))


### 2: Data Extraction ----
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


### 3: Create Vascular Referral Output ----
vascular_referral_count <- aaa_extract %>% 
  count(source_ref_to_vasc, year_screen) %>% 
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
         starts_with("Cumulative"))


### 4: Save Output ----
write_csv(vascular_referral_count, paste0(temp_path, "/9_vasc_referral_", 
                                          yymm, ".rds"))


#----------------------Vascular Referral Outcomes------------------------------#

#### 5: Re-call in data ####
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

## Who has negative screen_result?
neg <- vasc[vasc$screen_result == "02",]
neg$aaa_size
# 2.9
rm(neg)

table(vasc$result_size)
# 01 - 957, 02 - 6, Mar 2023
# 01 - 973, 02 - 6, Sep 2023

table(vasc$result_outcome, vasc$result_size)


#### 6: Reformat data ####
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


### Create annual (financial year) summaries by referral outcome ----
## Size >= 5.5cm ----
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
         outcome_type != 3) %>%
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
  mutate(cummulative = rowSums(pick(where(is.numeric)), na.rm = TRUE))

## Referrals with no outcome recorded ----
# This should be 0 records, but need to keep track and add to final table
vasc %>% filter(outcome_type == 3)
# If no records, manually add record of 0 to annual_greater
annual_great <- annual_great |>  
  add_row(outcome_type = "Total: no outcome recorded", 
          result_outcome = "96")

# Organize records by result_outcome
annual_great <- resout_list |> 
  left_join(annual_great) |> 
  mutate(result_size = "large",
         outcome_type = case_when(result_outcome %in% c("04", "05") ~ "final outcome",
                                  result_outcome == "14" ~ "non-final outcome",
                                  TRUE ~ outcome_type)) |> 
  select(result_size, outcome_type, result_outcome, all_of(fy_list), cummulative)


## Size < 5.5cm ----
# Check financial years, as not all present in data and will need to be added

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
         outcome_type != 3) %>% 
  mutate(count = 1) %>% 
  group_by(financial_year, outcome_type) %>%
  summarize(cases = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = financial_year, values_from = cases) %>% 
  mutate(result_outcome = case_when(outcome_type == "final outcome" ~ "98",
                                    outcome_type == "non-final outcome" ~ "97")
         , .after = outcome_type) %>% 
  # check for non-final outcome; likely 0, but need to adjust below if record present
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
  # add in row for non-final outcome, as this is 0 (from less_subtotal above)
  add_row(outcome_type = "Total: non-final outcome", 
          result_outcome = "97") %>% 
  mutate(result_size = "small",
         cummulative = rowSums(pick(where(is.numeric)), na.rm = TRUE)) %>% 
  # is there a better way of doing this?? Need to manually check what years needed
  mutate(`2012/13` = NA,
         `2014/15` = NA,
         `2016/17` = NA,
         `2017/18` = NA,
         `2019/20` = NA,
         `2021/22` = NA) %>% 
  # reorder columns
  select(result_size, outcome_type, result_outcome, all_of(fy_list), cummulative)


## Combine annual totals ----
annual <- rbind(annual_great, annual_less) %>% 
  mutate(outcome_type = case_when(result_outcome == "98" ~ "Total: final outcome",
                                  result_outcome == "97" ~ "Total: non-final outcome",
                                  TRUE ~ outcome_type))

write_rds(annual, paste0(temp_path, "/10_vasc_referrals", yymm, ".rds"))

rm(greater, greater_grantot, greater_subtotal, annual_great, resout_list,
   less, less_grantot, less_subtotal, annual_less, annual)


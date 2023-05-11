##########################################################
# 4.1_vascular_outcomes.R
# Karen Hotopp
# 21/10/2022
# Script 1 of ?
# 
# Translation of SPSS file '1. Vascular outcomes recorded.sps'
# Part of Theme 4 for AAA KPIs
# Tab: Vascular KPIs background
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


#### 1: Housekeeping ####
## Packages
library(here)
library(dplyr)
library(magrittr)
library(readr)
library(forcats)
library(tidylog)


rm(list = ls())


## Values
year <- 2023
month <- "03"
vas_cutoff <- "2023-03-31"


## Pathways
wd_path <-paste0("/PHI_conf/AAA/Topics/Screening/KPI",
                 "/", year, month)

extract_path <-paste0("/PHI_conf/AAA/Topics/Screening/extracts",
                      "/", year, month)


#### 2: Call in data ####
vasc <- read_rds(paste0(extract_path, "/output/aaa_extract_", 
                        year, month, ".rds")) %>% 
  select(financial_year, upi, hbres, hb_screen, 
         date_screen, screen_result, largest_measure, 
         aaa_size, date_referral_true, 
         result_outcome, date_death) %>% 
  # only want referrals to vascular
  filter(!is.na(date_referral_true),
         # remove "referred in error: appt w vascular not required"
         result_outcome != "02") %>%  
  filter(date_screen <= vas_cutoff) %>% 
  # categorize largest measurement into two bins
  mutate(result_size = if_else(largest_measure >= 5.5, 1, 2)) %>% 
  glimpse()

table(vasc$screen_result)
#  01  02 
# 880   1

# 01 - 962, 02 - 01, Mar 2023

## Who has negative screen_result?
neg <- vasc[vasc$screen_result == "02",]
neg$aaa_size
# 2.9
# 2.9, Mar 2023

table(vasc$result_size)
#   1   2 
# 876   5

# 01 - 957, 02 - 6, Mar 2023

table(vasc$result_outcome, vasc$result_size)
#      1   2
# 03   1   0
# 06  17   1
# 07   7   0
# 08 128   0
# 09   2   0
# 10   8   0
# 11  11   0
# 12   2   0
# 13   1   0
# 15 644   3
# 16   8   0
# 17   6   0
# 18  13   0
# 19   3   0
# 20  25   1

#      1   2
# 01   1   0
# 03   1   0
# 06  17   2
# 07   9   0
# 08 143   0
# 09   3   0
# 10  11   0
# 11  12   0
# 12   3   0
# 13   1   0
# 14   2   0
# 15 679   3
# 16   9   0
# 17   8   0
# 18  29   0
# 19   1   0
# 20  28   1, Mar 2023


#### 3: Reformat data ####
# Divide result_outcome into 'referrals with a final outcome' and 'referrals 
# with a non-final outcome' 
vasc %<>%
  mutate(outcome_type = case_when(result_outcome %in% 
                                    c('01','02','03','04','05','06','07','08',
                                      '11','12','13','15','16','20', '21') ~ 1,
                                  result_outcome %in% c('09','10','14','17',
                                                        '18','19') ~ 2,
                                  is.na(result_outcome) ~ 3,
                                  TRUE ~ 4)) %>% 
  glimpse()

# value labels outcome_type
# 1 'Referrals with a final outcome'
# 2 'Referrals with a non-final outcome'
# 3 'No Outcome recorded'
# 4 'Outcome code not categorized by syntax'.


# SPSS creates 'alloutcomes' variable (== 6) 
# SPSS creates 'allresults' variable (== 00)
# These are not recreated in R as aren't necessary

## Investigate result_outcome
table(vasc$result_outcome, useNA = "ifany")
# 03  06  07  08  09  10  11  12  13  15  16  17  18  19  20 
#  1  18   7 128   2   8  11   2   1 647   8   6  13   3  26
# Note: level 14 is missing; will need to be added in manually to 
# greater & annual_less (created below) to make sure it is
# included in the vascular referrals outcome table

# 01  03  06  07  08  09  10  11  12  13  14  15  16  17  18  19  20 
# 1   1  19   9 143   3  11  12   3   1   2 682   9   8  29   1  29, Mar 2023


### Create annual (financial year) summaries ----

## Size >= 5.5cm ----
## Outcome type by result outcome
greater <- vasc %>% 
  filter(result_size == 1) %>% 
  mutate(count = 1) %>% 
  group_by(financial_year, outcome_type, result_outcome) %>%
  summarize(cases = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = financial_year, values_from = cases) %>% 
  # add in missing result_outcome, as determined in table above
  add_row(outcome_type = 2, result_outcome ="14") %>% 
  mutate(result_size = 1, .before = outcome_type) %>% 
  arrange(result_outcome) %>% 
  glimpse()


## Outcome type by total result outcomes
greater2 <- vasc %>% 
  filter(result_size == 1,
         outcome_type != 3) %>% 
  mutate(count = 1) %>% 
  group_by(financial_year, outcome_type) %>%
  summarize(cases = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = financial_year, values_from = cases) %>% 
  mutate(result_size = 1, .before = outcome_type) %>% 
  mutate(result_outcome = "Total", .after = outcome_type) %>% 
  glimpse()


## Totals: Outcome type by result outcome
greater3 <- vasc %>% 
  filter(result_size == 1) %>% 
  mutate(count = 1) %>% 
  group_by(financial_year) %>%
  summarize(cases = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = financial_year, values_from = cases) %>% 
  mutate(result_size = 1,
         outcome_type = 99,
         result_outcome = "Total") %>% 
  relocate(result_size:result_outcome) %>% 
  glimpse()  


## Combine into >=5.5cm df
annual_great <- greater %>% 
  rbind(greater2, greater3) %>% 
  replace(is.na(.), 0) %>%
  mutate(cummulative = rowSums(across(`2012/13`:`2022/23`))) %>% 
  glimpse()


## Referrals with no outcome recorded ----
# This should be 0 records, but need to keep track and add to final table
no_outcome <- vasc %>%
  filter(outcome_type == 3)
# If 0, manually add record of 0 to annual_greater
annual_great %<>% 
  add_row(result_size = 1, outcome_type = 3, 
          result_outcome = "No outcome recorded") %>%
  replace(is.na(.), 0)


## Size < 5.5cm ----
# Make sure to check which financial years produce results in the three
# new dataframes!!
# Add new fin years to annual_less (combined df) as needed

## Outcome type by result outcome
less <- vasc %>% 
  filter(result_size == 2) %>% 
  mutate(count = 1) %>% 
  group_by(financial_year, outcome_type, result_outcome) %>%
  summarize(cases = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = financial_year, values_from = cases) %>% 
  # add in missing result_outcome, as determined in table above
  add_row(outcome_type = 2, result_outcome ="14") %>% 
  mutate(result_size = 2, .before = outcome_type) %>%
  arrange(result_outcome) %>% 
  glimpse()


## Outcome type by total result outcomes
less2 <- vasc %>% 
  filter(result_size == 2,
         outcome_type != 3) %>% 
  mutate(count = 1) %>% 
  group_by(financial_year, outcome_type) %>%
  summarize(cases = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = financial_year, values_from = cases) %>% 
  # add in missing result_outcome, as determined in table above
  # (result_outcome = "14" is the only outcome_type = 2 in small AAAs)
  add_row(outcome_type = 2) %>% 
  mutate(result_size = 2, .before = outcome_type) %>% 
  mutate(result_outcome = "Total", .after = outcome_type) %>% 
  glimpse()


## Totals: Outcome type by result outcome
less3 <- vasc %>% 
  filter(result_size == 2) %>% 
  mutate(count = 1) %>% 
  group_by(financial_year) %>%
  summarize(cases = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = financial_year, values_from = cases) %>% 
  mutate(result_size = 2,
         outcome_type = 99,
         result_outcome = "Total") %>% 
  relocate(result_size:result_outcome) %>% 
  glimpse()  


## Combine into <5.5cm df
annual_less <- less %>% 
  rbind(less2, less3) %>% 
  mutate(`2012/13` = 0,
         `2014/15` = 0,
         `2016/17` = 0,
         `2017/18` = 0,
         `2019/20` = 0,
         `2021/22` = 0,
         `2022/23` = 0) %>% 
  select(result_size:result_outcome,
         `2012/13`,
         `2013/14`,
         `2014/15`,
         `2015/16`,
         `2016/17`,
         `2017/18`,
         `2018/19`,
         `2019/20`,
         `2020/21`,
         `2021/22`,
         `2022/23`) %>% 
  replace(is.na(.), 0) %>%
  mutate(cummulative = rowSums(across(`2012/13`:`2022/23`))) %>% 
  glimpse()


## Combine annual totals ----
annual <- rbind(annual_great, annual_less) %>% 
  # Relabel for easier reading
  mutate(result_size = case_when(result_size == 1 ~ "Greater or equal to 5.5cm",
                                 result_size == 2 ~ "Less than 5.5cm"), 
         outcome_type = case_when(outcome_type == 1 ~ "Referral with final outcome",
                                  outcome_type == 2 ~ "Referral with non-final outcome",
                                  outcome_type == 3 ~ "No outcome recorded",
                                  outcome_type == 99 ~ "All referrals"),
         result_outcome = case_when(result_outcome == "03" ~ "DNA outpatient service: Self-discharge",
                                    result_outcome == "06" ~ "Referred in error: As determined by vascular service",
                                    result_outcome == "07" ~ "Died before surgical assessment completed",
                                    result_outcome == "08" ~ "Unfit for surgery",
                                    result_outcome == "09" ~ "Refer to another specialty",
                                    result_outcome == "10" ~ "Awaiting further AAA growth",
                                    result_outcome == "11" ~ "Appropriate for Surgery: Patient declined surgery",
                                    result_outcome == "12" ~ "Appropriate for Surgery: Died before treatment",
                                    result_outcome == "13" ~ "Appropriate for Surgery: Self-discharge",
                                    result_outcome == "14" ~ "Appropriate for Surgery: Patient deferred surgery",
                                    result_outcome == "15" ~ "Appropriate for Surgery: AAA repaired and survived 30 days",
                                    result_outcome == "16" ~ "Appropriate for Surgery: Died within 30 days of treatment",
                                    result_outcome == "17" ~ "Appropriate for Surgery: Final outcome pending",
                                    result_outcome == "18" ~ "Ongoing assessment by vascular",
                                    result_outcome == "19" ~ "Final outcome pending",
                                    result_outcome == "20" ~ "Other final outcome",
                                    TRUE ~ result_outcome)) %>%
  # Relevel according to table presentation
  mutate(result_size = fct_relevel(result_size, c("Greater or equal to 5.5cm",
                                                  "Less than 5.5cm")),
         outcome_type = fct_relevel(outcome_type, c("All referrals",
                                                    "Referral with final outcome",
                                                    "Referral with non-final outcome",
                                                    "No outcome recorded")),
         # is there not a way to just move a factor to the top level??
         result_outcome = fct_relevel(result_outcome, c("Total",
                                                        "DNA outpatient service: Self-discharge",
                                                        "Referred in error: As determined by vascular service",
                                                        "Died before surgical assessment completed",
                                                        "Unfit for surgery",
                                                        "Refer to another specialty",
                                                        "Awaiting further AAA growth",
                                                        "Appropriate for Surgery: Patient declined surgery",
                                                        "Appropriate for Surgery: Died before treatment",
                                                        "Appropriate for Surgery: Self-discharge",
                                                        "Appropriate for Surgery: Patient deferred surgery",
                                                        "Appropriate for Surgery: AAA repaired and survived 30 days",
                                                        "Appropriate for Surgery: Died within 30 days of treatment",
                                                        "Appropriate for Surgery: Final outcome pending",
                                                        "Ongoing assessment by vascular",
                                                        "Final outcome pending",
                                                        "Other final outcome"
         ))) %>% 
  arrange(result_size, outcome_type, result_outcome)
# Note: result_outcome == "Appropriate for Surgery: Patient deferred surgery" (14)
# not recognized level, meaning there are no records. Manually added in above

write_rds(annual, paste0(wd_path, "/temp/4_vasc_sizeOutcomes.rds"))

rm(greater, greater2, greater3, annual_great, 
   less, less2, less3, annual_less, annual, neg, no_outcome)


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

### 4 Save Output ----

# Save output

write.xlsx(annual, 
           paste0(wd_path, "/temp/", "4_vasc_sizeOutcomes.xlsx"))


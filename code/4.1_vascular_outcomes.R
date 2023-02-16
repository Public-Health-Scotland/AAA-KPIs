##########################################################
# 4.1_vascular_outcomes.R
# Karen Hotopp
# 21/10/2022
# Script 1 of ?
# 
# Translation of SPSS file '1. Vascular outcomes recorded.sps'
# Part of Theme 4 for AAA KPIs
# Takes the processed BOXI extracts and creates tables detailing
# vascular surgery outcomes
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
library(tidylog)


rm(list = ls())


## Values
year <- 2022
month <- "09"
vas_cutoff <- "2022-03-31"


## Pathways
wd_path <-paste0("/PHI_conf/AAA/Topics/Screening/KPI",
                 "/", year, month)

extract_path <-paste0("/PHI_conf/AAA/Topics/Screening/extracts",
                      "/", year, month)


#### 2: Call in data ####
vasc <- read_rds(paste0(extract_path, "/output/aaa_extract_", year, month, ".rds")) %>% 
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
## Who has negative screen_result?
neg <- vasc[vasc$screen_result == "02",]
neg$aaa_size
# 2.9

table(vasc$result_size)
#   1   2 
# 876   5
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

# SPSS creates 'alloutcomes' variable (== 6) 
# SPSS creates 'allresults' variable (== 00)
# These are not recreated in R as don't seem necessary


### Create annual (financial year) summaries ----

## Size >= 5.5cm
## Outcome type by result outcome
greater <- vasc %>% 
  filter(result_size == 1) %>% 
  mutate(count = 1) %>% 
  group_by(financial_year, outcome_type, result_outcome) %>%
  summarize(cases = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = financial_year, values_from = cases) %>% 
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
  mutate(cummulative = rowSums(across(`2012/13`:`2021/22`))) %>% 
  glimpse()

#write_rds(annual_great, paste0(wd_path, "/temp/4_vasc_sizeOutcomes_greater.rds"))  


## Size < 5.5cm
# Make sure to check which fin years produce results in the three
# new dataframes!!
# Remove and add new fin years to annual_less (combined) as needed

## Outcome type by result outcome
less <- vasc %>% 
  filter(result_size == 2) %>% 
  mutate(count = 1) %>% 
  group_by(financial_year, outcome_type, result_outcome) %>%
  summarize(cases = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = financial_year, values_from = cases) %>% 
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
         `2021/22` = 0) %>% 
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
         `2021/22`) %>% 
  replace(is.na(.), 0) %>%
  mutate(cummulative = rowSums(across(`2012/13`:`2021/22`))) %>% 
  glimpse()

#write_rds(annual_less, paste0(wd_path, "/temp/4_vasc_sizeOutcomes_less.rds"))  

## Combine annual totals
annual <- rbind(annual_great, annual_less)

write_rds(annual, paste0(wd_path, "/temp/4_vasc_sizeOutcomes.rds"))

rm(greater, greater2, greater3, annual_great, 
   less, less2, less3, annual_less, annual)


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
  filter(result_size == 1,
         # approp for treatment: died within 30 days of surgery
         result_outcome == "16") #%>% 
## Look over this again... this filter can't be correct!  
filter((financial_year %in% c("2018/19", "2019/20") & result_outcome == "20") |
           (financial_year == "2018/19" & result_outcome == "18"))

write_rds(letter, paste0(wd_path, "/temp/4_letters_CHI.rds"))


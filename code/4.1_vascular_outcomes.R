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

## Starting this as have been asked to show Vascular KPIs background information
## by HB for AAA BCG meeting (24Oct2022). May have useful bits for further 
## translating of SPSS gile, but started just to get HB info.


#### 1: Housekeeping ####
## Packages
library(here)
library(dplyr)
library(magrittr)
#library(stringr)
#library(forcats)
library(readr)
library(tidylog)


rm(list = ls())


## Values
year <- 2022
month <- "09"
vas_cutoff <- "2022-03-31"


## Pathways
wd_path <-paste0("/PHI_conf/AAA/Topics/Screening",
                 "/", year, month)



#### 2: Call in data ####
vasc <- read_rds(paste0(wd_path, "/extracts/output/aaa_extract_202209.rds")) %>% 
  # only want referrals to vascular
  filter(!is.na(date_referral_true),
         result_outcome != 2) %>%  ##!! This needs to be changed to "02" once fixed in script 1
  filter(date_screen <= vas_cutoff) %>% 
  # categorize largest measurement into two bins
  mutate(result_size = if_else(largest_measure >= 5.5, 1, 2)) %>% 
  glimpse()

table(vasc$result_outcome, vasc$result_size)


#### 2: Reformat data ####
vasc %<>%
  # remove first mutate (as.character) and add 0s to single digits below after fixed in script 1
  mutate(result_outcome = as.character(result_outcome),
         outcome_type = case_when(result_outcome %in% 
                                    c('1','2','3','4','5','6','7','8',
                                      '11','12','13','15','16','20', '21') ~ 1,
                                  result_outcome %in% c('9','10','14','17',
                                                        '18','19') ~ 2,
                                  is.na(result_outcome) ~ 3,
                                  TRUE ~ 4)) %>% 
  glimpse()

## Size >= 5.5cm ---
## SPSS line 148
greater <- vasc %>% 
  filter(result_size == 1) %>% 
  mutate(count = 1) %>% 
  group_by(financial_year, outcome_type, result_outcome) %>%
  summarize(cases = sum(count)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = financial_year, values_from = cases) %>% 
  glimpse()
  
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






names(vasc)
table(vasc$result_outcome)




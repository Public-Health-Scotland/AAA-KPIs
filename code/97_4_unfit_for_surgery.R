##########################################################
# 4.3_unfit_for_surgery.R
# Karen Hotopp
# March 2023
# Script 3 of ?
# 
# Translation of SPSS file '4. Unfit for surgery'
# Part of Theme 4 for AAA KPIs
# Tab: AAA Repairs
# Takes the processed BOXI extract and outputs referrals to
# vascular who were unfit for surgery by HB and year
# 
# Written/run on R Studio Server
# R version 3.6.1
##########################################################


#### 1: Housekeeping ####
## Packages
library(dplyr)
library(magrittr)
library(forcats)
library(readr)
library(tidylog)


rm(list = ls())


## Values
year <- 2023
month <- "03"

# Set cut off date as end of latest financial year.
# Where an interim update is being provided for the MEG (for instance, in March)
# the final output tables will need to include a footnote to indicate that it 
# represents a partial financial year (e.g. 1 April to end of February).
vas_cutoff <- "2023-03-31"


## Pathways
wd_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", year, month)

extract_path <- paste0("/PHI_conf/AAA/Topics/Screening/extracts",
                       "/", year, month)


#### 2: Format Data ####
extract <- readRDS(paste0(extract_path, "/output/aaa_extract_", 
                          year, month, ".rds")) %>% 
  filter(!is.na(date_referral_true),
         date_screen <= vas_cutoff) %>% 
  # select(hbres, aaa_size, date_referral_true, result_outcome,
  #        date_surgery, fy_surgery, surg_method) %>%
  # Add relevant variables, as don't need whole data set
  mutate(size_dichot = if_else(largest_measure >= 5.5, ">=5.5cm", "<5.5cm")) %>% 
  glimpse()


table(extract$screen_result)
# 01 (positive) = 898; 02 (negative) = 7   Sept 2022
# 01 (positive) = 1012; 02 (negative) = 7   Mar 2023

table(extract$result_outcome, extract$size_dichot)


###
# Background information (not for report) 
# Quick look at individuals who have been operated on and have result_outcome
# as 'other final'
# 'Other final outcome' reported as a single category in report & in records
check <- extract %>% 
  filter(result_outcome == "20",
         surg_method == "03",
         !is.na(date_surgery))
# Two records, from 2012/13 and 1014/15
rm(check)
###


# Remove where categorized as 'referred in error: appointment with 
# vascular service not required'.
extract %<>%
  filter(result_outcome != "02") %>% 
  ##!! is this necessary?? Not used once calculated...
  mutate(outcome_type = case_when(result_outcome %in% 
                                    c('01','02','03','04','05','06','07','08',
                                      '11','12','13','15','16','20', '21') ~ 1,
                                  result_outcome %in% c('09','10','14','17',
                                                        '18','19') ~ 2,
                                  is.na(result_outcome) ~ 3,
                                  TRUE ~ 4)) %>% 
  glimpse()
  
## Again, not needed in this script, as outcome_type only used in script 4.1
# value labels outcome_type
# 1 'Referrals with a final outcome'
# 2 'Referrals with a non-final outcome'
# 3 'No Outcome recorded'
# 4 'Outcome code not categorized by syntax'.



### Unfit for Surgery
unfit_surgery <- extract %>%
  filter(size_dichot == ">=5.5cm",
         outcome_type == 1) %>% 
  # create flag for anyone unfit for surgery
  mutate(unfit = if_else(result_outcome == "08", 1, 0),
         cohort = 1)

table(unfit_surgery$unfit)
# 128 patients unfit  Sept 2022
# 143 patients unfit  Mar 203


### Patients unfit for surgery by FY ----
## Unfit by health board and FY
unfit_hb <- unfit_surgery %>% 
  group_by(financial_year, hbres) %>% 
  summarize(cohort_n = sum(cohort), 
            unfit_n = sum(unfit)) %>% 
  ungroup()

## Unfit by Scotland and FY
unfit_scot <- unfit_surgery %>% 
  group_by(financial_year) %>% 
  summarize(cohort_n = sum(cohort), 
            unfit_n = sum(unfit)) %>% 
  ungroup() %>% 
  mutate(hbres = "Scotland", .after = financial_year)

## Combine and calculate percentage
unfit_fy <- rbind(unfit_hb, unfit_scot) %>% 
  mutate(unfit_p = (unfit_n/cohort_n)*100) %>% 
  pivot_wider(names_from = financial_year, 
              values_from = c(cohort_n:unfit_p)) %>% 
  # reorder columns to be in financial year order
  select(hbres, ends_with("13"), ends_with("14"), ends_with("15"),
         ends_with("16"), ends_with("17"), ends_with("18"),
         ends_with("19"), ends_with("20"), ends_with("21"),
         ends_with("22")) %>% # will need to add additional years as we go...
  mutate(hbres = fct_relevel(hbres, c("Scotland", "Ayrshire & Arran", "Borders",
                             "Dumfries & Galloway", "Fife", "Forth Valley",
                             "Grampian", "Greater Glasgow & Clyde",
                             "Highland", "Lanarkshire", "Lothian",
                             "Orkney", "Shetland", "Tayside",
                             "Western Isles"))) %>%
  arrange(hbres) %>% 
  glimpse


### Patients unfit for surgery Cumulative ----
## Unfit by health board
unfit_hb <- unfit_surgery %>% 
  group_by(hbres) %>% 
  summarize(cohort_n = sum(cohort), 
            unfit_n = sum(unfit)) %>% 
  ungroup()

## Unfit by Scotland
unfit_scot <- unfit_surgery %>% 
  group_by() %>% 
  summarize(cohort_n = sum(cohort), 
            unfit_n = sum(unfit)) %>% 
  ungroup() %>% 
  mutate(hbres = "Scotland", .before = cohort_n)

## Combine and calculate percentage
unfit_cum <- rbind(unfit_hb, unfit_scot) %>% 
  mutate(unfit_p = (unfit_n/cohort_n)*100,
         hbres = fct_relevel(hbres, c("Scotland", "Ayrshire & Arran", "Borders",
                                      "Dumfries & Galloway", "Fife", "Forth Valley",
                                      "Grampian", "Greater Glasgow & Clyde",
                                      "Highland", "Lanarkshire", "Lothian",
                                      "Orkney", "Shetland", "Tayside",
                                      "Western Isles"))) %>%
  arrange(hbres) %>% 
  rename(cohort_n_cum = cohort_n,
         unfit_n_cum = unfit_n,
         unfit_p_cum = unfit_p) %>% 
  glimpse()
           

#### 3: Combine & Export ####
unfit_programme <- left_join(unfit_fy, unfit_cum)

write_rds(unfit_programme, paste0(wd_path, "/temp/4_AAA_unfit_for_surgery.rds"))
write.xlsx(unfit_programme, paste0(wd_path, "/temp/4_AAA_unfit_for_surgery.xlsx"))

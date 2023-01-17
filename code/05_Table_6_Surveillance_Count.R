#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 05_Table_6_Surveillance_Count.R
# Eibhlin O'Sullivan
# Jan 2022
# Supplementary tables - Table 6 (Surveillance: number of men tested)
# Written/run on R Studio Server
# R version 3.6.1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### 1 - Housekeeping ----

library(here)

## Source housekeeping file
source(here::here("Syntax", "00_housekeeping.R"))

### 2 - Read in Extract ----

# AAA extract
aaa_extract <- readRDS(paste0(aaa_extracts_path,"aaa_extract_202209.rds"))

### 3 - Clean Data ----

# filter records prior to cut off date
# filter screening result as positive, negative or non-visualisation
surveillance_summary <- aaa_extract %>%
  filter(date_screen < cut_off_date,
         screen_result %in% c("01","02","04"))

# Check records without a follow up recommendation
View(surveillance_summary %>% tabyl(screen_result,followup_recom))

# for run based on 1 March 2018 extract there were 3 records without a follow-up recommendation.
# for run based on 1 March 2019 extract there were 0 records without a follow-up recommendation.
# for run based on 1 March 2020 extract there were 0 records without a follow-up recommendation.
# for run based on 9 April 2021 extract there were 0 records without a follow-up recommendation.
# for run based on 1 September 2021 extract there were 0 records without a follow-up recommendation.
# for run based on 14 September 2022 extract there were 0 records without a follow-up recommendation

## 3.1 - Determine Surveillance Type ----

# Get the type of of sur



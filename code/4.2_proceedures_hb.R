##########################################################
# 4.2_proceedures_hb.R
# Salomi Barkat & Karen Hotopp
# 21/10/2022
# Script 2 of ?
# 
# Translation of SPSS file 'AAA Procedures'
# Part of Theme 4 for AAA KPIs
# Tab: AAA Repairs
# Takes the processed BOXI extract and creates tables detailing
# vascular surgery (AAA repair surgery) outcomes
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
library(phsmethods)
library(forcats)
library(tidyr)
library(tidylog)


rm(list = ls())


## Values
year <- 2022
month <- "09"
vas_cutoff <- "2022-03-31"
# most recent three years
output_years <- c("2019/20", "2020/21", "2021/22")


## Pathways
wd_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", year, month)

extract_path <- paste0("/PHI_conf/AAA/Topics/Screening/extracts",
                      "/", year, month)


#### 2: Format Data ####
extract <- readRDS(paste0(extract_path, "/output/aaa_extract_", 
                          year, month, ".rds")) %>% 
  select(hbres, aaa_size, date_referral_true, result_outcome,
         date_surgery, fy_surgery, surg_method) %>%
  # Add relevant variables, as don't need whole data set
  glimpse()


## Set dates
extract %<>%
  filter(date_surgery <= vas_cutoff,
         # select referrals
         !is.na(date_referral_true) & aaa_size >= 5.5 & 
           surg_method %in% c("01", "02"))

table(extract$surg_method, useNA = "ifany")
# 296 EVAR, 335 open


####
## Where surg_method has response, result_outcome should only have: 
## 15 (approp for surgery and survived 30 days) or 
## 16 (approp for surgery and died within 30 days)
table(extract$result_outcome, extract$surg_method)

# Feb 2023
#     01  02
# 15 294 328
# 16   1   7
# 20   1   0

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
extract %<>%
  filter(result_outcome %in% c("15", "16"))

table(extract$surg_method)
# 295 EVAR, 335 open (matches KPI 4.1/4.2 additional numbers) 
##!! How easy is it to automate this check?? Check against outputs from 
##!! 2. 1, 3, 5, year mortality.sps (CP translating)


#### 3: Create Tables ####
## Tables to show number of AAA repairs for Scotland and HBs by
## financial years and surgery method
# Surgery method -- Health boards
method <- extract %>% 
  group_by(hbres, fy_surgery, surg_method) %>% 
  count(surg_method) %>% 
  ungroup()

# Surgery method -- Scotland
method_scot <- extract %>% 
  group_by(fy_surgery, surg_method) %>% 
  count(surg_method) %>% 
  ungroup() %>% 
  mutate(hbres = "Scotland", .before = fy_surgery)

# Surgery method -- Combine
method <- rbind(method_scot, method)


# Total repairs -- Health boards
repair <- extract %>% 
  group_by(hbres, fy_surgery) %>% 
  count(hbres) %>% 
  ungroup() %>% 
  mutate(surg_method = "03", .after = fy_surgery)

# Total repairs -- Scotland
repair_scot <- extract %>% 
  group_by(fy_surgery) %>% 
  count(fy_surgery) %>% 
  ungroup() %>% 
  mutate(hbres = "Scotland", .before = fy_surgery) %>% 
  mutate(surg_method = "03", .after = fy_surgery)

# Total repairs -- Combine
repair <- rbind(repair_scot, repair)

rm(method_scot, repair_scot)


## Reformat surgery methods tables
## Table of all years from the beginning of the programme ---
repairs_all <- rbind(method, repair) %>% 
  mutate(hbres = fct_relevel(hbres, c("Scotland", "Ayrshire & Arran", "Borders", 
                                      "Dumfries & Galloway", "Fife", "Forth Valley",
                                      "Grampian", "Greater Glasgow & Clyde", 
                                      "Highland", "Lanarkshire", "Lothian", 
                                      "Orkney", "Shetland", "Tayside", 
                                      "Western Isles")),
         surg_method = case_when(surg_method == "01" ~ "EVAR",
                                 surg_method == "02" ~ "Open",
                                 surg_method == "03" ~ "Total AAA repairs"),
         surg_method = fct_relevel(surg_method, c("Open", "EVAR",
                                                  "Total AAA repairs"))) %>% 
  arrange(hbres, fy_surgery, surg_method) %>% 
  glimpse()
  
# Cummulative totals
cumm <- repairs_all %>% 
  group_by(hbres, surg_method) %>% 
  summarize(n = sum(n)) %>% 
  ungroup() %>% 
mutate(fy_surgery = "Cummulative", .after = hbres)

# Reshape data
repairs_hist <- rbind(repairs_all, cumm) %>% 
  pivot_wider(names_from = c(fy_surgery, surg_method), 
              values_from = n, values_fill = 0)


## Table of current 3-year period ---
repairs_current <- repairs_all %>% 
  filter(fy_surgery %in% output_years) %>% 
  # check if all 14 HBs + Scotland are represented
  # Orkney missing, as no recent AAA surgerys performed
  add_row(hbres = "Orkney", fy_surgery = "2021/22",
          surg_method = "Total AAA repairs", n =0) %>% 
  # manually adding in hbres level seems to "unlevel" the variable...
  mutate(hbres = fct_relevel(hbres, c("Scotland", "Ayrshire & Arran", "Borders", 
                                      "Dumfries & Galloway", "Fife", "Forth Valley",
                                      "Grampian", "Greater Glasgow & Clyde", 
                                      "Highland", "Lanarkshire", "Lothian", 
                                      "Orkney", "Shetland", "Tayside", 
                                      "Western Isles")))
         
table(repairs_current$hbres)
# Any HBs with 0 need to be manually entered in previous step!

# Cummulative totals
cumm <- repairs_current %>% 
  group_by(hbres, surg_method) %>% 
  summarize(n = sum(n)) %>% 
  ungroup() %>% 
  mutate(fy_surgery = "Cummulative", .after = hbres)

# Reshape data
repairs_current <- rbind(repairs_current, cumm) %>% 
  pivot_wider(names_from = c(fy_surgery, surg_method), 
              values_from = n, values_fill = 0) %>% 
  arrange(hbres)


### Save files
## Current
saveRDS(repairs_current, paste0(wd_path, "/temp/4_AAA_repairs_current.rds"))

## Historical
saveRDS(repairs_hist, paste0(wd_path, "/temp/4_AAA_repairs_historic.rds"))


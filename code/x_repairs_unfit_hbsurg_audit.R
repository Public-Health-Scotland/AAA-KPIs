#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x_repairs_unfit_hbsurg_audit.R
# Aoife McCarthy
# Adapted from 07_4_vascular_referrals.R and 08_4_unfit_for_surgery.R
# February 2025
#
# Number of AAA repair operations following referral from screening programme
# - audit change is new table grouped by hb_surgery
#
# Men who were referred to vascular services and had a final outcome of unfit
# for surgery
# - audit change is new table grouped by hb_surgery
#
# Written/run on R Studio Server, R version 3.6.1
# Revised on Posit WB, R Version 4.1.2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 1: Housekeeping ---------------------------------------------------------

# Load packages

library(readr)
library(dplyr)
library(forcats)
library(tidylog)
library(tidyr)
library(phsaaa) # to install: devtools::install_github("Public-Health-Scotland/phsaaa")
library(janitor)
library(openxlsx)
library(common)

rm(list = ls())
gc()


source(here::here("code/00_housekeeping.R"))

rm (exclusions_path, hist_path, simd_path, hb_list, gp_prac_extract_date,
    cutoff_date, end_current, end_date, start_date, qpmg_month, extract_date,
    year1_end, year1_start, year2_end, year2_start, year1, year2)



# 2: AAA Repair Operations ---------------------------------------------------

## Call and format Data ----
extract <- read_rds(extract_path) %>% 
  select(hbres, aaa_size, date_referral_true, result_outcome,
         date_surgery, hb_surgery, fy_surgery, surg_method) |> 
  filter(date_surgery <= cut_off_date,
         # select referrals
         !is.na(date_referral_true) & aaa_size >= 5.5) |> 
  # add in surgery groupings (explanation in 05_4_kpi_3.R)
  mutate(
    hb_surgery_grp = case_when(
      hb_surgery %in% c("A", "Y", "L") ~ "Lanarkshire", 
      hb_surgery %in% c("B", "S") ~ "Lothian", 
      hb_surgery %in% c("F", "T") ~ "Tayside", 
      hb_surgery %in% c("V", "W", "G") ~ "Greater Glasgow & Clyde", 
      hb_surgery %in% c("R", "Z", "N") ~ "Grampian", 
      hb_surgery == "H" ~ "Highland", 
      .default = NA),
    hb_surgery_grp = fct_relevel(as.factor(hb_surgery_grp), 
                                 c("Grampian", "Greater Glasgow & Clyde", 
                                   "Highland", "Lanarkshire", "Lothian","Tayside")))

## Checks and filtering ----

table(extract$surg_method, useNA = "ifany")
# 296 EVAR (01), 335 open (02), Mar 2023
# 328 EVAR (01), 372 open (02), Sep 2023
# 369 EVAR (01), 401 open (02), Mar 2024
# 377 EVAR (01), 410 open (02), Sep 2024

###
check <- extract[extract$surg_method == "03",]
# Procedure abandoned (03): Historically, there are only two records  
# (Grampian & WIs); abandoned procedures are not reported on, but good to check
# if any more come up!
rm(check)
###

surg_base <- extract |> 
  filter(surg_method %in% c("01", "02"))

####
## Where surg_method has response, result_outcome should only have: 
## 15 (approp for surgery and survived 30 days) or 
## 16 (approp for surgery and died within 30 days)
table(surg_base$result_outcome, surg_base$surg_method)

#   Feb 2023  Sep 2023    Mar 2024   Sep 2024
#     01  02    01  02    01  02      01  02
# 15 294 328   326 364    366 391    375  400
# 16   1   7     1   8    1  10        1  10
# 17                      1   0       
# 20   1   0     1   0    1   0        1   0

## One record has result_outcome == 20 (other final outcome)
check <- surg_base[surg_base$result_outcome == "20",]
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
surg_base <- surg_base |> 
  filter(result_outcome %in% c("15", "16")) |> 
  filter(!is.na(hb_surgery_grp))

table(surg_base$surg_method)
# 327 EVAR (01), 372 open (02), Sep 2023
# 367 EVAR (01), 401 open (02), Mar 2024
## Check this against the total number of operations as identified in
## KPI 4.1/4.2 Additional A
check <- read_rds(paste0(temp_path, "/4_2_kpi_4_", yymm, ".rds")) |> 
  filter(financial_year == "Cumulative",
         group == "procedures_n")
View(check)
rm(check)

## Create small tables - method and totals ----
### Surgeries by method 
# Surgery method -- Health boards
method_hb <- surg_base %>% 
  group_by(hb_surgery_grp, fy_surgery, surg_method) %>% 
  count(surg_method) %>% 
  ungroup() |> 
  complete(hb_surgery_grp, fy_surgery, surg_method)

# Surgery method -- Scotland
method_scot <- surg_base %>% 
  group_by(fy_surgery, surg_method) %>% 
  count(surg_method) %>% 
  ungroup() %>% 
  mutate(hb_surgery_grp = "Scotland", .before = fy_surgery)

# Surgery method -- Combine
method <- rbind(method_scot, method_hb) 

### All repairs
# Total repairs -- Health boards
repair_hb <- surg_base %>% 
  group_by(hb_surgery_grp, fy_surgery) %>% 
  count(hb_surgery_grp) %>% 
  ungroup() %>% 
  complete(hb_surgery_grp, fy_surgery) |> 
  mutate(surg_method = "99", .after = fy_surgery)

# Total repairs -- Scotland
repair_scot <- surg_base %>% 
  group_by(fy_surgery) %>% 
  count(fy_surgery) %>% 
  ungroup() %>% 
  mutate(hb_surgery_grp = "Scotland", .before = fy_surgery) %>% 
  mutate(surg_method = "99", .after = fy_surgery)

# Total repairs -- combine
repair <- rbind(repair_scot, repair_hb)

rm(method_scot, method_hb, repair_scot, repair_hb) # tidy

### Combine: Surgery types and totals by hb_surgery_grp 
# Historical surgeries data
repairs_all <- rbind(method, repair) %>% 
  mutate(across(where(is.numeric), \(x) replace_na(x, 0))) |> 
  mutate(surg_method = case_when(surg_method == "01" ~ "EVAR",
                                 surg_method == "02" ~ "Open",
                                 surg_method == "99" ~ "Total AAA repairs"),
         surg_method = fct_relevel(surg_method, c("Open", "EVAR",
                                                  "Total AAA repairs"))) %>% 
  arrange(fy_surgery, surg_method) %>% 
  glimpse() 

## Extract current years and calculate cumulative ----
## Current 3-year reporting period
repairs_current <- repairs_all %>% 
  filter(fy_surgery %in% kpi_report_years) 

### Cumulative totals
repairs_cum <- repairs_all %>% 
  group_by(hb_surgery_grp, surg_method) %>% 
  summarize(n = sum(n, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(fy_surgery = "Cumulative", .after = hb_surgery_grp)

## Combine and save ----

repairs_full <- bind_rows(repairs_current, repairs_cum) |> 
  mutate_all(~replace(., is.nan(.), NA))

query_write_rds(repairs_full, paste0(temp_path, "/4_7_vasc_ref_repairs_", yymm, "_by_hbsurg.rds"))

rm(extract, method, repair, repairs_all, repairs_cum, 
   repairs_current, surg_base) # tidy

# 3: Unfit for surgery ----------------------------------------------------

## Call in and format Data ----
extract <- read_rds(extract_path) %>% 
  filter(!is.na(date_referral_true),
         date_screen <= cut_off_date) %>% 
  mutate(size_dichot = if_else(largest_measure >= 5.5, ">=5.5cm", "<5.5cm")) %>% 
  # add in surgery groupings (explanation in 05_4_kpi_3.R)
  ## NOTE having to use "theoretical hb surgery grp" as people "unfit for surg"
  ## obviously do not have a health board of surgery as no procedure should have
  ## happened - thus extrapolating from hbres about theoretical hb_surgery
  mutate(
    theoret_hb_surg_grp = case_when(
      hbres %in% c("Ayrshire & Arran", "Dumfries & Galloway", "Lanarkshire") ~ "Lanarkshire", 
      hbres %in% c("Borders", "Lothian") ~ "Lothian", 
      hbres %in% c("Fife", "Tayside") ~ "Tayside", 
      hbres %in% c("Forth Valley", "Western Isles", "Greater Glasgow & Clyde") ~ "Greater Glasgow & Clyde", 
      hbres %in% c("Orkney", "Shetland", "Grampian") ~ "Grampian", 
      hbres == "Highland" ~ "Highland", 
      .default = NA),
    theoret_hb_surg_gpr = fct_relevel(as.factor(theoret_hb_surg_grp), 
                                 c("Grampian", "Greater Glasgow & Clyde", 
                                   "Highland", "Lanarkshire", "Lothian","Tayside"))) |> 
  # remove NAs as shouldn't have any hbres which are NA
  filter(!is.na(theoret_hb_surg_grp))


table(extract$screen_result)
# 01 (positive) = 898; 02 (negative) = 7    Sep 2022
# 01 (positive) = 1012; 02 (negative) = 7   Mar 2023
# 01 (positive) = 1016; 02 (negative) = 7   Sep 2023
# 01 (positive) = 1148; 02 (negative) = 9   Mar 2023

table(extract$result_outcome, extract$size_dichot)


## Checks and filtering ----

###
# Quick look at individuals who have been operated on and have result_outcome
# as 'other final'
# 'Other final outcome' reported as a single category in report & in records
check <- extract %>% 
  filter(result_outcome == "20",
         !is.na(date_surgery))
# Two records, from 2012/13 and 1014/15, are where surgery was abandoned
# One record, from 2016/17, is Lothian man, mentioned in 91_4_vascular_referrals.R;
# now that HB of surgery recorded, need to f/up re: result_outcome
rm(check)
###


# Remove where categorized as 'referred in error: appointment with 
# vascular service not required'.
extract <- extract |> 
  filter(result_outcome != "02") %>% 
  mutate(outcome_type = case_when(result_outcome %in%
                                    c('01','03','04','05','06','07','08','11',
                                      '12','13','15','16','20') ~ "final outcome",
                                  result_outcome %in% c('09','10','14','17','18',
                                                        '19') ~ "non-final outcome",
                                  is.na(result_outcome) ~ "no outcome recorded",
                                  TRUE ~ "not categorized")) %>% 
  glimpse()


### Unfit for Surgery
unfit_surgery <- extract %>%
  filter(size_dichot == ">=5.5cm",
         outcome_type == "final outcome") %>% 
  # flag anyone unfit for surgery (08)
  mutate(unfit = if_else(result_outcome == "08", 1, 0),
         cohort = 1,
         financial_year = droplevels(financial_year))

table(unfit_surgery$unfit)
# 128 patients unfit  Sep 2022
# 143 patients unfit  Mar 2023
# 151 patients unfit  Sep 2023

## Creating temp tables ----
# Patients unfit for surgery by financial year
unfit_fy <- unfit_surgery %>% 
  group_by(financial_year, theoret_hb_surg_grp) %>% 
  summarize(cohort_n = sum(cohort), 
            unfit_n = sum(unfit)) %>% 
  # cumulative totals for FYs  
  group_modify(~ adorn_totals(.x, where = "row", name = "Scotland")) |> 
  ungroup() |> 
  complete(financial_year, theoret_hb_surg_grp) |> 
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) |> 
  mutate(unfit_p = round_half_up(unfit_n * 100/cohort_n, 1)) |> 
  mutate(unfit_p = ifelse(is.nan(unfit_p), NA, unfit_p)) |> 
  mutate(theoret_hb_surg_grp = fct_relevel(theoret_hb_surg_grp, 
                                           c("Scotland", "Grampian", 
                                             "Greater Glasgow & Clyde", "Highland",
                                             "Lanarkshire", "Lothian", "Tayside"))) |> 
  arrange(theoret_hb_surg_grp, financial_year)

# Cumulative total from programme implementation
unfit_cum <- unfit_surgery %>%
  group_by(theoret_hb_surg_grp) %>%
  summarize(cohort_n = sum(cohort),
            unfit_n = sum(unfit)) %>%
  group_modify(~ adorn_totals(.x, where = "row", name = "Scotland")) |>
  ungroup() |>
  mutate(unfit_p = round_half_up(unfit_n * 100/cohort_n, 1)) |>
  mutate(financial_year = "Cumulative", .before = theoret_hb_surg_grp) |> 
  mutate(theoret_hb_surg_grp = fct_relevel(theoret_hb_surg_grp,
                                           c("Scotland", "Grampian", 
                                             "Greater Glasgow & Clyde", "Highland", 
                                             "Lanarkshire", "Lothian", "Tayside"))) |> 
  arrange(theoret_hb_surg_grp, financial_year)

# Current 3-year reporting period
unfit_current <- unfit_fy %>% 
  filter(financial_year %in% kpi_report_years)

## Arrange and save output -----

unfit_full <- rbind(unfit_current, unfit_cum) |> 
  arrange(financial_year, theoret_hb_surg_grp)

query_write_rds(unfit_full, paste0(temp_path, "/4_8_unfit_for_surgery_", yymm, "_by_theoretical_hb_surg.rds"))

rm(unfit_cum, unfit_fy, unfit_surgery, unfit_current) # tidy



# 4: Arrange for Excel ----------------------------------------------------

## repairs ----

repairs_excel <- repairs_full |> 
  mutate(surg_method = fct_relevel(surg_method, c("Open", "EVAR", "Total AAA repairs")),
         hb_surgery_grp = fct_relevel(hb_surgery_grp, 
                                      c("Scotland", "Grampian", 
                                        "Greater Glasgow & Clyde", "Highland",
                                        "Lanarkshire", "Lothian", "Tayside"))) |> 
  arrange(hb_surgery_grp, fy_surgery, surg_method) |> 
  unite("fy_method", fy_surgery, surg_method, sep = "_") |> 
  pivot_wider(names_from = fy_method, values_from = n)


## unfit ----

unfit_excel <- unfit_full |>
  pivot_longer(cohort_n:unfit_p, names_to = "group", values_to = "value") |> 
  mutate(group = fct_relevel(group, c("cohort_n", "unfit_n", "unfit_p"))) |> 
  arrange(theoret_hb_surg_grp, financial_year, group) |> 
  unite("fy_grp", financial_year, group, sep = "_") |> 
  pivot_wider(names_from = fy_grp, values_from = value)

rm(extract, fy_tibble, hb_tibble) # tidy

# Write to Excel ----------------------------------------------------------
## Housekeeping ----

## Define reporting years
year_xx <- year(cut_off_date)
year_ww <- year_xx - 1
year_vv <- year_xx - 2
year_yy <- year_xx + 1
year_uu <- year_xx - 3
year_ss <- year_xx - 5

## File paths
template_path <- paste0("/PHI_conf/AAA/Topics/Screening/templates")

## Styles ----
source(here::here("code", "src", "Source_Excel_Styles.R"))

## Notes ----
## AAA Repairs 
ending_year_vv <- paste0("Year ending 31 March ", year_vv)
ending_year_ww <- paste0("Year ending 31 March ", year_ww)
ending_year_xx <- eval_seasonal_diff(
  season,
  {paste0("Year ending 31 March ", year_xx, {supsc('p')})}, # spring
  {paste0("Year ending 31 March ", year_xx)} # autumn
)
screened_cum <- eval_seasonal_diff(
  season,
  {paste0("Cumulative total from implementation to 31 March ", year_xx, {supsc('p')})}, # spring
  {paste0("Cumulative total from implementation to 31 March ", year_xx)} # autumn
)
kpi_4_prov <- paste0("p  Provisional. Data for ", year_ww, "/", substr(year_xx, 3, 4),
                     " are for the 11-month period 1 April ", year_ww, " to 28 ",
                     "February ", year_xx, ". The vascular referral data recorded ",
                     "for this period are incomplete at this stage. At ", extract_date,
                     " ", year_xx, " (date of data extraction), a proportion of vascular ",
                     "referrals in the 11-month period will have no vascular outcome data recorded.")

## Unfit for surgery 
refer_year_vv <- paste0("Referrals who were screened in year ending 31 March ", year_vv)
refer_year_ww <- paste0("Referrals who were screened in year ending 31 March ", year_ww)
refer_year_xx <- eval_seasonal_diff(
  season,
  {paste0("Referrals who were screened in year ending 31 March ", year_xx, {supsc('p')})}, # spring
  {paste0("Referrals who were screened in year ending 31 March ", year_xx)} # autumn
)
refer_year_cum <- eval_seasonal_diff(
  season,
  {paste0("Cumulative total referrals who were screened from ",
          "implementation to 31 March ", year_xx, {supsc('p')})}, # spring
  {paste0("Cumulative total referrals who were screened from ",
          "implementation to 31 March ", year_xx)} # autumn
)

unfit_prov <- paste0("p  Provisional. Data for the year ending 31 March ", year_xx, 
                     " are for the 11-month period 1 April ", year_ww, " to 28 February ",
                     year_xx, ". A number of vascual referrals for this period have ",
                     "a non-final outcome or no outcome recorded at this stage; ",
                     "therefore, the figures will change when the data becomes more complete.")		

## Setup workbook ----

# wb <- loadWorkbook(paste0(template_path, "/AAA_repairs_unfit_by_hbsurg_audit_",
#                           season, ".xlsx"))

## testing version
wb <- loadWorkbook(paste0(template_path, "/AAA_repairs_unfit_by_hbsurg_audit_",
                          "spring.xlsx"))

## Write to workbook ----

## AAA Repairs
# notes
writeData(wb, sheet = "AAA Repairs HB of Surgery", ending_year_vv, 
          startRow = 5, startCol = 2)
writeData(wb, sheet = "AAA Repairs HB of Surgery", ending_year_ww, 
          startRow = 5, startCol = 5)
writeData(wb, sheet = "AAA Repairs HB of Surgery", ending_year_xx, 
          startRow = 5, startCol = 8)
writeData(wb, sheet = "AAA Repairs HB of Surgery", screened_cum, 
          startRow = 5, startCol = 11)
addStyle(wb, sheet = "AAA Repairs HB of Surgery", styles$black_border_centre_12,
         rows = 5, cols = 2:13, gridExpand = T)
if (season == "spring") {
  writeData(wb, sheet = "AAA Repairs HB of Surgery", kpi_4_prov, 
            startRow = 18, startCol = 1)
  addStyle(wb, sheet = "AAA Repairs HB of Surgery", styles$orange_11,
           rows = 18, cols = 1)
}
# data
writeData(wb, sheet = "AAA Repairs HB of Surgery", repairs_excel, 
          startRow = 7, colNames = FALSE)
showGridLines(wb, "AAA Repairs HB of Surgery", showGridLines = FALSE)

## Unfit for Surgery
# notes
writeData(wb, sheet = "Unfit for surgery HB of Surgery", refer_year_vv, 
          startRow = 4, startCol = 2)
writeData(wb, sheet = "Unfit for surgery HB of Surgery", refer_year_ww, 
          startRow = 4, startCol = 5)
writeData(wb, sheet = "Unfit for surgery HB of Surgery", refer_year_xx, 
          startRow = 4, startCol = 8)
writeData(wb, sheet = "Unfit for surgery HB of Surgery", refer_year_cum, 
          startRow = 4, startCol = 11)
addStyle(wb, sheet = "Unfit for surgery HB of Surgery", styles$black_border_centre_12,
         rows = 4, cols = 2:13, gridExpand = T)
if (season == "spring") {
  writeData(wb, sheet = "Unfit for surgery HB of Surgery", unfit_prov, 
            startRow = 20, startCol = 1)
  addStyle(wb, sheet = "Unfit for surgery HB of Surgery", styles$orange_11,
           rows = 20, cols = 1)
}
# data
writeData(wb, sheet = "Unfit for surgery HB of Surgery", unfit_excel, 
          startRow = 7,  colNames = FALSE)
showGridLines(wb, "Unfit for surgery HB of Surgery", showGridLines = FALSE)

## Save output ----

query_saveWorkbook(wb, paste0(output_path, "/AAA_repairs_unfit_by_hbsurg_audit_",
                              yymm, ".xlsx"))

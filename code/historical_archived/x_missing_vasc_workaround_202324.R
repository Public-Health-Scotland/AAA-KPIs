#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x_missing_vasc_workaround_202324.R
# Aoife McCarthy
# February 2025
# 
# replacing 202409 instances of 2023/24 screen encounters where vascular module 
# data is missing - i.e. person HAS a vascular referral date, but no follow-up data
#
# output of this script will be used to calculate kpi 3.1 and 3.2 for 2023/24
# 
# Written/run/revised on R Posit WB
# R version 4.1.2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### NB: ONLY RELEVANT TO 202409 RUN UNLESS ANOTHER INSTANCE OF MISSING VASC DATA OCCURS ###

## BACKGROUND INFO

# 2023/24 publication - NHS Lanarkshire flagged missing individuals from KPI 3.1
# Happened just before publication, investigation ensued to understand why no data

# Outcome of investigation was that there was no follow-up vascular data for 11 
# individuals screened in 2023/24 who had been referred to vascular services

# Further investigation found that other HBs also had invididuals missing this data

# DECISION: send details of these cases to relevant HBs and collect the missing data 
# for cols: date_seen_outpatient, result_outcome, date_surgery, hb_surgery, surg_method

# This happened 5th Feb, deadline for HBs sending back data was 14th Feb
# This script is to be used to replace missing values in aaa_extract for 202409 run
# which will recreate KPIs 3.1 and 3.2 for the 2023/24 publication 

## STEPS
# 1: Read in 202409 extract
# 2: Read in filled-in templates from HBs
# 3: Replace values in 202409 extract with those from returned HB Excel templates
# 4: Save out new extract which will be read into 05_4_kpi_3.R to recreate 3.1 and 3.2


# 1: Housekeeping ---------------------------------------------------------

library(readr)
library(dplyr)
library(readxl)
library(tidylog)
library(phsaaa)  # to install: devtools::install_github("Public-Health-Scotland/phsaaa")
library(arsenal)

rm(list = ls())
gc()


source(here::here("code/00_housekeeping.R"))

rm (exclusions_path, output_path, simd_path, fy_tibble, 
    qpmg_month, cutoff_date, year1_end, year1_start, year2_end, year2_start, 
    year1, year2, extract_date, end_current, end_date, fy_list, gp_prac_extract_date, 
    hb_list, hist_path, start_date, cut_off_date, kpi_report_years, season, hb_tibble)

# other paths for this script
## location of filled-in templates from HBs
missing_vasc_templates_path <- paste0("/PHI_conf/AAA/Topics/Investigations/",
                                      "20250129-Lanarkshire-MissingKPI3.1Values/",
                                      "output/missing_vasc_templates/completed")

# save location
output_vasc_updated_extract_path <- substr(extract_path, 1, 53)

# 2: Data import ----------------------------------------------------------

aaa_extract_original <- read_rds(extract_path) |> 
  mutate(row_index = row_number())

# HB data - Borders, FV, GGC, Lan, Lothian
## Borders
borders_vasc <- read_excel(paste0(missing_vasc_templates_path,
                                  "/updated_vasc_data_NHS_Borders_202324.xlsx"), skip = 4)

## Forth Valley 2022/23
fv_vasc_23 <- read_excel(paste0(missing_vasc_templates_path,
                             "/updated_vasc_data_NHS_Forth Valley_202223.xlsx"), skip = 4)

## Forth Valley 2023/24
fv_vasc_34 <- read_excel(paste0(missing_vasc_templates_path,
                             "/updated_vasc_data_NHS_Forth Valley_202324.xlsx"), skip = 4) |> 
  mutate(`Result outcome` = as.character(`Result outcome`))

## GG&C
ggc_vasc <- read_excel(paste0(missing_vasc_templates_path,
                              "/updated_vasc_data_NHS_Greater Glasgow & Clyde_202324.xlsx"), skip = 4) |> 
  select(1:12)

## Lanarkshire 2022/23
lan_vasc_23 <- read_excel(paste0(missing_vasc_templates_path,
                                 "/updated_vasc_data_NHS_Lanarkshire_202223.xlsx"), skip = 4)

## Lanarkshire 2023/24
lan_vasc_34 <- read_excel(paste0(missing_vasc_templates_path,
                                 "/updated_vasc_data_NHS_Lanarkshire_202324.xlsx"), skip = 4)

## Lothian
lothian_vasc <- read_excel(paste0(missing_vasc_templates_path,
                                  "/updated_vasc_data_NHS_Lothian_202324.xlsx"), skip = 4)

names <- names(lan_vasc_23) # want to keep these names

## All HB data
hb_updated_vasc_data <- bind_rows(borders_vasc, fv_vasc_34, ggc_vasc, 
                                  lan_vasc_34, lothian_vasc) 
names(hb_updated_vasc_data) <- names


hb_updated_vasc_data <- bind_rows(hb_updated_vasc_data, fv_vasc_23, lan_vasc_23) |> 
  mutate(hb_surgery = case_when(hb_surgery == "GGC" ~ "G",
                                hb_surgery == "NHS Borders" ~ "B",
                                hb_surgery == "NHS Lothian" ~ "S",
                                hb_surgery == "NHSL" ~ "L",
                                TRUE ~ hb_surgery),
         surg_method = case_when(surg_method == "Open" ~ "02",
                                 surg_method == "open" ~ "02",
                                 surg_method == "EVAR" ~ "01",
                                 surg_method == "FEVAR" ~ "01",
                                 TRUE ~ surg_method))

table(hb_updated_vasc_data$hb_surgery, useNA = "ifany")
table(hb_updated_vasc_data$surg_method, useNA = "ifany")

rm(borders_vasc, fv_vasc_23, fv_vasc_34, ggc_vasc, 
   lan_vasc_23, lan_vasc_34, lothian_vasc) # tidy


### TEST - only use to test the script
# hb_updated_vasc_data <- read_excel(paste0(missing_vasc_templates_path,
#                                           "/X_updated_vasc_data_NHSLanarkshire_202324_TEST.xlsx")) |> 
#   select( -surname, -forename) |> 
#   mutate(result_outcome = as.character(result_outcome))


# 3: Match data between datasets ------------------------------------------

# need to identify full records where the data is missing
# then extract these into a separate dataframe
# and delete them from the original one
# and then replace the missing data
# and bring them back to the main extract

# checks that joins do not produce any duplicates
check1 <- hb_updated_vasc_data |> 
  inner_join(aaa_extract_original, 
             by = c("chi", "hbres", "date_screen", "largest_measure", "date_referral_true"))

check2 <- hb_updated_vasc_data |> 
  left_join(aaa_extract_original, 
            by = c("chi", "hbres", "date_screen", "largest_measure", "date_referral_true"))

check3 <- hb_updated_vasc_data |> 
  right_join(aaa_extract_original, 
             by = c("chi", "hbres", "date_screen", "largest_measure", "date_referral_true"))

# all pass, remove checks
rm(check1, check2, check3)


# join old and new data
matched <- aaa_extract_original |> 
  inner_join(hb_updated_vasc_data, 
             by = c("chi", "hbres", "date_screen", "largest_measure", "date_referral_true")) |> 
  # replace original missing data with new data
  mutate(date_seen_outpatient.x = date_seen_outpatient.y,
         result_outcome.x = result_outcome.y,
         date_surgery.x = date_surgery.y,
         hb_surgery.x = hb_surgery.y,
         surg_method.x = surg_method.y) |> 
  # remove duplicate cols and rename to original col names
  select(- contains(".y")) |> 
  rename(date_seen_outpatient = date_seen_outpatient.x,
         result_outcome = result_outcome.x,
         date_surgery = date_surgery.x,
         hb_surgery = hb_surgery.x,
         surg_method = surg_method.x)


# 3: Create new extract without matched rows ------------------------------

# find the rest of the rows for which there are no matches above
aaa_extract_without_missing_cases <- aaa_extract_original |> 
  anti_join(matched, 
            by = c("chi", "hbres", "date_screen", "largest_measure", "date_referral_true"))

# check that number of rows matches original (the matches + dataset without the matches)
stopifnot("STOP! Row count of new dataframes does not match original data" = 
            (nrow(matched) + nrow(aaa_extract_without_missing_cases)) == nrow(aaa_extract_original))


# 4: Combine updated cases with slim extract -------------------------------

new_extract <- bind_rows(matched, aaa_extract_without_missing_cases) |> 
  arrange(row_index) |> 
  mutate(hbres = as.factor(hbres),
         date_screen = as.Date(date_screen),
         date_referral_true = as.Date(date_referral_true),
         date_seen_outpatient = as.Date(date_seen_outpatient))

# check - should be identical save the updated cases
summary(comparedf(aaa_extract_original, new_extract))

# remove row_index from new extract for saving
new_extract <- new_extract |> 
  select(-row_index, -surname, -forename)

# 5: Save out new extract -------------------------------------------------

query_write_rds(new_extract, paste0(output_vasc_updated_extract_path, 
                                    "/aaa_extract_202409_updated_vasc.rds"))




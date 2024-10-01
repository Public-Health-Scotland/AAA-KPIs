# ~~~~~~~~~~~~~~~~~~~~~~~~~
# 91_2_coverage_gp_practice.R
# Angus Morton
# 01/02/2023
#
# Find coverage rates and number of self-referrals
# by gp practice
#
# Written on R Server (R Version 3.6.1)
# ~~~~~~~~~~~~~~~~~~~~~~~~~

## Notes:
# This script is only run in the autumn and uses AAA GP extract downloaded
# from BOXI. The script can be run after the QPMG has been convened.


### Step 1 : load packages ----

library(readr)
library(dplyr)
library(tidylog)
library(lubridate)
library(openxlsx)
library(janitor)

rm(list = ls())
gc()


source(here::here("code/0_housekeeping.R"))

rm (exclusions_path, hist_path, output_path, simd_path, qpmg_month, extract_date,
    fy_list, hb_list, fy_tibble, hb_tibble, season,
    cut_off_date, cutoff_date, end_current, end_date, start_date,
    year1_end, year1_start, year2_end, year2_start, year1, year2)

## Variables
fy_start <- paste0("01-04-", substr(yymm, 1, 2), (as.numeric(substr(yymm, 3, 4))-1))
fy_end <- paste0("31-03-", substr(yymm, 1, 4))
gp_reg_date <- paste0("01-04-", substr(yymm, 1, 2), substr(yymm, 3, 4)) # day after most recent complete FY

## File paths
## The GP practice history may be a single file or may be downloaded as two
## separate. If two files, use the a_path/b_path; if one, use the history_path
# gp_prac_a_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", yymm, "/data/",
#                           "GP_Practice_History_with_dob_selection_-_prior_to_1_4_1952.csv")
# 
# gp_prac_b_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", yymm, "/data/",
#                           "GP_Practice_History_with_dob_selection_-_post_1_4_1952.csv")

gp_history_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", yymm,
                          "/data/GP_Practice_History_with_dob_selection.csv")

prev_gp_data_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", yymm - 100,
                            "/data/gp_coverage_", substr(kpi_report_years[2], 3, 4),
                            substr(kpi_report_years[2], 6, 7), ".rds")

gp_output_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", yymm,
                         "/data/gp_coverage_", substr(kpi_report_years[3], 3, 4),
                         substr(kpi_report_years[3], 6, 7), ".rds")

gp_lookup_path <- paste0("/conf/linkage/output/lookups/Unicode/",
                         "National Reference Files/gpprac.csv")


## Function
# Create new GP variables (gp_hb and gp_desc) for use in making final output.

# The spss script has a big gp practice if/else statement
# for assigning gp practice codes to health boards.
# !! come back to check if this is the right thing

make_gp_vars <- function(df, gp_lookup) {
  
  # flag for if an individual is registered inside/outside their hb of residence
  dat <- df %>%
    mutate(in_hb = case_when(
      gp_join == 3139 ~ 1,
      hbres == "Ayrshire & Arran" & between(gp_join, 8000, 8399) ~ 1,
      hbres == "Borders" & between(gp_join, 1600, 1799) ~ 1,
      hbres == "Fife" & between(gp_join, 2000, 2499) ~ 1,
      hbres == "Lanarkshire" & 
        (between(gp_join, 6000, 6599) |
           gp_join %in% c(4626,4627,4653,4654,
                          4900,4902,4905,4906,
                          4911,4925,4943,4952,
                          4964,4969,4970,4979)) ~ 1,
      hbres == "Greater Glasgow & Clyde" &
        (between(gp_join, 4000, 5499) |
           between(gp_join, 8499, 8799)) &
        !gp_join %in% c(4626,4627,4653,4654,
                        4900,4902,4905,4906,
                        4911,4925,4943,4952,
                        4964,4969,4970,4979,
                        8500,8511,8514,8515,
                        8519) ~ 1,
      hbres == "Highland" &
        (between(gp_join, 5500, 5999) |
           between(gp_join, 8400, 8498) |
           gp_join %in% c(8500,8511,8514,8515,8519)) ~ 1,
      hbres == "Grampian" & between(gp_join, 3000, 3799) ~ 1,
      hbres == "Lothian" & between(gp_join, 7000, 7999) ~ 1,
      hbres == "Orkney" & between(gp_join, 3800, 3899) ~ 1,
      hbres == "Tayside" & between(gp_join, 1000, 1599) ~ 1,
      hbres == "Forth Valley" & between(gp_join, 2500, 2999) ~ 1,
      hbres == "Western Isles" & between(gp_join, 9000, 9099) ~ 1,
      hbres == "Dumfries & Galloway" & between(gp_join, 1800, 1999) ~ 1,
      hbres == "Shetland" & between(gp_join, 3900, 3999) ~ 1,
      is.na(gp_join) ~ 1,
      TRUE ~ 0),
      gp_join = as.character(gp_join))
  
  # create a variable for practice code if registered in health board
  dat <- dat %>%
    mutate(gp_hb = case_when(
      in_hb == 0 ~ "Practice outside hb area",
      TRUE ~ gp_join
    ))
  
  # Join on gp lookup to get names
  dat <- dat %>%
    left_join(gp_lookup, by = "gp_join")
  
  # Create gp description for output
  dat <- dat %>%
    mutate(gp_desc = if_else(gp_hb == "Practice outside hb area",
                             "Practice outside hb area", gp_desc))
  
  dat
  
}



### Step 2 : Import data ----

coverage_basefile <- read_rds(paste0(temp_path,
                                     "/1_2_coverage_basefile.rds"))

gp_history <- read_csv(gp_history_path)
# gp_prac_a <- read_csv(gp_prac_a_path)
# gp_prac_b <- read_csv(gp_prac_b_path)

gp_lookup <- read_csv(gp_lookup_path) %>%
  select(gp_join = praccode,
         gp_desc = `add 1`) %>%
  mutate(gp_join = substr(gp_join, 1, 4)) %>%
  filter(!(gp_join == 9999 & gp_desc == "PATIENTS REGISTERED WITH A GP"))


### Step 3 : Add GP registration episodes to coverage data ----
# Rename dataframe
# Use first line only if two gp_prac files
#gp_prac <- bind_rows(gp_prac_a, gp_prac_b)

gp_prac <- gp_history |> 
  clean_names()

rm(gp_history)


# Flag GP practice that was relevant at the end of the financial year
## AMc question: how do these dates relate to the 'end' of the financial year? should they be automated?
## also, what does the { is.na(valid_from) & is.na(valid_to) > dmy("01-04-2022") } do?
## I think this date relates to the 2021/22 FY run
gp_prac <- mutate(gp_prac,
                  valid = case_when(
                    valid_from < dmy(gp_reg_date) & 
                    valid_to > dmy(gp_reg_date) ~ 1,
                    valid_from < dmy(gp_reg_date) & is.na(valid_to) ~ 1,
                    is.na(valid_from) & is.na(valid_to) > dmy(gp_reg_date) ~ 1,
                    TRUE ~ 0))

gp_prac <- filter(gp_prac, valid == 1)

coverage_gp <- coverage_basefile %>%
  left_join(gp_prac, by = c("upi" = "upinumber")) %>%
  select(-c(area_of_residence, valid_from, valid_to, valid))


### Step 3 : Assign GP practices to health boards ---

# make practice code variable for joining
coverage_gp <- coverage_gp %>%
  mutate(gp_join = substr(practice_code, 2, 5),
         gp_join = as.numeric(gp_join))

# add gp_hb and gp_desc
coverage_gp <- make_gp_vars(coverage_gp, gp_lookup)


### Step 4 : Calculate KPI 1.2a by GP ----

# Calculate KPI 1.2a (coverage)
breakdown_1_2a <- coverage_gp %>%
  group_by(gp_hb, gp_desc, hbres) %>%
  summarise(
    across(cohort_year1:test_b_not_assigned, \(x) sum(x, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  mutate(gp_hb = if_else(is.na(gp_hb), "Unknown Practice", gp_hb))

hb_1_2a <- coverage_gp %>%
  group_by(hbres) %>%
  summarise(
    across(cohort_year1:test_b_not_assigned, \(x) sum(x, na.rm = TRUE))
  ) %>%
  ungroup() %>%
  mutate(gp_hb = hbres)

breakdown_1_2a <- bind_rows(breakdown_1_2a, hb_1_2a)

# Create percentages
breakdown_1_2a <- breakdown_1_2a %>%
  mutate(
    percent_a_year1 = (test_a_year1/cohort_year1)*100,
    percent_a_add_year1 = (test_a_add_year1/cohort_year1)*100
  )

# Output tables
output_1_2a <- breakdown_1_2a %>%
  select(hbres, gp_hb, gp_desc,
         cohort_year1, test_a_year1, percent_a_year1) %>%
  arrange(hbres, gp_hb)

## Save output for use next year
write_rds(output_1_2a, gp_output_path)


### Step 5 : Create KPI 1.2a GP output ----

# bring in previous year's data
# update practice names using most recent lookup to avoid duplication
prev_gp_data <- read_rds(prev_gp_data_path) %>%
  select(-gp_desc) |> 
  left_join(gp_lookup, by = c("gp_hb" = "gp_join")) |> 
  select(hbres, gp_hb, 
         # gp_desc,
         cohort_year_ww = cohort_year1,
         test_year_ww = test_a_year1,
         percent_year_ww = percent_a_year1)

# Format to be like the old file. This step can be changed when the macro
# gets converted to R
output_2year <- output_1_2a %>%
  mutate(gp_desc = if_else(gp_desc == "Practice outside hb area", 
                           as.character(NA), gp_desc)) %>%
  full_join(prev_gp_data, by = c("hbres", "gp_hb"
                                 #, "gp_desc"
                                 )) %>%
  select(hbres, gp_hb, 
         #gp_desc,
         cohort_year_ww,
         test_year_ww,
         percent_year_ww,
         cohort_year_xx = cohort_year1,
         test_year_xx = test_a_year1,
         percent_year_xx = percent_a_year1) %>%
  mutate(sortorder = case_when(
    hbres == gp_hb ~ 1,
    gp_hb == "Unknown Practice" ~ 3,
    gp_hb == "Practice outside hb area" ~ 4,
    TRUE ~ 2
  )) %>%
  arrange(hbres, sortorder, gp_hb) %>%
  select(-sortorder)

# Change 'NA's to 'NaN's in the two percentage columns for the macro.
# (This will say it's done nothing but it has)
# Re-code the NAs in the cohort/test columns as 0 (to match macro formatting).
x <- output_2year %>%
  mutate(across(contains("percent"), \(x) replace_na(x, NaN)),
         across(contains("percent"), \(x) round(x, digits = 2)),
         across(contains(c("cohort", "test")), \(x) replace_na(x, 0)))


# Write to excel
wb <- createWorkbook()
addWorksheet(wb, "data")
writeData(wb, sheet = "data", output_2year)
saveWorkbook(wb, paste0(temp_path, "/gp_practice_all_boards.xlsx"), 
             overwrite = TRUE)

rm(hb_1_2a, output_1_2a, prev_gp_data)

### Step 6 : Self referrals ----

extract <- read_rds(extract_path)

# Select screen dates from relevant financial year
extract_slim <- extract %>%
  filter(date_screen >= dmy(fy_start),
         date_screen <= dmy(fy_end))

# Trim
# pat_elig : Self referrals
# screen_type : initial or initial QA
# screen_result : exclude technical fails and external results
extract_slim <- extract_slim %>%
  filter(pat_elig == "03" &
           screen_type %in% c("01","03") &
           screen_result %in% c("01","02","04"))

# join on AAA GP practice file
extract_gp <- extract_slim %>%
  left_join(gp_prac |> rename(gp_practice_code = practice_code), 
            by = c("upi" = "upinumber"))

extract_gp <- extract_gp %>%
  mutate(gp_join = substr(practice_code, 2, 5),
         gp_join = as.numeric(gp_join))

extract_gp <- make_gp_vars(extract_gp, gp_lookup)

# aggregate to get totals

self_ref_gp <- extract_gp %>%
  group_by(gp_hb, gp_desc, hbres) %>%
  summarise(individuals = n()) %>%
  ungroup() %>%
  select(hbres, gp_hb, gp_desc, individuals)

self_ref_hb <- extract_gp %>%
  group_by(hbres) %>%
  summarise(individuals = n()) %>%
  ungroup() %>%
  mutate(gp_hb = hbres) %>%
  select(hbres, gp_hb, individuals)

self_ref_gp <- self_ref_gp %>%
  bind_rows(self_ref_hb)

self_ref_gp <- self_ref_gp %>%
  mutate(sortorder = case_when(
    hbres == gp_hb ~ 1,
    TRUE ~ 2
  )) %>%
  arrange(hbres, sortorder) %>%
  select(-sortorder)

# save out
wb <- createWorkbook()
addWorksheet(wb, "data")
writeData(wb, sheet = "data", self_ref_gp)
query_saveWorkbook(wb, paste0(temp_path, "/sr_all_boards.xlsx"))

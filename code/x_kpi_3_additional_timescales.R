#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x_kpi_3_additional_timescales.R
# Calum Purdie & Karen Hotopp
# Adapted by Aoife McCarthy
# 29/01/2025
# 
# KPI 3.1 percentage of men with AAA >= 5.5cm seen within two weeks of screening
# additional col = seen within 4 weeks of screening (audit for Spring 2025 QPMG)
#
# KPI 3.2 percentage of men with AAA >= 5.5cm deemed appropriate for 
# intervention/operated on within 8 weeks of screening
# additional cols = sugery within 3/6/9/12 months of screening (audit for Spring 2025 QPMG)
#
# Written/run on R Studio Server, R version 3.6.1
# Revised on Posit WB, R Version 4.1.2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1: Housekeeping ----
# Load packages
library(dplyr)
library(readr)
library(lubridate)
library(janitor)
library(tidylog)
library(tidyr)
library(phsaaa) # to install: devtools::install_github("Public-Health-Scotland/phsaaa")
library(forcats)

rm(list = ls())
gc()


source(here::here("code/00_housekeeping.R"))

rm (exclusions_path, output_path, simd_path, fy_tibble, 
    qpmg_month, cutoff_date, year1_end, year1_start, year2_end, year2_start, 
    year1, year2, extract_date)


# 2: Data Manipulation ----
# Keep records where date_referral_true is populated and largest measure is 
# greater than or equal to 5.5, where result_outcome is not "02" and 
# date_screen is less than or equal to cut_off_date
aaa_extract <- read_rds(extract_path) %>% 
  filter(!is.na(date_referral_true) & largest_measure >= 5.5, 
         result_outcome != "02", 
         date_screen <= cut_off_date)


# 3: KPI 3.1 ----

## AMc note: additional column is "seen within 4 weeks of screening" - requested at Oct 2024 QPMG
# will be management information for now, and will be printed into a separate workbook of audit materials
# after Spring 2025 QPMG we should know whether this is being kept or not

# Calculate time between date seen in outpatients to date of screening
kpi_3_1_base <- aaa_extract %>% 
  mutate(screen_to_seen = time_length(date_screen %--% date_seen_outpatient, 
                                      "days"), 
         seen_2wks = case_when(screen_to_seen <= 14 ~ 1, 
                               TRUE ~ 0),
         #  new var - seen within 4 weeks - audit for Spring KPIs 2025 - management information
         seen_4wks = case_when(screen_to_seen <= 28 ~ 1,
                               .default = 0))

# KPI 3.1 provisional footnote - people referred but not yet seen
# This section should be applied to the spring QPMG run because vascular data for
# the year end is not complete at that stage - it should not be run when
# producing data for the complete year end from the September extract
if (season == "spring"){
  
  pending <- kpi_3_1_base %>% 
    filter(financial_year == kpi_report_years[3]) %>% 
    mutate(pending_appt = case_when(!is.na(date_referral_true) & is.na(date_seen_outpatient) ~ 1, # people referred, but not yet seen
                                    TRUE ~ 0)) |> 
    count(pending_appt) |> 
    mutate(pending_appt = case_when(pending_appt == 0 ~ "Total referrals",
                                    pending_appt == 1 ~ "Referral date, no outpatient date",
                                    TRUE ~ "error"))
  
  query_write_rds(pending, paste0(temp_path, "/4_provisional_note_3.1.rds"))
  
}


# Keep records where result_outcome is "01", "03", "04" or "05" or where
# date_seen_outpatient is populated and screen_to_seen is greater than or equal 
# to zero
kpi_3_1_base <- kpi_3_1_base %>% 
  filter(result_outcome %in% c("01", "03", "04", "05") | 
           !is.na(date_seen_outpatient) & screen_to_seen >= 0) |> 
  mutate(financial_year = droplevels(financial_year))


## Health Board of Residence ----
# Health Boards
kpi_3_1_hb <- kpi_3_1_base %>% 
  group_by(hbres, financial_year) %>% 
  summarise(cohort_n = n(), 
            seen_2wks_n = sum(seen_2wks),
            seen_4wks_n = sum(seen_4wks)) |> 
  ungroup() |> 
  complete(hbres, financial_year) |> # completing table - all HBs for each FY
  mutate_at(vars(cohort_n:seen_4wks_n), ~ifelse(is.na(.), 0, .))

# Scotland
kpi_3_1_scot <- kpi_3_1_base %>% 
  group_by(financial_year) %>% 
  summarise(cohort_n = n(), 
            seen_2wks_n = sum(seen_2wks),
            seen_4wks_n = sum(seen_4wks)) %>% 
  mutate(hbres = "Scotland", .before = financial_year) |>
  ungroup()

# Combine
kpi_3_1 <- bind_rows(kpi_3_1_scot, kpi_3_1_hb) %>% 
  group_by(hbres) %>% 
  mutate(cover_2wks_p = round_half_up(seen_2wks_n * 100 / cohort_n, 1),
         cover_4wks_p = round_half_up(seen_4wks_n * 100 / cohort_n, 1)) |> 
  mutate(kpi = "KPI 3.1 Residence", .after = hbres) |>
  ungroup()

# arrange into longer format and order vars
kpi_3_1 <- kpi_3_1 |>
  pivot_longer(!hbres:financial_year, names_to = "group", values_to = "value") |> 
  mutate(group = fct_relevel(group, c("cohort_n", "seen_2wks_n", "cover_2wks_p", "seen_4wks_n", "cover_4wks_p"))) |> 
  arrange(hbres, financial_year, group) |> 
  rename(health_board = hbres)

# change NaNs to NAs
kpi_3_1$value[is.nan(kpi_3_1$value)] <- NA

table(kpi_3_1$health_board, kpi_3_1$financial_year) # all hbres/FY are 3 (5 now since added temp 4wks var)

# Tidy environment
rm(kpi_3_1_base, kpi_3_1_hb, kpi_3_1_scot)




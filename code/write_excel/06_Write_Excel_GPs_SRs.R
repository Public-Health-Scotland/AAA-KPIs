#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 06_Write_Excel_GPs_SRs.R
# Aoife McCarthy
# October 2024
# 
# Create KPI 1.2a by GP Practice + SR counts Excels for individual Health Boards
# 
# Written/run/revised on R Posit WB
# R version 4.1.2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### ONLY RUN IN AUTUMN ###

# 1. Housekeeping ---------------------------------------------------------

## libraries ----
library(dplyr)
library(readr)
library(tidylog)
library(openxlsx)
library(phsaaa) # devtools::install_github("Public-Health-Scotland/phsaaa")

rm(list=ls())
gc()

## functions ----

## AMc note: have transferred all these (plus create_gp_extracts) over to phsaaa, so once integrated, these can be deleted

# simplify year (from 20XX/YY to XXYY)
simplify_fy <- function(financial_year) {
  paste0(substr(financial_year, 3, 4), substr(financial_year, 6, 7))
}


# function to create hyperlink for openxlsx
format_excel_hyperlink <- function(name, url) {
  
  link <- data.frame(x = paste0("HYPERLINK(\"", url, "\", \"", name, "\")" ))
  class(link$x) <- c("formula","hyperlink")
  
  return(link)
  
}

## variables -----
source(here::here("code", "00_housekeeping.R"))

rm(fy_tibble, fy_list, cutoff_date, end_current, end_date, exclusions_path, extract_path,
   hb_list, hist_path, qpmg_month, simd_path, start_date, temp_path, year1, year1_end,
   year1_start, year2, year2_end, year2_start)

year_xx <- year(cut_off_date)
year_ww <- year_xx - 1


# filepaths
current_gp_data_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", yymm, 
                               "/data/gp_coverage_", simplify_fy(kpi_report_years[3]),".rds")

prev_gp_data_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", yymm - 100,
                            "/data/gp_coverage_", simplify_fy(kpi_report_years[2]), ".rds")

sr_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", yymm, 
                  "/data/self_referrals_", simplify_fy(kpi_report_years[3]), ".rds")

gp_lookup_path <- paste0("/conf/linkage/output/lookups/Unicode/",
                         "National Reference Files/gpprac.csv")


# 2: Data import ----

## GP lookup ----

lookup <- read_csv(gp_lookup_path) %>%
  select(gp_join = praccode,
         gp_desc = `add 1`) %>%
  mutate(gp_join = substr(gp_join, 1, 4)) %>%
  filter(!(gp_join == 9999 & gp_desc == "PATIENTS REGISTERED WITH A GP"))

## GP coverage data for current FY and previous FY ----
# gp_desc removed as GPs update their names year on year
# i.e. need to join prev + current on gp code alone, then add on new names afterwards
gp_current <- read_rds(current_gp_data_path) |> 
  rename(cohort_year_xx = cohort_year1,
         test_year_xx = test_a_year1,
         percent_year_xx = percent_a_year1) |> 
  select(-gp_desc) 

gp_prev <- read_rds(prev_gp_data_path) |> 
  rename(cohort_year_ww = cohort_year1,
         test_year_ww = test_a_year1,
         percent_year_ww = percent_a_year1) |> 
  select(-gp_desc) 

gp_data <- gp_prev |> 
  full_join(gp_current) |> 
  left_join(lookup, by = c("gp_hb" = "gp_join")) |> 
  mutate(across(contains(c("cohort", "test")), \(x) replace_na(x, 0))) |> 
  mutate(across(contains("percent"), \(x) replace(x, is.nan(x), NA))) |> 
  mutate(sortorder = case_when(
    hbres == gp_hb ~ 1,
    gp_hb == "Unknown Practice" ~ 3,
    gp_hb == "Practice outside hb area" ~ 4,
    TRUE ~ 2
  )) %>%
  arrange(hbres, sortorder, gp_hb) %>%
  select(-sortorder) |> 
  relocate(gp_desc, .after = gp_hb)

rm(gp_current, gp_prev)

## self-referral data just for current FY ----
sr_data <- read_rds(sr_path)


# 3: Write Excel workbooks ------------------------------------------------

# source function
source(here::here("code", "src", "Source_Excel_functions.R"))

# list hbs for loop
health_boards <- hb_tibble |> filter(hbres != "Scotland") |> pull()

# create GP Practices folder in output path
ifelse(!dir.exists(file.path(output_path, "GP Practices")), dir.create(file.path(output_path, "GP Practices")), "GP Practice directory already created")

# run loop
eval_seasonal_diff(season,
                   {print("GP Practice coverage and self-referrals are only calculated in Autumn")}, # spring
                   {for (i in health_boards) {
                     
                     create_aaa_gp_outputs(hb_name = i,
                                           financial_year = kpi_report_years[3],
                                           coverage_data = gp_data, 
                                           selfref_data = sr_data, 
                                           output_filepath = output_path,
                                           date_aaa_extracted = extract_date,
                                           date_gp_extracted = gp_prac_extract_date
                                           )}} # autumn
                   )

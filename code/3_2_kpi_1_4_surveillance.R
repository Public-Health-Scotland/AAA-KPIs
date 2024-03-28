#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3_2_kpi_1_4_surveillance.R
# Gavin Clark
# September 2023
# Create RDS files for KPI 1.4 a, b, surveillance table 6, and DNA exclusions
# Written/run on R Studio Server
# R version 3.6.1
# Revised/Run on Posit WB
# R version 4.1.2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Step 1: Housekeeping ----
# Update dates in 0_housekeeping.R before running this script.
if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  dplyr,
  magrittr,
  phsmethods,
  lubridate,
  janitor,
  tidyr,
  arsenal,
  glue,
  readr,
  tidylog
)

rm(list = ls())
gc()


source(here::here("code/0_housekeeping.R"))

rm (output_path, simd_path, fy_list, fy_tibble,
    cut_off_date, cutoff_date, end_current, end_date, start_date,
    year1_end, year1_start, year2_end, year2_start, year1, year2)

## Functions
history_building <- function(df, season){
  
  df
  
  if (season == "spring") {
    table(hist_db$kpi, hist_db$fin_year) 
    
    print("Don't add to the history file. Move along to next step")
    
  } else {
    
    if (season == "autumn") {
      ## Combine data from this script (KPI 1.4 a & b)
      df <- df |>
        filter(fin_year == kpi_report_years[3])
      
      new_hist_db <- bind_rows(hist_db, df)
      
      print(table(new_hist_db$kpi, new_hist_db$fin_year)) 
      
      ## Write out new historic file
      write_rds(new_hist_db, paste0(hist_path, "/aaa_kpi_historical_theme2.rds"))
      # change permissions to give the group read/write
      Sys.chmod(paste0(hist_path, "/aaa_kpi_historical_theme2.rds"),
                mode = "664", use_umask = FALSE)
      
      print("You made history! Proceed to the next script.")
      
    } else {
      
      print("Go check your calendar!")
      
    }
  }
}


## Step 2: Read in, check, and format files ----

# AAA extract
aaa_extract <- readRDS(extract_path)

# AAA exclusions
aaa_exclusions <- readRDS(exclusions_path)

### 2.2: Check Exclusions and filter ----
# check number of records
aaa_exclusions %>% nrow()
# 123,791 rows 2022/03
# 133,707 rows 2022/09
# 143,256 rows 2023/03
# 153,558 rows 2023/09
# 164,471 rows 2024/03

# organize data
aaa_exclusions %<>%
  select(upi, pat_inelig, date_start, date_end) %>% 
  arrange(upi, date_start, date_end) %>% 
  glimpse()

# List exclusions
exclusions <- aaa_exclusions %>%
  filter((pat_inelig %in% c('04','06','11','12','13','14',
                            '15','16','17','18','19','21', 
                            '22', '25', '26') &
            is.na(date_end))|
           pat_inelig %in% c('03','05')) |> 
  select(upi, date_start, date_end)

### 2.3: Check and filter extract to create cohort base ----

# create fin_month fields
# select relevant columns
aaa_extract %<>% 
  mutate(month = format(as.Date(date_screen, format = "%Y-%m-%d"), "%m"),
         fin_month = case_when(month %in% c('04','05','06','07','08','09','10',
                                            '11','12') ~ as.numeric(month) - 3,
                               month == '03' ~ 12,
                               month == '02' ~ 11,
                               month == '01' ~ 10)) %>%
  select(-month) %>% 
  select(-c(chi, dob:practice_name, ca2019:location_code, apl_measure,
            apt_measure, date_referral:date_seen_outpatient, 
            first_outcome:audit_batch_outcome )) %>% # eligibility_period:dob_eligibility needed?
  arrange(upi, date_screen) %>% 
  glimpse()
# 493,121 rows 2022/03
# 523,774 rows 2022/09
# 551,027 rows 2023/03
# 579,917 rows 2023/09
# 609,774 rows 2024/03

### 2.4: Create cohort of all of the screenings ----
screened_cohort <- aaa_extract %>% 
  # Can remove those screened more than a year prior to the period of interest
  # GC - issue - add to housekeeping script
  # GC - or remove if we are taking last three years
  #filter(date_screen >= as.Date(cut_off_12m)) |>
  mutate(screen_flag = ifelse(screen_result %in% c("01","02","04") &
                                (screen_type %in% c("02","04")), 1, 0)) %>%
  filter(screen_flag == 1) |>
  # These will be the attendance at the follow-up screening appointment
  # hence renaming as date_next_screen
  rename(date_next_screen = date_screen) %>%
  select(upi, hbres, date_next_screen, 
         next_screen_result = screen_result, screen_type)

## Step 3: Create surveillance cohort ----

### 3.1: Define cohort ----

# This should be:
## Everyone who was invited in the prior period, with a recommendation of 12
## month surveillance, where the screening date plus 365 is in the current 
## financial year
annual_surveillance_cohort <- aaa_extract %>% 
  filter(!is.na(financial_year)) %>% 
  filter(followup_recom == "02") %>% 
  mutate(date_next_screen_due = date_screen + 365,
         fy_due = extract_fin_year(date_next_screen_due)) %>%
  filter(fy_due %in% kpi_report_years) %>%
  # This is the appointment where the recommendation was surveillance
  # so renaming to make this clear
  rename(date_screen_surv = date_screen) 

## Check for duplicates, check the dates of screening to see how far apart
check_dups <- annual_surveillance_cohort %>%
  group_by(upi, fy_due) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n > 1)
# 50, mixture of date ranges, though most are close together
# KH: what columns are we actually checking here?

# Remove those where the appointment is within less than six months
# (arbitrary value)
annual_surveillance_cohort <- annual_surveillance_cohort |>
  arrange(upi, date_next_screen_due) |>
  group_by(upi) |>
    mutate(appt_less_6m = if_else(
      date_next_screen_due - lag(date_next_screen_due) < 180, 1, 0)) |>
    ungroup() |>
  filter(is.na(appt_less_6m)|appt_less_6m == 0)

## Check again for duplicates, check the dates of screening to see how far apart
check_dups <- annual_surveillance_cohort %>%
  group_by(upi, fy_due) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n > 1)
# 10, look more reasonable
# KH: not clear 

# for Spring run, introduce date filter to allow enough follow-up time 
# to accurately represent KPI 1.4a
# i.e. need date_next_screen_due to be >6 weeks before March extract date
# not required in autumn as Sept extract allows enough f-up time from FY end

if (season == "spring"){
  annual_surveillance_cohort <- annual_surveillance_cohort %>% 
    filter(date_next_screen_due <= paste0(substr(yymm, 1, 4), "-01-01"))
}

### 3.2: Identify those with follow-up within appropriate timeframe ----

# Create list of those with a follow-up appointment within the standard
# by merging 12-month surveillance cohort with screened cohort
annual_surveillance_f_up <- annual_surveillance_cohort |>
  inner_join(screened_cohort, by = "upi") |>
  # Have had a screening appointment after the surveillance appointment
  filter(date_next_screen > date_screen_surv) %>%
  # Person has a screen due in the period of interest
  # and they have been screened within 365 days plus 6 weeks
  mutate(met_kpi_1_4a = if_else(between(date_next_screen, 
                                        date_screen_surv,
                                        date_next_screen_due + 6 * 7), 1, 0)) %>%
  # Create list of compliant CHIs for linking to cohort
  group_by(upi, fy_due) |>
  summarise(met_kpi_1_4a = max(met_kpi_1_4a)) %>%
  ungroup()

### 3.3: Identify exclusions ----

# Create list of those in the cohort with an exclusion within the time
# they should have had a surveillance appointment
annual_exclusions <- annual_surveillance_cohort |>
  inner_join(exclusions, by = "upi") |>
  select(upi, date_start, date_end, date_screen_surv, 
         date_next_screen_due) |>
  mutate(exclusion = case_when(
    # Have an exclusion with a start date between last screen (plus one so that 
    # appointments for that screen are still counted) and due date of next 
    # screen minus one to include those excluded at their follow-up
    between(date_start, date_screen_surv+1, date_next_screen_due-1) ~ 1,
    # Have an exclusion which started after date next screen due
    date_start >= date_next_screen_due ~ 0,
    # Have a exclusion which has not yet ended, e.g. excluded before
    # last appointment(not sure if this is possible)
    is.na(date_end) ~ 1,
    # Have an exclusion which ended between last screen and next screen due date
    between(date_end, date_screen_surv, date_next_screen_due + 6 * 7) ~ 1,
    # Keep everyone else
    TRUE ~ 0)) |>
  filter(exclusion == 1) |>
  select(-date_next_screen_due)

### 3.4: Create final annual surveillance file and save ----

annual_surveillance_w_excl <- annual_surveillance_cohort |>
  left_join(annual_surveillance_f_up, by = c("upi", "fy_due")) |>
  mutate(met_kpi_1_4a = replace_na(met_kpi_1_4a, 0)) |>
  # Join creates duplicates, but they are excluded anyway so doesn't matter?
  left_join(annual_exclusions, by = c("upi", "date_screen_surv")) %>%
  filter(is.na(exclusion))

# Check for duplicates  
check_dups <- annual_surveillance_w_excl %>%
  group_by(upi, fy_due) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  filter(n > 1)
# 8 duplicates, look ok

# Save annual surveillance cohort (only if needed for checking)
# saveRDS(annual_surveillance_w_excl, paste0(temp_path, 
#                                            "/2_4_kpi_1_4a_annual.rds"))

rm(annual_exclusions, annual_surveillance_cohort, annual_surveillance_f_up,
   check_dups)


## Step 4: Create quarterly surveillance cohort ----

### 4.1: Define cohort ----

# This should be:
## Everyone who was invited in the prior period, with a recommendation of 12
## month surveillance, where the screening date plus 365 is in the current 
## financial year
quarterly_surveillance_cohort <- aaa_extract %>% 
  filter(!is.na(financial_year)) %>% 
  filter(followup_recom == "01") %>% 
  # GC - 3 months is not a consistent number, going with 91
  mutate(date_next_screen_due = date_screen + 91,
         fy_due = extract_fin_year(date_next_screen_due)) %>%
  filter(fy_due %in% kpi_report_years) %>%
  rename(date_screen_surv = date_screen) |>
  select(upi, hbres, fy_due, date_screen_surv, date_next_screen_due)

## Duplicates are expected in this cohort

# for Spring run, introduce date filter to allow enough follow-up time 
# to accurately represent KPI 1.4b
# i.e. need date_next_screen_due to be >4 weeks before March extract date
# not required in autumn as Sept extract allows enough f-up time from FY end

if (season == "spring"){
  quarterly_surveillance_cohort <- quarterly_surveillance_cohort %>% 
    filter(date_next_screen_due <= paste0(substr(yymm, 1, 4), "-01-01"))
}

### 4.2: Identify those with follow-up within appropriate timeframe ----

# Create list of those with a follow-up appointment within the standard
quarterly_surveillance_f_up <- quarterly_surveillance_cohort |>
  inner_join(screened_cohort, by = "upi") |>
  # Have had a screening appointment after the surveillance appointment
  filter(date_next_screen > date_screen_surv) %>%
  # Person has a screen due in the period of interest
  mutate(met_kpi_1_4b = if_else(
    between(date_next_screen, 
            # There are a small number of cases which are within a month
            # Excluding as presumably these are repeat screens in some way(?)
            # Perhaps one to double-check
            date_screen_surv + 30,
            # and they have been screened within 90 days plus 4 weeks
            date_next_screen_due + 4 * 7), 1, 0)) %>%
  # Create list of compliant CHIs
  filter(met_kpi_1_4b == 1) %>%
  # Small number where there appear to be two follow-up screenings for the
  # next appointment. Arbitrarily keep the earlier appointment
  arrange(upi, date_screen_surv, date_next_screen) |>
  distinct(upi, date_screen_surv, .keep_all = TRUE) |>
  select(upi, date_screen_surv, date_next_screen, met_kpi_1_4b)

### 4.3: Identify exclusions ----

quarterly_exclusions_list <- quarterly_surveillance_cohort |>
  inner_join(exclusions, by = "upi") |>
  select(upi, date_start, date_end, date_screen_surv, 
         date_next_screen_due) |>
  mutate(exclusion = case_when(
    # Have an exclusion with a start date between last screen (plus one so that 
    # exclusions from that screen are still counted) and due date of next 
    # screen minus one to include those excluded at their follow-up
    between(date_start, date_screen_surv+1, date_next_screen_due-1) ~ 1,
    # Have an exclusion which started after date next screen due plus four weeks
    date_start >= date_next_screen_due + 4 * 7 ~ 0,
    # Have a screen which has not yet ended, e.g. excluded before
    # last appointment(not sure if this is possible)
    is.na(date_end) ~ 1,
    # Have an exclusion which ended between last screen and next screen due date
    between(date_end, date_screen_surv, date_next_screen_due + 4 * 7) ~ 1,
    # Keep everyone else
    TRUE ~ 0)) |>
  filter(exclusion == 1) |>
  select(-date_next_screen_due)

### 4.4: Create quarterly surveillance file and save ----

quarterly_surveillance_w_excl <- quarterly_surveillance_cohort |>
  left_join(quarterly_surveillance_f_up, by = c("upi", "date_screen_surv")) |>
  mutate(met_kpi_1_4b = replace_na(met_kpi_1_4b, 0)) |>
  # Join creates duplicates, but they are excluded anyway so doesn't matter?
  left_join(quarterly_exclusions_list, by = c("upi", "date_screen_surv")) %>%
  filter(is.na(exclusion))

# Check the minimum number of days between appointments, to make sure there are
# none very close together, arbitrarily using a cut-off of 30 days
check_interval <- quarterly_surveillance_w_excl %>%
  select(upi, date_screen_surv) |>
  arrange(upi, date_screen_surv) |>
  group_by(upi) |>
  mutate(count = row_number()) |>
  ungroup() |>
  pivot_wider(names_from = count,
              names_prefix = "date_screen_surv_",
              values_from = date_screen_surv) %>%
  mutate(interval_1_2 = date_screen_surv_2 - date_screen_surv_1,
         interval_2_3 = date_screen_surv_3 - date_screen_surv_2,
         interval_3_4 = date_screen_surv_4 - date_screen_surv_3,
         interval_4_5 = date_screen_surv_5 - date_screen_surv_4) |>
  summarise(interval_1_2 = min(interval_1_2, na.rm = TRUE),
            interval_2_3 = min(interval_2_3, na.rm = TRUE),
            interval_3_4 = min(interval_3_4, na.rm = TRUE),
            interval_4_5 = min(interval_4_5, na.rm = TRUE))

check_interval
# 36 days is minimum (all should be >30)

# # Save quarterly surveillance file (only if needed for checking)
# saveRDS(quarterly_surveillance_w_excl, paste0(temp_path, 
#                                               "/2_5_kpi_1_4b_quarterly.rds"))

rm(quarterly_exclusions_list, quarterly_surveillance_cohort,
   quarterly_surveillance_f_up, check_interval)


# ### Optional Step 4z: Create output file of CHIs for checking methodology ----
# tayside_fife_annual <- annual_surveillance_w_excl |>
#   filter(hbres %in% c("Tayside", "Fife")) |>
#   select(
#     upi, hbres, date_screen_surv, met_kpi_1_4a
#   ) |>
#   arrange(upi, date_screen_surv) |>
#   distinct(upi, .keep_all = TRUE)
# 
# tayside_fife_quarterly <- quarterly_surveillance_w_excl |>
#   filter(hbres %in% c("Tayside", "Fife")) |>
#   select(
#     upi, hbres, date_screen_surv, met_kpi_1_4b
#   ) |> arrange(upi, date_screen_surv) |>
#   distinct(upi, .keep_all = TRUE)
# 
# write_csv(tayside_fife_annual, 
#           paste0(temp_path, "/tayside_fife_annual_for_checking.csv"))
# 
# write_csv(tayside_fife_quarterly, 
#           paste0(temp_path, "/tayside_fife_quarterly_for_checking.csv"))
# 
# rm(tayside_fife_annual, tayside_fife_quarterly)

## Step 5: Supplementary table 6 ----
## Count of number of men with a surveillance screen in the year of interest

sup_tab_6_cohort <- aaa_extract |> 
  # Select those recommended for surveillance at next appointment, need the 
  # followup_recom variable as this is the only one that says whether it is 
  # quarterly or annual surveillance
  filter(!is.na(financial_year), followup_recom %in% c("01", "02")) |>
  select(upi, date_screen_last = date_screen, followup_recom) |>
  # Link screened cohort to get those who attended the next appointment
  inner_join(screened_cohort, by = "upi") |>
  rename(date_screen = date_next_screen) |>
  # Full cohort so remove those screening dates prior to the screening with
  # the follow-up recommendation
  filter(date_screen > date_screen_last) |>
  mutate(
    # To remove the multiple matched UPIs between the dataset, we want to keep
    # the subsequent screening attendance to the follow-up recommendation.
    # To do this, first calculate the interval between follow-up recommendation
    # and date of screening attendance
    interval = date_screen - date_screen_last,
    fy_screen = extract_fin_year(date_screen),
    surveillance_interval = case_when(followup_recom == "01" ~ "quarterly",
                                      followup_recom == "02" ~ "annual",
                                      TRUE ~ "error")) |>
  filter(fy_screen %in% kpi_report_years) |>
  # For each instance of CHI, date of screen, and surveillance interval,
  # Find the lowest interval to identify and filter on the next appointment
  group_by(upi, date_screen) |>
  mutate(min_interval = min(interval)) |>
  ungroup() |>
  filter(interval == min_interval) |>
  distinct(upi, fy_screen, surveillance_interval, .keep_all = TRUE) |>
  select(upi, hbres, fy_screen, surveillance_interval)
# GC NOTE - there are some very long intervals, leaving in at the moment as
# they are being screened, and with AAA there are no repeat screenings other
# than as part of surveillance. May be one to revisit

### Next section taken directly from previous script 
### 6_2_Supplementary_Surveillance

## Step 6: opt-out/non-response surveillance exclusions ----

# filter if excluded from surveillance due to dna ('08') or has opted out ('02')
# include open exclusions only (i.e., exclusions with no end date)
# create variable for financial year based on date_start
dna_excluded_surveillance <- aaa_exclusions %>% 
  filter(pat_inelig %in% c("08", "02") &
           is.na(date_end)) %>% 
  mutate(financial_year = extract_fin_year(as.Date(date_start)))
# 85 rows 2022/09
# 94 rows 2023/03
# 97 rows 2023/09
# 100 rows 2024/03

## Step 7: Create KPI output ----
## KPI 1.4a
kpi_1_4a <- annual_surveillance_w_excl %>% 
  group_by(fy_due, hbres) %>% 
  summarise(cohort_n = n(), met_kpi_1_4a_n = sum(met_kpi_1_4a)) %>% 
  group_modify(~ adorn_totals(.x, where = "row", name = "Scotland")) %>% 
  ungroup() %>% 
  mutate(met_kpi_1_4a_p = round_half_up(met_kpi_1_4a_n * 100 / cohort_n, 1),
         kpi = "KPI 1.4a") |>
  pivot_longer(cols = cohort_n:met_kpi_1_4a_p,
               names_to = "group",
               values_to = "value") |>
  mutate(simd = NA,
         hbres = forcats::fct_relevel(hbres, hb_list)) |> 
  arrange(fy_due, hbres) |> 
  select(hbres,
         kpi,
         simd,
         fin_year = fy_due,
         group,
         value)
  
## KPI 1.4b
kpi_1_4b <- quarterly_surveillance_w_excl %>% 
  group_by(fy_due, hbres) %>% 
  summarise(cohort_n = n(), met_kpi_1_4b_n = sum(met_kpi_1_4b)) %>% 
  group_modify(~ adorn_totals(.x, where = "row", name = "Scotland")) %>% 
  ungroup() %>% 
  mutate(met_kpi_1_4b_p = round_half_up(met_kpi_1_4b_n * 100 / cohort_n, 1),
         kpi = "KPI 1.4b") |>
  pivot_longer(cols = cohort_n:met_kpi_1_4b_p,
               names_to = "group",
               values_to = "value") |>
  mutate(simd = NA,
         hbres = forcats::fct_relevel(hbres, hb_list)) |> 
  arrange(fy_due, hbres) |> 
  select(hbres,
         kpi,
         simd,
         fin_year = fy_due,
         group,
         value)

kpi_1_4 <- bind_rows(kpi_1_4a, kpi_1_4b)

## Table 6
sup_tab_6_hb <- sup_tab_6_cohort |>
  group_by(fy_screen, hbres, surveillance_interval) |>
  summarise(tested_n = n()) |>
  ungroup()

sup_tab_6_scotland <- sup_tab_6_hb |>
  group_by(fy_screen, surveillance_interval) |>
  summarise(tested_n = sum(tested_n)) |>
  ungroup() |>
  mutate(hbres = "Scotland")

sup_tab_6 <- bind_rows(sup_tab_6_hb, sup_tab_6_scotland) |>
  pivot_longer(tested_n,
               names_to = "group",
               values_to = "value") |>
  mutate(kpi = "Table 6") |>
  select(hbres, kpi, fin_year = fy_screen, surveillance_interval, group, value) |> 
  mutate(surveillance_interval = 
           forcats::fct_relevel(surveillance_interval, 
                                c("quarterly", "annual"))) |> 
  arrange(fin_year, surveillance_interval)

sup_tab_6 <- hb_tibble |> left_join(sup_tab_6, by = "hbres") 

## DNA exclusions
# summarise by pat_inelig and financial_year
# reformat the table
dna_excluded_table <- dna_excluded_surveillance %>%
  group_by(pat_inelig, financial_year) %>% 
  summarise(count = n()) %>% 
  mutate(pat_inelig = case_when(pat_inelig == "02" ~ "Opted Out Surveillance",
                                pat_inelig == "08" ~ "Non Responder Surveillance"),
         kpi = "DNA Exclusions") |>
  arrange(financial_year) |>
  select(kpi, fin_year = financial_year, pat_inelig, count)


## Write to temp
saveRDS(sup_tab_6, paste0(temp_path, "/2_2_Table_6_", yymm, ".rds"))
saveRDS(dna_excluded_table, paste0(temp_path, "/2_3_dna_exclusions_", 
                                   yymm, ".rds"))

# Read in file created in previous script (2_2_kpi_1_1-3_uptake_coverage.R)
kpi_1 <- read_rds(paste0(temp_path, "/2_1_invite_attend_", yymm, ".rds"))

table(kpi_1$kpi, kpi_1$fin_year)
table(kpi_1_4$kpi, kpi_1_4$fin_year)
# Check that the data for kpi_report_years 1&2 matches data already stored in the block

# keep only new records for most recent complete year
kpi_1_4 <- kpi_1_4 |>
  filter(fin_year == kpi_report_years[3])

# add to summary already created (includes most recent year's kpi 1.1-1.3)
report_db <- bind_rows(kpi_1, kpi_1_4)

table(report_db$kpi, report_db$fin_year)

## Write out new invite_attend file
write_rds(report_db, paste0(temp_path, "/2_1_invite_attend_", yymm, ".rds"))


# call in historical db to run next funtion
hist_db <- read_rds(paste0(hist_path,"/aaa_kpi_historical_theme2.rds"))

table(hist_db$kpi, hist_db$fin_year)

# Save KPI 1.4 a/b to theme 2 data block
history_building(kpi_1_4, season)


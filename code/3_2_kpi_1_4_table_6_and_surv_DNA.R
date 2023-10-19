#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# KPI_1_4.R
# Gavin Clark
# September 2023
# Create the outputs for KPI 1.4 a, b, surveillance table 6, and 
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

# GC - issue - add to housekeeping script
prev_year <- "2022-04-01"

rm(list = ls())
gc()


source(here::here("code/0_housekeeping.R"))

# hbres_list
template <- tibble(fy_due = financial_year_due,
                   hbres = c("Scotland","Ayrshire & Arran","Borders",
                             "Dumfries & Galloway", "Fife", "Forth Valley", 
                             "Grampian", "Greater Glasgow & Clyde", "Highland", 
                             "Lanarkshire", "Lothian", "Orkney",
                             "Shetland", "Tayside","Western Isles"))

## Step 2: Read in, check, and format files ----

# AAA extract
aaa_extract <- readRDS(extract_path)

# AAA exclusions
aaa_exclusions <- readRDS(exclusions_path)

### 2.2: Check Exclusions and filter ----
# check number of records
aaa_exclusions %>% nrow()
# 98,960  rows 2020/09
# 106,417 rows 2021/03
# 114,781 rows 2021/09
# 123,791 rows 2022/03
# 133,707 rows 2022/09
# 143,256 rows 2023/03
# 153,558 rows 2023/09

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
         fin_month = case_when(month %in% c('04','05','06','07','08','09',
                                            '10','11','12') ~ as.numeric(month)-3,
                               month == '03' ~ 12,
                               month == '02' ~ 11,
                               month == '01' ~ 10)) %>%
  select(-month) %>% 
  select(-c(chi, dob:practice_name, ca2019:location_code, apl_measure,
            apt_measure, date_referral:date_seen_outpatient, 
            first_outcome:audit_batch_outcome )) %>% # eligibility_period:dob_eligibility needed?
  arrange(upi, date_screen) %>% 
  glimpse()
# 417,692 rows 2020/09
# 440,014 rows 2021/03
# 465,782 rows 2021/09
# 493,121 rows 2022/03
# 523,774 rows 2022/09
# 551,027 rows 2023/03
# 579,917 rows 2023/09

### 2.4: Create cohort of all of the screenings ----
screened_cohort <- aaa_extract %>% 
  # Can remove those screened more than a year prior to the period of interest
  # GC - issue - add to housekeeping script
  filter(date_screen >= as.Date(cut_off_12m)) |>
  mutate(screen_flag = ifelse(screen_result %in% c("01","02","04") &
                                (screen_type %in% c("02","04")), 1, 0)) %>%
  filter(screen_flag == 1) |>
  # These will be the attendance at the follow-up screening appointment
  # hence renaming as date_next_screen
  rename(date_next_screen = date_screen) %>%
  select(upi, date_next_screen, next_screen_result = screen_result)

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
  filter(fy_due == financial_year_due
         ) %>%
  # This is the appointment where the recommendation was surveillance
  # so renaming to make this clear
  rename(date_screen_surv = date_screen) 

## Check for duplicates, check the dates of screening to see how far apart
check_dups <- annual_surveillance_cohort %>%
  group_by(upi) %>%
  mutate(
    n = n()
  ) %>%
  ungroup() %>%
  filter(n > 1)
# 14, all have an appointment within six months, believe these should
# be dropped

# Remove duplicates, keeping the earlier screening appointment (arbitrarily)
annual_surveillance_cohort %<>%
  arrange(upi, date_screen_surv) %>%
  distinct(upi, .keep_all = TRUE)
# Removed 7

### 3.2: Identify those with follow-up within appropriate timeframe ----

# Create list of those with a follow-up appointment within the standard
# by merging 12-month surveillance cohort with screened cohort
annual_surveillance_f_up <- annual_surveillance_cohort |>
  inner_join(screened_cohort, by = "upi") |>
  # Have had a screening appointment after the surveillance appointment
  filter(date_next_screen > date_screen_surv) %>%
  mutate(
    # Person has a screen due in the period of interest
    # and they have been screened within 365 days plus 6 weeks
    met_kpi_1_4a = if_else(
      between(date_next_screen, 
              date_screen_surv,
              date_next_screen_due + 6 * 7), 1, 0)) %>%
  # Create list of compliant CHIs for linking to cohort
  group_by(upi) |>
  summarise(
    met_kpi_1_4a = max(met_kpi_1_4a)
  ) %>%
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
    # Have a screen which has not yet ended, e.g. excluded before
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
  left_join(annual_surveillance_f_up, by = "upi") |>
  mutate(met_kpi_1_4a = replace_na(met_kpi_1_4a, 0)) |>
  # Join creates duplicates, but they are excluded anyway so doesn't matter?
  left_join(annual_exclusions, by = c("upi", "date_screen_surv")) %>%
  filter(is.na(exclusion))

# Check for duplicates  
check_dups <- annual_surveillance_w_excl %>%
  group_by(upi) %>%
  mutate(
    n = n()
  ) %>%
  ungroup() %>%
  filter(n > 1)
# No duplicates

# Save annual surveillance cohort
saveRDS(annual_surveillance_w_excl, 
        "temp/annual_surveillance_w_excl.rds")

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
  filter(fy_due == financial_year_due
  ) %>%
  rename(date_screen_surv = date_screen) |>
  select(upi, hbres, fy_due, date_screen_surv, date_next_screen_due)

## Duplicates are expected in this cohort

### 4.2: Identify those with follow-up within appropriate timeframe ----

# Create list of those with a follow-up appointment within the standard
quarterly_surveillance_f_up <- quarterly_surveillance_cohort |>
  inner_join(screened_cohort, by = "upi") |>
  # Have had a screening appointment after the surveillance appointment
  filter(date_next_screen > date_screen_surv) %>%
  mutate(
    # Person has a screen due in the period of interest
    met_kpi_1_4b = if_else(
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
  mutate(
    count = row_number()
  ) |>
  ungroup() |>
  pivot_wider(
    names_from = count,
    names_prefix = "date_screen_surv_",
    values_from = date_screen_surv) %>%
  mutate(
    interval_1_2 = date_screen_surv_2 - date_screen_surv_1,
    interval_2_3 = date_screen_surv_3 - date_screen_surv_2,
    interval_3_4 = date_screen_surv_4 - date_screen_surv_3,
    interval_4_5 = date_screen_surv_5 - date_screen_surv_4,
  ) |>
  summarise(
    interval_1_2 = min(interval_1_2, na.rm = TRUE),
    interval_2_3 = min(interval_2_3, na.rm = TRUE),
    interval_3_4 = min(interval_3_4, na.rm = TRUE),
    interval_4_5 = min(interval_4_5, na.rm = TRUE)
  ) 

# Save quarterly surveillance file
saveRDS(quarterly_surveillance_w_excl, 
          "temp/quarterly_surveillance_w_excl.rds")

## Optional Step 4z: Create output file of CHIs for checking of methodology ----

tayside_fife_annual <- annual_surveillance_w_excl |>
  filter(hbres %in% c("Tayside", "Fife")) |>
  select(
    upi, hbres, date_screen_surv, met_kpi_1_4a
  ) |>
  arrange(upi, date_screen_surv) |>
  distinct(upi, .keep_all = TRUE)

tayside_fife_quarterly <- quarterly_surveillance_w_excl |>
  filter(hbres %in% c("Tayside", "Fife")) |>
  select(
    upi, hbres, date_screen_surv, met_kpi_1_4b
  ) |> arrange(upi, date_screen_surv) |>
  distinct(upi, .keep_all = TRUE)

write_csv(tayside_fife_annual, "temp/tayside_fife_annual_for_checking.csv")

write_csv(tayside_fife_quarterly, "temp/tayside_fife_quarterly_for_checking.csv")

## Step 5: Supplementary table 6 ----

sup_tab_6_cohort <- annual_surveillance_cohort <- aaa_extract |> 
  filter(!is.na(financial_year),followup_recom %in% c("01", "02")) |>
  mutate(date_next_screen_due = case_when(
    followup_recom == "01" ~ date_screen + 91,
    followup_recom == "02" ~ date_screen + 365)) |>
  # GC TO DO - add to housekeeping, going with three months prior to start of 
  # financial year of interest, so that all screenings in the FY of interest are
  # captured. Ultimately selection is on the basis of date screened rather than
  # screen due.
  filter(date_next_screen_due > as.Date("2022-12-31")) |>
  select(upi, hbres, date_next_screen_due, followup_recom) |>
  # Link screened cohort to see who attended
  inner_join(screened_cohort, by = "upi") |>
  rename(date_screen = date_next_screen) |>
  mutate(fy_screen = extract_fin_year(date_screen),
         surveillance_interval = case_when(
           followup_recom == "01" ~ "quarterly",
           followup_recom == "02" ~ "annual",
           TRUE ~ "error"
         )) |>
  filter(fy_screen == financial_year_due) |>
  distinct(upi, surveillance_interval, .keep_all = TRUE)
  
sup_tab_6_cohort |> count(surveillance_interval)

sup_tab_6 <- sup_tab_6_cohort |>
  group_by(hbres, surveillance_interval) |>
  summarise(
    n = n()
  ) |>
  ungroup() |>
  pivot_wider(
    names_from = surveillance_interval,
    values_from = n
  )  |>
  adorn_totals("row") |>
  mutate(hbres = if_else(
    hbres == "Total", "Scotland", hbres),
    hbres = forcats::fct_relevel(
      hbres,
      "Scotland")) |>
  arrange(hbres)

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

# summarise by pat_inelig and financial_year
# reformat the table
# rename row names
dna_excluded_surveillance_table <- dna_excluded_surveillance %>%
  group_by(pat_inelig, financial_year) %>% 
  summarise(count = n()) %>% 
  arrange(financial_year) %>% 
  pivot_wider(names_from = financial_year,
              values_from = count) %>% 
  mutate(pat_inelig = case_when(pat_inelig == "08" ~ "Non Responder Surveillance",
                                pat_inelig == "02" ~ "Opted Out Surveillance")) %>% 
  rename(`Exclusion Type` = pat_inelig)


## Step 5: Create KPI output ----

kpi_1_4a <- annual_surveillance_w_excl %>% 
    group_by(fy_due, hbres) %>% 
    summarise(cohort_ac = n(), met_kpi_1_4a = sum(met_kpi_1_4a)) %>% 
    group_modify(~ adorn_totals(.x, where = "row", name = "Scotland")) %>% 
    ungroup() %>% 
    mutate(pc = met_kpi_1_4a * 100 / cohort_ac) %>%
    mutate(pc = round_half_up(pc, 1))
  
  #reorder rows
kpi_1_4a <- template %>% left_join(kpi_1_4a,  
                                     by = c("fy_due", "hbres" = "hbres"))
  
  View(kpi_1_4a)
  
  
kpi_1_4b <- quarterly_surveillance_w_excl %>% 
  group_by(fy_due, hbres) %>% 
  summarise(cohort_ac = n(), met_kpi_1_4b = sum(met_kpi_1_4b)) %>% 
  group_modify(~ adorn_totals(.x, where = "row", name = "Scotland")) %>% 
  ungroup() %>% 
  mutate(pc = met_kpi_1_4b * 100 / cohort_ac) %>%
  mutate(pc = round_half_up(pc, 1))

#reorder rows
kpi_1_4b <- template %>% left_join(kpi_1_4b,  
                                   by = c("fy_due", "hbres" = "hbres"))

View(kpi_1_4b)

# Write to csv
write_csv(kpi_1_4a, "temp/kpi_1_4a.csv")
write_csv(kpi_1_4b, "temp/kpi_1_4b.csv")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# KPI_1.4 (Surveillance_Assess_Recovery).R
# Eibhlin O'Sullivan
# Oct 2022
# Define housekeeping variables used by subsequent scripts
# Written/run on R Studio Server
# R version 3.6.1
# Revised/Run on Posit PWB (R version 4.1.2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Update dates in 0_housekeeping.R before running this script.

# There are three scripts for kpi 1.4A to be run together:
# 1) 3_2_kpi_1_4_surveillance.R 
# 2) 4_2_kpi_1_4_surveillance_assess_recovery.R - This may be removed as it is related to COVID Recovery - This script
# 3) 5_2_kpi_1_4_join_tables.R - Joins the output of 1 and 2 into the report format and outputs a CSV

### 1 - Housekeeping ----

# install.packages("pacman")

pacman::p_load(
  dplyr,
  magrittr,
  phsmethods,
  lubridate,
  janitor
)

rm(list = ls())
gc()


source(here::here("code/0_housekeeping.R"))

rm(gpd_lookups, cutoff_date,
   year1_start, year1_end, year2_start, year2_end)

# hbres_list
template <- tibble(fy_due = financial_year_due,
                   hbres = c("Scotland","Ayrshire & Arran","Borders",
                             "Dumfries & Galloway", "Fife", "Forth Valley", 
                             "Grampian", "Greater Glasgow & Clyde", "Highland", 
                             "Lanarkshire", "Lothian", "Orkney",
                             "Shetland", "Tayside","Western Isles"))

### 2 - Read in Files ----
# AAA extract path
aaa_extract <- readRDS(extract_path)

# AAA exclusions path
aaa_exclusions <- readRDS(exclusions_path)


### 3 - Format Tables ----
## 3.1 - Check Exclusions and filter ----

# check number of screened records
aaa_exclusions %>% nrow()
# 133,707 rows 2022/09
# 143,256 rows 2023/03

# add financial_year, quarter and month
# filter out financial_year is na
aaa_exclusions%<>%
  mutate(financial_year = extract_fin_year(as.Date(date_start)),# possibly can be removed once aaa_extract is updted
         month = format(as.Date(date_start, format = "%Y-%m-%d"),"%m"),
         financial_quarter = case_when(month %in% c('04','05','06') ~ 1,
                                       month %in% c('07','08','09') ~ 2,
                                       month %in% c('10','11','12') ~ 3,
                                       month %in% c('01','02','03') ~ 4),# possibly can be removed once aaa_extract is updated
         fin_month = case_when(month %in% c('04','05','06','07','08','09','10','11','12') ~ as.numeric(month)-3,
                               month == '03' ~ 12,
                               month == '02' ~ 11,
                               month == '01' ~ 10)) %>%
  select(-month) %>% 
  glimpse()

## 3.2 - Check and filter extract to create cohort base ----

# recode all NA largest measurements to 0
# create fin_month fields
# select relevant columns
aaa_extract %<>% 
  # select(upi, date_screen, date_offer_sent, followup_recom, screen_result,
  #        largest_measure, hbres, pat_elig, att_dna, screen_type, screen_exep,
  #        result_verified, result_outcome) %>%
  mutate(largest_measure = replace(largest_measure,is.na(largest_measure),0)) %>% 
  mutate(month = format(as.Date(date_screen, format = "%Y-%m-%d"),"%m"),
         fin_month = case_when(month %in% c('04','05','06','07','08','09','10',
                                            '11','12') ~ as.numeric(month)-3,
                               month == '03' ~ 12,
                               month == '02' ~ 11,
                               month == '01' ~ 10)) %>%
  select(-month) %>% 
  select(-c(chi, dob:practice_name, ca2019:location_code, apl_measure,
            apt_measure, date_referral:audit_batch_outcome)) %>% 
  glimpse()

# 485,728 rows sept 2022
# 551,027 rows 2023/03

# filter if screen result is positive, negative or non-visualisation
screened_cohort <- aaa_extract %>% 
  mutate(screen_flag = ifelse(screen_result %in% c("01","02","04") &
                                (screen_type %in% c("02","04")), 1 ,0)) 

# check records to be removed
screened_cohort %>% 
  group_by(screen_flag) %>% 
  summarise(screen_n = n())
# 18,647 1's, 505,127 0's sept 2022
# 20,020 1's, 531,007 0's 2023/03

# remove records
screened_cohort %<>%
  filter(screen_flag ==1) %>% 
  glimpse()

# re-check records removed
screened_cohort %>% 
  group_by(screen_flag) %>% 
  summarise(screen_n = n()) 

# remove unnecessary columns
screened_cohort%<>% 
  select(-screen_flag) %>% 
  glimpse()

# add sc for annual cohort to identify more easily when joining
colnames(screened_cohort) <- paste(colnames(screened_cohort),"sc",sep="_")

# Check all records with screen_result 01 or 03 (positive or tech failure): 
# must have a value for followup_recom
# Previous records with this issue have aged out of the extract 
# but we should still check that this does not crop up.
View(aaa_extract %>% tabyl(screen_result, followup_recom))


### 4 - 12 Month Surveillance Uptake ----
# Remove any records with no valid financial year
# Create a list of all screening results with a recommendation to follow up 
# in 12 months (followup_recomm = 02)
# Keep latest screening date per month per upi
# Create a cohort flag
annual_surveillance_cohort <- aaa_extract %>% 
  filter(!is.na(financial_year)) %>% 
  filter(followup_recom == "02") %>% 
  filter(financial_year %in% c(prev_year,current_year)) %>% 
  filter(date_screen <= as.Date(cut_off_12m)) |>  
  mutate(cohort = 1)

# add ac for annual cohort to identify more easily when joining
colnames(annual_surveillance_cohort)<-paste(colnames(
  annual_surveillance_cohort),"ac",sep="_")

# Get follow up appointments join aaa extract to annual_surveillance_cohort
# Calculate difference between appointments in days
# Filter date screen is before c date screen
# Filter days between 0 and 408 days (1 year 6 weeks)
# Keep only first upi record per month
# filter for current financial year

follow_up_appointments <- screened_cohort %>% 
  left_join(annual_surveillance_cohort, by = c("upi_sc"="upi_ac")) %>%
  mutate(interval = difftime(date_screen_sc,date_screen_ac,units = "days")) %>%
  mutate(fin_year_ac = extract_fin_year((as.POSIXct(date_screen_ac)))) %>%
  filter(date_screen_sc > date_screen_ac & 
           interval >= 0 & 
           interval <= 2000 &
           !is.na(fin_month_ac)) %>% # 4119 rows
  arrange(upi_sc,financial_year_ac,fin_month_ac,desc(date_screen_sc)) %>%
  group_by(upi_sc,financial_year_ac,fin_month_ac) %>% 
  slice(n()) %>% 
  ungroup() %>% 
  rename(upi = upi_sc) %>% 
  filter(fin_year_ac == current_year)
  # 1301 rows 2022/09
  # 1185 rows 2023/03

# Check duplicates
View(follow_up_appointments %>% get_dupes()) 
View(follow_up_appointments %>% get_dupes(upi))

# Match exclusions to cohort
# calculate interval and only keep those within 408 days of the 
# surveillance cohort date
# remove duplicates
exclusions_appointments <- aaa_exclusions %>% 
  left_join(annual_surveillance_cohort, by = c("upi"="upi_ac")) %>% 
  mutate(fin_year_ac = extract_fin_year((as.POSIXct(date_screen_ac)))) %>%
  mutate(interval = difftime(date_start,date_screen_ac,units = "days")) %>% 
  filter(date_start >= date_screen_ac & 
           interval >= 0 & 
           interval <= 408 &
           !is.na(fin_month_ac))%>%
  arrange(upi,financial_year_ac,fin_month_ac,date_start) %>%
  group_by(upi,financial_year_ac,fin_month_ac) %>% 
  slice(n()) %>% 
  ungroup() %>% 
  filter(fin_year_ac == current_year)
# 106 rows 2022/09
# 122 rows 2023/03

# Combine follow up appointments and exclusions
combined_appointments <- follow_up_appointments %>% 
  bind_rows(exclusions_appointments) %>% 
  filter(financial_year_ac == current_year)


final_follow_ups <- annual_surveillance_cohort %>% 
  left_join(combined_appointments, by = c("upi_ac"="upi"))
# 1407 rows 2022/09
# 2840 rows 2023/03

View(final_follow_ups %>% get_dupes()) 
View(final_follow_ups %>% get_dupes(upi_ac))

rm(annual_surveillance_cohort, exclusions_appointments, 
   follow_up_appointments, combined_appointments)


# Remove unnecessary columns
# Create a flag for attended follow up appointments
# If there is an exclusion flag but the appointment was attended and has a 
# date_screen recode exclusion to zero
# Filter for only those records with no exclusion
# select all records where screen_n_ac = 1 to select only records from the 
# original cohort
# Add expected follow-up year
final_follow_ups <- final_follow_ups %>%
  mutate(follow_up_screen_flag = (case_when(!is.na(date_screen_sc)~ 1,
                                            is.na(date_screen_sc)~0))) %>% 
  arrange(upi_ac) %>%
  mutate(attend = case_when(!is.na(date_screen_sc) ~ 1,
                            is.na(date_screen_sc) ~ 0)) %>%
  
  mutate(exclusion_flag = ifelse((pat_inelig %in% c('04','06','11','12','13',
                                                    '14','15','16','17','18',
                                                    '19','21', '22', '25', '26') &
                                    is.na(date_end))|
                                   pat_inelig %in% c('03','05'),1,0)) %>% 
  mutate(exclusion_flag_final = ifelse(attend ==1,0,exclusion_flag)) %>% 
  filter(exclusion_flag_final == 0) %>%
  mutate(fy_due = extract_fin_year((as.POSIXct(date_screen_ac.x)+years(1)))) %>% 
  filter(fy_due == financial_year_due) %>% 
  arrange(upi_ac, financial_year_ac.x, fin_month_ac.x, date_start) %>%
  group_by(upi_ac, financial_year_ac.x, fin_month_ac.x) %>% 
  slice(n()) %>% 
  ungroup() %>% 
  select(-ends_with(".y"))

# Rename columns
colnames(final_follow_ups) <- gsub(".x", "", colnames(final_follow_ups))

final_follow_ups %<>%
  # fix col names distorted by gsub above
  rename(exclusion_flag = clusion_flag,
         exclusion_flag_final = clusion_flag_final)


# Check duplicates
View(final_follow_ups %>% filter(financial_year_ac == current_year) %>%
       distinct(upi_ac, financial_year_ac, fin_month_ac))

View(final_follow_ups %>% tabyl(exclusion_flag))
View(final_follow_ups %>% get_dupes()) 
View(final_follow_ups %>% get_dupes(upi_ac))


### 5 Create KPI 1.4a table -----
# KPI 1.4 statistics are collated based on hb of residence where 
# follow up screen took place rather than the hbres where trigger appointment
# occurred. This means if a man had trigger screen in Highland and then 
# follow-up appointment 12m later in Grampian, his whole record will be 
# counted under Grampian.
# *If the man wasn't tested then assume the man is still in his health board 
# where the trigger screening occurred.

# Allocate correct hb in case an individual has moved between screenings
# Create variable for Scotland
final_follow_ups %<>% 
  mutate(hbres_final = (case_when(!is.na(date_screen_sc) ~ hbres_sc,
                                  is.na(date_screen_sc) ~ hbres_ac)))

# number of cohort and attendance by hbres
kpi_1.4a_assess <- final_follow_ups %>% 
  group_by(fy_due,hbres_final) %>% 
  summarise(sum(cohort_ac),sum(attend)) %>% 
  group_modify(~ adorn_totals(.x, where = "row", name = "Scotland")) %>% 
  ungroup() %>%
  mutate(pc = `sum(attend)` * 100 / `sum(cohort_ac)`) %>%
  mutate(pc = round_half_up(pc, 1))

# reorder rows
kpi_1.4a_assess <- template %>% 
  left_join(kpi_1.4a_assess, by = c("fy_due", "hbres" = "hbres_final"))


View(kpi_1.4a_assess)

################################################################################
### 6  Create 3M Surveillance figures ----

quarterly_surveillance_cohort <- aaa_extract %>% 
  filter(!is.na(financial_year)) %>% 
  filter(followup_recom == "01") %>%
  filter(date_screen <= as.Date(cut_off_3m)) |>
  mutate(cohort = 1) %>% 
  filter(date_screen < last_date)

# add ac for annual cohort to identify more easily when joining
colnames(quarterly_surveillance_cohort)<-paste(colnames(
  quarterly_surveillance_cohort),"qc",sep="_")

# join the follow up records to the screened cohort.
# calculate the interval between screening cohort day and follow up date
# filter for records where the follow up date is within + 120 days above date 
# screened.
# keep one record per month
quarterly_follow_up_appointments <- screened_cohort %>% 
  left_join(quarterly_surveillance_cohort, by = c("upi_sc"="upi_qc")) %>%
  mutate(interval = difftime(date_screen_sc, date_screen_qc, units = "days")) %>% 
  filter(date_screen_sc > date_screen_qc & 
           interval >= 0 & 
           interval <= 1999 &
           !is.na(fin_month_qc)) %>%
  arrange(upi_sc, financial_year_qc, fin_month_qc, desc(date_screen_sc)) %>%
  group_by(upi_sc, financial_year_qc, fin_month_qc) %>% 
  slice(n()) %>% 
  ungroup() %>% 
  rename(upi = upi_sc) %>% 
  filter(fy_quarter_qc %in% c(financial_quarters))


# Check duplicates
View(quarterly_follow_up_appointments %>% get_dupes()) 
View(quarterly_follow_up_appointments %>% get_dupes(upi))

# Match exclusions to cohort
# calculate interval and only keep those within 408 days of the surveillance 
# cohort date
# remove duplicates
quarterly_exclusions_appointments <- aaa_exclusions %>% 
  left_join(quarterly_surveillance_cohort, by = c("upi"="upi_qc")) %>% 
  mutate(interval = difftime(date_start,date_screen_qc,units = "days")) %>% 
  filter(date_start >= date_screen_qc & 
           interval >= 0 & 
           interval <= 120 &
           !is.na(fin_month_qc))%>%
  arrange(upi,financial_year_qc,fin_month_qc,date_start) %>%
  group_by(upi,financial_year_qc,fin_month_qc) %>% 
  slice(n()) %>% 
  ungroup() %>% 
  filter(fy_quarter_qc %in% c(financial_quarters))


# Check duplicates
View(quarterly_exclusions_appointments %>% get_dupes()) 
View(quarterly_exclusions_appointments %>% get_dupes(upi))

# Combine follow up appointments and exclusions and the surveillance cohort
# remove details for any follow up that matches trigger date but keep the record

quarterly_combined_appointments <- quarterly_surveillance_cohort %>% 
  left_join(quarterly_follow_up_appointments, 
            by = c("upi_qc"="upi", "date_screen_qc" = "date_screen_qc")) %>% 
  left_join(quarterly_exclusions_appointments, 
            by = c("upi_qc"="upi", "date_screen_qc" = "date_screen_qc")) %>% 
  mutate(across(c(financial_year_sc:fin_month), 
                ~ replace(., date_screen_qc == date_screen_sc, NA)))

View(quarterly_combined_appointments %>% get_dupes()) 
View(quarterly_combined_appointments %>% get_dupes(upi_qc))

# Remove unnecessary columns
# Create a flag for attended follow up appointments
# If there is an exclusion flag but the appointment was attended and has a date_screen recode exclusion to zero
# Filter for only those records with no exclusion
# select all records where screen_n_ac = 1 to select only records from the original cohort
# Add and filter for expected follow-up year
# remove duplicates
quarterly_final_follow_ups <- quarterly_combined_appointments %>%
  mutate(follow_up_screen_flag = (case_when(!is.na(date_screen_sc) ~ 1,
                                            is.na(date_screen_sc) ~ 0))) %>% 
  arrange(upi_qc) %>%
  mutate(attend = case_when(!is.na(date_screen_sc) ~ 1,
                            is.na(date_screen_sc) ~ 0)) %>%
  
  mutate(exclusion_flag = ifelse((pat_inelig %in% c('04','06','11','12','13',
                                                    '14','15','16','17','18',
                                                    '19','21', '22', '25', '26') &
                                    is.na(date_end))|
                                   pat_inelig %in% c('03','05'), 1, 0)) %>% 
  mutate(exclusion_flag_final = ifelse(attend == 1, 0, exclusion_flag)) %>% 
  filter(exclusion_flag_final == 0) %>%
  mutate(fy_due = extract_fin_year((as.POSIXct(date_screen_qc) %m+% months(3)))) %>% 
  filter(fy_due == financial_year_due)  %>% 
  rename(upi = upi_qc) |>
  select(-ends_with(".y")) %>%
  select(-ends_with("_qc"))

# Rename columns
colnames(quarterly_final_follow_ups) <- gsub(".x","",
                                             colnames(quarterly_final_follow_ups))

quarterly_final_follow_ups %<>% 
  # fix col names distorted by gsub above
  rename(exclusion_flag = clusion_flag,
         exclusion_flag_final = clusion_flag_final)

quarterly_final_follow_ups %<>% 
  mutate(hbres_final = (case_when(!is.na(date_screen_sc) ~ hbres_sc,
                                  is.na(date_screen_sc) ~ hbres_qc)))


View(quarterly_final_follow_ups %>% tabyl(exclusion_flag))
View(quarterly_final_follow_ups %>% get_dupes()) 
View(quarterly_final_follow_ups %>% get_dupes(upi))

# number of cohort and attendance by hbres
kpi_1.4b_assess <- quarterly_final_follow_ups %>% 
  group_by(fy_due, hbres_final) %>% 
  summarise(sum(cohort_qc), sum(attend)) %>% 
  group_modify(~ adorn_totals(.x, where = "row", name = "Scotland")) %>% 
  ungroup() %>%
  mutate(pc = `sum(attend)` * 100 / `sum(cohort_qc)`) %>%
  mutate(pc = round_half_up(pc, 1))

#reorder rows and account for hb's with no values
kpi_1.4b_assess <- template %>% 
  left_join(kpi_1.4b_assess,  by = c("fy_due", "hbres" = "hbres_final"))

View(kpi_1.4b_assess)

# Write out kpi tables

saveRDS(kpi_1.4a_assess, paste0(temp_path, "/kpi_1_4a_assess.rds"))
saveRDS(kpi_1.4b_assess, paste0(temp_path, "/kpi_1_4b_assess.rds"))


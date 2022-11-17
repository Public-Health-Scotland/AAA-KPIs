#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# KPI_1.4 (Surveillance_Assess_Recovery).R
# Eibhlin O'Sullivan
# Oct 2022
# Define housekeeping variables used by subsequent scripts
# Written/run on R Studio Server
# R version 3.6.1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### 1 - Housekeeping ----

# install.packages("pacman")

pacman::p_load(
  dplyr,
  magrittr,
  phsmethods,
  lubridate,
  janitor
)

# Define dates 

month <- '202209' # this may not be needed
prev_year <- "2019/20"
current_year <- "2020/21"
current_year_start <- "2020-03-01"
next_year_start <- "2021-03-01"

# Define filepaths

aaa_extracts_path <- (paste0("/PHI_conf/AAA/Topics/Screening/extracts/202209/output/"))

# Functions

`%!in%` <- Negate(`%in%`)

### 2 - Read in Files ----

# AAA extract path
aaa_extract <- readRDS(paste0(aaa_extracts_path,"aaa_extract_202209.rds"))

# AAA exclusions path
aaa_exclusions <- readRDS(paste0(aaa_extracts_path,"aaa_exclusions_202209.rds"))

### 3 - Format Tables ----







## 3.1 - Check Exclusions and filter ----

# check number of screened records
aaa_exclusions %>% nrow()# 133707 rows

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
  #filter(!is.na(financial_year)) %>% 
  mutate(exclusion = 1) %>% 
  glimpse()

# check duplicates in aaa_exclusions
View(aaa_exclusions %>% get_dupes())
# 65 duplicate rows but these are not removed in SPSS so keeping for now

## 3.2 - Check and filter extract to create cohort base ----

# assign numerator to those screened
# recode all NA largest measurements to 0
# create fin_month fields
aaa_extract %<>% 
  # select(upi,date_screen,date_offer_sent,followup_recom,screen_result,largest_measure,
  #        hbres,pat_elig,att_dna,screen_type,screen_exep,result_verified,result_outcome) %>%
  mutate(largest_measure = replace(largest_measure,is.na(largest_measure),0)) %>% 
  mutate(month = format(as.Date(date_screen, format = "%Y-%m-%d"),"%m"),
         fin_month = case_when(month %in% c('04','05','06','07','08','09','10','11','12') ~ as.numeric(month)-3,
                               month == '03' ~ 12,
                               month == '02' ~ 11,
                               month == '01' ~ 10)) %>%
  select(-month) %>% 
  select(-c(date_referral:audit_batch_outcome )) %>% 
  glimpse()
# 485,728 rows sept 2022



# filter if screen result is positive or negative o non - visualisation
# FOR REFERENCE: originally numerator file
screened_cohort <- aaa_extract %>% 
  mutate(screen_flag = ifelse(screen_result %in% c("01","02","04") &
                                (screen_type %in% c("02","04")),1,0)) %>% 
  filter(screen_flag ==1)

# check number of screened
screened_cohort %>% filter(screen_flag == 1) %>% nrow()# 18,647 screened
screened_cohort %>% filter(screen_flag == 0) %>% nrow()
View(screened_cohort %>% get_dupes()) # 1,386 duplicate rows - these should be removed later 



# check finiancial years
# screened_cohort %>% group_by(financial_year) %>% 
#   summarise(n())
# 
# # remove pre-2012/13 and future records
# screened_cohort %<>%
#   filter(financial_year %in% c("2012/13","2013/14","2014/15",
#                          "2015/16","2016/17","2017/18",
#                          "2018/19","2019/20","2020/21",
#                          "2021/22","2022/23"))

# add sc for annual cohort to identify more easily when joining
colnames(screened_cohort)<-paste(colnames(screened_cohort),"sc",sep="_")

# 485,721 rows remaining

# Check all records with screen_result 01 or 03 they must have a value for followup_recom
View(aaa_extract %>% tabyl(screen_result,followup_recom))

### 4 - 12 Month Surveillance Uptake ----

# Create a list of all screening results with a recommendation to follow up in 12 months followup_recomm = 02
# Keep latest screening date per month per upi

### WIP COMMENT: Combined 12m_cohort month combined total
annual_surveillance_cohort <- aaa_extract %>% 
  filter(!is.na(financial_year)) %>% 
  filter(followup_recom == "02") %>% #12,059 rows
  
  # arrange(upi, financial_year,fin_month,date_screen) %>% 
  # group_by(upi, financial_year,fin_month) %>% 
  # slice(n()) %>% 
  # ungroup() %>% #12057
  
  filter(financial_year %in% c(prev_year,current_year)) %>% 
  mutate(cohort = 1) #%>% # 2826 rows - matched
#filter(financial_year == "2020/21") # 1,353 rows

# add ac for annual cohort to identify more easily when joining
colnames(annual_surveillance_cohort)<-paste(colnames(annual_surveillance_cohort),"ac",sep="_")

# Check duplicates
View(annual_surveillance_cohort %>% get_dupes()) 
View(annual_surveillance_cohort %>% get_dupes(upi))

# Get follow up appointments join aaa extract to annual_surveillance_cohort
# Calculate difference between appointments in days
# Filter date screen is before c date screen
# Filter days between 0 and 408 days (1 year 6 weeks)
# Keep only first upi record per month

#target 2,045 rows
follow_up_appointments <- screened_cohort %>% 
  left_join(annual_surveillance_cohort, by = c("upi_sc"="upi_ac")) %>%
  #select(-upi_sc) %>% 
  # select(financial_year.x:sex.x,pat_elig.x:simd2020v2_hb2019_quintile.x,screen_type.x:followup_recom.x,
  #        largest_measure.x:date_verified.x,screen_n.x,fin_month.x,screen_type.y:date_verified.y,financial_year) %>% 
  mutate(interval = difftime(date_screen_sc,date_screen_ac,units = "days")) %>% 
  filter(date_screen_sc > date_screen_ac & 
           interval >= 0 & 
           interval <= 2000 &
           !is.na(fin_month_ac))  %>% # 3050 rows
  arrange(upi_sc,financial_year_ac,fin_month_ac,date_screen_sc) %>%
  group_by(upi_sc,financial_year_ac,fin_month_ac) %>% 
  slice(n()) %>% 
  ungroup() %>% 
  rename(upi = upi_sc)# 1301 rows

#colnames(follow_up_appointments)<-paste(colnames(follow_up_appointments),"fua",sep="_")

# Check duplicates
View(follow_up_appointments %>% get_dupes()) 
View(follow_up_appointments %>% get_dupes(upi_ac))

# Match exlusions to cohort
# calculate interval and only keep those within 408 days of the surveillance cohort date
# remove duplicates
exclusions_appointments <- aaa_exclusions %>% 
  left_join(annual_surveillance_cohort, by = c("upi"="upi_ac")) %>% 
  mutate(interval = difftime(date_start,date_screen_ac,units = "days")) %>% 
  filter(date_start >= date_screen_ac & 
           interval >= 0 & 
           interval <= 408 &
           !is.na(fin_month_ac))%>%
  arrange(upi,financial_year_ac,fin_month_ac,desc(date_start)) %>%
  group_by(upi,financial_year_ac,fin_month_ac) %>% 
  slice(n()) %>% 
  ungroup()
# 106 rows

#colnames(exclusions_appointments)<-paste(colnames(exclusions_appointments),"ea",sep="_")


# Check duplicates
View(exclusions_appointments %>% get_dupes()) 
View(exclusions_appointments %>% get_dupes(upi_ac))

# Combine follow up appointments and exclusions
combined_appointments <- follow_up_appointments %>% 
  bind_rows(exclusions_appointments) %>% 
  filter(financial_year_ac == current_year)


final_follow_ups <- annual_surveillance_cohort %>% 
  left_join(combined_appointments, by = c("upi_ac"="upi"))

# final_follow_ups %<>% 
#   filter(financial_year_ac.x == current_year) %>% 
#   distinct(upi_ac)

# Combine follow up appointments and exclusions
# combined_appointments <- annual_surveillance_cohort %>%
#   left_join(follow_up_appointments, by = c("upi_ac"="upi_sc_fua")) %>% 
#   left_join(exclusions_appointments, by = c("upi_ac"="upi_ea"))%>% 
#   filter(financial_year_ac == current_year)

#1407 rows

View(combined_appointments %>% get_dupes()) 
View(combined_appointments %>% get_dupes(upi_ac))

# Remove unecessary columns
# Create a flag for attended follow up appointments
# If there is an exclusion flag but the appointment was attended and has a date_screen recode exclusion to zero
# Filter for only those records with no exclusion
# select all records where screen_n_ac = 1 to select only records from the original cohort
# Add expected follow-up year


#####FUTURE EDIT: These are arbitrarily sorted before deduplication * 
# this comes out with a comparable result to SPSS but should be clarified in future
final_follow_ups <- final_follow_ups %>%
  mutate(follow_up_screen_flag = (case_when(!is.na(date_screen_sc)~ 1,
                                            is.na(date_screen_sc)~0))) %>% 
  
  
  arrange(upi_ac) %>%
  mutate(attend = case_when(!is.na(date_screen_sc) ~ 1,
                            is.na(date_screen_sc) ~ 0)) %>%
  
  mutate(exclusion_flag = ifelse((pat_inelig %in% c('04','06','11','12','13','14','15','16','17','18','19','21', '22', '25', '26') &
                                    is.na(date_end))|
                                   pat_inelig %in% c('03','05'),1,0)) %>% 
  mutate(exclusion_flag_final = ifelse(attend ==1,0,exclusion_flag)) %>% 
  filter(exclusion_flag_final == 0) %>%
  mutate(fy_due = extract_fin_year((as.POSIXct(date_screen_ac.x)+years(1)))) %>% 
  filter(fy_due == "2021/22") %>% 
  arrange(upi_ac,financial_year_ac.x,fin_month_ac.x,desc(date_start)) %>%
  group_by(upi_ac,financial_year_ac.x,fin_month_ac.x) %>% 
  slice(n()) %>% 
  ungroup()



View(final_follow_ups %>% filter(financial_year_ac.x == current_year) %>%
       distinct(upi_ac,financial_year_ac.x,fin_month_ac.x))

View(final_follow_ups %>% tabyl(exclusion_flag))
View(final_follow_ups %>% get_dupes()) 
View(final_follow_ups %>% get_dupes(upi_ac))

### 5-  Create KPI 1.4a table -----

# KPI 1.4 statistics are collated based on hb of residence where 
# follow up screen took place rather than the hbres where trigger appointment
# occurred. This means if a man had trigger screen in Highland and then 
# followup appoinment 12m later in Grampian, his whole record will be counted under Grampian.
# *If the man wasn't tested then assume the man is still in his health board where the trigger screening occurred.

# Allocate correct hb in case an individual has moved between screenings
# Create variable for Scotland
final_follow_ups %<>% 
  mutate(hbres_final = (case_when(!is.na(date_screen_sc) ~ hbres_sc,
                                  is.na(date_screen_sc) ~ hbres_ac.x)))

# number of screenings by 

kpi_1.4a <- final_follow_ups %>% 
  group_by(fy_due,hbres_final) %>% 
  summarise(sum(cohort_ac.x),sum(attend)) %>% 
  group_modify(~ adorn_totals(.x, where = "row")) %>% 
  ungroup() %>%
  mutate(pc = `sum(attend)` * 100 / `sum(cohort_ac.x)`) %>%
  mutate(pc = round_half_up(pc, 2))


View(kpi_1.4a)


### 6 - Create 3M Surveillance figures ----

quarterly_surveillance_cohort <- aaa_extract %>% 
  filter(!is.na(financial_year)) %>% 
  filter(followup_recom == "01") %>% #12,059 rows
  
  # arrange(upi, financial_year,fin_month,date_screen) %>% 
  # group_by(upi, financial_year,fin_month) %>% 
  # slice(n()) %>% 
  # ungroup() %>% #12057
  
  filter(fy_quarter %in% c("2020/21_4","2021/22_1","2021/22_2","2021/22_3")) %>% # needs to be added to beginning of script
  mutate(cohort = 1) #%>% # 2826 rows - matched
#filter(financial_year == "2020/21") # 1,353 rows

# add ac for annual cohort to identify more easily when joining
colnames(quarterly_surveillance_cohort)<-paste(colnames(quarterly_surveillance_cohort),"qc",sep="_")

quarterly_follow_up_appointments <- screened_cohort %>% 
  left_join(quarterly_surveillance_cohort, by = c("upi_sc"="upi_qc")) %>%
  #select(-upi_sc) %>% 
  # select(financial_year.x:sex.x,pat_elig.x:simd2020v2_hb2019_quintile.x,screen_type.x:followup_recom.x,
  #        largest_measure.x:date_verified.x,screen_n.x,fin_month.x,screen_type.y:date_verified.y,financial_year) %>% 
  mutate(interval = difftime(date_screen_sc,date_screen_qc,units = "days")) %>% 
  filter(date_screen_sc > date_screen_qc & 
           interval >= 0 & 
           interval <= 1999 &
           !is.na(fin_month_qc))  %>% # 3050 rows
  arrange(upi_sc,financial_year_qc,fin_month_qc,date_screen_sc) %>%
  group_by(upi_sc,financial_year_qc,fin_month_qc) %>% 
  slice(n()) %>% 
  ungroup() %>% 
  rename(upi = upi_sc)# 1301 rows

#colnames(follow_up_appointments)<-paste(colnames(follow_up_appointments),"fua",sep="_")

# Check duplicates
View(follow_up_appointments %>% get_dupes()) 
View(follow_up_appointments %>% get_dupes(upi_ac))

# Match exlusions to cohort
# calculate interval and only keep those within 408 days of the surveillance cohort date
# remove duplicates
quarterly_exclusions_appointments <- aaa_exclusions %>% 
  left_join(quarterly_surveillance_cohort, by = c("upi"="upi_qc")) %>% 
  mutate(interval = difftime(date_start,date_screen_qc,units = "days")) %>% 
  filter(date_start >= date_screen_qc & 
           interval >= 0 & 
           interval <= 120 &
           !is.na(fin_month_qc))%>%
  arrange(upi,financial_year_qc,fin_month_qc,desc(date_start)) %>%
  group_by(upi,financial_year_qc,fin_month_qc) %>% 
  slice(n()) %>% 
  ungroup()

# 106 rows

#colnames(exclusions_appointments)<-paste(colnames(exclusions_appointments),"ea",sep="_")


# Check duplicates
View(exclusions_appointments %>% get_dupes()) 
View(exclusions_appointments %>% get_dupes(upi_ac))

# Combine follow up appointments and exclusions
quarterly_combined_appointments <- quarterly_follow_up_appointments %>% 
  bind_rows(quarterly_exclusions_appointments) #%>% 
#filter(financial_year_qc == current_year)

# create final follow-ups table
quarterly_final_follow_ups <- quarterly_surveillance_cohort %>% 
  left_join(quarterly_combined_appointments, by = c("upi_qc"="upi"))


View(combined_appointments %>% get_dupes()) 
View(combined_appointments %>% get_dupes(upi_ac))

# Remove unecessary columns
# Create a flag for attended follow up appointments
# If there is an exclusion flag but the appointment was attended and has a date_screen recode exclusion to zero
# Filter for only those records with no exclusion
# select all records where screen_n_ac = 1 to select only records from the original cohort
# Add expected follow-up year

#### CHECK: line 720 SPSS - altered year due as SPSS suggests the appointment will always be due in the same year as the original screening however this is not the case
#####FUTURE EDIT: These are arbitrarily sorted before deduplication * 
# this comes out with a comparable result to SPSS but should be clarified in future
quarterly_final_follow_ups <- quarterly_final_follow_ups %>%
  mutate(follow_up_screen_flag = (case_when(!is.na(date_screen_sc)~ 1,
                                            is.na(date_screen_sc)~0))) %>% 
  
  
  arrange(upi_qc) %>%
  mutate(attend = case_when(!is.na(date_screen_sc) ~ 1,
                            is.na(date_screen_sc) ~ 0)) %>%
  
  mutate(exclusion_flag = ifelse((pat_inelig %in% c('04','06','11','12','13','14','15','16','17','18','19','21', '22', '25', '26') &
                                    is.na(date_end))|
                                   pat_inelig %in% c('03','05'),1,0)) %>% 
  mutate(exclusion_flag_final = ifelse(attend ==1,0,exclusion_flag)) %>% 
  filter(exclusion_flag_final == 0) %>%
  mutate(fy_due = extract_fin_year((as.POSIXct(date_screen_qc.x)+months(3)))) %>% 
  filter(!is.na(fy_due)) %>% 
  arrange(upi_qc,financial_year_qc.x,fy_quarter_qc.x,desc(date_start)) %>%
  group_by(upi_qc,financial_year_qc.x,fy_quarter_qc.x) %>% 
  slice(n()) %>% 
  ungroup()

quarterly_final_follow_ups %<>% 
  mutate(hbres_final = (case_when(!is.na(date_screen_sc) ~ hbres_sc,
                                  is.na(date_screen_sc) ~ hbres_qc.x)))

# View(final_follow_ups %>% filter(financial_year_ac.x == current_year) %>%
#        distinct(upi_ac,financial_year_ac.x,fin_month_ac.x))

View(final_follow_ups %>% tabyl(exclusion_flag))
View(final_follow_ups %>% get_dupes()) 
View(final_follow_ups %>% get_dupes(upi_ac))

# number of screenings by 

kpi_1.4b <- quarterly_final_follow_ups %>% 
  group_by(fy_due,hbres_final) %>% 
  summarise(sum(cohort_qc.x),sum(attend)) %>% 
  group_modify(~ adorn_totals(.x, where = "row")) %>% 
  ungroup() %>%
  mutate(pc = `sum(attend)` * 100 / `sum(cohort_qc.x)`) %>%
  mutate(pc = round_half_up(pc, 2))


View(kpi_1.4b)


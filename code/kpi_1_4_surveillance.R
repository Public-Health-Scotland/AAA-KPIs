#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# KPI_1.4 (Surveillance).R
# Eibhlin O'Sullivan
# Oct 2022
# Define housekeeping variables used by subsequent scripts
# Written/run on R Studio Server
# R version 3.6.1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### 1 - Housekeeping ---

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

# Define filepaths

aaa_extracts_path <- (paste0("/PHI_conf/AAA/Topics/Screening/extracts/202209/output/"))

### 2 - Read in Files ---

# update to take aaa_extract_path
aaa_extract <- readRDS("/PHI_conf/AAA/Topics/Screening/extracts/202209/output/aaa_extract_202209.rds")

aaa_exclusions <- readRDS("/PHI_conf/AAA/Topics/Screening/extracts/202209/output/aaa_exclusions_202209.rds")

### 3 - Format Tables ---

# check number of screened records
aaa_exclusions %>% nrow()
# 133707 rows

View(aaa_exclusions %>% get_dupes())
# 65 duplicate rows but these are not removed in SPSS so keeping for now
  

# aaa_extract remove columns
# assign numerator to those screened
# create fin_year, qtr, fin_month fields

# ** may need to add largest measure convert NA to 0
aaa_extract %<>% 
  select(upi,date_screen,date_offer_sent,followup_recom,screen_result,largest_measure,
         hbres,pat_elig,att_dna,screen_type,screen_exep,result_verified,result_outcome) %>%
  mutate(screen_n = ifelse(screen_result %in% c("01","02","04") &
                                (screen_type %in% c("02","04")),1,0)) %>% 
  mutate(fin_year = extract_fin_year(as.Date(date_screen)),
         month = format(as.Date(date_screen, format = "%Y-%m-%d"),"%m"),
         qtr = case_when(month %in% c('04','05','06') ~ 1,
                         month %in% c('07','08','09') ~ 2,
                         month %in% c('10','11','12') ~ 3,
                         month %in% c('01','02','03') ~ 4),
        fin_month = case_when(month %in% c('04','05','06','07','08','09','10','11','12') ~ as.numeric(month)-3,
                              month == '03' ~ 12,
                              month == '02' ~ 11,
                              month == '01' ~ 10)) %>% 
  select(-month) %>% 
  filter(!is.na(fin_year)) %>% 
  glimpse()

# 485,728 rows sept 2022

# check number of screened records
aaa_exclusions %>% nrow()
#
aaa_extract %>% filter(screen_n == 1) %>% nrow()

# 18,647 screened

### 4 - 12 Month Surveillance Uptake ---

# create a list of all screening results with a recommendation to follow up in 12 months followup_recomm = 02
# keep latest screening date per month per upi
annual_surveillance_cohort <- aaa_extract %>% 
  filter(followup_recom == "02") %>% 
  rename(c_date_screen = date_screen) %>% 
  arrange(upi, fin_year,fin_month,desc(c_date_screen)) %>% 
  group_by(upi, fin_year,fin_month) %>% 
  slice(n()) %>% 
  ungroup()


#join 

##### total indv result files contain 2826 rows

screening_cohort <- aaa_extract %>% 
  filter(screen_n == 1) #%>% 
  #inner_join(annual_surveillance_cohort, by = "upi")

#fin_month
#fin_qtr

  






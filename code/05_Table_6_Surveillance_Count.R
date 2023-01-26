#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 05_Table_6_Surveillance_Count.R
# Eibhlin O'Sullivan
# Jan 2022
# Supplementary tables - Table 6 (Surveillance: number of men tested)
# Written/run on R Studio Server
# R version 3.6.1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# NOTE: Please check this carefully as the output will not match the SPSS output.
# There is no sorting before the lag step in SPSS this has been amended here to
# sort by date so that the individuals previous appointment is taken rather than
# an arbitrary row.

# 1 - Housekeeping
# 2 - Read in Extract
# 3 - Clean Data
# 4 - Format and Save Table

### 1 - Housekeeping ----

library(here)

## Source housekeeping file
source(here::here("code", "00_housekeeping.R"))
wd <- (paste0("/PHI_conf/AAA/Topics/Projects/20221019-HackDay/EOS/AAA-KPIs"))

### 2 - Read in Extract ----

# AAA extract
aaa_extract <- readRDS(paste0(aaa_extracts_path,"aaa_extract_202209.rds"))

### 3 - Clean Data ----

# filter records prior to cut off date
# filter screening result as positive, negative or non-visualisation
surveillance_summary <- aaa_extract %>%
  filter(date_screen <= cut_off_date,
         screen_result %in% c("01","02","04")) %>% 
  arrange(upi,date_screen)


# 281,593 rows

# Check for records without a follow up recommendation
# there have been no records for several runs so the fix has not been moved from SPSS
surveillance_summary %>% tabyl(screen_result,followup_recom)
table(surveillance_summary$followup_recom)

# for run based on 1 March 2018 extract there were 3 records without a follow-up recommendation.
# for run based on 1 March 2019 extract there were 0 records without a follow-up recommendation.
# for run based on 1 March 2020 extract there were 0 records without a follow-up recommendation.
# for run based on 9 April 2021 extract there were 0 records without a follow-up recommendation.
# for run based on 1 September 2021 extract there were 0 records without a follow-up recommendation.
# for run based on 14 September 2022 extract there were 0 records without a follow-up recommendation

## 3.1 - Determine Surveillance Type ----

# Get the type of of surveillance from the previous appointmentin the patients record
# '01' '3 Months'
# '02' '12 Months'
# '03' 'Discharge'
# '05' 'Immediate recall'
# '06' 'No further recall'.
# *KH: '04' must be 'Refer to Vascular'?

# surveillance_summary$surveillance_type <- lag(surveillance_summary$followup_recom, n=1)

# Get the type of surveillance from the previous appointment in the patients record.
# SPSS does not sort any records before using a lag so this returns a different result
surveillance_summary %<>%
  mutate(upi1 = lag(upi, n = 1), 
          fr1= lag(followup_recom, n = 1),
         surveillance_type = ifelse(upi == upi1,fr1,NA)) %>%
  select(-c(upi1, fr1))
  
# check number of records per surveillance  
table(surveillance_summary$surveillance_type)

  View(surveillance_summary %>% get_dupes(upi))

# select only those with 12 month or vascular recall
surveillance_summary %<>%
  filter(screen_type %in% c("02","04"))
  
# check number of records per surveillance  
table(surveillance_summary$surveillance_type)
surveillance_summary %>%  nrow()

# If the surveillance type is blank/immediate recall and the eligibility is from previous 
# cohort and follow up is not referred to vascular use the current followup_recom 
surveillance_summary %<>%
  mutate(surveillance_type = if_else(((is.na(surveillance_type)|surveillance_type == "05") &
                                  followup_recom == "01" & followup_recom != "02"),followup_recom,surveillance_type))

# check number of records per surveillance  
table(surveillance_summary$surveillance_type)

# If the surveillance is still blank  or immediate recall AND the followup_recom is
# quarterly or annual surveillance,   just use the current FU recom as a proxy
surveillance_summary %<>%
  mutate(surveillance_type = if_else(((is.na(surveillance_type)|surveillance_type == "05") &
                                        (followup_recom == "01" | followup_recom == "02")), followup_recom,surveillance_type))

# check number of records per surveillance  
table(surveillance_summary$surveillance_type)

# keep records for surveillance 01 (3 months) and 02 (12 months)
# copy financial year to screen_year
surveillance_summary %<>% 
  filter(surveillance_type %in% c("01","02")) %>% 
  mutate(screen_year = ifelse(financial_year %in% c(prev_year,current_year,next_year),as.character(financial_year),NA)) %>% 
  mutate(count = 1) 

# check number of records per year and upi
surveillance_summary %>% 
  group_by(screen_year,upi,hbres,surveillance_type) %>% 
  summarise(count = n())

# check number of records per year  
surveillance_summary %>% 
  group_by(screen_year) %>% 
  summarise(count = n())


### 4 - Format and Save Table ----
# format output table
surveillance_summary_table6 <- surveillance_summary %>% 
  mutate(surveillance_type = case_when(surveillance_type == "01" ~ "3 months",
                                       surveillance_type == "02" ~ "12 months")) %>% 
  group_by(screen_year,hbres,surveillance_type) %>% 
  summarise(count = sum(count)) %>% 
  pivot_wider(names_from = c(hbres),values_from = count) %>% 
  mutate(Scotland = rowSums(across(where(is.numeric)), na.rm=TRUE)) %>% 
  pivot_longer(cols = c(3:17), names_to = "hbres", values_to = "count") %>% 
  arrange(screen_year, desc(surveillance_type)) %>% 
  pivot_wider(names_from = c(screen_year,surveillance_type), values_from = count)
  
# create order of rows
template <- tibble(hbres = c("Scotland","Ayrshire & Arran","Borders","Dumfries & Galloway", "Fife", "Forth Valley", 
                             "Grampian", "Greater Glasgow & Clyde", "Highland", "Lanarkshire", "Lothian", "Orkney",
                             "Shetland", "Tayside","Western Isles"))

# join summary table to template
surveillance_summary_table6 <- template %>% 
  left_join(surveillance_summary_table6, by  = "hbres")

# save out table file
write_rds(surveillance_summary_table6, paste0(wd, "/temp/surveillance_summary_table6.rds"))






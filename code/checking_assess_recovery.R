library(haven)

aaa_temp <- (paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/temp/KPIs/KPI1.4a - KPI1.4b/"))

# KPI 1.4 12m

# combine monthly scripts

M1_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_cohort_M1_2020_21.zsav"))
M2_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_cohort_M2_2020_21.zsav"))
M3_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_cohort_M3_2020_21.zsav"))
M4_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_cohort_M4_2020_21.zsav"))
M5_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_cohort_M5_2020_21.zsav"))
M6_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_cohort_M6_2020_21.zsav"))
M7_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_cohort_M7_2020_21.zsav"))
M8_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_cohort_M8_2020_21.zsav"))
M9_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_cohort_M9_2020_21.zsav"))
M10_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_cohort_M10_2020_21.zsav"))
M11_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_cohort_M11_2020_21.zsav"))
M12_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_cohort_M12_2020_21.zsav"))

total_cohort <- bind_rows(                
  M1_2020_21,              
  M10_2020_21,             
  M11_2020_21,              
  M12_2020_21,               
  M2_2020_21,             
  M3_2020_21,            
  M4_2020_21,           
  M5_2020_21,            
  M6_2020_21,           
  M7_2020_21,          
  M8_2020_21,         
  M9_2020_21)


files <- rm(M1_2020_21,              
              M10_2020_21,             
              M11_2020_21,              
              M12_2020_21,               
              M2_2020_21,             
              M3_2020_21,            
              M4_2020_21,           
              M5_2020_21,            
              M6_2020_21,           
              M7_2020_21,          
              M8_2020_21,         
              M9_2020_21)

# Compare 12m numerator

kpi_1_4_numerator <- read_sav(paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/temp/KPIs/KPI1.4a - KPI1.4b/KPI1.4_numerator.zsav"))

kpi_1_4_numerator_check <- kpi_1_4_numerator %>%
  select(UPI,Scrn_Date) %>% 
  rename(upi = UPI,
         date_screen = Scrn_Date)

numerator_check <- screened_cohort %>%
  select(upi_sc,date_screen_sc) %>% 
  rename(upi = upi_sc,
         date_screen = date_screen_sc)

compare_inr_ninspss <- numerator_check %>% 
  anti_join(kpi_1_4_numerator_check, by = c("upi","date_screen"))

compare_ninr_inspss <- kpi_1_4_numerator_check %>% 
  anti_join(numerator_check, by = c("upi","date_screen"))

### MATCHED ###

rm(kpi_1_4_numerator,kpi_1_4_numerator_check,numerator_check,compare_inr_ninspss,compare_ninr_inspss)


# Compare 12m cohort

total_cohort_check <- total_cohort %>%
  select(UPI,c_scrn_date) %>% 
  rename(upi = UPI,
         date_screen = c_scrn_date)

annual_surveillance_cohort_check <- annual_surveillance_cohort %>%
  select(upi_ac,date_screen_ac) %>% 
  rename(upi = upi_ac,
         date_screen = date_screen_ac)

compare_inr_ninspss <- annual_surveillance_cohort_check %>% 
  anti_join(total_cohort_check, by = c("upi","date_screen"))

compare_ninr_inspss <- total_cohort_check %>% 
  anti_join(annual_surveillance_cohort_check, by = c("upi","date_screen"))

### MATCHED ###

rm(total_cohort_check,annual_surveillance_cohort_check,compare_inr_ninspss,compare_ninr_inspss)


# Compare 12m numerator

total_numerator_12m_check <- total_numerator %>%
  select(UPI,c_scrn_date,Scrn_Date) %>% 
  rename(upi = UPI,
         date_screen_ac = c_scrn_date,
         date_screen_sc = Scrn_Date)

annual_follow_up_appointments_check <- follow_up_appointments %>%
  select(upi,date_screen_ac,date_screen_sc,interval) %>% 
  rename(upi = upi,
         date_screen_ac = date_screen_ac,
         date_screen_sc = date_screen_sc)

compare_inr_ninspss <- annual_follow_up_appointments_check %>% 
  anti_join(total_numerator_12m_check, by = c("upi","date_screen_ac","date_screen_sc"))

compare_ninr_inspss <- total_numerator_12m_check %>% 
  anti_join(annual_follow_up_appointments_check, by = c("upi","date_screen_ac","date_screen_sc"))

### MATCHED ###

rm(total_numerator_12m_check,annual_follow_up_appointments_check,compare_inr_ninspss,compare_ninr_inspss)


# Compare 12m exclusions

total_exclusions_12m_check <- total_exclusions %>%
  select(UPI,c_scrn_date,Start_Date) %>% 
  rename(upi = UPI,
         date_screen_ac = c_scrn_date,
         date_start = Start_Date)

exclusions_appointments_check <- exclusions_appointments %>%
  select(upi,date_screen_ac,date_start) %>% 
  rename(upi = upi,
         date_screen_ac = date_screen_ac)

compare_inr_ninspss <- exclusions_appointments_check %>% 
  anti_join(total_exclusions_12m_check, by = c("upi","date_screen_ac","date_start"))

compare_ninr_inspss <- total_exclusions_12m_check %>% 
  anti_join(exclusions_appointments_check, by = c("upi","date_screen_ac","date_start"))

### MATCHED ###

rm(total_exclusions_12m_check,exclusions_appointments_check,compare_inr_ninspss,compare_ninr_inspss)


# Compare 12m final vs final follow ups

kpi_1_4_final <- read_sav(paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/temp/KPIs/KPI1.4a - KPI1.4b/KPI1.4_12m_final.zsav"))

#MATCHED

######################################################################################################################################

# combine monthly scripts

M10_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_3m_excl_M10_2020_21.zsav"))
M11_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_3m_excl_M11_2020_21.zsav"))
M12_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_3m_excl_M12_2020_21.zsav"))

M1_2021_22 <- read_sav(paste0(aaa_temp,"KPI1.4_3m_excl_M1_2021_22.zsav"))
M2_2021_22 <- read_sav(paste0(aaa_temp,"KPI1.4_3m_excl_M2_2021_22.zsav"))
M3_2021_22 <- read_sav(paste0(aaa_temp,"KPI1.4_3m_excl_M3_2021_22.zsav"))
M4_2021_22 <- read_sav(paste0(aaa_temp,"KPI1.4_3m_excl_M4_2021_22.zsav"))
M5_2021_22 <- read_sav(paste0(aaa_temp,"KPI1.4_3m_excl_M5_2021_22.zsav"))
M6_2021_22 <- read_sav(paste0(aaa_temp,"KPI1.4_3m_excl_M6_2021_22.zsav"))
M7_2021_22 <- read_sav(paste0(aaa_temp,"KPI1.4_3m_excl_M7_2021_22.zsav"))
M8_2021_22 <- read_sav(paste0(aaa_temp,"KPI1.4_3m_excl_M8_2021_22.zsav"))
M9_2021_22 <- read_sav(paste0(aaa_temp,"KPI1.4_3m_excl_M9_2021_22.zsav"))


total_exclusions_3m <- bind_rows(                
  M10_2020_21,              
  M11_2020_21,             
  M12_2020_21,
  M1_2021_22,
  M2_2021_22,             
  M3_2021_22,           
  M4_2021_22,           
  M5_2021_22,            
  M6_2021_22,           
  M7_2021_22,          
  M8_2021_22,        
  M9_2021_22)


files <- rm(M10_2020_21,              
            M11_2020_21,             
            M12_2020_21,
            M1_2021_22,
            M2_2021_22,             
            M3_2021_22,           
            M4_2021_22,           
            M5_2021_22,            
            M6_2021_22,           
            M7_2021_22,          
            M8_2021_22,        
            M9_2021_22)


# Compare 3m cohort

kpi_1_4_cohort_3m <- read_sav(paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/temp/KPIs/KPI1.4a - KPI1.4b/KPI1.4_cohort_3M.zsav"))

kpi_1_4_cohort_3m_check <- kpi_1_4_cohort_3m %>%
  select(UPI,c_scrn_date) %>% 
  rename(upi = UPI,
         date_screen = c_scrn_date)

quarterly_surveillance_cohort_check <- quarterly_surveillance_cohort %>%
  select(upi_qc,date_screen_qc) %>% 
  rename(upi = upi_qc,
         date_screen = date_screen_qc)

compare_inr_ninspss <- quarterly_surveillance_cohort_check %>% 
  anti_join(kpi_1_4_cohort_3m_check, by = c("upi","date_screen"))

compare_ninr_inspss <- kpi_1_4_cohort_3m_check %>% 
  anti_join(quarterly_surveillance_cohort_check, by = c("upi","date_screen"))

### MATCHED ###

rm(kpi_1_4_cohort_3m_check,quarterly_surveillance_cohort_check,compare_inr_ninspss,compare_ninr_inspss)

# Compare 3m numerator

total_numerator_3m_check <- total_numerator_3m %>%
  select(UPI,c_scrn_date,Scrn_Date) %>% 
  rename(upi = UPI,
         date_screen_qc = c_scrn_date,
         date_screen_sc = Scrn_Date)

quarterly_follow_up_appointments_check <- quarterly_follow_up_appointments %>%
  select(upi,date_screen_qc,date_screen_sc) %>% 
  rename(upi = upi,
         date_screen_qc = date_screen_qc,
         date_screen_sc = date_screen_sc)

compare_inr_ninspss <- quarterly_follow_up_appointments_check %>% 
  anti_join(total_numerator_3m_check, by = c("upi","date_screen_qc","date_screen_sc"))

compare_ninr_inspss <- total_numerator_3m_check %>% 
  anti_join(quarterly_follow_up_appointments_check, by = c("upi","date_screen_qc","date_screen_sc"))

### MATCHED ###

rm(total_numerator_3m_check,quarterly_follow_up_appointments_check,compare_inr_ninspss,compare_ninr_inspss)

# Compare 12m exclusions

total_exclusions_3m_check <- total_exclusions_3m %>%
  select(UPI,c_scrn_date,Start_Date) %>% 
  rename(upi = UPI,
         date_screen_qc = c_scrn_date,
         date_start = Start_Date)

quarterly_exclusions_appointments_check <- quarterly_exclusions_appointments %>%
  select(upi,date_screen_qc,date_start) %>% 
  rename(upi = upi,
         date_screen_qc = date_screen_qc)

compare_inr_ninspss <- quarterly_exclusions_appointments_check %>% 
  anti_join(total_exclusions_3m_check, by = c("upi","date_screen_qc","date_start"))

compare_ninr_inspss <- total_exclusions_3m_check %>% 
  anti_join(quarterly_exclusions_appointments_check, by = c("upi","date_screen_qc","date_start"))

### MATCHED ###

rm(total_exclusions_3m_check,quarterly_exclusions_appointments_check,compare_inr_ninspss,compare_ninr_inspss)

# Compare 3m combined appointments

kpi_1_4_total_3m <- read_sav(paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/temp/KPIs/KPI1.4a - KPI1.4b/KPI1.4_3m_total.zsav"))

kpi_1_4_total_3m_check <- kpi_1_4_total_3m %>%
  select(UPI,c_scrn_date,Scrn_Date,PatInElig,Start_Date, End_Date) %>% 
  rename(upi = UPI,
         date_screen_qc = c_scrn_date,
         date_screen_sc = Scrn_Date,
         date_start = Start_Date)

quarterly_combined_appointments_check <- quarterly_combined_appointments %>%
  select(upi_qc,date_screen_qc,date_screen_sc,pat_inelig, date_start,date_end) %>% 
  rename(upi = upi_qc,
         date_screen_qc = date_screen_qc,
         date_screen_sc = date_screen_sc,
         date_start = date_start)

compare_inr_ninspss <- quarterly_combined_appointments_check %>% 
  anti_join(kpi_1_4_total_3m_check, by = c("upi","date_screen_qc","date_screen_sc","date_start"))

compare_ninr_inspss <- kpi_1_4_total_3m_check %>% 
  anti_join(quarterly_combined_appointments_check, by = c("upi","date_screen_qc","date_screen_sc","date_start"))

### MATCHED ###

rm(kpi_1_4_cohort_3m_check,quarterly_surveillance_cohort_check,compare_inr_ninspss,compare_ninr_inspss)

# Compare 3m final appointments

kpi_1_4_final_3m <- read_sav(paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/temp/KPIs/KPI1.4a - KPI1.4b/KPI1.4_3m_final.zsav"))

kpi_1_4_final_3m_check <- kpi_1_4_final_3m %>%
  select(UPI,c_scrn_date,Scrn_Date,PatInElig,Start_Date, End_Date) %>% 
  rename(upi = UPI,
         date_screen_qc = c_scrn_date,
         date_screen_sc = Scrn_Date,
         date_start = Start_Date)

quarterly_final_follow_ups_check <- quarterly_final_follow_ups %>%
  select(upi_qc,date_screen_qc,date_screen_sc,pat_inelig, date_start,date_end) %>% 
  rename(upi = upi_qc,
         date_screen_qc = date_screen_qc,
         date_screen_sc = date_screen_sc,
         date_start = date_start)

compare_inr_ninspss <- quarterly_final_follow_ups_check %>% 
  anti_join(kpi_1_4_final_3m_check, by = c("upi","date_screen_qc","date_screen_sc","date_start"))

compare_ninr_inspss <- kpi_1_4_final_3m_check %>% 
  anti_join(quarterly_final_follow_ups_check, by = c("upi","date_screen_qc","date_screen_sc","date_start"))

### MATCHED ###

rm(kpi_1_4_cohort_3m_check,quarterly_surveillance_cohort_check,compare_inr_ninspss,compare_ninr_inspss)


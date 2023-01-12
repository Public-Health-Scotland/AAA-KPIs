library(haven)

aaa_temp <- (paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/temp/KPIs/KPI1.4a - KPI1.4b/"))

# import spss  
kpi_1_4_12m_total <- read_sav(paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/temp/KPIs/KPI1.4a - KPI1.4b/KPI1.4_12m_total.zsav")) #%>% 
  select(FY,Q,M,UPI,count,c_scrn_date,Scrn_Date,Start_Date,End_Date)

kpi_1_4_12m_final <- read_sav(paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/temp/KPIs/KPI1.4a - KPI1.4b/KPI1.4_12m_final.zsav")) #%>% 
  select(FY,Q,M,UPI,count,c_scrn_date,Scrn_Date,Start_Date,End_Date)



  
kpi_1_4_12m_final_result <- read_sav(paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/temp/KPIs/KPI1.4a - KPI1.4b/KPI1.4_12m_FinalResult.zsav")) #%>% 
  select(FY,Q,M,UPI,count,c_scrn_date,Scrn_Date,Start_Date,End_Date)
  
kpi_1_4_3m_final_result <- read_sav(paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/temp/KPIs/KPI1.4a - KPI1.4b/KPI1.4_3m_FinalResult.zsav")) #%>% 
  select(FY,Q,M,UPI,count,c_scrn_date,Scrn_Date,Start_Date,End_Date)

# suffix datasets
  
colnames(kpi_1_4_12m_total)<-paste(colnames(kpi_1_4_12m_total),"spss",sep="_")

colnames(kpi_1_4_12m_final)<-paste(colnames(kpi_1_4_12m_final),"spss",sep="_")


combined_appointments_check <- combined_appointments %>% 
  select(upi_ac,screen_n_ac,date_screen_ac,date_screen,date_start,date_end,exclusion_flag)

colnames(combined_appointments_check)<-paste(colnames(combined_appointments_check),"r",sep="_")

# Check data sets

View(final_follow_ups %>% select(date_screen_ac.x,date_screen_sc,exclusion) %>% filter(is.na(date_screen_sc)))

View(combined_appointments %>% select(date_screen_ac.x,date_screen_sc,follow_up_screen_flag) %>% filter(follow_up_screen_flag==0))

View(quarterly_surveillance_cohort %>% filter(financial_year_qc == "2020/21"))

View(kpi_1_4_3m_final %>% distinct(UPI,FY,Q))

View(screened_cohort)

# combine monthly scripts
M1_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_excl_M1_2019_20.zsav"))
M2_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_excl_M2_2019_20.zsav"))
M3_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_excl_M3_2019_20.zsav"))
M4_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_excl_M4_2019_20.zsav"))
M5_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_excl_M5_2019_20.zsav"))
M6_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_excl_M6_2019_20.zsav"))
M7_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_excl_M7_2019_20.zsav"))
M8_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_excl_M8_2019_20.zsav"))
M9_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_excl_M9_2019_20.zsav"))
M10_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_excl_M10_2019_20.zsav"))
M11_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_excl_M11_2019_20.zsav"))
M12_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_excl_M12_2019_20.zsav"))

M1_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_excl_M1_2020_21.zsav"))
M2_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_excl_M2_2020_21.zsav"))
M3_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_excl_M3_2020_21.zsav"))
M4_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_excl_M4_2020_21.zsav"))
M5_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_excl_M5_2020_21.zsav"))
M6_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_excl_M6_2020_21.zsav"))
M7_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_excl_M7_2020_21.zsav"))
M8_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_excl_M8_2020_21.zsav"))
M9_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_excl_M9_2020_21.zsav"))
M10_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_excl_M10_2020_21.zsav"))
M11_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_excl_M11_2020_21.zsav"))
M12_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_excl_M12_2020_21.zsav"))

total_exclusions <- bind_rows(                
  M1_2019_20,                
  M1_2020_21,M10_2019_20,              
  M10_2020_21,M11_2019_20,             
  M11_2020_21,M12_2019_20,              
  M12_2020_21,M2_2019_20,               
  M2_2020_21,M3_2019_20,             
  M3_2020_21,M4_2019_20,            
  M4_2020_21,M5_2019_20,           
  M6_2020_21,M7_2019_20,           
  M7_2020_21,M8_2019_20,          
  M8_2020_21,M9_2019_20,         
  M9_2020_21)


files <- rm(M1_2019_20,                
              M1_2020_21,M10_2019_20,              
              M10_2020_21,M11_2019_20,             
              M11_2020_21,M12_2019_20,              
              M12_2020_21,M2_2019_20,               
              M2_2020_21,M3_2019_20,             
              M3_2020_21,M4_2019_20,            
              M4_2020_21,M5_2019_20,           
              M5_2020_21,M6_2019_20,            
              M6_2020_21,M7_2019_20,           
              M7_2020_21,M8_2019_20,          
              M8_2020_21,M9_2019_20,         
              M9_2020_21)

# Compare 12m_final and final followups

final_follow_ups_check <- final_follow_ups %>%
  select(upi_ac,date_screen_ac.x,date_screen_sc,exclusion, exclusion_flag, exclusion_flag_final, pat_inelig, attend, cohort_ac.x,cohort_ac.y, date_start,date_end) %>% 
  rename(upi = upi_ac,
         date_screen_ac = date_screen_ac.x,
         date_screen_sc = date_screen_sc,
         exclusion = exclusion)
  
kpi_1_4_12m_final_check <- kpi_1_4_12m_final %>%
  select(UPI,c_scrn_date,Scrn_Date,excl,attend,PatInElig,cohort, Start_Date, End_Date) %>% 
  rename(upi = UPI,
         date_screen_ac = c_scrn_date,
         date_screen_sc = Scrn_Date,
         exclusion = excl)



compare_inr_ninspss <- final_follow_ups_check %>% 
  anti_join(kpi_1_4_12m_final_check, by = c("upi","date_screen_ac","date_screen_sc"))

compare_ninr_inspss <- kpi_1_4_12m_final_check %>% 
  anti_join(final_follow_ups_check, by = c("upi","date_screen_ac","date_screen_sc"))

# Compare 3m cohort

kpi_1_4_3m_cohort <- read_sav(paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/temp/KPIs/KPI1.4a - KPI1.4b/KPI1.4_cohort_3M.zsav")) #%>% 
select(UPI,count,c_scrn_date,Scrn_Date,Start_Date,End_Date)

kpi_1_4_3m_cohort_check <- kpi_1_4_3m_cohort %>%
  select(UPI,c_scrn_date) %>% 
  rename(upi = UPI,
         date_screen_qc = c_scrn_date)

quarterly_surveillance_cohort_check <- quarterly_surveillance_cohort %>%
  select(upi,date_screen) %>% 
  rename(date_screen_qc = date_screen)

compare_inr_ninspss <- quarterly_surveillance_cohort_check %>% 
  anti_join(kpi_1_4_3m_cohort_check, by = c("upi","date_screen_qc"))

compare_ninr_inspss <- kpi_1_4_3m_cohort_check %>% 
  anti_join(quarterly_surveillance_cohort_check, by = c("upi","date_screen_qc"))

View(quarterly_surveillance_cohort_check %>% get_dupes())

# matched - 8997 to 8996 one duplicate which will be removed

rm(kpi_1_4_3m_cohort_check,quarterly_surveillance_cohort_check,compare_inr_ninspss,compare_ninr_inspss)

# Compare 3m numerator - quarterly surveillance cohort

total_cohort_3m %>% nrow()

kpi_1_4_3m_cohort_check <- total_cohort_3m %>%
  select(UPI,c_scrn_date) %>% 
  rename(upi = UPI,
         date_screen_qc = c_scrn_date)

quarterly_surveillance_cohort_check <- quarterly_surveillance_cohort %>%
  select(upi_qc,date_screen_qc) %>% 
  rename(upi = upi_qc)

compare_inr_ninspss <- quarterly_surveillance_cohort_check %>% 
  anti_join(kpi_1_4_3m_cohort_check, by = c("upi","date_screen_qc"))

compare_ninr_inspss <- kpi_1_4_3m_cohort_check %>% 
  anti_join(quarterly_surveillance_cohort_check, by = c("upi","date_screen_qc"))

# matched 1306

rm(kpi_1_4_3m_cohort_check,quarterly_surveillance_cohort_check,compare_inr_ninspss,compare_ninr_inspss)

# Compare 3m numerator - follow ups 

total_numerator_3m %>% nrow()

kpi_1_4_3m_num_check <- total_numerator_3m %>%
  select(UPI,c_scrn_date, Scrn_Date) %>% 
  rename(upi = UPI,
         date_screen_qc = c_scrn_date,
         date_screen_sc = Scrn_Date)

quarterly_follow_up_appointments_check <- quarterly_follow_up_appointments %>%
  select(upi,date_screen_qc,date_screen_sc) %>% 
  rename(upi = upi,
         date_screen_qc = date_screen_qc,
         date_screen_sc = date_screen_sc)

compare_inr_ninspss <- quarterly_follow_up_appointments_check %>% 
  anti_join(kpi_1_4_3m_num_check, by = c("upi","date_screen_qc","date_screen_sc" ))

compare_ninr_inspss <- kpi_1_4_3m_num_check %>% 
  anti_join(quarterly_follow_up_appointments_check, by = c("upi","date_screen_qc","date_screen_sc"))

# matched 1252 - removed 120 day filter

rm(kpi_1_4_3m_num_check,quarterly_follow_up_appointments_check,compare_inr_ninspss,
   compare_ninr_inspss)

# Compare 3m exclusions - exclusions 

total_numerator_3m %>% nrow()

kpi_1_4_3m_excl_check <- total_exclusions_3m %>%
  select(UPI,c_scrn_date, Start_Date) %>% 
  rename(upi = UPI,
         date_screen_qc = c_scrn_date,
         date_start = Start_Date)

quarterly_exclusions_appointments_check <- quarterly_exclusions_appointments %>%
  select(upi,date_screen_qc,date_start) %>% 
  rename(upi = upi,
         date_screen_qc = date_screen_qc,
         date_start = date_start)

compare_inr_ninspss <- quarterly_exclusions_appointments_check %>% 
  anti_join(kpi_1_4_3m_excl_check, by = c("upi","date_screen_qc","date_start" ))

compare_ninr_inspss <- kpi_1_4_3m_excl_check %>% 
  anti_join(quarterly_exclusions_appointments_check, by = c("upi","date_screen_qc","date_start"))

# matched 70

rm(kpi_1_4_3m_excl_check,quarterly_exclusions_appointments_check,compare_inr_ninspss,
   compare_ninr_inspss)


# Compare 3m total and followups

kpi_1_4_3m_total <- read_sav(paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/temp/KPIs/KPI1.4a - KPI1.4b/KPI1.4_3m_total.zsav")) 

kpi_1_4_3m_total_check <- kpi_1_4_3m_total %>%
  select(UPI,c_scrn_date,Scrn_Date,PatInElig,Start_Date, End_Date) %>% 
  rename(upi = UPI,
         date_screen_qc = c_scrn_date,
         date_screen_sc = Scrn_Date)

kpi_1_4_3m_total_check <- kpi_1_4_3m_total_check %>% mutate(PatInElig = na_if(PatInElig,""))

quarterly_final_follow_ups_check <- quarterly_final_follow_ups %>%
  select(upi_qc,date_screen_qc.x,date_screen_sc,pat_inelig, date_start,date_end) %>% 
  rename(upi = upi_qc,
         date_screen_qc = date_screen_qc.x,
         date_screen_sc = date_screen_sc)

compare_inr_ninspss <- quarterly_final_follow_ups_check %>% 
  anti_join(kpi_1_4_3m_total_check, by = c("upi","date_screen_qc","date_screen_sc"))

compare_ninr_inspss <- kpi_1_4_3m_total_check %>% 
  anti_join(quarterly_final_follow_ups_check, by = c("upi","date_screen_qc","date_screen_sc"))



rm(kpi_1_4_3m_total_check,kpi_1_4_3m_total_check,quarterly_exclusions_appointments_check,compare_inr_ninspss,
   compare_ninr_inspss)

# Compare 3m final and followups

kpi_1_4_3m_final <- read_sav(paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/temp/KPIs/KPI1.4a - KPI1.4b/KPI1.4_3m_final.zsav")) 

kpi_1_4_3m_final_check <- kpi_1_4_3m_final %>%
  select(UPI,c_scrn_date,Scrn_Date,excl,attend,PatInElig,cohort, Start_Date, End_Date) %>% 
  rename(upi = UPI,
         date_screen_qc = c_scrn_date,
         date_screen_sc = Scrn_Date,
         exclusion = excl,
         date_start = Start_Date) %>% 
  mutate(interval = difftime(date_screen_sc,date_screen_qc,units = "days"))
kpi_1_4_3m_final_check <- kpi_1_4_3m_final_check %>% mutate(PatInElig = na_if(PatInElig,""))

quarterly_final_follow_ups_check <- quarterly_final_follow_ups %>%
  select(upi_qc,date_screen_qc,date_screen_sc,exclusion, exclusion_flag, exclusion_flag_final, pat_inelig, attend, cohort_qc.x,cohort_qc.y, date_start,date_end) %>% 
  rename(upi = upi_qc,
         date_screen_qc = date_screen_qc,
         date_screen_sc = date_screen_sc,
         exclusion = exclusion) %>% 
  mutate(interval = difftime(date_screen_sc,date_screen_qc,units = "days"))


compare_inr_ninspss <- quarterly_final_follow_ups_check %>% 
  anti_join(kpi_1_4_3m_final_check, by = c("upi","date_screen_qc","date_screen_sc","date_start"))


compare_ninr_inspss <- kpi_1_4_3m_final_check %>% 
  anti_join(quarterly_final_follow_ups_check, by = c("upi","date_screen_qc","date_screen_sc","date_start"))

rm(kpi_1_4_3m_final_check,quarterly_final_follow_ups_check,compare_inr_ninspss,
   compare_ninr_inspss)

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


kpi_1_4_3m_final <- read_sav(paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/temp/KPIs/KPI1.4a - KPI1.4b/KPI1.4_3m_FinalResult.zsav"))
  
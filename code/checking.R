library(haven)

aaa_temp <- (paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/temp/KPIs/KPI1.4a - KPI1.4b/"))

aaa_temp <- (paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/temp/KPIs/KPI1.4a - KPI1.4b/"))

# summarise  
kpi_1_4_12m_total <- read_sav(paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/temp/KPIs/KPI1.4a - KPI1.4b/KPI1.4_12m_total.zsav")) #%>% 
  select(FY,Q,M,UPI,count,c_scrn_date,Scrn_Date,Start_Date,End_Date)


kpi_1_4_3m_cohort <- read_sav(paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/temp/KPIs/KPI1.4a - KPI1.4b/KPI1.4_cohort_3M.zsav")) #%>% 
  select(UPI,count,c_scrn_date,Scrn_Date,Start_Date,End_Date)

colnames(kpi_1_4_12m_total)<-paste(colnames(kpi_1_4_12m_total),"spss",sep="_")


combined_appointments_check <- combined_appointments %>% 
  select(upi_ac,screen_n_ac,date_screen_ac,date_screen,date_start,date_end,exclusion_flag)

colnames(combined_appointments_check)<-paste(colnames(combined_appointments_check),"r",sep="_")


View(final_follow_ups %>% select(date_screen_ac.x,date_screen_sc,exclusion) %>% filter(is.na(date_screen_sc)))

View(combined_appointments %>% select(date_screen_ac.x,date_screen_sc,follow_up_screen_flag) %>% filter(follow_up_screen_flag==0))

View(quarterly_surveillance_cohort %>% filter(financial_year_qc == "2020/21"))

View(kpi_1_4_3m_final %>% distinct(UPI,FY,Q))

View(screened_cohort)

# combine monthly scripts
M1_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_result_M1_2019_20.zsav"))
M2_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_result_M2_2019_20.zsav"))
M3_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_result_M3_2019_20.zsav"))
M4_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_result_M4_2019_20.zsav"))
M5_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_result_M5_2019_20.zsav"))
M6_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_result_M6_2019_20.zsav"))
M7_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_result_M7_2019_20.zsav"))
M8_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_result_M8_2019_20.zsav"))
M9_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_result_M9_2019_20.zsav"))
M10_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_result_M10_2019_20.zsav"))
M11_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_result_M11_2019_20.zsav"))
M12_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_result_M12_2019_20.zsav"))

M1_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_3m_cohort_M1_2021_22.zsav"))
M2_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_3m_cohort_M2_2021_22.zsav"))
M3_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_3m_cohort_M3_2021_22.zsav"))
M4_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_3m_cohort_M4_2021_22.zsav"))
M5_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_3m_cohort_M5_2021_22.zsav"))
M6_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_3m_cohort_M6_2021_22.zsav"))
M7_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_3m_cohort_M7_2021_22.zsav"))
M8_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_3m_cohort_M8_2021_22.zsav"))
M9_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_3m_cohort_M9_2021_22.zsav"))
M10_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_3m_cohort_M10_2020_21.zsav"))
M11_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_3m_cohort_M11_2020_21.zsav"))
M12_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_3m_cohort_M12_2020_21.zsav"))

total_result <- bind_rows(                
          M1_2020_21,              
          M10_2020_21,             
          M11_2020_21,              
          M12_2020_21,               
          M2_2020_21,            
          M3_2020_21,            
          M4_2020_21,           
          M6_2020_21,           
          M7_2020_21,         
          M8_2020_21,        
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

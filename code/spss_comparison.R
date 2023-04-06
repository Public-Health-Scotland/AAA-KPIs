library(haven)
library(arsenal)
# file path
aaa_temp <- (paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/temp/KPIs/KPI1.4a - KPI1.4b/"))


kpi_1_4_12m_total <- read_sav(paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/temp/KPIs/KPI1.4a - KPI1.4b/KPI1.4_12m_total.zsav"))

kpi_1_4_12m_final <- read_sav(paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/temp/KPIs/KPI1.4a - KPI1.4b/KPI1.4_12m_final.zsav"))

kpi_1_4_12m_finalresult <- read_sav(paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/temp/KPIs/KPI1.4a - KPI1.4b/KPI1.4_12m_FinalResult.zsav"))


# combine 12m cohort monthly scripts
M1_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_cohort_M1_2019_20.zsav"))
M2_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_cohort_M2_2019_20.zsav"))
M3_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_cohort_M3_2019_20.zsav"))
M4_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_cohort_M4_2019_20.zsav"))
M5_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_cohort_M5_2019_20.zsav"))
M6_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_cohort_M6_2019_20.zsav"))
M7_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_cohort_M7_2019_20.zsav"))
M8_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_cohort_M8_2019_20.zsav"))
M9_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_cohort_M9_2019_20.zsav"))
M10_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_cohort_M10_2019_20.zsav"))
M11_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_cohort_M11_2019_20.zsav"))
M12_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_cohort_M12_2019_20.zsav"))

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

combined_12m_cohort <- bind_rows(                
  M1_2019_20,                
  M1_2020_21,
  M10_2019_20,              
  M10_2020_21,
  M11_2019_20,             
  M11_2020_21,
  M12_2019_20,              
  M12_2020_21,
  M2_2019_20,               
  M2_2020_21,
  M3_2019_20,             
  M3_2020_21,
  M4_2019_20,            
  M4_2020_21,
  M5_2019_20,           
  M5_2020_21,
  M6_2019_20,            
  M6_2020_21,
  M7_2019_20,           
  M7_2020_21,
  M8_2019_20,          
  M8_2020_21,
  M9_2019_20,         
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

# combine 12m exclerator monthly scripts
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

combined_12m_exclusions <- bind_rows(                
  M1_2019_20,                
  M1_2020_21,
  M10_2019_20,              
  M10_2020_21,
  M11_2019_20,             
  M11_2020_21,
  M12_2019_20,              
  M12_2020_21,
  M2_2019_20,               
  M2_2020_21,
  M3_2019_20,             
  M3_2020_21,
  M4_2019_20,            
  M4_2020_21,
  M5_2019_20,           
  M5_2020_21,
  M6_2019_20,            
  M6_2020_21,
  M7_2019_20,           
  M7_2020_21,
  M8_2019_20,          
  M8_2020_21,
  M9_2019_20,         
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

# combine 12m numerator monthly scripts
M1_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_num_M1_2019_20.zsav"))
M2_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_num_M2_2019_20.zsav"))
M3_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_num_M3_2019_20.zsav"))
M4_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_num_M4_2019_20.zsav"))
M5_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_num_M5_2019_20.zsav"))
M6_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_num_M6_2019_20.zsav"))
M7_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_num_M7_2019_20.zsav"))
M8_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_num_M8_2019_20.zsav"))
M9_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_num_M9_2019_20.zsav"))
M10_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_num_M10_2019_20.zsav"))
M11_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_num_M11_2019_20.zsav"))
M12_2019_20 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_num_M12_2019_20.zsav"))

M1_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_num_M1_2020_21.zsav"))
M2_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_num_M2_2020_21.zsav"))
M3_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_num_M3_2020_21.zsav"))
M4_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_num_M4_2020_21.zsav"))
M5_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_num_M5_2020_21.zsav"))
M6_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_num_M6_2020_21.zsav"))
M7_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_num_M7_2020_21.zsav"))
M8_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_num_M8_2020_21.zsav"))
M9_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_num_M9_2020_21.zsav"))
M10_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_num_M10_2020_21.zsav"))
M11_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_num_M11_2020_21.zsav"))
M12_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_num_M12_2020_21.zsav"))

combined_12m_numerator <- bind_rows(                
  M1_2019_20,                
  M1_2020_21,
  M10_2019_20,              
  M10_2020_21,
  M11_2019_20,             
  M11_2020_21,
  M12_2019_20,              
  M12_2020_21,
  M2_2019_20,               
  M2_2020_21,
  M3_2019_20,             
  M3_2020_21,
  M4_2019_20,            
  M4_2020_21,
  M5_2019_20,           
  M5_2020_21,
  M6_2019_20,            
  M6_2020_21,
  M7_2019_20,           
  M7_2020_21,
  M8_2019_20,          
  M8_2020_21,
  M9_2019_20,         
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

# Compare annual cohort

combined_12m_cohort_slim <- combined_12m_cohort %>% 
  select(UPI,c_scrn_date) %>% 
  rename(upi_ac = UPI,
         date_screen_ac = c_scrn_date) %>% 
  arrange(upi_ac,date_screen_ac)

annual_surveillance_cohort_slim <- annual_surveillance_cohort %>% 
  select(upi_ac,date_screen_ac) %>% 
  arrange(upi_ac,date_screen_ac)

comparedf(combined_12m_cohort_slim,annual_surveillance_cohort_slim)
rm(combined_12m_cohort_slim,annual_surveillance_cohort_slim)

# matched in arsenal

# Compare exclusions

combined_12m_exclusions_slim <- combined_12m_exclusions %>% 
  select(UPI,Start_Date) %>% 
  rename(upi = UPI,
         date_start = Start_Date) %>% 
  arrange(upi,date_start)

exclusions_appointments_slim <- exclusions_appointments %>% 
  select(upi,date_start) %>% 
  arrange(upi,date_start)

comparedf(combined_12m_exclusions_slim,exclusions_appointments_slim)

rm(combined_12m_exclusions_slim,exclusions_appointments_slim)

# matched in arsenal

# Compare numerator files

combined_12m_numerator_slim <- combined_12m_numerator %>% 
  select(UPI,c_scrn_date,Scrn_Date) %>% 
  rename(upi = UPI,
         date_screen_ac = c_scrn_date,
         date_screen_sc = Scrn_Date) %>% 
  arrange(upi,date_screen_ac,date_screen_sc)

follow_up_appointments_slim <- follow_up_appointments %>% 
  select(upi,date_screen_ac,date_screen_sc) %>% 
  arrange(upi,date_screen_ac,date_screen_sc)

comparedf(combined_12m_numerator_slim,follow_up_appointments_slim)

rm(combined_12m_numerator_slim,follow_up_appointments_slim)

# matched in arsenal

# Compare final dataset
kpi_1_4_12m_final_slim <- kpi_1_4_12m_final %>% 
  select(UPI,c_scrn_date,Scrn_Date,Start_Date) %>% 
  rename(upi_ac = UPI,
         date_screen_ac.x = c_scrn_date,
         date_screen_sc = Scrn_Date,
         date_start = Start_Date) %>% 
  arrange(upi_ac,date_screen_ac.x,date_screen_sc,date_start)

final_follow_ups_slim <- final_follow_ups %>% 
  select(upi_ac,date_screen_ac.x,date_screen_sc,date_start) %>% 
  arrange(upi_ac,date_screen_ac.x,date_screen_sc,date_start)



comparedf(kpi_1_4_12m_final_slim,final_follow_ups_slim)

test <- final_follow_ups_slim %>% anti_join(kpi_1_4_12m_final_slim, 
                                         by = "upi_ac","date_screen_ac.x",
                                         "date_screen_sc","date_start")

test1 <- kpi_1_4_12m_final_slim %>% anti_join(final_follow_ups_slim, 
                                         by = "upi_ac","date_screen_ac.x",
                                         "date_screen_sc","date_start")

rm(kpi_1_4_12m_final_slim,final_follow_ups_slim)

exceptions <- test %>% 
  left_join(final_follow_ups, by = c("upi_ac","date_screen_ac.x",
            "date_screen_sc","date_start")) %>%
  select(upi_ac,date_screen_ac.x,date_screen_sc,
         date_start,pat_inelig,date_end,cohort_ac.x,
         exclusion,attend,attend,exclusion_flag,
         exclusion_flag) %>% 
  left_join(kpi_1_4_12m_final, by = c("upi_ac"="UPI","date_screen_ac.x" = "c_scrn_date",
                                      "date_screen_sc" = "Scrn_Date","date_start" = "Start_Date"))
  
  
  final_follow_ups %>% filter(upi_ac %in% c("0204550238",
                                                        "0404552072",
                                                        "0503556394",
                                                        "0609481150",
                                                        "0704540037",
                                                        "0805526374",
                                                        "1704513316",
                                                        "2008511170",
                                                        "2010515072",
                                                        "2208411072",
                                                        "2303541239",
                                                        "2408335191",
                                                        "2805496353",
                                                        "2910495191"
)) %>%
  select(upi_ac,date_screen_ac.x,date_screen_sc,
         date_start,pat_inelig,date_end,cohort_ac.x,
         exclusion,follow_up_screen_flag,attend,exclusion_flag,
         exclusion_flag)
  
  

exceptions <- final_follow_ups %>% filter(upi_ac %in% c("0204496594",
"0707421292",
"1506505198",
"1711505536",
"1905359071",
"2005471070",
"2212540051"
)) %>%
  select(upi_ac,date_screen_ac.x,date_screen_sc,
         date_start,pat_inelig,date_end,cohort_ac.x,
         exclusion,follow_up_screen_flag,attend,exclusion_flag,
         exclusion_flag,fy_due,hbres_final)


library(haven)

aaa_temp <- (paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/temp/KPIs/KPI1.4a - KPI1.4b/"))


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

M1_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_result_M1_2020_21.zsav"))
M2_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_result_M2_2020_21.zsav"))
M3_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_result_M3_2020_21.zsav"))
M4_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_result_M4_2020_21.zsav"))
M5_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_result_M5_2020_21.zsav"))
M6_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_result_M6_2020_21.zsav"))
M7_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_result_M7_2020_21.zsav"))
M8_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_result_M8_2020_21.zsav"))
M9_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_result_M9_2020_21.zsav"))
M10_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_result_M10_2020_21.zsav"))
M11_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_result_M11_2020_21.zsav"))
M12_2020_21 <- read_sav(paste0(aaa_temp,"KPI1.4_12m_result_M12_2020_21.zsav"))

files <- list(M1_2019_20,                
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


i = 0


for (file in files){
  rows <- nrow(file)
  i <- i+rows
  print(i)
}  

i

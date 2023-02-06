##########################################################
# 4.1_vascular_outcomes.R
# Salomi Barkat & Karen Hotopp
# 21/10/2022
# Script 1 of ?
# 
# Translation of SPSS file ' AAA Procedures'
# Part of Theme 4 for AAA KPIs
# Takes the processed BOXI extracts and creates tables detailing
# vascular surgery outcomes
# 
# Written/run on R Studio Server
# R version 3.6.1
##########################################################

# Description: number of AAA procedures carried out after being referred
# from screening programme, split by NHS Board


#### 1: Housekeeping ####
## Packages
library(here)
library(dplyr)
library(magrittr)
library(readr)
library(janitor)     ##for rounding 0.5 upwards
#library(expss)       ##for spss equvialent functions
#library(tidyr)       ##for reshaping data
library(openxlsx)
#library(writexl)     ##for saving excel files
#library(XLConnect)   ##for writing to excel templates (currently using this instead of openxlsx as openxlsx is overwritting the formatting)
library(tidylog)


rm(list = ls())


## Values
year <- 2022
month <- "09"
vas_cutoff <- "2022-03-31"
# most recent three years
output_years <- c("2019/20", "2020/21", "2021/22", "All years")


## Pathways
wd_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", year, month)

extract_path <- paste0("/PHI_conf/AAA/Topics/Screening/extracts",
                      "/", year, month)

template_path <- paste0("/PHI_conf/AAA/Topics/Screening/templates")

# location of Excel tables
table_output <- paste0("/PHI_conf/AAA/Topics/AAAScreening/Publications",
                       "/AAA Screening Programme Statistics/202203XX/Output/20211123 MEG")


#years for column titles in table
#fill in the first year to be shown, then other years will be automatically calculated from this
year1 <- 2019

########################################################################
### 2 - Produce totals----
########################################################################


#read in AAA extract and ensure all names are in standard format
extract <- readRDS(paste0(extract_path, "/output/aaa_extract_", 
                          year, month, ".rds")) %>% 
  glimpse()


#select if the date of surgery is in the required time-period
extract %<>%
  filter(date_surgery <= vas_cutoff,
         !is.na(date_referral_true) & aaa_size >= 5.5 & 
           surg_method %in% c("01", "02"))

#select referrals with AAA >=5.5 cm who had surgery 
#(nb there is a surgery recorded at AAA less than 5.5 which we don't want to include
#and not including aaa_surg_method '03' abandoned procedure)
data <- data %>%
  filter(!is.na(actual_referral_date) & largest_measurement>=5.5 & aaa_surg_method %in% c("01", "02"))

#check
table(extract$surg_method)
#262 EVAR, 286 open

#surgery method should only be recorded along result outcomes '15' (approp for surgery and 
#survived 30 days) or '16' (approp for surgery and died within 30 days) - check recordings
data %>% tabyl(result_outcome, aaa_surg_method)

#there is one EVAR recorded as other final outcome
#It's a lothian resident who was eventually operated on in GG&C. Once the board of surgery field is 
#added to vascular  module we will be asking board to change result outcome to 15 or 16 (and data will 
#be collated by board of surgery rather than board of residence so it will be counted under GG&C)

#October 2021, there are also:
#Other final outcome - 1 evar - looks like patient described above

#select result outcomes 15 and 16 only
data <- data %>%
  filter(result_outcome %in% c("15", "16"))

#check
fre(data$aaa_surg_method)
#261 EVAR, 286 open (matches KPI 4.1/4.2 underlying numbers)

#assign financial years to surgery dates
data <- data %>%
  mutate(yearsurg=(as.numeric(format(surgery_date, "%Y")) - (format(surgery_date, "%m") <= "03")))

#check
fre(data$yearsurg)

#add second half of financial year and join into a single new variable to show year in format '20xx/xx'
data <- data %>%
  mutate(year2=yearsurg+1) %>%   #create second half of financial year
  mutate(year3=paste0(yearsurg, "/", substr(year2, 3,4))) %>% #join first and second half of financial years
  select(-year2, -yearsurg) %>% #remove years which are not required
  rename(yearsurg=year3)  #rename to standard format

#check all years have been assigned
fre(data$yearsurg)


#add variables to count overall totals
data <- data %>%
  mutate(allyears="All years") %>%
  mutate(allsurg="All Types") %>%
  mutate(scotland="Scotland")

#function to produce totals by various groups

totals <- function(df, surg, year, geog) {
  
  data <- data %>%
    group_by({{surg}}, {{year}}, {{geog}}) %>%
    summarise(total=n()) %>%
    rename(geography={{geog}}, surg_type={{surg}}, year={{year}})
  
}

#run totals for each combination of surgery type/years/hb
temp1 <- totals(year=yearsurg, surg=aaa_surg_method, geog=hb_res_name)
temp2 <- totals(year=allyears, surg=aaa_surg_method, geog=hb_res_name)
temp3 <- totals(year=allyears, surg=allsurg, geog=hb_res_name)
temp4 <- totals(year=yearsurg, surg=allsurg, geog=hb_res_name)
temp5 <- totals(year=yearsurg, surg=aaa_surg_method, geog=scotland)
temp6 <- totals(year=allyears, surg=aaa_surg_method, geog=scotland)
temp7 <- totals(year=allyears, surg=allsurg, geog=scotland)
temp8 <- totals(year=yearsurg, surg=allsurg, geog=scotland)


#add together cumulative and individual totals
combined <- bind_rows(temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8)

#remove datasets which are no longer required
rm(temp1, temp2, temp3, temp4, temp5, temp6, temp7, temp8)

#rename procedures so that it is clear which is which ahead of next step
combined <- combined %>%
  mutate(surg_type=case_when(
    surg_type == "01" ~  "EVAR",
    surg_type == "02" ~ "Open", 
    surg_type == "03" ~ "Abandoned",
    TRUE ~ as.character(surg_type)
  ))

#reshape data to format required
combined <- combined %>%
  pivot_wider(names_from = c(year, surg_type), values_from = total, values_fill = 0)

#sort in order of hbs (rename scotland to get it to show at the top)
combined <- combined %>%
  mutate(geography=ifelse(geography=="Scotland", "aa_Scotland", geography)) %>%
  arrange(geography) %>%
  mutate(geography=ifelse(geography=="aa_Scotland", "Scotland", geography))


#save file with all years of data
write_xlsx(combined, paste0(output, "AAA repair numbers by type.xlsx"))


#select years to show in table
combined <- combined %>%
  select(geography, contains(years_to_show))

#reorder columns so that open appears before evar for each year
combined <- combined %>%
  select(1,3,2,4,6,5,7,9,8,10,12,11,13)


########################################################################
### 3 - Write to excel----
########################################################################

#load in template containing excel tables
tables <- loadWorkbook(template)
setStyleAction(tables,XLC$"STYLE_ACTION.NONE") #preserve existing formatting


#write data to relevant cells in table
writeWorksheet(tables, combined, "AAA Repairs", startRow=7, startCol=2, header=FALSE)


#update years in table column titles
#year 1
writeWorksheet(tables, paste0("Year ending 31 March ", year1), "AAA Repairs", startRow=5, startCol=3, header=FALSE)

#year 2
writeWorksheet(tables, paste0("Year ending 31 March ", (year1)+1), "AAA Repairs", startRow=5, startCol=6, header=FALSE)

#year 3
writeWorksheet(tables, paste0("Year ending 31 March ", (year1)+2), "AAA Repairs", startRow=5, startCol=9, header=FALSE)

#cumulative
writeWorksheet(tables, paste0("Cumulative total from implementation to 31 March ", (year1)+2), "AAA Repairs", startRow=5, startCol=12, header=FALSE)


#save
saveWorkbook(tables, paste0(table_output, "4. Referral for assessment - Treatment & Outcomes.xlsx"))


############end of script############








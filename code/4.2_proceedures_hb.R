##########################################################
# 4.2_proceedures_hb.R
# Salomi Barkat & Karen Hotopp
# 21/10/2022
# Script 1 of ?
# 
# Translation of SPSS file 'AAA Procedures'
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
library(phsmethods)
library(janitor)     ##for rounding 0.5 upwards
#library(expss)       ##for spss equvialent functions
#library(tidyr)       ##for reshaping data
library(openxlsx)
#library(writexl)     ##for saving excel files
#library(XLConnect)   ##for writing to excel templates (currently using this 
#instead of openxlsx as openxlsx is overwritting the formatting)
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

##!! Is this needed?? (redundant to output_years?)
#years for column titles in table
#fill in the first year to be shown, then other years will be automatically calculated from this
year1 <- 2019


#### 2: Format Data ####
extract <- readRDS(paste0(extract_path, "/output/aaa_extract_", 
                          year, month, ".rds")) %>% 
  #select(hbres, result_outcome, date_referral_true, date_surgery, fy_surgery, surg_method, ) %>%  
  ## Add relevant variables, as don't need whole data set
  glimpse()


## Set dates
extract %<>%
  filter(date_surgery <= vas_cutoff,
         # select referrals
         !is.na(date_referral_true) & aaa_size >= 5.5 & 
           surg_method %in% c("01", "02"))

table(extract$surg_method, useNA = "ifany")
# 296 EVAR, 335 open


####
## Where surg_method has response, result_outcome should only have: 
## 15 (approp for surgery and survived 30 days) or 
## 16 (approp for surgery and died within 30 days)
table(extract$result_outcome, extract$surg_method)

# Feb 2023
#     01  02
# 15 294 328
# 16   1   7
# 20   1   0

## One record has result_outcome == 20 (other final outcome)
check <- extract[extract$result_outcome == "20",]
## FY 2016/17
## Lothian resident who was eventually operated on in GG&C. 
## Once the board of surgery field is added to vascular module we will be 
## asking board to change result outcome to 15 or 16 (and data will be 
## collated by board of surgery rather than board of residence so it will be 
## counted under GG&C)
####


## Appropriate for surgery records
extract %<>%
  filter(result_outcome %in% c("15", "16"))

table(extract$surg_method)
# 295 EVAR, 335 open (matches KPI 4.1/4.2 additional numbers) 
##!! How easy is it to automate this check?? Check against outputs from 
##!! 2. 1, 3, 5, year mortality.sps (CP translating)


##!! This section adds FY numbers for date_surgery (YEAR OF SURGERY)
## TO BE MOVED TO QUARTERLY EXTRACTS PROCESS
## Create financial year from surgery date
extract %<>%
  mutate(fy_surgery = extract_fin_year(date_surgery)) %>%
  mutate(fy_surgery = forcats::fct_relevel(fy_surgery,
                                           c("2012/13", "2013/14", "2014/15",
                                             "2015/16", "2016/17", "2017/18",
                                             "2018/19", "2019/20", "2020/21",
                                             "2021/22"))) %>%
  relocate(fy_surgery, .after=date_surgery) %>%
  glimpse()

table(extract$fy_surgery)


#### 3: Create Tables ####
test <- extract %>% 
  select(hbres, result_outcome, date_referral_true, 
         date_surgery, fy_surgery, surg_method) %>% 
  group_by(hbres, fy_surgery, surg_method) %>% 
  glimpse()


#add variables to count overall totals
quarter <- extract %>%
  mutate(allyears="All years") %>%
  mutate(allsurg="All Types") %>%
  mutate(scotland="Scotland")

# #function to produce totals by various groups
# totals <- function(df, surg, year, geog) {
#   
#   data <- data %>%
#     group_by({{surg}}, {{year}}, {{geog}}) %>%
#     summarise(total=n()) %>%
#     rename(geography={{geog}}, surg_type={{surg}}, year={{year}})
#   
# }
# 
# #run totals for each combination of surgery type/years/hb
# temp1 <- totals(df=quarter, year=fy_surgery, surg=surg_method, geog=hbres)
# 
# 
# temp1 <- totals(year=yearsurg, surg=aaa_surg_method, geog=hb_res_name)
# temp2 <- totals(year=allyears, surg=aaa_surg_method, geog=hb_res_name)
# temp3 <- totals(year=allyears, surg=allsurg, geog=hb_res_name)
# temp4 <- totals(year=yearsurg, surg=allsurg, geog=hb_res_name)
# temp5 <- totals(year=yearsurg, surg=aaa_surg_method, geog=scotland)
# temp6 <- totals(year=allyears, surg=aaa_surg_method, geog=scotland)
# temp7 <- totals(year=allyears, surg=allsurg, geog=scotland)
# temp8 <- totals(year=yearsurg, surg=allsurg, geog=scotland)


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

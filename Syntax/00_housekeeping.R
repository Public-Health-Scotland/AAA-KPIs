#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1.DNA-excluded-surveillance.R
# Eibhlin O'Sullivan
# Jan 2022
# Define housekeeping variables used by subsequent scripts
# Written/run on R Studio Server
# R version 3.6.1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### 1 - Housekeeping ----

# Import libraries

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  dplyr,
  magrittr,
  phsmethods,
  lubridate,
  janitor,
  tidyr,
  arsenal,
  openxlsx,
  here
)

# Define dates 
prev_year <- "2019/20"
current_year <- "2020/21"
next_year <- "2021/22"
current_year_start <- "2020-03-01"
next_year_start <- "2021-03-01"
cut_off_date <- "2022-03-31"
financial_year_due <- "2021/22"
financial_quarters <- c("2020/21_4","2021/22_1","2021/22_2","2021/22_3")

# Define filepaths

aaa_extracts_path <- (paste0("/PHI_conf/AAA/Topics/Screening/extracts/202209/output/"))
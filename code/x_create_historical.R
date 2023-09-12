# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x_create_historical.R
# 
# Karen Hotopp
# Sept 2023
# 
# Create database of historical published figures for AAA KPIs
# 
# Written/run on Posit WB, R 4.1.2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### 1: Housekeeping
library(dplyr)
library(openxlsx)
library(stringr)
library(forcats)
library(tidylog)


## Values




## File paths
pub_path <- "/PHI_conf/AAA/Topics/Screening/publications/Completed/"
kpi_path <- "/PHI_conf/AAA/Topics/Screening/KPI/"
temp_path <- "/temp/KPIs/KPI1.1 - KPI1.3"


### KPI 1.1 ----
# Call in published figures for history of program
# For now, start with two previous year to 2022/23

## 2020/21
aaa_2021 <- read.xlsx(paste0(pub_path, 20220301, temp_path, 
                             "/KPI_1.1_overall.xlsx"), 
                      cols = 2:5, rows = c(1,3:17))

## 2021/22
aaa_2122 <- read.xlsx(paste0(kpi_path, 202209, temp_path,
                             "/KPI_1.1_overall.xlsx"), 
                      cols = 2:5, rows = c(1,3:17))

## Combine and clean
aaa_1.1 <- left_join(aaa_2021, aaa_2122, by = "X1") |> 
  mutate(X1 = str_remove(X1, 'xml:space="preserve">'),
         X1 = str_replace_all(X1, "&amp;", "&"),
         X1 = str_replace(X1, "AS", "S")) |> 
  mutate(X1 = fct_relevel(X1, c("Scotland", "Ayrshire & Arran", "Borders",
                                "Dumfries & Galloway", "Fife", "Forth Valley",
                                "Grampian", "Greater Glasgow & Clyde", "Highland",
                                "Lanarkshire", "Lothian", "Orkney", "Shetland",
                                "Tayside", "Western Isles")))

names(aaa_1.1) <- c("hbres", "cohort_20_21_n", "cohort_20_21_offer", 
                    "cohort_20_21_p", "cohort_21_22_n", 
                    "cohort_21_22_offer", "cohort_21_22_p")
















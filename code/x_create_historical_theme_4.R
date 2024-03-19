# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x_create_historical_theme_4.R
# 
# Karen Hotopp
# Oct 2023
# 
# Create database of historical published figures for AAA KPIs
# 
# Written/run on Posit WB, R 4.1.2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Notes:
# Call in published figures for history of program
# For now, start with two previous year to 2022/23


#### 1: Housekeeping ----
library(dplyr)
library(openxlsx)
library(stringr)
library(forcats)
library(readr)
library(tidylog)

rm(list=ls())
gc()


## Values
names_kpi3 <- c("hb", "FY_2012/13_cohort_n", "FY_2012/13_seen_n", "FY_2012/13_cover_p", 
                "FY_2013/14_cohort_n", "FY_2013/14_seen_n", "FY_2013/14_cover_p",
                "FY_2014/15_cohort_n", "FY_2014/15_seen_n", "FY_2014/15_cover_p", 
                "FY_2015/16_cohort_n", "FY_2015/16_seen_n", "FY_2015/16_cover_p",
                "FY_2016/17_cohort_n", "FY_2016/17_seen_n", "FY_2016/17_cover_p", 
                "FY_2017/18_cohort_n", "FY_2017/18_seen_n", "FY_2017/18_cover_p", 
                "FY_2018/19_cohort_n", "FY_2018/19_seen_n", "FY_2018/19_cover_p", 
                "FY_2019/20_cohort_n", "FY_2019/20_seen_n", "FY_2019/20_cover_p", 
                "FY_2020/21_cohort_n", "FY_2020/21_seen_n", "FY_2020/21_cover_p", 
                "FY_2021/22_cohort_n", "FY_2021/22_seen_n", "FY_2021/22_cover_p")

## File paths
pub_path <- "/PHI_conf/AAA/Topics/Screening/publications/Completed/"
kpi_path <- "/PHI_conf/AAA/Topics/Screening/KPI/"
temp_path <- "/temp/KPIs/KPI3.1 - KPI4.2"
# temp_supp_path <- "/temp/Supplementary/Coverage"
# temp_add_path <- "/temp/Additional Tables to KPIs/Excel"


#### 2: Create historical files ----
### KPI 3.1 ----
## Health Board of Residence
aaa_3.1 <- read.xlsx(paste0(kpi_path, 202209, temp_path, "/KPI_3.1_20220930.xlsx"), 
                      cols = 2:32, rows = c(3,5:19))

names(aaa_3.1) <- names_kpi3
names(aaa_3.1)

aaa_3.1 <- aaa_3.1 |> 
  mutate(hb = str_remove(hb, 'xml:space="preserve">'),
         hb = str_replace_all(hb, "&amp;", "&"),
         hb = str_replace(hb, "All participating boards", "Scotland")) |>
  pivot_longer(!hb, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year) |> 
  # clean variable levels
  mutate(fin_year = str_remove(fin_year, "FY_"),
         fin_year = str_remove(fin_year, "_cohort_n"),
         fin_year = str_remove(fin_year, "_seen_n"),
         fin_year = str_remove(fin_year, "_cover_p"),
         group = case_when(str_detect(group, "_cohort") ~ "cohort_n",
                           str_detect(group, "_seen") ~ "seen_n",
                           str_detect(group, "_cover") ~ "cover_p"),
         kpi = "KPI 3.1 HB Residence", .after = hb) |>
  glimpse()



### KPI 3.2 ----
## Health Board of Residence
aaa_3.2 <- read.xlsx(paste0(kpi_path, 202209, temp_path, "/KPI_3.2_20220930.xlsx"), 
                     cols = 2:32, rows = c(3,5:19))

names(aaa_3.2) <- names_kpi3
names(aaa_3.2)

aaa_3.2 <- aaa_3.2 |> 
  mutate(hb = str_remove(hb, 'xml:space="preserve">'),
         hb = str_replace_all(hb, "&amp;", "&"),
         hb = str_replace(hb, "All participating boards", "Scotland")) |>
  pivot_longer(!hb, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year) |> 
  # clean variable levels
  mutate(fin_year = str_remove(fin_year, "FY_"),
         fin_year = str_remove(fin_year, "_cohort_n"),
         fin_year = str_remove(fin_year, "_seen_n"),
         fin_year = str_remove(fin_year, "_cover_p"),
         group = case_when(str_detect(group, "_cohort") ~ "cohort_n",
                           str_detect(group, "_seen") ~ "surgery_n",
                           str_detect(group, "_cover") ~ "cover_p"),
         kpi = "KPI 3.2 HB Residence", .after = hb) |>
  glimpse()


## Health Board of Surgery
# This will need to be created fresh, as the HB of surgery is new in 2022/23.
# To be created as 2022/23 KPI 3.2 is analyzed and then saved to historic.


#### 3: Check and combine ----
names(aaa_3.1)
names(aaa_3.2)

aaa_kpi_historic <- rbind(aaa_3.1, aaa_3.2) |> 
  rename(hbres = hb) |> 
  mutate(hbres = fct_relevel(hbres, c("Scotland", "Ayrshire & Arran", "Borders",
                                      "Dumfries & Galloway", "Fife", "Forth Valley",
                                      "Grampian", "Greater Glasgow & Clyde", 
                                      "Highland", "Lanarkshire", "Lothian", "Orkney", 
                                      "Shetland", "Tayside", "Western Isles"))) |> 
  select(hbres, kpi, fin_year, group, value)

table(aaa_kpi_historic$kpi)
table(aaa_kpi_historic$kpi, aaa_kpi_historic$fin_year)


#### 4: Write out ----
write_rds(aaa_kpi_historic, paste0(kpi_path, 
                                   "historical/aaa_kpi_historical_theme4.rds"))
# change permissions to give the group read/write
Sys.chmod(paste0(kpi_path, "historical/aaa_kpi_historical_theme4.rds"),
          mode = "664", use_umask = FALSE)

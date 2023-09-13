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
library(tidyr)
library(forcats)
library(tidylog)


## Values




## File paths
pub_path <- "/PHI_conf/AAA/Topics/Screening/publications/Completed/"
kpi_path <- "/PHI_conf/AAA/Topics/Screening/KPI/"
temp_path <- "/temp/KPIs/KPI1.1 - KPI1.3"
temp_supp_path <- "/temp/Supplementary/Coverage"
temp_2_path <- "/temp/2. Invitation and Attendance"



### KPI 1.1 ----
# Call in published figures for history of program
# For now, start with two previous year to 2022/23

## 2020/21
aaa_2021 <- read.xlsx(paste0(pub_path, 20220301, temp_path, 
                             "/KPI_1.1_overall.xlsx"), 
                      cols = 2:5, rows = c(1,3:17)) |> 
  pivot_longer(!X1, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year)

## 2021/22
aaa_2122 <- read.xlsx(paste0(kpi_path, 202209, temp_path,
                             "/KPI_1.1_overall.xlsx"), 
                      cols = 2:5, rows = c(1,3:17)) |> 
  pivot_longer(!X1, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year)

## Combine and clean
aaa_1.1 <- rbind(aaa_2021, aaa_2122) |> 
  mutate(X1 = str_remove(X1, 'xml:space="preserve">'),
         X1 = str_replace_all(X1, "&amp;", "&"),
         X1 = str_replace(X1, "AS", "S")) |> 
  rename(hbres = X1) |> 
  mutate(fin_year = case_when(str_detect(fin_year, "FY2020_21") ~ "2020/21",
                              str_detect(fin_year, "FY2021_22") ~ "2021/22"),
         group = case_when(str_detect(group, "_c") ~ "cohort_n",
                           str_detect(group, "_offer") ~ "offer_n",
                           str_detect(group, "p_") ~ "coverage_p"),
         kpi = "KPI 1.1", .after = hbres) |> 
  glimpse()


### KPI 1.2a ----
## 2020/21
aaa_2021 <- read.xlsx(paste0(pub_path, 20220301, temp_supp_path, 
                             "/Coverage_202021_202122.xlsx"), 
                      cols = 2:5, rows = c(1,3:17)) |> 
  pivot_longer(!X1, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year)

## 2021/22
aaa_2122 <- read.xlsx(paste0(kpi_path, 202209, temp_2_path,
                             "/Coverage_202122_202223.xlsx"), 
                      cols = 2:5, rows = c(1,3:17)) |> 
  pivot_longer(!X1, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year)

## Combine and clean
aaa_1.2a <- rbind(aaa_2021, aaa_2122) |> 
  mutate(X1 = str_remove(X1, 'xml:space="preserve">'),
         X1 = str_replace_all(X1, "&amp;", "&"),
         X1 = str_replace(X1, "AS", "S")) |> 
  rename(hbres = X1) |> 
  mutate(fin_year = case_when(str_detect(fin_year, "FY2020_21") ~ "2020/21",
                              str_detect(fin_year, "FY2021_22") ~ "2021/22"),
         group = case_when(str_detect(group, "_tested") ~ "test_n",
                           str_detect(group, "_c") ~ "cohort_n",
                           str_detect(group, "p_") ~ "coverage_p"),
         kpi = "KPI 1.2a", .after = hbres) |> 
  glimpse()


### KPI 1.2b ----
## 2020/21
aaa_2021 <- read.xlsx(paste0(pub_path, 20220301, temp_path, 
                             "/KPI_1.2_overall.xlsx"), 
                      cols = 2:5, rows = c(1,3:17)) |> 
  pivot_longer(!X1, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year)

## 2021/22
aaa_2122 <- read.xlsx(paste0(kpi_path, 202209, temp_path,
                             "/KPI_1.2_overall.xlsx"), 
                      cols = 2:5, rows = c(1,3:17)) |> 
  pivot_longer(!X1, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year)

## Combine and clean
aaa_1.2b <- rbind(aaa_2021, aaa_2122) |> 
  mutate(X1 = str_remove(X1, 'xml:space="preserve">'),
         X1 = str_replace_all(X1, "&amp;", "&"),
         X1 = str_replace(X1, "AS", "S")) |> 
  rename(hbres = X1) |> 
  mutate(fin_year = case_when(str_detect(fin_year, "FY2020_21") ~ "2020/21",
                              str_detect(fin_year, "FY2021_22") ~ "2021/22"),
         group = case_when(str_detect(group, "_tested") ~ "test_n",
                           str_detect(group, "_offer") ~ "offer_n",
                           str_detect(group, "p_") ~ "uptake_p"),
         kpi = "KPI 1.2b", .after = hbres) |> 
  glimpse()


### KPI 1.2: Coverage by 1Sept ----
## 2020/21
aaa_2021 <- read.xlsx(paste0(pub_path, 20220301, temp_supp_path, 
                             "/Coverage_202021_202122.xlsx"), 
                      cols = 2:5, rows = c(19,21:35)) |> 
  pivot_longer(!X1, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year)

## 2021/22
aaa_2122 <- read.xlsx(paste0(kpi_path, 202209, temp_2_path,
                             "/Coverage_202122_202223.xlsx"), 
                      cols = 2:5, rows = c(19,21:35)) |> 
  pivot_longer(!X1, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year)

## Combine and clean
aaa_1.2_Sept <- rbind(aaa_2021, aaa_2122) |> 
  mutate(X1 = str_remove(X1, 'xml:space="preserve">'),
         X1 = str_replace_all(X1, "&amp;", "&"),
         X1 = str_replace(X1, "AS", "S")) |> 
  rename(hbres = X1) |> 
  mutate(fin_year = case_when(str_detect(fin_year, "FY2020_21") ~ "2020/21",
                              str_detect(fin_year, "FY2021_22") ~ "2021/22"),
         group = case_when(str_detect(group, "_tested") ~ "test_n",
                           str_detect(group, "_c") ~ "cohort_n",
                           str_detect(group, "p_") ~ "coverage_p"),
         kpi = "KPI 1.2 Sept coverage", .after = hbres) |> 
  glimpse()


### KPI 1.3a ----
## 2020/21
aaa_2021 <- read.xlsx(paste0(pub_path, 20220301, temp_supp_path, 
                             "/coverage_simd_202021_202122.xlsx"), 
                      cols = c(2,4:7), rows = c(1,3:107)) |> 
  fill(X1, .direction = "down") |> 
  pivot_longer(!X1:X2, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year)

## 2021/22
aaa_2122 <- read.xlsx(paste0(kpi_path, 202209, temp_2_path,
                             "/coverage_simd_202122_202223.xlsx"), 
                      cols = c(2,4:7), rows = c(1,3:107)) |> 
  fill(X1, .direction = "down") |> 
  pivot_longer(!X1:X2, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year)

## Combine and clean
aaa_1.3a <- rbind(aaa_2021, aaa_2122) |> 
  mutate(X1 = str_remove(X1, 'xml:space="preserve">'),
         X1 = str_replace_all(X1, "&amp;", "&"),
         X1 = str_replace(X1, "AS", "S")) |> 
  rename(hbres = X1,
         simd = X2) |> 
  mutate(simd = case_when(str_detect(simd, "0") ~ "Total",
                          str_detect(simd, "1") ~ "1",
                          str_detect(simd, "2") ~ "2",
                          str_detect(simd, "3") ~ "3",
                          str_detect(simd, "4") ~ "4",
                          str_detect(simd, "5") ~ "5",
                          str_detect(simd, "Unknown") ~ "Unknown"),
         fin_year = case_when(str_detect(fin_year, "FY2020_21") ~ "2020/21",
                              str_detect(fin_year, "FY2021_22") ~ "2021/22"),
         group = case_when(str_detect(group, "_tested") ~ "test_n",
                           str_detect(group, "_c") ~ "cohort_n",
                           str_detect(group, "p_") ~ "coverage_p"),
         kpi = "KPI 1.3a", .after = hbres) |> 
  glimpse()


### KPI 1.3b ----
## 2020/21
aaa_2021 <- read.xlsx(paste0(pub_path, 20220301, temp_path, 
                             "/KPI_1.3_overall.xlsx"), 
                      cols = c(2,4:7), rows = c(1,3:107)) |> 
  fill(X1, .direction = "down") |> 
  pivot_longer(!X1:X2, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year)

## 2021/22
aaa_2122 <- read.xlsx(paste0(kpi_path, 202209, temp_path, 
                             "/KPI_1.3_overall.xlsx"), 
                      cols = c(2,4:7), rows = c(1,3:107)) |> 
  fill(X1, .direction = "down") |> 
  pivot_longer(!X1:X2, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year)

## Combine and clean
aaa_1.3b <- rbind(aaa_2021, aaa_2122) |> 
  mutate(X1 = str_remove(X1, 'xml:space="preserve">'),
         X1 = str_replace_all(X1, "&amp;", "&"),
         X1 = str_replace(X1, "AS", "S")) |> 
  rename(hbres = X1,
         simd = X2) |> 
  mutate(simd = case_when(str_detect(simd, "0") ~ "Total",
                          str_detect(simd, "1") ~ "1",
                          str_detect(simd, "2") ~ "2",
                          str_detect(simd, "3") ~ "3",
                          str_detect(simd, "4") ~ "4",
                          str_detect(simd, "5") ~ "5",
                          str_detect(simd, "Unknown") ~ "Unknown"),
         fin_year = case_when(str_detect(fin_year, "FY2020_21") ~ "2020/21",
                              str_detect(fin_year, "FY2021_22") ~ "2021/22"),
         group = case_when(str_detect(group, "_tested") ~ "test_n",
                           str_detect(group, "offer") ~ "offer_n",
                           str_detect(group, "p_") ~ "uptake_p"),
         kpi = "KPI 1.3b", .after = hbres) |> 
  glimpse()


### KPI 1.3b additional ----
## 2020/21
aaa_2021 <- read.xlsx(paste0(pub_path, 20220301, temp_supp_path, 
                             "/coverage_boardlevelsimd_202021_202122.xlsx"), ### START HERE
                      cols = c(2,4:7), rows = c(1,3:107)) |> 
  fill(X1, .direction = "down") |> 
  pivot_longer(!X1:X2, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year)

## 2021/22
aaa_2122 <- read.xlsx(paste0(kpi_path, 202209, temp_path, 
                             "/KPI_1.3_overall.xlsx"), 
                      cols = c(2,4:7), rows = c(1,3:107)) |> 
  fill(X1, .direction = "down") |> 
  pivot_longer(!X1:X2, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year)

## Combine and clean
aaa_1.3b_add <- rbind(aaa_2021, aaa_2122) |> 
  mutate(X1 = str_remove(X1, 'xml:space="preserve">'),
         X1 = str_replace_all(X1, "&amp;", "&"),
         X1 = str_replace(X1, "AS", "S")) |> 
  rename(hbres = X1,
         simd = X2) |> 
  mutate(simd = case_when(str_detect(simd, "0") ~ "Total",
                          str_detect(simd, "1") ~ "1",
                          str_detect(simd, "2") ~ "2",
                          str_detect(simd, "3") ~ "3",
                          str_detect(simd, "4") ~ "4",
                          str_detect(simd, "5") ~ "5",
                          str_detect(simd, "Unknown") ~ "Unknown"),
         fin_year = case_when(str_detect(fin_year, "FY2020_21") ~ "2020/21",
                              str_detect(fin_year, "FY2021_22") ~ "2021/22"),
         group = case_when(str_detect(group, "_tested") ~ "test_n",
                           str_detect(group, "offer") ~ "offer_n",
                           str_detect(group, "p_") ~ "uptake_p"),
         kpi = "KPI 1.3b additional", .after = hbres) |> 
  glimpse()





###



















#|> 
  mutate(X1 = fct_relevel(X1, c("Scotland", "Ayrshire & Arran", "Borders",
                                "Dumfries & Galloway", "Fife", "Forth Valley",
                                "Grampian", "Greater Glasgow & Clyde", "Highland",
                                "Lanarkshire", "Lothian", "Orkney", "Shetland",
                                "Tayside", "Western Isles")))






















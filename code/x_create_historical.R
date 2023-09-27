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

## Notes:
# Call in published figures for history of program
# For now, start with two previous year to 2022/23


#### 1: Housekeeping ----
library(dplyr)
library(openxlsx)
library(stringr)
library(tidyr)
library(forcats)
library(readr)
library(tidylog)

rm(list=ls())
gc()

## Values



## File paths
pub_path <- "/PHI_conf/AAA/Topics/Screening/publications/Completed/"
kpi_path <- "/PHI_conf/AAA/Topics/Screening/KPI/"
temp_path <- "/temp/KPIs/KPI1.1 - KPI1.3"
temp_supp_path <- "/temp/Supplementary/Coverage"
temp_2_path <- "/temp/2. Invitation and Attendance"
temp_add_path <- "/temp/Additional Tables to KPIs/Excel"
temp_4_path <- "/temp/KPIs/KPI1.4a - KPI1.4b"

#### 2: Create historical files ----
### KPI 1.1 ----
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
  mutate(simd = NA, .after = kpi) |> 
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
  mutate(simd = NA, .after = kpi) |> 
  glimpse()


### KPI 1.2a: Coverage by 1Sept ----
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
aaa_1.2a_Sept <- rbind(aaa_2021, aaa_2122) |> 
  mutate(X1 = str_remove(X1, 'xml:space="preserve">'),
         X1 = str_replace_all(X1, "&amp;", "&"),
         X1 = str_replace(X1, "AS", "S")) |> 
  rename(hbres = X1) |> 
  mutate(fin_year = case_when(str_detect(fin_year, "FY2020_21") ~ "2020/21",
                              str_detect(fin_year, "FY2021_22") ~ "2021/22"),
         group = case_when(str_detect(group, "_tested") ~ "test_n",
                           str_detect(group, "_c") ~ "cohort_n",
                           str_detect(group, "p_") ~ "coverage_p"),
         kpi = "KPI 1.2a Sept coverage", .after = hbres) |> 
  filter(group %in% c("test_n", "coverage_p")) |> 
  mutate(simd = NA, .after = kpi) |> 
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
  mutate(simd = NA, .after = kpi) |> 
  glimpse()


### KPI 1.3a ----
## Scotland-level SIMD
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
         kpi = "KPI 1.3a Scotland SIMD", .after = hbres) |> 
  glimpse()


### KPI 1.3a: Coverage by 1Sept  ----
## 2020/21
aaa_2021 <- read.xlsx(paste0(pub_path, 20220301, temp_supp_path, 
                             "/coverage_simd_202021_202122.xlsx"), 
                      cols = c(2,4:7), rows = c(109,111:215)) |> 
  fill(X1, .direction = "down") |> 
  pivot_longer(!X1:X2, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year)

## 2021/22
aaa_2122 <- read.xlsx(paste0(kpi_path, 202209, temp_2_path,
                             "/coverage_simd_202122_202223.xlsx"), 
                      cols = c(2,4:7), rows = c(109,111:215)) |> 
  fill(X1, .direction = "down") |> 
  pivot_longer(!X1:X2, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year)

## Combine and clean
aaa_1.3a_Sept <- rbind(aaa_2021, aaa_2122) |> 
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
         kpi = "KPI 1.3a Sept coverage", .after = hbres) |> 
  filter(group %in% c("test_n", "coverage_p")) |> 
  glimpse()


### KPI 1.3a Health Board ----
## Health Board-level SIMD
## 2020/21
aaa_2021 <- read.xlsx(paste0(pub_path, 20220301, temp_supp_path, 
                             "/coverage_boardlevelsimd_202021_202122.xlsx"),
                      cols = c(2,4:7), rows = c(1,10:107)) |> 
  fill(X1, .direction = "down") |> 
  pivot_longer(!X1:X2, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year)

## 2021/22
aaa_2122 <- read.xlsx(paste0(kpi_path, 202209, temp_2_path, 
                             "/coverage_boardlevelsimd_202122_202223.xlsx"), 
                      cols = c(2,4:7), rows = c(1,10:107)) |> 
  fill(X1, .direction = "down") |> 
  pivot_longer(!X1:X2, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year)

## Combine and clean
aaa_1.3a_hb <- rbind(aaa_2021, aaa_2122) |> 
  mutate(X1 = str_remove(X1, 'xml:space="preserve">'),
         X1 = str_replace_all(X1, "&amp;", "&"),
         X1 = str_replace(X1, "AS", "S")) |> 
  rename(hbres = X1,
         simd = X2) |> 
  mutate(simd = case_when(str_detect(simd, "Total") ~ "Total",
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
         kpi = "KPI 1.3a HB SIMD", .after = hbres) |> 
  glimpse()


### KPI 1.3b ----
## Scotland-level SIMD
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
         kpi = "KPI 1.3b Scotland SIMD", .after = hbres) |> 
  glimpse()


### KPI 1.3b Health Board ----
## Health Board-level SIMD
## 2020/21
aaa_2021 <- read.xlsx(paste0(pub_path, 20220301, temp_add_path, 
                             "/KPI_1.3_additional.xlsx"),
                      cols = c(2,4:7), rows = c(1,10:107)) |> 
  fill(X1, .direction = "down") |> 
  pivot_longer(!X1:X2, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year)

## 2021/22
aaa_2122 <- read.xlsx(paste0(kpi_path, 202209, temp_2_path, 
                             "/KPI_1.3_additional.xlsx"), 
                      cols = c(2,4:7), rows = c(1,10:107)) |> 
  fill(X1, .direction = "down") |> 
  pivot_longer(!X1:X2, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year)

## Combine and clean
aaa_1.3b_hb <- rbind(aaa_2021, aaa_2122) |> 
  mutate(X1 = str_remove(X1, 'xml:space="preserve">'),
         X1 = str_replace_all(X1, "&amp;", "&"),
         X1 = str_replace(X1, "AS", "S")) |> 
  rename(hbres = X1,
         simd = X2) |> 
  mutate(simd = case_when(str_detect(simd, "Total") ~ "Total",
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
         kpi = "KPI 1.3b HB SIMD", .after = hbres) |> 
  glimpse()


### KPI 1.4a ----
## 2020/21
aaa_2021 <- read.xlsx(paste0(pub_path, 20220301, temp_4_path, 
                             "/KPI_1.4atable.xlsx"), 
                      cols = 2:5, rows = c(3,5:19)) |> 
  pivot_longer(!X1, names_to = "group", values_to = "value") |> 
  mutate(fin_year = "2020/21", .after = X1)

## 2021/22
aaa_2122 <- read.xlsx(paste0(kpi_path, 202209, temp_4_path,
                             "/KPI_1.4atable.xlsx"), 
                      cols = 2:5, rows = c(3,5:19)) |> 
  pivot_longer(!X1, names_to = "group", values_to = "value") |> 
  mutate(fin_year = "2021/22", .after = X1)

## Combine and clean
aaa_1.4a <- rbind(aaa_2021, aaa_2122) |> 
  mutate(X1 = str_remove(X1, 'xml:space="preserve">'),
         X1 = str_replace_all(X1, "&amp;", "&"),
         X1 = str_replace(X1, "A_S", "S")) |> 
  rename(hbres = X1) |> 
  mutate(group = case_when(str_detect(group, "attend") ~ "attend_n",
                           str_detect(group, "Cohort") ~ "cohort_n",
                           str_detect(group, "uptake") ~ "uptake_p"),
         kpi = "KPI 1.4a", .after = hbres) |> 
  mutate(simd = NA, .after = kpi) |> 
  glimpse()


### KPI 1.4b ----
## 2020/21
aaa_2021 <- read.xlsx(paste0(pub_path, 20220301, temp_4_path, 
                             "/KPI_1.4btable.xlsx"), 
                      cols = 2:5, rows = c(3,5:19)) |> 
  pivot_longer(!X1, names_to = "group", values_to = "value") |> 
  mutate(fin_year = "2020/21", .after = X1)

## 2021/22
aaa_2122 <- read.xlsx(paste0(kpi_path, 202209, temp_4_path,
                             "/KPI_1.4btable.xlsx"), 
                      cols = 2:5, rows = c(3,5:19)) |>
  tibble::add_row(X1 = "Shetland", 'xml:space=\"preserve\">Cohort' = 0,
                  'xml:space=\"preserve\">attend' = 0,
                  'xml:space=\"preserve\">uptake' = 0, .after = 12) |> 
  pivot_longer(!X1, names_to = "group", values_to = "value") |> 
  mutate(fin_year = "2021/22", .after = X1)

## Combine and clean
aaa_1.4b <- rbind(aaa_2021, aaa_2122) |> 
  mutate(X1 = str_remove(X1, 'xml:space="preserve">'),
         X1 = str_replace_all(X1, "&amp;", "&"),
         X1 = str_replace(X1, "A_S", "S")) |> 
  rename(hbres = X1) |> 
  mutate(group = case_when(str_detect(group, "attend") ~ "attend_n",
                           str_detect(group, "Cohort") ~ "cohort_n",
                           str_detect(group, "uptake") ~ "uptake_p"),
         kpi = "KPI 1.4b", .after = hbres) |> 
  mutate(simd = NA, .after = kpi) |> 
  glimpse()


#### 3: Check and combine ----
names(aaa_1.1)
names(aaa_1.2a)
names(aaa_1.2a_Sept)
names(aaa_1.2b)
names(aaa_1.3a)
names(aaa_1.3a_hb)
names(aaa_1.3a_Sept)
names(aaa_1.3b)
names(aaa_1.3b_hb)
names(aaa_1.4a)
names(aaa_1.4b)

aaa_kpi_historic <- rbind(aaa_1.1, aaa_1.2a, aaa_1.2a_Sept, aaa_1.2b,
                          aaa_1.3a, aaa_1.3a_hb, aaa_1.3a_Sept, aaa_1.3b,
                          aaa_1.3b_hb, aaa_1.4a, aaa_1.4b) |> 
  mutate(hbres = fct_relevel(hbres, c("Scotland", "Ayrshire & Arran", "Borders",
                                      "Dumfries & Galloway", "Fife", "Forth Valley",
                                      "Grampian", "Greater Glasgow & Clyde", 
                                      "Highland", "Lanarkshire", "Lothian", "Orkney", 
                                      "Shetland", "Tayside", "Western Isles")),
         simd = fct_relevel(simd, c("Total", "1", "2", "3", "4", "5",
                                    "Unknown"))) |> 
  select(hbres, kpi, fin_year, simd, group, value)

table(aaa_kpi_historic$kpi)


#### 4: Write out ----
write_rds(aaa_kpi_historic, paste0(kpi_path, 
                                   "historical/aaa_kpi_historical.rds"))
# change permissions to give the group read/write
Sys.chmod(paste0(kpi_path, "historical/aaa_kpi_historical_bckp.rds"),
          mode = "664", use_umask = FALSE)


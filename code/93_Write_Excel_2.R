# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 999_Write_Excel_2.R
# 
# Karen Hotopp
# Sept 2023
# 
# Write out to AAA Excel workbook 2: Invitation and Attendance
# 
# Written/run on Posit WB, R 4.1.2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Notes:
# This script calls in the 3_invite_attend_yyyymm.rds file create in the
# 2_2_kpi_1_1-1_3_uptake_coverage.R script and transforms the data to print 
# directly into the theme 2 Excel file for the MEG.


#### 1: Housekeeping ----
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(openxlsx)


rm(list=ls())
gc()


## Values
source(here::here("code/0_housekeeping.R"))

rm(hb_list, exclusions_path, extract_path, hist_path, simd_path, cutoff_date, 
   year1, year1_start, year1_end, year2_start, year2_end, start_date, end_date, 
   end_current, cut_off_date)

## File paths
template_path <- paste0("/PHI_conf/AAA/Topics/Screening/templates")


### 2: Import and format data ----
theme2 <- read_rds(paste0(temp_path, "/2_1_invite_attend_", yymm, ".rds")) |> 
  mutate(simd = case_when(simd == "1" ~ "1 (most deprived)",
                          simd == "5" ~ "5 (least deprived)",
                          TRUE ~ simd))

table(theme2$kpi, theme2$fin_year) 
# should be 3 most recent complete years + incomplete/active year

theme2_t6 <- read_rds(paste0(temp_path, "/2_2_Table_6_", yymm, ".rds"))
table(theme2_t6$kpi, theme2_t6$fin_year) 

theme2_dna <- read_rds(paste0(temp_path, "/2_3_dna_exclusions_", yymm, ".rds"))
table(theme2_dna$kpi, theme2_dna$fin_year) 


### 3: Format data ----
## KPI 1.1 year1 ----
## Data for three most recent complete years and extended coverage to 1 Sept
kpi_1.1 <- theme2 |> 
  filter(kpi %in% c("KPI 1.1", "KPI 1.1 Sept coverage"),
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.1 year2 ----
## Data for currently active year and extended coverage to 1 Sept
kpi_1.1_y2 <- theme2 |> 
  filter(kpi %in% c("KPI 1.1", "KPI 1.1 Sept coverage"),
         fin_year == year2) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.2a year1 & coverage to Sept ----
## Data for currently active year and extended coverage to 1 Sept
kpi_1.2a <- theme2 |> 
  filter(kpi %in% c("KPI 1.2a", "KPI 1.2a Sept coverage"),
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

# Extract extended coverage to 1 Sept
kpi_1.2a_sept <- kpi_1.2a |>
  select(hbres, contains("_cohort"), contains("Sept coverage"))

kpi_1.2a_sept <- kpi_1.2a_sept[ , c(1, 2, 5, 6, 3, 7, 8, 4, 9, 10)] 

# Remove extra historical coverage to 1 Sept data
kpi_1.2a <- kpi_1.2a[, -c(8:11)]

## KPI 1.2a year2 ----
## Data for currently active year and extended coverage to 1 Sept
kpi_1.2a_y2 <- theme2 |> 
  filter(kpi %in% c("KPI 1.2a", "KPI 1.2a Sept coverage"),
         fin_year == year2) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.2b year1 ----
## Data for currently active year and extended coverage to 1 Sept
kpi_1.2b <- theme2 |> 
  filter(kpi == "KPI 1.2b",
         fin_year  %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.2b year2 ----
## Data for currently active year and extended coverage to 1 Sept
kpi_1.2b_y2 <- theme2 |> 
  filter(kpi == "KPI 1.2b",
         fin_year == year2) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.3a year1  by Scotland SIMD & coverage to Sept ----
## Data for currently active year and extended coverage to 1 Sept
kpi_1.3a <- theme2 |> 
  filter(kpi %in% c("KPI 1.3a Scotland SIMD", "KPI 1.3a Sept coverage"),
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, simd, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

# Extract extended coverage to 1 Sept
kpi_1.3a_sept <- kpi_1.3a |>
  select(hbres, simd, contains("_cohort"), contains("Sept coverage"))

kpi_1.3a_sept <- kpi_1.3a_sept[ , c(1, 2, 3, 6, 7, 4, 8, 9, 5, 10, 11)] 

# Remove extra historical coverage to 1 Sept data
kpi_1.3a <- kpi_1.3a[, -c(9:12)]

## KPI 1.3a year1 by HB SIMD ----
## Data for currently active year by HB SIMD
kpi_1.3a_hb <- theme2 |> 
  filter(kpi == "KPI 1.3a HB SIMD",
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, simd, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.3a year2 ----
## Data for currently active year and extended coverage to 1 Sept
kpi_1.3a_y2 <- theme2 |> 
  filter(kpi %in% c("KPI 1.3a Scotland SIMD", "KPI 1.3a Sept coverage"),
         fin_year == year2,
         hbres ==  "Scotland") |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, simd, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.3b year1 by Scotland SIMD ----
## Data for currently active year by Scotland SIMD
kpi_1.3b <- theme2 |> 
  filter(kpi == "KPI 1.3b Scotland SIMD",
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, simd, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.3b year1 by HB SIMD ----
## Data for currently active year by HB SIMD
kpi_1.3b_hb <- theme2 |> 
  filter(kpi == "KPI 1.3b HB SIMD",
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, simd, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.4a ----
## Data for three most recent complete years (and extended coverage to 1 Sept)
kpi_1.4a <- theme2 |> 
  filter(kpi == "KPI 1.4a",
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.4b ----
## Data for three most recent complete years (and extended coverage to 1 Sept)
kpi_1.4b <- theme2 |> 
  filter(kpi == "KPI 1.4b",
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## Table 6: Surveillance ----
## Data for three most recent complete years
t6_surveill <- theme2_t6 |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, surveillance_interval, sep = "_")) |>
  select(hbres, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## DNA Exclusions ----
## Data for all years
dna_exclude <- theme2_dna |> 
  filter(fin_year %in% c(fy_list)) |> 
  # remove the last two numbers and / from the financial year
  mutate(year = str_remove(fin_year, "[:digit:][:digit:][:punct:]")) |>
  select(pat_inelig, year, count) |>
  # match Excel output
  pivot_wider(names_from = year, values_from = count)

#####
##!! KPI2 1.2a & 1.2b Prisons extract to go here


### 4: Write to Excel (openxlsx) ----
### Setup workbook ---
year2 <- gsub("/", "-", year2)
today <- paste0("Workbook created ", Sys.Date())
tab_1.1_add <- paste0("1.1 Additional (", year2, ")")
tab_1.2a_add <- paste0("1.2a Additional (", year2, ")")
tab_1.2b_add <- paste0("1.2b (uptake) Additional (", year2, ")")
kpi_1.3a_y2 <- select(kpi_1.3a_y2, -c(hbres, simd)) # to match Excel table

wb <- loadWorkbook(paste0(template_path, "/2_Invitation and Attendance_",
                          season, ".xlsx"))
# options("openxlsx.dateFormat" = "dd/mm/yyyy")

## Table of Contents ---
writeData(wb, sheet = "Table of Contents", today, startRow = 6)
writeData(wb, sheet = "Table of Contents", tab_1.1_add, startRow = 12)
writeData(wb, sheet = "Table of Contents", tab_1.2a_add, startRow = 15)
writeData(wb, sheet = "Table of Contents", tab_1.2b_add, startRow = 17)

## KPI 1.1 ---
writeData(wb, sheet = "KPI 1.1", kpi_1.1, startRow = 7, colNames = FALSE)

## KPI 1.1 Additional (20YY-YY)
writeData(wb, sheet = "KPI 1.1 Additional (20YY-YY)", 
          kpi_1.1_y2, startRow = 9, colNames = FALSE)
names(wb)[[3]] <- paste0("KPI 1.1 Additional (", year2, ")")

## KPI 1.2a ---
writeData(wb, sheet = "KPI 1.2a", kpi_1.2a, startRow = 7, colNames = FALSE)

## KPI 1.2a Coverage by 1 Sept
writeData(wb, sheet = "Coverage by 1 Sept", 
          kpi_1.2a_sept, startRow = 7, colNames = FALSE)

## KPI 1.2a Additional (20YY-YY)
writeData(wb, sheet = "KPI 1.2a Additional (20YY-YY)", 
          kpi_1.2a_y2, startRow = 9, colNames = FALSE)

## KPI 1.2b ---
writeData(wb, sheet = "KPI 1.2b", kpi_1.2b, startRow = 7, colNames = FALSE)

## KPI 1.2b Additional (20YY-YY)
writeData(wb, sheet = "KPI 1.2b Additional (20YY-YY)", 
          kpi_1.2b_y2, startRow = 9, colNames = FALSE)
names(wb)[[8]] <- paste0("KPI 1.2b Additional (", year2, ")")

## KPI 1.3a ---
writeData(wb, sheet = "KPI 1.3a", kpi_1.3a, startRow = 7, colNames = FALSE)

## KPI 1.3a Coverage by 1 Sept by SIMD
writeData(wb, sheet = "Coverage by 1 Sept by SIMD", 
          kpi_1.3a_sept, startRow = 7, colNames = FALSE)

## KPI 1.3a Additional (20YY-YY)
writeData(wb, sheet = "KPI 1.2a Additional (20YY-YY)",
          kpi_1.3a_y2, startRow = 34, startCol = 2, colNames = FALSE)
names(wb)[[6]] <- paste0("KPI 1.2a Additional (", year2, ")")

## KPI 1.3a HB SIMD
writeData(wb, sheet = "KPI 1.3a HB SIMD", 
          kpi_1.3a_hb, startRow = 8, colNames = FALSE)

## KPI 1.3b ---
writeData(wb, sheet = "KPI 1.3b", kpi_1.3b, startRow = 7, colNames = FALSE)

## KPI 1.3b HB SIMD
writeData(wb, sheet = "KPI 1.3b HB SIMD", 
          kpi_1.3b_hb, startRow = 8, colNames = FALSE)

## KPI 1.4a ---
writeData(wb, sheet = "KPI 1.4a", kpi_1.4a, startRow = 7, colNames = FALSE)

## KPI 1.4b
writeData(wb, sheet = "KPI 1.4b", kpi_1.2b_y2, startRow = 7, colNames = FALSE)

## Table 6: Surveillance
writeData(wb, sheet = "6) Surveillance", t6_surveill,
          startRow = 8, colNames = FALSE)

## DNA Exclusions
writeData(wb, sheet = "DNA Exclusions", dna_exclude,
          startRow = 6, colNames = FALSE)

# ## Prisons
# writeData(wb, sheet = "KPI 1.2a 1.2b Prisons", kpi_1.2a_prisons,
#           startRow = 7, colNames = FALSE)
# writeData(wb, sheet = "KPI 1.2a 1.2b Prisons", kpi_1.2b_prisons,
#           startRow = 16, colNames = FALSE)
 
## Save ----
saveWorkbook(wb, paste0(output_path, "/2_Invitation and Attendance_", 
                        yymm, ".xlsx"), overwrite = TRUE)


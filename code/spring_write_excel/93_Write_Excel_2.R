# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 999_Write_Excel_2.R
# 
# Karen Hotopp & Aoife McCarthy
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
library(lubridate)


rm(list=ls())
gc()

## Values
source(here::here("code/0_housekeeping.R"))

rm (exclusions_path, extract_path, hist_path, simd_path, hb_list, fy_tibble, 
    hb_tibble, cutoff_date, end_current, end_date, start_date,
    year1_end, year1_start, year2_end, year2_start, year1)

year_xx <- year(cut_off_date)
year_ww <- year_xx - 1
year_vv <- year_xx - 2
year_uu <- year_xx - 3
year_yy <- year_xx + 1

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
# autumn!!
## Data for currently active year and extended coverage to 1 Sept
# kpi_1.1_y2 <- theme2 |> 
#   filter(kpi %in% c("KPI 1.1", "KPI 1.1 Sept coverage"),
#          fin_year == year2) |> 
#   mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
#   select(hbres, FY_kpi_group, value) |>
#   # match Excel output
#   pivot_wider(names_from = FY_kpi_group, values_from = value)

# spring
## Data for currently active year and extended coverage to 1 Sept
kpi_1.1_y2 <- theme2 |> 
  filter(kpi %in% c("KPI 1.1"),
         fin_year == year2) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)


## KPI 1.1 Scotland SIMD ----
## Data for three most recent complete years and extended coverage to 1 Sept
kpi_1.1_simd <- theme2 |> 
  filter(kpi %in% c("KPI 1.1 Scotland SIMD", "KPI 1.1 Scotland SIMD Sept coverage"),
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, simd, FY_kpi_group, value) |>
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
# autumn
## Data for currently active year and extended coverage to 1 Sept
# kpi_1.2a_y2 <- theme2 |> 
#   filter(kpi %in% c("KPI 1.2a", "KPI 1.2a Sept coverage"),
#          fin_year == year2) |> 
#   mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
#   select(hbres, FY_kpi_group, value) |>
#   # match Excel output
#   pivot_wider(names_from = FY_kpi_group, values_from = value)


# spring
## Data for currently active year and extended coverage to 1 Sept
kpi_1.2a_y2 <- theme2 |> 
  filter(kpi %in% c("KPI 1.2a"),
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
# autumn
# kpi_1.3a_y2 <- theme2 |> 
#   filter(kpi %in% c("KPI 1.3a Scotland SIMD", "KPI 1.3a Sept coverage"),
#          fin_year == year2,
#          hbres ==  "Scotland") |> 
#   mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
#   select(hbres, simd, FY_kpi_group, value) |>
#   # match Excel output
#   pivot_wider(names_from = FY_kpi_group, values_from = value)

# spring
kpi_1.3a_y2 <- theme2 |> 
  filter(kpi %in% c("KPI 1.3a Scotland SIMD"),
         fin_year == year2,
         hbres ==  "Scotland") |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, simd, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

kpi_1.3a_y2 <- select(kpi_1.3a_y2, -c(hbres, simd)) # to match Excel table

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
### Setup workbook ----
wb <- loadWorkbook(paste0(template_path, "/2_Invitation and Attendance_",
                          season, ".xlsx"))

# current reporting year - used for additional management info
year2 <- gsub("/", "-", year2)
# bold black fonr used in some headers
bold_black_style_header <- createStyle(fontSize = 14, fontName = "Arial",
                                textDecoration = "bold", fontColour = "#000000")
# orange style for notes needing manual contribution
orange_font <- createStyle(fontSize = 11, fontName = "Arial", 
                           fontColour = "#ff9f00", wrapText = TRUE)
# turned 66 in year... titles for KPI tables
turn66_year_vv <- paste0("Turned 66 in year ending 31 March ", year_vv, '\n',
                         "(became eligible in year ending 31 March ", year_uu, ")")
turn66_year_ww <- paste0("Turned 66 in year ending 31 March ", year_ww, '\n',
                           "(became eligible in year ending 31 March ", year_vv, ")")
turn66_year_xx <- paste0("Turned 66 in year ending 31 March ", year_xx, '\n',
                         "(became eligible in year ending 31 March ", year_ww, ")")
turn66_year_yy <- paste0("Turned 66 in year ending 31 March ", year_yy, '\n',
                         "(became eligible in year ending 31 March ", year_xx, ")")

# note on additional cohort sheets about eligibility dates
add_cohort_note <- paste0("Data for latest annual cohort eligible for screening ",
                                 "(i.e., men reaching age 66 in ", year2, ").")
# note on additional cohort sheets about performance
add_performance_note <- paste0("Performance against the KPI thresholds for this ",
                               "cohort cannot be fully assessed at this stage. ",
                               "Data are shown to demonstrate the work-in-progress ",
                               "position at ", extract_date, " ", year_xx, 
                               ". The KPI data for this cohort will be finalised ",
                               "from the PHS data extract at 1 September ",
                               year_yy, ".")
add_performance_style <- createStyle(fontSize = 12,  fontColour = "#FF0000", 
                                     fontName = "Arial", textDecoration = c("bold"),
                                     wrapText = TRUE)

prov_data_note <- paste0("1. Data for year ending 31 March ", year_xx,
                         " are provisional: some men in this cohort had not ",
                         "reached age 66 and 3 months by the date of the PHS ",
                         "extract on ", extract_date, " ", year_xx, " (the ",
                         "youngest men in this cohort will reach age 66 and 3 ",
                         "months on 30 June ", year_xx, "). In addition, a few ",
                         "men in the eligible age range may move in or out of ",
                         "Scotland, which may result in small changes to the ",
                         "cohort of men offered screening before age 66 and the ",
                         "uptake rate. Data will be finalised from the PHS data ",
                         "extract at 1 September ", year_xx, ".")


### Table of Contents ----

## sheet headings
pub_year <- paste0("Data for year ending 31 March ", year_xx,
               " scheduled to be published in April ",  year_yy,
               " (final data will be produced from data extracted for PHS in ",
               "September ", year_xx, ").")
writeData(wb, sheet = "Table of Contents", pub_year, startRow = 3)

meg_note <- paste0("For review at MEG in ", meg_month, " ", year_xx)
writeData(wb, sheet = "Table of Contents", meg_note, startRow = 4)

today <- paste0("Workbook created ", Sys.Date())
writeData(wb, sheet = "Table of Contents", today, startRow = 6)

## TOC contents
tab_1.1_add <- paste0("1.1 Additional (", year2, ")")
writeData(wb, sheet = "Table of Contents", tab_1.1_add, startRow = 12)

tab_1.1_add_desc <- paste0("Percentage of eligible population who are sent an ",
                           "initial offer to screening before age 66: work-in-progress ",
                           "position for men reaching age 66 in year ending 31 March ", 
                           year_yy)
writeData(wb, sheet = "Table of Contents", tab_1.1_add_desc, startRow = 12,
          startCol = 2)

tab_1.2a_add <- paste0("1.2a Additional (", year2, ")")
writeData(wb, sheet = "Table of Contents", tab_1.2a_add, startRow = 15)

tab_1.2_add_desc <- paste0("Percentage of eligible population who are tested ",
                           "before age 66 and 3 months: work-in-progress ",
                           "position for men reaching age 66 in year ending ",
                           "31 March ", year_yy)
writeData(wb, sheet = "Table of Contents", tab_1.2_add_desc, startRow = 15,
          startCol = 2)

tab_1.2b_add <- paste0("1.2b (uptake) Additional (", year2, ")")
writeData(wb, sheet = "Table of Contents", tab_1.2b_add, startRow = 17)

tab_1.2b_add_desc <- paste0("Percentage of men offered screening before age 66 ",
                            "who are tested before age 66 and 3 months: ",
                            "work-in-progress position for men reaching age 66 ",
                            "in year ending 31 March ", year_yy)
writeData(wb, sheet = "Table of Contents", tab_1.2b_add_desc, startRow = 17,
          startCol = 2)

## footnotes
note_toc <- paste0("The provisional/partial data for the year ending 31 March ", 
                   year_xx, " are released for data quality assurance and ",
                   "management information purposes and should not be placed in ",
                   "the public domain. The information can be shared locally with ",
                   "those who have a legitimate need to review the data for ",
                   "quality assurance, managerial or operational purposes.")
writeData(wb, sheet = "Table of Contents", note_toc, startRow = 26)
addStyle(wb, "Table of Contents", style = add_performance_style, rows = 26, cols = 1)

showGridLines(wb, "Table of Contents", showGridLines = FALSE)

# options("openxlsx.dateFormat" = "dd/mm/yyyy")

### KPI 1.1 ----

## text/formatting ---

## table headers
writeData(wb, sheet = "KPI 1.1", turn66_year_vv, startRow = 4,
          startCol = 2)
writeData(wb, sheet = "KPI 1.1", turn66_year_ww, startRow = 4,
          startCol = 5)
writeData(wb, sheet = "KPI 1.1", turn66_year_xx, startRow = 4,
          startCol = 8)

## footnotes

kpi_1.1_note1 <- paste0("1. Data for year ending 31 March ", year_xx, " are ",
                        "provisional: data will be finalised from the PHS data ",
                        "extract at 1 September ", year_xx, ". Additionally, a ",
                        "few men in the eligible age range may move in or out ",
                        "of Scotland, which may result in small changes to the ",
                        "number of men in the cohort and invite rate.")
writeData(wb, sheet = "KPI 1.1", kpi_1.1_note1, startRow = 30)

# note 3 calculations 
# calculate number of men not invited for screening before 66
cohort_n <- kpi_1.1 %>% 
  filter(hbres=="Scotland") %>% 
  select(contains(kpi_report_years[3]) & contains("cohort_n")) %>% 
  pull()
                  
invited_before_66 <- kpi_1.1 %>% 
  filter(hbres=="Scotland") %>% 
  select(contains(kpi_report_years[3]) & contains("KPI 1.1_offer_n")) %>% 
  pull()

kpi_1.1_no_invite_before_66 <- cohort_n - invited_before_66

# calculated number of men invited after age of 66
invited_any_age <- kpi_1.1 %>% 
  filter(hbres=="Scotland") %>% 
  select(contains(kpi_report_years[3]) & contains("Sept coverage_offer_n")) %>% 
  pull()

kpi_1.1_invited_after_66 <- invited_any_age - invited_before_66

# calculated number of men not invited despite eligible
kpi_1.1_not_invited <- cohort_n - invited_any_age

rm(cohort_n, invited_before_66, invited_any_age) # tidy environment



kpi_1.1_note3 <- paste0("3. Additional management information: the data for the ",
                        "year ending 31 March ", year_xx, " shows there were ",
                        kpi_1.1_no_invite_before_66, " men in the latest cohort ",
                        "who were not invited for screening before age 66. ",
                        "Of these, ", kpi_1.1_invited_after_66, " were invited ",
                        "after their 66th birthday and the remaining ", 
                        kpi_1.1_not_invited, " had not been invited at ",
                        extract_date, " ", year_xx, " (date of PHS data extract).")
writeData(wb, sheet = "KPI 1.1", kpi_1.1_note3, startRow = 32)

rm(kpi_1.1_no_invite_before_66, kpi_1.1_invited_after_66, kpi_1.1_not_invited) # tidy

showGridLines(wb, "KPI 1.1", showGridLines = FALSE)

## data ---
writeData(wb, sheet = "KPI 1.1", kpi_1.1, startRow = 7, colNames = FALSE)

### KPI 1.1 Additional (20YY-YY) ----

## text/formatting ---

## sheet headings
writeData(wb, sheet =  "KPI 1.1 Additional (20XX-YY)", add_cohort_note, startRow = 3)
addStyle(wb, "KPI 1.1 Additional (20XX-YY)", style = bold_black_style_header, rows = 3, cols = 1)
#
writeData(wb, sheet =  "KPI 1.1 Additional (20XX-YY)", add_performance_note, startRow = 4)
addStyle(wb, "KPI 1.1 Additional (20XX-YY)", style = add_performance_style, rows = 4, cols = 1)

## table headers

writeData(wb, sheet = "KPI 1.1 Additional (20XX-YY)", turn66_year_yy, startRow = 6,
          startCol = 2)

## footnotes

kpi_1.1_add_note1 <- paste0("1. For the previous eligible cohorts at this stage, ",
                           "the equivalent percentages of men offered screening ",
                           "before age 66 were {x}% (", year_vv, "/", 
                           substr(year_ww, 3,4), ") and {x}% (", year_ww, "/",
                           substr(year_xx, 3,4), ").")
writeData(wb, sheet = "KPI 1.1 Additional (20XX-YY)", kpi_1.1_add_note1, startRow = 31)
addStyle(wb, "KPI 1.1 Additional (20XX-YY)", style = orange_font, rows = 31, cols = 1)

### AMc note: this footnote is probably too hard to figure out tbh
# need to call in 2_1_invite_attend files from prior 2 years
# then process them to look as above?

# calculate prior 2 yymm and temp_path objects

# 1 year prior
# yymm
# year <- substr(yymm, 1, 4)
# month <- substr(yymm, 5, 6)
# new_year <- as.numeric(year) - 1
# yymm_prior1 <- paste0(new_year, month)

# temp_path
# string <- temp_path
# year <- substr(string, nchar(string) - 10, nchar(string) - 7)
# new_year <- as.numeric(year) - 1
# temp_path_prior1 <- sub(year, new_year, string)


# 2 year prior
# yymm
# year <- substr(yymm, 1, 4)
# month <- substr(yymm, 5, 6)
# new_year <- as.numeric(year) - 2
# yymm_prior2 <- paste0(new_year, month)

# temp_path
# year <- substr(string, nchar(string) - 10, nchar(string) - 7)
# new_year <- as.numeric(year) - 2
# temp_path_prior2 <- sub(year, new_year, string)
# 
# rm(year, month, new_year, string)



# theme2_prior1 <- read_rds(paste0(temp_path_prior1, "/2_1_invite_attend_", yymm_prior1, ".rds" )) # doesn't exist, no 202203 file

# theme2_prior2 <- read_rds(paste0(temp_path_prior2, "/2_1_invite_attend_", yymm_prior2, ".rds" )) # doesn't exist, the files are called something different


# add_kpi_1.1_note <- paste0("1. For the previous eligible cohorts at this stage, the equivalent percentages of men offered screening before age 66 were {x}% (20VV/WW) and {x}% (20WW/XX).
# ")

showGridLines(wb, "KPI 1.1 Additional (20XX-YY)", showGridLines = FALSE)

## data ---
writeData(wb, sheet = "KPI 1.1 Additional (20XX-YY)", 
          kpi_1.1_y2, startRow = 9, colNames = FALSE)
## sheet name ---
names(wb)[[3]] <- paste0("KPI 1.1 Additional (", year2, ")")


### KPI 1.1 SIMD ----

## text/formatting ---

## table headers
writeData(wb, sheet = "KPI 1.1 SIMD", turn66_year_vv, startRow = 4,
          startCol = 3)
writeData(wb, sheet = "KPI 1.1 SIMD", turn66_year_ww, startRow = 4,
          startCol = 6)
writeData(wb, sheet = "KPI 1.1 SIMD", turn66_year_xx, startRow = 4,
          startCol = 9)

## footnotes ---

writeData(wb, sheet = "KPI 1.1 SIMD", kpi_1.1_note1, startRow = 120)
writeData(wb, sheet = "KPI 1.1 SIMD", kpi_1.1_note3, startRow = 122)

showGridLines(wb, "KPI 1.1 SIMD", showGridLines = FALSE)

## data ---
writeData(wb, sheet = "KPI 1.1 SIMD", 
          kpi_1.1_simd, startRow = 7, colNames = FALSE)


### KPI 1.2a ----

## text/formatting ---

## table headers
writeData(wb, sheet = "KPI 1.2a", turn66_year_vv, startRow = 4,
          startCol = 2)
writeData(wb, sheet = "KPI 1.2a", turn66_year_ww, startRow = 4,
          startCol = 5)
writeData(wb, sheet = "KPI 1.2a", turn66_year_xx, startRow = 4,
          startCol = 8)

kpi_1.2a_head1 <- paste0("Tested before 1 March ", year_xx, " (includes men ",
                         "tested after age 66 and 3 months)")
writeData(wb, sheet = "KPI 1.2a", kpi_1.2a_head1, startRow = 5,
          startCol = 11)

## footnotes
writeData(wb, sheet = "KPI 1.2a", prov_data_note, startRow = 30, colNames = FALSE)

showGridLines(wb, "KPI 1.2a", showGridLines = FALSE)

## data ---
writeData(wb, sheet = "KPI 1.2a", kpi_1.2a, startRow = 7, colNames = FALSE)

# autumn only
### KPI 1.2a Coverage by 1 Sept ----
# writeData(wb, sheet = "Coverage by 1 Sept", 
#           kpi_1.2a_sept, startRow = 7, colNames = FALSE)

### KPI 1.2a Additional (20XX-YY) ----

## text/formatting ---

## sheet headings
writeData(wb, sheet =  "KPI 1.2a Additional (20XX-YY)", add_cohort_note, startRow = 3)
addStyle(wb, "KPI 1.2a Additional (20XX-YY)", style = bold_black_style_header, rows = 3, cols = 1)

writeData(wb, sheet =  "KPI 1.2a Additional (20XX-YY)", add_performance_note, startRow = 4)
addStyle(wb, "KPI 1.2a Additional (20XX-YY)", style = add_performance_style, rows = 4, cols = 1)

## table headers

writeData(wb, sheet = "KPI 1.2a Additional (20XX-YY)", turn66_year_yy, startRow = 6,
          startCol = 2)

## footnotes
# AMc note: footnote 1 slightly too hard to compute automatically?
kpi_1.2a_add_note2 <- paste0("2. Some men in this cohort have not reached age ",
                             "66 and 3 months yet. The oldest men in the cohort ",
                             "will reach this age on 1 July ", year_xx, 
                             " and the youngest men in the cohort will reach ",
                             "this age on 30 June ", year_yy, ".")
writeData(wb, sheet =  "KPI 1.2a Additional (20XX-YY)", kpi_1.2a_add_note2, startRow = 27)

## data ---
writeData(wb, sheet = "KPI 1.2a Additional (20XX-YY)", 
          kpi_1.2a_y2, startRow = 9, colNames = FALSE)


### KPI 1.3a Additional (20XX-YY) ----

## text/formatting ---
## table headers

writeData(wb, sheet = "KPI 1.2a Additional (20XX-YY)", turn66_year_yy, startRow = 31,
          startCol = 2)

showGridLines(wb, "KPI 1.2a Additional (20XX-YY)", showGridLines = FALSE)

## footnotes

kpi_1.3a_add_note1 <- paste0("1. For the previous eligible cohort at this stage, ",
                             "the equivalent percentage of men tested before age ",
                             "66 and 3 months was {x}% (", year_ww, "/", 
                             substr(year_xx, 3, 4), ").")
writeData(wb, sheet = "KPI 1.2a Additional (20XX-YY)", kpi_1.3a_add_note1, startRow = 26)
addStyle(wb, "KPI 1.2a Additional (20XX-YY)", style = orange_font, rows = 26, cols = 1)


## data ---
writeData(wb, sheet = "KPI 1.2a Additional (20XX-YY)",
          kpi_1.3a_y2, startRow = 34, startCol = 2, colNames = FALSE)
## sheet name ---
names(wb)[[6]] <- paste0("KPI 1.3a Additional (", year2, ")")


### KPI 1.2b ----

## text/formatting ---
## table headers
writeData(wb, sheet = "KPI 1.2b", turn66_year_vv, startRow = 4,
          startCol = 2)
writeData(wb, sheet = "KPI 1.2b", turn66_year_ww, startRow = 4,
          startCol = 5)
writeData(wb, sheet = "KPI 1.2b", turn66_year_xx, startRow = 4,
          startCol = 8)

## footnotes
writeData(wb, sheet = "KPI 1.2b", prov_data_note, startRow = 30, colNames = FALSE)

showGridLines(wb, "KPI 1.2b", showGridLines = FALSE)

## data ---
writeData(wb, sheet = "KPI 1.2b", kpi_1.2b, startRow = 7, colNames = FALSE)


### KPI 1.2b Additional (20YY-YY) ----

## text/formatting ---
## sheet headings
writeData(wb, sheet =  "KPI 1.2b Additional (20XX-YY)", add_cohort_note, startRow = 3)
addStyle(wb, "KPI 1.2b Additional (20XX-YY)", style = bold_black_style_header, rows = 3, cols = 1)
#
writeData(wb, sheet =  "KPI 1.2b Additional (20XX-YY)", add_performance_note, startRow = 4)
addStyle(wb, "KPI 1.2b Additional (20XX-YY)", style = add_performance_style, rows = 4, cols = 1)

## table headers

writeData(wb, sheet = "KPI 1.2b Additional (20XX-YY)", turn66_year_yy, startRow = 6,
          startCol = 2)

kpi_1.2badd_foot <- paste0("1. The equivalent figure for the previous eligible ",
                           "cohort in Scotland at this stage was {x}% (", 
                           year_ww, "/", substr(year_xx, 3, 4), ").")
writeData(wb, sheet = "KPI 1.2b Additional (20XX-YY)", kpi_1.2badd_foot, startRow = 31)
addStyle(wb, "KPI 1.2b Additional (20XX-YY)", orange_font, rows = 31, cols = 1)

showGridLines(wb, "KPI 1.2b Additional (20XX-YY)", showGridLines = FALSE)

## data ---
writeData(wb, sheet = "KPI 1.2b Additional (20XX-YY)", 
          kpi_1.2b_y2, startRow = 9, colNames = FALSE)
## sheet names ---
names(wb)[[8]] <- paste0("KPI 1.2b Additional (", year2, ")")


### KPI 1.3a ----

## text/formatting ---
## table headers
writeData(wb, sheet = "KPI 1.3a", turn66_year_vv, startRow = 4,
          startCol = 3)
writeData(wb, sheet = "KPI 1.3a", turn66_year_ww, startRow = 4,
          startCol = 6)
writeData(wb, sheet = "KPI 1.3a", turn66_year_xx, startRow = 4,
          startCol = 9)

## footnotes --
writeData(wb, sheet = "KPI 1.3a", prov_data_note, startRow = 120, colNames = FALSE)

showGridLines(wb, "KPI 1.3a", showGridLines = FALSE)

## data ---
writeData(wb, sheet = "KPI 1.3a", kpi_1.3a, startRow = 7, colNames = FALSE)

# autumn only
### KPI 1.3a Coverage by 1 Sept by SIMD ----
# writeData(wb, sheet = "Coverage by 1 Sept by SIMD", 
#           kpi_1.3a_sept, startRow = 7, colNames = FALSE)



### KPI 1.3a HB SIMD ----

## text/formatting ---
## table headers
writeData(wb, sheet = "KPI 1.3a HB SIMD", turn66_year_vv, startRow = 5,
          startCol = 3)
writeData(wb, sheet = "KPI 1.3a HB SIMD", turn66_year_ww, startRow = 5,
          startCol = 6)
writeData(wb, sheet = "KPI 1.3a HB SIMD", turn66_year_xx, startRow = 5,
          startCol = 9)

## footnotes ---
writeData(wb, sheet = "KPI 1.3a HB SIMD", prov_data_note, startRow = 114, colNames = FALSE)

showGridLines(wb, "KPI 1.3a HB SIMD", showGridLines = FALSE)

## data ---
writeData(wb, sheet = "KPI 1.3a HB SIMD", 
          kpi_1.3a_hb, startRow = 8, colNames = FALSE)


### KPI 1.3b ----

## text/formatting ---
## table headers
writeData(wb, sheet = "KPI 1.3b", turn66_year_vv, startRow = 4,
          startCol = 3)
writeData(wb, sheet = "KPI 1.3b", turn66_year_ww, startRow = 4,
          startCol = 6)
writeData(wb, sheet = "KPI 1.3b", turn66_year_xx, startRow = 4,
          startCol = 9)

## footnotes ---
writeData(wb, sheet = "KPI 1.3b", prov_data_note, startRow = 120, colNames = FALSE)

showGridLines(wb, "KPI 1.3b", showGridLines = FALSE)

# data ---
writeData(wb, sheet = "KPI 1.3b", kpi_1.3b, startRow = 7, colNames = FALSE)




# sheet doesn't exist
### KPI 1.3b HB SIMD ----
# writeData(wb, sheet = "KPI 1.3b HB SIMD", 
#           kpi_1.3b_hb, startRow = 8, colNames = FALSE)

### KPI 1.4a ----

## text/formatting ---
## table headers

kpi_1.4a_head1 <- paste0("Due to attend annual surveillance in year ending 31",
                         '\n', "March ", year_vv)
writeData(wb, sheet = "KPI 1.4a", kpi_1.4a_head1, startRow = 4,
          startCol = 2)

kpi_1.4a_head2 <- paste0("Due to attend annual surveillance in year ending 31",
                         '\n', "March ", year_ww)
writeData(wb, sheet = "KPI 1.4a", kpi_1.4a_head2, startRow = 4,
          startCol = 4)

kpi_1.4a_head3 <- paste0("Due to attend annual surveillance from 1 April ",
                         year_ww, " -", '\n', "31 January ", year_xx, '\n',
                         "(partial data for financial year)")
writeData(wb, sheet = "KPI 1.4a", kpi_1.4a_head3, startRow = 4,
          startCol = 6)

kpi_1.4a_head4 <- paste0("Tested before 1 March ", year_xx, '\n', "(includes ",
                         "men tested more than 6", '\n', "weeks from due date)")
writeData(wb, sheet = "KPI 1.4a", kpi_1.4a_head3, startRow = 4,
          startCol = 8)
	

## footnotes ---

kpi_1.4a_note1 <- paste0("1. Due to attend surveillance 1 April ", year_ww, 
                         " to 31 January ", year_xx, ": provisional rates are ",
                         "presented for the 11-month period 1 March ", year_ww,
                         " to 31 January ", year_xx, " as data are not yet ",
                         "available for the full financial year ending 31 March ",
                         year_xx, " from the PHS extract at ", extract_date, " ",
                         year_xx, ". Data for the complete financial year ending ",
                         "31 March ", year_xx, " will be produced from the PHS ",
                         "data extract at 1 September ", year_xx, ".")
writeData(wb, sheet = "KPI 1.4a", kpi_1.4a_note1, startRow = 30, colNames = FALSE)

showGridLines(wb, "KPI 1.4a", showGridLines = FALSE)

## data ---
writeData(wb, sheet = "KPI 1.4a", kpi_1.4a, startRow = 7, colNames = FALSE)

### KPI 1.4b ----

## text/formatting ---
## table headers
kpi_1.4b_head1 <- paste0("Due to attend quarterly surveillance in year ending 31",
                         '\n', "March ", year_vv)
writeData(wb, sheet = "KPI 1.4b", kpi_1.4b_head1, startRow = 4,
          startCol = 2)

kpi_1.4b_head2 <- paste0("Due to attend quarterly surveillance in year ending 31",
                         '\n', "March ", year_ww)
writeData(wb, sheet = "KPI 1.4b", kpi_1.4b_head2, startRow = 4,
          startCol = 4)

kpi_1.4b_head3 <- paste0("Due to attend quarterly surveillance from 1 April ",
                         year_ww, " -", '\n', "31 January ", year_xx, '\n',
                         "(partial data for financial year)")
writeData(wb, sheet = "KPI 1.4b", kpi_1.4b_head3, startRow = 4,
          startCol = 6)

kpi_1.4b_head4 <- paste0("Tested before 1 March ", year_xx, '\n', "(includes ",
                         "men tested more than 6", '\n', "weeks from due date)")
writeData(wb, sheet = "KPI 1.4b", kpi_1.4b_head3, startRow = 4,
          startCol = 8)

## footnotes ---
### AMc note: this footnote is the SAME as above 1.2b - need to change this
writeData(wb, sheet = "KPI 1.4b", kpi_1.4a_note1, startRow = 30, colNames = FALSE)

showGridLines(wb, "KPI 1.4b", showGridLines = FALSE)

## data ---
writeData(wb, sheet = "KPI 1.4b", kpi_1.4b, startRow = 7, colNames = FALSE)

### Table 6: Surveillance----

## text/formatting ---
## table headers
table6_head1 <- paste0("Screened in year ending 31 March",  '\n',
                       year_vv)
writeData(wb, sheet = "6) Surveillance", table6_head1, startRow = 6,
          startCol = 2)

table6_head2 <- paste0("Screened in year ending 31 March",  '\n',
                      year_ww)
writeData(wb, sheet = "6) Surveillance", table6_head2, startRow = 6,
          startCol = 4)

table6_head3 <- paste0("Screened from 1 April ", year_ww, " - 28",
                       '\n', "February ", year_xx)
writeData(wb, sheet = "6) Surveillance", table6_head3, startRow = 6,
          startCol = 6)

showGridLines(wb, "6) Surveillance", showGridLines = FALSE)

## data ---
writeData(wb, sheet = "6) Surveillance", t6_surveill,
          startRow = 8, colNames = FALSE)

### DNA Exclusions ----

## footnotes ---
dna_note1 <- paste0("1. Data for year ending 31 March ", year_xx, " are ",
                    "provisional; data will be finalised from the PHS data ",
                    "extract at 1 September ", year_xx, ".")
writeData(wb, sheet = "DNA Exclusions", dna_note1,
          startRow = 10, colNames = FALSE)

showGridLines(wb, "DNA Exclusions", showGridLines = FALSE)

## data ---
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


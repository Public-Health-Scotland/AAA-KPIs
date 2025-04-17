#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x_kpi_1_3a_simd_audit.R
# Angus Morton
# Adapted by Aoife McCarthy
# 08/01/2025
#
# Produces range for KPI 1.3a by SIMD - audit for Spring 2025
#
# Written/run on Posit PWB (R version 4.1.2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1: Housekeeping and vars ----
library(dplyr)
library(readr)
library(tidyr)
library(forcats)
library(phsaaa)
library(openxlsx)

rm(list=ls())
gc()

source(here::here("code", "00_housekeeping.R"))

rm (exclusions_path, extract_path, fy_tibble, qpmg_month,
    cutoff_date, end_current, end_date, start_date, extract_date,
    hb_tibble, gp_prac_extract_date, simd_path, year1_start, year1_end, year2_start,
    year2_end, year1, year2, fy_list, hist_path)

# 2: Load data ----

# all data for KPI theme 2 workbook
theme2_data <- read_rds(paste0(temp_path, "/2_1_invite_attend_", yymm, ".rds")) 

kpi_1_3a_data <- theme2_data |> 
  filter(kpi == "KPI 1.3a Scotland SIMD", # only full data, not additional
         group == "coverage_p", # just coverage % required
         !simd == "Unknown", # unknown SIMD not relevant
         fin_year %in% kpi_report_years) # only want full years, not look forward 


# 3: Calculate difference in uptake between SIMDs ----

## Basefile ----
# total simd, min and max values - contains dups for the min and max
range_data_base <- kpi_1_3a_data |> 
  group_by(hbres, fin_year) |> 
  mutate(group = case_when(simd == "Total" ~ "total_simd_p",
                           value == max(value, na.rm = T) ~ "max_p", # sometimes > 1 per hbres/fy group
                           value == min(value, na.rm = T) ~ "min_p", # sometimes > 1 per hbres/fy group
                           TRUE ~ group)) |>
  # filter max and min values (to calc difference) and the total for reference
  filter(group %in% c("total_simd_p", "max_p", "min_p"))|> 
  ungroup() |> 
  mutate(group = fct_relevel(group, c("total_simd_p", "min_p", "max_p", "diff")),
         hbres = fct_relevel(hbres, hb_list)) |> 
  arrange(fin_year, hbres, group)

## Total SIMD values ----
# for each hbres and FY
range_data_totals <- range_data_base |> # separate out "Total" simd data for ease
  filter(group == "total_simd_p")

## Min and Max values ----
# for each hbres and FY with only 1 row per group
range_data_minmax <- range_data_base |> # keep min and max with simd level
  filter(group %in% c("max_p", "min_p")) |> 
  group_by(hbres, fin_year, group) |> 
  mutate(multiples_count = n()) |> # counts # of entries for each group
  mutate(keep_flag = case_when(multiples_count == 1 ~ 1,
                               # keep min value with the lowest simd quintile
                               multiples_count > 1 & group == "min_p" & simd == min(simd) ~ 1,
                               # keep max value with the highest simd quintile
                               multiples_count > 1 & group == "max_p" & simd == max(simd) ~ 1,
                               .default = 0)) |> 
  ungroup() |> 
  filter(keep_flag == 1) |> 
  select(-multiples_count, -keep_flag)

## Diff between min and max values ----
# for each hbres and FY
range_data_diff <- range_data_minmax |> 
  group_by(hbres, fin_year) |> 
  mutate(lag_value = lag(value, n = 1)) |> # lag so that lag_value is the min_p, and value is the max_p
  filter(!is.na(lag_value)) |> 
  summarise(hbres = unique(hbres), 
            kpi = unique(kpi),
            fin_year = unique(fin_year),
            simd = "diff",
            group = "diff_pp", # pp = percentage points
            value = value - lag_value) |>
  ungroup()

## Combined data ----
range_data <- range_data_totals |> 
  rbind(range_data_minmax, range_data_diff) |> 
  arrange(fin_year, hbres, group)
# should have 180 rows in the df - 4 per hb per fy


# 3: Save rds output ----------------------------------------------------------

query_write_rds(range_data, paste0(temp_path, "/x_uptake_simd_diff.rds"))

rm(theme2_data, kpi_1_3a_data, range_data_base, range_data_totals, range_data_minmax, range_data_diff)

# 4: Format for Excel -----------------------------------------------------

# ISSUE - NEED SIMD TO BE A VALUE FOR EACH YEAR AS THEY WILL NOT BE THE SAME MAX AND MIN EACH TIME

# make new df of min and max SIMD values for each hbres and FY
# this is because we need the min and max values for every FY
simd_minmax <- range_data |> 
  filter(group %in% c("max_p", "min_p")) |> 
  mutate(group = case_when(group == "max_p" ~ "max_simd",
                           group == "min_p" ~ "min_simd",
                           .default = group),
         value = as.numeric(simd))
  


# usual excel format is wide - add in SIMD values

excel_format_data <- range_data |> 
  bind_rows(simd_minmax) |> 
  select(-kpi, -simd) |> 
  mutate(group = fct_relevel(group, c("total_simd_p", "max_simd", "max_p", "min_simd", "min_p", "diff_pp"))) |> 
  arrange(fin_year, hbres, group) |> 
  unite(fy_group, c("fin_year", "group")) |> 
  pivot_wider(names_from = fy_group, values_from = value)
  


# 5: Write to Excel -------------------------------------------------------

source(here::here("code", "src", "Source_Excel_Styles.R"))

add_cond_format <- function(col_number) {
  conditionalFormatting(wb, "KPI 1.3a difference", rows = refs$data:refs$data_end, cols = col_number,
                        type = "between", rule = c(0 , 74.9), style = cond_form$red_cond) 
  conditionalFormatting(wb, "KPI 1.3a difference", rows = refs$data:refs$data_end, cols = col_number,
                        type = "between", rule = c(75, 84.9), style = cond_form$yellow_cond) 
  conditionalFormatting(wb, "KPI 1.3a difference", rows = refs$data:refs$data_end, cols = col_number,
                        type = "between", rule = c(85, 100), style = cond_form$green_cond) 
}

wb <- createWorkbook()

addWorksheet(wb, "KPI 1.3a difference")
make_openxlsx_funcs(wb, "KPI 1.3a difference") # makes custom funcs with that wb name and sheet name


# refs
refs <- list()
refs$title <- 1
refs$headers <- refs$title + 4
refs$data <- refs$headers + 3
refs$data_end <- refs$data + nrow(excel_format_data) - 1


# conditional formatting
cond_form <- list()
cond_form$red_cond <- createStyle(fontSize = 12, fontName = "Arial", fontColour = "#FFFFFF", bgFill = "#911913")
cond_form$yellow_cond <- createStyle(fontSize = 12, fontName = "Arial", fontColour = "#000000", bgFill = "#F7EC6D")
cond_form$green_cond <- createStyle(fontSize = 12, fontName = "Arial", fontColour = "#000000", bgFill = "#6AB42D")

# notes etc
year_xx <- year(cut_off_date)
year_ww <- year_xx - 1
year_vv <- year_xx - 2
year_uu <- year_xx - 3
year_yy <- year_xx + 1

turn66_year_vv <- paste0("Turned 66 in year ending 31 March ", year_vv, '\n',
                         "(became eligible in year ending 31 March ", year_uu, ")")
turn66_year_ww <- paste0("Turned 66 in year ending 31 March ", year_ww, '\n',
                         "(became eligible in year ending 31 March ", year_vv, ")")
turn66_year_xx <- paste0("Turned 66 in year ending 31 March ", year_xx, '\n',
                         "(became eligible in year ending 31 March ", year_ww, ")")

heads <- rep(c("Total", "Highest uptake", " ", "Lowest uptake", " ", "Difference"), 3)
subheads <- rep(c("%", "SIMD quintile", "%", "SIMD quintile", "%", "% points"), 3)

# formatting
custMergeCells(rows = refs$title, cols = 1:19)
custMergeCells(rows = refs$title + 1, cols = 1:19)
custMergeCells(rows = refs$headers:(refs$headers+2), cols = 1)
custMergeCells(rows = refs$headers, cols = 2:7)
custMergeCells(rows = refs$headers, cols = 8:13)
custMergeCells(rows = refs$headers, cols = 14:19)
custMergeCells(rows = refs$headers + 1, cols = 3:4)
custMergeCells(rows = refs$headers + 1, cols = 5:6)
custMergeCells(rows = refs$headers + 1, cols = 9:10)
custMergeCells(rows = refs$headers + 1, cols = 11:12)
custMergeCells(rows = refs$headers + 1, cols = 15:16)
custMergeCells(rows = refs$headers + 1, cols = 17:18)

custSetColWidths(cols = 1, widths = 23)
custSetColWidths(cols = 2:20, widths = 7)
custSetColWidths(cols = c(7, 13, 19), widths = 9.4)

custSetRowHeights(rows = refs$headers + 2, heights = 30)


# title
custWriteData("Management information", startRow = refs$title, startCol = refs$title)
custAddStyle(styles$red_bold_12, rows = refs$title, cols = 1)
custWriteData("KPI 1.3a difference: range between SIMD quintiles with highest and lowest uptake", startRow = refs$title + 1, startCol = 1)
custAddStyle(styles$black_bold_nowrap_18, rows = refs$title + 1, cols = 1)

# headers
## hbres
custWriteData("Health Board of Residence", startRow = refs$headers, startCol = 1)
custAddStyle(styles$black_12, rows = refs$headers, cols = 1)

## turned 66 in year ...
custWriteData(turn66_year_vv, startRow = refs$headers, startCol = 2)
custWriteData(turn66_year_ww, startRow = refs$headers, startCol = 8)
custWriteData(turn66_year_xx, startRow = refs$headers, startCol = 14)
custAddStyle(styles$black_border_thin_centre_12, rows = refs$headers, cols = 2:19)

## total/highest/lowest/diff
custWriteData(t(heads), startRow = refs$headers + 1, startCol = 2, colNames = F)

## value types
custWriteData(t(subheads), startRow = refs$headers + 2, startCol = 2, colNames = F)
custAddStyle(styles$black_11, rows = (refs$headers + 1):(refs$headers + 2), cols = 2:19)

# data
custWriteData(excel_format_data, startRow = refs$data, startCol = 1, colNames = F)
custAddStyle(styles$black_nowrap_11, rows = refs$data:refs$data_end, cols = 1:19)

# adding lines/overall style
custAddStyle(styles$a_centre, rows = refs$headers:refs$data_end, cols = 2:19)
custAddStyle(styles$b_left_bold, rows = refs$headers:refs$data_end, cols = c(1, 2, 8, 14, 20))
custAddStyle(styles$b_left, rows = (refs$headers + 1):refs$data_end, cols = c(3, 5, 7, 9, 11, 13, 15, 17, 19))
custAddStyle(styles$b_top_bold, rows = c(refs$headers, refs$data, refs$data_end + 1), cols = 1:19)
custAddStyle(styles$b_top, rows = c(refs$headers + 1, refs$headers + 2), cols = 2:19)

# adding conditional formatting
add_cond_format(2)
add_cond_format(4)
add_cond_format(6)
add_cond_format(8)
add_cond_format(10)
add_cond_format(12)
add_cond_format(14)
add_cond_format(16)
add_cond_format(18)

# Save output
query_saveWorkbook(wb, paste0(output_path, "/audit_kpi13a_difference.xlsx"))



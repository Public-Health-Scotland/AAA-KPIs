#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5_2_kpi_1_4_join_tables.R
# Eibhlin O'Sullivan
# Jan 2022
# Reformats output tables and sent to excel
# Written/run on R Studio Server
# R version 3.6.1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# There are three scripts for kpi 1.4A to be run together:
# 1) 3_2_kpi_1_4_surveillance.R 
# 2) 4_2_kpi_1_4_surveillance_assess_recovery.R - This may be removed as it is related to COVID Recovery
# 3) 5_2_kpi_1_4_join_tables.R - Joins the output of 1 and 2 into the report format and outputs a CSV - This script

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

rm(list = ls())
gc()


# Name objects
yymm <- 202303
temp_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", yymm, "/temp")

# Read in data
kpi_1.4a <- readRDS(paste0(temp_path, "/kpi_1_4_a_updated.rds"))
kpi_1.4b <- readRDS(paste0(temp_path, "/kpi_1_4_b_updated.rds"))
kpi_14a_assess <- readRDS(paste0(temp_path, "/kpi_1_4a_assess_updated.rds"))
kpi_14b_assess <- readRDS(paste0(temp_path, "/kpi_1_4b_assess_updated.rds"))

### 2 - Reformat KPI outputs ----

## KPI 1.4a --
kpi1.4a_final <- kpi_1.4a %>%
  left_join(kpi_14a_assess, by = c("fy_due", "hbres")) %>% 
  rename(appointments_due_12m_n = `sum(cohort_ac).x`,
         tested_within_12w_due_n = `sum(attend).x`,
         tested_within_12w_due_pc = pc.x,
         all_tested_12m_n = `sum(attend).y`,
         all_tested_12m_pc = pc.y) %>% 
  select(-`sum(cohort_ac).y`) %>%
  glimpse()


## KPI 1.4b --
kpi1.4b_final <- kpi_1.4b %>%
  left_join(kpi_14b_assess, by = c("fy_due", "hbres")) %>% 
  rename(appointments_due_3m_n = `sum(cohort_qc).x`,
         tested_within_120d_due_n = `sum(attend).x`,
         tested_within_120d_due_pc = pc.x,
         all_tested_3m_n = `sum(attend).y`,
         all_tested_3m_pc = pc.y) %>% 
  select(-`sum(cohort_qc).y`) %>%
  glimpse()


### Output to RDS
saveRDS(kpi1.4a_final, paste0(temp_path, "/kpi_1_4a_final_updated.rds"))
saveRDS(kpi1.4b_final, paste0(temp_path, "/kpi_1_4b_final_updated.rds"))

## re-write to save out as a single RDS file; will require renaming variables.


### 3 - Write to Excel ----

# Create workbook

wb <- createWorkbook()

# Define a header style for workbook

hs <- createStyle(halign = "center", valign = "center", 
                  textDecoration = "bold", border = "TopBottomLeftRight")

## 3.1 - Create tab KPI1.4a --

addWorksheet(wb, sheetName = "KPI1.4a", gridLines = FALSE)

# Add Titles
writeData(wb, sheet = "KPI1.4a", paste0("12 month follow up recommendation"),
          startCol = 1, startRow = 1)

writeData(wb, sheet = "KPI1.4a", kpi1.4a_final, borders = "all", headerStyle = hs, startCol = 1, startRow = 4)

setColWidths(wb, sheet = "KPI1.4a", cols = 1:16, widths = "auto")

## 3.2 - Create tab KPI1.4b --

addWorksheet(wb, sheetName = "KPI1.4b", gridLines = FALSE)

# Add Titles
writeData(wb, sheet = "KPI1.4b", paste0("3 month follow up recommendation"),
          startCol = 1, startRow = 1)

writeData(wb, sheet = "KPI1.4b", kpi1.4b_final, borders = "all", headerStyle = hs, startCol = 1, startRow = 4)

setColWidths(wb, sheet = "KPI1.4b", cols = 1:16, widths = "auto")

## 3.2 - Save Workbook --

saveWorkbook(wb, paste0(temp_path,"/kpi1_4_updated.xlsx"), overwrite = TRUE)





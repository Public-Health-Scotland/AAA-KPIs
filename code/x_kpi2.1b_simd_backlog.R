# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# x_kpi2.1b_simd_backlog.R
# Aoife McCarthy
# 2024/09/25
#
# Written on Posit WB, R Version 4.1.2
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# code copied over from 4_3_kpi_2.R, adjusting to include 2021/22 and 2022/23 metric
# data used is still the most recent extract (202409)

# loading packages
library(readr)
library(dplyr)
library(phsmethods)
library(janitor)
library(lubridate)
library(forcats)
library(stringr)
library(tidylog)
library(tidyr)
library(phsaaa) # to install: devtools::install_github("aoifem01/phsaaa")


rm(list = ls())
gc()


source(here::here("code/0_housekeeping.R"))

rm (exclusions_path, output_path, simd_path, fy_list, hb_list, fy_tibble,
    qpmg_month, cut_off_date, cutoff_date, year1_end, year1_start, year2_end, 
    year2_start, year1, year2, extract_date)

# adjusting housekeeping variables for 2021/22 - 2022/23 period
start_date <- "2021-04-01" # was "2023-04-01"
end_date <- "2023-03-31" # was "2024-03-31"

# SIMD levels
simd_levels <- c("1", "2", "3", "4", "5", "Unknown")

# table for right join later on
hb_simd <- crossing(hb_tibble, simd_levels, tibble(financial_year = c("2021/22", "2022/23"))) |> 
  rename(simd = simd_levels,
         hb_screen = hbres)


#### Step 2: Read in data ----
extract <- read_rds(extract_path)

#### Step 3: Create summary tables ----
# Create relevant subset of data
extract2 <- extract %>%
  # filter for relevant dates and keep positive, negative and non-vis screens
  filter(between(date_screen, as.Date(start_date), as.Date(end_date)),
         screen_result %in% c('01','02','04')) %>%
  mutate(non_vis_n = if_else(screen_result == '04', 1, 0),
         screened_n = if_else(screen_result %in% c('01','02','04'), 1, 0),
         # year when patient turned 66
         fin_year_66 = extract_fin_year(dob + years(66))
         # ,
         # # flags screens performed with new devices
         # device = if_else(date_screen > ymd(device_swap_date), "new", "old")
         ) 


# kpi 2.1b Scotland SIMD --------------------------------------------------

# need financial years 2021/22 and 2022/23

# deduplicate extract
extract2_dedup_hb <- extract2 %>%
  # GC - keeps non-visualised if there is one
  arrange(upi, financial_year, hb_screen, desc(non_vis_n)) %>%
  distinct(upi, financial_year, hb_screen, .keep_all = TRUE)

extract2_dedup_scotland <- extract2 %>%
  # GC - keeps non-visualised if there is one
  arrange(upi, financial_year, desc(non_vis_n)) %>%
  distinct(upi, financial_year, .keep_all = TRUE)

# KPI 1.2b Scotland SIMD - 
# Percentage of MEN screened where aorta could not be visualised
# Denominator = The number of MEN attended screening, excluding technical failure
# Numerator = Number of MEN with at least 1 screen where the aorta could not be 

kpi_2_1b_hb_simd <- extract2_dedup_hb %>%
  group_by(financial_year, hb_screen, simd2020v2_sc_quintile) %>%
  summarise(non_vis_n = sum(non_vis_n),
            screen_n = sum(screened_n)) %>%
  ungroup()

kpi_2_1b_scotland_simd <- extract2_dedup_scotland %>%
  mutate(hb_screen = "Scotland") %>%
  group_by(financial_year, hb_screen, simd2020v2_sc_quintile) %>% 
  summarise(non_vis_n = sum(non_vis_n),
            screen_n = sum(screened_n)) %>%
  ungroup()

kpi_2_1b_simd <- bind_rows(kpi_2_1b_scotland_simd, kpi_2_1b_hb_simd) %>%
  mutate(hb_screen = fct_relevel(as.factor(hb_screen), "Scotland"),
         non_vis_p = round_half_up(non_vis_n/screen_n * 100, 1),
         kpi = "KPI 2.1b SIMD",
         simd2020v2_sc_quintile = replace_na(as.character(simd2020v2_sc_quintile), "Unknown")) |> 
  select(hb_screen, kpi, financial_year, simd = simd2020v2_sc_quintile, screen_n, non_vis_n, non_vis_p) |> 
  right_join(hb_simd) |> 
  pivot_longer(!hb_screen:simd, 
               names_to = "group", values_to = "value") |> 
  arrange(hb_screen, financial_year, group, simd)

rm(kpi_2_1b_hb_simd, kpi_2_1b_scotland_simd)

# formatting output - to match historical db

output <- kpi_2_1b_simd |> 
  rename(fin_year = financial_year,
         hbres = hb_screen) |> 
  droplevels()


# saving output -----------------------------------------------------------

query_write_rds(kpi_2_1b_simd, paste0(temp_path, "/3_1_kpi_2_1b_simd_backlog(2021-2023).rds"))

# adding to historical db -------------------------------------------------

hist_db <- read_rds(paste0(hist_path, "/aaa_kpi_historical_theme3.rds"))

# add in SIMD column to historical db
hist_db <- hist_db |> 
  mutate(simd = NA, .before = "group")

new_hist <- add_new_rows(hist_db, output, kpi, fin_year)

# save out new hist_db (overwrite current one)
viz_kpi_finyear(hist_db)
viz_kpi_finyear(new_hist)

query_write_rds(new_hist, paste0(hist_path, "/aaa_kpi_historical_theme3.rds"))

# add to backup hist too
hist_bckp <- read_rds(paste0(hist_path, "/aaa_kpi_historical_theme3_bckp.rds"))
viz_kpi_finyear(hist_bckp)

viz_kpi_finyear(new_hist)
new_bckp <- new_hist |> 
  filter(!fin_year == "2022/23")
viz_kpi_finyear(new_bckp)

query_write_rds(new_bckp, paste0(hist_path, "/aaa_kpi_historical_theme3_bckp.rds"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x_kpi1.1_simd_backlog.R
# 
# Aoife McCarthy
# Jul 2024
# 
# Creating historical data for KPI 1.1 SIMD, adding this to historical backup
# 
# Written/run on Posit WB, R 4.1.2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# housekeeping ------------------------------------------------------------

library(readr)
library(dplyr)
library(lubridate)
library(phsmethods)
library(stringr)
library(forcats)
library(tidylog)
library(phsaaa)

rm(list = ls())
gc()


source(here::here("code/0_housekeeping.R"))

rm (exclusions_path, extract_path, output_path, fy_tibble,
    cut_off_date, cutoff_date, end_current, end_date, start_date,
    extract_date, hb_list, fy_list, qpmg_month, yymm, year1, year1_end,
    year1_start, year2, year2_end, year2_start)

# SIMD levels
simd_level <- tibble(simd = c("Total", "1","2","3", "4", "5", "Unknown"))


# data input --------------------------------------------------------------

invite_uptake <- read_rds(paste0(temp_path, "/1_1_invite_uptake_initial.rds"))

pc_simd <- read_rds(simd_path) |>
  select(pc8, simd2020v2_hb2019_quintile)


# create years variables --------------------------------------------------

# list of calendar years in which offers have been sent
yrs_offers_list <- invite_uptake |> 
  filter(!is.na(date_first_offer_sent)) |> 
  mutate(yr_offer = as.numeric(substr(as.character(date_first_offer_sent), 1, 4))) |> 
  select(yr_offer) |> 
  unique() |> 
  filter(yr_offer>2000) |>
  bind_rows(tibble(yr_offer =  2025)) |> # use for older and 2024/25
  arrange(yr_offer)
  

# 2012/13 up to 2024/25 needed for kpi 1.1
# yrs_offers indicate 2nd year of FYs, e.g. 2013 = 2012/13
# need to create start and end dates for each of these

yrs_offers <- yrs_offers_list |> 
  mutate(year_start = paste0("01-04-", as.character((yr_offer-67))),
         year_end = paste0("31-03-", as.character((yr_offer-66)))) |> 
  mutate(fy_offer = paste0(as.character(yr_offer-1),
                           "/",
                           substr(as.character(yr_offer), 3, 4))) |> 
  mutate(yr_cohort = paste0("year", substr(fy_offer, 3,4), substr(fy_offer, 6,7))) |> 
  mutate(yr_offer = paste0("offer_", yr_cohort),
#         yr_test = paste0("tested_", yr_cohort)
) |> 
  select(fy_offer, year_start, year_end, yr_cohort, yr_offer #, yr_test
         )

# # adding in row for people who were "older than 65" when programme began
# yrs_offers <- yrs_offers |> 
#   bind_rows(tibble(fy_offer = "Older",
#                    year_start = "01-01-1800",
#                    year_end = "31-03-1945",
#                    yr_cohort = "year0000",
#                    yr_offer = "offer_year0000",
# #                   yr_test = "tested_year0000"
# ))


rm(yrs_offers_list)

# deriving kpi 1.1 --------------------------------------------------------
# kpi 1.1: # people invited by 66y

# kpi 1.1  
invite_uptake_cohorts <- invite_uptake %>%
  # calculate age at screening (in months as 66 and 3 months is the key age)
  mutate(age_screen = age_calculate(dob, screen_date, units = "months")) %>%
  # calculate age at offer (in years as 66 is the key age)
  mutate(age_offer = interval(dob, date_first_offer_sent) %/% years(1)) |> 
  # add cohort column to id each year's cohort
  mutate(cohort = "unassigned")

# Denominator: eligible population for each financial year
# assigning each row to a cohort ("yearXXYY")
for (i in 1:nrow(yrs_offers)) {
  invite_uptake_cohorts <- invite_uptake_cohorts |> 
    mutate(cohort = ifelse(between(invite_uptake_cohorts$dob, dmy(yrs_offers$year_start[i]), dmy(yrs_offers$year_end[i])),
                           yrs_offers$yr_cohort[i],
                           cohort))
  rm(i)
}

# flags for cohort
invite_uptake_cohorts <- invite_uptake_cohorts |> 
  mutate(cohort_flag = ifelse(!is.na(cohort), 1, 0)) |> 
  arrange(cohort)

# rearranging to have cohort as column names
invite_uptake_offers <- invite_uptake_cohorts |> 
  pivot_wider(names_from = cohort, values_from = cohort_flag) |> 
  select(-unassigned)

# Numerator: eligible individuals sent initial offer to screening during current 
# analysis year and current active year (offered before age 66)
for (i in 1:nrow(yrs_offers)) {
  yr_offer_col <- sym(yrs_offers$yr_offer[i])
  yr_cohort_col <- sym(yrs_offers$yr_cohort[i])
  
  invite_uptake_offers <- invite_uptake_offers %>%
    mutate(!!yr_offer_col := case_when(
      !!yr_cohort_col == 1 & inoffer == 1 & age_offer < 66 ~ 1,
      TRUE ~ as.numeric(NA)))
  
  rm(yr_offer_col, yr_cohort_col, i)
}


### AMc note: is this additional management information useful by SIMD? probably not

# # Additional management information (COVID recovery related -- not KPI data)
# # Secondary numerator: eligible individuals offered any time before 1 Sept 
# # (after age 66)
# invite_uptake <- invite_uptake %>%
#   mutate(offer_add_year1 = case_when(cohort_year1 == 1 & inoffer == 1 ~ 1,
#                                      TRUE ~ as.numeric(NA)),
#          offer_add_year2 = case_when(cohort_year2 == 1 & inoffer == 1 ~ 1,
#                                      TRUE ~ as.numeric(NA)))

# JOINING PC/SIMD DATA #

invite_uptake_simd <- invite_uptake_offers %>% 
  left_join(pc_simd, by = c("postcode" = "pc8")) |>
  relocate(simd2020v2_hb2019_quintile, .before = hbres)

rm(pc_simd, simd_path, invite_uptake_cohorts, invite_uptake_offers)


# kpi 1.1 summary ---------------------------------------------------------

# kpi_1_1_initial <- invite_uptake_simd  |> 
#   select(hbres, year1112:offer_year2425) |> 
#   group_by(hbres) |> 
#   summarise(across(year1112:offer_year2425, sum, na.rm = TRUE)) %>%
#   group_modify(~ janitor::adorn_totals(.x, where = "row", name = "Scotland")) %>% 
#   ungroup() |> 
#   glimpse()
# 
# kpi_1_1_cov <- kpi_1_1_initial
# 
# # calculate coverage
# for (i in 1:nrow(yrs_offers)){
#   coverage_col <- sym(paste0("coverage_", yrs_offers$yr_cohort[i]))
#   offer_col <- sym(yrs_offers$yr_offer[i])
#   cohort_col <- sym(yrs_offers$yr_cohort[i])
#   
#   kpi_1_1_cov <- kpi_1_1_cov |> 
#     mutate(!!coverage_col := round(((!!offer_col/!!cohort_col)*100), digits = 1))
#   
#   rm(i, coverage_col, offer_col, cohort_col)
# }
# 
# # Reformat to match historical data
# kpi_1_1 <- kpi_1_1_cov |> 
#   pivot_longer(!hbres, names_to = "group", values_to = "value") |> 
#   mutate(yr_cohort = paste0("year", substr(group, (nchar(group)-3), nchar(group))),
#          kpi = "KPI 1.1") |> 
#   left_join(yrs_offers) |> 
#   select(- c(year_start, year_end, yr_offer,yr_cohort)) |> 
#   mutate(group = case_when(str_detect(group, "offer") ~ "offer_n",
#                            str_detect(group, "coverage") ~ "coverage_p",
#                            TRUE ~ "cohort_n"),
#          simd = NA,
#          fin_year = fy_offer) |> 
#   select(hbres, kpi, fin_year, simd, group, value)
# 
# kpi_1_1 <- hb_tibble |> left_join(kpi_1_1, by = "hbres")
# 
# rm(kpi_1_1_initial, kpi_1_1_cov)

# kpi 1.1 by Scotland-level SIMD ------------------------------------------

# KPI 1.1: individuals offered screen before 66 (+ additional COVID info)
# grouped by Scotland-level SIMD quintiles
kpi_1_1_simd_initial <- invite_uptake_simd  |> 
  select(hbres, simd2020v2_sc_quintile, year1112:offer_year2425) |> 
  group_by(hbres, simd2020v2_sc_quintile) |> 
  summarise(across(year1112:offer_year2425, sum, na.rm = TRUE)) |> 
  group_modify(~ janitor::adorn_totals(.x, where = "row", name = "Total")) |>  
  ungroup() |> 
  glimpse()

# Scotland
kpi_1_1_simd_scot <- invite_uptake_simd  |> 
  select(simd2020v2_sc_quintile, year1112:offer_year2425) |> 
  group_by(simd2020v2_sc_quintile) |> 
  summarise(across(year1112:offer_year2425, sum, na.rm = TRUE)) |> 
  group_modify(~ janitor::adorn_totals(.x, where = "row", name = "Total")) |>  
  ungroup() |> 
  mutate(hbres = "Scotland", .before = simd2020v2_sc_quintile) |> 
  glimpse()

# Combine & order by SIMD
kpi_1_1_simd_comb <- bind_rows(kpi_1_1_simd_scot, kpi_1_1_simd_initial) |> 
  mutate(simd2020v2_sc_quintile = if_else(is.na(simd2020v2_sc_quintile), 
                                          "Unknown", simd2020v2_sc_quintile))
kpi_1_1_simd_comb <- simd_level |> left_join(kpi_1_1_simd_comb, 
                                        by = c("simd" = "simd2020v2_sc_quintile"))

kpi_1_1_simd_cov <- kpi_1_1_simd_comb

for (i in 1:nrow(yrs_offers)){
  coverage_col <- sym(paste0("coverage_", yrs_offers$yr_cohort[i]))
  offer_col <- sym(yrs_offers$yr_offer[i])
  cohort_col <- sym(yrs_offers$yr_cohort[i])
  
  kpi_1_1_simd_cov <- kpi_1_1_simd_cov |> 
    mutate(!!coverage_col := round(((!!offer_col/!!cohort_col)*100), digits = 1))
  
  rm(i, coverage_col, offer_col, cohort_col)
}

# Reformat to match historical data
kpi_1_1_simd <- kpi_1_1_simd_cov |> 
  pivot_longer(!hbres:simd, names_to = "group", values_to = "value") |> 
  mutate(yr_cohort = paste0("year", substr(group, (nchar(group)-3), nchar(group))),
         kpi = "KPI 1.1 SIMD") |> 
  left_join(yrs_offers) |> 
  select(- c(year_start, year_end, yr_offer,yr_cohort)) |> 
  mutate(group = case_when(str_detect(group, "offer") ~ "offer_n",
                           str_detect(group, "coverage") ~ "coverage_p",
                           TRUE ~ "cohort_n"),
         fin_year = fy_offer) |> 
  select(hbres, kpi, fin_year, simd, group, value)

kpi_1_1_simd <- hb_tibble |> left_join(kpi_1_1_simd, by = "hbres")

rm(kpi_1_1_simd_scot, kpi_1_1_simd_cov, kpi_1_1_simd_initial, kpi_1_1_simd_comb)

# reading in historical data ----------------------------------------------

hist_db_bckp <- read_rds(paste0(hist_path, "/aaa_kpi_historical_theme2_bckp.rds"))

hist_db <- read_rds(paste0(hist_path, "/aaa_kpi_historical_theme2.rds"))

table(hist_db$fin_year, hist_db$kpi)
table(hist_db_bckp$fin_year, hist_db_bckp$kpi)

# creating new historical with KPI 1.1 SIMD -------------------------------

# hist_db for 202309 addition
kpi_1_1_simd_hist <- kpi_1_1_simd |> 
  filter(fin_year %in% c("2019/20", "2020/21", "2021/22", "2022/23"))

new_hist_simd <- hist_db |> 
  add_new_rows(kpi_1_1_simd_hist, kpi, fin_year)

table(new_hist_simd$fin_year, new_hist_simd$kpi)

# hist_db_bckp for 202309 addition
kpi_1_1_simd_hist_bckp <- kpi_1_1_simd |> 
  filter(fin_year %in% c("2019/20", "2020/21", "2021/22"))

new_hist_bckp_simd <- hist_db_bckp |> 
  add_new_rows(kpi_1_1_simd_hist_bckp, kpi, fin_year)

table(new_hist_bckp_simd$fin_year, new_hist_bckp_simd$kpi)

# save output -------------------------------------------------------------

write_rds(new_hist_simd, paste0(hist_path, "/aaa_kpi_historical_theme2_kpi1.1simd.rds"))
write_rds(new_hist_bckp_simd, paste0(hist_path, "/aaa_kpi_historical_theme2_bckp_kpi1.1simd.rds"))

##########################################################
# x_historical_updates.R
# Aoife McCarthy
# 19/07/2024
#
# Renaming `financial_year` col of theme 4 to `fin_year`
#
# Written/run on R Studio Server, R version 3.6.1
# Revised on Posit WB, R Version 4.1.2
##########################################################

# housekeeping ------------------------------------------------------------

# packages
library(readr)
library(dplyr)

rm(list = ls())
gc()

# filepaths
hist_path <- "/PHI_conf/AAA/Topics/Screening/KPI/historical/202406_backup" # temp directory - will change back to just historical folder once things sorted
new_hist_path <- "/PHI_conf/AAA/Topics/Screening/KPI/historical/202406_backup_finyear"

# data load ---------------------------------------------------------------

prev_theme4_hist <- read_rds(paste0(hist_path, "/aaa_kpi_historical_theme4.rds"))

prev_theme4_hist_bckp <- read_rds(paste0(hist_path, "/aaa_kpi_historical_theme4_bckp.rds"))

# wrangling ---------------------------------------------------------------

## looking at old data ----

table(prev_theme4_hist$kpi, prev_theme4_hist$financial_year) # financial year
                    # 2012/13 2013/14 2014/15 2015/16 2016/17 2017/18 2018/19 2019/20 2020/21 2021/22 2022/23 2023/24
# KPI 3.1 Residence      45      45      45      45      45      45      45      45      45      45      45      45
# KPI 3.2 Residence      45      45      45      45      45      45      45      45      45      45      45      45
# KPI 3.2 Surgery         9      27      30      30      30      30      30      27      24      24      24      24

table(prev_theme4_hist_bckp$kpi, prev_theme4_hist_bckp$financial_year) # financial_year

                    # 2012/13 2013/14 2014/15 2015/16 2016/17 2017/18 2018/19 2019/20 2020/21 2021/22 2022/23 2023/24
# KPI 3.1 Residence      45      45      45      45      45      45      45      45      45      45      45      45
# KPI 3.2 Residence      45      45      45      45      45      45      45      45      45      45      45      45
# KPI 3.2 Surgery         9      27      30      30      30      30      30      27      24      24      24      24

# AMc note - bckp should only have up to 2021/22, hist should only have up to 2022/23

## building new data ----

new_theme4_hist <- prev_theme4_hist |> 
  filter(!financial_year == "2023/24") |> 
  rename(fin_year = financial_year,
         hbres == health_board)
table(new_theme4_hist$kpi, new_theme4_hist$fin_year)
                    # 2012/13 2013/14 2014/15 2015/16 2016/17 2017/18 2018/19 2019/20 2020/21 2021/22 2022/23
# KPI 3.1 Residence      45      45      45      45      45      45      45      45      45      45      45
# KPI 3.2 Residence      45      45      45      45      45      45      45      45      45      45      45
# KPI 3.2 Surgery         9      27      30      30      30      30      30      27      24      24      24

new_theme4_hist_bckp <- prev_theme4_hist_bckp |> 
  filter(!financial_year %in% c("2022/23", "2023/24")) |> 
  rename(fin_year = financial_year)
table(new_theme4_hist_bckp$kpi, new_theme4_hist_bckp$fin_year)
                    # 2012/13 2013/14 2014/15 2015/16 2016/17 2017/18 2018/19 2019/20 2020/21 2021/22
# KPI 3.1 Residence      45      45      45      45      45      45      45      45      45      45
# KPI 3.2 Residence      45      45      45      45      45      45      45      45      45      45
# KPI 3.2 Surgery         9      27      30      30      30      30      30      27      24      24

# AMc note: looks better

# save outputs ------------------------------------------------------------

write_rds(new_theme4_hist, paste0(new_hist_path, "/aaa_kpi_historical_theme4.rds"))
write_rds(new_theme4_hist_bckp, paste0(new_hist_path, "/aaa_kpi_historical_theme4_bckp.rds"))


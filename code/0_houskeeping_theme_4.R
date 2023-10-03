#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 0_houskeeping_theme_4.R
# Karen Hotopp
# Oct 2023
# 
# Define housekeeping variables used by subsequent scripts
# 
# Written/run on R Posit PWB
# R version 4.1.2
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


## Ubiquitous Variables
yymm <- 202309

# year_one <- "2020/21"
# year_two <- "2021/22"
# year_three <- "2022/23"
cut_off_date <- as.Date("2023-03-31") # How is this date defined?? potential match: 6_2_Suppl_Surve.R
meg_month <- "May"

# Years needed from the historical extract for current KPI report
# This includes the three most recent *complete* years of data (including 
# current year of analysis) and the year in current screening process)
kpi_report_years <- c("2020/21", "2021/22", "2022/23") 

# hbres_list
hb_list <- tibble(hbres = c("Scotland","Ayrshire & Arran","Borders",
                            "Dumfries & Galloway", "Fife", "Forth Valley", 
                            "Grampian", "Greater Glasgow & Clyde", "Highland", 
                            "Lanarkshire", "Lothian", "Orkney",
                            "Shetland", "Tayside","Western Isles"))


## Ubiquitous Pathways
extract_path <- paste0("/PHI_conf/AAA/Topics/Screening/extracts/", yymm,
                       "/output/aaa_extract_", yymm, ".rds")

exclusions_path <- paste0("/PHI_conf/AAA/Topics/Screening/extracts/", yymm, 
                          "/output/aaa_exclusions_", yymm, ".rds")

temp_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", yymm,
                    "/temp")

hist_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/historical")

output_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", yymm,
                      "/temp")





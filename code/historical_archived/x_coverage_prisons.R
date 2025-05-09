# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x_coverage_prisons.R
# Karen Hotopp & Salomi Barkat
# November 2023
#
# KPI 1.2a/1.2b - Prisoners
# Coverage/uptake rates for men who were registered with the Prison GP 
# practice code (N3139)
#
# Written/run on Posit WB, R 4.1.2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Notes:
# This script is only run in the autumn and uses AAA GP extract downloaded
# from BOXI. 


### 1: Housekeeping ----
## Packages
library(readr)
library(dplyr)
library(phsmethods)
library(lubridate)
library(janitor)
library(openxlsx)
library(tidylog)
library(phsaaa)

rm(list = ls())
gc()


source(here::here("code/0_housekeeping.R"))

rm(cutoff_date, hb_list, hist_path, simd_path,
   year1, year1_start, year2, year2_end, year2_start)

financial_year <- kpi_report_years[3]

cutoff_date <- year1_end ## AMc note: this date not verified, was 31-03-1957 when came to do 202409 autumn run

rm(year1_end)

# date of GP history extract
# used to create valid_to date for queries (not sure what queries)
date_valid_to <- "02-10-2024"

fy_start <- start_date
fy_end <- end_date

rm(start_date, end_date)


## Filepaths
gp_history_path_a <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", yymm, "/data/",
                          "GP_Practice_History_with_dob_selection_-_prior_to_1_4_1952.csv")

gp_history_path_b <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", yymm, "/data/",
                            "GP_Practice_History_with_dob_selection_-_post_1_4_1952.csv")

gp_lookup_path <- paste0("/conf/linkage/output/lookups/Unicode/",
                         "National Reference Files/gpprac.csv")

prev_gp_data_path <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/", yymm - 100,
                            "/data/gp_coverage_2122.rds")


# #location of processing file (used for checks)
# processing <- '/PHI_conf/AAA/Topics/AAAScreening/Publications/AAA Screening Programme Statistics/20210302/Temp/Processing/Exclusion Files/'



### 2: GP History ----
# Read in GP practice history and identify men registered with prison practice 
# code (N3139).
# Use the variables valid_from and valid_to to identify length of registration 
# with prison practice code.

# pre-1952 birthdays GP history
gp_history_a <- read_csv(gp_history_path_a) |> 
  rename(upi = Upinumber,
         hb_residence = `Area of Residence`,
         practice_code = `Practice Code`,
         valid_from = `Valid from`,
         valid_to = `Valid to`)

# post-1952 birthdays GP history
gp_history_b <- read_csv(gp_history_path_b) |> 
  rename(upi = Upinumber,
         hb_residence = `Area of Residence`,
         practice_code = `Practice Code`,
         valid_from = `Valid from`,
         valid_to = `Valid to`)

# full extract
gp_history <- bind_rows(gp_history_a, gp_history_b) |> 
  filter(practice_code == "N3139") |>
  mutate(upi = as.character(upi),
         upi = chi_pad(upi)) |> 
  # simplify dates and turn into date variables
  mutate(valid_from = ymd_hms(valid_from),
         valid_to = ymd_hms(valid_to)) |> 
  mutate(valid_from = date(valid_from),
         valid_to = date(valid_to))

# Check that all CHI are valid
chi_check(gp_history$upi)

rm(gp_history_a, gp_history_b) # tidy env


# Re-code valid_to to allow "status" queries
gp_history <- gp_history |> 
  mutate(valid_to = if_else(is.na(valid_to), dmy(date_valid_to), valid_to),
         prison_days = interval(valid_from, valid_to) %/% days(1)) |> 
  arrange(upi, valid_from, valid_to)

# If the upi number of the previous episode is the same and the end date of the 
# previous episode is within 1 day of the start date of the next episode, then 
# use the initial episode start date as the start date

# Create a variable to assess whether the end date of the previous episode is 
# within 1 day of the start date of the next episode

#first create a lagged version of valid to
practice_history <- gp_history |> 
  mutate(valid_to_lagged = lag(valid_to))

#extract first valid from date for each person
practice_history <- practice_history |> 
  group_by(upi) %>%
  mutate(valid_from_initial = first(valid_from)) %>%
  ungroup()

#calculate date diff between valid from and valid to lag
practice_history <- practice_history |> 
  mutate(datediff = interval(valid_from, valid_to_lagged) %/% days(1))

#if end date is within 1 day of start of next episode, use initial episode start date
practice_history <- practice_history |> 
  mutate(valid_from = case_when(lag(upi, 1L) == upi & 
                                  datediff <= 1 ~ lag(valid_from_initial),
                                TRUE ~ valid_from))

#note that spss suggests valid from was recorded to the lagged valid from, but investigation showed
#that for anyone with 3 or more episodes who met the conditions, spss was taking the initial valid from 
#date, not the lagged valid from date. So have coded to do the same here.


#aggregate to get the last area of residence
practice_history <- practice_history |>
  group_by(upi, valid_from) %>%
  summarise(hb_residence = last(hb_residence),
            valid_to = last(valid_to))  |> 
  ungroup()


### 3 - Match screening data ----
#read in invite and uptake rates file created for recent QPMG report (this file is one record
#per UPI with the date of their first invite and the scrn_date they were first 'tested')
#men_screened <- 
invite_uptake <- read_rds(paste0(temp_path, "/1_2_coverage_basefile.rds"))


#join men screened file with men who were registered with prison practice
joined <- practice_history |> 
  left_join(invite_uptake, by = "upi") |> 
  # calculate length of time registered at prison
  mutate(registration_length = interval(valid_from, valid_to) %/%  "months"(1)) |> 
  # group by 6-month periods
  mutate(registration_length_gp = case_when(
    registration_length >=0 & registration_length <=6 ~ 1,
    registration_length >=7 & registration_length <=12 ~ 2,
    registration_length >=13 & registration_length <=18 ~ 3,
    registration_length >=19 & registration_length <=24 ~ 4,
    registration_length >=25 & registration_length <=36 ~ 5,
    registration_length >=37 ~ 6)) |> 
  mutate(total_offer = 1)

## Remove men too young to have been offered screening
joined <- joined |> 
  mutate(dob_temp = dob_from_chi(upi)) |> 
  filter(dob_temp <= dmy(cutoff_date))


###
## Check eligibility
table(joined$dob_eligibility, useNA = "ifany")
# Small number of men (15) with no dob_eligibility 
# Need to check against exclusions extract

check <- joined[is.na(joined$dob_eligibility),]

check_upi <- c(check$upi)

# Read in exclusions extract to compare 
exclusion <- read_rds(exclusions_path)
exclusion_upi <- exclusion |> 
  filter(upi %in% check_upi)

table(exclusion_upi$upi)
# All but 1 UPI appear on exclusions list (and still open)
rm(check, check_upi, exclusions_path, exclusion, exclusion_upi)
###

joined <- joined |> 
  filter(!(is.na(dob_eligibility))) 

length(unique(joined$upi))

#note some men may have more than one record at this stage
#create a variable to count totals
joined <- joined |> 
  mutate(cohort = 1)


### Discard data for previous years as can replicate the cohort in previous 
# years based on the current file 
# (particularly the registered anytime figures)
table(joined$dob_eligibility, useNA = "ifany")

current_prisons <- joined %>%
  filter(dob_eligibility == paste0("Turned 66 in year ", financial_year)) |> 
  group_by(upi, dob_eligibility) %>%
  mutate(across(c(cohort, inoffer, inresult, age_offer, age_screen), first),
         registration_length = max(registration_length)) |> 
  ungroup()


################################################################################
### 4a - Men registered with prison practice at any time - tested within KPI timeframes ----


#numerator is tested before age 66 and 3 months regardless of when invited

# KPI 1.2a - screening coverage
anytime_reg_kpis <- current_prisons %>%
  mutate(tested_1.2a = case_when(age_screen < 795 ~ 1,
                                 TRUE ~ 0))  %>%
  #KPI 1.2b - screening uptake
  #flag if offered before age 66 (denominator for KPI1.2b)
  mutate(offered_1.2b = case_when(
    age_offer < 66 ~ 1,
    TRUE ~ 0
  )) %>%
  #flag if offered by age 66 and tested by age 66 and 3 months (i.e. age screen less than 795 days) 
  #(numerator KPI1.2b)
  mutate(tested_1.2b = case_when(
    age_offer <66 & age_screen <795 ~ 1,
    TRUE ~ 0
  ))


#aggregate to get totals
anytime_reg_kpis <- anytime_reg_kpis |> 
  group_by(dob_eligibility) %>%
  summarise(across(c(cohort, tested_1.2a, offered_1.2b, tested_1.2b), sum)) |> 
  ungroup()

#calculate percentages, rounding to 1 decimal place
anytime_reg_kpis <- anytime_reg_kpis |>
  mutate(p_1.2a = round_half_up((tested_1.2a/cohort)*100, 1),
         p_1.2b = round_half_up((tested_1.2b/offered_1.2b)*100, 1))

#reorder variables in standard order 
anytime_reg_kpis <- anytime_reg_kpis |>
  select(dob_eligibility, cohort, tested_1.2a, p_1.2a, offered_1.2b, tested_1.2b, p_1.2b)


################################################################################
### 4b - Men registered with prison practice at any time - tested anytime
#ie. includes men tested after 66 y 3m ----

anytime_reg_tested <- current_prisons %>%
  group_by(dob_eligibility) %>%
  summarise(cohort = sum(cohort), tested = sum(inresult)) %>%
  #calculate percentages and round
  mutate(p_coverage = round_half_up(tested/cohort*100, 1)) |> 
  ungroup()


################################################################################
### Flag and select if screened date or first offer date was during prison practice registration ----
#if the first_offer was sent whilst registered with prison practice then flag the cases
during_reg <- joined %>%
  mutate(firstoffer_duringreg = case_when(date_first_offer_sent >= valid_from & 
                                            date_first_offer_sent <= valid_to ~ 1,
                                          TRUE ~ 0)) |>
  #if the man was screened during registration with prison practice then flag the cases
  mutate(screened_duringreg = case_when(screen_date >= valid_from & 
                                          screen_date <= valid_to ~ 1,
                                        TRUE ~ 0))

#select cases where screening offered or tested during prison practice registration
during_reg <- during_reg |> 
  filter(firstoffer_duringreg == 1 | screened_duringreg == 1)

during_reg %>% tabyl(firstoffer_duringreg, screened_duringreg)


#all men only have one record but aggregated in case multiple records per UPI appear in future runs
during_reg <- during_reg |> 
  group_by(upi, dob_eligibility) %>%
  mutate(across(c(cohort, inoffer, inresult, age_offer, age_screen), first),
         registration_length = max(registration_length)) |> 
  ungroup()


################################################################################
### 5a - Men offered/tested during time registered at prison practice - tested within KPI timeframes ----
################################################################################

#coverage kpi 1.2a
#numerator is tested before age 66 and 3 months regardless of when invited
during_reg_kpis <- during_reg %>%
  mutate(tested_1.2a = case_when(age_screen < 795 ~ 1,
                                 TRUE ~ 0))  %>%
  #KPI 1.2b - screening uptake
  #flag if offered before age 66 (denominator for KPI1.2b)
  mutate(offered_1.2b = case_when(age_offer < 66 ~ 1,
                                  TRUE ~ 0)) %>%
  #flag if offered by age 66 and tested by age 66 and 3 months (i.e. age screen less than 795 days) 
  #(numerator KPI1.2b)
  mutate(tested_1.2b = case_when(age_offer < 66 & age_screen < 795 ~ 1,
                                 TRUE ~ 0))  

#aggregate to get totals
during_reg_kpis <- during_reg_kpis |> 
  group_by(dob_eligibility) %>%
  summarise(across(c(cohort, tested_1.2a, offered_1.2b, tested_1.2b), sum)) |> 
  ungroup()

#calculate percentages, rounding to 1 decimal place
during_reg_kpis <- during_reg_kpis |>
  mutate(p_1.2a = round_half_up((tested_1.2a/cohort)*100, 1),
         p_1.2b = round_half_up((tested_1.2b/offered_1.2b)*100, 1)) |> 
  select(dob_eligibility, cohort, tested_1.2a, p_1.2a, 
         offered_1.2b, tested_1.2b, p_1.2b)


################################################################################
### 5b - men sent first offer or screened during prison registration - tested anytime
#ie. includes men tested after 66 y 3m ----
################################################################################

during_reg_tested <- during_reg %>%
  group_by(dob_eligibility) %>%
  summarise(cohort = sum(cohort), tested = sum(inresult)) %>%
  #calculate percentages and round
  mutate(p_coverage = round_half_up(tested/cohort*100, 1)) |> 
  ungroup()



################################################################################
### 6 - Save files ----
################################################################################


# write_xlsx(list("anytime_reg_kpis" = anytime_reg_kpis, "anytime_reg_tested" = anytime_reg_tested, 
#                 "during_reg_kpis" = during_reg_kpis, "during_reg_tested" = during_reg_tested),
#            paste0(output, "prison practice screening.xlsx"))


### 6.1 Set Up Workbook ----

# Create a workbook

wb <- createWorkbook()

# Define a header and title style for workbook

hs <- createStyle(fontColour = "#ffffff", fgFill = "#0078D4",
                  halign = "center", valign = "center", 
                  textDecoration = "bold", border = "TopBottomLeftRight")

title_style <- createStyle(fontSize = 14, textDecoration = "bold")


### 6.2 Add Anyone Registered At Any Time - KPIs ----

# Add sheet for KPIs for people registered with a prison at any time
# Set column widths to auto for this sheet

addWorksheet(wb, "Registered Anytime - KPIs", gridLines = FALSE)

addStyle(wb, "Registered Anytime - KPIs", title_style, rows = 1, cols = 1)

writeData(wb, sheet = "Registered Anytime - KPIs", 
          paste0("Registered Anytime - KPIs"), 
          startCol = 1, startRow = 1)

writeData(wb, sheet = "Registered Anytime - KPIs", anytime_reg_kpis, 
          startCol = 1, startRow = 3, borders = "all", headerStyle = hs, 
          colNames = TRUE)

setColWidths(wb, sheet = "Registered Anytime - KPIs", cols = 1:7, 
             widths = "auto")


### 6.3 Add Anyone Registered At Any Time - Tested ----

# Add sheet for tested data for people registered with a prison at any time
# Set column widths to auto for this sheet

addWorksheet(wb, "Registered Anytime - Tested", gridLines = FALSE)

addStyle(wb, "Registered Anytime - Tested", title_style, rows = 1, cols = 1)

writeData(wb, sheet = "Registered Anytime - Tested", 
          paste0("Registered Anytime - Tested"), 
          startCol = 1, startRow = 1)

writeData(wb, sheet = "Registered Anytime - Tested", anytime_reg_tested, 
          startCol = 1, startRow = 3, borders = "all", headerStyle = hs, 
          colNames = TRUE)

setColWidths(wb, sheet = "Registered Anytime - Tested", cols = 1:4, 
             widths = "auto")


### 6.4 Add Anyone Offered During Registered Time - KPIs ----

# Add sheet for KPIs for people registered with a prison at any time
# Set column widths to auto for this sheet

addWorksheet(wb, "Registered During - KPIs", gridLines = FALSE)

addStyle(wb, "Registered During - KPIs", title_style, rows = 1, cols = 1)

writeData(wb, sheet = "Registered During - KPIs", 
          paste0("Registered During - KPIs"), 
          startCol = 1, startRow = 1)

writeData(wb, sheet = "Registered During - KPIs", during_reg_kpis, 
          startCol = 1, startRow = 3, borders = "all", headerStyle = hs, 
          colNames = TRUE)

setColWidths(wb, sheet = "Registered During - KPIs", cols = 1:7, 
             widths = "auto")


### 6.5 Add Anyone Offered During Registered Time - Tested ----

# Add sheet for tested data for people registered with a prison at any time
# Set column widths to auto for this sheet

addWorksheet(wb, "Registered During - Tested", gridLines = FALSE)

addStyle(wb, "Registered During - Tested", title_style, rows = 1, cols = 1)

writeData(wb, sheet = "Registered During - Tested", 
          paste0("Registered During - Tested"), 
          startCol = 1, startRow = 1)

writeData(wb, sheet = "Registered During - Tested", during_reg_tested, 
          startCol = 1, startRow = 3, borders = "all", headerStyle = hs, 
          colNames = TRUE)

setColWidths(wb, sheet = "Registered During - Tested", cols = 1:4, 
             widths = "auto")


### 6.6 Output ----

# Save workbook

saveWorkbook(wb, paste0(temp_path, "/prison_coverage.xlsx"), 
             overwrite = TRUE)


################################################################################
### 7 - quick check of exclusions ----
#function to work through each exclusion file and check if anyone in prison 
#file appears in exclusions

##!! Moved higher up in script and changed method. 
##Should be able to delete this section.

# exclusion_check <- function(exclusion_name) {
#   
#   # read in exclusion file
#   exclusion <- read_rds(exclusions_path)
#     # add a variable to say what the exclusion is
#     mutate(exclusion = exclusion_name)
#   
#   #join with prisoners file
#   check <- left_join(practice_history, exclusion, by = "upi")
#   
#   #check number of exclusions
#   check <- table(check$exclusion)
#   print(check)
#   
# }
# 
# check <- exclusions(exclusion_name = "tempresidents_gana")
# check <- exclusions(exclusion_name = "externalresult")
# check <- exclusions(exclusion_name = "alreadyonsurveillance")
# check <- exclusions(exclusion_name = "deceased date")
# check <- exclusions(exclusion_name = "priorscreen date")
# check <- exclusions(exclusion_name = "optedoutinitial_noenddate date")
# check <- exclusions(exclusion_name = "repaired date")
# check <- exclusions(exclusion_name = "undersurveillancevascular date")
# check <- exclusions(exclusion_name = "referredvascular_nonroutine date")
# check <- exclusions(exclusion_name = "unfit_for_scanning_nfr date")
# check <- exclusions(exclusion_name = "permanentexclusion_other date")




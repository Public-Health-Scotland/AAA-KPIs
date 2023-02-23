# ~~~~~~~~~~~~~~~~~~~~~~~~~
# x_coverage_gp_practice.R
# Angus Morton
# 01/02/2023
#
# Coverage and uptake rates for men who were registered
# with the Prison GP practice code
#
# Written on RServer (R Version 3.6.1)
# ~~~~~~~~~~~~~~~~~~~~~~~~~

### Step 1 : load packages and filepaths ----

library(readr)
library(haven)
library(dplyr)
library(tidylog)
library(lubridate)


# This should be the only step which needs edited each time

invite_uptake_fpath <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/",
                              "temp/KPIs/KPI1.1 - KPI1.3/",
                              "inviteanduptake_initial.rds")

output_fpath <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/",
                       "temp/KPIs/KPI1.1 - KPI1.3/")

gp_prac_a_fpath <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/data/",
                          "GP Practice History with dob selection - prior to 1_4_1952.csv")

gp_prac_b_fpath <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/data/",
                          "GP Practice History with dob selection - post 1_4_1952.csv")

gp_lookup_fpath <- paste0("/conf/linkage/output/lookups/Unicode/",
                          "National Reference Files/gpprac.sav")

prev_gp_data_fpath <- paste0("/PHI_conf/AAA/Topics/Screening/publications/",
                             "Completed/20220301/Temp/Management Information/",
                             "Practice/",
                             "Temp All Boards output_coverage 2021.zsav")


year1_start <- dmy("01-04-1955")
year1_end <- dmy("31-03-1956")

year2_start <- dmy("01-04-1956")
year2_end <- dmy("31-03-1957")


### Step 2 : Import data ----

#invite_uptake <- read_rds(invite_uptake_fpath)
coverage_basefile <- read_rds(paste0(output_fpath,
                                     "coverage_basefile.rds"))

gp_prac_a <- read_csv(gp_prac_a_fpath)
gp_prac_b <- read_csv(gp_prac_b_fpath)

gp_lookup <- read_sav(gp_lookup_fpath) %>%
  select(praccode, add1)

#prev_gp_data <- read_sav(prev_gp_data_fpath)

prev_gp_data <- read_rds(prev_gp_data_fpath)

### Step 3 : ----

gp_prac <- bind_rows(gp_prac_a, gp_prac_b)

# Flag gp practice that was relevant at the end of the financial year
gp_prac <- mutate(gp_prac,
                  valid = case_when(
                    `Valid from` < dmy("01-04-2022") & `Valid to` > dmy("01-04-2022") ~ 1,
                    `Valid from` < dmy("01-04-2022") & is.na(`Valid to`) ~ 1,
                    is.na(`Valid from`) & `Valid to` > dmy("01-04-2022") ~ 1,
                    TRUE ~ 0)
)

gp_prac <- filter(gp_prac, valid == 1)


coverage_gp <- coverage_basefile %>%
  left_join(gp_prac, by = c("upi" = "Upinumber")) %>%
  select(-c(`Area of Residence`, `Valid from`, `Valid to`, `valid`)) %>%
  rename(practice_code = `Practice Code`)

### Assign gp practices to health boards
# The spss script has a big gp practice if/else statement
# !! come back to check if this is the right thing
# creates 'cypher' doesn't seem to be used
# creates 'prac2' which is practice code without the leading HB letter
# prac2 is used for joining on the reference file
# creates 'prac3' which is prac2 unless patient is resident outside the
# health board they're registered in
# prac3 is used for aggregation

# make gp_join
coverage_gp <- coverage_gp %>%
  mutate(gp_join = substr(practice_code, 2, 5))

# flag for if an individual is registered inside/outside their hb of residence
coverage_gp <- coverage_gp %>%
  mutate(in_hb = case_when(
    gp_join == 3139 ~ 1,
    hbres == "Ayrshire & Arran" & between(gp_join, 8000, 8399) ~ 1,
    hbres == "Borders" & between(gp_join, 1600, 1799) ~ 1,
    hbres == "Fife" & between(gp_join, 2000, 2499) ~ 1,
    hbres == "Lanarkshire" & 
      (between(gp_join, 6000, 6599) |
      gp_join %in% c(4626,4627,4653,4654,
                     4900,4902,4905,4906,
                     4911,4925,4943,4952,
                     4964,4969,4970,4979)) ~ 1,
    hbres == "Greater Glasgow & Clyde" &
      (between(gp_join, 4000, 5499) |
         between(gp_join, 8499, 8799)) &
         !gp_join %in% c(4626,4627,4653,4654,
                      4900,4902,4905,4906,
                      4911,4925,4943,4952,
                      4964,4969,4970,4979,
                      8500,8511,8514,8515,
                      8519) ~ 1,
    hbres == "Highland" &
      (between(gp_join, 5500, 5999) |
         between(gp_join, 8400, 8498) |
         gp_join %in% c(8500,8511,8514,8515,8519)) ~ 1,
    hbres == "Grampian" & between(gp_join, 3000, 3799) ~ 1,
    hbres == "Lothian" & between(gp_join, 7000, 7999) ~ 1,
    hbres == "Orkney" & between(gp_join, 3800, 3899) ~ 1,
    hbres == "Tayside" & between(gp_join, 1000, 1599) ~ 1,
    hbres == "Forth Valley" & between(gp_join, 2500, 2999) ~ 1,
    hbres == "Western Isles" & between(gp_join, 9000, 9099) ~ 1,
    hbres == "Dumfries & Galloway" & between(gp_join, 1800, 1999) ~ 1,
    hbres == "Shetland" & between(gp_join, 3900, 3999) ~ 1,
    is.na(gp_join) ~ 1,
    TRUE ~ 0
  ))

# create a variable for practice code if registered in health board
coverage_gp <- coverage_gp %>%
  mutate(gp_hb = case_when(
    in_hb == 0 ~ "Practice outside hb area",
    TRUE ~ gp_join
  ))

# Add in practice description using reference lookup file
gp_lookup <- gp_lookup %>%
  rename(gp_join = praccode,
         gp_desc = add1) %>%
  mutate(gp_join = substr(gp_join, 1, 4))

# There are two '9999' codes (99995=practice in england, wales, ni)
# and (99999=unknown). Take 9999 as unknown.
gp_lookup <- gp_lookup %>%
  filter(!(gp_join == 9999 & gp_desc == "PATIENTS REGISTERED WITH A GP"))

coverage_gp <- coverage_gp %>%
  left_join(gp_lookup, by = "gp_join")

coverage_gp <- coverage_gp %>%
  mutate(gp_desc = if_else(gp_hb == "Practice outside hb area",
                           "Practice outside hb area", gp_desc))



# calculate KPI 1.2a (coverage)

breakdown_1_2a <- coverage_gp %>%
  group_by(gp_hb, gp_desc, hbres) %>%
  summarise(
    across(cohort_year1:tested2_any_not_assigned, sum, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(gp_hb = if_else(is.na(gp_hb), "Unknown Practice", gp_hb))


hb_1_2a <- coverage_gp %>%
  group_by(hbres) %>%
  summarise(
    across(cohort_year1:tested2_any_not_assigned, sum, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(gp_hb = hbres)

breakdown_1_2a <- bind_rows(breakdown_1_2a, hb_1_2a)

# create percentages
breakdown_1_2a <- breakdown_1_2a %>%
  mutate(
    percent_year2 = (tested2_year2/cohort_year2)*100,
    
    percent_any_year2 = (tested2_any_year2/cohort_year2)*100
  )

# Output tables
output_1_2a <- breakdown_1_2a %>%
  select(hbres, gp_hb, gp_desc,
         cohort_year2, tested2_year2, percent_year2) %>%
  arrange(hbres, gp_hb)


# bring in previous year's data
# update practice names using most recent lookup to avoid duplication

prev_gp_data <- prev_gp_data %>%
  left_join(gp_lookup, by = c("prac3" = "gp_join"))

# now join to most recent year
output_2year <- output_1_2a %>%
  full_join(prev_gp_data, by = c("hbres" = "hbresname",
                                 "gp_hb" = "prac3",
                                 "gp_desc" = "gp_desc")) %>%
  select(hbres, gp_hb, gp_desc, FY2020_21_cohort, FY2020_21_tested, p_FY2020_21,
         cohort_year2, tested2_year2, percent_year2)


# save out

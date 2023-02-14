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


year1_start <- dmy("01-04-1955")
year1_end <- dmy("31-03-1956")

year2_start <- dmy("01-04-1956")
year2_end <- dmy("31-03-1957")


### Step 2 : Import data ----

invite_uptake <- read_rds(invite_uptake_fpath)
coverage_basefile <- read_rds(paste0(output_fpath,
                                     "coverage_basefile.rds"))

gp_prac_a <- read_csv(gp_prac_a_fpath)
gp_prac_b <- read_csv(gp_prac_b_fpath)


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


coverage_basefile <- coverage_basefile %>%
  left_join(gp_prac, by = c("upi" = "Upinumber")) %>%
  select(-c(`Area of Residence`, `Valid from`, `Valid to`, `valid`))

# Assign gp practices to health boards

# Add in practice description using reference lookup file

# calculate coverage KPIs

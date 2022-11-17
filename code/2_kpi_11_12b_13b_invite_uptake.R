#~~~~~~~~~~~~~~~~~~~~~~~~~
# 2_kpi_11_12b_13b_invite_uptake.R
# Angus Morton
# 17/11/2022
#
# Produce KPIs 1.1, 1.2b and 1.3b
#
# Written on RServer (R Version 3.6.1)
#~~~~~~~~~~~~~~~~~~~~~~~~~

# KPI 1.1  : Percentage of eligible population who are sent an initial
#            offer to screening before age 66

# KPI 1.2b : Percentage of men offered screening before age 66 who are
#            tested before age 66 and 3 months 

# KPI 1.3b : Percentage of men offered screening before age 66 who are
#            tested before age 66 and 3 months by Scottish Index of
#            Multiple Deprivation (SIMD) quintile



# Step 1 : Import packages and filepaths
# Step 2 : Import and trim data
# Step 3 : 
# Step x : Write out


### Step 1 : Import packages and filepaths ----

# This should be the only step which needs edited each time

library(readr)
library(dplyr)
library(tidylog)
library(lubridate)

invite_uptake_fpath <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/202209/",
                              "temp/KPIs/KPI1.1 - KPI1.3/",
                              "inviteanduptake_initial.rds")


### Step 2 : Import and trim data ----

invite_uptake <- read_rds(invite_uptake_fpath)

# trim date
invite_uptake <- invite_uptake %>%
  filter(dob >= dmy("01-04-1948"))

### Step 3 : Create derived variables ----

## for KPI 1.1
# age_at_screen (they want age in months because 66 and 3 months is the
# key age. Don't really like this as ambiguous what a month is)
invite_uptake <- invite_uptake %>%
  mutate(age_at_screen =)

# age_at_offer (they want this in years because 66 is the key age)


# assign year eligible cohorts
# assign year offer cohorts (offered before 66)
# secondary numerator

## for KPI 1.2b
# assign year offer cohorts (already have this?)
# offered before age 66 and

### KPI 1.1 ----

# create individual board breakdown
# join on scotland data
# create percentages
# create output tables?

### KPI 1.2b ----

### KPI 1.3b ----





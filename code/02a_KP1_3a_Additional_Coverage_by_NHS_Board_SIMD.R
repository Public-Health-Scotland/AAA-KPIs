#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 02a_KP1_3a_Additional_Coverage_by_NHS_Board_SIMD.R
# Eibhlin O'Sullivan
# Jan 2022
# Takes processed extract and creates summary of patients
# Written/run on R Studio Server
# R version 3.6.1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### 1 - Housekeeping ----

## Source housekeeping file
source(here::here("code", "00_housekeeping.R"))

### 2 - Read in Files ----

# Import coverage basefile
coverage_basefile <- readRDS(paste0(coverage_basefile_path,"coverage_basefile.rds")) %>% 
  arrange(postcode)

# Import SIMD file
# simd_lookup <- readRDS(paste0(simd_lookup_path,"postcode_2022_2_simd2020v2.rds"))

### 3 - Join Files ----

# Join simd lookup to basefile
# Select necessary columns

coverage_basefile_simd <- coverage_basefile %>% 
  left_join(simd_lookup, by = c("postcode" = "pc7")) %>% 
  select(upi:tested2_any_not_assigned,)


 

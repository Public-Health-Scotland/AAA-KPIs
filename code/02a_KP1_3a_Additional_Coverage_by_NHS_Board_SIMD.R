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
coverage_basefile <- readRDS(glue("{coverage_basefile_path}/coverage_basefile.rds")) %>% 
  arrange(postcode) %>% 
  mutate(pc7 = format_postcode(postcode,format = "pc7")) %>% 
  select(-postcode)

# Import SIMD file
# NEEDS updating to current file!
pc_simd <- readRDS(glue("{gpd_lookups}/Deprivation/",
                        "postcode_2022_2_simd2020v2.rds"))

### 3 - Join Files ----
coverage_by_NHS_Board_SIMD <- coverage_basefile %>% 
  left_join(pc_simd, by = "pc7") %>% 
  select(-c(pc8:simd2020v2_hb2019_decile,simd2020v2_hscp2019_decile:simd2020v2_crime_rank))

rm(coverage_basefile)
### KPI 1.3 ----

## Coverage by simd
breakdown_1_3 <- coverage_by_NHS_Board_SIMD %>%
  group_by(hbres, simd2020v2_hb2019_quintile) %>%
  summarise(
    across(cohort_year1:tested2_any_not_assigned, sum, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(simd2020v2_hb2019_quintile = as.character(simd2020v2_hb2019_quintile)) %>% 
  mutate(simd2020v2_hb2019_quintile = case_when(is.na(simd2020v2_hb2019_quintile) ~ "Unknown",
                                                !is.na(simd2020v2_hb2019_quintile) ~ simd2020v2_hb2019_quintile))

breakdown_1_3_tot <- coverage_by_NHS_Board_SIMD %>%
  group_by(hbres) %>%
  summarise(
    across(cohort_year1:tested2_any_not_assigned, sum, na.rm = TRUE)
  ) %>%
  ungroup() %>% 
  mutate(simd2020v2_hb2019_quintile = "Total")


# bind together including non simd totals from previous kpi
breakdown_1_3 <- bind_rows(breakdown_1_3_tot, breakdown_1_3) %>% 
  arrange(hbres)


### KPI 1.3a ----

# create percentages
breakdown_1_3a <- breakdown_1_3 %>%
  mutate(
    percent_year1 = (tested2_year1/cohort_year1)*100,
    percent_year2 = (tested2_year2/cohort_year2)*100,
    
    percent_any_year1 = (tested2_any_year1/cohort_year1)*100,
    percent_any_year2 = (tested2_any_year2/cohort_year2)*100
  )

### KPI 1.3b ----

# create percentages
breakdown_1_3b <- breakdown_1_3 %>%
  mutate(
    percent_year1 = (tested_year1/offer_year1)*100,
    percent_year2 = (tested_year2/offer_year2)*100,
    
    p_not_assigned = (tested_not_assigned/offer_not_assigned)*100
  )

# Output tables
output_1_3a <- breakdown_1_3a %>%
  select(hbres, simd2020v2_hb2019_quintile, cohort_year1, tested2_year1, percent_year1,
         cohort_year2, tested2_year2, percent_year2)
  
# Output tables
output_1_3b <- breakdown_1_3b %>%
  select(hbres, simd2020v2_hb2019_quintile, offer_year1, tested_year1,
         percent_year1, offer_year2, tested_year2, percent_year2) %>%
  mutate_all(~ifelse(is.nan(.), NA, .))


 

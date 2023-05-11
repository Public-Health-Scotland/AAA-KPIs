##########################################################
# theme_3_KPI_2_1_2_2.R
# Gavin Clark
# 19/10/2022
# Data extraction/preparation
# Written/run on R Studio Server
# R version 3.6.1
# Translation of the syntaxes associated with theme 4:
#### KPIs - 4. KPI2.1-2.2 (Non Vis and QA)
#### Management Information - 1. KPI 2.2 Additional - audit breakdown
####                        - 2a. Table 4 - Non Vis eligible cohort
####                        - 2b. Table 4 - Non Vis Self Ref
####                        - Audit fails (LSWG-reports)
##########################################################

#### Step 1: Housekeeping ----

#   loading packages
library(dplyr)
library(haven)
library(janitor)
library(tidyr)
library(stringr)
library(lubridate)
library(here)
library(ggplot2)
library(forcats)
library(readr)
library(readxl)
library(odbc)
library(tidylog)
library(phsmethods)

# Extract file location
extract_loc <- paste0('/PHI_conf/AAA/Topics/Screening/extracts/202303/output/',
                      'aaa_extract_202303.rds')

# Cover base file location
coverage_basefile_loc <- paste0("/PHI_conf/AAA/Topics/Screening/KPI/202303/temp/",
                                "1_coverage_basefile.rds")

# Dates of first and last financial year
start_date <- "2022-04-01" #"2019-04-01"

end_date <- "2023-03-31" #"2022-03-31"

#### Step 2: Read in and process data ----

extract <- readRDS(extract_loc)

coverage_basefile <- readRDS(coverage_basefile_loc)

### Step 3: Create summary tables ----

# Create relevant subset of data
extract2 <- extract %>%
  # Filter for relevant dates and keep positive, negative and non-vis screens
  filter(between(date_screen, as.Date(start_date), as.Date(end_date)),
         screen_result %in% c('01','02','04')) %>%
  mutate(
    fin_year = extract_fin_year(date_screen),
    non_vis_n = if_else(screen_result == '04', 1, 0),
    screened_n = if_else(screen_result %in% c('01','02','04'), 1, 0),
    fin_year_66 = extract_fin_year(dob + years(66))
  ) 

#### Step 3a: KPI 2.1a ----
#KPI 2.1a: Percentage of screening appointments, where the aorta could not be
# visualised
# Denominator = total number of attended scans (excluding technical failure) 
# Numerator = Number of scans with a screening result of non-visualisation
kpi_2_1_a_scotland <- extract2 %>%
  mutate(hb_screen = "Scotland") %>%
  group_by(fin_year, hb_screen) %>%
  summarise(
    non_vis_n = sum(non_vis_n),
    screened_n = sum(screened_n)
  ) %>%
  ungroup()

kpi_2_1_a_hb <- extract2 %>%
  group_by(fin_year, hb_screen) %>%
  summarise(
    non_vis_n = sum(non_vis_n),
    screened_n = sum(screened_n)
  ) %>%
  ungroup()

kpi_2_1_a <- bind_rows(kpi_2_1_a_scotland, kpi_2_1_a_hb) %>%
  mutate(hb_screen = fct_relevel(as.factor(hb_screen), "Scotland"),
         non_vis_p = round_half_up(non_vis_n/screened_n*100, 1)) %>%
  arrange(fin_year, hb_screen) %>%
  pivot_wider(names_from = fin_year,
              values_from = c(screened_n, non_vis_n, non_vis_p),
              names_vary = 'slowest'
  )


#### Step 3b: KPI 2.1b ----
# KPI 2.1b: Percentage of men screened where aorta could not be visualised
# Denominator = The number of MEN attended screening, excluding technical failure
# Numerator = Number of MEN with at least 1 screen where the aorta could not be 
# visualised

extract2_dedup_hb <- extract2 %>%
  # GC - keeps non-visualised if there is one
  arrange(upi, fin_year, hb_screen, desc(non_vis_n)) %>%
  distinct(upi, fin_year, hb_screen, .keep_all = TRUE)

extract2_dedup_scotland <- extract2 %>%
  # GC - keeps non-visualised if there is one
  arrange(upi, fin_year, desc(non_vis_n)) %>%
  distinct(upi, fin_year, .keep_all = TRUE)
# GC TO DO
# decide whether it is acceptable to have differing totals for scotland
# vs. if adding the HBs individually

kpi_2_1_b_scotland <- extract2_dedup_scotland %>%
  mutate(hb_screen = "Scotland") %>%
  group_by(fin_year, hb_screen) %>%
  summarise(
    non_vis_n = sum(non_vis_n),
    screened_n = sum(screened_n)
  ) %>%
  ungroup()


kpi_2_1_b_hb <- extract2_dedup_hb %>%
  group_by(fin_year, hb_screen) %>%
  summarise(
    non_vis_n = sum(non_vis_n),
    screened_n = sum(screened_n)
  ) %>%
  ungroup()

kpi_2_1_b <- bind_rows(kpi_2_1_b_scotland, kpi_2_1_b_hb) %>%
  mutate(
    hb_screen = fct_relevel(as.factor(hb_screen), "Scotland"),
    non_vis_p = round_half_up(non_vis_n/screened_n*100, 1)) %>%
  pivot_wider(names_from = fin_year,
              values_from = c(screened_n, non_vis_n, non_vis_p),
              names_vary = 'slowest')


### Step 3c: KPI 2.2 ----
# KPI 2.2: Percentage of images that failed the AUDIT  AND required an immediate recall 

extract_audit <- extract %>%
  filter(between(date_screen, as.Date(start_date), as.Date(end_date)),
         audit_flag == '01') %>%
  mutate(
    fin_year = extract_fin_year(date_screen),
    audit_n = if_else(audit_flag == '01', 1, 0),
    fail_im_recall_n = if_else(audit_result == '02' & 
                                 audit_outcome == '01', 1, 0),
    no_audit_result_n = case_when(
      is.na(audit_result) ~ 1,
      !audit_result %in% c("01", "02") ~ 1,
      TRUE ~ 0),
    standard_met_n = if_else(
      audit_result == '01', 1, 0)
  )

kpi_2_2_hb <- extract_audit %>%
  group_by(fin_year, hb_screen) %>%
  summarise(
    fail_im_recall_n = sum(fail_im_recall_n),
    audit_n = sum(audit_n),
    fail_im_recall_p = round_half_up(fail_im_recall_n/audit_n*100, 1)
  ) %>%
  ungroup()

kpi_2_2_scotland <- extract_audit %>%
  group_by(fin_year) %>%
  summarise(
    fail_im_recall_n = sum(fail_im_recall_n),
    audit_n = sum(audit_n),
    fail_im_recall_p = round_half_up(fail_im_recall_n/audit_n*100, 1)
  ) %>%
  ungroup() %>%
  mutate(hb_screen = "Scotland")

kpi_2_2 <- bind_rows(kpi_2_2_scotland, kpi_2_2_hb) %>%
  mutate(hb_screen = fct_relevel(as.factor(hb_screen), "Scotland")) %>%
  arrange(fin_year, hb_screen) %>%
  pivot_wider(names_from = fin_year,
              values_from = c(audit_n, fail_im_recall_n, fail_im_recall_p),
              names_vary = 'slowest')


### Step 3d: KPI 2.2 additional ----

kpi_2_2_add_hb <- extract_audit %>%
  group_by(fin_year, hb_screen) %>%
  summarise(
    no_audit_result_n = sum(no_audit_result_n),
    audit_n = sum(audit_n),
    no_audit_result_p = round_half_up(no_audit_result_n/audit_n*100, 1),
    audit_n2 = sum(audit_n),
    standard_met_n = sum(standard_met_n),
    standard_met_p = round_half_up(standard_met_n/audit_n*100, 1),
    standard_not_met_n = audit_n - standard_met_n,
    standard_not_met_p = round_half_up(standard_not_met_n/audit_n*100, 1)) %>%
  ungroup()

kpi_2_2_add_scotland <- extract_audit %>%
  group_by(fin_year) %>%
  summarise(
    no_audit_result_n = sum(no_audit_result_n),
    audit_n = sum(audit_n),
    no_audit_result_p = round_half_up(no_audit_result_n/audit_n*100, 1),
    audit_n2 = sum(audit_n),
    standard_met_n = sum(standard_met_n),
    standard_met_p = round_half_up(standard_met_n/audit_n*100, 1),
    standard_not_met_n = audit_n - standard_met_n,
    standard_not_met_p = round_half_up(standard_not_met_n/audit_n*100, 1)
  ) %>%
  ungroup() %>%
  mutate(hb_screen = "Scotland")

kpi_2_2_add <- bind_rows(kpi_2_2_add_scotland, kpi_2_2_add_hb) %>%
  mutate(hb_screen = fct_relevel(as.factor(hb_screen), "Scotland")) %>%
  arrange(fin_year, hb_screen) %>%
  pivot_wider(names_from = fin_year,
              values_from = c(audit_n, no_audit_result_n, audit_n, no_audit_result_p,
                              audit_n2, standard_met_n, standard_met_p,
                              standard_not_met_n, standard_not_met_p),
              names_vary = 'slowest')

### Step 3?: KPI 2.2 additional (b)

extract_audit_b <- extract %>%
  filter(between(date_screen, as.Date(start_date), as.Date(end_date)),
         audit_flag == '01') %>%
  mutate(
    fin_year = extract_fin_year(date_screen),
    audit_n = if_else(audit_flag == '01', 1, 0),
    fail_im_recall_n = if_else(audit_result == '02' & 
                                 audit_outcome == '01', 1, 0),
    no_audit_result_n = case_when(
      is.na(audit_result) ~ 1,
      !audit_result %in% c("01", "02") ~ 1,
      TRUE ~ 0),
    standard_met_n = if_else(
      audit_result == '01', 1, 0),
    imm_recall_n =  case_when(audit_outcome == '01' ~ 1, TRUE ~ 0),
    recall_cc_n = case_when(audit_outcome == '02' ~ 1, TRUE ~ 0),
    no_recall_sat_interim_n = case_when(audit_outcome == '03' ~ 1, TRUE ~ 0),
    no_recall_refer_vasc_n = case_when(audit_outcome == '04' ~ 1, TRUE ~ 0),
    no_recall_sec_opin_n = case_when(audit_outcome == '05' ~ 1,
                                     is.na(audit_outcome) ~ 0, TRUE ~ 0)
  )

kpi_2_2_add_b_hb <- extract_audit_b %>%
  group_by(fin_year, hb_screen) %>%
  summarise(
    audit_n = sum(audit_n),
    standard_met_n = sum(standard_met_n),
    standard_not_met_n = (audit_n - standard_met_n),
    standard_not_met_n = sum(standard_not_met_n),
    imm_recall_n = sum(imm_recall_n),
    imm_recall_p = round_half_up(imm_recall_n/standard_not_met_n*100, 1),
    recall_cc_n = sum(recall_cc_n),
    recall_cc_p = round_half_up(recall_cc_n/standard_not_met_n*100, 1),
    no_recall_sat_interim_n = sum(no_recall_sat_interim_n),
    no_recall_sat_interim_p = round_half_up(no_recall_sat_interim_n/standard_not_met_n*100, 1),
    no_recall_refer_vasc_n = sum(no_recall_refer_vasc_n),
    no_recall_refer_vasc_p = round_half_up(no_recall_refer_vasc_n/standard_not_met_n*100, 1),
    no_recall_sec_opin_n = sum(no_recall_sec_opin_n),
    no_recall_sec_opin_p = round_half_up(no_recall_sec_opin_n/standard_not_met_n*100, 1)
    ) %>%
  ungroup()

kpi_2_2_add_b_scotland <- extract_audit_b %>%
  group_by(fin_year) %>%
  summarise(
    no_audit_result_n = sum(no_audit_result_n),
    audit_n = sum(audit_n),
    standard_met_n = sum(standard_met_n),
    standard_not_met_n = audit_n - standard_met_n,
    imm_recall_n = sum(imm_recall_n),
    imm_recall_p = round_half_up(imm_recall_n/standard_not_met_n*100, 1),
    recall_cc_n = sum(recall_cc_n),
    recall_cc_p = round_half_up(recall_cc_n/standard_not_met_n*100, 1),
    no_recall_sat_interim_n = sum(no_recall_sat_interim_n),
    no_recall_sat_interim_p = round_half_up(no_recall_sat_interim_n/standard_not_met_n*100, 1),
    no_recall_refer_vasc_n = sum(no_recall_refer_vasc_n),
    no_recall_refer_vasc_p = round_half_up(no_recall_refer_vasc_n/standard_not_met_n*100, 1),
    no_recall_sec_opin_n = sum(no_recall_sec_opin_n),
    no_recall_sec_opin_p = round_half_up(no_recall_sec_opin_n/standard_not_met_n*100, 1)
  ) %>%
  ungroup() %>%
  mutate(hb_screen = "Scotland")

kpi_2_2_add_b <- bind_rows(kpi_2_2_add_b_scotland, kpi_2_2_add_b_hb) %>%
  mutate(hb_screen = fct_relevel(as.factor(hb_screen), "Scotland")) %>%
  arrange(fin_year, hb_screen) %>%
  pivot_wider(names_from = fin_year,
              values_from = c(audit_n, standard_not_met_n, imm_recall_n, imm_recall_p,
                              recall_cc_n, recall_cc_p, recall_cc_n,
                              recall_cc_p, no_recall_sat_interim_n, no_recall_sat_interim_p,
                              no_recall_refer_vasc_n, no_recall_refer_vasc_p, no_recall_sec_opin_n,
                              no_recall_sec_opin_p),
              names_vary = 'slowest')
    
### Step 3e: Supplementary table 4 ----

coverage_basefile <- coverage_basefile %>%
  #filter(!is.na(screen_result)) %>%
  mutate(    fin_year_66 = case_when(
    age_calculate(dob, as.Date("2020-03-31")) == 66 ~ "2019/20",
    age_calculate(dob, as.Date("2021-03-31")) == 66 ~ "2020/21",
    age_calculate(dob, as.Date("2022-03-31")) == 66 ~ "2021/22",
    age_calculate(dob, as.Date("2023-03-31")) == 66 ~ "2022/23",
    TRUE ~ "66 in a different year"))

extract_sup_4 <- extract %>%
  # Filter for relevant dates and keep positive, negative and non-vis screens
  filter(!is.na(screen_result)) %>%
  mutate(
    non_vis_n = if_else(screen_result == '04', 1, 0)
  ) %>%
  group_by(upi) %>%
  mutate(
    non_vis_any = max(non_vis_n)
  ) %>%
  ungroup() %>%
  filter(non_vis_any == 1) %>%
  # total nonvis matches SPSS at this point
  mutate(
    non_vis_n = 1,
    result_temp = case_when(
      audit_outcome == "01" ~ 0,
      screen_result %in% c("01", "02", "05", "06") ~ 1,
      TRUE ~ 0
    )
  ) %>%
  group_by(upi) %>%
  mutate(flag_result = max(result_temp)) %>%
  ungroup() %>%
  filter(flag_result == 0, 
         pat_elig != '03', 
         screen_type %in% c('01', '03'),
         screen_result != '03'
  )

extract_sup_4 %>% count(flag_result)

# create lookup
non_vis_lookup <- extract_sup_4 %>%
  group_by(upi) %>%
  summarise(non_vis = n()) %>%
  ungroup()
# 2,516, same as SPSS line 171

non_vis_lookup_2 <- non_vis_lookup %>%
  filter(non_vis >= 2)
# 1,675 matches SPSS line 176

non_vis_match <- coverage_basefile %>%
  filter(!is.na(screen_result)) %>%
  left_join(non_vis_lookup, by = "upi") %>%
  mutate(
    #non_vis_n = if_else(screen_result == '04', 1, 0),
    non_vis = replace_na(non_vis, 0),
    fin_year_66 = case_when(
      age_calculate(dob, as.Date("2020-03-31")) == 66 ~ "2019/20",
      age_calculate(dob, as.Date("2021-03-31")) == 66 ~ "2020/21",
      age_calculate(dob, as.Date("2022-03-31")) == 66 ~ "2021/22",
      age_calculate(dob, as.Date("2023-03-31")) == 66 ~ "2022/23",
      TRUE ~ "66 in a different year"
    )) %>%
  filter(fin_year_66 != "66 in a different year")

non_vis_sum_scot <- non_vis_match %>%
  group_by(fin_year_66) %>%
  summarise(
    n = n(),
    non_vis_n = sum(non_vis >= 1),
    non_vis_2_more_n = sum(non_vis >= 2),
    non_vis_1_n = sum(non_vis == 1)
  ) %>%
  ungroup() %>%
  mutate(
    non_vis_p = round_half_up(non_vis_n/n*100, 1),
    non_vis_2_more_p = round_half_up(non_vis_2_more_n/n*100, 1),
    non_vis_1_p = round_half_up(non_vis_1_n/n*100, 1),
    hbres = "Scotland"
  )

non_vis_sum_hb <- non_vis_match %>%
  # hb_screen not on coverage basefile, try hbres
  group_by(fin_year_66, hbres) %>%
  # hbres giving same as SPSS output
  summarise(
    n = n(),
    non_vis_n = sum(non_vis >= 1),
    non_vis_2_more_n = sum(non_vis >= 2),
    non_vis_1_n = sum(non_vis == 1)
  ) %>%
  ungroup() %>%
  mutate(
    non_vis_p = round_half_up(non_vis_n/n*100, 1),
    non_vis_2_more_p = round_half_up(non_vis_2_more_n/n*100, 1),
    non_vis_1_p = round_half_up(non_vis_1_n/n*100, 1)
  )

sup_tab_4_eligible <- bind_rows(non_vis_sum_scot, non_vis_sum_hb) %>%
  mutate(hbres = fct_relevel(as.factor(hbres), "Scotland")) %>%
  arrange(fin_year_66, hbres) %>%
  pivot_wider(names_from = fin_year_66,
              values_from = c(
                n, non_vis_n, non_vis_p,
                non_vis_2_more_n, non_vis_2_more_p,
                non_vis_1_n, non_vis_1_p),
              names_vary = 'slowest')



# 
# non_vis_2_match <- coverage_basefile %>%
#   left_join(non_vis_lookup_2, by = "upi") %>%
#   mutate(
#     #non_vis_n = if_else(screen_result == '04', 1, 0),
#     non_vis = replace_na(non_vis, 0),
#     fin_year_66 = case_when(
#       age_calculate(dob, as.Date("2020-03-31")) == 66 ~ "2019/20",
#       age_calculate(dob, as.Date("2021-03-31")) == 66 ~ "2020/21",
#       age_calculate(dob, as.Date("2022-03-31")) == 66 ~ "2021/22",
#       TRUE ~ "66 in a different year"
#     )) %>%
#   filter(fin_year_66 != "66 in a different year")
# 
# non_vis_2_sum <- non_vis_2_match %>%
#   group_by(fin_year_66) %>%
#   summarise(
#     n = n(),
#     non_vis = sum(non_vis >= 2)
#   ) %>%
#   ungroup()

# sup_tab_4 <- extract_sup_4 %>%
#     mutate(
#       fin_year_66 = case_when(
#         age_calculate(dob, as.Date("2020-03-31")) == 66 ~ "2019/20",
#         age_calculate(dob, as.Date("2021-03-31")) == 66 ~ "2020/21",
#         age_calculate(dob, as.Date("2022-03-31")) == 66 ~ "2021/22",
#         TRUE ~ "66 in a different year")) %>%
#   filter(fin_year_66 %in% c("2019/20", "2020/21", "2021/22")
#          ) %>%
#   group_by(fin_year_66) %>%
#   summarise(
#     non_vis_n = sum(non_vis_n)
#   ) %>%
#   ungroup()

### Step 3f: Self-referrals ----

extract_sup_4_sr <- extract %>%
  # Filter for relevant dates and keep positive, negative and non-vis screens
  filter(!is.na(screen_result)) %>%
  mutate(
    non_vis_n = if_else(screen_result == '04', 1, 0)
  ) %>%
  group_by(upi) %>%
  mutate(
    non_vis_count = sum(non_vis_n),
    non_vis_any = max(non_vis_n)
  ) %>%
  ungroup() %>%
  filter(non_vis_any == 1) %>%
  # total nonvis matches SPSS at this point
  mutate(
    non_vis_n == 1,
    result_temp = case_when(
      audit_outcome == "01" ~ 0,
      screen_result %in% c("01", "02", "05", "06") ~ 1,
      TRUE ~ 0
    )
  ) %>%
  group_by(upi) %>%
  mutate(flag_result = max(result_temp)) %>%
  ungroup() %>%
  filter(flag_result == 0, 
         pat_elig == '03', 
         screen_type %in% c('01', '03'),
         screen_result != '03'
  ) %>%
  distinct(upi, .keep_all = TRUE) %>%
  group_by() %>%
  summarise(
    non_vis_n = n(),
    non_vis_1_n = sum(non_vis_count == 1),
    non_vis_2_more_n = sum(non_vis_count >= 2)
  ) %>%
  ungroup()


# Get total no. of self-referrals
extract_sr <- extract %>%
  filter(pat_elig == '03', 
         date_screen <= as.Date(end_date),
         screen_type %in% c('01', '03'),
         !is.na(screen_result),
         screen_result != '03'
  ) %>% 
  distinct(upi) %>% 
  count() %>%
  rename(self_referrals = n)

sup_tab_4_sr <- bind_cols(extract_sr, extract_sup_4_sr)



### Step 3g: Batch QA standard not met ----

qa_standard <- extract %>%
  filter(!(screen_result %in% c('05','06')),
         audit_result == '02',
         financial_year  %in% c("2019/20", "2020/21", "2021/22", "2022/23")) %>%
  # GC - add to issues
  mutate(
    
    audit_n = if_else(audit_flag == '01', 1, 0),
    standard_met_n = if_else(
      audit_result == '01', 1, 0),
    audit_fail_reason_text = case_when(
      audit_fail_reason == '01' ~ "Calliper",
      audit_fail_reason == "02" ~ "Angle",
      audit_fail_reason == '03' ~ "Image quality",
      audit_fail_reason == '04' ~ "Anatomy",
      TRUE ~ "No audit fail"
    ),
    audit_fail_reason_text = fct_relevel(audit_fail_reason_text,
                                         "Calliper",
                                         "Angle",
                                         "Image quality",
                                         "Anatomy",
                                         "No audit fail"
    )
  )

qa_standard_hb_sum <- qa_standard %>%
  group_by(financial_year, hb_screen, audit_fail_reason_text) %>%
  summarise(
    standard_not_met_n = sum(audit_n) - sum(standard_met_n)
  ) %>%
  ungroup()

qa_standard_scot_sum <- qa_standard_hb_sum %>%
  group_by(financial_year, audit_fail_reason_text) %>%
  summarise(standard_not_met_n = sum(standard_not_met_n)) %>%
  ungroup() %>%
  mutate(hb_screen = "Scotland")

# Combine 
qa_standard_sum <- bind_rows(qa_standard_scot_sum, qa_standard_hb_sum)

# Calculate total where standard not met
qa_standard_totals <- qa_standard_sum %>%
  group_by(financial_year, hb_screen) %>%
  summarise(standard_not_met_n = sum(standard_not_met_n)) %>%
  ungroup() %>%
  mutate(audit_fail_reason_text = "Standard not met")

qa_standard_not_met <- bind_rows(qa_standard_totals, qa_standard_sum) %>%
  # GC - move to KH code?
  mutate(hb_screen = fct_relevel(hb_screen, "Scotland"),
         audit_fail_reason_text = fct_relevel(audit_fail_reason_text, 
                                              "Standard not met",
                                              "Calliper",
                                              "Angle",
                                              "Image quality",
                                              "Anatomy",
                                              "No audit fail")
  ) %>%
  arrange(financial_year, hb_screen, audit_fail_reason_text) %>%
  pivot_wider(
    values_from = standard_not_met_n,
    names_from = c(audit_fail_reason_text),
    names_glue = "{audit_fail_reason_text}_n"
  ) %>%
  clean_names() %>%
  mutate(calliper_p = round_half_up(calliper_n/standard_not_met_n*100, 1),
         angle_p = round_half_up(angle_n/standard_not_met_n*100, 1),
         image_quality_p = round_half_up(image_quality_n/standard_not_met_n*100, 1),
         anatomy_p = round_half_up(anatomy_n/standard_not_met_n*100, 1)
  ) %>%
  select(
    hb_screen, financial_year, 
    standard_not_met_n, calliper_n, calliper_p,
    angle_n, angle_p, image_quality_n, image_quality_p,
    anatomy_n, anatomy_p
  ) %>%
  pivot_wider(names_from = financial_year,
              names_vary = "slowest",
              values_from = standard_not_met_n:anatomy_p)

### Step 3h: Standard not met detail ----

detail <- qa_standard %>%
  select(
    financial_year, audit_fail_1:audit_fail_5
  ) %>%
  pivot_longer(
    cols = audit_fail_1:audit_fail_5,
    names_to = "column"
  ) %>%
  filter(!is.na(value)) %>%
  mutate(
    detail_text = case_when(
      value == "01" ~ "Calliper - APL",
      value == "02" ~ "Calliper - APT",
      value == "03" ~ "Calliper - Anterior Calliper",
      value == "04" ~ "Calliper - Posterior Calliper",
      value == "05" ~ "Angle - APL",
      value == "06" ~ "Angle - APT",
      value == "07" ~ "Angle - Image Angle",
      value == "08" ~ "Angle - Measurement Angle",
      value == "09" ~ "Image Quality - Gain",
      value == "10" ~ "Image Quality - Depth",
      value == "11" ~ "Image Quality - Focus",
      value == "12" ~ "Image Quality - Section Width",
      value == "13" ~ "Image Quality - Image Size",
      value == "14" ~ "Anatomy - see QA notes" 
    ),
    detail_text = fct_relevel(detail_text,
                              "Calliper - APL",
                              "Calliper - APT",
                              "Calliper - Anterior Calliper",
                              "Calliper - Posterior Calliper",
                              "Angle - APL",
                              "Angle - APT",
                              "Angle - Image Angle",
                              "Angle - Measurement Angle",
                              "Image Quality - Gain",
                              "Image Quality - Depth",
                              "Image Quality - Focus",
                              "Image Quality - Section Width",
                              "Image Quality - Image Size",
                              "Anatomy - see QA notes" 
    ),
    summary_text = case_when(
      str_detect(detail_text, "Calliper") ~ "Total - Calliper",
      str_detect(detail_text, "Angle") ~ "Total - Angle",
      str_detect(detail_text, "Image Quality") ~ "Total - Quality",
      str_detect(detail_text, "Anatomy") ~ "Total - Anatomy"),
    summary_text = fct_relevel(
      summary_text,
      "Total - Calliper",
      "Total - Angle",
      "Total - Quality",
      "Total - Anatomy"
    )) 

summary_detail_text <- detail %>%
  group_by(financial_year, detail_text) %>%
  summarise(
    n = n()
  ) %>%
  ungroup() %>%
  pivot_wider(names_from = financial_year,
              values_from = n) %>%
  clean_names() %>%
  rename(detail = detail_text) %>%
  mutate(
    # GC - this not perfect as years will have to be changed manually
    x2019_20_p = round_half_up(x2019_20/sum(x2019_20)*100, 1),
    x2020_21_p = round_half_up(x2020_21/sum(x2020_21)*100, 1),
    x2021_22_p = round_half_up(x2021_22/sum(x2021_22)*100, 1),
    x2022_23_p = round_half_up(x2022_23/sum(x2022_23)*100, 1)
  )

summary_text <- detail %>%
  group_by(financial_year, summary_text) %>%
  summarise(
    n = n()
  ) %>%
  ungroup() %>%
  pivot_wider(names_from = financial_year,
              values_from = n) %>%
  clean_names() %>%
  rename(detail = summary_text) %>%
  mutate(
    # GC - this not perfect as years will have to be changed manually
    x2019_20_p = round_half_up(x2019_20/sum(x2019_20)*100, 1),
    x2020_21_p = round_half_up(x2020_21/sum(x2020_21)*100, 1),
    x2021_22_p = round_half_up(x2021_22/sum(x2021_22)*100, 1),
    x2022_23_p = round_half_up(x2022_23/sum(x2022_23)*100, 1)
  )


qa_std_not_met_detail <- bind_rows(summary_text, summary_detail_text) %>%
  mutate(detail = fct_relevel(detail,
                              "Calliper - APL",
                              "Calliper - APT",
                              "Calliper - Anterior Calliper",
                              "Calliper - Posterior Calliper",
                              "Total - Calliper",
                              "Angle - APL",
                              "Angle - APT",
                              "Angle - Image Angle",
                              "Angle - Measurement Angle",
                              "Total - Angle",
                              "Image Quality - Gain",
                              "Image Quality - Depth",
                              "Image Quality - Focus",
                              "Image Quality - Section Width",
                              "Image Quality - Image Size",
                              "Total - Quality",
                              "Anatomy - see QA notes",
                              "Total - Anatomy")) %>%
  arrange(detail) %>%
  select(
    detail,
    x2019_20_n = x2019_20,
    x2019_20_p,
    x2020_21_n = x2020_21,
    x2020_21_p,
    x2021_22_n = x2021_22,
    x2021_22_p,
    x2022_23_n = x2022_23,
    x2022_23_p,
  )

### Step 3i: MI Batch QA standard not met health board ----

standard_not_met_hb <- extract2 %>%
  filter(!(screen_result %in% c('05','06')),
         !is.na(audit_batch_fail)) %>%
  
  mutate(
    std_not_met = case_when(
      audit_batch_fail == "01" ~ "Screener",
      audit_batch_fail == "02" ~ "Equipment",
      audit_batch_fail == "03" ~ "Location",
      audit_batch_fail == "04" ~ "Other with notes"
    )
  ) %>%
  group_by(hb_screen, financial_year, std_not_met) %>%
  summarise(
    n = n()
  ) %>%
  ungroup() %>%
  pivot_wider(names_from = c(financial_year, std_not_met),
              values_from = n)

write_rds(standard_not_met_hb, "standard_not_met_hb.rds")

#### Step 4: Write tables out to R ----
write_rds(kpi_2_1_a, "temp/kpi_2_1_a.rds")
write_rds(kpi_2_1_b, "temp/kpi_2_1_b.rds")
write_rds(kpi_2_2, "temp/kpi_2_2.rds")
write_rds(sup_tab_4_eligible, "temp/sup_tab_4_eligible.rds")
write_rds(sup_tab_4_sr, "temp/sup_tab_4_sr.rds")
write_rds(qa_standard_not_met, "temp/qa_standard_not_met.rds")
write_rds(qa_std_not_met_detail, "temp/qa_std_not_met_detail.rds")

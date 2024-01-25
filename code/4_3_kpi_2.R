##########################################################
# 4_3_kpi_2.R
# Gavin Clark & Karen Hotopp
# 19/10/2022
#
# Processing data for Theme 3 workbook of AAA KPIs for MEG
# KPI 2: Quality Assurance and Audit of Scans
#
# Written/run on R Studio Server, R version 3.6.1
# Revised on Posit WB, R Version 4.1.2
##########################################################

#### Step 1: Housekeeping ----

# loading packages
library(readr)
library(dplyr)
library(phsmethods)
library(janitor)
library(lubridate)
library(forcats)
library(stringr)
library(tidylog)


rm(list = ls())
gc()


source(here::here("code/0_housekeeping.R"))

rm(hb_list, exclusions_path, output_path)

# Cover base file location
coverage_basefile_path <- paste0(temp_path, "/2_coverage_basefile.rds")


# Table 4 variables
end_minus_1 <- end_current %m-% years(1) 
end_minus_2 <- end_current %m-% years(2)
end_minus_3 <- end_current %m-% years(3) ## But why?

# QA standard not met detailed reasons list
qa_detail_list <- tibble(detail = c("Calliper - APL",
                                    "Calliper - APT",
                                    "Calliper - Anterior Calliper",
                                    "Calliper - Posterior Calliper",
                                    "Total (Calliper)",
                                    "Angle - APL",
                                    "Angle - APT",
                                    "Angle - Image Angle",
                                    "Angle - Measurement Angle",
                                    "Total (Angle)",
                                    "Image Quality - Gain",
                                    "Image Quality - Depth",
                                    "Image Quality - Focus",
                                    "Image Quality - Section Width",
                                    "Image Quality - Image Size",
                                    "Total (Quality)",
                                    "Anatomy - see QA notes",
                                    "Total (Anatomy)",
                                    "Total (Overall)"))

qa_batch_list <- tibble(std_not_met = c("Screener", "Equipment", "Location",
                                        "Other with notes", "Total"))

qa_recall_list <- tibble(recall_advice = c("Immediate recall", 
                                           "Recall in current cycle",
                                           "No recall: Satisfactory interim scan",
                                           "No recall: Referred to vascular",
                                           "No recall: Verified by 2nd opinion",
                                           "Total"))


#### Step 2: Read in and process data ----
extract <- read_rds(extract_path)

coverage_basefile <- read_rds(coverage_basefile_path)


### Step 3: Create summary tables ----
# Create relevant subset of data
extract2 <- extract %>%
  # filter for relevant dates and keep positive, negative and non-vis screens
  filter(between(date_screen, as.Date(start_date), as.Date(end_date)),
         screen_result %in% c('01','02','04')) %>%
  mutate(#fin_year = extract_fin_year(date_screen), # already in df
         non_vis_n = if_else(screen_result == '04', 1, 0),
         screened_n = if_else(screen_result %in% c('01','02','04'), 1, 0),
         # year when patient turned 66
         fin_year_66 = extract_fin_year(dob + years(66))) 

#### KPI 2.1a ----
# Percentage of screening appointments, where the aorta could not be visualised
# Denominator = Total number of attended scans (excluding technical failure) 
# Numerator = Number of scans with a screening result of non-visualisation
kpi_2_1a <- extract2 %>%
  group_by(financial_year, hb_screen) %>% 
  summarise(non_vis_n = sum(non_vis_n),
            screen_n = sum(screened_n)) %>%
  group_modify(~adorn_totals(.x, where = "row", name = "Scotland")) |>  
  ungroup()

kpi_2_1a <- kpi_2_1a %>%
  mutate(hb_screen = fct_relevel(as.factor(hb_screen), "Scotland"),
         non_vis_p = round_half_up(non_vis_n/screen_n*100, 1)) %>%
  arrange(hb_screen) |> 
  mutate(kpi = "KPI 2.1a") |> 
  select(hb_screen, kpi, financial_year, screen_n, non_vis_n, non_vis_p) |> 
  pivot_longer(!hb_screen:financial_year, 
               names_to = "group", values_to = "value")


#### KPI 2.1b ----
# Percentage of MEN screened where aorta could not be visualised
# Denominator = The number of MEN attended screening, excluding technical failure
# Numerator = Number of MEN with at least 1 screen where the aorta could not be 
# visualised
extract2_dedup_hb <- extract2 %>%
  # GC - keeps non-visualised if there is one
  arrange(upi, financial_year, hb_screen, desc(non_vis_n)) %>%
  distinct(upi, financial_year, hb_screen, .keep_all = TRUE)

extract2_dedup_scotland <- extract2 %>%
  # GC - keeps non-visualised if there is one
  arrange(upi, financial_year, desc(non_vis_n)) %>%
  distinct(upi, financial_year, .keep_all = TRUE)
# GC TO DO
# decide whether it is acceptable to have differing totals for scotland
# vs. if adding the HBs individually
# ##!! Can we talk through this?

kpi_2_1b_hb <- extract2_dedup_hb %>%
  group_by(financial_year, hb_screen) %>%
  summarise(non_vis_n = sum(non_vis_n),
            screen_n = sum(screened_n)) %>%
  ungroup()

kpi_2_1b_scotland <- extract2_dedup_scotland %>%
  mutate(hb_screen = "Scotland") %>%
  group_by(financial_year, hb_screen) %>% 
  summarise(non_vis_n = sum(non_vis_n),
            screen_n = sum(screened_n)) %>%
  ungroup()

kpi_2_1b <- bind_rows(kpi_2_1b_scotland, kpi_2_1b_hb) %>%
  mutate(hb_screen = fct_relevel(as.factor(hb_screen), "Scotland"),
         non_vis_p = round_half_up(non_vis_n/screen_n * 100, 1),
         kpi = "KPI 2.1b") |> 
  select(hb_screen, kpi, financial_year, screen_n, non_vis_n, non_vis_p) |> 
  pivot_longer(!hb_screen:financial_year, 
               names_to = "group", values_to = "value")

rm(extract2_dedup_hb, extract2_dedup_scotland, kpi_2_1b_hb, kpi_2_1b_scotland)


### KPI 2.2 ----
# Percentage of images that failed the audit and required an immediate recall 
# Create subset of extract focused on audited records
extract_audit <- extract %>%
  filter(between(date_screen, as.Date(start_date), as.Date(end_date)),
         # keep records sampled for QA audit
         audit_flag == '01') %>%
  mutate(audit_n = if_else(audit_flag == '01', 1, 0),
         # failed audit and immediate recall
         recall_n = if_else(audit_result == '02' & 
                              audit_outcome == '01', 1, 0),
         # either not audited or no audit result 
         no_audit_result_n = case_when(is.na(audit_result) ~ 1,
                                       !audit_result %in% c("01", "02") ~ 1,
                                       TRUE ~ 0),
         # passed audit
         standard_met_n = if_else(audit_result == '01', 1, 0),
         # immediate recall
         imm_recall_n =  case_when(audit_outcome == '01' ~ 1, TRUE ~ 0),
         # recall in current cycle
         recall_cc_n = case_when(audit_outcome == '02' ~ 1, TRUE ~ 0),
         # no recall: satisfactory interim scan
         no_recall_sat_interim_n = case_when(audit_outcome == '03' ~ 1, TRUE ~ 0),
         # no recall: referred to vascular
         no_recall_refer_vasc_n = case_when(audit_outcome == '04' ~ 1, TRUE ~ 0),
         # no recall: verified by second opinion
         no_recall_sec_opin_n = case_when(audit_outcome == '05' ~ 1,
                                          is.na(audit_outcome) ~ 0, TRUE ~ 0))

kpi_2_2 <- extract_audit %>%
  group_by(financial_year, hb_screen) %>%
  summarise(audit_n = sum(audit_n),
            recall_n = sum(recall_n)) %>%
  group_modify(~adorn_totals(.x, where = "row", name = "Scotland")) |>  
  ungroup() |>
  mutate(recall_p = round_half_up(recall_n/audit_n*100, 1))

kpi_2_2 <- kpi_2_2 %>%
  mutate(hb_screen = fct_relevel(as.factor(hb_screen), "Scotland")) %>%
  arrange(financial_year, hb_screen) %>%
  mutate(kpi = "KPI 2.2") |> 
  select(hb_screen, kpi, financial_year, audit_n, recall_n, recall_p) |> 
  pivot_longer(!hb_screen:financial_year, 
               names_to = "group", values_to = "value")


### KPI 2.2 Additional A ----
## Number of screens selected for the QA audit by audit result
kpi_2_2_add_a <- extract_audit %>%
  group_by(financial_year, hb_screen) %>%
  summarise(audit_n = sum(audit_n),
            no_audit_result_n = sum(no_audit_result_n),
            audit_n2 = sum(audit_n),
            standard_met_n = sum(standard_met_n),
            standard_not_met_n = audit_n - standard_met_n,
            ) %>%
  group_modify(~adorn_totals(.x, where = "row", name = "Scotland")) |>  
  ungroup() |>
  mutate(
    no_audit_result_p = round_half_up(no_audit_result_n/audit_n*100, 1),
    standard_met_p = round_half_up(standard_met_n/audit_n*100, 1),
    standard_not_met_p = round_half_up(standard_not_met_n/audit_n*100, 1)
  )

kpi_2_2_add_a <- kpi_2_2_add_a %>%
  mutate(hb_screen = fct_relevel(as.factor(hb_screen), "Scotland")) %>%
  arrange(financial_year, hb_screen) %>%
  mutate(kpi = "KPI 2.2 Additional A") |> 
  select(hb_screen, kpi, financial_year, audit_n, no_audit_result_n, 
         no_audit_result_p, audit_n2, standard_met_n, standard_met_p, 
         standard_not_met_n, standard_not_met_p) |> 
  pivot_longer(!hb_screen:financial_year, 
               names_to = "group", values_to = "value")


### KPI 2.2 Additional B ----
## Number of screens that did not meet the QA audit standard by audit outcome
## (recall advice)
## Any reason this can't sit with above extract reformatting (line 137)?
# extract_audit_b <- extract %>%
#   filter(between(date_screen, as.Date(start_date), as.Date(end_date)),
#          # keep records sampled for QA audit
#          audit_flag == '01') %>%
#   mutate(audit_n = if_else(audit_flag == '01', 1, 0),
#          # failed audit and immediate recall
#          recall_n = if_else(audit_result == '02' & 
#                               audit_outcome == '01', 1, 0),
#          # either not audited or no audit result 
#          no_audit_result_n = case_when(is.na(audit_result) ~ 1,
#                                        !audit_result %in% c("01", "02") ~ 1,
#                                        TRUE ~ 0),
#          # passed audit
#          standard_met_n = if_else(audit_result == '01', 1, 0),
#          # immediate recall
#          imm_recall_n =  case_when(audit_outcome == '01' ~ 1, TRUE ~ 0),
#          # recall in current cycle
#          recall_cc_n = case_when(audit_outcome == '02' ~ 1, TRUE ~ 0),
#          # no recall: satisfactory interim scan
#          no_recall_sat_interim_n = case_when(audit_outcome == '03' ~ 1, TRUE ~ 0),
#          # no recall: referred to vascular
#          no_recall_refer_vasc_n = case_when(audit_outcome == '04' ~ 1, TRUE ~ 0),
#          # no recall: verified by second opinion
#          no_recall_sec_opin_n = case_when(audit_outcome == '05' ~ 1,
#                                           is.na(audit_outcome) ~ 0, TRUE ~ 0))

kpi_2_2_add_b <- extract_audit %>%
  group_by(financial_year, hb_screen) %>%
  summarise(
    audit_n = sum(audit_n),
    standard_met_n = sum(standard_met_n),
    standard_not_met_n = (audit_n - standard_met_n),
    standard_not_met_n = sum(standard_not_met_n),
    imm_recall_n = sum(imm_recall_n),
    recall_cc_n = sum(recall_cc_n),
    no_recall_sat_interim_n = sum(no_recall_sat_interim_n),
    no_recall_refer_vasc_n = sum(no_recall_refer_vasc_n),
    no_recall_sec_opin_n = sum(no_recall_sec_opin_n),
    no_audit_result_n = sum(no_audit_result_n)
    ) %>%
  group_modify(~adorn_totals(.x, where = "row", name = "Scotland")) |>  
  ungroup() |>
  mutate(
    imm_recall_p = round_half_up(imm_recall_n/standard_not_met_n*100, 1),
    recall_cc_p = round_half_up(recall_cc_n/standard_not_met_n*100, 1),
    no_recall_sat_interim_p = round_half_up(no_recall_sat_interim_n/standard_not_met_n*100, 1),
    no_recall_refer_vasc_p = round_half_up(no_recall_refer_vasc_n/standard_not_met_n*100, 1),
    no_recall_sec_opin_p = round_half_up(no_recall_sec_opin_n/standard_not_met_n*100, 1),
    no_audit_result_p = round_half_up(no_audit_result_n/standard_not_met_n*100, 1)
  )

kpi_2_2_add_b <- kpi_2_2_add_b %>%
  mutate(hb_screen = fct_relevel(as.factor(hb_screen), "Scotland")) %>%
  arrange(financial_year, hb_screen) %>%
  mutate(kpi = "KPI 2.2 Additional B") |> 
  select(hb_screen, kpi, financial_year, standard_not_met_n, imm_recall_n, 
         imm_recall_p, recall_cc_n, recall_cc_p, recall_cc_n, recall_cc_p, 
         no_recall_sat_interim_n, no_recall_sat_interim_p, no_recall_refer_vasc_n, 
         no_recall_refer_vasc_p, no_recall_sec_opin_n, no_recall_sec_opin_p,
         no_audit_result_n, no_audit_result_p) |> 
  pivot_longer(!hb_screen:financial_year, 
               names_to = "group", values_to = "value")

rm(extract_audit)
    
### Supplementary table 4 ----
## Percentage of men tested who had no conclusive final result from initial 
# screening as aorta could not be fully visualized

##!! This doesn't seem to be used again... what is it for?? To delete...?
# coverage_basefile_a <- coverage_basefile %>%
#   #filter(!is.na(screen_result)) %>%
#   # create new variable for FY when patient turned 66
#   mutate(fin_year_66 = case_when(  ##!! LOOK AT CHANGING BELOW AWAY FROM DATES!!
#     age_calculate(dob, as.Date("2020-03-31")) == 66 ~ "2019/20",
#     age_calculate(dob, as.Date("2021-03-31")) == 66 ~ kpi_report_years[[1]],
#     age_calculate(dob, as.Date("2022-03-31")) == 66 ~ kpi_report_years[[2]],
#     age_calculate(dob, as.Date("2023-03-31")) == 66 ~ kpi_report_years[[3]],
#     TRUE ~ "66 in a different year"))

## Eligible cohort ---
extract_sup_4 <- extract %>%
  filter(!is.na(screen_result)) %>%
  # create flag for non-vis screens
  mutate(non_vis_n = if_else(screen_result == '04', 1, 0)) %>%
  group_by(upi) %>%
  # create flag for any UPIs that have had a non-visualization
  mutate(non_vis_any = max(non_vis_n)) %>%
  ungroup() %>%
  filter(non_vis_any == 1) %>%
  mutate(non_vis_n = 1,
         # create flag where screens were not immediate recall,
         result_temp = case_when(audit_outcome == "01" ~ 0,
                                 # and were positive or negative for AAA >= 3.0cm
                                 screen_result %in% c("01", "02", "05", "06") ~ 1,
                                 TRUE ~ 0)) %>%
  group_by(upi) %>%
  # create flag for any UPIs that fit result_temp flag
  mutate(flag_result = max(result_temp)) %>%
  ungroup() %>%
  filter(flag_result == 0, 
         pat_elig != '03', # not a self-referral
         screen_type %in% c('01', '03'), # initial/QA initial screen
         screen_result != '03') # not a tech fail

extract_sup_4 %>% count(flag_result)

# Create look-up of total non-visualizations per individual
non_vis_lookup <- extract_sup_4 %>%
  group_by(upi) %>%
  summarise(non_vis = n()) %>%
  ungroup()

# # Create 2nd look-up of individuals with multiple non-visualizations 
# ## NEEDED?? Don't think this is used anywhere...
# non_vis_lookup_2 <- non_vis_lookup %>% 
#   filter(non_vis >= 2)

non_vis_match <- coverage_basefile %>%
  filter(!is.na(screen_result)) %>%
  left_join(non_vis_lookup, by = "upi") %>%
  mutate(
    #non_vis_n = if_else(screen_result == '04', 1, 0), ## NEEDED??
    non_vis = replace_na(non_vis, 0),
    # create new variable for FY when patient turned 66
    fin_year_66 = case_when(
      age_calculate(dob, end_minus_3) == 66 ~ finyear_minus_3, ## NEEDED??
      age_calculate(dob, end_minus_2) == 66 ~ kpi_report_years[[1]],
      age_calculate(dob, end_minus_1) == 66 ~ kpi_report_years[[2]],
      age_calculate(dob, end_current) == 66 ~ kpi_report_years[[3]],
      TRUE ~ "66 in a different year"
    )) %>%
  filter(fin_year_66 != "66 in a different year")

# Create summary tables
sup_tab_4_eligible <- non_vis_match %>%
  # hb_screen not on coverage basefile, try hbres ##!! GO BACK AND FIX THIS!!
  group_by(fin_year_66, hbres) %>%
  summarise(n = n(),
            non_vis_n = sum(non_vis >= 1),
            non_vis_2_more_n = sum(non_vis >= 2),
            non_vis_1_n = sum(non_vis == 1)) %>%
  ungroup() %>%
  group_by(fin_year_66) |> 
  group_modify(~ adorn_totals(.x, where = "row",
                              name = "Scotland")) |>
  ungroup() |> 
  mutate(non_vis_p = round_half_up(non_vis_n/n*100, 1),
         non_vis_2_more_p = round_half_up(non_vis_2_more_n/n*100, 1),
         non_vis_1_p = round_half_up(non_vis_1_n/n*100, 1))

sup_tab_4_eligible <- sup_tab_4_eligible %>%
  mutate(hbres = fct_relevel(as.factor(hbres), "Scotland")) %>%
  select(health_board = hbres, 
         financial_year = fin_year_66,
         tested_n = n,
         non_vis_n, non_vis_p,
         non_vis_2_more_n, non_vis_2_more_p,
         non_vis_1_n, non_vis_1_p) %>%
  mutate(kpi = "Table 4: Eligible cohort", .after = health_board)
  

## CAN THIS BE DELETED?? LOOKS LIKE DUPLICATION? 
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

## Self-referrals ---
sup_tab_4_sr <- extract %>%
  filter(!is.na(screen_result)) %>%
  # create flag for non-vis screens
  mutate(non_vis_n = if_else(screen_result == '04', 1, 0)) %>%
  group_by(upi) %>%
  # create flag for total number of non-visualizations for each UPI and 
  # another for any UPIs that have had a non-visualization
  mutate(non_vis_count = sum(non_vis_n),
         non_vis_any = max(non_vis_n)) %>%
  ungroup() %>%
  filter(non_vis_any == 1) %>%
  mutate(non_vis_n == 1,
         # create flag where screens were not immediate recall,
         result_temp = case_when(audit_outcome == "01" ~ 0,
                                 # and were positive or negative for AAA >= 3.0cm
                                 screen_result %in% c("01", "02", "05", "06") ~ 1,
                                 TRUE ~ 0)) %>%
  group_by(upi) %>%
  mutate(flag_result = max(result_temp)) %>%
  ungroup() %>%
  filter(flag_result == 0, 
         pat_elig == '03', # not a self-referral 
         screen_type %in% c('01', '03'), # initial/QA initial screen
         screen_result != '03') %>% # not tech fail
  distinct(upi, .keep_all = TRUE) %>%
  group_by() %>%
  # create variable counts
  summarise(non_vis_n = n(),
            non_vis_2_more_n = sum(non_vis_count >= 2),
            non_vis_1_n = sum(non_vis_count == 1)) %>%
  ungroup()

# Total number self-referrals
extract_sr <- extract %>%
  filter(pat_elig == '03', # eligible: self-referral
         # want cumulative total
         date_screen <= as.Date(end_date),
         screen_type %in% c('01', '03'), # initial/QA initial screen
         !is.na(screen_result),
         screen_result != '03') %>% # not tech fail
  distinct(upi) %>% 
  count()

sup_tab_4_sr <- bind_cols(extract_sr, sup_tab_4_sr) |> 
  mutate(non_vis_p = round_half_up(non_vis_n/n*100, 1),
         non_vis_2_more_p = round_half_up(non_vis_2_more_n/n*100, 1),
         non_vis_1_p = round_half_up(non_vis_1_n/n*100, 1)) |> 
  mutate(health_board = "Scotland",
         kpi = "Table 4: Self-referral",
         financial_year = kpi_report_years[[3]]) |> 
  select(health_board, kpi, financial_year,
         tested_n = n,
         non_vis_n, non_vis_p,
         non_vis_2_more_n, non_vis_2_more_p,
         non_vis_1_n, non_vis_1_p)

# Combine eligible cohort and self-referrals and reshape
table_4 <- bind_rows(sup_tab_4_eligible, sup_tab_4_sr) |> 
  pivot_longer(!health_board:financial_year, 
               names_to = "group", values_to = "value")

rm(extract_sup_4, non_vis_lookup, non_vis_lookup_2, non_vis_match, extract_sr,
   sup_tab_4_eligible, sup_tab_4_sr)


### QA standard not met REASON ---- 
## Number of screens that did not meet the QA audit standard by reason
qa_standard <- extract %>%
  filter(!(screen_result %in% c('05','06')), # not an external result (+ve or -ve)
         audit_result == '02', # standard not met
         #financial_year  %in% c(finyear_minus_3, kpi_report_years)) %>% # Why 4 years? Report only uses 3.
         financial_year  %in% c(kpi_report_years)) %>%
  # GC - add to issues (Which part?)
  mutate(
    audit_n = if_else(audit_flag == '01', 1, 0),
    standard_met_n = if_else(audit_result == '01', 1, 0), # Haven't these been removed?
    audit_fail_reason_text = case_when(audit_fail_reason == '01' ~ "calliper",
                                       audit_fail_reason == "02" ~ "angle",
                                       audit_fail_reason == '03' ~ "image quality",
                                       audit_fail_reason == '04' ~ "anatomy",
                                       TRUE ~ "No audit fail"),
    audit_fail_reason_text = fct_relevel(audit_fail_reason_text,
                                         "calliper",
                                         "angle",
                                         "image quality",
                                         "anatomy",
                                         "no audit fail"))

qa_standard_sum <- qa_standard %>%
  group_by(financial_year, hb_screen, audit_fail_reason_text) %>%
  ##!! Would it not make more sense to sum(audit_result) below?
  summarise(standard_not_met_n = sum(audit_n) - sum(standard_met_n)) |>  
  ungroup() %>%
  group_by(financial_year, audit_fail_reason_text) |> 
  group_modify(~adorn_totals(.x, where = "row", name = "Scotland")) |> 
  ungroup() |> 
  select(financial_year, hb_screen, standard_not_met_n, audit_fail_reason_text)


# Calculate total where standard not met
qa_standard_totals <- qa_standard_sum %>%
  group_by(financial_year, hb_screen) %>%
  summarise(standard_not_met_n = sum(standard_not_met_n)) %>%
  ungroup() %>%
  mutate(audit_fail_reason_text = "standard not met")

qa_reason <- bind_rows(qa_standard_totals, qa_standard_sum) %>%
  # GC - move to KH code? ##!! Which part? Can move fct_relevel for hb_screen
  mutate(hb_screen = fct_relevel(hb_screen, "Scotland"),
         audit_fail_reason_text = fct_relevel(audit_fail_reason_text, 
                                              "standard not met",
                                              "calliper",
                                              "angle",
                                              "image quality",
                                              "anatomy",
                                              "no audit fail")) %>%
  arrange(financial_year, hb_screen, audit_fail_reason_text) %>%
  pivot_wider(values_from = standard_not_met_n,
              names_from = c(audit_fail_reason_text),
              names_glue = "{audit_fail_reason_text}_n") |> 
  clean_names()
  
qa_reason <- qa_reason |> 
  mutate(calliper_p = round_half_up(calliper_n/standard_not_met_n*100, 1),
         angle_p = round_half_up(angle_n/standard_not_met_n*100, 1),
         image_quality_p = round_half_up(image_quality_n/standard_not_met_n*100, 1),
         anatomy_p = round_half_up(anatomy_n/standard_not_met_n*100, 1)) %>%
  mutate(kpi = "QA Not Met: Reason") |> 
  select(hb_screen, kpi, financial_year, standard_not_met_n,
         calliper_n, calliper_p, angle_n, angle_p, 
         image_quality_n, image_quality_p, anatomy_n, anatomy_p) |> 
  pivot_longer(!hb_screen:financial_year, 
               names_to = "group", values_to = "value")

rm(qa_standard_sum, qa_standard_totals)  
  

### QA standard not met DETAIL ----
## Screens that did not meet the quality assurance standard by detailed reasons
detail <- qa_standard %>%
  select(financial_year, audit_fail_1:audit_fail_5) %>%
  pivot_longer(cols = audit_fail_1:audit_fail_5,
               names_to = "column") %>%
  filter(!is.na(value)) %>%
  mutate(detail_text = case_when(value == "01" ~ "Calliper - APL",
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
                                 value == "14" ~ "Anatomy - see QA notes"),
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
                              "Anatomy - see QA notes"),
    summary_text = case_when(
      str_detect(detail_text, "Calliper") ~ "Total (Calliper)",
      str_detect(detail_text, "Angle") ~ "Total (Angle)",
      str_detect(detail_text, "Image Quality") ~ "Total (Quality)",
      str_detect(detail_text, "Anatomy") ~ "Total (Anatomy)"),
    summary_text = fct_relevel(summary_text,
                               "Total (Calliper)",
                               "Total (Angle)",
                               "Total (Quality)",
                               "Total (Anatomy)")) 

# Change FY variable to generalized level
detail <- detail |> 
  filter(financial_year %in% c(kpi_report_years)) |> # should already be filtered
  mutate(financial_year = case_when(financial_year == kpi_report_years[[1]] ~ "year1",
                                    financial_year == kpi_report_years[[2]] ~ "year2",
                                    financial_year == kpi_report_years[[3]] ~ "year3",
                                    TRUE ~ financial_year))

table(detail$financial_year)

## Calculate percent for each year
# Detailed categories
summary_detail <- detail %>%
  group_by(financial_year, detail_text) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = financial_year,
              values_from = n) %>%
  rename(detail = detail_text) %>%
  mutate(year1_p = round_half_up(year1/sum(year1)*100, 1),
         year2_p = round_half_up(year2/sum(year2)*100, 1),
         year3_p = round_half_up(year3/sum(year3)*100, 1))

# Totaled categories
summary_text <- detail %>%
  group_by(financial_year, summary_text) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = financial_year,
              values_from = n) %>%
  rename(detail = summary_text) %>%
  mutate(year1_p = round_half_up(year1/sum(year1)*100, 1),
         year2_p = round_half_up(year2/sum(year2)*100, 1),
         year3_p = round_half_up(year3/sum(year3)*100, 1))

# Total standard not met
summary <- detail %>%
  group_by(financial_year) %>%
  summarise(n = n()) %>%
  ungroup() %>%
  pivot_wider(names_from = financial_year,
              values_from = n) %>%
  mutate(detail = "Total (Overall)", .before = year1) |> 
  mutate(year1_p = round_half_up(year1/sum(year1)*100, 1),
         year2_p = round_half_up(year2/sum(year2)*100, 1),
         year3_p = round_half_up(year3/sum(year3)*100, 1))

# Combine and reorganize
qa_detail <- bind_rows(summary_text, summary_detail, summary) %>%
  # # this step (mutate) can be removed b/c of qa_detail_list step below
  # mutate(detail = fct_relevel(detail,
  #                             "Calliper - APL",
  #                             "Calliper - APT",
  #                             "Calliper - Anterior Calliper",
  #                             "Calliper - Posterior Calliper",
  #                             "Total (Calliper)",
  #                             "Angle - APL",
  #                             "Angle - APT",
  #                             "Angle - Image Angle",
  #                             "Angle - Measurement Angle",
  #                             "Total (Angle)",
  #                             "Image Quality - Gain",
  #                             "Image Quality - Depth",
  #                             "Image Quality - Focus",
  #                             "Image Quality - Section Width",
  #                             "Image Quality - Image Size",
  #                             "Total (Quality)",
  #                             "Anatomy - see QA notes",
  #                             "Total (Anatomy)",
  #                             "Total (Overall)")) %>%
  select(detail,
         year1_n = year1,
         year1_p,
         year2_n = year2,
         year2_p,
         year3_n = year3,
         year3_p)

# Insert any missing detail categories and arrange
qa_detail <- qa_detail_list |> left_join(qa_detail, by = "detail")

# Reformat
qa_detail <- qa_detail |> 
  pivot_longer(!detail, names_to = "group", values_to = "value") |> 
  mutate(financial_year = group, .after = detail) |> 
  mutate(kpi = "QA Not Met: Detail", .after = detail) |> 
  mutate(financial_year = case_when(str_detect(financial_year, "year1") ~ kpi_report_years[[1]],
                                    str_detect(financial_year, "year2") ~ kpi_report_years[[2]],
                                    str_detect(financial_year, "year3") ~ kpi_report_years[[3]]),
         group = case_when(str_detect(group, "_n") ~ "n",
                           str_detect(group, "_p") ~ "p")) |> 
  mutate(value = ifelse(is.na(value), 0, value))

rm(qa_standard, detail, summary_detail, summary_text, summary, qa_detail_list)


### Batch QA standard not met ----
## Batch QA standard not met by reason by Scotland total and HB of screening, 
## and batch standard not met screens by recall advice by HB of screening
# Scotland total ---
qa_batch_scot <- extract2 %>%
  filter(!(screen_result %in% c('05','06')), # not an external result (+ve or -ve)
         !is.na(audit_batch_fail)) %>%
  mutate(std_not_met = case_when(audit_batch_fail == "01" ~ "Screener",
                                 audit_batch_fail == "02" ~ "Equipment",
                                 audit_batch_fail == "03" ~ "Location",
                                 audit_batch_fail == "04" ~ "Other with notes")) %>%
  group_by(std_not_met) %>%
  summarise(n = n()) %>%
  group_modify(~adorn_totals(.x, where = "row", name = "Total")) |> 
  ungroup()

# Insert any missing detail categories and arrange
qa_batch_scot <- qa_batch_list |> left_join(qa_batch_scot, by = "std_not_met") |> 
  mutate(n = ifelse(is.na(n), 0, n)) |> 
  mutate(hb_screen = "Scotland", 
         kpi = "QA Batch standard not met: Reason",
         financial_year = kpi_report_years[3]) |> 
  select(hb_screen, kpi, financial_year, 
         group = std_not_met,
         value = n)
  
# HB of screening total ---
qa_batch_hb <- extract2 %>%
  filter(!(screen_result %in% c('05','06')), # not an external result (+ve or -ve)
         !is.na(audit_batch_fail)) %>%
  mutate(std_not_met = case_when(audit_batch_fail == "01" ~ "Screener",
                                 audit_batch_fail == "02" ~ "Equipment",
                                 audit_batch_fail == "03" ~ "Location",
                                 audit_batch_fail == "04" ~ "Other with notes")) %>%
  group_by(std_not_met, hb_screen) %>%
  summarise(n = n()) %>%
  ungroup() |> 
  group_by(hb_screen) |> 
  group_modify(~adorn_totals(.x, where = "row", name = "Total")) |> 
  ungroup()

# Insert any missing detail categories and arrange
qa_batch_hb <- qa_batch_list |> left_join(qa_batch_hb, by = "std_not_met") |> 
  mutate(n = ifelse(is.na(n), 0, n)) |> 
  ##!! CHANGE NEXT STEP!! This will only work for 2022/23 data and needs to 
  ## be rewritten to take multiple HBs into account; just time-saving for now,
  ## sorry to the next person who runs this code...
  mutate(hb_screen = "Tayside", 
         kpi = "QA Batch standard not met: Reason",
         financial_year = kpi_report_years[3]) |> 
  select(hb_screen, kpi, financial_year, 
         group = std_not_met,
         value = n)

# HB of screening recall advice ---
qa_batch_recall <- extract2 %>%
  filter(!(screen_result %in% c('05','06')), # not an external result (+ve or -ve)
         !is.na(audit_batch_outcome)) %>%
  mutate(recall_advice = case_when(audit_batch_outcome == "01" ~ 
                                     "Immediate recall",
                                   audit_batch_outcome == "02" ~ 
                                     "Recall in current cycle",
                                   audit_batch_outcome == "03" ~ 
                                     "No recall: Satisfactory interim scan",
                                   audit_batch_outcome == "04" ~ 
                                     "No recall: Referred to vascular",
                                   audit_batch_outcome == "05" ~ 
                                     "No recall: Verified by 2nd opinion")) #%>%

# Health Board total  
qa_batch_recall_hb <- qa_batch_recall |> 
  group_by(recall_advice, hb_screen) %>%
  summarise(n = n()) %>%
  ungroup() |> 
  group_by(hb_screen) |> 
  group_modify(~adorn_totals(.x, where = "row", name = "Total")) |> 
  ungroup()

# Insert any missing detail categories and arrange
qa_batch_recall_hb <- qa_recall_list |> left_join(qa_batch_recall_hb, 
                                               by = "recall_advice") |> 
  mutate(n = ifelse(is.na(n), 0, n)) |> 
  ##!! CHANGE NEXT STEP!! This will only work for 2022/23 data and needs to 
  ## be rewritten to take multiple HBs into account; just time-saving for now,
  ## sorry to the next person who runs this code...
  mutate(hb_screen = "Tayside", 
         kpi = "QA Batch standard not met: Recall Advice",
         financial_year = kpi_report_years[3]) |> 
  select(hb_screen, kpi, financial_year, 
         group = recall_advice,
         value = n)

# Scotland total  
qa_batch_recall_scot <- qa_batch_recall |> 
  group_by(recall_advice) %>%
  summarise(n = n()) %>%
  group_modify(~adorn_totals(.x, where = "row", name = "Total")) |> 
  ungroup() |> 
  mutate(hb_screen = "Scotland", .before = recall_advice)

# Insert any missing detail categories and arrange
qa_batch_recall_scot <- qa_recall_list |> left_join(qa_batch_recall_scot, 
                                                    by = "recall_advice") |> 
  mutate(n = ifelse(is.na(n), 0, n)) |> 
##!! CHANGE NEXT STEP!! This will only work for 2022/23 data and needs to 
## be rewritten to take multiple HBs into account; just time-saving for now,
## sorry to the next person who runs this code...
mutate(hb_screen = "Scotland", 
       kpi = "QA Batch standard not met: Recall Advice",
       financial_year = kpi_report_years[3]) |> 
  select(hb_screen, kpi, financial_year, 
         group = recall_advice,
         value = n)
  
# Combine
qa_batch_recall <- bind_rows(qa_batch_recall_hb, qa_batch_recall_scot)

rm(qa_batch_recall_hb, qa_batch_recall_scot) 
  

#### Step 4: Combine and Output ----
## Check names of variables to see if they can be combined
names(kpi_2_1a)
names(kpi_2_1b)
names(kpi_2_2)
names(kpi_2_2_add_a)
names(table_4)
names(kpi_2_2_add_b)
names(qa_reason)
names(qa_detail)
names(qa_batch_scot)
names(qa_batch_hb)
names(qa_batch_recall)

## Temporarily change table_4 from "health_board" to "hb_screen"; Eventually,
## should go back and change variable (will need to go back into scripts 1 & 2.)
table_4 <- rename(table_4, hb_screen = health_board)

## This doesn't really work, but I want a nice, neat package at the moment, 
## so I'm doing it anyway. It would make sense to concatenate the "detail" variable 
## with the "group" variable and add Scotland as the hb_screen, but that would 
## mean having to undo that in the Excel output script, creating more unecessary work.
qa_detail <- rename(qa_detail, hb_screen = detail)

# Combine
# Note: Table 4 not added until after new historical file has been created,
# as data is recalculated for each report and not retained in historical file
kpi_2 <- bind_rows(kpi_2_1a, kpi_2_1b, kpi_2_2, kpi_2_2_add_a, 
                   kpi_2_2_add_b, qa_reason, qa_detail,  
                   qa_batch_scot, qa_batch_hb, qa_batch_recall)


### Historical database ---
## Full records (currently only from 2019/20; need to add full historical)
hist_db <- read_rds(paste0(hist_path,"/aaa_kpi_historical_theme3.rds"))

table(hist_db$kpi, hist_db$fin_year)
table(kpi_2$kpi, kpi_2$financial_year)


if (season == "spring") {
  table(hist_db$kpi, hist_db$fin_year) 
  
  print("Don't add to the history file. Move along to next step")

    } else {

      if (season == "autumn") {
        # save a backup of hist_db
        write_rds(hist_db, paste0(hist_path, "/aaa_kpi_historical_theme3_bckp.rds"))
        # change permissions to give the group read/write
        Sys.chmod(paste0(hist_path, "/aaa_kpi_historical_theme3_bckp.rds"),
                  mode = "664", use_umask = FALSE)
        
        ## Combine data from current to historical
        current_kpi <- kpi_2 |> 
          filter(financial_year == kpi_report_years[3]) |> 
          rename(fin_year = financial_year, ## decide how these should be standardized
                 hb = hb_screen) ## move this higher up
        
        print(table(current_kpi$kpi, current_kpi$fin_year)) 

        hist_db <- bind_rows(hist_db, current_kpi)
        print(table(hist_db$kpi, hist_db$fin_year)) 
        
        ## Write out new historic file
        write_rds(hist_db, paste0(hist_path, "/aaa_kpi_historical_theme3.rds"))
        # change permissions to give the group read/write
        Sys.chmod(paste0(hist_path, "/aaa_kpi_historical_theme3.rds"),
                  mode = "664", use_umask = FALSE)
        
        print("You made history! Proceed to the next step")
        
      } else {
      
      print("Go check your calendar!")
      
    }}


### Current database ---
## Take current reporting years from new historic
kpi_2_full <- hist_db |> 
  filter(fin_year %in% c(kpi_report_years))

## Add in Table 4 data
table(table_4$kpi, table_4$financial_year)

# Should be able to remove next chunk once variables are standardized
table_4 <- table_4 |> 
  rename(fin_year = financial_year, ## decide how these should be standardized
         hb = hb_screen) |> ## move this higher up
  filter(fin_year %in% c(kpi_report_years)) ## go back and remove year-1 (2019/20) when creating df

table(table_4$kpi, table_4$fin_year)

kpi_2_full <- bind_rows(kpi_2_full, table_4)

table(kpi_2_full$kpi, kpi_2_full$fin_year)

## Save data block
write_rds(kpi_2_full, paste0(temp_path, "/3_1_kpi_2_", yymm, ".rds"))

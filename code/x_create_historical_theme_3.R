# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# x_create_historical_theme_3.R
# 
# Angus Morton
# Oct 2023
# 
# Create database of historical published figures for theme 3 AAA KPIs
# 
# Written/run on Posit WB, R 4.1.2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Notes:
# Call in published figures for history of program
# For now, start from 2019/20


#### 1: Housekeeping ----
library(dplyr)
library(openxlsx)
library(stringr)
library(forcats)
library(readr)
library(tidylog)

rm(list=ls())
gc()


## Values
names_kpi2 <- c("hb", 
                "FY_2019/20_screen_n", "FY_2019/20_nonvi_n",
                "FY_2019/20_rate_p",
                "FY_2020/21_screen_n", "FY_2020/21_nonvi_n",
                "FY_2020/21_rate_p",
                "FY_2021/22_screen_n", "FY_2021/22_nonvi_n",
                "FY_2021/22_rate_p")

## File paths
pub_path <- "/PHI_conf/AAA/Topics/Screening/publications/Completed/"
kpi_path <- "/PHI_conf/AAA/Topics/Screening/KPI/"
temp_path <- "/temp/KPIs/KPI2.1a - KPI2.2"
temp_3_path <- "/temp/3. Quality Assurance"


#### 2: Create historical files ----
### KPI 2.1a ----
## Health Board of Residence
aaa_2.1a <- read.xlsx(paste0(kpi_path, 202209, temp_path,
                            "/KPI_2.1a_Output_TABLE_20220927.xlsx"), 
                     cols = 2:11, rows = c(3,5:19))

names(aaa_2.1a) <- names_kpi2
names(aaa_2.1a)

aaa_2.1a <- aaa_2.1a |> 
  mutate(hb = str_remove(hb, 'xml:space="preserve">'),
         hb = str_replace_all(hb, "&amp;", "&"),
         hb = str_replace_all(hb, " Scotland", "Scotland")) |>
  pivot_longer(!hb, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year) |> 
  # clean variable levels
  mutate(fin_year = str_remove(fin_year, "FY_"),
         fin_year = str_remove(fin_year, "_screen_n"),
         fin_year = str_remove(fin_year, "_nonvi_n"),
         fin_year = str_remove(fin_year, "_rate_p"),
         group = case_when(str_detect(group, "_screen") ~ "screen_n",
                           str_detect(group, "_nonvi") ~ "non_vis_n",
                           str_detect(group, "_rate_p") ~ "non_vis_p"),
         kpi = "KPI 2.1a", .after = hb) |>
  glimpse()


### KPI 2.1b ----
## Health Board of Residence
aaa_2.1b <- read.xlsx(paste0(kpi_path, 202209, temp_path,
                             "/KPI_2.1b_Output_TABLE_20220927.xlsx"), 
                     cols = 2:11, rows = c(3,5:19))

names(aaa_2.1b) <- names_kpi2
names(aaa_2.1b)

aaa_2.1b <- aaa_2.1b |> 
  mutate(hb = str_remove(hb, 'xml:space="preserve">'),
         hb = str_replace_all(hb, "&amp;", "&"),
         hb = str_replace_all(hb, " Scotland", "Scotland")) |>
  pivot_longer(!hb, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year) |> 
  # clean variable levels
  mutate(fin_year = str_remove(fin_year, "FY_"),
         fin_year = str_remove(fin_year, "_screen_n"),
         fin_year = str_remove(fin_year, "_nonvi_n"),
         fin_year = str_remove(fin_year, "_rate_p"),
         group = case_when(str_detect(group, "_screen") ~ "screen_n",
                           str_detect(group, "_nonvi") ~ "non_vis_n",
                           str_detect(group, "_rate_p") ~ "non_vis_p"),
         kpi = "KPI 2.1b", .after = hb) |>
  glimpse()


### KPI 2.2 ----
## Health Board of Residence

names_kpi2.2 <- c("hb", 
                  "FY_2019/20_audit_n", "FY_2019/20_recall_n",
                  "FY_2019/20_recall_p",
                  "FY_2020/21_audit_n", "FY_2020/21_recall_n",
                  "FY_2020/21_recall_p",
                  "FY_2021/22_audit_n", "FY_2021/22_recall_n",
                  "FY_2021/22_recall_p")

aaa_2.2 <- read.xlsx(paste0(kpi_path, 202209, temp_path,
                             "/KPI_2.2_Output_TABLE_20220927.xlsx"), 
                      cols = 2:11, rows = c(3,5:19))

names(aaa_2.2) <- names_kpi2.2
names(aaa_2.2)

aaa_2.2 <- aaa_2.2 |> 
  mutate(hb = str_remove(hb, 'xml:space="preserve">'),
         hb = str_replace_all(hb, "&amp;", "&"),
         hb = str_replace_all(hb, " Scotland", "Scotland")) |>
  pivot_longer(!hb, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year) |> 
  # clean variable levels
  mutate(fin_year = str_remove(fin_year, "FY_"),
         fin_year = str_remove(fin_year, "_audit_n"),
         fin_year = str_remove(fin_year, "_recall_n"),
         fin_year = str_remove(fin_year, "_recall_p"),
         group = case_when(str_detect(group, "_audit") ~ "audit_n",
                           str_detect(group, "_recall") ~ "recall_n",
                           str_detect(group, "_recall_p") ~ "recall_p"),
         kpi = "KPI 2.2", .after = hb) |>
  glimpse()

### kpi 2.2 additional (a) ----

names_kpi2.2_add_a <- c("hb", 
                        "FY_2019/20_audit_n",
                        "FY_2019/20_no_audit_result_n", "FY_2019/20_no_audit_result_p",
                        "FY_2019/20_standard_met_n", "FY_2019/20_standard_met_p",
                        "FY_2019/20_standard_not_met_n", "FY_2019/20_standard_not_met_p",
                        
                        "FY_2020/21_audit_n",
                        "FY_2020/21_no_audit_result_n", "FY_2020/21_no_audit_result_p",
                        "FY_2020/21_standard_met_n", "FY_2020/21_standard_met_p",
                        "FY_2020/21_standard_not_met_n", "FY_2020/21_standard_not_met_p",
                        
                        "FY_2021/22_audit_n",
                        "FY_2021/22_no_audit_result_n", "FY_2021/22_no_audit_result_p",
                        "FY_2021/22_standard_met_n", "FY_2021/22_standard_met_p",
                        "FY_2021/22_standard_not_met_n", "FY_2021/22_standard_not_met_p")

aaa_2.2_add_a <- read.xlsx(paste0(kpi_path, 202209, temp_3_path,
                                  "/Additional_KPI_2.2_all_auditresults.xlsx"), 
                           cols = c(2,43:45,47:53,55:61,63:66), rows = c(9:23),
                           colNames = FALSE)

names(aaa_2.2_add_a) <- names_kpi2.2_add_a
names(aaa_2.2_add_a)

aaa_2.2_add_a <- aaa_2.2_add_a |> 
  mutate(hb = str_remove(hb, 'xml:space="preserve">'),
         hb = str_replace_all(hb, "&amp;", "&")) |>
  mutate(across(everything(), str_remove, 'xml:space="preserve">'),
         across(2:last_col(), str_replace, "-", "0"),
         across(2:last_col(), as.numeric)) |> 
  pivot_longer(!hb, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year) |> 
  # clean variable levels
  mutate(fin_year = str_remove(fin_year, "FY_"),
         fin_year = str_remove(fin_year, "_audit_n"),
         fin_year = str_remove(fin_year, "_no_audit_result_n"),
         fin_year = str_remove(fin_year, "_no_audit_result_p"),
         fin_year = str_remove(fin_year, "_standard_met_n"),
         fin_year = str_remove(fin_year, "_standard_met_p"),
         fin_year = str_remove(fin_year, "_standard_not_met_n"),
         fin_year = str_remove(fin_year, "_standard_not_met_p"),
         group = case_when(str_detect(group, "_audit_n") ~ "audit_n",
                           str_detect(group, "_no_audit_result_n") ~ "no_audit_result_n",
                           str_detect(group, "_no_audit_result_p") ~ "no_audit_result_p",
                           str_detect(group, "_standard_met_n") ~ "standard_met_n",
                           str_detect(group, "_standard_met_p") ~ "standard_met_p",
                           str_detect(group, "_standard_not_met_n") ~ "standard_not_met_n",
                           str_detect(group, "_standard_not_met_p") ~ "standard_not_met_p"),
         kpi = "KPI 2.2 Additional A", .after = hb) |>
  glimpse()


### Eligible no final result ----

# total

names_elig_non_vis_total <- c("hb", 
                              "FY_2019/20_tested_n", "FY_2019/20_non_vis_total_n",
                              "FY_2019/20_non_vis_total_p",
                              "FY_2020/21_tested_n", "FY_2020/21_non_vis_total_n",
                              "FY_2020/21_non_vis_total_p",
                              "FY_2021/22_tested_n", "FY_2021/22_non_vis_total_n",
                              "FY_2021/22_non_vis_total_p")

elig_non_vis_total <- read.xlsx(paste0(kpi_path, 202209, temp_3_path,
                                       "/KPI_2.1_additional_TotalNonVis.xlsx"), 
                                cols = c(2:11), rows = c(4:18),
                                colNames = FALSE)

names(elig_non_vis_total) <- names_elig_non_vis_total
names(elig_non_vis_total)

elig_non_vis_total <- elig_non_vis_total |> 
  mutate(hb = str_remove(hb, 'xml:space="preserve">'),
         hb = str_replace_all(hb, "&amp;", "&"),
         hb = str_replace(hb, "AScotland", "Scotland")) |>
  pivot_longer(!hb, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year) |> 
  # clean variable levels
  mutate(fin_year = str_remove(fin_year, "FY_"),
         fin_year = str_remove(fin_year, "_tested_n"),
         fin_year = str_remove(fin_year, "_non_vis_total_n"),
         fin_year = str_remove(fin_year, "_non_vis_total_p"),
         group = case_when(str_detect(group, "_tested") ~ "tested_n",
                           str_detect(group, "_non_vis_total_n") ~ "non_vis_total_n",
                           str_detect(group, "_non_vis_total_p") ~ "non_vis_total_p"),
         kpi = "Eligible No Result", .after = hb) |>
  glimpse()

# 2 Screens

names_elig_non_vis_2_screens <- c("hb", 
                              "FY_2019/20_non_vis_2_screens_n",
                              "FY_2019/20_non_vis_2_screens_p",
                              "FY_2020/21_non_vis_2_screens_n",
                              "FY_2020/21_non_vis_2_screens_p",
                              "FY_2021/22_non_vis_2_screens_n",
                              "FY_2021/22_non_vis_2_screens_p")

elig_non_vis_2_screens <- read.xlsx(paste0(kpi_path, 202209, temp_3_path,
                                       "/KPI_2.1_additional_TwoPlusNonVis.xlsx"), 
                                cols = c(2,4,5,7,8,10,11), rows = c(4:18),
                                colNames = FALSE)

names(elig_non_vis_2_screens) <- names_elig_non_vis_2_screens
names(elig_non_vis_2_screens)

elig_non_vis_2_screens <- elig_non_vis_2_screens |> 
  mutate(hb = str_remove(hb, 'xml:space="preserve">'),
         hb = str_replace_all(hb, "&amp;", "&"),
         hb = str_replace(hb, "AScotland", "Scotland")) |>
  pivot_longer(!hb, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year) |> 
  # clean variable levels
  mutate(fin_year = str_remove(fin_year, "FY_"),
         fin_year = str_remove(fin_year, "_non_vis_2_screens_n"),
         fin_year = str_remove(fin_year, "_non_vis_2_screens_p"),
         group = case_when(str_detect(group, "_non_vis_2_screens_n") ~ "non_vis_2_screens_n",
                           str_detect(group, "_non_vis_2_screens_p") ~ "non_vis_2_screens_p"),
         kpi = "Eligible No Result", .after = hb) |>
  glimpse()

# combine total and 2 screen
eligible_non_vis <- bind_rows(elig_non_vis_total, elig_non_vis_2_screens)

# calculate 1 screen figures
eligible_non_vis_1_screen <- eligible_non_vis |> 
  pivot_wider(names_from = "group", values_from = "value") |> 
  group_by(hb, kpi, fin_year) |> 
  summarise(hb = hb,
            kpi = kpi,
            fin_year = fin_year,
            non_vis_1_screen_n = non_vis_total_n-non_vis_2_screens_n,
            non_vis_1_screen_p = (non_vis_1_screen_n/tested_n)*100) |> 
  ungroup() |> 
  select(hb, kpi, fin_year, non_vis_1_screen_n, non_vis_1_screen_p) |> 
  pivot_longer(cols = c("non_vis_1_screen_n", "non_vis_1_screen_p"),
               names_to = "group", values_to = "value")

eligible_non_vis <- bind_rows(eligible_non_vis, eligible_non_vis_1_screen)

### KPI 2.2 additional (b) ----

names_kpi2.2_add_b <- c("hb", 
                        "FY_2019/20_standard_not_met_n",
                        "FY_2019/20_imm_recall_n", "FY_2019/20_imm_recall_p",
                        "FY_2019/20_recall_cc_n", "FY_2019/20_recall_cc_p",
                        "FY_2019/20_no_recall_sat_interim_n", 
                        "FY_2019/20_no_recall_sat_interim_p",
                        "FY_2019/20_no_recall_refer_vasc_n", 
                        "FY_2019/20_no_recall_refer_vasc_p",
                        "FY_2019/20_no_recall_sec_opin_n",
                        "FY_2019/20_no_recall_sec_opin_p",
                        "FY_2019/20_no_audit_result_n", "FY_2019/20_no_audit_result_p",
                        
                        "FY_2020/21_standard_not_met_n",
                        "FY_2020/21_imm_recall_n", "FY_2020/21_imm_recall_p",
                        "FY_2020/21_recall_cc_n", "FY_2020/21_recall_cc_p",
                        "FY_2020/21_no_recall_sat_interim_n", 
                        "FY_2020/21_no_recall_sat_interim_p",
                        "FY_2020/21_no_recall_refer_vasc_n", 
                        "FY_2020/21_no_recall_refer_vasc_p",
                        "FY_2020/21_no_recall_sec_opin_n",
                        "FY_2020/21_no_recall_sec_opin_p",
                        "FY_2020/21_no_audit_result_n", "FY_2020/21_no_audit_result_p",
                        
                        "FY_2021/22_standard_not_met_n",
                        "FY_2021/22_imm_recall_n", "FY_2021/22_imm_recall_p",
                        "FY_2021/22_recall_cc_n", "FY_2021/22_recall_cc_p",
                        "FY_2021/22_no_recall_sat_interim_n", 
                        "FY_2021/22_no_recall_sat_interim_p",
                        "FY_2021/22_no_recall_refer_vasc_n", 
                        "FY_2021/22_no_recall_refer_vasc_p",
                        "FY_2021/22_no_recall_sec_opin_n",
                        "FY_2021/22_no_recall_sec_opin_p",
                        "FY_2021/22_no_audit_result_n", "FY_2021/22_no_audit_result_p")

aaa_2.2_add_b <- read.xlsx(paste0(kpi_path, 202209, temp_3_path,
                                  "/Additional_KPI_2.2_all_auditresults.xlsx"), 
                           cols = c(2,68:106), rows = c(33:47),
                           colNames = FALSE)

names(aaa_2.2_add_b) <- names_kpi2.2_add_b
names(aaa_2.2_add_b)

aaa_2.2_add_b <- aaa_2.2_add_b |> 
  mutate(hb = str_remove(hb, 'xml:space="preserve">'),
         hb = str_replace_all(hb, "&amp;", "&")) |>
  mutate(across(everything(), str_remove, 'xml:space="preserve">'),
         across(2:last_col(), str_replace, "-", "0"),
         across(2:last_col(), as.numeric)) |> 
  pivot_longer(!hb, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year) |> 
  # clean variable levels
  mutate(fin_year = str_remove(fin_year, "FY_"),
         fin_year = str_remove(fin_year, "_standard_not_met_n"),
         fin_year = str_remove(fin_year, "_imm_recall_n"),
         fin_year = str_remove(fin_year, "_imm_recall_p"),
         fin_year = str_remove(fin_year, "_recall_cc_n"),
         fin_year = str_remove(fin_year, "_recall_cc_p"),
         fin_year = str_remove(fin_year, "_no_recall_sat_interim_n"),
         fin_year = str_remove(fin_year, "_no_recall_sat_interim_p"),
         fin_year = str_remove(fin_year, "_no_recall_refer_vasc_n"),
         fin_year = str_remove(fin_year, "_no_recall_refer_vasc_p"),
         fin_year = str_remove(fin_year, "_no_recall_sec_opin_n"),
         fin_year = str_remove(fin_year, "_no_recall_sec_opin_p"),
         fin_year = str_remove(fin_year, "_no_audit_result_n"),
         fin_year = str_remove(fin_year, "_no_audit_result_p"),
         group = case_when(str_detect(group, "_standard_not_met_n") ~ "standard_not_met_n",
                           str_detect(group, "_imm_recall_n") ~ "imm_recall_n",
                           str_detect(group, "_imm_recall_p") ~ "imm_recall_p",
                           str_detect(group, "_recall_cc_n") ~ "recall_cc_n",
                           str_detect(group, "_recall_cc_p") ~ "recall_,cc_p",
                           str_detect(group, "_no_recall_sat_interim_n") ~ 
                             "no_recall_sat_interim_n",
                           str_detect(group, "_no_recall_sat_interim_p") ~ 
                             "no_recall_sat_interim_p",
                           str_detect(group, "_no_recall_refer_vasc_n") ~ 
                             "no_recall_refer_vasc_n",
                           str_detect(group, "_no_recall_refer_vasc_p") ~ 
                             "no_recall_refer_vasc_p",
                           str_detect(group, "_no_recall_sec_opin_n") ~ 
                             "no_recall_sec_opin_n",
                           str_detect(group, "_no_recall_sec_opin_p") ~ 
                             "no_recall_sec_opin_p",
                           str_detect(group, "_no_audit_result_n") ~ "no_audit_result_n",
                           str_detect(group, "_no_audit_result_p") ~ "no_audit_result_p"),
         kpi = "KPI 2.2 Additional B", .after = hb) |>
  glimpse()

### QA standard not met reason ----

names_not_met_reason <- c("hb", 
                          "FY_2019/20_not_met_n",
                          "FY_2019/20_calliper_n", "FY_2019/20_calliper_p",
                          "FY_2019/20_angle_n", "FY_2019/20_angle_p",
                          "FY_2019/20_image_quality_n", "FY_2019/20_image_quality_p",
                          "FY_2019/20_anatomy_n", "FY_2019/20_anatomy_p",
                          
                          "FY_2020/21_not_met_n",
                          "FY_2020/21_calliper_n", "FY_2020/21_calliper_p",
                          "FY_2020/21_angle_n", "FY_2020/21_angle_p",
                          "FY_2020/21_image_quality_n", "FY_2020/21_image_quality_p",
                          "FY_2020/21_anatomy_n", "FY_2020/21_anatomy_p",
                          
                          "FY_2021/22_not_met_n",
                          "FY_2021/22_calliper_n", "FY_2021/22_calliper_p",
                          "FY_2021/22_angle_n", "FY_2021/22_angle_p",
                          "FY_2021/22_image_quality_n", "FY_2021/22_image_quality_p",
                          "FY_2021/22_anatomy_n", "FY_2021/22_anatomy_p")

# SPSS output seems to be missing so take straight from the excel report
# This is the published data so should be correct
not_met_reason1 <- read.xlsx(paste0(kpi_path, 202209, "/output",
                                   "/3. Quality Assurance_202209.xlsx"),
                            sheet = "QA standard not met reason",
                            cols = c(2:20), rows = c(8:22),
                            colNames = FALSE)

not_met_reason2 <- read.xlsx(paste0(kpi_path, 202209, "/output",
                                    "/3. Quality Assurance_202209.xlsx"),
                             sheet = "QA standard not met reason",
                             cols = c(3:11), rows = c(29:43),
                             colNames = FALSE)

not_met_reason <- bind_cols(not_met_reason1, not_met_reason2)

names(not_met_reason) <- names_not_met_reason
names(not_met_reason)

not_met_reason <- not_met_reason |> 
  mutate(hb = str_remove(hb, 'xml:space="preserve">'),
         hb = str_replace_all(hb, "&amp;", "&")) |>
  mutate(across(everything(), str_remove, 'xml:space="preserve">'),
         across(2:last_col(), str_replace, "-", "0"),
         across(2:last_col(), as.numeric)) |> 
  pivot_longer(!hb, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year) |> 
  # clean variable levels
  mutate(fin_year = str_remove(fin_year, "FY_"),
         fin_year = str_remove(fin_year, "_not_met_n"),
         fin_year = str_remove(fin_year, "_calliper_n"),
         fin_year = str_remove(fin_year, "_calliper_p"),
         fin_year = str_remove(fin_year, "_angle_n"),
         fin_year = str_remove(fin_year, "_angle_p"),
         fin_year = str_remove(fin_year, "_image_quality_n"),
         fin_year = str_remove(fin_year, "_image_quality_p"),
         fin_year = str_remove(fin_year, "_anatomy_n"),
         fin_year = str_remove(fin_year, "_anatomy_p"),
         group = case_when(str_detect(group, "_not_met_n") ~ "std_met_n",
                           str_detect(group, "_calliper_n") ~ "im_recall_n",
                           str_detect(group, "_calliper_p") ~ "im_recall_p",
                           str_detect(group, "_angle_n") ~ "recall_in_cycle_n",
                           str_detect(group, "_angle_p") ~ "recall_in_cycle_p",
                           str_detect(group, "_image_quality_n") ~ "nr_sat_scan_n",
                           str_detect(group, "_image_quality_p") ~ "nr_sat_scan_p",
                           str_detect(group, "_anatomy_n") ~ "nr_referred_n",
                           str_detect(group, "_anatomy_p") ~ "nr_referred_p"),
         kpi = "QA standard not met reason", .after = hb) |>
  glimpse()

### QA standard not met detail ----

names_not_met_detail <- c("n_2019/20", "p_2019/20", "n_2020/21", "p_2020/21",
                          "n_2021/22", "p_2021/22")

not_met_detail <- read.xlsx(paste0(kpi_path, 202209, "/output",
                                    "/3. Quality Assurance_202209.xlsx"),
                             sheet = "QA standard not met detail",
                             cols = c(3:8), rows = c(9:25, 27),
                             colNames = FALSE)

names(not_met_detail) <- names_not_met_detail
names(not_met_detail)

not_met_detail <- not_met_detail |> 
  mutate(measures = c("calliper_apl", "calliper_apt","calliper_anterior",
                      "calliper_posterior","calliper_total",
                      
                      "angle_apl","angle_apt","angle_image","angle_measurement",
                      "angle_total",
                      
                      "quality_gain", "quality_depth", "quality_focus",
                      "quality_width", "quality_size", "quality_total",
                      
                      "anatomy",
                      
                      "total_reasons"), .before = 1)

not_met_detail <- not_met_detail |> 
  pivot_longer(cols = c("n_2019/20", "p_2019/20", "n_2020/21", "p_2020/21",
                        "n_2021/22", "p_2021/22")) |> 
  mutate(hb = "Scotland",
         kpi = "QA standard not met detail",
         measures = paste0(measures, "_", str_sub(name, 1, 1)),
         fin_year = str_sub(name,3,9)) |> 
  select(-name) |> 
  select(hb, kpi, fin_year, group = measures, value)

# Add on standard not met
not_met_detail_totals <- read.xlsx(paste0(kpi_path, 202209, "/output",
                                          "/3. Quality Assurance_202209.xlsx"),
                                   sheet = "QA standard not met detail",
                                   cols = c(3,5,7), rows = 5,
                                   colNames = FALSE) |> 
  pivot_longer(cols = c("X1","X2","X3")) |> 
  mutate(hb = "Scotland",
         kpi = "QA standard not met detail",
         group = "total_std_not_met",
         fin_year = c("2019/20","2020/21","2021/22")) |> 
  select(hb, kpi, fin_year, group, value)

not_met_detail <- bind_rows(not_met_detail, not_met_detail_totals)

### Batch QA standard not met ----

names_batch_reason <- c("hb",
                        "FY_2019/20_screener_n", "FY_2019/20_equipment_n",
                        "FY_2019/20_location_n", "FY_2019/20_other_n",
                        
                        "FY_2020/21_screener_n", "FY_2020/21_equipment_n",
                        "FY_2020/21_location_n", "FY_2020/21_other_n",
                        
                        "FY_2021/22_screener_n", "FY_2021/22_equipment_n",
                        "FY_2021/22_location_n", "FY_2021/22_other_n")

batch_reason <- read.xlsx(paste0(kpi_path, 202209, "/output",
                                 "/3. Quality Assurance_202209.xlsx"),
                          sheet = "Batch QA standard not met",
                          cols = c(2:6, 8:11, 13:16), rows = c(18:20),
                          colNames = FALSE)

names(batch_reason) <- names_batch_reason
names(batch_reason)

batch_reason <- batch_reason |> 
  mutate(hb = str_remove(hb, 'xml:space="preserve">'),
         hb = str_replace_all(hb, "&amp;", "&"),
         hb = str_replace(hb, "Total", "Scotland")) |>
  pivot_longer(!hb, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year) |> 
  # clean variable levels
  mutate(fin_year = str_remove(fin_year, "FY_"),
         fin_year = str_remove(fin_year, "_screener_n"),
         fin_year = str_remove(fin_year, "_equipment_n"),
         fin_year = str_remove(fin_year, "_location_n"),
         fin_year = str_remove(fin_year, "_other_n"),
         group = case_when(str_detect(group, "_screener_n") ~ "screener_n",
                           str_detect(group, "_equipment_n") ~ "equipment_n",
                           str_detect(group, "_location_n") ~ "location_n",
                           str_detect(group, "_other_n") ~ "other_n"),
         kpi = "Batch QA standard not met", .after = hb,
         value = if_else(value == "-", "0", value),
         value = as.numeric(value)) |>
  glimpse()


names_batch_advice <- c("hb",
                        "FY_2019/20_im_recall_n",
                        "FY_2019/20_recall_in_cycle_n",
                        "FY_2019/20_nr_sat_scan_n",
                        "FY_2019/20_nr_referred_n",
                        "FY_2019/20_nr_2nd_opinion_n",
                        
                        "FY_2020/21_im_recall_n",
                        "FY_2020/21_recall_in_cycle_n",
                        "FY_2020/21_nr_sat_scan_n",
                        "FY_2020/21_nr_referred_n",
                        "FY_2020/21_nr_2nd_opinion_n",
                        
                        "FY_2021/22_im_recall_n",
                        "FY_2021/22_recall_in_cycle_n",
                        "FY_2021/22_nr_sat_scan_n",
                        "FY_2021/22_nr_referred_n",
                        "FY_2021/22_nr_2nd_opinion_n")

batch_advice <- read.xlsx(paste0(kpi_path, 202209, "/output",
                                 "/3. Quality Assurance_202209.xlsx"),
                          sheet = "Batch QA standard not met",
                          cols = c(2:7, 9:13, 15:19), rows = c(27:29),
                          colNames = FALSE)

names(batch_advice) <- names_batch_advice
names(batch_advice)

batch_advice <- batch_advice |> 
  mutate(hb = str_remove(hb, 'xml:space="preserve">'),
         hb = str_replace_all(hb, "&amp;", "&"),
         hb = str_replace(hb, "Total", "Scotland")) |>
  pivot_longer(!hb, names_to = "fin_year", values_to = "value") |> 
  mutate(group = fin_year, .after = fin_year) |> 
  # clean variable levels
  mutate(fin_year = str_remove(fin_year, "FY_"),
         fin_year = str_remove(fin_year, "_im_recall_n"),
         fin_year = str_remove(fin_year, "_recall_in_cycle_n"),
         fin_year = str_remove(fin_year, "_nr_sat_scan_n"),
         fin_year = str_remove(fin_year, "_nr_referred_n"),
         fin_year = str_remove(fin_year, "_nr_2nd_opinion_n"),
         group = case_when(str_detect(group, "_im_recall_n") ~ "im_recall_n",
                           str_detect(group, "_recall_in_cycle_n") ~ "recall_in_cycle_n",
                           str_detect(group, "_nr_sat_scan_n") ~ "nr_sat_scan_n",
                           str_detect(group, "_nr_referred_n") ~ "nr_referred_n",
                           str_detect(group, "_nr_2nd_opinion_n") ~ "nr_2nd_opinion_n"),
         kpi = "Batch QA standard not met", .after = hb,
         value = as.numeric(value)) |>
  glimpse()

batch <- bind_rows(batch_reason, batch_advice)

#### 3: Check and combine ----
names(aaa_2.1a)
names(aaa_2.1b)
names(aaa_2.2)
names(aaa_2.2_add_a)
names(eligible_non_vis)
names(aaa_2.2_add_b)
names(not_met_reason)
names(not_met_detail)
names(batch)

aaa_kpi_historic <- rbind(aaa_2.1a, aaa_2.1b, aaa_2.2, aaa_2.2_add_a,
                          eligible_non_vis, aaa_2.2_add_b,
                          not_met_reason, not_met_detail, batch) |> 
  mutate(hb = fct_relevel(hb, c("Scotland", "Ayrshire & Arran", "Borders",
                                "Dumfries & Galloway", "Fife", "Forth Valley",
                                "Grampian", "Greater Glasgow & Clyde", 
                                "Highland", "Lanarkshire", "Lothian", "Orkney", 
                                "Shetland", "Tayside", "Western Isles"))) |> 
  select(hb, kpi, fin_year, group, value)

table(aaa_kpi_historic$kpi)


#### 4: Write out ----
write_rds(aaa_kpi_historic, paste0(kpi_path, 
                                   "historical/aaa_kpi_historical_theme_3.rds"))
# change permissions to give the group read/write
Sys.chmod(paste0(kpi_path, "historical/aaa_kpi_historical_theme_3.rds"),
          mode = "664", use_umask = FALSE)


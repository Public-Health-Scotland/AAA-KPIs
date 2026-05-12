# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 01_Write_Excel_2.R
# 
# Karen Hotopp & Aoife McCarthy
# Sept 2023
# 
# Write out to AAA Excel workbook 2: Invitation and Attendance
# 
# Written/run on Posit WB, R 4.1.2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Notes:
# This script calls in the 3_invite_attend_yyyymm.rds file create in the
# 02_2_kpi_1_1-1_3_uptake_coverage.R script and transforms the data to print 
# directly into the theme 2 Excel file for the QPMG.

# 1: Housekeeping ----
library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(openxlsx)
library(lubridate)
library(forcats)
library(ggplot2)
library(broom)
library(phsaaa) # devtools::install_github("Public-Health-Scotland/phsaaa")
rm(list=ls())
gc()

## Values
source(here::here("code","00_housekeeping.R"))

rm (exclusions_path, extract_path, hist_path, simd_path, fy_tibble, 
    hb_tibble, cutoff_date, end_current, end_date, start_date,
    year1_end, year1_start, year2_end, year2_start, year1)

year_xx <- year(cut_off_date)
year_ww <- year_xx - 1
year_vv <- year_xx - 2
year_uu <- year_xx - 3
year_yy <- year_xx + 1

## File paths
template_path <- paste0("/PHI_conf/AAA/Topics/Screening/templates")


### 2: Import and format data ----

theme2 <- read_rds(paste0(temp_path, "/2_1_invite_attend_", yymm, ".rds")) |> 
  mutate(simd = case_when(simd == "1" ~ "1 (most deprived)",
                          simd == "5" ~ "5 (least deprived)",
                          TRUE ~ simd))

table(theme2$kpi, theme2$fin_year) 
# should be 3 most recent complete years + incomplete/active year

theme2_t6 <- read_rds(paste0(temp_path, "/2_2_Table_6_", yymm, ".rds"))
table(theme2_t6$kpi, theme2_t6$fin_year) 

theme2_dna <- read_rds(paste0(temp_path, "/2_3_dna_exclusions_", yymm, ".rds"))
table(theme2_dna$kpi, theme2_dna$fin_year) 


# 3: Format data ----
## KPI 1.1 year1 ----
## Data for three most recent complete years and extended coverage to 1 Sept
kpi_1.1 <- theme2 |> 
  filter(kpi %in% c("KPI 1.1", "KPI 1.1 Sept coverage"),
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.1 year2 ----
kpi_1.1_y2 <- eval_seasonal_diff(
  season,
  {## Data for currently active year only
    theme2 |> 
      filter(kpi %in% c("KPI 1.1"),
             fin_year == year2) |> 
      mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
      select(hbres, FY_kpi_group, value) |>
      # match Excel output
      pivot_wider(names_from = FY_kpi_group, values_from = value)},
  {# Data for currently active year and extended coverage to 1 Sept
    theme2 |>
      filter(kpi %in% c("KPI 1.1", "KPI 1.1 Sept coverage"),
             fin_year == year2) |>
      mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
      select(hbres, FY_kpi_group, value) |>
      # match Excel output
      pivot_wider(names_from = FY_kpi_group, values_from = value)}
)

## KPI 1.1 Scotland SIMD ----
## Data for three most recent complete years and extended coverage to 1 Sept
kpi_1.1_simd <- theme2 |> 
  filter(kpi %in% c("KPI 1.1 Scotland SIMD", "KPI 1.1 Scotland SIMD Sept coverage"),
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, simd, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.2a year1 & coverage to Sept ----
## Data for currently active year and extended coverage to 1 Sept
kpi_1.2a <- theme2 |> 
  filter(kpi %in% c("KPI 1.2a", "KPI 1.2a Sept coverage"),
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

# Extract extended coverage to 1 Sept
kpi_1.2a_sept <- kpi_1.2a |>
  select(hbres, contains("_cohort"), contains("Sept coverage"))

kpi_1.2a_sept <- kpi_1.2a_sept[ , c(1, 2, 5, 6, 3, 7, 8, 4, 9, 10)] 

# Remove extra historical coverage to 1 Sept data
kpi_1.2a <- kpi_1.2a[, -c(8:11)]

## KPI 1.2a year2 ----

kpi_1.2a_y2 <- eval_seasonal_diff(
  season,
  {## Data for currently active year only
    theme2 |> 
      filter(kpi %in% c("KPI 1.2a"),
             fin_year == year2) |> 
      mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
      select(hbres, FY_kpi_group, value) |>
      # match Excel output
      pivot_wider(names_from = FY_kpi_group, values_from = value)},
  {theme2 |>
      filter(kpi %in% c("KPI 1.2a", "KPI 1.2a Sept coverage"),
             fin_year == year2) |>
      mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
      select(hbres, FY_kpi_group, value) |>
      # match Excel output
      pivot_wider(names_from = FY_kpi_group, values_from = value)}
)

## KPI 1.2b year1 ----
## Data for currently active year and extended coverage to 1 Sept
kpi_1.2b <- theme2 |> 
  filter(kpi == "KPI 1.2b",
         fin_year  %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.2b year2 ----
## Data for currently active year and extended coverage to 1 Sept
kpi_1.2b_y2 <- theme2 |> 
  filter(kpi == "KPI 1.2b",
         fin_year == year2) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
  select(hbres, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.3a year1  by Scotland SIMD & coverage to Sept ----
## Data for currently active year and extended coverage to 1 Sept
kpi_1.3a <- theme2 |> 
  filter(kpi %in% c("KPI 1.3a Scotland SIMD", "KPI 1.3a Sept coverage"),
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, simd, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

# Extract extended coverage to 1 Sept
kpi_1.3a_sept <- kpi_1.3a |>
  select(hbres, simd, contains("_cohort"), contains("Sept coverage"))

kpi_1.3a_sept <- kpi_1.3a_sept[ , c(1, 2, 3, 6, 7, 4, 8, 9, 5, 10, 11)] 

# Remove extra historical coverage to 1 Sept data
kpi_1.3a <- kpi_1.3a[, -c(9:12)]

# 1.3a slope index of inequality
kpi_1.3a_slope <- kpi_1.3a %>% 
  select(hbres, simd, `2024/25_KPI 1.3a Scotland SIMD_coverage_p`, `2024/25_KPI 1.3a Scotland SIMD_cohort_n` ) %>% 
  filter(!simd %in% c("Unknown", "Total")) %>%
  rename(denominator = `2024/25_KPI 1.3a Scotland SIMD_cohort_n`, HB = hbres, SIMD = simd , percent_uptake = `2024/25_KPI 1.3a Scotland SIMD_coverage_p`)

kpi_1.3a_cum <- kpi_1.3a_slope %>%
  group_by(HB) %>%
  arrange(HB, SIMD) %>%
  mutate(total_denominator=sum(denominator),
         cum_denominator=cumsum(denominator),
         cum_denominator_prop=cum_denominator/total_denominator-(denominator/(2*total_denominator))) %>%
  ungroup()

# This next part performs a weighted linear regression for each HB of the percentage_uptake on the cumulative proportion in each SIMD,. The weights are the denominator in each SIMD
# The slope of the fitted line is the slope index of inequality

results_1.3a <- kpi_1.3a_cum %>%
  group_by(HB) %>%
  do({model <- lm(percent_uptake ~ cum_denominator_prop, 
                  data = .,
                  weights = denominator)
  tidy(model)
  }) %>%
  ungroup()

# extract the slopes of the fitted line - this is the slope index of inequality 

slopes_1.3a<- results_1.3a %>% 
  filter(term == "cum_denominator_prop") %>% 
  arrange(desc(HB == "Scotland"), HB) %>% 
  select(estimate) %>% 
  mutate(estimate = format(estimate, nsmall = 1))

slopes_1.3a_chart <- results_1.3a %>% 
  filter(term == "cum_denominator_prop") %>% 
  arrange(desc(HB == "Scotland"), HB) %>% 
  mutate(estimate = round(estimate, 1)) 

# Chart 1.3a inequality 
kpi_1.3a_chart <- kpi_1.3a %>% 
  select(hbres, simd, `2024/25_KPI 1.3a Scotland SIMD_coverage_p`) %>% 
  filter(!simd %in% c("Unknown")) %>%
  pivot_wider(
    names_from = simd,
    values_from = `2024/25_KPI 1.3a Scotland SIMD_coverage_p`
  ) %>% 
  mutate(`SIMD_5_minus_1` = `5 (least deprived)` - `1 (most deprived)`) %>% 
  mutate(`SIMD_5_minus_1` = round(`SIMD_5_minus_1`,1)) %>% 
  mutate(across(where(is.numeric),
                ~ ifelse(is.na(.x),
                         "-",  # replace NA with dash
                         format(round(.x, 1), nsmall = 1))))
         


# Pivot to long for plotting
kpi_long_1.3a <- kpi_1.3a_chart %>%
  pivot_longer(
    cols = c(`1 (most deprived)`, `2`, `3`, `4`, `5 (least deprived)`),
    names_to = "SIMD_quintile",
    values_to = "coverage"
  ) %>%
  mutate(SIMD_quintile = factor(SIMD_quintile,
                                levels = c("1 (most deprived)", "2", "3", "4", "5 (least deprived)")))


# Reorder hbres so that "Scotland" is last and ensure numeric
kpi_long_1.3a$hbres <- factor(kpi_long_1.3a$hbres,
                              levels = c(setdiff(unique(kpi_long_1.3a$hbres), "Scotland"), "Scotland"))

kpi_1.3a_chart$hbres <- factor(kpi_1.3a_chart$hbres,
                               levels = c(setdiff(unique(kpi_1.3a_chart$hbres), "Scotland"), "Scotland"))
slopes_1.3a_chart$HB <- factor(slopes_1.3a_chart$HB,
                               levels = c(setdiff(unique(slopes_1.3a_chart$HB), "Scotland"), "Scotland"))
kpi_long_1.3a$coverage <- as.numeric(trimws(kpi_long_1.3a$coverage))
kpi_1.3a_chart$SIMD_5_minus_1 <- as.numeric(trimws(kpi_1.3a_chart$SIMD_5_minus_1))

# Custom colors
simd_colors <- c(
  "1 (most deprived)" = "#12436D",  # Dark blue
  "2" = "#28A197",                  # Turquoise
  "3" = "#801650",                  # Pink
  "4" = "#F46A25",                  # Orange
  "5 (least deprived)" = "#3F085C"  # Dark purple
)

# Bar chart
p1a <- ggplot(kpi_long_1.3a, aes(x = hbres, y = coverage, , fill = SIMD_quintile)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  scale_fill_manual(values = simd_colors) +
  scale_x_discrete(
    expand = expansion(add = 0.5),
    labels = function(x) str_wrap(x, width = 10) )+
  scale_y_continuous(
    limits = c(0, NA),           
    expand = expansion(mult = c(0, 0.05))) +
  labs(title = "",
       x = "Health Board", y = "Percentage (%)", fill = "SIMD Quintile") +
  theme_minimal() +
  # Two black horizontal lines with different dash styles
  geom_hline(aes(yintercept = 75, linetype = "Essential"), color = "black", size = 1) +
  geom_hline(aes(yintercept = 85, linetype = "Desirable"), color = "black", size = 1) +
  
  # Custom legend for the lines
  scale_linetype_manual(
    name = "Thresholds",
    values = c("Essential" = "dashed", "Desirable" = "dotdash")
  )+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, color = "black"),
        axis.title.y = element_text(angle = 0, hjust = 0.5, size = 8, vjust = 0.5, color = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 


p2a <-  ggplot(kpi_1.3a_chart, aes(x = hbres, y = SIMD_5_minus_1)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), fill = "#3E8ECC", color = "black") +
  geom_text(
    aes(y = 0.1,  label = ifelse(is.na(SIMD_5_minus_1), "N/A", NA) ),
    vjust = -0.5, color = "black", size = 3, na.rm = FALSE)+
  scale_x_discrete(
    expand = expansion(add = 0.5),
    labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(
    limits = c(0, 25),           
    expand = expansion(mult = c(0, 0.05)) ) +
  labs(title = "",
       x = "Health Board", y = str_wrap("Percentage Point Difference", width = 10)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, color = "black"),
        axis.title.y = element_text(angle = 0, hjust = 0.5, size = 8, vjust = 0.5, color = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )

p3a <-  ggplot(slopes_1.3a_chart, aes(x = HB, y = estimate)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), fill = "#3E8ECC", color = "black") +
  scale_x_discrete(
    expand = expansion(add = 0.5),
    labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(
    limits = c(0, 25),           
    expand = expansion(mult = c(0, 0.05)) ) +
  labs(title = "",
       x = "Health Board", y = str_wrap("Slope index of inequality", width = 10)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, color = "black"),
        axis.title.y = element_text(angle = 0, hjust = 0.5, size = 8, vjust = 0.5, color = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )


ggsave("p1a.png", p1a, width = 14, height =6)
ggsave("p2a.png", p2a, width = 12, height =6)
ggsave("p3a.png", p3a, width = 12, height =6)
## KPI 1.3a year1 by HB SIMD ----
## Data for currently active year by HB SIMD
kpi_1.3a_hb <- theme2 |> 
  filter(kpi == "KPI 1.3a HB SIMD",
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, simd, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.3a year2 ----
kpi_1.3a_y2 <- eval_seasonal_diff(
  season,
  {## Data for currently active year only
    theme2 |> 
      filter(kpi %in% c("KPI 1.3a Scotland SIMD"),
             fin_year == year2,
             hbres ==  "Scotland") |> 
      mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |> 
      select(hbres, simd, FY_kpi_group, value) |>
      # match Excel output
      pivot_wider(names_from = FY_kpi_group, values_from = value)},
  {# Data for currently active year and extended coverage to 1 Sept
    theme2 |>
      filter(kpi %in% c("KPI 1.3a Scotland SIMD", "KPI 1.3a Sept coverage"),
             fin_year == year2,
             hbres ==  "Scotland") |>
      mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
      select(hbres, simd, FY_kpi_group, value) |>
      # match Excel output
      pivot_wider(names_from = FY_kpi_group, values_from = value)}
)

kpi_1.3a_y2 <- select(kpi_1.3a_y2, -c(hbres, simd)) # to match Excel table

## KPI 1.3b year1 by Scotland SIMD ----
## Data for currently active year by Scotland SIMD
kpi_1.3b <- theme2 |> 
  filter(kpi == "KPI 1.3b Scotland SIMD",
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, simd, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.3b year1 by HB SIMD ----
## Data for currently active year by HB SIMD
kpi_1.3b_hb <- theme2 |> 
  filter(kpi == "KPI 1.3b HB SIMD",
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, simd, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

# 1.3b slope index of inequality
kpi_1.3b_slope <- kpi_1.3b %>% 
  select(hbres, simd, `2024/25_KPI 1.3b Scotland SIMD_uptake_p`, `2024/25_KPI 1.3b Scotland SIMD_offer_n` ) %>% 
  filter(!simd %in% c("Unknown", "Total")) %>%
  rename(denominator = `2024/25_KPI 1.3b Scotland SIMD_offer_n`, HB = hbres, SIMD = simd , percent_uptake = `2024/25_KPI 1.3b Scotland SIMD_uptake_p`)

kpi_1.3b_cum <- kpi_1.3b_slope %>%
  group_by(HB) %>%
  arrange(HB, SIMD) %>%
  mutate(total_denominator=sum(denominator),
         cum_denominator=cumsum(denominator),
         cum_denominator_prop=cum_denominator/total_denominator-(denominator/(2*total_denominator))) %>%
  ungroup()

# This next part performs a weighted linear regression for each HB of the percentage_uptake on the cumulative proportion in each SIMD,. The weights are the denominator in each SIMD
# The slope of the fitted line is the slope index of inequality

results_1.3b <- kpi_1.3b_cum %>%
  group_by(HB) %>%
  do({model <- lm(percent_uptake ~ cum_denominator_prop, 
                  data = .,
                  weights = denominator)
  tidy(model)
  }) %>%
  ungroup()

# extract the slopes of the fitted line - this is the slope index of inequality 

slopes_1.3b<- results_1.3b%>% 
  filter(term == "cum_denominator_prop") %>% 
  arrange(desc(HB == "Scotland"), HB) %>% 
  select(estimate) %>% 
  mutate(estimate = format(estimate, nsmall = 1)) 

slopes_1.3b_chart <- results_1.3b %>% 
  filter(term == "cum_denominator_prop") %>% 
  arrange(desc(HB == "Scotland"), HB) %>% 
  mutate(estimate = round(estimate, 1))


#kpi 1.3b simd comparison data 
kpi_1.3b_chart <- kpi_1.3b %>% 
  select(hbres, simd, `2024/25_KPI 1.3b Scotland SIMD_uptake_p`) %>% 
  filter(!simd %in% c("Unknown")) %>%
  pivot_wider(
    names_from = simd,
    values_from = `2024/25_KPI 1.3b Scotland SIMD_uptake_p`
  ) %>% 
  mutate(`SIMD_5_minus_1` = `5 (least deprived)` - `1 (most deprived)`) %>% 
  mutate(`SIMD_5_minus_1` = round(`SIMD_5_minus_1`, 1)) %>% 
  mutate(across(where(is.numeric),
                ~ ifelse(is.na(.x),
                         "-",  # replace NA with dash
                         format(round(.x, 1), nsmall = 1))))
 
# Pivot to long for plotting
kpi_long_1.3b <- kpi_1.3b_chart %>%
  pivot_longer(
    cols = c(`1 (most deprived)`, `2`, `3`, `4`, `5 (least deprived)`),
    names_to = "SIMD_quintile",
    values_to = "coverage"
  ) %>%
  mutate(SIMD_quintile = factor(SIMD_quintile,
                                levels = c("1 (most deprived)", "2", "3", "4", "5 (least deprived)")))


# Reorder hbres so that "Scotland" is last and ensure numeric
kpi_long_1.3b$hbres <- factor(kpi_long_1.3b$hbres,
                              levels = c(setdiff(unique(kpi_long_1.3b$hbres), "Scotland"), "Scotland"))

kpi_1.3b_chart$hbres <- factor(kpi_1.3b_chart$hbres,
                                levels = c(setdiff(unique(kpi_1.3b_chart$hbres), "Scotland"), "Scotland"))
slopes_1.3b_chart$HB <- factor(slopes_1.3b_chart$HB,
                               levels = c(setdiff(unique(slopes_1.3b_chart$HB), "Scotland"), "Scotland"))
kpi_long_1.3b$coverage <- as.numeric(trimws(kpi_long_1.3b$coverage))
kpi_1.3b_chart$SIMD_5_minus_1 <- as.numeric(trimws(kpi_1.3b_chart$SIMD_5_minus_1))

# Custom colors
simd_colors <- c(
  "1 (most deprived)" = "#12436D",  # Dark blue
  "2" = "#28A197",                  # Turquoise
  "3" = "#801650",                  # Pink
  "4" = "#F46A25",                  # Orange
  "5 (least deprived)" = "#3F085C"  # Dark purple
)

# Bar chart
p1b <- ggplot(kpi_long_1.3b, aes(x = hbres, y = coverage, , fill = SIMD_quintile)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), color = "black") +
  scale_fill_manual(values = simd_colors) +
  scale_x_discrete(
    expand = expansion(add = 0.5),
    labels = function(x) str_wrap(x, width = 10)
  )+
  scale_y_continuous(
    limits = c(0, NA),           
    expand = expansion(mult = c(0, 0.05)) 
  ) +
  labs(title = "",
       x = "Health Board", y = "Percentage (%)", fill = "SIMD Quintile") +
  theme_minimal() +
  # Two black horizontal lines with different dash styles
  geom_hline(aes(yintercept = 75, linetype = "Essential"), color = "black", size = 1) +
  geom_hline(aes(yintercept = 85, linetype = "Desirable"), color = "black", size = 1) +
  
  # Custom legend for the lines
  scale_linetype_manual(
    name = "Thresholds",
    values = c("Essential" = "dashed", "Desirable" = "dotdash")
  )+
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, color = "black"),
        axis.title.y = element_text(angle = 0, hjust = 0.5, size = 8, vjust = 0.5, color = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) 
  

p2b <-  ggplot(kpi_1.3b_chart, aes(x = hbres, y = SIMD_5_minus_1)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), fill = "#3E8ECC", color = "black") +
  geom_text(
    aes(y = 0.1,  label = ifelse(is.na(SIMD_5_minus_1), "N/A", NA) ),
    vjust = -0.5, color = "black", size = 3, na.rm = FALSE)+
  scale_x_discrete(
    expand = expansion(add = 0.5),
    labels = function(x) str_wrap(x, width = 10)
  ) +
   scale_y_continuous(
    limits = c(0, 25),           
    expand = expansion(mult = c(0, 0.05)) 
  ) +
  labs(title = "",
       x = "Health Board", y = str_wrap("Percentage Point Difference", width = 10)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, color = "black"),
        axis.title.y = element_text(angle = 0, hjust = 0.5, size = 8, vjust = 0.5, color = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
        )

p3b <-  ggplot(slopes_1.3b_chart, aes(x = HB, y = estimate)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), fill = "#3E8ECC", color = "black") +
  scale_x_discrete(
    expand = expansion(add = 0.5),
    labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous(
    limits = c(0, 25),           
    expand = expansion(mult = c(0, 0.05)) ) +
  labs(title = "",
       x = "Health Board", y = str_wrap("Slope index of inequality", width = 10)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5, color = "black"),
        axis.title.y = element_text(angle = 0, hjust = 0.5, size = 8, vjust = 0.5, color = "black"),
        plot.title = element_text(hjust = 0.5),
        axis.line = element_line(color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
  )

ggsave("p1b.png", p1b, width = 14, height =6)
ggsave("p2b.png", p2b, width = 12, height =6)
ggsave("p3b.png", p3b, width = 12, height =6)
## KPI 1.4a ----
## Data for three most recent complete years (and extended coverage to 1 Sept)
kpi_1.4a <- theme2 |> 
  filter(kpi == "KPI 1.4a",
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value)

## KPI 1.4b ----
## Data for three most recent complete years (and extended coverage to 1 Sept)
kpi_1.4b <- theme2 |> 
  filter(kpi == "KPI 1.4b",
         fin_year %in% kpi_report_years) |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, group, sep = "_")) |>
  select(hbres, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value) |> 
  mutate(hbres = forcats::fct_relevel(hbres, hb_list)) |> 
  arrange(hbres)

## Table 6: Surveillance ----
## Data for three most recent complete years
t6_surveill <- theme2_t6 |> 
  mutate(FY_kpi_group = paste(fin_year, kpi, surveillance_interval, sep = "_")) |>
  select(hbres, FY_kpi_group, value) |>
  # match Excel output
  pivot_wider(names_from = FY_kpi_group, values_from = value) |> 
  mutate(hbres = forcats::fct_relevel(hbres, hb_list)) |> 
  arrange(hbres)

## DNA Exclusions ----
## Data for all years
dna_exclude <- theme2_dna |> 
  filter(fin_year %in% c(fy_list)) |> 
  # remove the last two numbers and / from the financial year
  mutate(year = str_remove(fin_year, "[:digit:][:digit:][:punct:]")) |>
  select(`Exclusion type` = pat_inelig, year, count) |>
  # match Excel output
  pivot_wider(names_from = year, values_from = count)


## Prisons ----

# 4: Write to Excel (openxlsx) ----
## Setup workbook ----
wb <- loadWorkbook(paste0(template_path, "/2_Invitation and Attendance_", season, "_rev.xlsx"))

source(here::here("code", "src", "Source_Excel_2.R"))
source(here::here("code", "src", "Source_Excel_functions.R"))


## Table of Contents ----
writeData(wb, sheet = "Table of Contents", pub_year, 
          startRow = 3)
addStyle(wb, "Table of Contents", styles$black_nowrap_12, 
         rows = 3, cols = 1:2, gridExpand = TRUE)
writeData(wb, sheet = "Table of Contents", qpmg_note, 
          startRow = 4)
addStyle(wb, "Table of Contents", styles$black_bold_12, 
         rows = 4, cols = 1)
writeData(wb, sheet = "Table of Contents", today, 
          startRow = 6)
addStyle(wb, "Table of Contents", styles$black_nowrap_12, 
         rows = 6, cols = 1)
writeData(wb, sheet = "Table of Contents", tab_1.1_add, 
          startRow = 12)
writeData(wb, sheet = "Table of Contents", tab_1.1_add_desc, 
          startRow = 12, startCol = 2)
writeData(wb, sheet = "Table of Contents", tab_1.2a_add, 
          startRow = line_no_tab_1.2a_add)
writeData(wb, sheet = "Table of Contents", tab_1.2_add_desc, 
          startRow = line_no_tab_1.2a_add, startCol = 2)
writeData(wb, sheet = "Table of Contents", tab_1.2b_add, 
          startRow = line_no_tab_1.2b_add)
writeData(wb, sheet = "Table of Contents", tab_1.2b_add_desc, 
          startRow = line_no_tab_1.2b_add, startCol = 2)
addStyle(wb, "Table of Contents", styles$black_border_12, 
         rows = 11:24, cols = 1, gridExpand = TRUE)
addStyle(wb, "Table of Contents", styles$blue_border_underline_12, 
         rows = 11:24, cols = 2, gridExpand = TRUE)

writeData(wb, sheet = "Table of Contents", note_toc,
          startRow = line_no_note_toc)
addStyle(wb, "Table of Contents", styles$red_bold_12, 
         rows = line_no_note_toc, cols = 1)

showGridLines(wb, "Table of Contents", showGridLines = FALSE)

# options("openxlsx.dateFormat" = "dd/mm/yyyy")

## KPI 1.1 ----
# notes
writeData(wb, sheet = "KPI 1.1", turn66_year_vv,
          startRow = 4, startCol = 2)
writeData(wb, sheet = "KPI 1.1", turn66_year_ww,
          startRow = 4, startCol = 5)
writeData(wb, sheet = "KPI 1.1", turn66_year_xx,
          startRow = 4, startCol = 8)
addStyle(wb, "KPI 1.1", styles$black_border_centre_12, 
         rows = 4, cols = 2:12, gridExpand = TRUE)
writeData(wb, sheet = "KPI 1.1", kpi_1.1_head_mgmt,
          startRow = 5, startCol = 11)
addStyle(wb, "KPI 1.1", styles$blue_border_centre_12, 
         rows = 5, cols = 11)
if (season == "spring") {
  writeData(wb, "KPI 1.1", kpi_1.1_notep, 
            startRow = 29)
  addStyle(wb, "KPI 1.1", styles$black_11, 
           rows = 29, cols = 1)
  }
writeData(wb, sheet = "KPI 1.1", kpi_1.1_note2, 
          startRow = 31)
addStyle(wb, "KPI 1.1", styles$black_11, 
         rows = 31, cols = 1)
showGridLines(wb, "KPI 1.1", showGridLines = FALSE)
# data
writeData(wb, sheet = "KPI 1.1", kpi_1.1, 
          startRow = 7, colNames = FALSE)

## KPI 1.1 Additional (20XX-YY) ----
# notes
writeData(wb, sheet =  "KPI 1.1 Additional (20XX-YY)", add_cohort_note, 
          startRow = 3)
addStyle(wb, "KPI 1.1 Additional (20XX-YY)", styles$black_bold_nowrap_14, 
         rows = 3, cols = 1)
writeData(wb, sheet =  "KPI 1.1 Additional (20XX-YY)", add_performance_note, 
          startRow = 4)
addStyle(wb, "KPI 1.1 Additional (20XX-YY)", styles$red_bold_12, 
         rows = 4, cols = 1)
writeData(wb, sheet = "KPI 1.1 Additional (20XX-YY)", turn66_year_yy, 
          startRow = 6, startCol = 2)
addStyle(wb, "KPI 1.1 Additional (20XX-YY)", styles$black_border_centre_12,
         rows = 6, cols = 2:4, gridExpand = TRUE)
if (season == "autumn") {
  writeData(wb, sheet =  "KPI 1.1 Additional (20XX-YY)", kpi_1.1_head_mgmt,
            startRow = 7, startCol = 5)
  addStyle(wb, "KPI 1.1 Additional (20XX-YY)", styles$blue_border_centre_12,
           rows = 7, cols = 5)
}
writeData(wb, sheet = "KPI 1.1 Additional (20XX-YY)", kpi_1.1_add_note1,
          startRow = 31)
addStyle(wb, "KPI 1.1 Additional (20XX-YY)", styles$orange_11,
         rows = 31, cols = 1)
showGridLines(wb, "KPI 1.1 Additional (20XX-YY)", showGridLines = FALSE)
# data
writeData(wb, sheet = "KPI 1.1 Additional (20XX-YY)", kpi_1.1_y2, 
          startRow = 9, colNames = FALSE)
names_pos <- which("KPI 1.1 Additional (20XX-YY)" == names(wb))[[1]] # finds index position for this name
names(wb)[[names_pos]] <- paste0("KPI 1.1 Additional (", year2, ")")


## KPI 1.2a ----
# notes
writeData(wb, sheet = "KPI 1.2a", turn66_year_vv, 
          startRow = 4, startCol = 2)
writeData(wb, sheet = "KPI 1.2a", turn66_year_ww, 
          startRow = 4, startCol = 5)
writeData(wb, sheet = "KPI 1.2a", turn66_year_xx, 
          startRow = 4, startCol = 8)
addStyle(wb, "KPI 1.2a", styles$black_border_centre_12,
         rows = 4, cols = 2:12, gridExpand = TRUE)
writeData(wb, sheet = "KPI 1.2a", kpi_1.2a_head_mgmt, 
          startRow = 5, startCol = 11)
addStyle(wb, "KPI 1.2a", styles$blue_border_centre_12, 
         rows = 5, cols = 11)
if(season == "spring") {
  writeData(wb, sheet = "KPI 1.2a", prov_data_note, 
            startRow = 30, colNames = FALSE)
  addStyle(wb, "KPI 1.2a", styles$black_11, 
           rows = 30, cols = 1)
}
# data
writeData(wb, sheet = "KPI 1.2a", kpi_1.2a, 
          startRow = 7, colNames = FALSE)
showGridLines(wb, "KPI 1.2a", showGridLines = FALSE)


## KPI 1.2a Coverage by 1 Sept ----
if (season == "autumn") {
  # notes
  writeData(wb, sheet = "Coverage by 1 Sept", turn66_year_vv,
            startRow = 4, startCol = 2)
  writeData(wb, sheet = "Coverage by 1 Sept", turn66_year_ww,
            startRow = 4, startCol = 5)
  writeData(wb, sheet = "Coverage by 1 Sept", turn66_year_xx,
            startRow = 4, startCol = 8)
  addStyle(wb, "Coverage by 1 Sept", styles$black_border_centre_12,
           rows = 4, cols = 2:10, gridExpand = TRUE)
  writeData(wb, sheet = "Coverage by 1 Sept", kpi_1.2a_head_mgmt,
            startRow = 5, startCol = 9)
  addStyle(wb, "Coverage by 1 Sept", styles$blue_border_centre_12,
           rows = 5, cols = 9, gridExpand = TRUE)
  writeData(wb, sheet = "Coverage by 1 Sept", sept_cov_note1,
            startRow = 24, startCol = 1)
  addStyle(wb, "Coverage by 1 Sept", styles$orange_11,
           rows = 24, cols = 1)
  # data
  writeData(wb, sheet = "Coverage by 1 Sept", kpi_1.2a_sept, 
            startRow = 7, colNames = FALSE)
  showGridLines(wb, "Coverage by 1 Sept", showGridLines = FALSE)
}


## KPI 1.2a Additional (20XX-YY) ----
# KPI 1.2a Additional
# notes
writeData(wb, sheet =  "KPI 1.2a Additional (20XX-YY)", add_cohort_note, 
          startRow = 3)
addStyle(wb, "KPI 1.2a Additional (20XX-YY)", styles$black_bold_nowrap_14, 
         rows = 3, cols = 1)
writeData(wb, sheet =  "KPI 1.2a Additional (20XX-YY)", add_performance_note, 
          startRow = 4)
addStyle(wb, "KPI 1.2a Additional (20XX-YY)", styles$red_bold_12, 
         rows = 4, cols = 1)
writeData(wb, sheet = "KPI 1.2a Additional (20XX-YY)", turn66_year_yy, 
          startRow = 6, startCol = 2)
eval_seasonal_diff(
  season,
  {addStyle(wb, "KPI 1.2a Additional (20XX-YY)", styles$black_border_thin_centre_12, 
            rows = 6, cols = 2:4, gridExpand = TRUE)}, # spring 
  {addStyle(wb, "KPI 1.2a Additional (20XX-YY)", styles$black_border_thin_centre_12, 
            rows = 6, cols = 2:6, gridExpand = TRUE)} # autumn
)

if(season == "autumn"){
  writeData(wb, sheet = "KPI 1.2a Additional (20XX-YY)", kpi_1.2a_head_mgmt, 
            startRow = 7, startCol = 5)
  addStyle(wb, "KPI 1.2a Additional (20XX-YY)", styles$blue_border_centre_12, 
           rows = 7, cols = 5)
}
writeData(wb, sheet =  "KPI 1.2a Additional (20XX-YY)", kpi_1.2a_add_note1, 
          startRow = 26)
addStyle(wb, "KPI 1.2a Additional (20XX-YY)", styles$orange_11,
         rows = 26, cols = 1)
writeData(wb, sheet =  "KPI 1.2a Additional (20XX-YY)", kpi_1.2a_add_note2, 
          startRow = 27)
addStyle(wb, "KPI 1.2a Additional (20XX-YY)", styles$black_11,
         rows = 27, cols = 1)
# data
writeData(wb, sheet = "KPI 1.2a Additional (20XX-YY)", kpi_1.2a_y2, 
          startRow = 9, colNames = FALSE)

# KPI 1.3a Additional
# notes
writeData(wb, sheet = "KPI 1.2a Additional (20XX-YY)", turn66_year_yy,
          startRow = 32, startCol = 2)
eval_seasonal_diff(
  season,
  {addStyle(wb, "KPI 1.2a Additional (20XX-YY)", styles$black_border_thin_centre_12,
            rows = 32, cols = 2:4, gridExpand = TRUE)}, # spring
  {addStyle(wb, "KPI 1.2a Additional (20XX-YY)", styles$black_border_thin_centre_12,
            rows = 32, cols = 2:6, gridExpand = TRUE)} # autumn
)
if (season == "autumn") {
  writeData(wb, sheet = "KPI 1.2a Additional (20XX-YY)", kpi_1.2a_head_mgmt,
            startRow = 33, startCol = 5)
  addStyle(wb, "KPI 1.2a Additional (20XX-YY)", styles$blue_border_centre_12,
           rows = 33, cols = 5)
}

# data
writeData(wb, sheet = "KPI 1.2a Additional (20XX-YY)", kpi_1.3a_y2, 
          startRow = 35, startCol = 2, colNames = FALSE)
showGridLines(wb, "KPI 1.2a Additional (20XX-YY)", showGridLines = FALSE)
names_pos <- which("KPI 1.2a Additional (20XX-YY)" == names(wb))[[1]] # finds index position for this name
names(wb)[[names_pos]] <- paste0("KPI 1.2a Additional (", year2, ")")

## KPI 1.2b ----
# notes
writeData(wb, sheet = "KPI 1.2b", turn66_year_vv, 
          startRow = 4, startCol = 2)
writeData(wb, sheet = "KPI 1.2b", turn66_year_ww, 
          startRow = 4, startCol = 5)
writeData(wb, sheet = "KPI 1.2b", turn66_year_xx, 
          startRow = 4, startCol = 8)
addStyle(wb, "KPI 1.2b", styles$black_border_centre_12,
         rows = 4, cols = 2:10, gridExpand = TRUE)
if(season == "spring") {
  writeData(wb, sheet = "KPI 1.2b", prov_data_note, 
            startRow = 30, colNames = FALSE)
  addStyle(wb, "KPI 1.2b", styles$black_11,
           rows = 30, cols = 1)
}
# data
writeData(wb, sheet = "KPI 1.2b", kpi_1.2b, 
          startRow = 7, colNames = FALSE)
showGridLines(wb, "KPI 1.2b", showGridLines = FALSE)

## KPI 1.2b Additional (20YY-YY) ----
# notes
writeData(wb, sheet =  "KPI 1.2b Additional (20XX-YY)", add_cohort_note, 
          startRow = 3)
addStyle(wb, "KPI 1.2b Additional (20XX-YY)", styles$black_bold_nowrap_14, 
         rows = 3, cols = 1)
writeData(wb, sheet =  "KPI 1.2b Additional (20XX-YY)", add_performance_note, 
          startRow = 4)
addStyle(wb, "KPI 1.2b Additional (20XX-YY)", styles$red_bold_12, 
         rows = 4, cols = 1)
writeData(wb, sheet = "KPI 1.2b Additional (20XX-YY)", turn66_year_yy, 
          startRow = 6, startCol = 2)
addStyle(wb, "KPI 1.2b Additional (20XX-YY)", styles$black_border_centre_12,
         rows = 6, cols = 2:4, gridExpand = TRUE)
writeData(wb, sheet = "KPI 1.2b Additional (20XX-YY)", kpi_1.2badd_foot, 
          startRow = 31)
addStyle(wb, "KPI 1.2b Additional (20XX-YY)", styles$orange_11, 
         rows = 31, cols = 1)
# data
showGridLines(wb, "KPI 1.2b Additional (20XX-YY)", showGridLines = FALSE)
writeData(wb, sheet = "KPI 1.2b Additional (20XX-YY)", kpi_1.2b_y2, 
          startRow = 9, colNames = FALSE)
names_pos <- which("KPI 1.2b Additional (20XX-YY)" == names(wb))[[1]] # finds index position for this name
names(wb)[[names_pos]] <- paste0("KPI 1.2b Additional (", year2, ")")

## KPI 1.3a ----
# notes
writeData(wb, sheet = "KPI 1.3a", turn66_year_vv, 
          startRow = 4, startCol = 3)
writeData(wb, sheet = "KPI 1.3a", turn66_year_ww, 
          startRow = 4,  startCol = 6)
writeData(wb, sheet = "KPI 1.3a", turn66_year_xx, 
          startRow = 4,  startCol = 9)
addStyle(wb, "KPI 1.3a", styles$black_border_centre_12, 
         rows = 4, cols = 3:13, gridExpand = TRUE)
writeData(wb, sheet = "KPI 1.3a", kpi_1.2a_head_mgmt, 
          startRow = 5, startCol = 12)
addStyle(wb, "KPI 1.3a", styles$blue_border_centre_12, 
         rows = 5, cols = 12)
if (season == "spring") {
  writeData(wb, sheet = "KPI 1.3a", prov_data_note, 
            startRow = 120)
  addStyle(wb, "KPI 1.3a", styles$black_11,
           rows = 120, cols = 1)
}
# data
writeData(wb, sheet = "KPI 1.3a", kpi_1.3a, 
          startRow = 7, colNames = FALSE)
showGridLines(wb, "KPI 1.3a", showGridLines = FALSE)

# autumn only
## KPI 1.3a Coverage by 1 Sept by SIMD ----

if (season == "autumn") {
  # notes
  writeData(wb, sheet = "Coverage by 1 Sept by SIMD", turn66_year_vv,
            startRow = 4, startCol = 3)
  writeData(wb, sheet = "Coverage by 1 Sept by SIMD", turn66_year_ww,
            startRow = 4, startCol = 6)
  writeData(wb, sheet = "Coverage by 1 Sept by SIMD", turn66_year_xx,
            startRow = 4, startCol = 9)
  addStyle(wb, "Coverage by 1 Sept by SIMD", styles$black_border_centre_12,
           rows = 4, cols = 3:11, gridExpand = TRUE)
  # data
  writeData(wb, sheet = "Coverage by 1 Sept by SIMD", kpi_1.3a_sept, 
            startRow = 7, colNames = FALSE)
  showGridLines(wb, "Coverage by 1 Sept by SIMD", showGridLines = FALSE)
}


## KPI 1.3a HB SIMD ----
# notes
writeData(wb, sheet = "KPI 1.3a HB SIMD", turn66_year_vv, 
          startRow = 5, startCol = 3)
writeData(wb, sheet = "KPI 1.3a HB SIMD", turn66_year_ww, 
          startRow = 5, startCol = 6)
writeData(wb, sheet = "KPI 1.3a HB SIMD", turn66_year_xx, 
          startRow = 5, startCol = 9)
addStyle(wb, "KPI 1.3a HB SIMD", styles$black_border_centre_12, 
         rows = 5, cols = 3:11, gridExpand = TRUE)
if (season == "spring") {
  writeData(wb, sheet = "KPI 1.3a HB SIMD", prov_data_note, 
            startRow = 114, colNames = FALSE)
  addStyle(wb, "KPI 1.3a HB SIMD", styles$black_11, 
           rows = 114, cols = 1)
}
# data
writeData(wb, sheet = "KPI 1.3a HB SIMD", kpi_1.3a_hb, 
          startRow = 8, colNames = FALSE)
showGridLines(wb, "KPI 1.3a HB SIMD", showGridLines = FALSE)


## KPI 1.3a inequality ----


#Titles
writeData(wb, sheet = "KPI 1.3a inequality", "KPI 1.3a Inequality differential: Percentage of eligible population who are tested before age 66 and 3 months by Scottish Index of Multiple Deprivation (SIMD) quintile", startCol = 1, startRow = 2)
addStyle(wb, sheet = "KPI 1.3a inequality", styles$black_bold_nowrap_18, cols = 1, rows = 2)
writeData(wb, sheet = "KPI 1.3a inequality", "Management Information", startCol = 1, startRow = 1)
addStyle(wb, sheet = "KPI 1.3a inequality", styles$red_bold_nowrap_1, cols = 1, rows = 1)
writeData(wb, sheet = "KPI 1.3a inequality", "Chart 1:  Percentage of eligible population who are tested before age 66 and 3 months by Scottish Index of Multiple Deprivation (SIMD) quintile", startCol = 1, startRow = 30)
addStyle(wb, sheet = "KPI 1.3a inequality", styles$black_bold_nowrap_14, cols = 1, rows = 30)
writeData(wb, sheet = "KPI 1.3a inequality", "Chart 2: Percentage of eligible population who are tested before age 66 and 3 months -  Difference between SIMD 5 and SIMD 1", startCol = 1, startRow = 63)
addStyle(wb, sheet = "KPI 1.3a inequality", styles$black_bold_nowrap_14, cols = 1, rows = 63)
writeData(wb, sheet = "KPI 1.3a inequality", "Chart 3: Percentage of eligible population who are tested before age 66 and 3 months -  Slope Index of inequality", startCol = 1, startRow = 96)
addStyle(wb, sheet = "KPI 1.3a inequality", styles$black_bold_nowrap_14, cols = 1, rows = 96)
writeData(wb, sheet = "KPI 1.3a inequality", "- Not applicable", startCol = 1, startRow =  24)
addStyle(wb, sheet = "KPI 1.3a inequality", styles$black_nowrap_11, cols = 1, rows = 24)

#plots
insertImage(wb, sheet = "KPI 1.3a inequality", file = "p1a.png", startRow = 32, startCol = 2, width = 14, height = 6)
insertImage(wb, sheet = "KPI 1.3a inequality", file = "p2a.png", startRow = 65, startCol = 2, width = 12, height = 6)
insertImage(wb, sheet = "KPI 1.3a inequality", file = "p3a.png", startRow = 98, startCol = 2, width = 12, height = 6)
#Table
writeData(wb, sheet = "KPI 1.3a inequality", "NHS Board of Residence", startCol = 1, startRow = 6)
writeData(wb, sheet = "KPI 1.3a inequality", "SIMD Quintile", startCol = 2, startRow = 5)
writeData(wb, sheet = "KPI 1.3a inequality", "Inequality Measures", startCol = 8, startRow = 5)
writeData(wb, sheet = "KPI 1.3a inequality", "Total", startCol = 2, startRow = 6)
writeData(wb, sheet = "KPI 1.3a inequality", "1 (Most Deprived)", startCol = 3, startRow = 6)
writeData(wb, sheet = "KPI 1.3a inequality", "2", startCol = 4, startRow = 6)
writeData(wb, sheet = "KPI 1.3a inequality", "3", startCol = 5, startRow = 6)
writeData(wb, sheet = "KPI 1.3a inequality", "4", startCol = 6, startRow = 6)
writeData(wb, sheet = "KPI 1.3a inequality", "5 (Least Deprived)", startCol = 7, startRow = 6)
writeData(wb, sheet = "KPI 1.3a inequality", "SIMD  Qunitile 5 - Quintile 1", startCol = 8, startRow = 6)
writeData(wb, sheet = "KPI 1.3a inequality", "Slope index of inequality", startCol = 9, startRow = 6)

# Style table
addStyle(wb, sheet = "KPI 1.3a inequality", styles$black_bold_nowrap_12, cols = 1:9, rows = 7, gridExpand = TRUE)
addStyle(wb, sheet = "KPI 1.3a inequality", styles$black_nowrap_12_center , cols = 1:9, rows = 8:21, gridExpand = TRUE)
mergeCells(wb, sheet = "KPI 1.3a inequality", cols = 2:7, rows = 5)
mergeCells(wb, sheet = "KPI 1.3a inequality", cols = 8:9, rows = 5)
addStyle(wb, sheet = "KPI 1.3a inequality", styles$black_border_centre_12 , cols = 1:9, rows = 5:6, gridExpand = TRUE)
setColWidths(wb, sheet = "KPI 1.3a inequality", cols = 1:9, widths = 20 )
addStyle(wb, sheet = "KPI 1.3a inequality", styles$black_12_center_leftright, cols = 8:9, rows = 8:21, gridExpand = TRUE)
addStyle(wb, sheet = "KPI 1.3a inequality", styles$b_top_bold, cols = 1:9, rows = 22, gridExpand = TRUE)
addStyle(wb, sheet = "KPI 1.3a inequality", styles$b_left_bold, cols = 10, rows = 7:21, gridExpand = TRUE)
addStyle(wb, sheet = "KPI 1.3a inequality", styles$black_12_center_left, cols = 2, rows = 8:21, gridExpand = TRUE)
addStyle(wb, sheet = "KPI 1.3a inequality", styles$black_12_center_left_bold, cols = 2, rows = 7)
addStyle(wb, sheet = "KPI 1.3a inequality", styles$black_12_center_leftright_bold, cols = 8:9, rows = 7)



#data
writeData(wb, sheet = "KPI 1.3a inequality", kpi_1.3a_chart, startCol = 1, startRow = 7,  colNames = FALSE, na.string = "-")
writeData(wb, sheet = "KPI 1.3a inequality", slopes_1.3a, startCol = 9, startRow = 7, colNames = FALSE, rowNames = FALSE, na.string = "-")
showGridLines(wb, sheet = "KPI 1.3a inequality", showGridLines = FALSE)

## KPI 1.3b inequality ----

#Titles
writeData(wb, sheet = "KPI 1.3b inequality", "KPI 1.3b inequality differential: (supplementary measure): Percentage of men offered screening before age 66 who are tested before age 66 and 3 months by Scottish Index of Multiple Deprivation (SIMD) quintile", startCol = 1, startRow = 2)
addStyle(wb, sheet = "KPI 1.3b inequality", styles$black_bold_nowrap_18, cols = 1, rows = 2)
writeData(wb, sheet = "KPI 1.3b inequality", "Management Information", startCol = 1, startRow = 1)
addStyle(wb, sheet = "KPI 1.3b inequality", styles$red_bold_nowrap_1, cols = 1, rows = 1)
writeData(wb, sheet = "KPI 1.3b inequality", "Chart 1:  Percentage of men offered screening before age 66 who are tested before age 66 and 3 months by Scottish Index of Multiple Deprivation (SIMD) quintile", startCol = 1, startRow = 30)
addStyle(wb, sheet = "KPI 1.3b inequality", styles$black_bold_nowrap_14, cols = 1, rows = 30)
writeData(wb, sheet = "KPI 1.3b inequality", "Chart 2: Percentage of men offered screening before age 66 who are tested before age 66 and 3 months- Difference between SIMD 5-1", startCol = 1, startRow = 63)
addStyle(wb, sheet = "KPI 1.3b inequality", styles$black_bold_nowrap_14, cols = 1, rows = 63)
writeData(wb, sheet = "KPI 1.3b inequality", "Chart 3: Percentage of men offered screening before age 66 who are tested before age 66 and 3 months - Slope Index of inequality", startCol = 1, startRow = 96)
addStyle(wb, sheet = "KPI 1.3b inequality", styles$black_bold_nowrap_14, cols = 1, rows = 96)
writeData(wb, sheet = "KPI 1.3b inequality", "- Not applicable", startCol = 1, startRow =  24)
addStyle(wb, sheet = "KPI 1.3b inequality", styles$black_nowrap_11, cols = 1, rows = 24)

#plots
insertImage(wb, sheet = "KPI 1.3b inequality", file = "p1b.png", startRow = 32, startCol = 2, width = 14, height = 6)
insertImage(wb, sheet = "KPI 1.3b inequality", file = "p2b.png", startRow = 65, startCol = 2, width = 12, height = 6)
insertImage(wb, sheet = "KPI 1.3b inequality", file = "p3b.png", startRow = 98, startCol = 2, width = 12, height = 6)
#Table
writeData(wb, sheet = "KPI 1.3b inequality", "NHS Board of Residence", startCol = 1, startRow = 6)
writeData(wb, sheet = "KPI 1.3b inequality", "SIMD Quintile", startCol = 2, startRow = 5)
writeData(wb, sheet = "KPI 1.3b inequality", "Inequality Measures", startCol = 7, startRow = 5)
writeData(wb, sheet = "KPI 1.3b inequality", "Total", startCol = 2, startRow = 6)
writeData(wb, sheet = "KPI 1.3b inequality", "1 (Most Deprived)", startCol = 3, startRow = 6)
writeData(wb, sheet = "KPI 1.3b inequality", "2", startCol = 4, startRow = 6)
writeData(wb, sheet = "KPI 1.3b inequality", "3", startCol = 5, startRow = 6)
writeData(wb, sheet = "KPI 1.3b inequality", "4", startCol = 6, startRow = 6)
writeData(wb, sheet = "KPI 1.3b inequality", "5 (Least Deprived)", startCol = 7, startRow = 6)
writeData(wb, sheet = "KPI 1.3b inequality", "SIMD  Qunitile 5 - Quintile 1", startCol = 8, startRow = 6)
writeData(wb, sheet = "KPI 1.3b inequality", "Slope index of inequality", startCol = 9, startRow = 6)

# Style table
addStyle(wb, sheet = "KPI 1.3b inequality", styles$black_bold_nowrap_12, cols = 1:9, rows = 7, gridExpand = TRUE)
addStyle(wb, sheet = "KPI 1.3b inequality", styles$black_nowrap_12_center , cols = 1:9, rows = 8:21, gridExpand = TRUE)
mergeCells(wb, sheet = "KPI 1.3b inequality", cols = 2:7, rows = 5)
mergeCells(wb, sheet = "KPI 1.3b inequality", cols = 8:9, rows = 5)
addStyle(wb, sheet = "KPI 1.3b inequality", styles$black_border_centre_12 , cols = 1:9, rows = 5:6, gridExpand = TRUE)
setColWidths(wb, sheet = "KPI 1.3b inequality", cols = 1:9, widths = 20 )
addStyle(wb, sheet = "KPI 1.3b inequality", styles$black_12_center_leftright, cols = 8:9, rows = 8:21, gridExpand = TRUE)
addStyle(wb, sheet = "KPI 1.3b inequality", styles$b_top_bold, cols = 1:9, rows = 22, gridExpand = TRUE)
addStyle(wb, sheet = "KPI 1.3b inequality", styles$b_left_bold, cols = 10, rows = 7:21, gridExpand = TRUE)
addStyle(wb, sheet = "KPI 1.3b inequality", styles$black_12_center_left, cols = 2, rows = 8:21, gridExpand = TRUE)
addStyle(wb, sheet = "KPI 1.3b inequality", styles$black_12_center_left_bold, cols = 2, rows = 7)
addStyle(wb, sheet = "KPI 1.3b inequality", styles$black_12_center_leftright_bold, cols = 8:9, rows = 7)

#data
writeData(wb, sheet = "KPI 1.3b inequality", kpi_1.3b_chart, startCol = 1, startRow = 7,  colNames = FALSE, na.string = "-")
writeData(wb, sheet = "KPI 1.3b inequality", slopes_1.3b, startCol = 9, startRow = 7, colNames = FALSE, rowNames = FALSE, na.string = "-")
showGridLines(wb, sheet = "KPI 1.3b inequality", showGridLines = FALSE)

## KPI 1.3b ----
writeData(wb, sheet = "KPI 1.3b", turn66_year_vv, 
          startRow = 4, startCol = 3)
writeData(wb, sheet = "KPI 1.3b", turn66_year_ww, 
          startRow = 4, startCol = 6)
writeData(wb, sheet = "KPI 1.3b", turn66_year_xx, 
          startRow = 4, startCol = 9)
addStyle(wb, "KPI 1.3b", styles$black_border_centre_12,
         rows = 4, cols = 3:11, gridExpand = TRUE)
if (season == "spring") {
  writeData(wb, sheet = "KPI 1.3b", prov_data_note, 
            startRow = 120, colNames = FALSE)
  addStyle(wb, "KPI 1.3b", styles$black_11,
           rows = 120, cols = 1)
}
# data
writeData(wb, sheet = "KPI 1.3b", kpi_1.3b, 
          startRow = 7, colNames = FALSE)
showGridLines(wb, "KPI 1.3b", showGridLines = FALSE)


## KPI 1.3b HB SIMD ----
if (season == "autumn") {
  # notes
  writeData(wb, sheet = "KPI 1.3b HB SIMD", turn66_year_vv, 
            startRow = 5, startCol = 3)
  writeData(wb, sheet = "KPI 1.3b HB SIMD", turn66_year_ww, 
            startRow = 5, startCol = 6)
  writeData(wb, sheet = "KPI 1.3b HB SIMD", turn66_year_xx, 
            startRow = 5, startCol = 9)
  addStyle(wb, "KPI 1.3b HB SIMD", styles$black_border_centre_12, 
           rows = 5, cols = 3:11, gridExpand = TRUE)
  # data
  writeData(wb, sheet = "KPI 1.3b HB SIMD", kpi_1.3b_hb, 
            startRow = 8, colNames = FALSE)
  showGridLines(wb, "KPI 1.3b HB SIMD", showGridLines = FALSE)
}


## KPI 1.4a ----
# notes
writeData(wb, sheet = "KPI 1.4a", kpi_1.4a_head1, 
          startRow = 4, startCol = 2)
writeData(wb, sheet = "KPI 1.4a", kpi_1.4a_head2, 
          startRow = 4, startCol = 5)
writeData(wb, sheet = "KPI 1.4a", kpi_1.4a_head3, 
          startRow = 4, startCol = 8)
addStyle(wb, "KPI 1.4a", styles$black_border_centre_12,
         rows = 4, cols = 2:10, gridExpand = TRUE)
if (season == "spring") {
  writeData(wb, sheet = "KPI 1.4a", prov_data_note, 
            startRow = 29, colNames = FALSE)
  writeData(wb, sheet = "KPI 1.4a", kpi_1.4a_note1, 
            startRow = 30, colNames = FALSE)
  addStyle(wb, "KPI 1.4a", styles$black_11,
           rows = 29:30, cols = 1, gridExpand = TRUE)
}
# data
writeData(wb, sheet = "KPI 1.4a", kpi_1.4a, 
          startRow = 7, colNames = FALSE)
showGridLines(wb, "KPI 1.4a", showGridLines = FALSE)


## KPI 1.4b ----
# notes
writeData(wb, sheet = "KPI 1.4b", kpi_1.4b_head1, 
          startRow = 4, startCol = 2)
writeData(wb, sheet = "KPI 1.4b", kpi_1.4b_head2, 
          startRow = 4, startCol = 5)
writeData(wb, sheet = "KPI 1.4b", kpi_1.4b_head3, 
          startRow = 4, startCol = 8)
addStyle(wb, "KPI 1.4b", styles$black_border_centre_12,
         rows = 4, cols = 2:10, gridExpand = TRUE)
if (season == "spring") {
  writeData(wb, sheet = "KPI 1.4b", prov_data_note, 
            startRow = 29, colNames = FALSE)
  writeData(wb, sheet = "KPI 1.4b", kpi_1.4a_note1, 
            startRow = 30, colNames = FALSE)
  addStyle(wb, "KPI 1.4b", styles$black_11,
           rows = 29:30, cols = 1, gridExpand = TRUE)
}
# data
writeData(wb, sheet = "KPI 1.4b", kpi_1.4b, 
          startRow = 7, colNames = FALSE)
showGridLines(wb, "KPI 1.4b", showGridLines = FALSE)


## Table 6: Surveillance----
# notes
writeData(wb, sheet = "6) Surveillance", table6_head1, 
          startRow = 6, startCol = 2)
writeData(wb, sheet = "6) Surveillance", table6_head2, 
          startRow = 6, startCol = 4)
writeData(wb, sheet = "6) Surveillance", table6_head3, 
          startRow = 6, startCol = 6)
addStyle(wb, "6) Surveillance", styles$black_border_centre_12,
         rows = 6, cols = 2:7, gridExpand = TRUE)

if (season == "spring") {
  writeData(wb, sheet = "6) Surveillance", prov_data_note, 
            startRow = 25, colNames = FALSE)
  addStyle(wb, "6) Surveillance", styles$black_11,
           rows = 25, cols = 1)
}
# data
writeData(wb, sheet = "6) Surveillance", t6_surveill,
          startRow = 8, colNames = FALSE)
showGridLines(wb, "6) Surveillance", showGridLines = FALSE)


## DNA Exclusions ----

# PM march 2025 - this is giving an error

write_dna_exclusions(wb, "DNA Exclusions", season,
                     data = dna_exclude, provisional_note = dna_note1)

## Prisons ----
# if (season == "autumn") {
   
 # writeData(wb, sheet = "KPI 1.2a 1.2b Prisons", kpi_1.2a_prisons,
  #           startRow = 7, colNames = FALSE)
   #writeData(wb, sheet = "KPI 1.2a 1.2b Prisons", kpi_1.2b_prisons,
    #         startRow = 16, colNames = FALSE)
 #}

 
# 5: Save output ----
query_saveWorkbook(wb, paste0(output_path, "/2_Invitation and Attendance_",
                                      yymm, ".xlsx"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 93_Source_Excel_2.R
# 
# Aoife McCarthy
# Apr 2024
# 
# Set up notes for Theme 2 Excel workbook
# 
# Written/run on Posit WB, R 4.1.2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Functions

# this has been included in phsaaa - migrate over after this PR
eval_seasonal_diff <- function(expr_spring, expr_autumn) {
  if (season == "spring") {
    eval(substitute(expr_spring), envir = .GlobalEnv)
  } else if (season == "autumn") {
    eval(substitute(expr_autumn), envir = .GlobalEnv)
  } else {
    stop("Go check your calendar!")
  }
}

## Notes:
# This script automates the titles and notes for each tab of the theme 2 Excel
# workbook for each (spring/autumn) QPMG.

## Styles ----

# bold black fonr used in some headers
bold_black_14 <- createStyle(fontSize = 14, fontColour = "#000000",
                             fontName = "Arial", textDecoration = "bold")
# orange style for notes needing manual contribution
orange_11 <- createStyle(fontSize = 11, fontName = "Arial", 
                           fontColour = "#ff9f00", wrapText = TRUE)
# red performance font 
bold_red_12 <- createStyle(fontSize = 12, fontColour = "#FF0000", 
                           fontName = "Arial", textDecoration = c("bold"),
                           wrapText = TRUE)
# bright blue font
blue_12 <- createStyle(fontSize = 12, fontName = "Arial", 
                       fontColour = "#0000FF", wrapText = TRUE)

## General notes ----
# current reporting year - used for additional management info
year2 <- gsub("/", "-", year2)

# turned 66 in year... titles for KPI tables
turn66_year_vv <- paste0("Turned 66 in year ending 31 March ", year_vv, '\n',
                         "(became eligible in year ending 31 March ", year_uu, ")")
turn66_year_ww <- paste0("Turned 66 in year ending 31 March ", year_ww, '\n',
                         "(became eligible in year ending 31 March ", year_vv, ")")
turn66_year_xx <- paste0("Turned 66 in year ending 31 March ", year_xx, '\n',
                         "(became eligible in year ending 31 March ", year_ww, ")")
turn66_year_yy <- paste0("Turned 66 in year ending 31 March ", year_yy, '\n',
                         "(became eligible in year ending 31 March ", year_xx, ")")

# additional cohort sheets - eligibility dates
add_cohort_note <- paste0("Data for latest annual cohort eligible for screening ",
                          "(i.e., men reaching age 66 in ", year2, ").")
# additional cohort sheets - performance
add_performance_note <- paste0("Performance against the KPI thresholds for this ",
                               "cohort cannot be fully assessed at this stage. ",
                               "Data are shown to demonstrate the work-in-progress ",
                               "position at ", extract_date, " ", year_xx, 
                               ". The KPI data for this cohort will be finalised ",
                               "from the PHS data extract at 1 September ",
                               year_yy, ".")

prov_data_note <- paste0("1. Data for year ending 31 March ", year_xx,
                         " are provisional: some men in this cohort had not ",
                         "reached age 66 and 3 months by the date of the PHS ",
                         "extract on ", extract_date, " ", year_xx, " (the ",
                         "youngest men in this cohort will reach age 66 and 3 ",
                         "months on 30 June ", year_xx, "). In addition, a few ",
                         "men in the eligible age range may move in or out of ",
                         "Scotland, which may result in small changes to the ",
                         "cohort of men offered screening before age 66 and the ",
                         "uptake rate. Data will be finalised from the PHS data ",
                         "extract at 1 September ", year_xx, ".")


## Table of Contents notes ----

### sheet headings ----
pub_year <- eval_seasonal_diff(
  {paste0("Data for year ending 31 March ", year_xx, " scheduled to ",
          "be published in April ", year_yy, " (final data will be ",
          "produced from data extracted for PHS in September ",
          year_xx, ").")}, #spring
  {paste0("KPI data for year ending 31 March ", year_xx, 
          " and some supplementary information are planned ",
          "for publication in March ", year_yy, ".")} # autumn
)
qpmg_note <- paste0("For review at QPMG in ", qpmg_month, " ", year_xx)
today <- paste0("Workbook created ", Sys.Date())

### TOC contents ----
tab_1.1_add <- paste0("1.1 Additional (", year2, ")")
tab_1.1_add_desc <- paste0("Percentage of eligible population who are sent an ",
                           "initial offer to screening before age 66: work-in-progress ",
                           "position for men reaching age 66 in year ending 31 March ", 
                           year_yy)
line_no_tab_1.2a_add <- eval_seasonal_diff({15}, {16})
tab_1.2a_add <- paste0("1.2a Additional (", year2, ")")
tab_1.2_add_desc <- paste0("Percentage of eligible population who are tested ",
                           "before age 66 and 3 months: work-in-progress ",
                           "position for men reaching age 66 in year ending ",
                           "31 March ", year_yy)
line_no_tab_1.2b_add <- eval_seasonal_diff({17}, {18})
tab_1.2b_add <- paste0("1.2b (uptake) Additional (", year2, ")")
tab_1.2b_add_desc <- paste0("Percentage of men offered screening before age 66 ",
                            "who are tested before age 66 and 3 months: ",
                            "work-in-progress position for men reaching age 66 ",
                            "in year ending 31 March ", year_yy)

### footnotes ----
line_no_note_toc <- eval_seasonal_diff({26},{30})
note_toc_data <- eval_seasonal_diff(
  {"The provisional/partial "}, # spring version,
  {"The "} # autumn version
)
note_toc <- paste0(note_toc_data, "data for the year ending 31 March ", 
            year_xx, " are released for data quality assurance and ",
            "management information purposes and should not be placed in ",
            "the public domain. The information can be shared locally with ",
             "those who have a legitimate need to review the data for ",
             "quality assurance, managerial or operational purposes.")
rm(note_toc_data)

## KPI 1.1 notes ----
### headers ----
kpi_1.1_head_mgmt <- paste0("Offered screening before ", extract_date, " ", 
                            year_xx, "includes men offered screening after",
                            "66th birthday)")

### footnotes ----

kpi_1.1_notep <- paste0("1. Data for year ending 31 March ", year_xx, " are ",
                        "provisional: data will be finalised from the PHS data ",
                        "extract at 1 September ", year_xx, ". Additionally, a ",
                        "few men in the eligible age range may move in or out ",
                        "of Scotland, which may result in small changes to the ",
                        "number of men in the cohort and invite rate.")

# note 3 calculations 
# calculate number of men not invited for screening before 66
cohort_n <- kpi_1.1 %>% 
  filter(hbres=="Scotland") %>% 
  select(contains(kpi_report_years[3]) & contains("cohort_n")) %>% 
  pull()

invited_before_66 <- kpi_1.1 %>% 
  filter(hbres=="Scotland") %>% 
  select(contains(kpi_report_years[3]) & contains("KPI 1.1_offer_n")) %>% 
  pull()

kpi_1.1_no_invite_before_66 <- cohort_n - invited_before_66

# calculated number of men invited after age of 66
invited_any_age <- kpi_1.1 %>% 
  filter(hbres=="Scotland") %>% 
  select(contains(kpi_report_years[3]) & contains("Sept coverage_offer_n")) %>% 
  pull()

kpi_1.1_invited_after_66 <- invited_any_age - invited_before_66

# calculated number of men not invited despite eligible
kpi_1.1_not_invited <- cohort_n - invited_any_age

rm(cohort_n, invited_before_66, invited_any_age) # tidy environment

# final note:
kpi_1.1_note2 <- paste0("2. Additional management information: the data for the ",
                        "year ending 31 March ", year_xx, " shows there were ",
                        kpi_1.1_no_invite_before_66, " men in the latest cohort ",
                        "who were not invited for screening before age 66. ",
                        "Of these, ", kpi_1.1_invited_after_66, " were invited ",
                        "after their 66th birthday and the remaining ", 
                        kpi_1.1_not_invited, " had not been invited at ",
                        extract_date, " ", year_xx, " (date of PHS data extract).")

rm(kpi_1.1_no_invite_before_66, kpi_1.1_invited_after_66, kpi_1.1_not_invited) # tidy

## KPI 1.1 Additional (20XX-YY) Notes ----

### footnotes ----
kpi_1.1_add_note1 <- paste0("1. For the previous eligible cohorts at this stage, ",
                            "the equivalent percentages of men offered screening ",
                            "before age 66 were {x}% (", year_vv, "/", 
                            substr(year_ww, 3,4), ") and {x}% (", year_ww, "/",
                            substr(year_xx, 3,4), ").")

## KPI 1.1 SIMD Notes ----
# none

## KPI 1.2a Notes ----
### table headers ----
kpi_1.2a_head1 <- eval_seasonal_diff(
  {paste0("Tested before 1 March ", year_xx, '\n',  " (includes men ",
          "tested after age 66", '\n', " and 3 months)")}, # spring
  {kpi_1.2a_head1 <- paste0("Tested before 1 September ", year_xx, '\n',  " (includes men ",
                            "tested after age 66", '\n', " and 3 months)") } # autumn
)

# autumn only
## KPI 1.2a Coverage by 1 Sept notes ----
if (season == "autumn"){
    sept_cov_note1 <- paste("1. The cohorts are based on men eligible for screening at ",
                            "each of the reporting points; data for the year ending 31 March ",
                            year_vv, " were extracted on {x} September ", year_vv, 
                            ", data for the year ending 31 March ", year_ww, 
                            " were extracted on {x} September ", year_ww, 
                            " and data for the year ending 31 March ", year_xx, 
                            " were extracted on ", extract_date, " ", year_xx, ".")
  }

## KPI 1.2a Additional (20XX-YY) Notes ----
### footnotes ----
kpi_1.2a_add_note2 <- paste0("2. Some men in this cohort have not reached age ",
                             "66 and 3 months yet. The oldest men in the cohort ",
                             "will reach this age on 1 July ", year_xx, 
                             " and the youngest men in the cohort will reach ",
                             "this age on 30 June ", year_yy, ".")

## KPI 1.3a Additional (20XX-YY) Notes ----
### footnotes ----

kpi_1.3a_add_note1 <- paste0("1. For the previous eligible cohort at this stage, ",
                             "the equivalent percentage of men tested before age ",
                             "66 and 3 months was {x}% (", year_ww, "/", 
                             substr(year_xx, 3, 4), ").")

## KPI 1.2b Notes ----
# None


## KPI 1.2b Additional (20YY-YY) Notes ----
### footnotes ----
kpi_1.2badd_foot <- paste0("1. The equivalent figure for the previous eligible ",
                           "cohort in Scotland at this stage was {x}% (", 
                           year_ww, "/", substr(year_xx, 3, 4), ").")

## KPI 1.3a Notes ----
# none


# autumn only
## KPI 1.3a Coverage by 1 Sept by SIMD Notes ----
# none


## KPI 1.3a HB SIMD Notes ----
# none

## KPI 1.3b Notes ----
# none


## KPI 1.4a Notes ----
### table headers ----
kpi_1.4a_head1 <- paste0("Due to attend annual surveillance in year ending 31",
                         '\n', "March ", year_vv)
kpi_1.4a_head2 <- paste0("Due to attend annual surveillance in year ending 31",
                         '\n', "March ", year_ww)
kpi_1.4a_head3 <- paste0("Due to attend annual surveillance from 1 April ",
                         year_ww, " -", '\n', "31 January ", year_xx, '\n',
                         "(partial data for financial year)")

### footnotes ----
kpi_1.4a_note1 <- paste0("1. Due to attend surveillance 1 April ", year_ww, 
                         " to 31 January ", year_xx, ": provisional rates are ",
                         "presented for the 11-month period 1 March ", year_ww,
                         " to 31 January ", year_xx, " as data are not yet ",
                         "available for the full financial year ending 31 March ",
                         year_xx, " from the PHS extract at ", extract_date, " ",
                         year_xx, ". Data for the complete financial year ending ",
                         "31 March ", year_xx, " will be produced from the PHS ",
                         "data extract at 1 September ", year_xx, ".")

## KPI 1.4b Notes ----
### table headers ----
kpi_1.4b_head1 <- paste0("Due to attend quarterly surveillance in year ending 31",
                         '\n', "March ", year_vv)
kpi_1.4b_head2 <- paste0("Due to attend quarterly surveillance in year ending 31",
                         '\n', "March ", year_ww)
kpi_1.4b_head3 <- paste0("Due to attend quarterly surveillance from 1 April ",
                         year_ww, " -", '\n', "31 January ", year_xx, '\n',
                         "(partial data for financial year)")

## Table 6: Surveillance Notes ----
### table headers ----
table6_head1 <- paste0("Screened in year ending 31 March",  '\n',
                       year_vv)
table6_head2 <- paste0("Screened in year ending 31 March",  '\n',
                       year_ww)
table6_head3 <- paste0("Screened from 1 April ", year_ww, " - 28",
                       '\n', "February ", year_xx)

## DNA Exclusions Notes ----
### footnotes---- 
dna_note1 <- paste0("1. Data for year ending 31 March ", year_xx, " are ",
                    "provisional; data will be finalised from the PHS data ",
                    "extract at 1 September ", year_xx, ".")

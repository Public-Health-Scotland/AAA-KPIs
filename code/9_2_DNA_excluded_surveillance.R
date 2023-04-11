#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 01_DNA_excluded_surveillance.R
# Eibhlin O'Sullivan
# Jan 2022
# Takes processed extract and creates summary of patients
# Written/run on R Studio Server
# R version 3.6.1
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

### 1 - Housekeeping ----

## Source housekeeping file
source(here::here("Syntax", "00_housekeeping.R"))

### 2 - Read in Files ----

# AAA exclusions path
aaa_exclusions <- readRDS(paste0(aaa_extracts_path,"aaa_exclusions_202209.rds"))


# filter if man has been excluded from surveillance due to dna ('08') or has opted out ('02').
# include open exlcusions only i.e. exclusions with no end date.
# create variable for financial year based on date_start
dna_excluded_surveillance <- aaa_exclusions %>% 
  filter(pat_inelig %in% c("08","02") &
           is.na(date_end)) %>% 
  mutate(financial_year = extract_fin_year(as.Date(date_start)))
# 85 rows

# summarise by pat_inelig and financial_year
# reformat the table
# rename row names
dna_excluded_surveillance_table <- dna_excluded_surveillance %>%
  group_by(pat_inelig,financial_year) %>% 
  summarise(count = n()) %>% 
  arrange(financial_year) %>% 
  pivot_wider(names_from = financial_year,
              values_from = count) %>% 
  mutate(pat_inelig = case_when(pat_inelig == "08" ~ "Non Responder Surveillance",
                                pat_inelig == "02" ~ "Opted Out Surveillance")) %>% 
  rename(`Exclusion Type` = pat_inelig)

### 3 - Write to Excel ----

# Create workbook

wb <- createWorkbook()

# Define a header style for workbook

hs <- createStyle(halign = "center", valign = "center", 
                  textDecoration = "bold", border = "TopBottomLeftRight")

## 3.1 - DNA Figures Tab --

addWorksheet(wb, sheetName = "DNA Exclusions", gridLines = FALSE)

# Add Titles
writeData(wb, sheet = "DNA Exclusions", paste0("Management Information"),
          startCol = 1, startRow = 1)

writeData(wb, sheet = "DNA Exclusions", paste0("Number of men excluded from AAA screening surveillance due to opting out or non-response to invitations"),
          startCol = 1, startRow = 2)

writeData(wb, sheet = "DNA Exclusions", dna_excluded_surveillance_table, borders = "all", headerStyle = hs, startCol = 1, startRow = 4)

setColWidths(wb, sheet = "DNA Exclusions", cols = 1:16, widths = "auto")

## 3.2 - Save Workbook --

saveWorkbook(wb, file = here("temp","theme_2.xlsx"),overwrite = TRUE)






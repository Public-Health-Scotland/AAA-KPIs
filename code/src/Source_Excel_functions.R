# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Source_Excel_functions.R
# 
# Aoife McCarthy
# Oct 2024
# 
# Functions to add openxlsx-created sheets to workbook
# 
# Written/run on Posit WB, R 4.1.2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


library(openxlsx)
library(common)
  
# Write DNA Exclusions sheet of theme 2 Workbook (main table + notes, header in template still)
write_dna_exclusions <- function(workbook, sheet_name, season_var, data, provisional_note) {
  


# notes and styles --------------------------------------------------------
  ## text for writing in
  text <- list()
  text$header <- "Excluded in year ending 31 March"
  text$subheader <- "Exclusion type"
  text$source <- "Source: Scottish AAA Call Recall System"
  if(season_var == "spring"){
    text$final_header <- paste0(colnames(data)[max(ncol(data))], " (provisional data)", {supsc("p")})
  }
  
  # source styles
  source(here::here("code", "src", "Source_Excel_Styles.R"))
  
# formatting cells --------------------------------------------------------

  # merging
  mergeCells(workbook, sheet_name, rows = 4, cols = 2:(ncol(data))) # top header
  mergeCells(workbook, sheet_name, rows = 4:5, cols = 1) # subheader
  mergeCells(workbook, sheet_name, rows = 8, cols = (ncol(data) - 3):ncol(data)) # source
  mergeCells(workbook, sheet_name, rows = 10, cols = 1:(ncol(data))) # provisional note (spring only)
  
  # col/row heights
  setColWidths(workbook, sheet_name, cols = 1, widths = 30)
  setColWidths(workbook, sheet_name, cols = 2:ncol(data), widths = 12)
  setRowHeights(workbook, sheet_name, rows = c(4, 6, 7), heights = 22.5)
  setRowHeights(workbook, sheet_name, rows = 5, heights = 50)
  setRowHeights(workbook, sheet_name, rows = 8:15, heights = 15.5)
  
  # gridlines
  showGridLines(workbook, sheet_name, showGridLines = F)

# writing data ------------------------------------------------------------

  writeData(workbook, sheet_name, text$header,
            startRow = 4, startCol = 2) # header
  writeData(workbook, sheet_name, data,
            startRow = 5, startCol = 1) # data
  writeData(workbook, sheet_name, text$subheader,
            startRow = 4, startCol = 1) # subhead
  writeData(workbook, sheet_name, text$source,
            startRow = 8, startCol = ncol(data)) # source info
  if(season_var == "spring") {
    writeData(workbook, sheet_name, text$final_header, 
              startRow = 5, startCol = ncol(data)) # provisional year head
    writeData(workbook, sheet_name, provisional_note,
              startRow = 10, startCol = 1) # provisional note
  }

# adding styles -----------------------------------------------------------

  # text styles
  addStyle(workbook, sheet_name, styles$black_bold_nowrap_12,
           rows = 4, cols = 1:(ncol(data)), gridExpand = T, stack = T) # subheader
  addStyle(workbook, sheet_name, styles$black_12, 
           rows = c(4:8, 10), cols = 1:(ncol(data)), gridExpand = T, stack = T) # body
  
  # alignment
  addStyle(workbook, sheet_name, styles$a_middle,
           rows = c(4, 6:7), cols = c(2:ncol(data)), gridExpand = T, stack = T) # vertical align
  addStyle(workbook, sheet_name, styles$a_centre,
           rows = 4:7, cols = 2:ncol(data), gridExpand = T, stack = T) # horizontal align
  addStyle(workbook, sheet_name, styles$a_right,
           rows = 8, cols = ncol(data), gridExpand = T, stack = T) # source horizontal align
  
  # borders
  addStyle(workbook, sheet_name, styles$b_left_bold, 
           rows = 4:7, cols = c(1, 2, (ncol(data)+1)), gridExpand = T, stack = T) # bold left
  addStyle(workbook, sheet_name, styles$b_left, 
           rows = 5:7, cols = 3:(ncol(data)), gridExpand = T, stack = T) # left
  addStyle(workbook, sheet_name, styles$b_top_bold, 
           rows = c(4:6, 8), cols = 1:(ncol(data)), gridExpand = T, stack = T) # bold top
  
}



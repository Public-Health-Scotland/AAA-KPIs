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
library(phsaaa)
  


# DNA Exclusions ----------------------------------------------------------

# Write DNA Exclusions sheet of theme 2 Workbook (main table + notes, header in template still)
write_dna_exclusions <- function(workbook, sheet_name, season_var, data, provisional_note) {
  


# notes and styles --------------------------------------------------------
  ## texts for writing in
  texts <- list()
  texts$header <- "Excluded in year ending 31 March"
  texts$subheader <- "Exclusion type"
  texts$source <- "Source: Scottish AAA Call Recall System"
  if(season_var == "spring"){
    texts$final_header <- paste0(colnames(data)[max(ncol(data))], " (provisional data)", {supsc("p")})
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

  writeData(workbook, sheet_name, texts$header,
            startRow = 4, startCol = 2) # header
  writeData(workbook, sheet_name, data,
            startRow = 5, startCol = 1) # data
  writeData(workbook, sheet_name, texts$subheader,
            startRow = 4, startCol = 1) # subhead
  writeData(workbook, sheet_name, texts$source,
            startRow = 8, startCol = ncol(data)) # source info
  if(season_var == "spring") {
    writeData(workbook, sheet_name, texts$final_header, 
              startRow = 5, startCol = ncol(data)) # provisional year head
    writeData(workbook, sheet_name, provisional_note,
              startRow = 10, startCol = 1) # provisional note
  }

# adding styles -----------------------------------------------------------

  # texts styles
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



# Batch QA ----------------------------------------------------------------






# Write Batch QA standard not met screens excel sheet for theme 3 workbook (sheet and headers already in template)
write_batch_qa <- function(workbook, sheet_name, season_var, financial_years, data_scot, data_reason, data_recall, provisional_note) {

# notes and styles --------------------------------------------------------

  # texts notes
  texts <- list()
   if(season_var == "spring") {
     texts$years <- c(paste0("Screened in year ending 31 March ", substr(financial_years[1], 1, 2), substr(financial_years[1], 6, 7)),
                      paste0("Screened in year ending 31 March ", substr(financial_years[2], 1, 2), substr(financial_years[2], 6, 7)),
                      paste0("Screened in year ending 31 March ", substr(financial_years[3], 1, 2), substr(financial_years[3], 6, 7), " (partial data)"))
   } else if(season_var == "autumn") {
     texts$years <- c(paste0("Screened in year ending 31 March ", substr(financial_years[1], 1, 2), substr(financial_years[1], 6, 7)),
                      paste0("Screened in year ending 31 March ", substr(financial_years[2], 1, 2), substr(financial_years[2], 6, 7)),
                      paste0("Screened in year ending 31 March ", substr(financial_years[3], 1, 2), substr(financial_years[3], 6, 7)))
   }
  texts$scot_head <- "Batch standard not met screens by reason: Scotland summary" # scotland table
  texts$scot_grp <- "Batch standard not met reason"
  texts$scot_subhead <- c(rep("N", 3))
  texts$reason_head <- "Batch standard not met screens by reason: NHS Board of Screening summary" # reason table
  texts$reason_recall_grp <- "NHS Board of Screening"
  texts$reason_subhead1 <- c(rep(c("Reason", rep("", 4)), 3))
  # texts$reason_subhead1 <- c(rep("Reason", 3))
  texts$reason_subhead2 <- data.frame(x = c("Screener", "Equipment", "Location", "Other with notes", "Total"),
                                    y = c(rep("N", 5)))
  texts$recall_head <- "Batch standard not met screens by recall advice: NHS Board of Screening summary" # recall table
  texts$recall_subhead1 <- c(rep(c("Recall advice", rep("", 5)), 3))
  texts$recall_subhead2 <- data.frame(x = c("Immediate Recall", "Recall in Current Cycle", "No Recall - Satisfactory Interim Scan", 
                                          "No Recall - Referred to Vascular", "No Recall - Verified by 2nd opinion", "Total"),
                                    y = c(rep("N", 6)))
  texts$source <- "Source: Scottish AAA Call Recall System" # source
  texts$notes <- c("Notes", " ", " ", "- Zero", " ", " ", "Return to Table of Contents") # notes
  texts$p_note <- provisional_note
  
  # source styles
  source(here::here("code", "src", "Source_Excel_Styles.R"))
  
  # refs - rows for data to be written to 
  # head is the main header of table, start is the first row of data of table
  ref <- list()
  ref$scot_head <- 4 # scotland table
  ref$scot_start <- ref$scot_head + 3
  ref$reason_head <- ref$scot_start + nrow(data_scot) + 1 # reason table
  ref$reason_start <- ref$reason_head + 5
  ref$recall_head <- ref$reason_start + nrow(data_reason) + 1 # recall table
  ref$recall_start <- ref$recall_head + 5
  ref$source <- ref$recall_start + nrow(data_recall) # source
  ref$notes_start <- ref$source + 3 # notes

# formatting cells --------------------------------------------------------
  
  # merging
  mergeCells(workbook, sheet_name, rows = (ref$scot_head + 1):(ref$scot_head + 2), cols = 1) # scotland table
  mergeCells(workbook, sheet_name, rows = (ref$reason_head + 1):(ref$reason_head + 4), cols = 1) # reason table
  mergeCells(workbook, sheet_name, rows = (ref$reason_head + 1), cols = 2:6)
  mergeCells(workbook, sheet_name, rows = (ref$reason_head + 1), cols = 7:11)
  mergeCells(workbook, sheet_name, rows = (ref$reason_head + 1), cols = 12:16)
  mergeCells(workbook, sheet_name, rows = (ref$reason_head + 2), cols = 2:6)
  mergeCells(workbook, sheet_name, rows = (ref$reason_head + 2), cols = 7:11)
  mergeCells(workbook, sheet_name, rows = (ref$reason_head + 2), cols = 12:16)
  mergeCells(workbook, sheet_name, rows = (ref$recall_head + 1):(ref$recall_head + 4), cols = 1) # recall table
  mergeCells(workbook, sheet_name, rows = (ref$recall_head + 1), cols = 2:7)
  mergeCells(workbook, sheet_name, rows = (ref$recall_head + 1), cols = 8:13)
  mergeCells(workbook, sheet_name, rows = (ref$recall_head + 1), cols = 14:19)
  mergeCells(workbook, sheet_name, rows = (ref$recall_head + 2), cols = 2:7)
  mergeCells(workbook, sheet_name, rows = (ref$recall_head + 2), cols = 8:13)
  mergeCells(workbook, sheet_name, rows = (ref$recall_head + 2), cols = 14:19)
  mergeCells(workbook, sheet_name, rows = ref$source, cols = 17:19) # source
  mergeCells(workbook, sheet_name, rows = ref$notes_start + 1, cols = 1:19) # notes
  
  # col/row dimensions
  setColWidths(workbook, sheet_name, cols = 1, widths = 17)
  setColWidths(workbook, sheet_name, cols = 2:19, widths = 13.8)
  setRowHeights(workbook, sheet_name, 
                rows = (ref$scot_head):((ref$notes_start)+(length(texts$notes))), heights = 14.5) # body
  setRowHeights(workbook, sheet_name, rows = c(ref$scot_head, ref$reason_head, ref$recall_head), heights = 18.5) # heads
  setRowHeights(workbook, sheet_name, 
                rows = c(ref$scot_head+1, ref$reason_head+1, ref$recall_head+1, 
                         ref$reason_head+2, ref$recall_head+2, 
                         ref$scot_head+2, ref$reason_head+4, ref$recall_head+4), 
                heights = c(57, rep(20.4, 7))) # screened/subheads
  setRowHeights(workbook, sheet_name, rows = c(ref$reason_head+3, ref$notes_start+1, ref$recall_head+3),
                heights = c(rep(33, 2), 43)) # spec subheads + prov note
  
  # gridlines
  showGridLines(workbook, sheet_name, showGridLines = F)

# writing data ------------------------------------------------------------
  
  # scotland table
  writeData(workbook, sheet_name, texts$scot_head, startRow = ref$scot_head, startCol = 1)
  writeData(workbook, sheet_name, texts$scot_grp, startRow = ref$scot_head+1, startCol = 1)
  writeData(workbook, sheet_name, t(texts$years), colNames = F, startRow = ref$scot_head+1, startCol = 2)
  writeData(workbook, sheet_name, t(texts$scot_subhead), colNames = F, startRow = ref$scot_head+2, startCol = 2)
  writeData(workbook, sheet_name, data_scot, colNames = F, startRow = ref$scot_start, startCol = 1)

  # reason table
  writeData(workbook, sheet_name, texts$reason_head, startRow = ref$reason_head, startCol = 1)
  writeData(workbook, sheet_name, texts$reason_recall_grp, startRow = ref$reason_head+1, startCol = 1)
  writeData(workbook, sheet_name, texts$years[1], startRow = ref$reason_head+1, startCol = 2)
  writeData(workbook, sheet_name, texts$years[2], startRow = ref$reason_head+1, startCol = 7)
  writeData(workbook, sheet_name, texts$years[3], startRow = ref$reason_head+1, startCol = 12)
  writeData(workbook, sheet_name, t(texts$reason_subhead1), colNames = F, startRow = ref$reason_head+2, startCol = 2)
  writeData(workbook, sheet_name, t(texts$reason_subhead2), colNames = F, startRow = ref$reason_head+3, startCol = 2)
  writeData(workbook, sheet_name, t(texts$reason_subhead2), colNames = F, startRow = ref$reason_head+3, startCol = 7)
  writeData(workbook, sheet_name, t(texts$reason_subhead2), colNames = F, startRow = ref$reason_head+3, startCol = 12)
  writeData(workbook, sheet_name, data_reason, colNames = F, startRow = ref$reason_start, startCol = 1)

  # recall table
  writeData(workbook, sheet_name, texts$recall_head, startRow = ref$recall_head, startCol = 1)
  writeData(workbook, sheet_name, texts$reason_recall_grp, startRow = ref$recall_head+1, startCol = 1)
  writeData(workbook, sheet_name, texts$years[1], startRow = ref$recall_head+1, startCol = 2)
  writeData(workbook, sheet_name, texts$years[2], startRow = ref$recall_head+1, startCol = 8)
  writeData(workbook, sheet_name, texts$years[3], startRow = ref$recall_head+1, startCol = 14)
  writeData(workbook, sheet_name, t(texts$recall_subhead1), colNames = F, startRow = ref$recall_head+2, startCol = 2)
  writeData(workbook, sheet_name, t(texts$recall_subhead2), colNames = F, startRow = ref$recall_head+3, startCol = 2)
  writeData(workbook, sheet_name, t(texts$recall_subhead2), colNames = F, startRow = ref$recall_head+3, startCol = 8)
  writeData(workbook, sheet_name, t(texts$recall_subhead2), colNames = F, startRow = ref$recall_head+3, startCol = 14)
  writeData(workbook, sheet_name, data_recall, colNames = F, startRow = ref$recall_start, startCol = 1)

  # source
  writeData(workbook, sheet_name, texts$source, startRow = ref$source, startCol = 17)

  # notes
  writeData(workbook, sheet_name, texts$notes, startRow = ref$notes_start, startCol = 1)
  if(season_var == "spring") {
    writeData(workbook, sheet_name, texts$p_note, startRow = ref$notes_start+1, startCol = 1)
  }

# adding styles -----------------------------------------------------------

  
}



season <- "spring"
wb <- loadWorkbook(paste0(template_path, "/3_Quality Assurance_",
                          season, "_TEST.xlsx"))
write_batch_qa(wb, "Batch QA standard not met", season, financial_years = kpi_report_years, 
               qa_batch_scot, qa_batch_hb, qa_recall, kpi_2.2_notep)


query_saveWorkbook(wb, paste0(output_path, "/3_Quality Assurance_", yymm, "_TEST.xlsx"))

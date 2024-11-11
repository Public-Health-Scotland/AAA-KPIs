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

  # text styles
  ## headers
  addStyle(workbook, sheet_name, styles$black_bold_nowrap_14,
           rows = c(ref$scot_head, ref$reason_head, ref$recall_head), cols = c(1, 1, 1), gridExpand = T, stack = T) # headers
  ## scotland table
  addStyle(workbook, sheet_name, styles$black_11,
           rows = (ref$scot_head+1):(ref$scot_head+7), cols = 1:(ncol(data_scot)+1), gridExpand = T, stack = T) # body
  addStyle(workbook, sheet_name, styles$black_bold_11,
           rows = ref$scot_start+nrow(data_scot)-1, cols = 1:(ncol(data_scot)+1), gridExpand = T, stack = T) # total row
  ## reason table
  addStyle(workbook, sheet_name, styles$black_11,
           rows = (ref$reason_head+1):(ref$reason_head+7), cols = 1:(ncol(data_reason)+1), gridExpand = T, stack = T) # body
  addStyle(workbook, sheet_name, styles$black_bold_11,
           rows = ref$reason_start+nrow(data_reason)-1, cols = 1:(ncol(data_reason)+1), gridExpand = T, stack = T) # total row
  ## recall table
  addStyle(workbook, sheet_name, styles$black_11,
          rows = (ref$recall_head+1):(ref$recall_head+8), cols = 1:(ncol(data_recall)+1), gridExpand = T, stack = T) # body
  addStyle(workbook, sheet_name, styles$black_bold_11,
           rows = ref$recall_start+nrow(data_recall)-1, cols = 1:(ncol(data_recall)+1), gridExpand = T, stack = T) # total row
  ## notes
  addStyle(workbook, sheet_name, styles$black_bold_12,
           rows = ref$notes_start, cols = 1, gridExpand = T, stack = T) # notes head
  addStyle(workbook, sheet_name, styles$black_11,
           rows = (ref$notes_start+1):(ref$notes_start+2), cols = 1, gridExpand = T, stack = T) # notes body
  addStyle(workbook, sheet_name, styles$blue_nowrap_underline_11,
           rows = ref$notes_start+6, cols = 1, gridExpand = T, stack = T) # return to contents

  # alignment
  ## scotland table
  addStyle(workbook, sheet_name, styles$a_left,
           rows = (ref$scot_head+1):(ref$scot_start+nrow(data_scot)-1), cols = 1, gridExpand = T, stack = T) # left align
  addStyle(workbook, sheet_name, styles$a_centre,
           rows = (ref$scot_head+1):(ref$scot_start+nrow(data_scot)-1), cols = 2:(ncol(data_scot)+1), gridExpand = T, stack = T) # centre halign
  addStyle(workbook, sheet_name, styles$a_middle,
           rows = (ref$scot_head+1):(ref$scot_start+nrow(data_scot)-1), cols = 2:(ncol(data_scot)+1), gridExpand = T, stack = T) # middle valign
  ## reason table
  addStyle(workbook, sheet_name, styles$a_left,
           rows = (ref$reason_head+1):(ref$reason_start+nrow(data_reason)-1), cols = 1, gridExpand = T, stack = T) # left align
  addStyle(workbook, sheet_name, styles$a_centre,
           rows = (ref$reason_head+1):(ref$reason_start+nrow(data_reason)-1), cols = 2:(ncol(data_reason)+1), gridExpand = T, stack = T) # centre halign
  addStyle(workbook, sheet_name, styles$a_middle,
           rows = (ref$reason_head+1):(ref$reason_start+nrow(data_reason)-1), cols = 2:(ncol(data_reason)+1), gridExpand = T, stack = T) # middle valign
  ## recall table
  addStyle(workbook, sheet_name, styles$a_left,
           rows = (ref$recall_head+1):(ref$recall_start+nrow(data_recall)-1), cols = 1, gridExpand = T, stack = T) # left align
  addStyle(workbook, sheet_name, styles$a_centre,
           rows = (ref$recall_head+1):(ref$recall_start+nrow(data_recall)-1), cols = 2:(ncol(data_recall)+1), gridExpand = T, stack = T) # centre halign
  addStyle(workbook, sheet_name, styles$a_middle,
           rows = (ref$recall_head+1):(ref$recall_start+nrow(data_recall)-1), cols = 2:(ncol(data_recall)+1), gridExpand = T, stack = T) # middle valign
  ## notes
  addStyle(workbook, sheet_name, styles$a_right,
           rows = ref$source, cols = 17, gridExpand = T, stack = T) # right align source
  addStyle(workbook, sheet_name, styles$a_left,
           rows = ref$notes_start:(ref$notes_start + 6), cols = 1, gridExpand = T, stack = T) # left align

  # borders
  ## scotland table
  addStyle(workbook, sheet_name, styles$b_left_bold,
           rows = (ref$scot_head+1):(ref$scot_start+nrow(data_scot)-1),
           cols = 1:(ncol(data_scot)+1), gridExpand = T, stack = T) # left bold
  addStyle(workbook, sheet_name, styles$b_top_bold,
           rows = c(ref$scot_head+1, ref$scot_head+2, ref$scot_start, (ref$scot_start+nrow(data_scot))),
           cols = 1:ncol(data_scot), gridExpand = T, stack = T) # top bold
  ## reason table
  addStyle(workbook, sheet_name, styles$b_left_bold,
           rows = (ref$reason_head+1):(ref$reason_start+nrow(data_reason)-1),
           cols = c(1, 2, 7, 12, ncol(data_reason)+1), gridExpand = T, stack = T) # left bold
  addStyle(workbook, sheet_name, styles$b_left,
           rows = (ref$reason_head+3):(ref$reason_start+nrow(data_reason)-1),
           cols = c(3:6, 8:11, 13:16), gridExpand = T, stack = T) # left
  addStyle(workbook, sheet_name, styles$b_top_bold,
           rows = c(ref$reason_head+1, ref$reason_head+2, ref$reason_start, (ref$reason_start+nrow(data_reason))),
           cols = 1:ncol(data_reason), gridExpand = T, stack = T) # top bold
  addStyle(workbook, sheet_name, styles$b_top,
           rows = c(ref$reason_head+3, ref$reason_head+4), 
           cols = 1:ncol(data_reason), gridExpand = T, stack = T) # top
  ## recall table
  addStyle(workbook, sheet_name, styles$b_left_bold,
           rows = (ref$recall_head+1):(ref$recall_start+nrow(data_recall)-1),
           cols = c(1, 2, 8, 14, ncol(data_recall)+1), gridExpand = T, stack = T) # left bold
  addStyle(workbook, sheet_name, styles$b_left,
           rows = (ref$recall_head+3):(ref$recall_start+nrow(data_recall)-1),
           cols = c(3:7, 9:13, 15:19), gridExpand = T, stack = T) # left
  addStyle(workbook, sheet_name, styles$b_top_bold,
           rows = c(ref$recall_head+1, ref$recall_head+2, ref$recall_start, (ref$recall_start+nrow(data_recall))),
           cols = 1:ncol(data_recall), gridExpand = T, stack = T) # top bold
  addStyle(workbook, sheet_name, styles$b_top,
           rows = c(ref$recall_head+3, ref$recall_head+4), 
           cols = 1:ncol(data_recall), gridExpand = T, stack = T) # top
}



# KPI 4.1 Additional ------------------------------------------------------

# Write KPI 4.1 Additional excel sheet for theme 3 workbook (sheet and headers already in template)
write_kpi4.1_add <- function(workbook, sheet_name, season_var, financial_years, data_A, data_B, data_C, provisional_note) {
  
  # notes and styles --------------------------------------------------------
  
  # texts for writing in
  texts <- list()
  texts$titles <- c("KPI 4.1 Additional (A): Number of deaths within 30 days following open elective surgery by financial year",
                    "KPI 4.1 Additional (B): Number of deaths within 30 days following open elective surgery by NHS Board of Screening",
                    "KPI 4.1 Additional (C): Number of deaths within 30 days following open elective surgery by NHS Board of Surgery")
  texts$year_op <- "Year of operation"
  texts$year_end <- paste0("Year ending 31 March ", substr(financial_years[3], 1, 2), substr(financial_years[3], 6, 7), {supsc("p")}) # only used in spring
  if (season_var == "spring") {
    texts$cumulative <- paste0("Cumulative total to 31 March ", substr(financial_years[3], 1, 2), substr(financial_years[3], 6, 7), {supsc("p")})
  }
  else if (season_var == "autumn") {
    texts$cumulative <- paste0("Cumulative total to 31 March ", substr(financial_years[3], 1, 2), substr(financial_years[3], 6, 7))
  }
  texts$source <- "Source: Scottish AAA Call Recall System"
  texts$notes <- c("Notes", "1. Men may be treated in a different NHS Board to where they are screened; therefore, this KPI is calculated using NHS Board of surgery.",
                   " ", " ", "-   Zero / Not applicable", " ", "Return to Table of Contents")
  texts$p_note <- provisional_note
  texts$head_A <- "Open Surgery"  ## table A
  texts$subhead_A <- tibble(x = c("Operations", "Died within 30 days"), y = c("N", "N"))
  texts$head_B <- "NHS Board of Screening"   ## table B
  texts$names_B <- names(data_B)
  texts$head_C <- "NHS Board of Surgery"  ## table C
  texts$names_C <- names(data_C)
  
  
  # source styles
  source(here::here("code", "src", "Source_Excel_Styles.R"))
  
  # refs - rows for data to be written to 
  # head is the main header of table, start is the first row of data of table
  ref <- list()
  ref$head_A <- 4 # header A
  ref$start_A <- ref$head_A + 3 # data start A
  ref$src_A <- ref$start_A + nrow(data_A) # source A
  ref$head_B <- ref$src_A + 4 # header B
  ref$start_B <- ref$head_B + 2 # data start B
  ref$src_B <- ref$start_B + nrow(data_B)  # source B
  ref$head_C <- ref$src_B + 4 # header C
  ref$start_C <- ref$head_C + 2 # data start C
  ref$src_C <- ref$start_C + nrow(data_C)  # source C
  ref$notes_start <- ref$src_C + 2 # notes
  
  
  # formatting cells --------------------------------------------------------
  
  # merging
  mergeCells(workbook, sheet_name, rows = (ref$head_A):(ref$head_A + 2), cols = 1) # add A table
  mergeCells(workbook, sheet_name, rows = ref$head_A , cols = 2:ncol(data_A))
  mergeCells(workbook, sheet_name, rows = ref$src_A, cols = (ncol(data_A)-1):ncol(data_A))
  mergeCells(workbook, sheet_name, rows = (ref$head_B):(ref$head_B + 1), cols = 1) # add B table
  mergeCells(workbook, sheet_name, rows = ref$head_B , cols = 2:ncol(data_B))
  mergeCells(workbook, sheet_name, rows = ref$src_B, cols = (ncol(data_B)-1):ncol(data_B)) 
  mergeCells(workbook, sheet_name, rows = (ref$head_C):(ref$head_C + 1), cols = 1) # add C table
  mergeCells(workbook, sheet_name, rows = ref$head_C, cols = 2:ncol(data_C))
  mergeCells(workbook, sheet_name, rows = ref$src_C, cols = (ncol(data_C)-1):ncol(data_C)) 
  mergeCells(workbook, sheet_name, rows = ref$notes_start + 1, cols = 1:9) # notes
  mergeCells(workbook, sheet_name, rows = ref$notes_start + 2, cols = 1:10)
  
  # col/row heights
  setColWidths(workbook, sheet_name, cols = 1, widths = 38)
  setColWidths(workbook, sheet_name, cols = 2:ncol(data_B), widths = 17.8)
  setRowHeights(workbook, sheet_name, rows = c((ref$head_A - 2), (ref$head_B - 2), (ref$head_C - 2)), heights = 23) # titles
  setRowHeights(workbook, sheet_name, rows = c(ref$head_A, ref$head_B, ref$head_C), heights = 19.3) # heads
  setRowHeights(workbook, sheet_name, rows = c((ref$head_A + 1), (ref$head_B + 1), (ref$head_C + 1)), heights = 31.5, wrap = F) # subheads
  setRowHeights(workbook, sheet_name, rows = ref$head_A + 2, heights = 16) # subhead 2 add A
  setRowHeights(workbook, sheet_name, 
                rows = c((ref$start_A:ref$src_A), (ref$start_B:ref$src_B), (ref$start_C:ref$src_C), (ref$notes_start:(ref$notes_start + 7))), 
                heights = 15.5) # body of tables
  if(season_var == "spring") {
    setRowHeights(workbook, sheet_name, rows = ref$notes_start + 2, heights = 28.5)}
  
  
  # gridlines
  showGridLines(workbook, sheet_name, showGridLines = F)
  
  
  # writing data ------------------------------------------------------------
  
  # Add A table
  writeData(workbook, sheet_name, texts$titles[1], startRow = ref$head_A -2)
  writeData(workbook, sheet_name, texts$year_op, startRow = ref$head_A, startCol = 1)
  writeData(workbook, sheet_name, texts$head_A, startRow = ref$head_A, startCol = 2)
  writeData(workbook, sheet_name, t(texts$subhead_A), startRow = ref$head_A + 1, startCol = 2, colNames = F)
  writeData(workbook, sheet_name, data_A, startRow = ref$start_A, colNames = F)
  if(season_var == "spring") {
    writeData(workbook, sheet_name, texts$year_end, startRow = ref$src_A - 2, colNames = F)
  }
  writeData(workbook, sheet_name, texts$cumulative, startRow = ref$src_A - 1, colNames = F)
  writeData(workbook, sheet_name, texts$source, startRow = ref$src_A, startCol = (ncol(data_A) - 1), colNames = F)
  
  # Add B table
  writeData(workbook, sheet_name, texts$titles[2], startRow = ref$head_B -2)
  writeData(workbook, sheet_name, texts$year_op, startRow = ref$head_B, startCol = 1)
  writeData(workbook, sheet_name, texts$head_B, startRow = ref$head_B, startCol = 2)
  writeData(workbook, sheet_name, t(texts$names_B), startRow = ref$head_B + 1, startCol = 1, colNames = F)
  writeData(workbook, sheet_name, data_B, startRow = ref$start_B, colNames = F)
  if(season_var == "spring") {
    writeData(workbook, sheet_name, texts$year_end, startRow = ref$src_B - 2, colNames = F)
  }
  writeData(workbook, sheet_name, texts$cumulative, startRow = ref$src_B - 1, colNames = F)
  writeData(workbook, sheet_name, texts$source, startRow = ref$src_B, startCol = (ncol(data_B) - 1), colNames = F)
  
  # Add C table
  writeData(workbook, sheet_name, texts$titles[3], startRow = ref$head_C -2)
  writeData(workbook, sheet_name, texts$year_op, startRow = ref$head_C, startCol = 1)
  writeData(workbook, sheet_name, texts$head_C, startRow = ref$head_C, startCol = 2)
  writeData(workbook, sheet_name, t(texts$names_C), startRow = ref$head_C + 1, startCol = 1, colNames = F)
  writeData(workbook, sheet_name, data_C, startRow = ref$start_C, colNames = F)
  if(season_var == "spring") {
    writeData(workbook, sheet_name, texts$year_end, startRow = ref$src_C - 2, colNames = F)
  }
  writeData(workbook, sheet_name, texts$cumulative, startRow = ref$src_C - 1, colNames = F)
  writeData(workbook, sheet_name, texts$source, startRow = ref$src_C, startCol = (ncol(data_C) - 1), colNames = F)
  
  # Notes
  writeData(workbook, sheet_name, texts$notes, startRow = ref$notes_start, colNames = F)
  if(season_var == "spring") {
    writeData(workbook, sheet_name, texts$p_note, startRow = ref$notes_start + 2, colNames = F)
  }
  
  # adding styles -----------------------------------------------------------
  
  # texts styles
  ## titles
  addStyle(workbook, sheet_name, styles$black_bold_18, 
           rows = c(ref$head_A - 2, ref$head_B - 2, ref$head_C - 2), cols = 1, stack = T, gridExpand = T)
  # add A table
  addStyle(workbook, sheet_name, styles$black_12, 
           rows = (ref$head_A):(ref$src_A - 1), cols = 1:ncol(data_A), stack = T, gridExpand = T)
  addStyle(workbook, sheet_name, styles$black_nowrap_11, 
           rows = ref$src_A, cols = (ncol(data_A) - 1), stack = T, gridExpand = T)
  ## add B table
  addStyle(workbook, sheet_name, styles$black_12, 
           rows = (ref$head_B):(ref$src_B - 1), cols = 1:ncol(data_B), stack = T, gridExpand = T) 
  addStyle(workbook, sheet_name, styles$black_nowrap_11, 
           rows = ref$src_B, cols = (ncol(data_B) - 1), stack = T, gridExpand = T)
  ## add C table
  addStyle(workbook, sheet_name, styles$black_12, 
           rows = (ref$head_C):(ref$src_C - 1), cols = 1:ncol(data_C), stack = T, gridExpand = T) 
  addStyle(workbook, sheet_name, styles$black_nowrap_11, 
           rows = ref$src_C, cols = (ncol(data_C) - 1), stack = T, gridExpand = T)
  ## notes
  addStyle(workbook, sheet_name, styles$black_bold_nowrap_12, 
           rows = ref$notes_start, cols = 1, stack = T, gridExpand = T) 
  addStyle(workbook, sheet_name, styles$black_12,
           rows = (ref$notes_start + 1):(ref$notes_start + 4), cols = 1, stack = T, gridExpand = T)
  addStyle(workbook, sheet_name, styles$blue_nowrap_underline_11,
           rows = ref$notes_start + 6, cols = 1, stack = T, gridExpand = T)
  
  # alignment
  ## add A table
  addStyle(workbook, sheet_name, styles$a_middle, 
           rows = ref$head_A:ref$src_A, cols = 2:ncol(data_A), stack = T, gridExpand = T) # v middle
  addStyle(workbook, sheet_name, styles$a_centre, 
           rows = ref$head_A:(ref$src_A - 1), cols = 2:ncol(data_A), stack = T, gridExpand = T) # h centre
  addStyle(workbook, sheet_name, styles$a_right,
           rows = ref$src_A, cols = (ncol(data_A) - 1), stack = T, gridExpand = T) # h right
  ## add B table
  addStyle(workbook, sheet_name, styles$a_middle, 
           rows = ref$head_B:ref$src_B, cols = 2:ncol(data_B), stack = T, gridExpand = T) # v middle
  addStyle(workbook, sheet_name, styles$a_centre, 
           rows = ref$head_B:(ref$src_B - 1), cols = 2:ncol(data_B), stack = T, gridExpand = T) # h centre
  addStyle(workbook, sheet_name, styles$a_right,
           rows = ref$src_B, cols = (ncol(data_B) - 1), stack = T, gridExpand = T) # h right
  ## add C table
  addStyle(workbook, sheet_name, styles$a_middle, 
           rows = ref$head_C:ref$src_C, cols = 2:ncol(data_C), stack = T, gridExpand = T) # v middle
  addStyle(workbook, sheet_name, styles$a_centre, 
           rows = ref$head_C:(ref$src_C - 1), cols = 2:ncol(data_C), stack = T, gridExpand = T) # h centre
  addStyle(workbook, sheet_name, styles$a_centre, 
           rows = ref$head_C:(ref$src_C - 1), cols = 2:ncol(data_C), stack = T, gridExpand = T) # h right
  
  # borders
  ## add A table
  addStyle(workbook, sheet_name, styles$b_left_bold,
           rows = ref$head_A:(ref$src_A - 1), cols = c(1, 2, ncol(data_A) + 1), stack = T, gridExpand = T) # left bold
  addStyle(workbook, sheet_name, styles$b_left,
           rows = (ref$head_A + 1):(ref$src_A - 1), cols = ncol(data_A), stack = T, gridExpand = T) # left
  addStyle(workbook, sheet_name, styles$b_top_bold,
           rows = c(ref$head_A, ref$head_A + 1, ref$start_A, ref$src_A - 1, ref$src_A), cols = 1:ncol(data_A), stack = T, gridExpand = T) # top bold
  addStyle(workbook, sheet_name, styles$b_top,
           rows = ref$start_A - 1, cols = 2:ncol(data_A), stack = T, gridExpand = T) # top
  ## add B table
  addStyle(workbook, sheet_name, styles$b_left_bold,
           rows = ref$head_B:(ref$src_B - 1), cols = c(1, 2, ncol(data_B) + 1), stack = T, gridExpand = T) # left bold
  addStyle(workbook, sheet_name, styles$b_left,
           rows = (ref$head_B + 1):(ref$src_B - 1), cols = ncol(data_B), stack = T, gridExpand = T) # left
  addStyle(workbook, sheet_name, styles$b_top_bold,
           rows = c(ref$head_B, ref$head_B + 1, ref$start_B, ref$src_B - 1, ref$src_B), cols = 1:ncol(data_B), stack = T, gridExpand = T) # top bold
  ## add C table
  addStyle(workbook, sheet_name, styles$b_left_bold,
           rows = ref$head_C:(ref$src_C - 1), cols = c(1, 2, ncol(data_C) + 1), stack = T, gridExpand = T) # left bold
  addStyle(workbook, sheet_name, styles$b_left,
           rows = (ref$head_C + 1):(ref$src_C - 1), cols = ncol(data_B), stack = T, gridExpand = T) # left
  addStyle(workbook, sheet_name, styles$b_top_bold,
           rows = c(ref$head_C, ref$head_C + 1, ref$start_C, ref$src_C - 1, ref$src_C), cols = 1:ncol(data_C), stack = T, gridExpand = T) # top bold
 
  
  # number formats
  addStyle(workbook, sheet_name, styles$counts,
           rows = ref$start_A:(ref$src_A - 1), cols = 2:(ncol(data_A) - 1), stack = T, gridExpand = T) # add A
  addStyle(workbook, sheet_name, styles$counts,
           rows = ref$start_B:(ref$src_B - 1), cols = 2:(ncol(data_B) - 1), stack = T, gridExpand = T) # add B
  addStyle(workbook, sheet_name, styles$counts,
           rows = ref$start_C:(ref$src_C - 1), cols = 2:(ncol(data_C) - 1), stack = T, gridExpand = T) # add C

}



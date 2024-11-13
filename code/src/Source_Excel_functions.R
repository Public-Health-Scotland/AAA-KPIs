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
write_kpi4_add <- function(workbook, sheet_name, season_var, financial_years, data_A, data_B, data_C, provisional_note) {
  
  # notes and styles --------------------------------------------------------
  
  # texts for writing in
  texts <- list()
  texts$titles <- c(paste0(sheet_name, " (A): Number of deaths within 30 days following open elective surgery by financial year"),
                    paste0(sheet_name, " (B): Number of deaths within 30 days following open elective surgery by NHS Board of Screening"),
                    paste0(sheet_name, " (C): Number of deaths within 30 days following open elective surgery by NHS Board of Surgery"))
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
  if(sheet_name == "KPI 4.1 Additional") {
    texts$head_A <- "Open Surgery"  ## table A
  } else if (sheet_name == "KPI 4.2 Additional") {
    texts$head_A <- "EVAR"  ## table A
  }
  texts$subhead_A <- tibble(x = c("Operations", "Died within 30 days"), y = c("N", "N"))
  texts$head_B <- "NHS Board of Screening"   ## table B
  texts$names_B <- names(data_B)
  texts$head_C <- paste0("NHS Board of Surgery", {supsc("1")})  ## table C
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
  addStyle(workbook, sheet_name, styles$black_bold_nowrap_18, 
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



# Vascular KPIs background ------------------------------------------------


# Write KPI 4.1 Additional excel sheet for theme 3 workbook (sheet and headers already in template)
write_vasc_background <- function(workbook, sheet_name, season_var, financial_years, data_outcomes, provisional_note) {

  # formatting data ---------------------------------------------------------
  data_outcomes <- data_outcomes |> 
    mutate(result_outcome = case_when(result_outcome == "99" & result_size == "large" ~ "Total referrals ≥ 5.5cm",
                                      result_outcome == "99" & result_size == "small" ~ paste0("Total referrals < 5.5cm", {supsc("1")}),
                                      result_outcome == "98" ~ "Referrals with a final outcome",
                                      result_outcome == "01" ~ "Declined vascular referral",
                                      result_outcome == "02" ~ "Referred in error: Appointment with vascular service not required",
                                      result_outcome == "03" ~ "DNA outpatient service: Self-discharge",
                                      result_outcome == "04" ~ "DNA outpatient service: Died before appointment (died within 10 working days of referral)",
                                      result_outcome == "05" ~ "DNA outpatient service: Died before appointment (died more than 10 working days from referral)",
                                      result_outcome == "06" ~ "Referred in error: As determined by vascular service",
                                      result_outcome == "07" ~ "Died before surgical assessment completed",
                                      result_outcome == "08" ~ "Unfit for surgery",
                                      result_outcome == "11" ~ "Appropriate for Surgery: Patient declined surgery",
                                      result_outcome == "12" ~ "Appropriate for Surgery: Died before treatment",
                                      result_outcome == "13" ~ "Appropriate for Surgery: Self-discharge",
                                      result_outcome == "15" ~ "Appropriate for Surgery: AAA repaired and survived 30 days",
                                      result_outcome == "16" ~ "Appropriate for Surgery: Died within 30 days of treatment",
                                      result_outcome == "20" ~ "Other final outcome",
                                      result_outcome == "97" ~ "Referrals with a non-final outcome",
                                      result_outcome == "09" ~ "Refer to another specialty",
                                      result_outcome == "10" ~ "Awaiting further AAA growth",
                                      result_outcome == "14" ~ "Appropriate for surgery: Patient deferred surgery",
                                      result_outcome == "17" ~ "Appropriate for surgery: Final outcome pending",
                                      result_outcome == "18" ~ "Ongoing assessment by vascular",
                                      result_outcome == "19" ~ "Final outcome pending",
                                      result_outcome == "96" ~ "Referrals with no outcome recorded",
                                      TRUE ~ "Error!!!!"
                                      )) |> 
    filter(!(!str_detect(outcome_type, "Total") & cumulative == 0)) # keeps "Total" rows where cumulative == 0, removes all other cumulative == 0 
  
  
  large_tot <- data_outcomes |> 
    filter(result_size == "large" & outcome_type == "Total") |>
    select(-c(result_size, outcome_type))
  
  large_fin <- data_outcomes |> 
    filter(result_size == "large" & (outcome_type == "Total: final outcome" | outcome_type == "final outcome")) |> 
    select(-c(result_size, outcome_type))
  
  large_nonfin <- data_outcomes |> 
    filter(result_size == "large" & (outcome_type == "Total: non-final outcome" | outcome_type == "non-final outcome")) |> 
    select(-c(result_size, outcome_type))
  
  large_noout <- data_outcomes |> 
    filter(result_size == "large" & outcome_type == "Total: no outcome recorded") |> 
    select(-c(result_size, outcome_type))
  
  small_tot <- data_outcomes |> 
    filter(result_size == "small" & outcome_type == "Total") |>
    select(-c(result_size, outcome_type))
  
  small_fin <- data_outcomes |> 
    filter(result_size == "small" & (outcome_type == "Total: final outcome" | outcome_type == "final outcome")) |> 
    select(-c(result_size, outcome_type))
  
  small_nonfin <- data_outcomes |> 
    filter(result_size == "small" & (outcome_type == "Total: non-final outcome" | outcome_type == "non-final outcome")) |> 
    select(-c(result_size, outcome_type))
  
  small_noout <- data_outcomes |> 
    filter(result_size == "small" & outcome_type == "Total: no outcome recorded") |> 
    select(-c(result_size, outcome_type))
  
  
  
  # notes and styles --------------------------------------------------------
  ## texts for writing in
  texts <- list()
  texts$head <- "Vascular referral outcome"
  texts$names <- paste0("Screened in year ending 31 March ", 
                        substr(names(data_outcomes |> select(-c(result_size, outcome_type, result_outcome))), 1, 2), 
                        substr(names(data_outcomes |> select(-c(result_size, outcome_type, result_outcome))), 6, 7)) # produces wrong output for cumulative - corrected below
  if(season_var == "spring") {
    texts$names[length(texts$names) - 1] <- paste0(texts$names[length(texts$names) - 1], {supsc("p")}) # adding provisional p
  }
  texts$names[length(texts$names)] <- paste0("Cumulative Screened to ", substr(texts$names[length(texts$names) - 1], 25, 37)) # correcting cumulative header
  texts$source <- "Source: Scottish AAA Call Recall System"
  texts$notes <- c("Notes", "1. Only referrals with AAA ≥ 5.5cm are included in the vascular KPIs (3.1 to 4.2).",
                   " ", " ", "-   Zero / Not applicable", " ", " ", "Return to Table of Contents")
  texts$p_note <- provisional_note

  
  # source styles
  source(here::here("code", "src", "Source_Excel_Styles.R"))
  
  # ref - rows for data to be written to 
  ref <- list()
  ref$head <- 4 # table headers
  ref$l_tot <- ref$head + 1 # large
  ref$l_fin <- ref$l_tot + nrow(large_tot) + 1
  ref$l_nonfin <- ref$l_fin + nrow(large_fin) + 1
  ref$l_noout <- ref$l_nonfin + nrow(large_nonfin) + 1
  ref$s_tot <- ref$l_noout + nrow(large_noout) + 1 # small
  ref$s_fin <- ref$s_tot + nrow(small_tot) + 1
  ref$s_nonfin <- ref$s_fin + nrow(small_fin) + 1
  ref$s_noout <- ref$s_nonfin + nrow(small_nonfin) + 1
  ref$src <- ref$s_noout + 1 # source
  ref$notes <- ref$src + 2 # notes
  
  
  # formatting cells --------------------------------------------------------
  
  # merging
  mergeCells(workbook, sheet_name, rows = ref$src, cols = (ncol(large_tot) - 2):(ncol(large_tot))) # source
  mergeCells(workbook, sheet_name, rows = ref$notes + 1, cols = 1:ncol(large_tot)) # notes
  mergeCells(workbook, sheet_name, rows = ref$notes + 2, cols = 1:ncol(large_tot))
  
  # col/row heights
  setColWidths(workbook, sheet_name, cols = 1, widths = 64)
  setColWidths(workbook, sheet_name, cols = 2:ncol(large_tot), widths = 14)
  setRowHeights(workbook, sheet_name, rows = ref$head, heights = 70)
  setRowHeights(workbook, sheet_name, rows = ref$l_tot:ref$src, heights = 15.5)
  setRowHeights(workbook, sheet_name, rows = ref$notes + 2, heights = 33)
  
  # gridlines
  showGridLines(workbook, sheet_name, showGridLines = F)
  
  # writing data ------------------------------------------------------------
  
  writeData(workbook, sheet_name, texts$head, startRow = ref$head, startCol = 1, colNames = F) # headers
  writeData(workbook, sheet_name, t(texts$names), startRow = ref$head, startCol = 2, colNames = F)
  writeData(workbook, sheet_name, large_tot, startRow = ref$l_tot, colNames = F) # large
  writeData(workbook, sheet_name, large_fin, startRow = ref$l_fin, colNames = F)
  writeData(workbook, sheet_name, large_nonfin, startRow = ref$l_nonfin, colNames = F)
  writeData(workbook, sheet_name, large_noout, startRow = ref$l_noout, colNames = F)
  writeData(workbook, sheet_name, small_tot, startRow = ref$s_tot, colNames = F) # small
  writeData(workbook, sheet_name, small_fin, startRow = ref$s_fin, colNames = F)
  writeData(workbook, sheet_name, small_nonfin, startRow = ref$s_nonfin, colNames = F)
  writeData(workbook, sheet_name, small_noout, startRow = ref$s_noout, colNames = F)
  writeData(workbook, sheet_name, texts$source, startRow = ref$src, startCol = (ncol(large_tot) - 2), colNames = F) # source
  writeData(workbook, sheet_name, texts$notes, startRow = ref$notes, colNames = F) # notes
  if(season_var == "spring") {
    writeData(workbook, sheet_name, texts$p_note, startRow = ref$notes + 2, colNames = F) # provisional note
  }
  
  # adding styles -----------------------------------------------------------
  
  # texts styles
  ## table
  addStyle(workbook, sheet_name, styles$black_12, 
           rows = ref$head:(ref$s_noout), cols = 1:ncol(large_tot), stack = T, gridExpand = T) # body
  addStyle(workbook, sheet_name, styles$black_bold_12, 
           rows = ref$l_tot, cols = 1:ncol(large_tot), stack = T, gridExpand = T) # l tot
  addStyle(workbook, sheet_name, styles$black_bold_12, 
           rows = ref$l_fin, cols = 1:ncol(large_tot), stack = T, gridExpand = T) # l fin
  addStyle(workbook, sheet_name, styles$black_bold_12, 
           rows = ref$l_nonfin, cols = 1:ncol(large_tot), stack = T, gridExpand = T) # l nonfin
  addStyle(workbook, sheet_name, styles$black_bold_12, 
           rows = ref$l_noout, cols = 1:ncol(large_tot), stack = T, gridExpand = T) # l noout
  addStyle(workbook, sheet_name, styles$black_bold_12, 
           rows = ref$s_tot, cols = 1:ncol(large_tot), stack = T, gridExpand = T) # s tot
  addStyle(workbook, sheet_name, styles$black_bold_12, 
           rows = ref$s_fin, cols = 1:ncol(large_tot), stack = T, gridExpand = T) # s fin
  addStyle(workbook, sheet_name, styles$black_bold_12, 
           rows = ref$s_nonfin, cols = 1:ncol(large_tot), stack = T, gridExpand = T) # s nonfin
  addStyle(workbook, sheet_name, styles$black_bold_12, 
           rows = ref$s_noout, cols = 1:ncol(large_tot), stack = T, gridExpand = T) # s noout
  addStyle(workbook, sheet_name, styles$black_11, 
           rows = ref$src, cols = (ncol(large_tot) - 2), stack = T, gridExpand = T) # source
  
  ## notes
  addStyle(workbook, sheet_name, styles$black_bold_11, 
           rows = ref$notes, cols = 1, stack = T, gridExpand = T) # head
  addStyle(workbook, sheet_name, styles$black_11, 
           rows = (ref$notes + 1):(ref$notes + 4), cols = 1, stack = T, gridExpand = T) # body
  addStyle(workbook, sheet_name, styles$blue_nowrap_underline_11,
           rows = (ref$notes + 7), cols = 1, stack = T, gridExpand = T)
  
  
  # alignment
  addStyle(workbook, sheet_name, styles$a_middle,
           rows = ref$head:ref$src, cols = 1:ncol(large_tot), stack = T, gridExpand = T) # vertical mid
  addStyle(workbook, sheet_name, styles$a_left,
           rows = ref$head:(ref$notes + 7), cols = 1, stack = T, gridExpand = T) # horizontal left
  addStyle(workbook, sheet_name, styles$a_centre,
           rows = ref$head, cols = 2:ncol(large_tot), stack = T, gridExpand = T) # horizontal middle
  addStyle(workbook, sheet_name, styles$a_right,
           rows = ref$src, cols = (ncol(large_tot) - 2), stack = T, gridExpand = T) # horizontal right

  # borders
  ## large table
  addStyle(workbook, sheet_name, styles$b_left_bold,
           rows = ref$head:ref$l_noout, cols = c(1, 2, ncol(large_tot), ncol(large_tot) + 1), stack = T, gridExpand = T) # left bold
  addStyle(workbook, sheet_name, styles$b_left,
           rows = ref$head:ref$l_noout, cols = 3:(ncol(large_tot) - 1), stack = T, gridExpand = T) # left
  addStyle(workbook, sheet_name, styles$b_top_bold,
           rows = c(ref$head, ref$l_noout + 1), cols = 1:ncol(large_tot), stack = T, gridExpand = T) # top bold
  addStyle(workbook, sheet_name, styles$b_top,
           rows = ref$l_tot, cols = 1:ncol(large_tot), stack = T, gridExpand = T) # top

  ## small table
  addStyle(workbook, sheet_name, styles$b_left_bold,
           rows = ref$s_tot:ref$s_noout, cols = c(1, 2, ncol(large_tot), ncol(large_tot) + 1), stack = T, gridExpand = T) # left bold
  addStyle(workbook, sheet_name, styles$b_left,
           rows = ref$s_tot:ref$s_noout, cols = 3:(ncol(large_tot) - 1), stack = T, gridExpand = T) # left
  addStyle(workbook, sheet_name, styles$b_top_bold,
           rows = c(ref$s_tot, ref$s_noout + 1), cols = 1:ncol(large_tot), stack = T, gridExpand = T) # top bold

  # number formats
  addStyle(workbook, sheet_name, styles$counts,
           rows = ref$l_tot:ref$s_noout, cols = 2:(ncol(large_tot) - 1), stack = T, gridExpand = T) # number format - 0s to '-'s

}


# GP outputs (autumn only) ------------------------------------------------

# function to write Excel output for both KPI 1.2a and Self-referrals
create_aaa_gp_outputs <- function(hb_name, financial_year, coverage_data, selfref_data, output_filepath, date_aaa_extracted, date_gp_extracted) {
  
  # 1. text and styles inputs ----
  
  ## text
  text <- list()
  
  ### KPI 1.2a/coverage - specific texts
  text$cov_title <- "KPI 1.2a: Percentage of eligible poulation who are tested before age 66 years 3 months by GP practice"
  text$cov_heads <- c("GP Code", "GP Practice Name", paste0("Turned 66 in year ending 31 March ", year_ww), paste0("Turned 66 in year ending 31 March ", year_xx))
  text$cov_subheads <- c("Men eligible", "Tested before age 66 years 3 months", "N", "N", "%")
  text$cov_key <- c("Key", "Essential threshold not met", "Essential ≥ 75%", "Desirable ≥ 85%")
  text$cov_notes <- data.frame(
    title = c("Notes", 
              "1. Number of men eligible:",
              "2. Tested before age 66 and 3 months: ",
              "3. GP practice: "),
    content = c("", 
                paste0("Men turning age 66 in the financial year ending 31 March ", year_xx, ". Men become eligible for screening when they reach age 65 and should be invited for screening before their 66th birthday."),
                paste0("Men are counted as having been tested if they were screened before they reached the age of 66 and 3 months and had a screening result of positive, negative or non-visualisation. Men who attended clinics and were not screened due to technical failure are not included."),
                paste0("Data are based on the GP practice the man was registered at on 31 March ", year_xx, " and so may not always reflect the practice the man was registered with when invited or screened. Men may attend a GP practice outside the NHS Board that they are resident in. These men are included in the 'Practice outside NHS Board area' figures. For a small number of men, GP practice of registration is unknown. GP practice data has been extracted from the AAA screening business objects data universe (at ", date_gp_extracted, " ", year_xx, ") and mapped to the PHS extract (from ", date_aaa_extracted, " ", year_xx, ").")))
  ### self-referrals - specific texts 
  text$sr_title <- "Self-referrals: Number of men tested by GP practice"
  text$sr_heads <- c("Practice Code", "Practice Name", paste0("Tested in year ending 31 March ", year_xx))
  text$sr_notes <- data.frame(
    title = c("Notes", 
              "1. Self-referral: ",
              "2. Men tested: ",
              "3. GP practice: ",
              "4. Multiple initial screens: "),
    content = c("",
                "Any man over the age of 65 who has not previously been screened and who contacts their local AAA screening centre directly to request screening.",
                "Men are counted as tested if they have an initial screening result of positive, negative or non-visualisation. Men who attended clinics and were not screened due to technical failure are not included.",
                paste0("Data for year ending 31 March ", year_xx, " are based on the GP practice that the individual was registered at on 31 March ", year_xx, ". This means the data may not always reflect the practice the man was registered with when invited or screened. Men may attend a GP practice outside the NHS Board that they are resident in. These men are included in the 'Practice outside NHS Board area' figures. For a small number of men, GP practice of registration is unknown. GP practice data has been extracted from the AAA screening business objects data universe (at ", date_gp_extracted, " ", year_xx, ") and mapped to the PHS extract (from ", date_aaa_extracted, " ", year_xx, ")."),
                "Self-referrals who had initial screens in more than one year are counted under the year of their most recent screening result only."))
  ### general text
  text$disclosure_text <- c("Practice-level data and disclosive cells", 
                            "These data are released for management purposes and have not been adjusted to conform to PHS’s Statistical Disclosure Control protocol. The tables may contain disclosive cells (cells with small numbers that might enable an individual patient or member of staff to be identified, perhaps with the aid of further knowledge of the topic) and should not be published without further consideration of the contents in light of the guidance (link below). Please contact phs.aaascreenstats@phs.scot if you have any queries regarding this.")
  text$disclosure_link <- format_excel_hyperlink("PHS Statistical Disclosure Control", "https://publichealthscotland.scot/publications/public-health-scotland-statistical-disclosure-protocol/public-health-scotland-statistical-disclosure-protocol-version-22/")
  text$legend <- c("-   Zero", "..   Not applicable")
  
  ## styles
  source(here::here("code", "src", "Source_Excel_Styles.R")) # generic styles
  
  styles_gp <- list() # styles specific to this script
  styles_gp$disclosure_head <- createStyle(fontSize = 12, fontName = "Arial", fontColour = "#FF0000", textDecoration = c("bold", "underline"), wrapText = F)
  ### conditional formatting
  styles_gp$red <- createStyle(fontSize = 12, fontName = "Arial", fontColour = "#FFFFFF", fgFill = "#911913")
  styles_gp$yellow <- createStyle(fontSize = 12, fontName = "Arial", fontColour = "#000000", fgFill = "#F7EC6D")
  styles_gp$green <- createStyle(fontSize = 12, fontName = "Arial", fontColour = "#000000", fgFill = "#6AB42D")
  styles_gp$red_cond <- createStyle(fontSize = 12, fontName = "Arial", fontColour = "#FFFFFF", bgFill = "#911913")
  styles_gp$yellow_cond <- createStyle(fontSize = 12, fontName = "Arial", fontColour = "#000000", bgFill = "#F7EC6D")
  styles_gp$green_cond <- createStyle(fontSize = 12, fontName = "Arial", fontColour = "#000000", bgFill = "#6AB42D")
  
  # 2. functions ----
  
  # AMc note: a function to create these individual functions would be useful for other projects where there are more than 2 sheets?
  
  ## coverage functions
  ### data writing
  write_cov_data <- function(data, row, col) {
    writeData(wb, "KPI 1.2a", x = data, startRow = row, startCol = col, colNames = F)
  }
  
  ### styling
  add_cov_style <- function(style, row, col) {
    addStyle(wb, "KPI 1.2a", style = style, rows = row, cols = col, stack = T, gridExpand = T)
  }
  
  ### merge cells
  merge_cov_cells <- function(row, col) {
    mergeCells(wb, "KPI 1.2a", rows = row, cols = col)
  }
  
  ### col widths
  set_cov_col_widths <- function(col, width) {
    setColWidths(wb, "KPI 1.2a", cols = col, widths = width)
  }
  
  ### row heights
  set_cov_row_heights <- function(row, height) {
    setRowHeights(wb, "KPI 1.2a", rows = row, heights = height)
  }
  
  ## self-referrals functions
  ### data writing
  write_sr_data <- function(data, row, col) {
    writeData(wb, "Self-referrals", x = data, startRow = row, startCol = col, colNames = F)
  }
  
  ### styling
  add_sr_style <- function(style, row, col) {
    addStyle(wb, "Self-referrals", style = style, rows = row, cols = col, stack = T, gridExpand = T)
  }
  
  ### merge cells
  merge_sr_cells <- function(row, col) {
    mergeCells(wb, "Self-referrals", rows = row, cols = col)
  }
  
  ### col widths
  set_sr_col_widths <- function(col, width) {
    setColWidths(wb, "Self-referrals", cols = col, widths = width)
  }
  
  ### row heights
  set_sr_row_heights <- function(row, height) {
    setRowHeights(wb, "Self-referrals", rows = row, heights = height)
  }
  
  
  # 3. data filtering ----
  ## KPI 1.2a data
  cov <- coverage_data |> 
    filter(hbres == hb_name) |> 
    select(!hbres) |> 
    mutate(across(contains("percent"), \(x) replace_na(x, -1000))) 
  # AMc note: this done to allow these NaN/NA percentages (where cohort= 0) to be recoded as ".." in the output
  # replacing empty cells with openxlsx is not possible unfortunately
  
  ## SR data
  sr <- selfref_data |> 
    filter(hbres == hb_name) |> 
    select(!hbres)
  
  
  # 4: row numbers ----
  
  # list of row numbers for where major parts of workbook are located - means that adapting later on is made easier
  
  row_refs <- list()
  # coverage
  row_refs$cov_title <- 1 # title row
  row_refs$cov_heads <- 3 # headings row (usually +2 from title)
  row_refs$cov_subheads <- row_refs$cov_heads + 1
  row_refs$cov_length <- nrow(cov)
  row_refs$cov_start <- 6 # data start row (can be calculated from headings etc if necessary)
  row_refs$cov_end <- row_refs$cov_start + row_refs$cov_length - 1
  row_refs$cov_key_start <- row_refs$cov_end + 2
  row_refs$cov_legend_start <- row_refs$cov_end + 6
  row_refs$cov_disc_start <- row_refs$cov_end + 10
  row_refs$cov_notes_start <- row_refs$cov_end + 14
  # sr
  row_refs$sr_title <- 1 # title row
  row_refs$sr_heads <- 3 # headings row (usually +2 from title)
  row_refs$sr_length <- nrow(sr)
  row_refs$sr_start <- 4 # data start row (can be calculated from headings etc if necessary)
  row_refs$sr_end <- row_refs$sr_start + row_refs$sr_length -1
  row_refs$sr_legend_start <- row_refs$sr_end + 2
  row_refs$sr_disc_start <- row_refs$sr_end + 6
  row_refs$sr_notes_start <- row_refs$sr_end + 10
  
  
  # 4. writing Excel ----
  ## create workbook ----
  wb <- createWorkbook()
  addWorksheet(wb, sheetName = "KPI 1.2a")
  addWorksheet(wb, sheetName = "Self-referrals")
  
  ## KPI/Coverage workbook ----
  
  ### merging cells
  merge_cov_cells(row = row_refs$cov_title, col = 1:10) # title
  merge_cov_cells(row = (row_refs$cov_heads):(row_refs$cov_heads+2), col = 1) # subheads
  merge_cov_cells(row = (row_refs$cov_heads):(row_refs$cov_heads+2), col = 2)
  merge_cov_cells(row = row_refs$cov_heads, col = 3:5) 
  merge_cov_cells(row = row_refs$cov_heads, col = 6:ncol(cov))
  merge_cov_cells(row = row_refs$cov_heads+1, col = 4:5) # sub subheads
  merge_cov_cells(row = row_refs$cov_heads+1, col = 7:ncol(cov))
  merge_cov_cells(row = row_refs$cov_disc_start, col = 1:2) # disclosure head
  merge_cov_cells(row = row_refs$cov_disc_start+1, col = 1:ncol(cov)) 
  merge_cov_cells(row = row_refs$cov_disc_start+2, col = 1:2)
  merge_cov_cells(row = row_refs$cov_notes_start+1, col = 2:ncol(cov)) # notes
  merge_cov_cells(row = row_refs$cov_notes_start+2, col = 2:ncol(cov))
  merge_cov_cells(row = row_refs$cov_notes_start+3, col = 2:ncol(cov))
  
  
  ### data: headers
  write_cov_data(text$cov_title, row = row_refs$cov_title, col = 1) # title
  write_cov_data(t(text$cov_heads[1:3]), row = row_refs$cov_heads, col = 1) # subheads
  write_cov_data(text$cov_heads[4], row = row_refs$cov_heads, col = 6)
  write_cov_data(t(text$cov_subheads[1:2]), row = row_refs$cov_heads+1, col = 3) # sub subheads
  write_cov_data(t(text$cov_subheads[1:2]), row = row_refs$cov_heads+1, col = 6)
  write_cov_data(t(text$cov_subheads[3:5]), row = row_refs$cov_heads+2, col = 3)
  write_cov_data(t(text$cov_subheads[3:5]), row = row_refs$cov_heads+2, col = 6)
  
  ### data: dataset
  write_cov_data(cov, row = row_refs$cov_start, col = 1)
  
  ### data: notes etc
  write_cov_data(text$cov_key, row = row_refs$cov_key_start, col= 1) # key
  write_cov_data(text$legend, row = row_refs$cov_legend_start, col = 1) # legend
  write_cov_data(text$disclosure_text, row = row_refs$cov_disc_start, col = 1) # disclosure head
  write_cov_data(text$disclosure_link, row = row_refs$cov_disc_start+2, col = 1)
  write_cov_data(text$cov_notes, row = row_refs$cov_notes_start, col = 1) # notes head
  
  
  ### styles: font
  add_cov_style(styles$black_bold_nowrap_18, row = row_refs$cov_title, col = 1) # title
  add_cov_style(styles$black_12, row = (row_refs$cov_heads):(row_refs$cov_notes_start+3), col = (1:ncol(cov))) # entire worksheet minus title
  add_cov_style(styles$black_bold_12, row = row_refs$cov_start, col = 1:ncol(cov)) # total HB row
  add_cov_style(styles$black_bold_12, row = c(row_refs$cov_key_start, (row_refs$cov_notes_start):(row_refs$cov_notes_start+3)), col = 1) # key and notes
  add_cov_style(styles_gp$disclosure_head, row = row_refs$cov_disc_start, col = 1) # disclosure header
  add_cov_style(styles$red_bold_12, row = row_refs$cov_disc_start+1, col = 1) # disclosure body
  
  ### styles: border
  #### top
  # AMc note: when I tried to use c() for row and col here, it produced a corrupt output, hence doing each one individually
  add_cov_style(styles$b_top, row = row_refs$cov_heads, col = 1:ncol(cov)) # data
  add_cov_style(styles$b_top, row = row_refs$cov_subheads, col = 3:ncol(cov))
  add_cov_style(styles$b_top, row = row_refs$cov_subheads +1, col = 3:ncol(cov))
  add_cov_style(styles$b_top, row = row_refs$cov_start, col = 1:ncol(cov))
  add_cov_style(styles$b_top, row = row_refs$cov_end+1, col = 1:ncol(cov))
  add_cov_style(styles$b_top, row = row_refs$cov_notes_start+1, col = 1:ncol(cov)) # notes
  add_cov_style(styles$b_top, row = row_refs$cov_notes_start+2, col = 1:ncol(cov))
  add_cov_style(styles$b_top, row = row_refs$cov_notes_start+3, col = 1:ncol(cov))
  add_cov_style(styles$b_top, row = row_refs$cov_notes_start+4, col = 1:ncol(cov))
  #### left
  add_cov_style(styles$b_left, row = (row_refs$cov_heads):(row_refs$cov_end), col = 1:(ncol(cov)+1)) # data
  add_cov_style(styles$b_left, row = (row_refs$cov_notes_start+1):(row_refs$cov_notes_start+3), col = c(1, (ncol(cov)+1))) # notes
  
  ### styles: text alignment
  add_cov_style(styles$a_centre, row = (row_refs$cov_heads):(row_refs$cov_subheads), col = 3:ncol(cov)) # head/subhead
  add_cov_style(styles$a_right, row = (row_refs$cov_subheads+1), col = 3:ncol(cov)) # sub subhead
  add_cov_style(styles$a_middle, row = row_refs$cov_disc_start+1, col = 1) # disclosure
  add_cov_style(styles$a_middle, row = (row_refs$cov_notes_start+1):(row_refs$cov_notes_start+4), col = 1:2) # notes
  
  ### styles: background fill
  add_cov_style(styles_gp$red, row = row_refs$cov_key_start+1, col = 1) # key
  add_cov_style(styles_gp$yellow, row= row_refs$cov_key_start+2, col = 1)
  add_cov_style(styles_gp$green, row= row_refs$cov_key_start+3, col = 1)
  
  ### styles: number formatting
  add_cov_style(styles$count, row = (row_refs$cov_start):(row_refs$cov_end), col = 3:4) # counts
  add_cov_style(styles$count, row = (row_refs$cov_start):(row_refs$cov_end), col = 6:7)
  add_cov_style(styles$percentages, row = (row_refs$cov_start):(row_refs$cov_end), col = 5) # percentages
  add_cov_style(styles$percentages, row = (row_refs$cov_start):(row_refs$cov_end), col = 8)
  
  ### conditional formatting
  
  conditionalFormatting(wb, "KPI 1.2a", rows = (row_refs$cov_start):(row_refs$cov_end), cols = 5, # essential not met
                        type = "between", rule = c(0, 74.9), style = styles_gp$red_cond)             
  conditionalFormatting(wb, "KPI 1.2a", rows = (row_refs$cov_start):(row_refs$cov_end), cols = 8,
                        type = "between", rule = c(0, 74.9), style = styles_gp$red_cond)
  conditionalFormatting(wb, "KPI 1.2a", rows = (row_refs$cov_start):(row_refs$cov_end), cols = 5, # essential
                        type = "between", rule = c(75, 84.9), style = styles_gp$yellow_cond)         
  conditionalFormatting(wb, "KPI 1.2a", rows = (row_refs$cov_start):(row_refs$cov_end), cols = 8,
                        type = "between", rule = c(75, 84.9), style = styles_gp$yellow_cond)
  conditionalFormatting(wb, "KPI 1.2a", rows = (row_refs$cov_start):(row_refs$cov_end), cols = 5, # desirable
                        type = "between", rule = c(85, 100), style = styles_gp$green_cond)
  conditionalFormatting(wb, "KPI 1.2a", rows = (row_refs$cov_start):(row_refs$cov_end), cols = 8,
                        type = "between", rule = c(85, 100), style = styles_gp$green_cond)
  
  ### worksheet formatting
  showGridLines(wb, "KPI 1.2a", showGridLines = F)
  #### col widths
  set_cov_col_widths(col = 1, width = 30)
  set_cov_col_widths(col = 2, width = 45)
  set_cov_col_widths(col = 3:8, width = 15)
  #### row heights
  set_cov_row_heights(row = row_refs$cov_subheads, height = 33) # subheadings
  set_cov_row_heights(row = row_refs$cov_disc_start+1, height = 70) # disclosure
  set_cov_row_heights(row = row_refs$cov_notes_start+1, height = 45) # notes
  set_cov_row_heights(row = row_refs$cov_notes_start+2, height = 38)
  set_cov_row_heights(row = row_refs$cov_notes_start+3, height = 80)
  
  
  
  
  ## Self-referrals workbook ----
  
  ### merging cells
  merge_sr_cells(row = row_refs$sr_title, col = 1:ncol(sr)) # title
  merge_sr_cells(row = row_refs$sr_disc_start, col = 1:ncol(sr)) # disclosure head
  merge_sr_cells(row = row_refs$sr_disc_start+1, col = 1:ncol(sr))
  merge_sr_cells(row = row_refs$sr_disc_start+2, col = 1:ncol(sr))
  merge_sr_cells(row = row_refs$sr_notes_start+1, col = 2:ncol(sr)) # notes
  merge_sr_cells(row = row_refs$sr_notes_start+2, col = 2:ncol(sr))
  merge_sr_cells(row = row_refs$sr_notes_start+3, col = 2:ncol(sr))
  merge_sr_cells(row = row_refs$sr_notes_start+4, col = 2:ncol(sr))
  
  ### data: headers
  write_sr_data(text$sr_title, row = row_refs$sr_title, col = 1) # title
  write_sr_data(t(text$sr_heads), row = row_refs$sr_heads, col = 1) # headers
  
  ### data: dataset
  write_sr_data(sr, row = row_refs$sr_start, col = 1)
  
  ### data: notes etc
  write_sr_data(text$legend, row = row_refs$sr_legend_start, col = 1) # legend
  write_sr_data(text$disclosure_text, row = row_refs$sr_disc_start, col = 1) # disclosure text
  write_sr_data(text$disclosure_link, row = row_refs$sr_disc_start+2, col = 1) # disclosure hyperlink
  write_sr_data(text$sr_notes, row = row_refs$sr_notes_start, col = 1) # notes
  
  ### styles: font
  add_sr_style(styles$black_bold_nowrap_18, row = row_refs$sr_title, col = 1) # title
  add_sr_style(styles$black_12, row = (row_refs$sr_heads):(row_refs$cov_key_start+5), col = 1:ncol(sr)) # entire worksheet minus title
  add_sr_style(styles$black_bold_12, row = row_refs$sr_start, col = 1:ncol(sr)) # hb totals
  add_sr_style(styles_gp$disclosure_head, row = row_refs$sr_disc_start, col = 1) # disclosure head
  add_sr_style(styles$red_bold_12, row = row_refs$sr_disc_start+1, col = 1) # disclosure body
  add_sr_style(styles$black_bold_12, row = (row_refs$sr_notes_start):(row_refs$sr_notes_start+4), col = 1) # notes
  
  ### styles: border
  #### top
  # AMc note: when I tried to use c() for row and col here, it produced a corrupt output, hence doing each one individually
  add_sr_style(styles$b_top, row = row_refs$sr_heads, col = 1:ncol(sr)) # data
  add_sr_style(styles$b_top, row = row_refs$sr_heads+1, col = 1:ncol(sr))
  add_sr_style(styles$b_top, row = row_refs$sr_end+1, col = 1:ncol(sr))
  add_sr_style(styles$b_top, row = row_refs$sr_notes_start+1, col = 1:ncol(sr)) # notes
  add_sr_style(styles$b_top, row = row_refs$sr_notes_start+2, col = 1:ncol(sr))
  add_sr_style(styles$b_top, row = row_refs$sr_notes_start+3, col = 1:ncol(sr))
  add_sr_style(styles$b_top, row = row_refs$sr_notes_start+4, col = 1:ncol(sr))
  add_sr_style(styles$b_top, row = row_refs$sr_notes_start+5, col = 1:ncol(sr))
  #### left
  add_sr_style(styles$b_left, row = (row_refs$sr_heads):(row_refs$sr_end), col = 1:(ncol(sr)+1)) # data
  add_sr_style(styles$b_left, row = (row_refs$sr_notes_start+1):(row_refs$sr_notes_start+4), col = c(1, ncol(sr)+1)) # notes
  
  ### styles: text alignment
  add_sr_style(styles$a_centre, row = row_refs$sr_heads, col = ncol(sr)) # heads
  add_sr_style(styles$a_middle, row = row_refs$sr_disc_start+1, col = 1) # disclosure
  add_sr_style(styles$a_middle, row = (row_refs$sr_notes_start+1):(row_refs$sr_notes_start+4), col = 1:2) # notes
  
  ### styles: number formatting
  add_sr_style(styles$counts, row = (row_refs$sr_start):(row_refs$sr_end), col = ncol(sr)) # counts
  
  ### worksheet formatting
  showGridLines(wb, "Self-referrals", showGridLines = F)
  #### col widths
  set_sr_col_widths(col = 1, width = 27)
  set_sr_col_widths(col = 2, width = 50)
  set_sr_col_widths(col = 3, width = 15)
  #### row heights
  set_sr_row_heights(row = row_refs$sr_heads, height = 50) # header
  set_sr_row_heights(row = row_refs$sr_disc_start+1, height = 112)# notes
  set_sr_row_heights(row = row_refs$sr_notes_start+1, height = 50)
  set_sr_row_heights(row = row_refs$sr_notes_start+2, height = 50)
  set_sr_row_heights(row = row_refs$sr_notes_start+3, height = 170)
  set_sr_row_heights(row = row_refs$sr_notes_start+4, height = 40)
  
  # 5: Write output ----
  output <- paste0(output_filepath, "/GP Practices/NHS ", hb_name, " Practice level ", simplify_fy(financial_year), ".xlsx")
  saveWorkbook(wb, output, overwrite = T)
  print(paste0("Output saved for NHS ", hb_name))
}


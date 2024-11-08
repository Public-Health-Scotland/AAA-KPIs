# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Source_Excel_Styles.R
# 
# Aoife McCarthy
# July 2024
# 
# Create openxlsx styles to be used in Write_Excel scripts
# 
# Written/run on Posit WB, R 4.1.2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


library(openxlsx)

styles <- list()

# bold black size 18, no wrap
styles$black_bold_18 <- createStyle(fontSize = 18, fontColour = "#000000",
                                    fontName = "Arial", textDecoration = "bold")

# bold black size 14, wrapped
styles$black_bold_14 <- createStyle(fontSize = 14, fontColour = "#000000",
                             fontName = "Arial", textDecoration = "bold",
                             wrapText = TRUE)
# bold black size 14, no wrap
styles$black_bold_nowrap_14 <- createStyle(fontSize = 14, fontColour = "#000000",
                                    fontName = "Arial", textDecoration = "bold")
# bold black 12, wrapped
styles$black_bold_12 <- createStyle(fontSize = 12, fontColour = "#000000",
                             fontName = "Arial", textDecoration = "bold",
                             wrapText = TRUE)
# bold black 12, no wrap
styles$black_bold_nowrap_12 <- createStyle(fontSize = 12, fontColour = "#000000",
                                    fontName = "Arial", textDecoration = "bold")
# black 12, wrapped
styles$black_12 <- createStyle(fontSize = 12, fontColour = "#000000",
                        fontName = "Arial", wrapText = TRUE)
# black centered 12, wrapped
styles$black_centre_12 <- createStyle(fontSize = 12, fontColour = "#000000",
                               fontName = "Arial", wrapText = TRUE,
                               halign = "center", valign = "center")
# black without wrapping 12
styles$black_nowrap_12 <- createStyle(fontSize = 12, fontColour = "#000000",
                               fontName = "Arial")
# black complete border 12, wrapped
styles$black_border_12 <- createStyle(fontSize = 12, fontName = "Arial",
                               fontColour = "#000000", border = "TopBottomLeftRight",
                               wrapText = TRUE, halign = "left", valign = "center")
# black complete medium border centred 12, wrapped
styles$black_border_centre_12 <- createStyle(fontSize = 12, fontName = "Arial",
                                      fontColour = "#000000", border = "TopBottomLeftRight",
                                      wrapText = TRUE, halign = "center", valign = "center",
                                      borderStyle = "medium")
# black complete thin border centred 12, wrapped
styles$black_border_thin_centre_12 <- createStyle(fontSize = 12, fontName = "Arial",
                                           fontColour = "#000000", border = "TopBottomLeftRight",
                                           wrapText = TRUE, halign = "center", valign = "center",
                                           borderStyle = "thin")
# bold black 11, wrapped
styles$black_bold_11 <- createStyle(fontSize = 11, fontColour = "#000000", textDecoration = "bold",
                                    fontName = "Arial", wrapText = TRUE)
# black 11, wrapped
styles$black_11 <- createStyle(fontSize = 11, fontColour = "#000000",
                        fontName = "Arial", wrapText = TRUE)
# black without wrapping 11
styles$black_nowrap_11 <- createStyle(fontSize = 11, fontColour = "#000000",
                               fontName = "Arial")
# black complete border 11, no wrap
styles$black_border_11 <- createStyle(fontSize = 11, fontName = "Arial",
                                      fontColour = "#000000", border = "TopBottomLeftRight",
                                      halign = "center", valign = "bottom")

# orange size 11, wrapped
styles$orange_11 <- createStyle(fontSize = 11, fontName = "Arial", 
                                fontColour = "#ff9f00", wrapText = TRUE)
# bold red 12, wrapped 
styles$red_bold_12 <- createStyle(fontSize = 12, fontColour = "#FF0000", 
                                  fontName = "Arial", textDecoration = c("bold"),
                                  wrapText = TRUE)
# bright blue centered 12, wrapped
styles$blue_border_centre_12 <- createStyle(fontSize = 12, fontName = "Arial", 
                                            fontColour = "#0000FF", wrapText = TRUE,
                                            halign = "center", border = "TopBottomLeftRight")
# bright blue bordered centred underlined 12, wrapped
styles$blue_border_underline_12 <- createStyle(fontSize = 12, fontName = "Arial",
                                               fontColour = "#0000FF", border = "TopBottomLeftRight",
                                               textDecoration = "underline", wrapText = TRUE, 
                                                halign = "left", valign = "center")
# blue 11 underlined, no wrap
styles$blue_nowrap_underline_11 <- createStyle(fontSize = 11, fontName = "Arial",
                                               fontColour = "#0000FF", textDecoration = "underline",
                                               halign = "left", valign = "center")
    
# summary header white font
styles$white_centre_12 <- createStyle(fontSize = 12, fontName = "Arial", fgFill = "#462682",
                               fontColour = "#ffffff", wrapText = TRUE,
                               border = c("top", "bottom", "left", "right"),
                               borderStyle = "medium", halign = "center", valign = "bottom",
                               textDecoration = "bold")
# write excel 5 - report type style
styles$report_type_style <- eval_seasonal_diff(
  season,
  {createStyle(fontSize = 12, fontName = "Arial",
               textDecoration = "bold", fontColour = "#FF0000")}, # spring
  {createStyle(fontSize = 12, fontName = "Arial",
               textDecoration = "bold", fontColour = "#000000")} # autumn
)

# rolling font used in theme 1 workbook
styles$rolling_font <- createStyle(fontSize = 12, fontName = "Arial",
                                   textDecoration = "bold", fontColour = "#000000",
                                   wrapText = TRUE, border = c("top", "bottom", "left", "right"),
                            borderStyle = "medium", halign = "center", valign = "center")

# general styles ----------------------------------------------------------

### borders
styles$b_left <- createStyle(border = "left")
styles$b_top <- createStyle(border = "top")
styles$b_left_bold <- createStyle(border = "left", borderStyle = "medium")
styles$b_top_bold <- createStyle(border = "top", borderStyle = "medium")
### aligning
styles$a_middle <- createStyle(valign = "center")
styles$a_right <- createStyle(halign = "right")
styles$a_centre <- createStyle(halign = "center")
styles$a_left <- createStyle(halign = "left")


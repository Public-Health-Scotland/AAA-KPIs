# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# x_master.R
#
# Aoife McCarthy
# 2024/09/26
#
# Master script to run entire analysis pipeline up until Excel writing
# only really run if certain that checks etc will be same as when manually run previously
#
# Written/run on PWB, R version 4.1.2
#
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# housekeeping ------------------------------------------------------------

library(here)
library(dplyr)
library(tidylog)
library(tidytable)

rm(list = ls())
gc()

source_file_from_subdir <- function(subdir, file_name) {
  
  rel_path <- paste0(subdir, "/", file_name)
  
  source(here(rel_path))
}


# list files to source ----------------------------------------------------

files_all <- list.files(here("code/")) |> as_tibble() |> pivot_wider(names_from = value)

files <- files_all |> 
  select(!contains(c("x", "scrap", "write"))) |> # removes one-time, Excel, and scrap codes
  pivot_longer(cols = everything()) |> 
  select(-name) |> 
  pull()

files <- paste0("code/", files)


    

# source all files --------------------------------------------------------

for(x in files) {
  
  print(paste0("code",))
  
  source_file_from_subdir("code", x)
}


# AMC -- THIS DOESN'T WORK BECAUSE ALL THE OTHER FILES DO RM ALL

source(here(files[1]))
source(here(files[2]))
source(here(files[3]))
source(here(files[4]))
source(here(files[5]))
source(here(files[6]))
source(here(files[7]))
source(here(files[8]))
source(here(files[9]))
source(here(files[10]))
source(here(files[11]))

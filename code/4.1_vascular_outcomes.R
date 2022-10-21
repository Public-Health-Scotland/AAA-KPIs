##########################################################
# 4.1_vascular_outcomes.R
# Karen Hotopp
# 21/10/2022
# Script 1 of ?
# 
# Translation of SPSS file '1. Vascular outcomes recorded.sps'
# Part of Theme 4 for AAA KPIs
# Takes the processed BOXI extracts and creates tables detailing
# vascular surgery outcomes
# 
# Written/run on R Studio Server
# R version 3.6.1
##########################################################

## Starting this as have been asked to show Vascular KPIs background information
## by HB for AAA BCG meeting (24Oct2022). May have useful bits for further 
## translating of SPSS gile, but started just to get HB info.


#### 1: Housekeeping ####
## Packages
library(here)
library(dplyr)
library(magrittr)
#library(stringr)
#library(forcats)
library(readr)
library(tidylog)


rm(list = ls())


## Values
year <- 2022
month <- "09"


## Pathways
wd_path <-paste0("/PHI_conf/AAA/Topics/Screening/extracts",
                 "/", year, month)

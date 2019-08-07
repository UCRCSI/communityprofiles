# This is the global.R code (that affects ui.R and server.R) for state_of_work
library(shiny)
library(tidyverse)
library(DT)
library(readr)
library(readxl)
# install.packages("writexl")
library(writexl)
library(openxlsx)
options(shiny.sanitize.errors = FALSE)

labels <- read_rds("dta/group_lookup.rds")
labels <- labels %>% 
  arrange(code)
group_labels <- labels$group

# excel_style <- function(){
#   addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "$ #,##0"), rows = 9, cols = 2)
#   addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "$ #,##0"), rows = 9, cols = 3)
#   addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "$ #,##0"), rows = 9, cols = 4)
#   addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "$ #,##0"), rows = 9, cols = 5)
#   addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "$ #,##0"), rows = 9, cols = 6)
#   
#   addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "#,##0"), rows = 10:18, cols = 2)
#   addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "#,##0"), rows = 10:18, cols = 3)
#   addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "#,##0"), rows = 10:18, cols = 4)
#   addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "#,##0"), rows = 10:18, cols = 5)
#   addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "#,##0"), rows = 10:18, cols = 6)
#   
#   addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "0.0 %"), rows = 19:42, cols = 2)
#   addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "0.0 %"), rows = 19:42, cols = 3)
#   addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "0.0 %"), rows = 19:42, cols = 4)
#   addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "0.0 %"), rows = 19:42, cols = 5)
#   addStyle(wb = wb, sheet = "data", style = createStyle(numFmt = "0.0 %"), rows = 19:42, cols = 6)
# }
# geo_labels <- read_rds("dta/geo_labels.rds")

# dta_groupnames <- read_rds("dta/groupname_lookup.rds")
# dta_groupnames <- dta_groupnames %>% rename(value = variable,
#                                             label=group)

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
  arrange(desc(variable))
group_labels <- labels$group
# geo_labels <- read_rds("dta/geo_labels.rds")

# dta_groupnames <- read_rds("dta/groupname_lookup.rds")
# dta_groupnames <- dta_groupnames %>% rename(value = variable,
#                                             label=group)

library(tidyverse)
library(readxl)
library(stringr)
library(data.table)
library(scales)

# test for removing est if moe > 25% * est --------------------------------

#1. pull a sample data
# df <- dta %>%
#   select(est_vc03, moe_vc03) %>%
#   rename(estimate = est_vc03,
#          moe = moe_vc03)

#2 function
MOE_count <- function(df){

  df <- df %>%
    mutate(moe = as.numeric(moe),
           moe = case_when(
             is.na(moe)==T ~0,
             TRUE ~moe),
           estimate =case_when(
             moe <= .25 * estimate ~ round(estimate),
             is.na(moe) == T ~ round(estimate),
             TRUE ~ NA_real_),
           estimate = scales::comma(estimate)) %>%
  select(-moe)

}

MOE_income <- function(df){

  df <- df %>%
    mutate(moe = as.numeric(moe),
           moe = case_when(
             is.na(moe)==T ~0,
             TRUE ~moe),
           estimate =case_when(
             moe <= .25 * estimate ~ round(estimate),
             is.na(moe) == T ~ round(estimate),
             TRUE ~ NA_real_),
           estimate = scales::dollar(estimate)) %>%
    select(-moe)

}

#3 test
# df2 <- MOE_count(df)

# Test for est range if it is a proportion --------------------------------

#1. pull a sample data
# df <- dta %>%
#   select(est_vc12, moe_vc12) %>%
#   rename(estimate = est_vc12,
#          moe = moe_vc12)

#2 function
MOE_prop <- function(df){
 df <- df %>%
    mutate(moe = round(as.numeric(moe), digits = 1),
      #calculate the proportion and format with %
      estimate = estimate / 100,
      estimate = scales::percent(estimate, 1),
      range = paste("<small>", "+/-", moe, "</small>", sep=""),
      range = case_when(
        is.na(moe)==T ~paste("<small>", "*****", "</small>", sep=""),
        TRUE ~range),
      value_final = paste(estimate, range, sep = "<br>")) %>%
      #if MOE = ***** then range = estimate and the value final will not have CI
      # value_final = case_when(
      #        is.na(moe)==T ~paste(estimate, "<small>", "*****", ")</small>", sep = ""),
      #        TRUE ~value_final)) %>%
    #deselect not useful columns
    select(-range, -moe, -estimate) %>%
    rename(estimate = value_final)
}


#3 test
# df2 <- MOE_prop(df)


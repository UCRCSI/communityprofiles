#### ONLY SETTING THIS UP IN THE EVENT WE HAVE TO
####
library(tidyverse)
library(tidycensus)
label <- load_variables(2017,"acs5", cache = TRUE) %>%
  select(-concept) %>%
  mutate(name = sub('E$', '',name), # get rid of E at the end of each name
         label = sub('Estimate!!', '',label)) %>%
  rename(variable = name)

detail_alone_asn <- get_acs(geography = "state",table = "B02015",year = 2017,survey = "acs5")
detail_alone_nhpi <- get_acs(geography = "state",table = "B02016",year = 2017,survey = "acs5")
detail_combo_asn <- get_acs(geography = "state",table = "B02018",year = 2017,survey = "acs5")
detail_combo_nhpi <- get_acs(geography = "state",table = "B02019",year = 2017,survey = "acs5")

detail_all <- rbind(detail_alone_asn,detail_alone_nhpi,detail_combo_asn,detail_combo_nhpi)

detail_all %>%
  left_join(label) %>%
  filter(!variable %in% c("B02015_001","B02016_001","B02018_001","B02019_001")) %>%
  mutate(label = str_remove(label,pattern = "Total!!")) %>%
  mutate(label = str_remove(label,pattern = "Total Groups Tallied!!")) %>%
  mutate(label = case_when(str_detect(variable, "B02015") ~ paste(label,"Alone", sep=" "),
                           str_detect(variable, "B02016") ~ paste(label,"Alone", sep=" "),
                           str_detect(variable, "B02018") ~ paste(label,"Combo", sep=" "),
                           str_detect(variable, "B02019") ~ paste(label,"Combo", sep=" "))) %>%
  select(-GEOID, -variable) %>%
  mutate(estimate = case_when(moe > (.25*estimate)~ NA_real_,
                              TRUE ~ estimate))





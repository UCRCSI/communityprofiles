library(shiny)
library(tidyverse)
library(DT)
dta_geonames <- read_rds("dta/geoname_lookup.rds")
dta_geonames <- dta_geonames %>% rename(value = GEOID,
                                        label=NAME)

geo_labels <- dta_geonames %>% pull(value)
names(geo_labels) <- dta_geonames$label


dta_groupnames <- read_rds("dta/groupname_lookup.rds")
dta_groupnames <- dta_groupnames %>% rename(value = variable,
                                            label=group)
group_labels <- dta_groupnames %>% pull(value)
names(group_labels) <- dta_groupnames$label

dta <- read_rds("dta/cleaned_totalpop.rds")

names(group_labels)[group_labels == "B02018_015"]

group_labels =="B02018_015"


dta %>%
  filter(GEOID =="06") %>%
  filter(variable %in% c("B02001_004","B02001_001","B02001_005","B02018_007","B03001_004")) %>%
  left_join(dta_groupnames, by = c("variable"="value")) %>%
  select(label,estimate, moe) %>%
  mutate(estimatenew = case_when( moe >= (.25*estimate) ~ "-",
                                  is.na(moe) ~ as.character(scales::comma(estimate)),
                                  TRUE ~ as.character(scales::comma(estimate)))) %>%
  mutate(moeNew = case_when( moe >= (.25 * estimate) ~ "-",
                             is.na(moe) ~ "-",
                             TRUE ~ as.character(scales::comma(moe)))) %>%
  select(-moe,-estimate) %>%
  mutate(topic = "Total Population") %>%
  mutate(estimatenew = paste(estimatenew, "<br>+/-",moeNew)) %>%
  select(-moeNew) %>%
  spread(label, estimatenew)


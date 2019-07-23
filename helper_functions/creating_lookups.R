library(shiny)
library(tidyverse)
library(DT)

dta_geonames <- read_rds("dta/geoname_lookup.rds")
dta_geonames <- dta_geonames %>% rename(value = GEOID,
                                        label=NAME)
dta_geonames <- dta_geonames %>% mutate(label = str_replace(label,"Congressional District","CD"))
geo_labels <- dta_geonames %>% pull(value)
names(geo_labels) <- dta_geonames$label

geo_labels %>% saveRDS("dta/geo_labels.rds")

dta_groupnames <- read_rds("dta/groupname_lookup.rds")
dta_groupnames <- dta_groupnames %>% rename(value = variable,
                                            label=group)
group_labels <- dta_groupnames %>% pull(value)
names(group_labels) <- dta_groupnames$label


group_labels %>% saveRDS("dta/group_labels.rds")

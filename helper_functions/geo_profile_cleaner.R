library(tidyverse)
library(readxl)
dta <- read_csv("dta/ACS_17_1YR_S0201_raceonly.csv",
                col_types = cols(GEO.id = "c",
                                 GEO.id2 = "c",
                                 POPGROUP.id = "c"))
dta <- dta %>% mutate(geo_length = str_length(GEO.id))

pop_lookup <- read_excel("dta/pop_lookup_race.xlsx")
pop_lookup$popgroup_id <- as.character(pop_lookup$popgroup_id)

# drop_chinese <- c(35,16)
dta <- janitor::clean_names(dta)
dta <- dta %>%
  # filter(!popgroup_id %in% drop_chinese) %>%
  mutate(geo_type = case_when(geo_length == 11 ~ "State",
                              geo_length == 9 ~ "US",
                              geo_length == 13 ~ "Congressional District",
                              geo_length == 14 ~ "County")) %>%
    left_join(pop_lookup)
dta <- dta %>%
  select(geo_id, geo_id2,geo_type,geo_display_label, popgroup_id,starts_with("group_"),everything()) %>%
  select(-popgroup_display_label)
dta <- dta %>% mutate(geo_display_label = str_replace(geo_display_label, "Congressional District","CD"))
dta %>% colnames()
dta %>% write_rds("dta/geo_profile_dta_acs1yr.rds")

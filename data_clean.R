library(tidyverse)
library(readr)

# data clean --------------------------------------------------------------

raw <- read_csv("dta/community_profile_raw.csv")
final <- raw %>% 
  select(-variable, -code) %>% 
  write_rds("dta/final_dta.rds")









#generate lookup table
group_lookup <- raw %>% 
  select(group, variable, code)
#save lookup table
saveRDS(group_lookup, "dta/group_lookup.rds")
#cleanup the data
dta <- raw %>% 
  filter(variable == "aapi_alone" | variable == "detailed_hisp") %>% 
  select(-group) %>% 
  gather(label, estimate, -variable, -code)

#Cutting the dataset by topics
poverty <- dta %>% 
  filter(label %in% c("pov", "kpov", "spov"))
saveRDS(poverty, "dta/cleaned_poverty.rds")

cvap <- dta %>%
  filter(label == "cvap")
saveRDS(cvap, "dta/cleaned_cvap.rds")

age <- dta %>% 
# poverty -----------------------------------------------------------------
#pull raw data
dta1 <- read_csv("/Users/sunnyshao/Dropbox/AAPIData HQ/Other Sites/Racial Data/geoprofile_ipums/output tables/aapi_alone_kpov.csv", skip = 2) %>% 
  rename(group=`0`, estimate = `X2`) %>% 
  mutate(label = "pov_k")
dta2 <- read_csv("/Users/sunnyshao/Dropbox/AAPIData HQ/Other Sites/Racial Data/geoprofile_ipums/output tables/aapi_alone_pov.csv", skip = 2) %>% 
  rename(group=`0`, estimate = `X2`) %>% 
  mutate(label = "pov")
dta3 <- read_csv("/Users/sunnyshao/Dropbox/AAPIData HQ/Other Sites/Racial Data/geoprofile_ipums/output tables/aapi_alone_spov.csv", skip = 2) %>% 
  rename(group=`0`, estimate = `X2`) %>% 
  mutate(label = "pov_s")
dta4 <- read_csv("/Users/sunnyshao/Dropbox/AAPIData HQ/Other Sites/Racial Data/geoprofile_ipums/output tables/detailed_hisp_kpov.csv", skip = 2) %>% 
  rename(group=`0`, estimate = `X2`) %>% 
  mutate(label = "pov_k")
dta5 <- read_csv("/Users/sunnyshao/Dropbox/AAPIData HQ/Other Sites/Racial Data/geoprofile_ipums/output tables/detailed_hisp_pov.csv", skip = 2) %>% 
  rename(group=`0`, estimate = `X2`) %>% 
  mutate(label = "pov")
dta6 <- read_csv("/Users/sunnyshao/Dropbox/AAPIData HQ/Other Sites/Racial Data/geoprofile_ipums/output tables/detailed_hisp_spov.csv", skip = 2) %>% 
  rename(group=`0`, estimate = `X2`) %>% 
  mutate(label = "pov_s")
#merge into one
poverty <- rbind(dta1, dta2, dta3, dta4, dta5, dta6)
saveRDS(poverty, "dta/cleaned_poverty.RDS")


# cvap --------------------------------------------------------------------



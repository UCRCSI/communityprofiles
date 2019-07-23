dta_cvap %>%
  filter(GEOID == "35028") %>%
  filter(group %in% c("B02001_003")) %>%
  left_join(dta_groupnames, by = c("group"="value")) %>%
  select(label.y,label.x,estimate) %>%
  mutate(estimate = case_when(is.na(estimate) ~ "-",
                              estimate == 0 ~ "-",
                              TRUE ~ as.character(round(estimate*100,1)))) %>%
  mutate(estimatenew = paste(paste(estimate)," % ",sep="")) %>%
  select(-estimate) %>%
  spread(label.y, estimatenew) %>%
  rename(Estimate = label.x) -> dta

dta$Estimate <- fct_recode(dta$Estimate,
                           "Citizen Age Voting Population" = "cvap",
                           "Foreign Born" = "fb",
                           "Native Born" = "native")
## Reordering dta$label.x
dta$Estimate <- factor(dta$Estimate, levels=c(  "Native Born" ,   "Foreign Born",  "Citizen Age Voting Population"))


dta_ins %>%
  filter(GEOID == "06037") %>%
  filter(group %in% c("B02001_003", "B03002_012", "B03002_003")) %>%
  left_join(dta_groupnames, by = c("group"="value")) %>%
  select(label.y,label.x,estimate, moe) %>%
  mutate(moe = case_when(is.na(moe) ~ "-",
                         TRUE ~ as.character(moe))) %>%
  mutate(estimatenew = paste(scales::percent(estimate), "<br>+/-",moe,"%",sep="")) %>%
  select( -moe,-estimate) %>%
  spread(label.y, estimatenew) %>%
  rename(Estimate = label.x) -> dta


dta$Estimate <- fct_recode(dta$Estimate,
               "No Health Insurance" = "noins")

dta_lep%>% filter(GEOID=="01113") %>% View()
dta_edu %>% filter(GEOID=="01113") %>% View()



dta_home %>%
  filter(GEOID == "06037") %>%
  filter(group %in% c("B02001_003", "B03002_012", "B03002_003")) %>%
  left_join(dta_groupnames, by = c("group"="value")) %>%
  select(label.y,label.x,estimate, moe) %>%
  mutate(moe = case_when(is.na(moe) ~ "-",
                         TRUE ~ as.character(moe))) %>%
  mutate(estimatenew = paste(scales::percent(estimate), "<br>+/-",moe,"%",sep="")) %>%
  select( -moe,-estimate) %>%
  spread(label.y, estimatenew) %>%
  rename(Estimate = label.x) -> dta


dta$Estimate <- fct_recode(dta$Estimate,
                           "Owner" = "owner",
                           "Renter" = "renter")
return(dta)
#

# dta_lep %>%
#   filter(GEOID == "0103") %>%
#   filter(group %in% c("B02001_003", "B02001_005", "B03002_003")) %>%
#   left_join(dta_groupnames, by = c("group"="value")) %>%
#   select(label.y,label.x,estimate, moe) %>%
#   mutate(estimatenew = paste(scales::percent(estimate), "<br>+/-",moe,"%",sep="")) %>%
#   select( -moe,-estimate) %>%
#   spread(label.y, estimatenew) -> dta
# ## Recoding dta$label.x
# dta$label.x <- fct_recode(dta$label.x,
#                "Speak only English" = "eng",
#                "Limited English Proficient" = "lep",
#                "Speak Language other than English at home" = "otherlang")
# ## Reordering dta$label.x
# dta$label.x <- factor(dta$label.x, levels=c("Speak only English", "Speak Language other than English at home", "Limited English Proficient"))
# dta_edu %>% count(group)

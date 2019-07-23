



dta %>%
  filter(topic == "Educational Attainment") %>%
  filter(GEOID == "06") %>%
  separate(variable, into = c("variable","type"), sep = ":") %>%
  filter(variable %in% c("B02001_003", "B02001_005", "B03002_003")) %>%
  left_join(dta_groupnames, by = c("variable" = "value"))


  # left_join(dta_groupnames, by = c("variable"="value")) %>%
  select(label,estimate, moe) %>%
  mutate(estimatenew = case_when( moe >= (.25*estimate) ~ "-",
                                  is.na(moe) ~ as.character(scales::comma(estimate)),
                                  TRUE ~ as.character(scales::comma(estimate)))) %>%
  mutate(moeNew = case_when( moe >= (.25*estimate) ~ "-",
                             is.na(moe) ~ "-",
                             TRUE ~ as.character(scales::comma(moe)))) %>%
  select(-moe,-estimate) %>%
  mutate(topic = "Total Population") %>%
  mutate(estimatenew = paste(estimatenew, "<br>+/-",moeNew)) %>%
  select(-moeNew) %>%
  spread(label, estimatenew) %>%
  rename(Estimate = topic)

library(tidyverse)
library(readxl)
library(readr)
library(tidycensus)
source("helper_functions/MOE_check.R")

#estimates we need
income_vars <- c("est_vc319","est_vc354","est_vc355")
pop_vars <- c("est_vc03")
educational_attainment <- c("est_vc135","est_vc136","est_vc137","est_vc138","est_vc139")
age_distribution <- c("est_vc27","est_vc16","est_vc17","est_vc18","est_vc19","est_vc20","est_vc21","est_vc22","est_vc23","est_vc24")
language <- c("est_vc233","est_vc234","est_vc235")
health_ins <- c('est_vc360','est_vc361','est_vc362')
homeowner <- c("est_vc390","est_vc391")

#moe we need
income_moe <- c("moe_vc319","moe_vc354","moe_vc355")
pop_moe <- c("moe_vc03")
educational_attainment_moe <- c("moe_vc135","moe_vc136","moe_vc137","moe_vc138","moe_vc139")
age_distribution_moe <- c("moe_vc27","moe_vc16","moe_vc17","moe_vc18","moe_vc19","moe_vc20","moe_vc21","moe_vc22","moe_vc23","moe_vc24")
language_moe <- c("moe_vc233","moe_vc234","moe_vc235")
health_ins_moe <- c('moe_vc360','moe_vc361','moe_vc362')
homeowner_moe <- c("moe_vc390","moe_vc391")

# subset to variables of interests ----------------------------------------
dta <- read_rds("dta/geo_profile_dta_acs1yr.rds")
dta[, 7:623] <- sapply(dta[,7:623], as.numeric)

df1 <- dta %>%
  select(geo_id, geo_id2, geo_type, geo_display_label, group_name,
         pop_vars, income_vars, educational_attainment, age_distribution,
         language, health_ins, homeowner) %>%
  gather(est_vars, estimate, -geo_id, -geo_id2,
         -geo_type, -geo_display_label, -group_name)

df2 <- dta %>%
  select(geo_id, geo_id2, geo_type, geo_display_label, group_name,
         pop_moe, income_moe, educational_attainment_moe, age_distribution_moe,
         language_moe, health_ins_moe, homeowner_moe) %>%
  gather(moe_vars, moe, -geo_id, -geo_id2,
         -geo_type, -geo_display_label, -group_name) %>%
  mutate(est_vars = str_replace(moe_vars, "moe_", "est_")) %>%
  select(-moe_vars)


dta_merge <- df1 %>%
  left_join(df2) %>%
mutate(estimate_type = case_when(
  est_vars == "est_vc319" ~"income",
  est_vars == "est_vc354" ~"income",
  est_vars == "est_vc355" ~"income",
  est_vars == "est_vc03" ~"count",
  est_vars == "est_vc27" ~"count",
  TRUE ~"prop"))

#count type estimates
count <- dta_merge %>%
  filter(estimate_type == "count")
count <- MOE_count(count)

#income type estimates
income <- dta_merge %>%
  filter(estimate_type == "income")
income <- MOE_income(income)

#proportion type estimates
prop <- dta_merge %>%
  filter(estimate_type == "prop")
prop <- MOE_prop(prop)

#final dta
final <- rbind(count, prop, income)
final <- final %>% mutate(geo_type = as.factor(geo_type))
## Reordering final$geo_type
final$geo_type <- factor(final$geo_type, levels=c("US", "State", "County", "Congressional District"))
final <- final %>% arrange(geo_type)
final2 <- final %>%
select(geo_type,geo_display_label, group_name, est_vars, estimate) %>%
  spread(est_vars, estimate)
#write a lookup table to reduce the datasize
# topic_lookup <- final %>%
#   select(topic) %>%
#   unique() %>%
#   mutate(topicID = dplyr::row_number()) %>%
#   write_csv("dta/topic_lookup.csv")
#
# group_lookup <- final %>%
#   select(group_name) %>%
#   unique() %>%
#   mutate(groupID = dplyr::row_number() + 100) %>%
#   write_csv("dta/group_lookup.csv")
#
# geo_lookup <- dta %>%
#   select(geo_id, geo_display_label) %>%
#   unique() %>%
#   write_csv("dta/geo_lookup.csv")
#
# geo_type <- final %>%
#   select(geo_type) %>%
#   unique() %>%
#   mutate(geo_typeID = dplyr::row_number() + 1110) %>%
#   write_csv("dta/geo_type_lookup.csv")
#
# final2 <- final %>%
#   left_join(topic_lookup) %>%
#   left_join(group_lookup) %>%
#   left_join(geo_type) %>%
#   left_join(geo_lookup) %>%
#   select(geo_id, geo_typeID, groupID, topicID, value_final)

final2 %>%  write_rds("dta/geo_profile_acs17_Sunnyclean.rds")


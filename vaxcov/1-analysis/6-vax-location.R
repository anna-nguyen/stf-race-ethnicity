##########################################
# Shoo the Flu evaluation
# Vaccination coverage analysis

# Examine location of vaccination, 
# stratified by race/ethnicity
##########################################

rm(list = ls())

# define directories, load libraries
source(here::here("0-config.R"))
library(purrr)

load(district_demographics_path)

data = read.csv(data_path_2017)
data$dist = factor(data$dist,levels = c("WCCUSD","OUSD"))

data = data %>% 
  mutate(Race = case_when(
    race == "Black" ~ "Black/African American",
    race == "Latino" ~ "Hispanic/Latino",
    race == "Asian" ~ "Asian/Pacific Islander", 
    race == "Pacific islander" ~ "Asian/Pacific Islander",
    race == "Multi" ~ "Multiple Races", 
    TRUE ~ as.character(race))) %>% 
  filter(Race %in% c("White", "Asian/Pacific Islander", "Multiple Races", "Black/African American", "Hispanic/Latino")) 

data.y4 = read.csv(data_path_2018)
data.y4$dist = factor(data.y4$dist,levels = c("WCCUSD","OUSD"))

data.y4$vx1718yn = ifelse(data.y4$vx1718 == 1,1,0)

data.y4 = data.y4 %>% 
  mutate(Race = case_when(
    race == "Black" ~ "Black/African American",
    race == "Latino" ~ "Hispanic/Latino",
    race == "Asian" ~ "Asian/Pacific Islander", 
    race == "Pacific islander" ~ "Asian/Pacific Islander",
    race == "Multi" ~ "Multiple Races", 
    TRUE ~ as.character(race))) %>% 
  filter(Race %in% c("White", "Asian/Pacific Islander", "Multiple Races", "Black/African American", "Hispanic/Latino")) 

vxloc_race_tbl_1415 = data %>%
  group_by(Race, district, vxloc1415) %>%
  summarise(n = n()) %>%
  mutate(Percent = n / sum(n) * 100) %>% 
  ungroup() %>% 
  filter(vxloc1415 %in% c("Doctor/clinic", "Other", "School")) %>% 
  mutate(season = "2014-15") %>% 
  rename(vxloc = vxloc1415)

vxloc_race_tbl_1516 = data %>%
  group_by(Race, district, vxloc1516) %>%
  summarise(n = n()) %>%
  mutate(Percent = n / sum(n) * 100) %>% 
  ungroup() %>% 
  filter(vxloc1516 %in% c("Doctor/clinic", "Other", "School")) %>% 
  mutate(season = "2015-16") %>% 
  rename(vxloc = vxloc1516)

vxloc_race_tbl_1617 = data %>%
  group_by(Race, district, vxloc1617) %>%
  summarise(n = n()) %>%
  mutate(Percent = n / sum(n) * 100) %>% 
  ungroup() %>% 
  filter(vxloc1617 %in% c("Doctor/clinic", "Other", "School")) %>% 
  mutate(season = "2016-17") %>% 
  rename(vxloc = vxloc1617)

vxloc_race_tbl_1718 = data.y4 %>%
  group_by(Race, district, vxloc1718) %>%
  summarise(n = n()) %>%
  mutate(Percent = n / sum(n) * 100) %>% 
  ungroup() %>% 
  filter(vxloc1718 %in% c("Doctor/clinic", "Other", "School")) %>% 
  mutate(season = "2017-18") %>% 
  rename(vxloc = vxloc1718)

vxloc_race_tbl = rbind(vxloc_race_tbl_1415, vxloc_race_tbl_1516, vxloc_race_tbl_1617, vxloc_race_tbl_1718) %>% 
  mutate(district = ifelse(district == "OUSD", "Intervention District", "Comparison District"))

vxloc_race_tbl$vxloc = factor(vxloc_race_tbl$vxloc, 
                              levels = c("Doctor/clinic", "School", "Other"))

vxloc_race_tbl$district = factor(vxloc_race_tbl$district, 
                              levels = c("Intervention District", "Comparison District"))


saveRDS(vxloc_race_tbl, paste0(local_res_path, "/vax_location.RDS"))

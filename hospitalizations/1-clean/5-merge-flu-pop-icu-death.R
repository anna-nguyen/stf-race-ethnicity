##########################################
# Shoo the Flu evaluation
# Surveillance analysis

# Analysis of Shoo the Flu indirect effects
# on flu hospitalization

# Parameter estimation
# using data subset by zip code for numerator and
# denominator
##########################################
rm(list=ls())
source(here::here("0-config.R"))

# ----------------------------------------
# import datasets
# ----------------------------------------
# load CEIP flu case dataset
# (it's the same across all datasets)
flu = readRDS(paste0(data_dir, "ceip-flu-clean.RDS"))

# load census data subset by zip code
census_zip_age_sex_race =readRDS(paste0(clean_census_dir, "Censpop-agecat-sex-race-zip.RDS"))

# subset to flu positive cases
flu_sub = subset(flu,flu$flupos==1)

# ----------------------------------------
# combine less common race categories
# ----------------------------------------
# drop if race is unknown since that category is
# not in the census 
print(paste("Number of observations from flu dataset dropped due to unknown race:", 
            nrow(flu_sub[flu_sub$race=="Unknown",]), "out of", nrow(flu_sub)))
flu_sub = flu_sub %>% filter(race!="Unknown")

# combine less common race categories
flu_sub = flu_sub %>% mutate(race = case_when(
  race == "American Indian/Alaska Native" ~ "Other", 
  race == "Two or more races" ~ "Other",
  race == "Hispanic" ~ "Other",
  TRUE ~ race))

flu_agg = flu_sub %>%
  group_by(flusesn, dist, agecat, sex, race) %>%
  summarise(flucases = sum(flupos, na.rm=TRUE),
            deaths = sum(death),
            icu = sum(ICU, na.rm=TRUE)) %>%
  rename(seas = flusesn) %>% 
  ungroup() %>% 
  mutate(dist = as.factor(dist)) %>%
  mutate(sex = recode_factor(sex, "Female" = "female", "Male" = "male"),
         race = factor(race, levels = c("API", "Black", "Other", "White"))) 

# filter out anyone with missing agecat
# since they will be dropped in the merge
# with the census data
flu_agg = flu_agg %>% filter(!is.na(agecat))

# summarise population data
pop_agg = prep_census(census_zip_age_sex_race, 
                      season_list = c(0809, 0910, 1011, 1112, 1213, 1314, 1415, 1516, 1617, 1718),
                      covariates = c("agecat", "sex","race"))

pop_agg = pop_agg %>% mutate(dist = as.factor(dist))
pop_agg = pop_agg %>%
  mutate(agecat = factor(agecat, levels = levels(flu_agg$agecat)))

# merge numerator and denominator data
flu.merge=full_join(pop_agg, flu_agg,by=c("seas","dist","sex","agecat", "race"))

# create log pop offset and treatment indicator
flu.merge = flu.merge %>% 
  mutate(logN = log(N),
         tr = ifelse(dist=="OUSD", 1, 0))

# assign missing values as 0s, recode season
icu_death = flu.merge %>% 
  mutate(flucases = ifelse(is.na(flucases), 0, flucases),
         deaths = ifelse(is.na(deaths), 0, deaths),
         icu = ifelse(is.na(icu), 0, icu),
         season = as.factor(case_when(
           seas == 0809 ~ "2008-09",
           seas == 0910 ~ "2009-10",
           seas == 1011 ~ "2010-11",
           seas == 1112 ~ "2011-12",
           seas == 1213 ~ "2012-13",
           seas == 1314 ~ "2013-14",
           seas == 1415 ~ "2014-15",
           seas == 1516 ~ "2015-16",
           seas == 1617 ~ "2016-17",
           seas == 1718 ~ "2017-18"
         )))

# drop observations with no age
icu_death = icu_death %>% filter(agecat!="(Missing)")

saveRDS(icu_death, file=paste0(data_dir, "ceip-icu_death-data-age-sex-zip.RDS"))

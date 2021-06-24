##########################################
# Shoo the Flu evaluation
# Surveillance analysis

# Analysis of Shoo the Flu indirect effects
# on flu hospitalization

# merge CEIP data and census/ACS data
##########################################
rm(list=ls())

source(here::here("0-config.R"))

# ----------------------------------------
# import datasets
# ----------------------------------------
# load CEIP flu case dataset
# (it's the same across all datasets)
flu = readRDS(paste0(data_dir, "/ceip-flu-clean.RDS"))

# load CEIP flu case dataset
# excluding zip codes on the border of the districts
flu_sens = readRDS(paste0(data_dir, "ceip-flu-clean-sens.RDS"))

# load census data subset by school district
census_dist = readRDS(paste0(clean_census_dir, "Censpop-agecat-sex-race-dist.RDS"))

# load census data subset by zip code
census_zip_age_sex =readRDS(paste0(clean_census_dir, "Censpop-ageyrs-sex-zip.RDS"))
census_zip_age_sex_race = readRDS(paste0(clean_census_dir, "Censpop-agecat-sex-race-zip.RDS"))

# load census data subset by zip code with complete race info
census_zip_age_sex_race_complete = readRDS(paste0(clean_census_dir, "Censpop-agecat-sex-race-zip-race-complete.RDS"))

# load census data subset by zip code
# excluding zip codes on the school district boundaries
census_zip_sens = readRDS(paste0(clean_census_dir, "Censpop-agecat-sex-race-zip-sens.RDS")) 
census_zip_age_sens = readRDS(paste0(clean_census_dir, "Censpop-ageyrs-sex-zip-sens.RDS")) 

# load acs data
acs = readRDS(paste0(clean_census_dir, "ACSyearpop.RDS"))

seasons = c(0809, 0910, 1011, 1112, 1213, 1314, 1415, 1516, 1617, 1718)

subsets = as.list(c("All", "fluseasCDPH_2_5", "ceip_peakwk"))

# ----------------------------------------
# Flu definition: any test is flu positive

# Population: census stratified by sex, age
# and subset by zip code 
# ----------------------------------------
# subset to flu positive cases
flu_sub = subset(flu,flu$flupos==1)

flu_sub_list = lapply(subsets, function(x) subset_data(data = flu_sub, subset = x))
names(flu_sub_list) = unlist(subsets)

merged_list = lapply(
  flu_sub_list, function(x)
    merge_flucount_pop(flucount = x, 
                       pop = census_zip_age_sex, 
                       pop_type = "census", 
                       covariates = c("ageyrs", "sex"),
                       season_list = seasons)
)

# save datasets
saveRDS(merged_list, file=paste0(data_dir, "ceip-flu-data-age-sex-zip.RDS"))
rm(merged_list, flu_sub, flu_sub_list)

# ----------------------------------------
# Flu definition: any test is flu positive

# Population: census stratified by race, sex, age
# and subset by zip code 
# ----------------------------------------
# subset to flu positive cases
flu_sub = subset(flu,flu$flupos==1)

flu_sub_list = lapply(subsets, function(x) subset_data(data = flu_sub, subset = x))
names(flu_sub_list) = unlist(subsets)

# merge flu counts and population data
merged_list = lapply(
  flu_sub_list, function(x)
    merge_flucount_pop(flucount = x, 
                       pop = census_zip_age_sex_race, 
                       pop_type = "census", 
                       covariates = c("agecat", "sex", "race"),
                       season_list = seasons)
)

# save datasets
saveRDS(merged_list, file=paste0(data_dir, "ceip-flu-data-age-sex-race-zip.RDS"))
rm(merged_list, flu_sub, flu_sub_list)

# ----------------------------------------
# Flu definition: any test is flu positive

# Population: census stratified by race, sex, age
# and subset by zip code 

# Keep more rare race categories for race-stratified analysis
# ----------------------------------------
# combine pacific islander and asian as we did for flu numerator
census_zip_age_sex_race_complete = census_zip_age_sex_race_complete %>%
  mutate(race_complete = case_when(
    race_complete == "Pacific Islander" ~ "API",
    race_complete == "Asian American" ~ "API",
    TRUE ~ race_complete)) %>%
  group_by(agecat, sex, race_complete) %>%
  summarise(OUSD = sum(OUSD),
            WCCUSD = sum(WCCUSD))

# subset to flu positive cases
flu_sub = subset(flu,flu$flupos==1)

flu_sub_list = lapply(subsets, function(x) subset_data(data = flu_sub, subset = x))
names(flu_sub_list) = unlist(subsets)

merged_list = lapply(
  flu_sub_list, function(x)
    merge_flucount_pop(flucount = x, 
                       pop = census_zip_age_sex_race_complete, 
                       pop_type = "census", 
                       covariates = c("agecat", "sex", "race"),
                       season_list = seasons,
                       all_race_cats = TRUE)
)

# save datasets
saveRDS(merged_list, file=paste0(data_dir, "ceip-flu-data-age-sex-race-complete-zip.RDS"))
rm(merged_list, flu_sub)

# ----------------------------------------
# Flu definition: any test is flu positive
# EXCLUDING ICU

# Population: census stratified bysex, age
# and subset by zip code 
# ----------------------------------------
# subset to flu positive cases
flu_sub = subset(flu,flu$flupos==1)
flu_sub = subset(flu_sub,flu_sub$ICU==0)

flu_sub_list = lapply(subsets, function(x) subset_data(data = flu_sub, subset = x))
names(flu_sub_list) = unlist(subsets)

# merge flu counts and population data
merged_list = lapply(
  flu_sub_list, function(x)
    merge_flucount_pop(flucount = x, 
                       pop = census_zip_age_sex_race, 
                       pop_type = "census", 
                       covariates = c("agecat", "sex", "race"),
                       season_list = seasons)
)

# save datasets
saveRDS(merged_list, file=paste0(data_dir, "ceip-flu-data-noICU-age-sex-race-zip.RDS"))
rm(merged_list, flu_sub, flu_sub_list)


# ----------------------------------------
# Flu definition: any test is flu positive
# ONLY ICU

# Population: census stratified by race, sex, age
# and subset by zip code 
# ----------------------------------------
# subset to flu positive cases
flu_sub = subset(flu,flu$flupos==1)
flu_sub = subset(flu_sub,flu_sub$ICU==1)

flu_sub_list = lapply(subsets, function(x) subset_data(data = flu_sub, subset = x))
names(flu_sub_list) = unlist(subsets)

# merge flu counts and population data
merged_list = lapply(
  flu_sub_list, function(x)
    merge_flucount_pop(flucount = x, 
                       pop = census_zip_age_sex_race, 
                       pop_type = "census", 
                       covariates = c("agecat", "sex", "race"),
                       season_list = seasons)
)

# save datasets
saveRDS(merged_list, file=paste0(data_dir, "ceip-flu-data-ICU-age-sex-race-zip.RDS"))
rm(merged_list, flu_sub, flu_sub_list)


# ----------------------------------------
# Flu definition: most sensitive test if more 
# than one test was done ("fpsens")

# Population: census stratified by sex, age, race
# and subset by zip code 
# ----------------------------------------
# subset to flu positive cases
flu_sub = subset(flu,flu$flupsens==1)

flu_sub_list = lapply(subsets, function(x) subset_data(data = flu_sub, subset = x))
names(flu_sub_list) = unlist(subsets)

# merge flu counts and population data
merged_list = lapply(
  flu_sub_list, function(x)
    merge_flucount_pop(flucount = x, 
                       pop = census_zip_age_sex_race, 
                       pop_type = "census", 
                       covariates = c("agecat", "sex", "race"),
                       season_list = seasons)
)

# save datasets
saveRDS(merged_list, file=paste0(data_dir, "ceip-flu-data-fpsens-age-sex-race-zip.RDS"))
rm(merged_list, flu_sub, flu_sub_list)

# ----------------------------------------
# Flu definition: any test is flu positive

# Population: census stratified by sex, age
# and subset by school district boundaries
# ----------------------------------------
# subset to flu positive cases
flu_sub = subset(flu,flu$flupos==1)

flu_sub_list = lapply(subsets, function(x) subset_data(data = flu_sub, subset = x))
names(flu_sub_list) = unlist(subsets)

# merge flu counts and population data
merged_list = lapply(
  flu_sub_list, function(x)
    merge_flucount_pop(flucount = x, 
                       pop = census_dist, 
                       pop_type = "census", 
                       covariates = c("agecat", "sex", "race"),
                       season_list = seasons)
)
  
# save datasets
saveRDS(merged_list, file=paste0(data_dir, "ceip-flu-data-age-sex-race-dist.RDS"))
rm(merged_list, flu_sub, flu_sub_list)

# ----------------------------------------
# Flu definition: any test is flu positive

# Population: census stratified by race, sex, age
# and subset by zip code, excluding zip codes 
# on the school district boundaries ("sens")
# ----------------------------------------
# subset to flu positive cases
flu_sub = subset(flu_sens, flu$flupos==1)
flu_sub = flu_sub %>% filter(
  zip %in% c(ousd.zip.sens, wcc.zip.sens)
)

flu_sub_list = lapply(subsets, function(x) subset_data(data = flu_sub, subset = x))
names(flu_sub_list) = unlist(subsets)

# merge flu counts and population data
merged_list = lapply(
  flu_sub_list, function(x)
    merge_flucount_pop(flucount = x, 
                       pop = census_zip_sens, 
                       pop_type = "census", 
                       covariates = c("agecat", "sex", "race"),
                       season_list = seasons)
)

# save datasets
saveRDS(merged_list, file=paste0(data_dir, "ceip-flu-data-age-sex-race-zipsens.RDS"))
rm(merged_list, flu_sub, flu_sub_list)

# ----------------------------------------
# Flu definition: any test is flu positive

# Population: acs stratified by age and 
# years 2010-2014
# ----------------------------------------
# subset to flu positive cases
flu_sub = subset(flu, flu$flupos==1)

flu_sub_list = lapply(subsets, function(x) subset_data(data = flu_sub, subset = x))
names(flu_sub_list) = unlist(subsets)

# merge flu counts and population data
merged_list = lapply(
  flu_sub_list, function(x)
    merge_flucount_pop(flucount = x, 
                       pop = acs, 
                       pop_type = "acs", 
                       covariates = "agecat",
                       season_list = seasons)
)

# save datasets
saveRDS(merged_list, file=paste0(data_dir, "ceip-flu-data-age-acs.RDS"))
rm(merged_list, flu_sub, flu_sub_list)




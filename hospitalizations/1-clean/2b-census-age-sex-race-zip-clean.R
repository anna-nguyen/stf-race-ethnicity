##########################################
# Shoo the Flu evaluation
# Surveillance analysis

# Import and clean 2010 census data to use as 
# denominators in calculating rates using
# CEIP data

# stratified by age and sex and race
# subsetting census data by zip code
##########################################

rm(list=ls())
source(here::here("0-config.R"))


####################################################################################
# import 2010 Census Data
####################################################################################

# data
cens.white.all=read.csv(paste0(raw_census_dir,"Decenniel census - age race sex zip code/DEC_10_SF1_P12A_with_ann.csv"))
cens.black.all=read.csv(paste0(raw_census_dir,"Decenniel census - age race sex zip code/DEC_10_SF1_P12B_with_ann.csv"))
cens.nativ.all=read.csv(paste0(raw_census_dir,"Decenniel census - age race sex zip code/DEC_10_SF1_P12C_with_ann.csv"))
cens.asian.all=read.csv(paste0(raw_census_dir,"Decenniel census - age race sex zip code/DEC_10_SF1_P12D_with_ann.csv"))
cens.PI.all=read.csv(paste0(raw_census_dir,"Decenniel census - age race sex zip code/DEC_10_SF1_P12E_with_ann.csv"))
cens.other.all=read.csv(paste0(raw_census_dir,"Decenniel census - age race sex zip code/DEC_10_SF1_P12F_with_ann.csv"))
cens.multi.all=read.csv(paste0(raw_census_dir,"Decenniel census - age race sex zip code/DEC_10_SF1_P12G_with_ann.csv"))
cens.hisp.all=read.csv(paste0(raw_census_dir,"Decenniel census - age race sex zip code/DEC_10_SF1_P12H_with_ann.csv"))

# meta data
cens.white.meta=read.csv(paste0(raw_census_dir,"Decenniel census - age race sex zip code/DEC_10_SF1_P12A_metadata.csv"))
cens.black.meta=read.csv(paste0(raw_census_dir,"Decenniel census - age race sex zip code/DEC_10_SF1_P12B_metadata.csv"))
cens.nativ.meta=read.csv(paste0(raw_census_dir,"Decenniel census - age race sex zip code/DEC_10_SF1_P12C_metadata.csv"))
cens.asian.meta=read.csv(paste0(raw_census_dir,"Decenniel census - age race sex zip code/DEC_10_SF1_P12D_metadata.csv"))
cens.PI.meta=read.csv(paste0(raw_census_dir,"Decenniel census - age race sex zip code/DEC_10_SF1_P12E_metadata.csv"))
cens.other.meta=read.csv(paste0(raw_census_dir,"Decenniel census - age race sex zip code/DEC_10_SF1_P12F_metadata.csv"))
cens.multi.meta=read.csv(paste0(raw_census_dir,"Decenniel census - age race sex zip code/DEC_10_SF1_P12G_metadata.csv"))
cens.hisp.meta=read.csv(paste0(raw_census_dir,"Decenniel census - age race sex zip code/DEC_10_SF1_P12H_metadata.csv"))

####################################################################################
# process meta data 
####################################################################################
#----------------------------------------
# define ages in census labels
#----------------------------------------
agecats = c(
  "Under 5",
  "5 to 9",
  "10 to 14",
  "15 to 17",
  "18 and 19",
  "20",
  "21",
  "22 to 24",
  "25 to 29",
  "30 to 34",
  "35 to 39",
  "40 to 44",
  "45 to 49",
  "50 to 54",
  "55 to 59",
  "60 and 61",
  "62 to 64",
  "65 and 66",
  "67 to 69",
  "70 to 74",
  "75 to 79",
  "80 to 84",
  "85 and over"
)

#----------------------------------------
# filter meta data 
#----------------------------------------
meta_list = list(cens.white.meta, cens.black.meta, cens.nativ.meta,
                 cens.asian.meta, cens.PI.meta, cens.other.meta,
                 cens.multi.meta, cens.hisp.meta)

clean_meta_data = lapply(meta_list, function(x) clean_age_sex(x, agecats))


####################################################################################
# process census data
####################################################################################
census_list = list(cens.white.all, cens.black.all, cens.nativ.all,
                   cens.asian.all, cens.PI.all, cens.other.all,
                   cens.multi.all, cens.hisp.all)

race_list = list("White", "Black", "American Indian/Alaska Native",
                 "Asian American", "Pacific Islander",
                 "Other", "Two or more races", "Hispanic")

#------------------------------------------------
# primary analysis
#------------------------------------------------
cens_clean_list = list()
for(i in 1:length(census_list)){
  cens_clean_list[[i]] = process_census(
    census_data = census_list[[i]],
    meta_data = clean_meta_data[[i]],
    zip_name = "GEO.id2",
    ousd_zips = ousd.zip,
    wcc_zips = wcc.zip
  ) 
  
  cens_clean_list[[i]] = cens_clean_list[[i]] %>% 
    mutate(race = race_list[[i]])
}

#------------------------------------------------
# zip code subset
#------------------------------------------------
cens_clean_list_sens = list()

for(i in 1:length(census_list)){
  cens_clean_list_sens[[i]] = process_census(
    census_data = census_list[[i]],
    meta_data = clean_meta_data[[i]],
    zip_name = "GEO.id2",
    ousd_zips = ousd.zip.sens,
    wcc_zips = wcc.zip.sens
  ) 
  
  cens_clean_list_sens[[i]] = cens_clean_list_sens[[i]] %>% 
    mutate(race = race_list[[i]])
}



#------------------------------------------------
# combine into one data frame
#------------------------------------------------
cens_all = bind_rows(cens_clean_list)
cens_all_sens = bind_rows(cens_clean_list_sens)

#------------------------------------------------
# combine asian and native hawaiian/pacific islander
# to align with CEIP categorization
#------------------------------------------------
# primary analysis
cens_all = cens_all %>% 
  mutate(race_complete = race,
         race = ifelse(race=="Asian American" | race=="Pacific Islander", "API", race))

cens_all_race_complete = cens_all %>% 
  ungroup() %>%
  group_by(agecat, sex, race_complete) %>%
  mutate(OUSD = as.numeric(OUSD),
         WCCUSD = as.numeric(WCCUSD)) %>%
  summarise(OUSD = sum(OUSD),
            WCCUSD = sum(WCCUSD))

cens_all = cens_all %>% group_by(agecat, sex, race) %>%
  filter(race!="Hispanic") %>%
  mutate(OUSD = as.numeric(OUSD),
         WCCUSD = as.numeric(WCCUSD)) %>%
  summarise(OUSD = sum(OUSD),
            WCCUSD = sum(WCCUSD))

# zip code subset
cens_all_sens = cens_all_sens %>% 
  mutate(race = ifelse(race=="Asian American" | race=="Pacific Islander", "API", race))

cens_all_sens = cens_all_sens %>% group_by(agecat, sex, race) %>%
  mutate(OUSD = as.numeric(OUSD),
         WCCUSD = as.numeric(WCCUSD)) %>%
  summarise(OUSD = sum(OUSD),
            WCCUSD = sum(WCCUSD))

#------------------------------------------------
# save data
#------------------------------------------------
saveRDS(cens_all,file=paste0(clean_census_dir, "Censpop-agecat-sex-race-zip.RDS"))
saveRDS(cens_all_sens,file=paste0(clean_census_dir, "Censpop-agecat-sex-race-zip-sens.RDS"))

saveRDS(cens_all_race_complete,file=paste0(clean_census_dir, "Censpop-agecat-sex-race-zip-race-complete.RDS"))



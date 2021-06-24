##########################################
# Shoo the Flu evaluation
# Surveillance analysis

# Import and clean 2010 census data to use as 
# denominators in calculating rates using
# CEIP data

# stratified by age and sex
# subsetting census data by school district
##########################################

rm(list=ls())
source(here::here("0-config.R"))

####################################################################################
# import 2010 Census Data
####################################################################################

cens.all=read.csv(paste0(raw_census_dir,"/Decennial census/DEC_10_SF1_QTP2_with_ann.csv"))
meta=read.csv(paste0(raw_census_dir,"Decenniel census - age sex zip code/DEC_10_SF1_QTP2_metadata.csv"))

####################################################################################
# process meta data 
####################################################################################
#----------------------------------------
# define ages in census labels
#----------------------------------------
single_ageyrs = seq(2, 99, 1)
single_ageyrs_f = paste0(single_ageyrs, " years")
all_ages = c("Under 1 year", "1 year", single_ageyrs_f,
             "100 to 104 years",
             "105 to 109 years", "110 years and over")

#----------------------------------------
# filter meta data 
#----------------------------------------
meta_sub = meta %>% 
  mutate(ageyrs = clean_age(Id),
         sex = clean_sex(Id)) %>%
  filter(sex %in% c("male", "female")) %>%
  filter(ageyrs %in% all_ages) 

# only keep rows with "Number -"
meta_sub = meta_sub[grep("Number -", meta_sub$Id),]

# check that there is one row for each age and sex 
assert_that(max(data.frame(table(meta_sub$ageyrs, meta_sub$sex))$Freq)==1)

# create vector of labels to keep
keep = as.character(meta_sub$GEO.id)

####################################################################################
# process census data
####################################################################################
# cens=cens.all[,colnames(cens.all) %in% c("GEO.display.label",keep)]

#----------------------------------------
# subset to single age ageyrs and sex columns
#----------------------------------------
cens = cens.all %>% select(c(GEO.display.label, keep))

#----------------------------------------
# subset by district
#----------------------------------------
cens = cens %>% filter(
  GEO.display.label == "Oakland Unified School District, California" |
  GEO.display.label == "West Contra Costa Unified School District, California"
) %>%
  mutate(dist = case_when(
    GEO.display.label == "Oakland Unified School District, California"  ~ "OUSD",
    GEO.display.label == "West Contra Costa Unified School District, California"  ~ "WCCUSD"
  ))

#----------------------------------------
# aggregate within district
#----------------------------------------
cens.dist = cens %>% select(-GEO.display.label) 

# convert factor to numeric
for(i in 1:(ncol(cens.dist)-1)){
  cens.dist[,i] = as.numeric(as.character(cens.dist[,i]))
}

cens.dist = cens.dist %>%
  group_by(dist) %>%
  summarise_all(sum)

cens.dist.l = t(cens.dist)
colnames(cens.dist.l) = c("OUSD", "WCCUSD")
cens.dist.l = cens.dist.l[-1,]
cens.dist.l = as.data.frame(cens.dist.l)

#----------------------------------------
# merge in labels from metadata
#----------------------------------------
cens.dist.l$label = rownames(cens.dist.l)
rownames(cens.dist.l) = NULL

meta_sub = meta_sub %>% rename(label = GEO.id) %>%
  select(-Id)

cens.clean = full_join(cens.dist.l, meta_sub, by = c("label"))

# drop label
cens.clean = cens.clean %>% select(-label)

saveRDS(cens.clean,file=paste0(clean_census_dir,"Censpop-ageyrs-sex-dist.RDS"))




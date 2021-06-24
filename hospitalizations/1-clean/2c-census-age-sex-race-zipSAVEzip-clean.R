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

# function that inputs a race-specific census file
# and subsets it to the cells with age- and sex-specific
# information and zip code

ousd.zip=c(94601, 94602, 94603, 94605, 94606, 94607, 94608, 94609, 
           94610, 94611, 94612, 94618, 94619, 94621, 94705, 94613, 
           94704, 94604, 94614, 94615, 94617, 94622, 94623, 94624, 
           94649, 94659, 94660, 94661, 94666)

wcc.zip=c(94530, 94547, 94564, 94707, 94708, 94801, 94803, 94804, 
          94805, 94806, 94850)

#------------------------------------------------
# Import datasets
#------------------------------------------------
cens.white.all=read.csv("~/Dropbox/Flu/Surveillance/Census/Decenniel census - age race sex zip code/DEC_10_SF1_P12A_with_ann.csv")
cens.black.all=read.csv("~/Dropbox/Flu/Surveillance/Census/Decenniel census - age race sex zip code/DEC_10_SF1_P12B_with_ann.csv")
cens.nativ.all=read.csv("~/Dropbox/Flu/Surveillance/Census/Decenniel census - age race sex zip code/DEC_10_SF1_P12C_with_ann.csv")
cens.asian.all=read.csv("~/Dropbox/Flu/Surveillance/Census/Decenniel census - age race sex zip code/DEC_10_SF1_P12D_with_ann.csv")
cens.PI.all=read.csv("~/Dropbox/Flu/Surveillance/Census/Decenniel census - age race sex zip code/DEC_10_SF1_P12E_with_ann.csv")
cens.other.all=read.csv("~/Dropbox/Flu/Surveillance/Census/Decenniel census - age race sex zip code/DEC_10_SF1_P12F_with_ann.csv")
cens.multi.all=read.csv("~/Dropbox/Flu/Surveillance/Census/Decenniel census - age race sex zip code/DEC_10_SF1_P12G_with_ann.csv")
cens.latino.all=read.csv("~/Dropbox/Flu/Surveillance/Census/Decenniel census - age race sex zip code/DEC_10_SF1_P12H_with_ann.csv")
cens.whitenotL.all=read.csv("~/Dropbox/Flu/Surveillance/Census/Decenniel census - age race sex zip code/DEC_10_SF1_P12I_with_ann.csv")


#------------------------------------------------
# Reformat datasets
#------------------------------------------------
cens.white=cens.race(cens.white.all,race="White",ousd.zip=ousd.zip,wcc.zip=wcc.zip)
cens.black=cens.race(cens.black.all,race="Black",ousd.zip=ousd.zip,wcc.zip=wcc.zip)
cens.nativ=cens.race(cens.nativ.all,race="American Indian/Alaska Native",ousd.zip=ousd.zip,wcc.zip=wcc.zip)
cens.asian=cens.race(cens.asian.all,race="Asian",ousd.zip=ousd.zip,wcc.zip=wcc.zip)
cens.PI=cens.race(cens.PI.all,race="Native Hawaiian/Pacific Islander",ousd.zip=ousd.zip,wcc.zip=wcc.zip)
cens.other=cens.race(cens.other.all,race="Other",ousd.zip=ousd.zip,wcc.zip=wcc.zip)
cens.multi=cens.race(cens.multi.all,race="Two or more races",ousd.zip=ousd.zip,wcc.zip=wcc.zip)
cens.latino=cens.race(cens.latino.all,race="Latino",ousd.zip=ousd.zip,wcc.zip=wcc.zip)
cens.whitenotL=cens.race(cens.whitenotL.all,race="White not latino",ousd.zip=ousd.zip,wcc.zip=wcc.zip)


#------------------------------------------------
# combine into one data frame
#------------------------------------------------
cens.all=rbind(cens.white,cens.black,cens.nativ,cens.asian,cens.PI,
               cens.other,cens.multi)

# combine asian and native hawaiian/pacific islander
# to align with CEIP categorization
cens.all = cens.all %>%
  mutate(race = replace(race,race=="Asian","API"),
         race = replace(race,race=="Native Hawaiian/Pacific Islander","API")) 

cens.all = cens.all %>% rename(agecat = age)

save(cens.all,file="~/Dropbox/Flu/StFData/Census/Censpop-agecat-sex-race-zip2.RData")




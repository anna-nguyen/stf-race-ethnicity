##########################################
# Shoo the Flu evaluation
# Surveillance analysis

# Import and clean ACS data to use as 
# denominators in calculating rates using
# CEIP data
##########################################

rm(list=ls())
source(here::here("0-config.R"))

#------------------------------------------------
# import American Community Survey Data
# keep estimates for each school district by age
# for a specific year
# year = 4 digit year string
#------------------------------------------------


import.acs=function(year){
  acs=read.csv(paste(raw_census_dir,"ACS/",year,"/5-year",
    "/ACS_",substr(year,3,4),"_5YR_DP05_with_ann.csv",sep=""),header=TRUE)
  # drop first row 
  acs=acs[c(2:3),]
  
  # keep columns with total population estimates each year
  # for 2010, 2011, 2012
  keep.cols.1=c("HC01_VC07","HC01_VC08","HC01_VC09","HC01_VC10","HC01_VC11",
              "HC01_VC12","HC01_VC13","HC01_VC14","HC01_VC15",
              "HC01_VC16","HC01_VC17","HC01_VC18","HC01_VC19")
  
  # for 2013, 2014, 2015, 2016
  keep.cols.2=c("HC01_VC08","HC01_VC09","HC01_VC10","HC01_VC11",
              "HC01_VC12","HC01_VC13","HC01_VC14","HC01_VC15",
              "HC01_VC16","HC01_VC17","HC01_VC18","HC01_VC19",
              "HC01_VC20")
  
  # for 2017
  keep.cols.3=c("HC01_VC09","HC01_VC10","HC01_VC11","HC01_VC12",
                "HC01_VC13","HC01_VC14","HC01_VC15","HC01_VC16",
                "HC01_VC17","HC01_VC18","HC01_VC19","HC01_VC20", 
                "HC01_VC21")
  
  if(as.numeric(year) >= 2010 & as.numeric(year) <= 2012){
    acs=acs[,colnames(acs) %in% keep.cols.1]
  }
  
  if(as.numeric(year) >= 2013 & as.numeric(year) <= 2016){
    acs=acs[,colnames(acs) %in% keep.cols.2]
  }
  
  if(as.numeric(year) == 2017){
    acs=acs[,colnames(acs) %in% keep.cols.3]
  }
  
  acs=as.data.frame(t(acs))
  colnames(acs)=c("OUSD","WCCUSD")
  acs$agecat=c("Under 5 years","5-9 years","10-14 years","15-19 years",
                "20-24 years","25-34 years","35-44 years","45-54 years",
                "55-59 years","60-64 years","65-74 years","75-84 years",
                "85+ years")
  
  acs$year=as.numeric(year)
  rownames(acs)=NULL
  
  return(acs)
}

acs2010=import.acs(2010)
acs2011=import.acs(2011)
acs2012=import.acs(2012)
acs2013=import.acs(2013)
acs2014=import.acs(2014)
acs2015=import.acs(2015)
acs2016=import.acs(2016)
acs2017=import.acs(2017)

# acs 2018 not yet available, using acs 2017
acs2018 = acs2017 %>%
  mutate(year = 2018)

acs=rbind(acs2010,acs2011,acs2012,acs2013,acs2014, acs2015, acs2016, acs2017, acs2018)

acs$OUSD=as.numeric(as.character(acs$OUSD))
acs$WCCUSD=as.numeric(as.character(acs$WCCUSD))

acs$agecat=factor(acs$agecat,levels=c("Under 5 years","5-9 years","10-14 years",
   "15-19 years","20-24 years","25-34 years","35-44 years","45-54 years",
    "55-59 years","60-64 years","65-74 years","75-84 years","85+ years"))


saveRDS(acs,
     file=paste0(clean_census_dir,"ACSyearpop.RDS"))


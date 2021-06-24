##########################################
# Shoo the Flu evaluation
# Surveillance analysis

# Import and clean 2010 census data to use as 
# denominators in calculating rates using
# CEIP data

# stratified by age and sex and race
# subsetting census data by school district
##########################################

rm(list=ls())
source(here::here("0-config.R"))


# function that inputs a race-specific census file
# and subsets it to the cells with age- and sex-specific
# information

cens.race=function(data,male,female,race){
  male=c("D003", "D004", "D005", "D006", "D007", "D008", "D009", "D010", 
         "D011", "D012", "D013", "D014", "D015", "D016", "D017", "D018", 
         "D019", "D020", "D021", "D022", "D023", "D024", "D025")
  
  female=c("D027", "D028", "D029", "D030", "D031", "D032", "D033", "D034", 
           "D035", "D036", "D037", "D038", "D039", "D040", "D041", "D042", 
           "D043", "D044", "D045", "D046", "D047", "D048", "D049")
  
  data.out=data[,colnames(data) %in% c("GEO.display.label",male,female)]
  
  # drop the first row 
  
  # subset to relevant districts
  data.out=subset(data.out,data.out$GEO.display.label==
     "Oakland Unified School District, California" |
     data.out$GEO.display.label==
     "West Contra Costa Unified School District, California"|
     data.out$GEO.display.label=="Geography")
  
  # transpose matrix and relabel
  data.out=data.frame(t(data.out))
  colnames(data.out)=c("label","OUSD","WCCUSD")
  data.out=data.out[2:nrow(data.out),]
  rownames(data.out)=NULL
  
  data.out$label=as.character(data.out$label)
  data.out$years=apply(as.matrix(data.out$label),1,getyears)
  data.out$sex=apply(as.matrix(data.out$label),1,getsex)
  
  data.out$label=NULL
  
  data.out$OUSD=as.numeric(as.character(data.out$OUSD))
  data.out$WCCUSD=as.numeric(as.character(data.out$WCCUSD))
  
  data.out=data.out[,c("years","sex","OUSD","WCCUSD")]
  
  data.out$race=race
  
  return(data.out)
}

#-----------------------------------------------
# this function finds the last dash in a string
# and subsets to the number of years after the dash
# and removes the word "years"
#-----------------------------------------------
getyears=function(x){
  pos=gregexpr("-",x)
  lastdash=pos[[1]][length(pos[[1]])]
  len=nchar(x)
  years=substr(x,lastdash+2,len)
  years=gsub(" years","",years)
  years=gsub(" year","",years)
  years=gsub("Under 1","0",years)
  return(years)
}

#-----------------------------------------------
# this function extracts the sex from the label
#-----------------------------------------------
getsex=function(x){
  colon=gregexpr(":",x)[[1]][1]
  sex=substr(x,1,colon-1)
  return(sex)
}

#------------------------------------------------
# Import datasets
#------------------------------------------------

cens.white.all=read.csv(paste0(raw_census_dir, "Decenniel census - age race sex/DEC_10_SF1_P12A_with_ann.csv"))
cens.black.all=read.csv(paste0(raw_census_dir, "Decenniel census - age race sex/DEC_10_SF1_P12B_with_ann.csv"))
cens.nativ.all=read.csv(paste0(raw_census_dir, "Decenniel census - age race sex/DEC_10_SF1_P12C_with_ann.csv"))
cens.asian.all=read.csv(paste0(raw_census_dir, "Decenniel census - age race sex/DEC_10_SF1_P12D_with_ann.csv"))
cens.PI.all=read.csv(paste0(raw_census_dir, "Decenniel census - age race sex/DEC_10_SF1_P12E_with_ann.csv"))
cens.other.all=read.csv(paste0(raw_census_dir, "Decenniel census - age race sex/DEC_10_SF1_P12F_with_ann.csv"))
cens.multi.all=read.csv(paste0(raw_census_dir, "Decenniel census - age race sex/DEC_10_SF1_P12G_with_ann.csv"))
cens.latino.all=read.csv(paste0(raw_census_dir, "Decenniel census - age race sex/DEC_10_SF1_P12H_with_ann.csv"))
cens.whitenotL.all=read.csv(paste0(raw_census_dir, "Decenniel census - age race sex/DEC_10_SF1_P12I_with_ann.csv"))

#------------------------------------------------
# Reformat datasets
#------------------------------------------------

cens.white=cens.race(cens.white.all,race="White")
cens.black=cens.race(cens.black.all,race="Black")
cens.nativ=cens.race(cens.nativ.all,race="American Indian/Alaska Native")
cens.asian=cens.race(cens.asian.all,race="Asian")
cens.PI=cens.race(cens.PI.all,race="Native Hawaiian/Pacific Islander")
cens.other=cens.race(cens.other.all,race="Other")
cens.multi=cens.race(cens.multi.all,race="Two or more races")
cens.latino=cens.race(cens.latino.all,race="Latino")
cens.whitenotL=cens.race(cens.whitenotL.all,race="White not latino")

# combine into one data frame
cens.all=rbind(cens.white,cens.black,cens.nativ,cens.asian,cens.PI,
    cens.other,cens.multi)

# combine asian and native hawaiian/pacific islander
# to align with CEIP categorization
cens.all$race[cens.all$race=="Asian"]="API"
cens.all$race[cens.all$race=="Native Hawaiian/Pacific Islander"]="API"

cens.all=aggregate(cens.all[,c("OUSD","WCCUSD")],
    list(agecat=cens.all$years,sex=cens.all$sex,race=cens.all$race),sum)

saveRDS(cens.all,file=paste0(clean_census_dir, "Censpop-agecat-sex-race-dist.RDS"))




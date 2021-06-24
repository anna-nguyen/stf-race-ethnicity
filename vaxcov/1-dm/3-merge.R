########################################
# Merge vaccine coverage survey data with
# data on high/low performing schools
# and distressed schools

# Only includes 2016-17 data
########################################

rm(list=ls())

# define directories, load libraries
source(here::here("0-config.R"))

load(paste0(clean_data_path_2017,".RData"))

#--------------------------------------
# Read in SDI data 
#--------------------------------------
sdi=read.csv(raw_sdi_data_path)
sdi=sdi[,c("School.Name","School.SDI_studentZIP","StudentPartRate_2016")]
colnames(sdi)=c("schoolname","sdi","par2016")

sdi$schoolname=as.character(sdi$schoolname)
sdi$schoolname[sdi$schoolname=="Carl Munck Elementary"]="Carl B. Munck Elementary"
sdi$schoolname[sdi$schoolname=="Henry J. Kaiser Elementary"]="Kaiser Elementary"
sdi$schoolname[sdi$schoolname=="Think College Now Elementary"]="Think College Now"
sdi$schoolname[sdi$schoolname=="Encompass Elementary"]="EnCompass Academy Elementary"
sdi$schoolname[sdi$schoolname=="Grass Valley"]="Grass Valley Elementary"
sdi$schoolname[sdi$schoolname=="RISE Elementary"]="Rise Community"
sdi$schoolname[sdi$schoolname=="East Oakland PRIDE Elementary"]="East Oakland Pride Elementary"

#--------------------------------------
# 2014-17
#--------------------------------------
data=merge(data.import,sdi,by="schoolname",all.x=TRUE,all.y=FALSE)
data$par2016=data$par2016*100

# merge in stf participation in year 1 and 2
cov=read.csv(raw_cov_data_path,header=TRUE)
cov=cov[,c("SCHOOL","SCHOOL.TYPE","X2015...Student.Participation","X2014...Student.Participation")]
cov=cov[cov$SCHOOL.TYPE=="OUSD",]
cov$SCHOOL.TYPE=NULL
colnames(cov)=c("school","par2015","par2014")

cov$par2015len=sapply(as.character(cov$par2015),nchar)
cov$par2014len=sapply(as.character(cov$par2014),nchar)

cov$par2015f=NA
cov$par2014f=NA
for(i in 1:nrow(cov)){
  cov$par2015f[i]=substr(cov$par2015[i],1,cov$par2015len[i]-1)
  cov$par2014f[i]=substr(cov$par2014[i],1,cov$par2014len[i]-1)
}

cov$par2015=NULL
cov$par2014=NULL
cov$par2015len=NULL
cov$par2014len=NULL

cov[,"par2015f"]=as.numeric(cov[,"par2015f"])
cov[,"par2014f"]=as.numeric(cov[,"par2014f"])

colnames(cov)[1]="schoolname"

cov$schoolname=as.character(cov$schoolname)
cov$schoolname[cov$schoolname=="EnCompass Academy"]="EnCompass Academy Elementary"
cov$schoolname[cov$schoolname=="Rise Community School"]="Rise Community"
cov$schoolname[cov$schoolname=="Henry J. Kaiser Elementary"]="Kaiser Elementary"
cov$schoolname[cov$schoolname=="Think College Now Elementary"]="Think College Now"

data=merge(data,cov,by="schoolname",all.x=TRUE)
colnames(data)[which(colnames(data)=="par2014f")]="par2014"
colnames(data)[which(colnames(data)=="par2015f")]="par2015"

save(data,file=paste0(complete_data_path_2017, ".RData"))
write.csv(data,file=paste0(complete_data_path_2017,".csv"),row.names=FALSE)



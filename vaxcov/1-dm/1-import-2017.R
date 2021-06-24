########################################
# Import and clean vaccine coverage data 
# Survey conducted in March 2017
########################################
rm(list=ls())
# define directories, load libraries
source(here::here("0-config.R"))

dat=read.csv(raw_data_path_2017)

# check for errors - entered more than one choice
# for a single choice question
dat$Q2A1_err=ifelse(dat$Q2A1_1+dat$Q2A1_2+dat$Q2A1_3>1,1,0)
dat$Q2B1_err=ifelse(dat$Q2B1_1+dat$Q2B1_2+dat$Q2B1_3>1,1,0)
dat$Q2C1_err=ifelse(dat$Q2C1_1+dat$Q2C1_2+dat$Q2C1_3>1,1,0)

dat$Q2A2_err=ifelse(dat$Q2A2_1+dat$Q2A2_2+dat$Q2A2_3>1,1,0)
dat$Q2B2_err=ifelse(dat$Q2B2_1+dat$Q2B2_2+dat$Q2B2_3>1,1,0)
dat$Q2C2_err=ifelse(dat$Q2C2_1+dat$Q2C2_2+dat$Q2C2_3>1,1,0)

dat$Q2A3_err=ifelse(dat$Q2A3_1+dat$Q2A3_2+dat$Q2A3_3+dat$Q2A3_4>1,1,0)
dat$Q2B3_err=ifelse(dat$Q2B3_1+dat$Q2B3_2+dat$Q2B3_3+dat$Q2B3_4>1,1,0)
dat$Q2C3_err=ifelse(dat$Q2C3_1+dat$Q2C3_2+dat$Q2C3_3+dat$Q2C3_4>1,1,0)

dat$Q3_err=ifelse(dat$Q3_1+dat$Q3_2+dat$Q3_3+dat$Q3_4>1,1,0)

dat$Q5_err=ifelse(dat$Q5_1+dat$Q5_2+dat$Q5_3+dat$Q5_4>1,1,0)

# create edited variables accounting for errors
# 9 = don't know/missing
# 8 = entered more than one choice (error)
dat$Q2A1_clean=9
dat$Q2A1_clean[dat$Q2A1_1==1]=1
dat$Q2A1_clean[dat$Q2A1_2==1]=0
dat$Q2A1_clean[dat$Q2A1_3==1]=9
dat$Q2A1_clean[dat$Q2A1_err==1]=8

dat$Q2B1_clean=9
dat$Q2B1_clean[dat$Q2B1_1==1]=1
dat$Q2B1_clean[dat$Q2B1_2==1]=0
dat$Q2B1_clean[dat$Q2B1_3==1]=9
dat$Q2B1_clean[dat$Q2B1_err==1]=8

dat$Q2C1_clean=9
dat$Q2C1_clean[dat$Q2C1_1==1]=1
dat$Q2C1_clean[dat$Q2C1_2==1]=0
dat$Q2C1_clean[dat$Q2C1_3==1]=9
dat$Q2C1_clean[dat$Q2C1_err==1]=8

dat$Q2A2_clean="Missing/Don't know"
dat$Q2A2_clean[dat$Q2A2_1==1]="Shot"
dat$Q2A2_clean[dat$Q2A2_2==1]="Spray"
dat$Q2A2_clean[dat$Q2A2_3==1]="Missing/Don't know"
dat$Q2A2_clean[dat$Q2A2_err==1]="Error"

dat$Q2B2_clean="Missing/Don't know"
dat$Q2B2_clean[dat$Q2B2_1==1]="Shot"
dat$Q2B2_clean[dat$Q2B2_2==1]="Spray"
dat$Q2B2_clean[dat$Q2B2_3==1]="Missing/Don't know"
dat$Q2B2_clean[dat$Q2B2_err==1]="Error"

dat$Q2C2_clean="Missing/Don't know"
dat$Q2C2_clean[dat$Q2C2_1==1]="Shot"
dat$Q2C2_clean[dat$Q2C2_2==1]="Spray"
dat$Q2C2_clean[dat$Q2C2_3==1]="Missing/Don't know"
dat$Q2C2_clean[dat$Q2C2_err==1]="Error"

dat$Q2A3_clean="Missing/Don't know"
dat$Q2A3_clean[dat$Q2A3_1==1]="School"
dat$Q2A3_clean[dat$Q2A3_2==1]="Doctor/clinic"
dat$Q2A3_clean[dat$Q2A3_3==1]="Other"
dat$Q2A3_clean[dat$Q2A3_4==1]="Missing/Don't know"
dat$Q2A3_clean[dat$Q2A3_err==1]="Error"

dat$Q2B3_clean="Missing/Don't know"
dat$Q2B3_clean[dat$Q2B3_1==1]="School"
dat$Q2B3_clean[dat$Q2B3_2==1]="Doctor/clinic"
dat$Q2B3_clean[dat$Q2B3_3==1]="Other"
dat$Q2B3_clean[dat$Q2B3_4==1]="Missing/Don't know"
dat$Q2B3_clean[dat$Q2B3_err==1]="Error"

dat$Q2C3_clean="Missing/Don't know"
dat$Q2C3_clean[dat$Q2C3_1==1]="School"
dat$Q2C3_clean[dat$Q2C3_2==1]="Doctor/clinic"
dat$Q2C3_clean[dat$Q2C3_3==1]="Other"
dat$Q2C3_clean[dat$Q2C3_4==1]="Missing/Don't know"
dat$Q2C3_clean[dat$Q2C3_err==1]="Error"

dat$Q3_clean="Missing"
dat$Q3_clean[dat$Q3_1==1]="School"
dat$Q3_clean[dat$Q3_2==1]="Doctor/clinic"
dat$Q3_clean[dat$Q3_3==1]="Other"
dat$Q3_clean[dat$Q3_4==1]="No vaccine"
dat$Q3_clean[dat$Q3_err==1]="Error"

dat$Q5_clean="Missing"
dat$Q5_clean[dat$Q5_1==1]="Less than high school"
dat$Q5_clean[dat$Q5_2==1]="High school"
dat$Q5_clean[dat$Q5_3==1]="Associate/College"
dat$Q5_clean[dat$Q5_4==1]="Postgrad"
dat$Q5_clean[dat$Q5_err==1]="Error"

colnames(dat)=tolower(colnames(dat))

data.import=dat[,c("district","school","class","child","language","q1",
           "q2a1_clean","q2b1_clean","q2c1_clean",
           "q2a2_clean","q2b2_clean","q2c2_clean",
           "q2a3_clean","q2b3_clean","q2c3_clean",
           "q3_clean","q4_1","q4_2","q4_3","q4_4","q4_5","q4_6",
           "q5_clean")]

colnames(data.import)=c("district","school","class","child","language",
                 "grade","vx1617","vx1516","vx1415",
                 "vxtype1617","vxtype1516","vxtype1415",
                 "vxloc1617","vxloc1516","vxloc1415",
                 "vxplan","race_natamer","race_pi","race_asian",
                 "race_white","race_black","race_lat",
                 "edu")

# code variables
data.import$district=ifelse(data.import$district==1,"OUSD","WCCUSD")
data.import$language=ifelse(data.import$language==1,"English","Spanish")

# get school names
sch=read.csv("~/Dropbox/Flu/StFData/2016-2017/Data/Raw/ShootheFlu_SchoolandClassCodes.csv")
colnames(sch)=c("districtno","school","range","district","schoolname")
sch=sch[,c("district","school","schoolname")]

data.import$multi=data.import$race_natamer + data.import$race_pi + data.import$race_asian + data.import$race_white +
  data.import$race_black + data.import$race_lat

data.import$race = "Missing"
data.import$race[data.import$race_natamer==1 & data.import$race_pi==0 & data.import$race_asian==0 &
            data.import$race_white==0 & data.import$race_black==0 & data.import$race_lat==0] ="Native American"
data.import$race[data.import$race_natamer==0 & data.import$race_pi==1 & data.import$race_asian==0 &
            data.import$race_white==0 & data.import$race_black==0 & data.import$race_lat==0] ="Pacific islander"
data.import$race[data.import$race_natamer==0 & data.import$race_pi==0 & data.import$race_asian==1 &
            data.import$race_white==0 & data.import$race_black==0 & data.import$race_lat==0] = "Asian"
data.import$race[data.import$race_natamer==0 & data.import$race_pi==0 & data.import$race_asian==0 &
            data.import$race_white==1 & data.import$race_black==0 & data.import$race_lat==0] = "White"
data.import$race[data.import$race_natamer==0 & data.import$race_pi==0 & data.import$race_asian==0 &
            data.import$race_white==0 & data.import$race_black==1 & data.import$race_lat==0] = "Black"
data.import$race[data.import$race_natamer==0 & data.import$race_pi==0 & data.import$race_asian==0 &
            data.import$race_white==0 & data.import$race_black==0 & data.import$race_lat==1] = "Latino"
data.import$race[data.import$multi>1] = "Multi"
data.import$race=factor(data.import$race,levels=c("White","Black","Latino","Asian",
        "Native American","Pacific islander","Multi","Missing"))

data.import$edu=factor(data.import$edu,levels=c("Less than high school","High school",
             "Associate/College","Postgrad","Missing","Error"))

data.import=merge(data.import, sch, by=c("district","school"))

# get matchids

# create indicator for whether school is in matched pair 
data.import$matchid=NA
data.import$matchid[data.import$schoolname=="Horace Mann Elementary"]=1
data.import$matchid[data.import$schoolname=="Emerson Elementary"]=2
data.import$matchid[data.import$schoolname=="Laurel Elementary"]=3 
data.import$matchid[data.import$schoolname=="Reach Academy"]=4 
data.import$matchid[data.import$schoolname=="Carl B. Munck Elementary"]=5 
data.import$matchid[data.import$schoolname=="Joaquin Miller Elementary"]=6 
data.import$matchid[data.import$schoolname=="Chabot Elementary"]=7 
data.import$matchid[data.import$schoolname=="Lafayette Elementary"]=8 
data.import$matchid[data.import$schoolname=="Crocker Highlands Elementary"]=9 
data.import$matchid[data.import$schoolname=="Fruitvale Elementary"]=10 
data.import$matchid[data.import$schoolname=="Burckhalter Elementary"]=11 
data.import$matchid[data.import$schoolname=="Montclair Elementary"]=12 
data.import$matchid[data.import$schoolname=="Fred T. Korematsu Discovery Academy"]=13 
data.import$matchid[data.import$schoolname=="Allendale Elementary"]=14 
data.import$matchid[data.import$schoolname=="Glenview Elementary"]=15 
data.import$matchid[data.import$schoolname=="Brookfield Elementary"]=16 
data.import$matchid[data.import$schoolname=="EnCompass Academy Elementary"]=17 
data.import$matchid[data.import$schoolname=="Grass Valley Elementary"]=18 
data.import$matchid[data.import$schoolname=="East Oakland Pride Elementary"]=19 
data.import$matchid[data.import$schoolname=="Bella Vista Elementary"]=20 
data.import$matchid[data.import$schoolname=="Community United Elementary"]=21 
data.import$matchid[data.import$schoolname=="Cleveland Elementary"]=22 
data.import$matchid[data.import$schoolname=="Think College Now"]=23 
data.import$matchid[data.import$schoolname=="Markham Elementary"]=24 
data.import$matchid[data.import$schoolname=="Garfield Elementary"]=25 
data.import$matchid[data.import$schoolname=="Franklin Elementary"]=26 
data.import$matchid[data.import$schoolname=="La Escuelita Elementary"]=27 
data.import$matchid[data.import$schoolname=="Rise Community"]=28  
data.import$matchid[data.import$schoolname=="Kaiser Elementary"]=29 
data.import$matchid[data.import$schoolname=="Howard Elementary"]=30 
data.import$matchid[data.import$schoolname=="Global Family School"]=31 
data.import$matchid[data.import$schoolname=="Parker Elementary"]=32 
data.import$matchid[data.import$schoolname=="Bridges Academy"]=33 
data.import$matchid[data.import$schoolname=="Manzanita Community School"]=34 
data.import$matchid[data.import$schoolname=="Sheldon Elementary"]=1 
data.import$matchid[data.import$schoolname=="Shannon Elementary"]=2 
data.import$matchid[data.import$schoolname=="Tara Hills Elementary"]=3 
data.import$matchid[data.import$schoolname=="Lake Elementary"]=4 
data.import$matchid[data.import$schoolname=="Ohlone Elementary"]=5 
data.import$matchid[data.import$schoolname=="Montalvin Manor Elementary"]=6 
data.import$matchid[data.import$schoolname=="Kensington Elementary"]=7 
data.import$matchid[data.import$schoolname=="Stege Elementary"]=8 
data.import$matchid[data.import$schoolname=="Harding Elementary"]=9 
data.import$matchid[data.import$schoolname=="Highland Elementary"]=10 
data.import$matchid[data.import$schoolname=="Collins Elementary"]=11 
data.import$matchid[data.import$schoolname=="Peres Elementary"]=12 
data.import$matchid[data.import$schoolname=="Wilson Elementary"]=13 
data.import$matchid[data.import$schoolname=="Murphy Elementary"]=14 
data.import$matchid[data.import$schoolname=="Valley View Elementary"]=15 
data.import$matchid[data.import$schoolname=="Ford Elementary"]=16 
data.import$matchid[data.import$schoolname=="Riverside Elementary"]=17 
data.import$matchid[data.import$schoolname=="Lupine Hills Elementary"]=18 
data.import$matchid[data.import$schoolname=="Verde Elementary"]=19 
data.import$matchid[data.import$schoolname=="Olinda Elementary"]=20 
data.import$matchid[data.import$schoolname=="Grant Elementary"]=21 
data.import$matchid[data.import$schoolname=="Madera Elementary"]=22 
data.import$matchid[data.import$schoolname=="Coronado Elementary"]=23 
data.import$matchid[data.import$schoolname=="Nystrom Elementary"]=24 
data.import$matchid[data.import$schoolname=="Dover Elementary"]=25 
data.import$matchid[data.import$schoolname=="Fairmont Elementary"]=26 
data.import$matchid[data.import$schoolname=="Hanna Ranch Elementary"]=27 
data.import$matchid[data.import$schoolname=="King Elementary"]=28 
data.import$matchid[data.import$schoolname=="Washington Elementary"]=29 
data.import$matchid[data.import$schoolname=="Ellerhorst Elementary"]=30 
data.import$matchid[data.import$schoolname=="Downer Elementary"]=31 
data.import$matchid[data.import$schoolname=="Lincoln Elementary" & data.import$district=="WCCUSD"]=32 
	data.import$matchid[data.import$schoolname=="Chavez Elementary"]=33  
data.import$matchid[data.import$schoolname=="Bayview Elementary"]=34 

race_columns = data.import %>% dplyr::select(starts_with("race_"))
data.import=data.import[,c(1:2,26:27,3:16,23,25)]

data.import = cbind(data.import, race_columns) 
save(data.import,file=paste0(clean_data_path_2017,".RData"))
write.csv(data.import,file=paste0(clean_data_path_2017,".csv"),row.names=FALSE)

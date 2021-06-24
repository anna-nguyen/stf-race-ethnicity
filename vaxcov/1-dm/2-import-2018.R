########################################
# Import and clean vaccine coverage data 
# Survey conducted in March 2017
########################################
rm(list=ls())

# define directories, load libraries
source(here::here("0-config.R"))

o=read.csv(raw_ousd_data_path_2017)
w=read.csv(raw_wcc_data_path_2017)

w=w %>%
  mutate(Q3_5=NA, Q3_10=NA, Q3_11=NA, Q3_12=NA)

dat=rbind(o,w)

# check for errors - entered more than one choice
# for a single choice question
dat$Q2A1_err=ifelse(dat$Q2A1_1+dat$Q2A1_2+dat$Q2A1_3>1,1,0)
dat$Q2B1_err=ifelse(dat$Q2B1_1+dat$Q2B1_2+dat$Q2B1_3>1,1,0)
dat$Q2C1_err=ifelse(dat$Q2C1_1+dat$Q2C1_2+dat$Q2C1_3>1,1,0)

dat$Q2A3_err=ifelse(dat$Q2A3_1+dat$Q2A3_2+dat$Q2A3_3+dat$Q2A3_4>1,1,0)
dat$Q2B3_err=ifelse(dat$Q2B3_1+dat$Q2B3_2+dat$Q2B3_3+dat$Q2B3_4>1,1,0)
dat$Q2C3_err=ifelse(dat$Q2C3_1+dat$Q2C3_2+dat$Q2C3_3+dat$Q2C3_4>1,1,0)

dat$Q3_err=ifelse(dat$Q3_1+dat$Q3_2+dat$Q3_3+dat$Q3_4 +
      dat$Q3_5+dat$Q3_6+dat$Q3_7+dat$Q3_8+dat$Q3_9+
      dat$Q3_10+dat$Q3_11+dat$Q3_12+dat$Q3_13>2,1,0)

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
dat$Q3_clean[dat$Q3_1==1]="I don’t believe in it"
dat$Q3_clean[dat$Q3_2==1]="It costs too much"
dat$Q3_clean[dat$Q3_3==1]="My student is afraid of needles"
dat$Q3_clean[dat$Q3_4==1]="I didn’t know where to get it"
dat$Q3_clean[dat$Q3_5==1]="I didn’t trust schools to vaccinate my student"
dat$Q3_clean[dat$Q3_6==1]="Our doctor did not recommend it"
dat$Q3_clean[dat$Q3_7==1]="I didn’t have time to take my student to the doctor"
dat$Q3_clean[dat$Q3_8==1]="I believe it might make my student sick"
dat$Q3_clean[dat$Q3_9==1]="I thought my student needed health insurance to get it"
dat$Q3_clean[dat$Q3_10==1]="I didn’t receive the consent form to get the vaccine at school"
dat$Q3_clean[dat$Q3_11==1]="I forgot to return the consent form to get the vaccine at school"
dat$Q3_clean[dat$Q3_12==1]="I didn’t want to share my insurance information on the consent form to get the vaccine at school"

dat <- dat %>%
  mutate(Q3tot=Q3_1+Q3_2+Q3_3+Q3_4+Q3_5+Q3_6+Q3_7+Q3_8+
           Q3_9+Q3_10+Q3_11+Q3_12)
table(dat$Q3tot)

dat$multi=dat$Q4_1 + dat$Q4_2 + dat$Q4_3 + dat$Q4_4 +
  dat$Q4_5 + dat$Q4_6

dat$Q4_clean = "Missing"
dat$Q4_clean[dat$Q4_1==1 & dat$Q4_2==0 & dat$Q4_3==0 &
               dat$Q4_4==0 & dat$Q4_5==0 & dat$Q4_6==0] ="Native American"
dat$Q4_clean[dat$Q4_1==0 & dat$Q4_2==1 & dat$Q4_3==0 &
               dat$Q4_4==0 & dat$Q4_5==0 & dat$Q4_6==0] ="Pacific islander"
dat$Q4_clean[dat$Q4_1==0 & dat$Q4_2==0 & dat$Q4_3==1 &
               dat$Q4_4==0 & dat$Q4_5==0 & dat$Q4_6==0] = "Asian"
dat$Q4_clean[dat$Q4_1==0 & dat$Q4_2==0 & dat$Q4_3==0 &
               dat$Q4_4==1 & dat$Q4_5==0 & dat$Q4_6==0] = "White"
dat$Q4_clean[dat$Q4_1==0 & dat$Q4_2==0 & dat$Q4_3==0 &
               dat$Q4_4==0 & dat$Q4_5==1 & dat$Q4_6==0] = "Black"
dat$Q4_clean[dat$Q4_1==0 & dat$Q4_2==0 & dat$Q4_3==0 &
               dat$Q4_4==0 & dat$Q4_5==0 & dat$Q4_6==1] = "Latino" 

dat = dat %>% rename("race_natamer" = Q4_1,
                     "race_pi" = Q4_2, 
                     "race_asian" = Q4_3,
                     "race_white" = Q4_4, 
                     "race_black" = Q4_5,
                     "race_lat" = Q4_6)

dat$Q4_clean[dat$multi>1] = "Multi"
dat$Q4_clean=factor(dat$Q4_clean,levels=c("White","Black","Latino","Asian",
                                          "Native American","Pacific islander","Multi","Missing"))

dat$Q5_clean="Missing"
dat$Q5_clean[dat$Q5_1==1]="Less than high school"
dat$Q5_clean[dat$Q5_2==1]="High school"
dat$Q5_clean[dat$Q5_3==1]="Associate/College"
dat$Q5_clean[dat$Q5_4==1]="Postgrad"
dat$Q5_clean[dat$Q5_err==1]="Error"

colnames(dat)=tolower(colnames(dat))

data.import = dat %>%
  dplyr::select(c(district, school, class, child, language,
           q1, 
           vx1718=q2a1_clean, vx1617_18=q2b1_clean, vx1516_18=q2c1_clean,
           vxloc1718=q2a3_clean, vxloc1617_18=q2b3_clean, vxloc1516_18=q2c3_clean, 
           whynot=q3_clean, race=q4_clean, edu=q5_clean, 
           whynot_1=q3_1, whynot_2=q3_2, whynot_3=q3_3, 
           whynot_4=q3_4, whynot_5=q3_5, whynot_6=q3_6, 
           whynot_7=q3_7, whynot_8=q3_8, whynot_9=q3_9,
           whynot_10=q3_10, whynot_11=q3_11, whynot_12=q3_12), starts_with("race_"))

# code variables
data.import$district=ifelse(data.import$district==1,"OUSD","WCCUSD")
data.import$language=ifelse(data.import$language==1,"English","Spanish")

# get school names
sch=read.csv(school_names_data_path)
colnames(sch)=c("districtno","school","range","district","schoolname")
sch=sch[,c("district","school","schoolname")]

data.import$edu=factor(data.import$edu,levels=c("Less than high school","High school",
             "Associate/College","Postgrad","Missing"))

data.import=data.import %>% mutate(district=as.factor(district))
data.import=left_join(data.import, sch, by=c("district","school"))

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

save(data.import,file=paste0(clean_data_path_2018,".RData"))
write.csv(data.import,file=paste0(clean_data_path_2018,".csv"),row.names=FALSE)

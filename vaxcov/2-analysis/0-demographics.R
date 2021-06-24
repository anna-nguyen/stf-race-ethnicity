########################################
# Vaccine coverage data 
# Survey conducted in March 2017

# Compare demographics in survey
# to those in overall district
########################################
rm(list=ls())

# define directories, load libraries
source(here::here("0-config.R"))

load(paste0(complete_data_path_2017,".RData"))

table(data$district)
table(data$grade)

# check demographics
table(data$edu,data$dist,useNA="ifany")
sample.edu=prop.table(table(data$edu,data$dist),2)*100

table(data$race,data$dist,useNA="ifany")
sample.race=prop.table(table(data$race,data$dist),2)*100

# compare to overall district demographics ------------------------
load(school_match_data_path)

nr.oak=mean(school.data["per.Not reported"][school.data$district=="Oakland Unified",])
nr.wcc=mean(school.data["per.Not reported"][school.data$district=="West Contra Costa Unified",])

nat.oak=mean(school.data["per.NativeAmer"][school.data$district=="Oakland Unified",])
nat.wcc=mean(school.data["per.NativeAmer"][school.data$district=="West Contra Costa Unified",])

asi.oak=mean(school.data["per.Asian"][school.data$district=="Oakland Unified",])
asi.wcc=mean(school.data["per.Asian"][school.data$district=="West Contra Costa Unified",])

pi.oak=mean(school.data["per.PI"][school.data$district=="Oakland Unified",])
pi.wcc=mean(school.data["per.PI"][school.data$district=="West Contra Costa Unified",])

fil.oak=mean(school.data["per.Filipino"][school.data$district=="Oakland Unified",])
fil.wcc=mean(school.data["per.Filipino"][school.data$district=="West Contra Costa Unified",])

his.oak=mean(school.data["per.Hispanic"][school.data$district=="Oakland Unified",])
his.wcc=mean(school.data["per.Hispanic"][school.data$district=="West Contra Costa Unified",])

af.oak=mean(school.data["per.AfAmer"][school.data$district=="Oakland Unified",])
af.wcc=mean(school.data["per.AfAmer"][school.data$district=="West Contra Costa Unified",])

whi.oak=mean(school.data["per.White"][school.data$district=="Oakland Unified",])
whi.wcc=mean(school.data["per.White"][school.data$district=="West Contra Costa Unified",])

mul.oak=mean(school.data["per.Twoplus"][school.data$district=="Oakland Unified",])
mul.wcc=mean(school.data["per.Twoplus"][school.data$district=="West Contra Costa Unified",])

oak=c(whi.oak,af.oak,his.oak,asi.oak+fil.oak,nat.oak,pi.oak, mul.oak,nr.oak)
wcc=c(whi.wcc,af.wcc,his.wcc,asi.wcc+fil.wcc,nat.wcc,pi.wcc,mul.wcc,nr.wcc)

dist.race=data.frame(lab=c("White","African American","Latino","Asian","Native American",
                           "Pacific Islander","Multiple","Not reported"),oak=oak,wcc=wcc)

write.csv(dist.race, file = paste0(local_res_path, "/dist_race.csv"))

nothsg.oak=mean(school.data["per.not_hsg"][school.data$district=="Oakland Unified",])
nothsg.wcc=mean(school.data["per.not_hsg"][school.data$district=="West Contra Costa Unified",])

hsg.oak=mean(school.data["per.hsg"][school.data$district=="Oakland Unified",])
hsg.wcc=mean(school.data["per.hsg"][school.data$district=="West Contra Costa Unified",])

some_col.oak=mean(school.data["per.some_col"][school.data$district=="Oakland Unified",])
some_col.wcc=mean(school.data["per.some_col"][school.data$district=="West Contra Costa Unified",])

col.oak=mean(school.data["per.col_grad"][school.data$district=="Oakland Unified",])
col.wcc=mean(school.data["per.col_grad"][school.data$district=="West Contra Costa Unified",])

grad.oak=mean(school.data["per.grad_sch"][school.data$district=="Oakland Unified",])
grad.wcc=mean(school.data["per.grad_sch"][school.data$district=="West Contra Costa Unified",])

dist.edu=data.frame(lab=c("Less than high school","High school graduate",
                          "College graduate","Graduate school"),
                    oak=c(nothsg.oak,hsg.oak,some_col.oak+col.oak,grad.oak),
                    wcc=c(nothsg.wcc,hsg.wcc,some_col.wcc+col.wcc,grad.wcc))

write.csv(dist.race, file = paste0(local_res_path, "/dist_edu.csv"))


# compare to sampled school overall demographics ------------------------
svy.schools=names(table(data$schoolname))

school.data$sampled=ifelse(school.data$school %in% svy.schools,1,0)
school.data$sampled[school.data$school=="Fred T. Korematsu Discovery Academy"]=1
school.data$school=droplevels(school.data$school)

nr.oak.s=mean(school.data["per.Not reported"][school.data$district=="Oakland Unified" & school.data$sampled==1,])
nr.wcc.s=mean(school.data["per.Not reported"][school.data$district=="West Contra Costa Unified" & school.data$sampled==1,])

nat.oak.s=mean(school.data["per.NativeAmer"][school.data$district=="Oakland Unified" & school.data$sampled==1,])
nat.wcc.s=mean(school.data["per.NativeAmer"][school.data$district=="West Contra Costa Unified" & school.data$sampled==1,])

asi.oak.s=mean(school.data["per.Asian"][school.data$district=="Oakland Unified" & school.data$sampled==1,])
asi.wcc.s=mean(school.data["per.Asian"][school.data$district=="West Contra Costa Unified" & school.data$sampled==1,])

pi.oak.s=mean(school.data["per.PI"][school.data$district=="Oakland Unified" & school.data$sampled==1,])
pi.wcc.s=mean(school.data["per.PI"][school.data$district=="West Contra Costa Unified" & school.data$sampled==1,])

fil.oak.s=mean(school.data["per.Filipino"][school.data$district=="Oakland Unified" & school.data$sampled==1,])
fil.wcc.s=mean(school.data["per.Filipino"][school.data$district=="West Contra Costa Unified" & school.data$sampled==1,])

his.oak.s=mean(school.data["per.Hispanic"][school.data$district=="Oakland Unified" & school.data$sampled==1,])
his.wcc.s=mean(school.data["per.Hispanic"][school.data$district=="West Contra Costa Unified" & school.data$sampled==1,])

af.oak.s=mean(school.data["per.AfAmer"][school.data$district=="Oakland Unified" & school.data$sampled==1,])
af.wcc.s=mean(school.data["per.AfAmer"][school.data$district=="West Contra Costa Unified" & school.data$sampled==1,])

whi.oak.s=mean(school.data["per.White"][school.data$district=="Oakland Unified" & school.data$sampled==1,])
whi.wcc.s=mean(school.data["per.White"][school.data$district=="West Contra Costa Unified" & school.data$sampled==1,])

mul.oak.s=mean(school.data["per.Twoplus"][school.data$district=="Oakland Unified" & school.data$sampled==1,])
mul.wcc.s=mean(school.data["per.Twoplus"][school.data$district=="West Contra Costa Unified" & school.data$sampled==1,])

oak.s=c(whi.oak.s,af.oak.s,his.oak.s,asi.oak+fil.oak.s,nat.oak.s,pi.oak.s, mul.oak.s,nr.oak.s)
wcc.s=c(whi.wcc.s,af.wcc.s,his.wcc.s,asi.wcc+fil.wcc.s,nat.wcc.s,pi.wcc.s,mul.wcc.s,nr.wcc.s)

dist.race.s=data.frame(lab=c("White","African American","Latino","Asian","Native American",
                             "Pacific Islander","Multiple","Not reported"),oak=oak.s,wcc=wcc.s)

write.csv(dist.race.s, file = paste0(local_res_path, "/sample_race.csv"))

nothsg.oak.s=mean(school.data["per.not_hsg"][school.data$district=="Oakland Unified" & school.data$sampled==1,])
nothsg.wcc.s=mean(school.data["per.not_hsg"][school.data$district=="West Contra Costa Unified" & school.data$sampled==1,])

hsg.oak.s=mean(school.data["per.hsg"][school.data$district=="Oakland Unified" & school.data$sampled==1,])
hsg.wcc.s=mean(school.data["per.hsg"][school.data$district=="West Contra Costa Unified" & school.data$sampled==1,])

some_col.oak.s=mean(school.data["per.some_col"][school.data$district=="Oakland Unified" & school.data$sampled==1,])
some_col.wcc.s=mean(school.data["per.some_col"][school.data$district=="West Contra Costa Unified" & school.data$sampled==1,])

col.oak.s=mean(school.data["per.col_grad"][school.data$district=="Oakland Unified" & school.data$sampled==1,])
col.wcc.s=mean(school.data["per.col_grad"][school.data$district=="West Contra Costa Unified" & school.data$sampled==1,])

grad.oak.s=mean(school.data["per.grad_sch"][school.data$district=="Oakland Unified" & school.data$sampled==1,])
grad.wcc.s=mean(school.data["per.grad_sch"][school.data$district=="West Contra Costa Unified" & school.data$sampled==1,])

dist.edu.s=data.frame(lab=c("Less than high school","High school graduate",
                            "College graduate","Graduate school"),
                      oak=c(nothsg.oak.s,hsg.oak.s,some_col.oak.s+col.oak.s,grad.oak.s),
                      wcc=c(nothsg.wcc.s,hsg.wcc.s,some_col.wcc.s+col.wcc.s,grad.wcc.s))

write.csv(dist.edu.s, file = paste0(local_res_path, "/sample_edu.csv"))

save(dist.race,dist.race.s, dist.edu, dist.edu.s, 
     file=district_demographics_path)

colnames(dist.race) = c("Race/Ethnicity", "OUSD - District", "WCCUSD - District")
colnames(dist.race.s) = c("Race/Ethnicity", "OUSD - Sample", "WCCUSD - Sample")

race = dist.race %>% merge(dist.race.s, by = "Race/Ethnicity") %>% 
            dplyr::select("Race/Ethnicity", "OUSD - District", "OUSD - Sample", "WCCUSD - District", "WCCUSD - Sample")

write.csv(race, file = paste0(local_res_path, "/race_demog.csv"))

colnames(dist.edu) = c("Parent Education Level", "OUSD - District", "WCCUSD - District")
colnames(dist.edu.s) = c("Parent Education Level", "OUSD - Sample", "WCCUSD - Sample")

edu = dist.edu %>% merge(dist.edu.s, by = "Parent Education Level") %>% 
  dplyr::select("Parent Education Level", "OUSD - District", "OUSD - Sample", "WCCUSD - District", "WCCUSD - Sample")

write.csv(edu, file = paste0(local_res_path, "/edu_demog.csv"))

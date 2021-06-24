##########################################
# Shoo the Flu evaluation
# Surveillance analysis

# Import and clean CEIP data
# influenza outcomes
##########################################
rm(list=ls())

source(here::here("0-config.R"))

flu0811 = read.csv(paste0(raw_data_dir, "2008-2011/CEIP_Shoo the flu_2008.11_updated 4.4.19.csv"), header=TRUE, stringsAsFactors = FALSE)
flu1114=read.csv(paste0(raw_data_dir, "BAFS_toJB_with icu death_053116.csv"),header=TRUE)
flu1516=read.csv(paste0(raw_data_dir, "CEIP_shoo the Flu_2015_2016_fnl.csv"), header=TRUE)
flu1617=read.csv(paste0(raw_data_dir, "2017/CEIP_shoo the Flu_2016_2017_fnl.csv"), header=TRUE)
flu1718=read.csv(paste0(raw_data_dir, "2017-18/CEIP_shootheflu_2017.18_040419.csv"), header=TRUE)

cdph = readRDS(paste0(data_dir, "cdph_season.RDS"))

# ---------------------------------------
# Clean 2008-2010 - create / clean variables to match later data
# ---------------------------------------
colnames(flu0811)[which(colnames(flu0811)=="AgeYr")]="ageyrs"
colnames(flu0811)[which(colnames(flu0811)=="AIndAlsk")]="aindalsk"
colnames(flu0811)[which(colnames(flu0811)=="MultiRace")]="multirace"
colnames(flu0811)[which(colnames(flu0811)=="RaceUnk")]="raceunk"

flu0811$zip = as.character(flu0811$zip)
flu0811$API = ifelse(flu0811$Asian==1 | flu0811$HPacIsl==1,1,NA)
flu0811$Asian = NULL
flu0811$HPacIsl = NULL
flu0811$Culture = ifelse(flu0811$TstTyp_fnl==3, 1, 0)
flu0811$Unknowntest = ifelse(flu0811$TstTyp_fnl==6, 1, 0)
flu0811$PCR = ifelse(flu0811$TstTyp_fnl==2, 1, 0)
flu0811$Rapid = ifelse(flu0811$TstTyp_fnl==1, 1, 0)
flu0811$Rap_Name = NULL
flu0811$PosTstDt = NULL
flu0811$Flu_Type = NULL
flu0811$ASubtype = NULL
flu0811$flupos=ifelse(flu0811$TstRes_fnl==10,0,1)

# only one kind of test was included, 
# so assigning flupsens NA
flu0811$flupsens = NA

flu0811 = flu0811 %>%
  mutate(TstTyp_fnl = NA,
         Sample = NA,
         Rapid_molecular = NA)


# ---------------------------------------
# Clean 2014-16
# ---------------------------------------
colnames(flu1516)[which(colnames(flu1516)=="AIndAlsk")]="aindalsk"
colnames(flu1516)[which(colnames(flu1516)=="MultiRace")]="multirace"
colnames(flu1516)[which(colnames(flu1516)=="RaceUnk")]="raceunk"

colnames(flu1617)[which(colnames(flu1617)=="zip5")]="zip"
colnames(flu1617)[which(colnames(flu1617)=="AIndAlsk")]="aindalsk"
colnames(flu1617)[which(colnames(flu1617)=="MultiRace")]="multirace"
colnames(flu1617)[which(colnames(flu1617)=="RaceUnk")]="raceunk"

flu1617$flusesn=1617

flu1516=flu1516[,colnames(flu1114)]
flu1617=flu1617[,colnames(flu1114)]

flu1617$zip=as.character(flu1617$zip)

flu1117=rbind(flu1114,flu1516,flu1617)

flu1117$PCR = ifelse(flu1117$TstTyp_fnl==2, 1, 0)
flu1117$Rapid = ifelse(flu1117$TstTyp_fnl==1, 1, 0)
flu1117$Culture = ifelse(flu1117$TstTyp_fnl==3, 1, 0)
flu1117$Unknowntest = ifelse(flu1117$TstTyp_fnl==7, 1, 0)
flu1117$DOB = NULL

flu1117$ageyrs = ifelse(flu1117$AgeUnit==1, flu1117$Age, 0)
flu1117$Age = NULL
flu1117$AgeUnit = NULL

flu1117$PosTstDt_fnl = as.character(flu1117$PosTstDt_fnl)


# NOTE: the 2008-2010 data did not include multiple tests
flu1117$flupos=ifelse(flu1117$TstRes_fnl==10,0,1)
flu1117$flupos1=ifelse(flu1117$TstRes1==10,0,1)
flu1117$flupos2=ifelse(flu1117$TstRes2==10,0,1)
flu1117$flupos3=ifelse(flu1117$TstRes3==10,0,1)

# flu positives based on sensitivity of test
# Note: this code is specific to this data. If run 
# on additional rows, it must be checked to make sure
# all logical cases are considered. 

# this code applies only to 2011-12 onward: 
flu1117$flupsens=flu1117$flupos
# test1 rapid pos, test2 pcr neg, no test3
flu1117$flupsens[flu1117$flupos1==1 & flu1117$flupos2==0 & !is.na(flu1117$flupos2) & 
                   is.na(flu1117$flupos3) & flu1117$TstTyp1==1 & flu1117$TstTyp2==2]=0
# test1 rapid pos, test2 pcr pos, test3 neg
flu1117$flupsens[flu1117$flupos1==1 & flu1117$flupos2==1 & !is.na(flu1117$flupos2) & 
                   flu1117$flupos3==0 & !is.na(flu1117$flupos3) & 
                   flu1117$TstTyp3==2]=0
# test1 rapid pos, test2 pcr neg, test3 rapid neg
flu1117$flupsens[flu1117$flupos1==1 & flu1117$flupos2==0 & !is.na(flu1117$flupos2) & 
                   flu1117$flupos3==0 & !is.na(flu1117$flupos3) & 
                   flu1117$TstTyp1==1 & flu1117$TstTyp2==2 & flu1117$TstTyp3==2]=0
# test1 rapid pos, test2 pcr neg, test3 rapid neg
flu1117$flupsens[flu1117$flupos1==1 & flu1117$flupos2==0 & !is.na(flu1117$flupos2) & 
                   flu1117$flupos3==0 & !is.na(flu1117$flupos3) & 
                   flu1117$TstTyp1==1 & flu1117$TstTyp2==2 & flu1117$TstTyp3==1]=0
# test1 rapid pos, test2 pcr neg, test3 pcr neg
flu1117$flupsens[flu1117$flupos1==1 & flu1117$flupos2==0 & !is.na(flu1117$flupos2) & 
                   flu1117$flupos3==0 & !is.na(flu1117$flupos3) & 
                   flu1117$TstTyp1==1 & flu1117$TstTyp2==2 & flu1117$TstTyp3==2]=0
# test1 pcr pos, test2 pcr pos, test3 pcr neg
flu1117$flupsens[flu1117$flupos1==1 & flu1117$flupos2==1 & !is.na(flu1117$flupos2) & 
                   flu1117$flupos3==0 & !is.na(flu1117$flupos3) & 
                   flu1117$TstTyp1==2 & flu1117$TstTyp2==2 & flu1117$TstTyp3==2]=1

flu1117 = flu1117 %>% select(-c(flupos1, flupos2, flupos3,
                                TstTyp1, TstTyp2, TstTyp3, 
                                TstRes1, TstRes2, TstRes3, 
                                PosTstDt1, PosTstDt2, PosTstDt3))

flu1117 = flu1117 %>% mutate(Sample = NA,
                             Rapid_molecular = NA)

# ---------------------------------------
# Clean 2017-18 data
# ---------------------------------------
colnames(flu1718)[which(colnames(flu1718)=="Aindalsk")]="aindalsk"
colnames(flu1718)[which(colnames(flu1718)=="zip5")] = "zip"
colnames(flu1718)[which(colnames(flu1718)=="MultiRace")] = "multirace"
colnames(flu1718)[which(colnames(flu1718)=="RaceUnk")] = "raceunk"

flu1718$ageyrs = ifelse(flu1718$AgeUnit==1, flu1718$Age, 0)
flu1718$ageyrs = ifelse(flu1718$AgeUnit>1, 0, flu1718$Age)
flu1718$Age = NULL
flu1718$AgeUnit = NULL
flu1718$flusesn = 1718
flu1718$zip=as.character(flu1718$zip)
flu1718$LOS = NULL

flu1718$mmwr = NULL
flu1718$HospLab3 = NULL

# NOTE: the 2008-2010 data did not include multiple tests
flu1718$flupos=ifelse(flu1718$TstRes_fnl==10,0,1)
flu1718$flupos1=ifelse(flu1718$TstRes1==10,0,1)
flu1718$flupos2=ifelse(flu1718$TstRes2==10,0,1)
flu1718$flupos3=ifelse(flu1718$TstRes3==10,0,1)

# variable to indicate which test was 
# most sensitive
flu1718 = flu1718 %>%
  mutate(senstest = case_when(
    TstTyp1 == 2 &                    (TstTyp2 != 2 | is.na(TstTyp2)) & (TstTyp3 != 2 | is.na(TstTyp3)) ~ 1,
    (TstTyp1 != 2 | is.na(TstTyp1)) & TstTyp2 == 2 &                    (TstTyp3 != 2 | is.na(TstTyp3)) ~ 2,
    (TstTyp1 != 2 | is.na(TstTyp1)) & (TstTyp2 != 2 | is.na(TstTyp2)) & TstTyp3 == 2 ~ 3,
    TstTyp1 == 7 &                    (TstTyp2 != 2 | is.na(TstTyp2)) & (TstTyp3 != 2 | is.na(TstTyp3)) ~ 1,
    (TstTyp1 != 2 | is.na(TstTyp1)) & TstTyp2 == 7 &                    (TstTyp3 != 2 | is.na(TstTyp3)) ~ 2,
    (TstTyp1 != 2 | is.na(TstTyp1)) & (TstTyp2 != 2 | is.na(TstTyp2)) & TstTyp3 == 7 ~ 3,
    TstTyp1 == 1 &                     is.na(TstTyp2)                & is.na(TstTyp3) ~ 1,
    
    # if more than one of the same test performed, classify as 9
    (TstTyp1 != 2 | is.na(TstTyp1)) & (TstTyp2 != 2 | is.na(TstTyp2)) & is.na(TstTyp3) ~ 9,
    TstTyp1 == 2 &                     TstTyp2 == 2                   & is.na(TstTyp3) ~ 9,
    TstTyp1 == 1 &                     TstTyp2 == 2                   & TstTyp3 == 2 ~ 9,
    TstTyp1 == 2 &                     TstTyp2 == 1                   & TstTyp3 == 2 ~ 9,
    TstTyp1 == 1 &                     TstTyp2 == 1                   & is.na(TstTyp3) ~ 9
    
    
  ))

# flu positives based on sensitivity of test
# Note: this code is specific to this data. If run 
# on additional rows, it must be checked to make sure
# all logical cases are considered.
flu1718 = flu1718 %>%
  mutate(flupsens = case_when(
    senstest == 1 & flupos1 ==1 ~ 1, 
    senstest == 1 & flupos1 ==0 ~ 0, 
    senstest == 2 & flupos2 ==1 ~ 1,
    senstest == 2 & flupos2 ==0 ~ 0,
    senstest == 3 & flupos3 ==1 ~ 1,
    senstest == 3 & flupos3 ==0 ~ 0,
    senstest == 9 & TstTyp1 ==1 & TstTyp2 ==1 & is.na(TstTyp3) & flupos1==1 & flupos2==1 ~ 1,
    senstest == 9 & TstTyp1 ==2 & TstTyp2 ==2 & is.na(TstTyp3) & flupos1==1 & flupos2==1 ~ 1,
    senstest == 9 & TstTyp1 ==1 & TstTyp2 ==1 & is.na(TstTyp3) & flupos1==1 & flupos2==1 ~ 1,
    senstest == 9 & TstTyp1 ==2 & TstTyp2 ==2 & is.na(TstTyp3) & flupos1==1 & flupos2==1 ~ 1,
    
    senstest == 9 & TstTyp1 ==1 & TstTyp2 ==2 & TstTyp3==2 & flupos1==1 & flupos2==0 & flupos2==0 ~ 0,
    senstest == 9 & TstTyp1 ==2 & TstTyp2 ==1 & TstTyp3==2 & flupos1==1 & flupos2==0 & flupos2==0 ~ NA_real_,
    TRUE ~ NA_real_
  ))

flu1718 = flu1718 %>% select(-c(senstest, flupos1, flupos2, flupos3,
                                TstTyp1, TstTyp2, TstTyp3, 
                                TstRes1, TstRes2, TstRes3, 
                                PosTstDt1, PosTstDt2, PosTstDt3))

flu1718 = flu1718 %>%
  mutate(PCR =             ifelse(TstTyp_fnl == 2,1,0),
         Rapid =           ifelse(TstTyp_fnl == 1,1,0),
         Rapid_molecular = ifelse(TstTyp_fnl == 7,1,0),
         Culture =      NA,
         Unknowntest =  NA)


# ---------------------------------------
# Append datasets from different years
# ---------------------------------------
# check that colnames match
assert_that(all(colnames(flu1718) %in% colnames(flu1117)))
assert_that(all(colnames(flu0811) %in% colnames(flu1117)))
assert_that(all(colnames(flu1718) %in% colnames(flu0811)))

# align column names
flu0811 = flu0811[,colnames(flu1117)]
flu1718 = flu1718[,colnames(flu1117)]

# bind data together
flu = bind_rows(flu0811, flu1117)
flu = bind_rows(flu, flu1718)

# ---------------------------------------
# Create date / month / age variables
# ---------------------------------------
slash = gregexpr("/",flu$PosTstDt_fnl)

first_slash = matrix(NA,nrow(flu),1)
second_slash = matrix(NA,nrow(flu),1)

for(i in 1:length(slash)){
  first_slash[i,] = slash[i][[1]][1]
  second_slash[i,] = slash[i][[1]][2]
}

flu = flu %>% 
  mutate(first_slash = as.numeric(first_slash),
         second_slash = as.numeric(second_slash),
         dob_length = nchar(PosTstDt_fnl)) %>%
  mutate(month = as.numeric(substr(PosTstDt_fnl, 1, first_slash - 1)),
         day =   as.numeric(substr(PosTstDt_fnl, first_slash + 1, second_slash -1)),
         year =  as.numeric(substr(PosTstDt_fnl, second_slash + 1, dob_length))) %>%
  mutate(year = ifelse(year<2000, year + 2000, year))

# flu = flu %>% mutate(PosTstDt_fnl = as.Date(PosTstDt_fnl, format="%m/%d/%y"),
#                      week = week(PosTstDt_fnl))
flu = flu %>% mutate(PosTstDt_fnl = as.Date(paste(month, day, year, sep="-"), format="%m-%d-%Y"),
                     week = week(PosTstDt_fnl))

flu$first_slash = NULL
flu$second_slash = NULL
flu$dob_length = NULL

# age category 
flu$agecat=""
flu$agecat[flu$ageyr>=0 & flu$ageyr<5]="Under 5"
flu$agecat[flu$ageyr>=5 & flu$ageyr<=9]="5 to 9"
flu$agecat[flu$ageyr>=10 & flu$ageyr<=14]="10 to 14"
flu$agecat[flu$ageyr>=15 & flu$ageyr<=17]="15 to 17"
flu$agecat[flu$ageyr>=18 & flu$ageyr<=19]="18 and 19"
flu$agecat[flu$ageyr==20]="20"
flu$agecat[flu$ageyr==21]="21"
flu$agecat[flu$ageyr>=22 & flu$ageyr<=24]="22 to 24"
flu$agecat[flu$ageyr>=25 & flu$ageyr<=29]="25 to 29"
flu$agecat[flu$ageyr>=30 & flu$ageyr<=34]="30 to 34"
flu$agecat[flu$ageyr>=35 & flu$ageyr<=39]="35 to 39"
flu$agecat[flu$ageyr>=40 & flu$ageyr<=44]="40 to 44"
flu$agecat[flu$ageyr>=45 & flu$ageyr<=49]="45 to 49"
flu$agecat[flu$ageyr>=50 & flu$ageyr<=54]="50 to 54"
flu$agecat[flu$ageyr>=55 & flu$ageyr<=59]="55 to 59"
flu$agecat[flu$ageyr>=60 & flu$ageyr<=61]="60 and 61"
flu$agecat[flu$ageyr>=62 & flu$ageyr<=64]="62 to 64"
flu$agecat[flu$ageyr>=65 & flu$ageyr<=66]="65 and 66"
flu$agecat[flu$ageyr>=67 & flu$ageyr<=69]="67 to 69"
flu$agecat[flu$ageyr>=70 & flu$ageyr<=74]="70 to 74"
flu$agecat[flu$ageyr>=75 & flu$ageyr<=79]="75 to 79"
flu$agecat[flu$ageyr>=80 & flu$ageyr<=84]="80 to 84"
flu$agecat[flu$ageyr>=85]="85 and over"

flu$agecat=factor(flu$agecat,levels=c(
  "Under 5","5 to 9", "10 to 14", "15 to 17", "18 and 19",
  "20", "21", "22 to 24", "25 to 29", "30 to 34", "35 to 39",
  "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 and 61",
  "62 to 64", "65 and 66", "67 to 69", "70 to 74", "75 to 79",
  "80 to 84", "85 and over"))

flu = flu %>% mutate(agecat = fct_explicit_na(agecat, na_level = "(Missing)"))

# ---------------------------------------
# alternative flu season definitions
# ---------------------------------------
cdph = cdph %>% rename(year = yr)
flu = flu %>% mutate(week = week(PosTstDt_fnl))

flu = left_join(flu, cdph, by = c("year", "week"))

# manual corrections for 2011-12
flu = flu %>% mutate(peakwk_2.5 = ifelse(
  PosTstDt_fnl >= as.Date("2011-12-30") & PosTstDt_fnl <= as.Date("2012-01-06"),
  1, peakwk_2.5
))

# manual corrections for 2016-17 because
# the default included only 1 day in peak week
flu = flu %>% mutate(peakwk_2.5 = ifelse(
    PosTstDt_fnl >= as.Date("2016-12-31") & PosTstDt_fnl <= as.Date("2017-01-06"),
    1, peakwk_2.5
  ))

# ---------------------------------------
# peak week ceip 
# ---------------------------------------
ceip_wk = flu %>% 
  group_by(flusesn, week) %>%
  summarise(flucount = sum(flupos)) 

ceip_peak_wk = ceip_wk %>%
  group_by(flusesn) %>%
  summarise(max = max(flucount))

ceip_peak_wk = full_join(ceip_peak_wk, ceip_wk, by = "flusesn") %>%
  mutate(ceip_peakwk = ifelse(max==flucount, 1,0)) %>%
  select(flusesn, week, ceip_peakwk)

flu = full_join(flu, ceip_peak_wk, by = c("flusesn", "week"))

# ---------------------------------------
# clean race variables - different coding for 
# ethnic variable in 1112
# ---------------------------------------
flu = flu %>%
  mutate(Ethnic = case_when(
    flusesn<1213 & Ethnic == 1 ~ 1,
    flusesn<1213 & Ethnic == 2 ~ 0,
    flusesn<1213 & Ethnic == 3 ~ 99,
    flusesn>=1213 & Ethnic == 1 ~ 1,
    flusesn>=1213 & Ethnic == 0 ~ 0,
    flusesn>=1213 & Ethnic == 99 ~ 99,
    is.na(Ethnic) ~ NA_real_,
    TRUE ~ NA_real_
  ))

flu$hispanic=flu$Ethnic
flu$hispanic[flu$Ethnic==99]=NA

# flu$Ethnic = NULL

flu$multirace_calc = rowSums(flu[,c("White", "Black", "API", "aindalsk",
                                    "raceunk")], na.rm=TRUE)

flu = flu %>% mutate(
  race = case_when(
    White == 1 & is.na(Black) & is.na(API) & is.na(aindalsk) & is.na(raceunk) & is.na(multirace) ~ "White",
    is.na(White) & Black == 1 & is.na(API) & is.na(aindalsk) & is.na(raceunk) & is.na(multirace) ~ "Black",
    is.na(White) & is.na(Black) & API == 1 & is.na(aindalsk) & is.na(raceunk) & is.na(multirace) ~ "API",
    is.na(White) & is.na(Black) & is.na(API) & aindalsk ==1 & is.na(raceunk) & is.na(multirace) ~ "American Indian/Alaska Native",
    
    is.na(White) & is.na(Black) & is.na(API) & is.na(aindalsk) & is.na(raceunk) & multirace == 1 ~ "Two or more races",
    multirace_calc>1  ~ "Two or more races",
    
    is.na(White) & is.na(Black) & is.na(API) & is.na(aindalsk) & raceunk == 1 & is.na(multirace) ~ "Unknown",
    is.na(White) & is.na(Black) & is.na(API) & is.na(aindalsk) & is.na(raceunk) & is.na(multirace) ~ "Other"
    
  )
)

head(flu %>% select(White, Black, API, aindalsk, raceunk, multirace, multirace_calc, race) %>%
       filter(race=="American Indian/Alaska Native"))

flu$multirace_calc  =  NULL

# ---------------------------------------
# recode sex
# ---------------------------------------
flu$sex[flu$Sex==1]="Male"
flu$sex[flu$Sex==2]="Female"
flu$Sex=NULL

# ---------------------------------------
# create additional variables for analysis
# ---------------------------------------
flu$eld=ifelse(flu$ageyrs>=65,1,0)
flu$nonelem=ifelse(flu$ageyrs<5 | flu$ageyrs>14,1,0)

flu$death = ifelse(flu$Outcome==1, 1, 0)
flu$ICU = ifelse(flu$ICU==99, NA, flu$ICU)

# ---------------------------------------
# create indicator for district=OUSD
# ---------------------------------------
flu$zip=as.character(flu$zip)
flu$zip=substr(flu$zip,1,5)
flu$zip=as.numeric(flu$zip)
flu$dist=""

flu$dist[flu$zip %in% ousd.zip]="OUSD"
flu$dist[flu$zip %in% wcc.zip]="WCCUSD"

flu.sens = flu
flu.sens$dist[flu.sens$zip %in% ousd.zip.sens]="OUSD"
flu.sens$dist[flu.sens$zip %in% wcc.zip.sens]="WCCUSD"

print(paste0("Dropping ", nrow(flu[flu$dist=="",]), " rows with mismatched or missing zip codes"))
flu = flu %>% filter(dist!="")

print(paste0("Dropping ", nrow(flu.sens[flu.sens$dist=="",]), " rows with mismatched or missing zip codes - sensitivity analysis"))
flu.sens = flu.sens %>% filter(dist!="")

# ---------------------------------------
# check which variables are missing in 
# entire seasons
# ---------------------------------------
flu %>% group_by(flusesn) %>%
  summarise(
    PCR = max(PCR, na.rm=TRUE),
    Rapid = max(Rapid, na.rm=TRUE),
    Rapid_molecular = max(Rapid_molecular, na.rm=TRUE),
    Culture = max(Culture, na.rm=TRUE),
    
    Outcome = max(Outcome, na.rm=TRUE),
    ICU = max(ICU, na.rm=TRUE)) 

# ---------------------------------------
# reorder columns
# ---------------------------------------
flu = flu %>% arrange(PosTstDt_fnl) %>%
  select(flusesn, dist, city, zip, sex, ageyrs, agecat, race,Ethnic,
         eld, nonelem,
         month, day, week, year,
         PosTstDt_fnl, TstTyp_fnl, TstRes_fnl,
         PCR, Rapid, Rapid_molecular, Culture, Unknowntest, 
         Outcome, ICU, death,
         flupos, flupsens,  Sample,
         fluseasCDPH_2, fluseasCDPH_2.5, fluseasCDPH_3,
         peakwk_2, peakwk_2.5, peakwk_3, ceip_peakwk)

flu = flu %>%
  rename(fluseasCDPH_2_5 = fluseasCDPH_2.5,
         peakwk_2_5 = peakwk_2.5)

flu.sens = flu.sens %>% arrange(PosTstDt_fnl) %>%
  select(flusesn, dist, city, zip, sex, ageyrs, agecat, race,
         eld, nonelem,
         month, day, week, year,
         PosTstDt_fnl, TstTyp_fnl, TstRes_fnl,
         PCR, Rapid, Rapid_molecular, Culture, Unknowntest, 
         Outcome, ICU, death,
         flupos, flupsens, Sample,
         fluseasCDPH_2, fluseasCDPH_2.5, fluseasCDPH_3,
         peakwk_2, peakwk_2.5, peakwk_3, ceip_peakwk)

flu.sens = flu.sens %>%
  rename(fluseasCDPH_2_5 = fluseasCDPH_2.5,
         peakwk_2_5 = peakwk_2.5)
# ---------------------------------------
# save data
# ---------------------------------------
saveRDS(flu, file=paste0(data_dir, "ceip-flu-clean.RDS"))

saveRDS(flu.sens, file=paste0(data_dir,"ceip-flu-clean-sens.RDS"))




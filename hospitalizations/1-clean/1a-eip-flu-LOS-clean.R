##########################################
# Shoo the Flu evaluation
# Surveillance analysis

# Import and clean CEIP data
# influenza outcomes

# Length of stay datasets
##########################################
rm(list=ls())

source(here::here("0-config.R"))

los1011 = read.csv(paste0(raw_data_dir, "Length of stay/CEIP STF_1011_LOS.csv"))
los1114 = read.csv(paste0(raw_data_dir, "Length of stay/CEIP STF_1112_1314_los.csv"))
los1518 = read.csv(paste0(raw_data_dir, "Length of stay/CEIP STF_1415_1718_los.csv"))

cdph = readRDS(paste0(data_dir, "cdph_season.RDS"))

# ---------------------------------------
# Combine datasets
# ---------------------------------------
# confirm that no rows include negative tests
assert_that(!10 %in% unique(los1011$TstRes_fnl))
assert_that(!10 %in% unique(los1114$TstRes_fnl))
assert_that(!10 %in% unique(los1518$TstRes_fnl))

# los1011 combine Asian and PI  
los1011 = los1011 %>% 
  mutate(API = ifelse(Asian == 1 | HPacIsl == 1, 1, NA)) %>%
  select(-c(Asian, HPacIsl))

# add final test date

los1114 = los1114 %>%
  mutate(
    test1_match_fnl = ifelse(TstRes1 == TstRes_fnl, 1, 0),
    test2_match_fnl = ifelse(TstRes2 == TstRes_fnl, 1, 0),
    test3_match_fnl = ifelse(TstRes3 == TstRes_fnl, 1, 0)
  ) %>%
  mutate(
    testdate_fnl = case_when(
      test1_match_fnl == 1  ~ as.character(PosTstDt1),
      test2_match_fnl == 1 & (test1_match_fnl == 0 | is.na(test1_match_fnl)) & (test3_match_fnl == 0 | is.na(test3_match_fnl)) ~ as.character(PosTstDt2),
      test3_match_fnl == 1 & (test1_match_fnl == 0 | is.na(test1_match_fnl)) & (test2_match_fnl == 0 | is.na(test2_match_fnl)) ~ as.character(PosTstDt3)
    )
  ) %>%
  rename(PosTstDt = testdate_fnl)


los1518 = los1518 %>%
  mutate(
    test1_match_fnl = ifelse(TstRes1 == TstRes_fnl, 1, 0),
    test2_match_fnl = ifelse(TstRes2 == TstRes_fnl, 1, 0),
    test3_match_fnl = ifelse(TstRes3 == TstRes_fnl, 1, 0)
  ) %>%
  mutate(
    testdate_fnl = case_when(
      test1_match_fnl == 1  ~ as.character(PosTstDt1),
      test2_match_fnl == 1 & (test1_match_fnl == 0 | is.na(test1_match_fnl)) & (test3_match_fnl == 0 | is.na(test3_match_fnl)) ~ as.character(PosTstDt2),
      test3_match_fnl == 1 & (test1_match_fnl == 0 | is.na(test1_match_fnl)) & (test2_match_fnl == 0 | is.na(test2_match_fnl)) ~ as.character(PosTstDt3)
    )
  ) %>%
  rename(PosTstDt = testdate_fnl)


# keep only covariates and length of stay
los1011 = los1011 %>%
  select(zip5, AGEYRS, Sex, Ethnic, White, API, 
         Black, AIndAlsk, MultiRace, RaceUnk,
         LOS, PosTstDt) %>%
  mutate(sesn = 1011)

los1114 = los1114 %>% 
  select(zip5, AGEYRS, Sex, Ethnic, White, API, 
         Black, AIndAlsk, MultiRace, RaceUnk,
         LOS, PosTstDt, sesn)

los1518 = los1518 %>% 
  select(zip5, AGEYRS, Sex, Ethnic, White, API, 
         Black, AIndAlsk, MultiRace, RaceUnk,
         LOS, PosTstDt, sesn)

los = bind_rows(los1011, los1114, los1518)

los = los %>% 
  rename(zip = zip5, 
         ageyrs = AGEYRS,
         sex = Sex,
         los = LOS,
         flusesn = sesn) %>%
  mutate(
    Ethnic = ifelse(Ethnic >1, 0, Ethnic),
    sex = ifelse(sex == 1, "Male", "Female"))


# ---------------------------------------
# Create date / month / age variables
# ---------------------------------------
slash = gregexpr("/",los$PosTstDt)

first_slash = matrix(NA,nrow(los),1)
second_slash = matrix(NA,nrow(los),1)

for(i in 1:length(slash)){
  first_slash[i,] = slash[i][[1]][1]
  second_slash[i,] = slash[i][[1]][2]
}

los = los %>% 
  mutate(first_slash = as.numeric(first_slash),
         second_slash = as.numeric(second_slash),
         dob_length = nchar(PosTstDt)) %>%
  mutate(month = as.numeric(substr(PosTstDt, 1, first_slash - 1)),
         day =   as.numeric(substr(PosTstDt, first_slash + 1, second_slash -1)),
         year =  as.numeric(substr(PosTstDt, second_slash + 1, dob_length))) %>%
  mutate(year = ifelse(year<2000, year + 2000, year))

los = los %>% mutate(PosTstDt = as.Date(PosTstDt, format="%m/%d/%y"),
                     week = week(PosTstDt))

los$first_slash = NULL
los$second_slash = NULL
los$dob_length = NULL


# ---------------------------------------
# Code race/ethnicity
# ---------------------------------------
los$hispanic=los$Ethnic
los$hispanic[los$Ethnic==99]=NA

los$multirace_calc = rowSums(los[,c("White", "Black", "API", "AIndAlsk",
                                    "RaceUnk")], na.rm=TRUE)

los = los %>% mutate(
  race = case_when(
    White == 1 & is.na(Black) & is.na(API) & is.na(AIndAlsk) & is.na(RaceUnk) & is.na(MultiRace) ~ "White",
    is.na(White) & Black == 1 & is.na(API) & is.na(AIndAlsk) & is.na(RaceUnk) & is.na(MultiRace) ~ "Black",
    is.na(White) & is.na(Black) & API == 1 & is.na(AIndAlsk) & is.na(RaceUnk) & is.na(MultiRace) ~ "API",
    is.na(White) & is.na(Black) & is.na(API) & AIndAlsk ==1 & is.na(RaceUnk) & is.na(MultiRace) ~ "American Indian/Alaska Native",
    
    is.na(White) & is.na(Black) & is.na(API) & is.na(AIndAlsk) & is.na(RaceUnk) & MultiRace == 1 ~ "Two or more races",
    multirace_calc>1  ~ "Two or more races",
    
    is.na(White) & is.na(Black) & is.na(API) & is.na(AIndAlsk) & RaceUnk == 1 & is.na(MultiRace) ~ "Unknown",
    is.na(White) & is.na(Black) & is.na(API) & is.na(AIndAlsk) & is.na(RaceUnk) & is.na(MultiRace) ~ "Other"
    
  )
)

los$multirace_calc  =  NULL
los$White  =  NULL
los$Black  =  NULL
los$API  =  NULL
los$AIndAlsk  =  NULL
los$RaceUnk  =  NULL
los$MultiRace  =  NULL


# ---------------------------------------
# create age category 
# ---------------------------------------
los$agecat=""
los$agecat[los$ageyr>=0 & los$ageyr<5]="Under 5"
los$agecat[los$ageyr>=5 & los$ageyr<=9]="5 to 9"
los$agecat[los$ageyr>=10 & los$ageyr<=14]="10 to 14"
los$agecat[los$ageyr>=15 & los$ageyr<=17]="15 to 17"
los$agecat[los$ageyr>=18 & los$ageyr<=19]="18 and 19"
los$agecat[los$ageyr==20]="20"
los$agecat[los$ageyr==21]="21"
los$agecat[los$ageyr>=22 & los$ageyr<=24]="22 to 24"
los$agecat[los$ageyr>=25 & los$ageyr<=29]="25 to 29"
los$agecat[los$ageyr>=30 & los$ageyr<=34]="30 to 34"
los$agecat[los$ageyr>=35 & los$ageyr<=39]="35 to 39"
los$agecat[los$ageyr>=40 & los$ageyr<=44]="40 to 44"
los$agecat[los$ageyr>=45 & los$ageyr<=49]="45 to 49"
los$agecat[los$ageyr>=50 & los$ageyr<=54]="50 to 54"
los$agecat[los$ageyr>=55 & los$ageyr<=59]="55 to 59"
los$agecat[los$ageyr>=60 & los$ageyr<=61]="60 and 61"
los$agecat[los$ageyr>=62 & los$ageyr<=64]="62 to 64"
los$agecat[los$ageyr>=65 & los$ageyr<=66]="65 and 66"
los$agecat[los$ageyr>=67 & los$ageyr<=69]="67 to 69"
los$agecat[los$ageyr>=70 & los$ageyr<=74]="70 to 74"
los$agecat[los$ageyr>=75 & los$ageyr<=79]="75 to 79"
los$agecat[los$ageyr>=80 & los$ageyr<=84]="80 to 84"
los$agecat[los$ageyr>=85]="85 and over"

los$agecat=factor(los$agecat,levels=c(
  "Under 5","5 to 9", "10 to 14", "15 to 17", "18 and 19",
  "20", "21", "22 to 24", "25 to 29", "30 to 34", "35 to 39",
  "40 to 44", "45 to 49", "50 to 54", "55 to 59", "60 and 61",
  "62 to 64", "65 and 66", "67 to 69", "70 to 74", "75 to 79",
  "80 to 84", "85 and over"))

los = los %>% mutate(agecat = fct_explicit_na(agecat, na_level = "(Missing)"))


# ---------------------------------------
# create indicator for district=OUSD
# ---------------------------------------
los$zip=as.character(los$zip)
los$zip=substr(los$zip,1,5)
los$zip=as.numeric(los$zip)
los$dist=""

los$dist[los$zip %in% ousd.zip]="OUSD"
los$dist[los$zip %in% wcc.zip]="WCCUSD"

los.sens = los
los.sens$dist[los.sens$zip %in% ousd.zip.sens]="OUSD"
los.sens$dist[los.sens$zip %in% wcc.zip.sens]="WCCUSD"

print(paste0("Dropping ", nrow(los[los$dist=="",]), " rows with mismatched or missing zip codes"))
los = los %>% filter(dist!="")

# ---------------------------------------
# create additional variables for analysis
# ---------------------------------------
los$eld=ifelse(los$ageyrs>=65,1,0)
los$nonelem=ifelse(los$ageyrs<5 | los$ageyrs>14,1,0)

# ---------------------------------------
# drop if no length of stay available
# ---------------------------------------
print(paste0(  table(is.na(los$los))[2], " out of ", nrow(los), " records had no length of stay recorded"))
nrow(los)
los = los[!is.na(los$los),]
nrow(los)

# ---------------------------------------
# save data
# ---------------------------------------
saveRDS(los, file=paste0(data_dir, "ceip-flu-los-clean.RDS"))





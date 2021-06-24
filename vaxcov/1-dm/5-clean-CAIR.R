########################################
# Import and clean CAIR vaccine coverage data 
# February 2019
########################################
# define directories, load libraries
source(here::here("0-config.R"))

#import datasets
alameda <- read.csv(here::here("data", "Raw", "Alameda.txt")) 

contracosta <- read.csv(here::here("data", "Raw", "ContraCosta.txt"))

#merge datasets
combined <- rbind(alameda, contracosta)

#count number of flu vaccines, zip codes and providers
flu_vax <- count(combined, "VACCINE") #25 flu vaccines
zip_count <- count(combined, "ZIP_CODE") #39 zip codes
prov_type <- count(combined, "PROVIDER")

#keep only data on flu vaccines
flu <- combined[grep("Flu", combined$VACCINE),]
influenza <- combined[grep("Influenza", combined$VACCINE),]
combined_flu <- rbind(flu, influenza)
#fluvax <- data.frame(table(combined_flu$VACCINE)) 
rm(flu, influenza)

#subset to only zip codes in our study
OUSD_zips <- c(94601, 94602, 94603, 94605, 94606, 94607, 94608,	94609, 94610, 94611, 94612, 94618, 94619, 94621, 94705, 94613, 94704, 94604, 94614, 94615, 94617, 94622, 94623, 94624, 94649, 94659, 94660, 94661, 94666)
WCC_zips <- c(94530, 94547,	94564, 94707,	94708, 94801,	94803, 94804,	94805, 94806,	94850)
study_zips <- c(OUSD_zips, WCC_zips) #40 total zip codes
combined_flu <- combined_flu %>% filter(ZIP_CODE %in% study_zips)

combined_flu <- combined_flu %>% mutate(dist = case_when(
  ZIP_CODE %in% OUSD_zips ~ "OUSD",
  ZIP_CODE %in% WCC_zips ~ "WCCUSD"
))

#check that we have all 25 flu vaccine types and 39 zip codes
vaccine_check <- combined_flu %>%
  count(VACCINE)
assert_that(nrow(vaccine_check) == 25)

zip_check <- combined_flu %>%
  count(ZIP_CODE)
assert_that(nrow(zip_check) == (length(OUSD_zips) + length(WCC_zips))) #fails because missing zip 94659 in CAIR data

#Create variable for year according to CDC flu season
combined_flu$VACCINATION_DATE <- as.Date(combined_flu$VACCINATION_DATE, "%m/%d/%Y")
combined_flu <- combined_flu %>%
  mutate(season = case_when(
    VACCINATION_DATE >= as.Date("1943-01-01") & VACCINATION_DATE <= as.Date("2008-10-4") ~ "before 2008",
    VACCINATION_DATE > as.Date("2008-10-4") & VACCINATION_DATE <= as.Date("2009-10-10") ~ "2008-2009",
    VACCINATION_DATE > as.Date("2009-10-10") & VACCINATION_DATE <= as.Date("2010-10-9") ~ "2009-2010",
    VACCINATION_DATE > as.Date("2010-10-9") & VACCINATION_DATE <= as.Date("2011-10-8") ~ "2010-2011",
    VACCINATION_DATE > as.Date("2011-10-8") & VACCINATION_DATE <= as.Date("2012-10-6") ~ "2011-2012",
    VACCINATION_DATE > as.Date("2012-10-6") & VACCINATION_DATE <= as.Date("2013-9-27") ~ "2012-2013",
    VACCINATION_DATE > as.Date("2013-9-27") & VACCINATION_DATE <= as.Date("2014-10-4") ~ "2013-2014",
    VACCINATION_DATE > as.Date("2014-10-4") & VACCINATION_DATE <= as.Date("2015-10-10") ~ "2014-2015",
    VACCINATION_DATE > as.Date("2015-10-10") & VACCINATION_DATE <= as.Date("2016-10-8") ~ "2015-2016",
    VACCINATION_DATE > as.Date("2016-10-8") & VACCINATION_DATE <= as.Date("2017-10-7") ~ "2016-2017",
    VACCINATION_DATE > as.Date("2017-10-7") & VACCINATION_DATE <= as.Date("2018-10-6") ~ "2017-2018",
    VACCINATION_DATE > as.Date("2018-10-6") ~ "2018-2019"
  )) 

#head(combined_flu[is.na(combined_flu$year),"VACCINATION_DATE"])

#check for duplicates by year
table(duplicated(combined_flu))
duplicates <- combined_flu[duplicated(combined_flu),]

#Remove duplicates
combined_flu <- combined_flu %>% distinct()

#create variable for provider type
school <- c("SHOO THE FLU", "school", "high school", "middle school")
hospital <- c("hospital", "childrens hospital", "children's health", "pediatrics", "clinic", "medical group", "Kaiser", "Sutter", "wellness", "center", "medical foundation", "group", "health services", "Alta Bates", "Alta Vista", "health system", "UCSF")
health_dept <- c("public health", "health department", "health dept", "immunization program", "county dept", "county department")
doc <- c("MD", "family practice", "DR.", "internal medicine", "primary care", "family medicine", "hill health")
pharm <- c("Walgreens", "Walgreen", "pharmacy", "Rite Aid", "CVS", "Costco", "Walmart", "Safeway")

combined_flu$PROVIDER_TYPE <- rep(NA, nrow(combined_flu))

combined_flu <- combined_flu %>% 
  mutate(PROVIDER_TYPE = case_when(grepl(paste(school, collapse = "|"), PROVIDER, ignore.case=TRUE) ~ "School",
                                   grepl(paste(hospital, collapse = "|"), PROVIDER, ignore.case=TRUE) ~ "Clinic, Health Center or Other Medical Place",
                                   grepl(paste(health_dept, collapse = "|"), PROVIDER, ignore.case=TRUE) ~ "Health Department",
                                   grepl(paste(doc, collapse = "|"), PROVIDER, ignore.case=TRUE) ~ "Doctor's Office",
                                   grepl(paste(pharm, collapse = "|"), PROVIDER, ignore.case=TRUE) ~ "Pharmacy/Store",
                                   is.na(PROVIDER_TYPE) ~ "OTHER"))  


#create variable for flu vaccine type
combined_flu <- combined_flu %>% 
  mutate(VACCINE_TYPE = case_when(grepl("inject", VACCINE) ~ "Injectable",
                                  grepl("nasal", VACCINE) ~ "Nasal",
                                  grepl("high dose", VACCINE) ~ "Injectable",
                                  grepl("intradermal", VACCINE) ~ "Injectable",
                                  grepl("Flucelvax", VACCINE) ~ "Injectable",
                                  VACCINE == "Flu trivalent adjuvanted pfree" | VACCINE == "Influenza A monovalent (H5N1) ADJUVANTED" | VACCINE == "Flu NOS" |  VACCINE == "Flu trivalent adjuvanted pfree" | VACCINE == "Flu split virus" | VACCINE == "Flu whole virus" | VACCINE == "Influenza-H1N1-09" | VACCINE == "Influenza-H1N1-09, NOS" | VACCINE == "Influenza-H1N1-09, pfree" ~ "Injectable"))

#drop extra levels of VACCINE
combined_flu$VACCINE <- droplevels(combined_flu$VACCINE)

#create variable for quadrivalent/trivalent
combined_flu$Quad_Tri <- rep(NA, nrow(combined_flu))
combined_flu <- combined_flu %>% 
  mutate(Quad_Tri = case_when(grepl("quadrivalent", VACCINE, ignore.case=TRUE) ~ "Quadrivalent",
                                  grepl("trivalent", VACCINE, ignore.case=TRUE) ~ "Trivalent",
                              is.na(Quad_Tri) ~ "Unknown"))

#add column representing number of vaccines
n_occur <- combined_flu %>%
  group_by(season) %>%
  count(Modified_ID)
names(n_occur)[3]<-"N_vax_per_flu_season"
combined_flu <- merge(combined_flu, n_occur, by=c("Modified_ID", "season"))
rm(n_occur)

#create age cat variable
combined_flu <-combined_flu %>%
  mutate(age_cat = case_when(AGE_IN_YEARS < 5 ~ "Under 5 years",
                             AGE_IN_YEARS <= 12 ~ "5-12 years",
                             AGE_IN_YEARS <= 17 ~ "13-17 years",
                             AGE_IN_YEARS <= 44 ~ "18-44 years",
                             AGE_IN_YEARS <= 64 ~ "45-64 years",
                             AGE_IN_YEARS > 64 ~ "65+ years"))

#create indicator with 1=vaccinated, 0=not vaccinated, error, missing
combined_flu$vxpre08yn=ifelse(combined_flu$season=="before 2008",1,0)
combined_flu$vx0809yn=ifelse(combined_flu$season=="2008-2009",1,0)
combined_flu$vx0910yn=ifelse(combined_flu$season=="2009-2010",1,0)
combined_flu$vx1011yn=ifelse(combined_flu$season=="2010-2011",1,0)
combined_flu$vx1112yn=ifelse(combined_flu$season=="2011-2012",1,0)
combined_flu$vx1213yn=ifelse(combined_flu$season=="2012-2013",1,0)
combined_flu$vx1314yn=ifelse(combined_flu$season=="2013-2014",1,0)
combined_flu$vx1415yn=ifelse(combined_flu$season=="2014-2015",1,0)
combined_flu$vx1516yn=ifelse(combined_flu$season=="2015-2016",1,0)
combined_flu$vx1617yn=ifelse(combined_flu$season=="2016-2017",1,0)
combined_flu$vx1718yn=ifelse(combined_flu$season=="2017-2018",1,0)
combined_flu$vx1819yn=ifelse(combined_flu$season=="2018-2019",1,0)

#Change all variable names to lower case
tolower(colnames(combined_flu))

saveRDS(combined_flu, file = (here::here("data", "clean-CAIR.rds")))


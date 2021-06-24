########################################
# Merge Vaccine coverage survey coverage
# data with school Shoo the Flu participation
# data
########################################
rm(list =ls())

# define directories, load libraries
source(here::here("0-config.R"))

#------------------------------------
# load Shoo the Flu participation data
#------------------------------------

participation_pre17 = read.csv(participation_pre17_data_path)
participation_1718 = read.csv(participation_1718_data_path)

# Subset to ousd public schools
participation_pre17 <- participation_pre17 %>% filter(type=="OUSD") %>%
  dplyr::select(-type) %>%
  dplyr::select("school" = "School.Name", student2016per, student2015per, student2014per) 

participation_1718 <- participation_1718 %>% 
  dplyr::select("school" = "School.Name", "student2017pers" = "Student.Participation...and.Total.Vaccinated") %>%
  filter(school != "") %>%
  mutate(student2017per = as.numeric(gsub("%","",student2017pers))/100) %>%
  dplyr::select(-student2017pers)

participation = left_join(participation_pre17, participation_1718, by="school")

#------------------------------------
# load vaccination coverage
#------------------------------------
load(paste0(complete_data_path_2017, ".RData"))
cov_pre17 = data %>% 
  filter(district=="OUSD") %>%
  group_by(schoolname) %>%
  mutate(vx1415 = ifelse(vx1415==1,1,0), 
         vx1516 = ifelse(vx1516==1,1,0), 
         vx1617 = ifelse(vx1617==1,1,0)) %>%
  summarise(vx1415 = mean(vx1415), 
            vx1516 = mean(vx1516),
            vx1617 = mean(vx1617))

load(paste0(clean_data_path_2018, ".RData"))

cov_1718 = data.import %>% 
  filter(district=="OUSD") %>%
  group_by(schoolname) %>%
  mutate(vx1718 = ifelse(vx1718==1, 1, 0)) %>%
  summarise(vx1718 = mean(vx1718))

# combine coverage datasets
cov = full_join(cov_pre17, cov_1718, by = "schoolname")

cov = cov %>% rename(school=schoolname)

cov_l = melt(cov, id.vars = "school")
colnames(cov_l) = c("school", "year", "coverage")
cov_l = cov_l %>% mutate(year = case_when(
  year == "vx1415" ~ "2014-15",
  year == "vx1516" ~ "2015-16",
  year == "vx1617" ~ "2016-17",
  year == "vx1718" ~ "2017-18"
))

#------------------------------------
# merge coverage and participation
#------------------------------------
participation = participation %>% 
  mutate(school = as.character(school)) %>%
  mutate(school = case_when(
       school == "Fred T. Korematsu Elementary" ~ "Fred T. Korematsu Discovery Academy",
       school == "EnCompass Academy" ~ "EnCompass Academy Elementary",
       school == "Henry J. Kaiser Elementary" ~ "Kaiser Elementary",
       school == "Rise Community School" ~ "Rise Community",
       school == "Think College Now Elementary" ~ "Think College Now",
       TRUE ~ school
  )
  )

participation_l = melt(participation, id.vars = "school")
colnames(participation_l) = c("school", "year", "participation")
participation_l = participation_l %>% mutate(year = case_when(
  year == "student2014per" ~ "2014-15",
  year == "student2015per" ~ "2015-16",
  year == "student2016per" ~ "2016-17",
  year == "student2017per" ~ "2017-18"
))

x = left_join(cov_l, participation_l, by = c("school", "year"))
x = x[complete.cases(x),]

#------------------------------------
# merge in school level covariates
#------------------------------------
school_data = read.csv(school_data_path)

school_data = school_data %>% 
  filter(district=="Oakland Unified" & stype=="Elementary Schools (Public)") %>%
  dplyr::select(-c(CDS, districtcode, county, charter, stype, latitude, longitude))

x = left_join(x, school_data, by = "school")


saveRDS(x, file = paste0(coverage_participation_data_path, ".RDS"))
write.csv(x, file = paste0(coverage_participation_data_path, ".csv"))


##########################################
# Shoo the Flu evaluation
# Vaccination coverage analysis

# Find study distributions of race/ethnicity 
# and highest parental education level
##########################################

# define directories, load libraries
source(here::here("0-config.R"))

#---------------------------------------------
# 2017 survey
#---------------------------------------------

data = read.csv(data_path_2017)
data$dist=as.factor(data$dist)

data$edu=as.character(data$edu)

data = data %>% mutate(college = case_when(
  edu == "Less than high school" ~ "No",
  edu == "High school" ~ "No", 
  edu == "Associate/College"~ "Yes",
  edu == "Postgrad" ~ "Yes",
  edu == "Missing" ~ "Missing/Error",
  edu == "Error" ~ "Missing/Error"
))


race_survey2017 = data %>% group_by(district, race) %>% summarize(n = n()) %>%  mutate(prop = n/sum(n))
write.csv(race_survey2017, paste0(local_tab_path, "/race_survey2017.csv"))

multi_race = data %>% filter(race == "Multi") %>% dplyr::select(starts_with("race"), district)
multi_race = multi_race %>% mutate(black_white = (race_white == 1 & race_black == 1 &
                                      race_asian == 0 & race_pi == 0 & 
                                      race_lat  == 0 & race_natamer == 0),
                                  api_white = ((race_white == 1 & race_black == 0 &
                                                 race_asian == 1 & race_pi == 0 & 
                                                 race_lat  == 0 & race_natamer == 0) | 
                                                 (race_white == 1 & race_black == 0 &
                                                    race_asian == 0 & race_pi == 1 & 
                                                    race_lat  == 0 & race_natamer == 0)),
                                  black_latino = (race_white == 0 & race_black == 1 &
                                                    race_asian == 0 & race_pi == 0 & 
                                                    race_lat == 1 & race_natamer == 0),
                                  white_latino = (race_white == 1 & race_black == 0 &
                                                    race_asian == 0 & race_pi == 0 & 
                                                    race_lat  == 1 & race_natamer == 0), 
                                  api_latino = ((race_white == 0 & race_black == 0 &
                                                  race_asian == 1 & race_pi == 0 & 
                                                  race_lat  == 1 & race_natamer == 0) | 
                                                 (race_white == 0 & race_black == 0 &
                                                    race_asian == 0 & race_pi == 1 & 
                                                    race_lat  == 1 & race_natamer == 0)),
                                  api_black = ((race_white == 0 & race_black == 1 &
                                                  race_asian == 1 & race_pi == 0 & 
                                                  race_lat  == 0 & race_natamer == 0) | 
                                                 (race_white == 0 & race_black == 1 &
                                                    race_asian == 0 & race_pi == 1 & 
                                                    race_lat  == 0 & race_natamer == 0)))

multi_race = multi_race %>% mutate(other = ifelse(black_white == 0 & 
                                                    api_white == 0 & 
                                                    black_latino == 0 & 
                                                    white_latino == 0 &  
                                                    api_latino == 0 & 
                                                    api_black == 0, 1, 0))

race_cols = multi_race %>% dplyr::select(black_white, api_white, black_latino, white_latino, api_latino, api_black, other)

multi_race$race_cat = names(race_cols)[max.col(race_cols)]

multi_race2017 = multi_race %>% group_by(district, race_cat) %>% summarize(n = n()) %>%  mutate(prop = n/sum(n))
write.csv(multi_race2017, paste0(local_tab_path, "/multi_race2017.csv"))

#---------------------------------------------
# 2018 survey
#---------------------------------------------
data = read.csv(data_path_2018)
data$dist=as.factor(data$dist)

data$edu=as.character(data$edu)

data = data %>% mutate(college = case_when(
  edu == "Less than high school" ~ "No",
  edu == "High school" ~ "No", 
  edu == "Associate/College"~ "Yes",
  edu == "Postgrad" ~ "Yes",
  edu == "Missing" ~ "Missing/Error",
  edu == "Error" ~ "Missing/Error"
))

prop.table(table(data$language, data$district),2)
prop.table(table(data$college, data$district),2)
prop.table(table(data$race, data$district),2)
race_survey2018 = data %>% group_by(district, race) %>% summarize(n = n()) %>%  mutate(prop = n/sum(n))
write.csv(race_survey2018, paste0(local_tab_path, "/race_survey2018.csv"))

multi_race = data %>% filter(race == "Multi") %>% dplyr::select(starts_with("race"), district)
multi_race = multi_race %>% mutate(black_white = (race_white == 1 & race_black == 1 &
                                                    race_asian == 0 & race_pi == 0 & 
                                                    race_lat  == 0 & race_natamer == 0),
                                   api_white = ((race_white == 1 & race_black == 0 &
                                                   race_asian == 1 & race_pi == 0 & 
                                                   race_lat  == 0 & race_natamer == 0) | 
                                                  (race_white == 1 & race_black == 0 &
                                                     race_asian == 0 & race_pi == 1 & 
                                                     race_lat  == 0 & race_natamer == 0)),
                                   black_latino = (race_white == 0 & race_black == 1 &
                                                     race_asian == 0 & race_pi == 0 & 
                                                     race_lat == 1 & race_natamer == 0),
                                   white_latino = (race_white == 1 & race_black == 0 &
                                                     race_asian == 0 & race_pi == 0 & 
                                                     race_lat  == 1 & race_natamer == 0), 
                                   api_latino = ((race_white == 0 & race_black == 0 &
                                                    race_asian == 1 & race_pi == 0 & 
                                                    race_lat  == 1 & race_natamer == 0) | 
                                                   (race_white == 0 & race_black == 0 &
                                                      race_asian == 0 & race_pi == 1 & 
                                                      race_lat  == 1 & race_natamer == 0)),
                                   api_black = ((race_white == 0 & race_black == 1 &
                                                   race_asian == 1 & race_pi == 0 & 
                                                   race_lat  == 0 & race_natamer == 0) | 
                                                  (race_white == 0 & race_black == 1 &
                                                     race_asian == 0 & race_pi == 1 & 
                                                     race_lat  == 0 & race_natamer == 0)))

multi_race = multi_race %>% mutate(other = ifelse(black_white == 0 & 
                                                  api_white == 0 & 
                                                  black_latino == 0 & 
                                                  white_latino == 0 &  
                                                  api_latino == 0 & 
                                                  api_black == 0, 1, 0))
race_cols = multi_race %>% dplyr::select(black_white, api_white, black_latino, white_latino, api_latino, api_black, other)

multi_race$race_cat = names(race_cols)[max.col(race_cols)]

multi_race2018 = multi_race %>% group_by(district, race_cat) %>% summarize(n = n()) %>%  mutate(prop = n/sum(n))
write.csv(multi_race2018, paste0(local_tab_path, "/multi_race2018.csv"))

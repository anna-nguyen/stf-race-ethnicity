##########################################
# Shoo the Flu evaluation
# Vaccination coverate analysis

# Calculate the mean difference in vaccine
# coverage between districts
##########################################

rm(list=ls())

# define directories, load libraries
source(here::here("0-config.R"))
library(gridExtra)

load(district_demographics_path)

data = read.csv(data_path_2017)
data$dist=factor(data$dist,levels=c("WCCUSD","OUSD"))

data.y4 = read.csv(data_path_2018)
data.y4$dist=factor(data.y4$dist,levels=c("WCCUSD","OUSD"))

# drop Wilson because it has no matched pair
data=data[data$schoolname!="Wilson Elementary",]

data$vx1415yn=ifelse(data$vx1415==1,1,0)
data$vx1516yn=ifelse(data$vx1516==1,1,0)
data$vx1617yn=ifelse(data$vx1617==1,1,0)
data.y4$vx1718yn=ifelse(data.y4$vx1718==1,1,0)

data$race = as.character(data$race)
data = data %>% mutate(race = ifelse(race == "Asian" | race == "Pacific islander",
                                     "API", 
                                     race))

white = data %>% filter(race == "White")
API = data %>% filter(race == "API") 
multi = data %>% filter(race == "Multi")
black = data %>% filter(race == "Black")
latino = data %>% filter(race == "Latino")


data.y4$race = as.character(data.y4$race)
data.y4 = data.y4 %>% mutate(race = ifelse(race == "Asian" | race == "Pacific islander",
                                           "API", 
                                           race))

white.y4 = data.y4 %>% filter(race == "White")
API.y4 = data.y4 %>% filter(race == "API")
multi.y4 = data.y4 %>% filter(race == "Multi")
black.y4 = data.y4 %>% filter(race == "Black")
latino.y4 = data.y4 %>% filter(race == "Latino")


# function to fit glm and format result
fit_vxcov = function(season, race_data, adj_edu = FALSE){
  vax_glm_formula = paste0("vx", season, "yn ~ dist")
  if (adj_edu) {vax_glm_formula = paste0(vax_glm_formula, " + edu")}
  
  fit.vx = glm(vax_glm_formula, data = race_data)
  vcovCL.vx = sandwichSE(fm = fit.vx, cluster = race_data$matchid)
  rfit.vx = coeftest(fit.vx, vcovCL.vx)
  res.vx = format.glm(rfit.vx, family = "gaussian")
  return(res.vx)
}


#----------------------------------------
# White
#----------------------------------------

res.white.y1 = fit_vxcov(1415, white)
res.white.y2 = fit_vxcov(1516, white)
res.white.y3 = fit_vxcov(1617, white)
res.white.y4 = fit_vxcov(1718, white.y4)

white_res = as.data.frame(rbind(res.white.y1, res.white.y2, res.white.y3, res.white.y4)) %>%
  mutate(year = c(1, 2, 3, 4),
         race = "White") %>%
  dplyr::select(year, race, everything())

#----------------------------------------
# White, adjusted for education
#----------------------------------------
res.white.adjedu.y1 = fit_vxcov(1415, white, adj_edu = TRUE)
res.white.adjedu.y2 = fit_vxcov(1516, white, adj_edu = TRUE)
res.white.adjedu.y3 = fit_vxcov(1617, white, adj_edu = TRUE)
res.white.adjedu.y4 = fit_vxcov(1718, white.y4, adj_edu = TRUE)

white_adjedu_res = as.data.frame(rbind(res.white.adjedu.y1, res.white.adjedu.y2, 
                                       res.white.adjedu.y3, res.white.adjedu.y4)) %>%
  mutate(year = c(1, 2, 3, 4),
         race = "White") %>%
  dplyr::select(year, race, everything())

#----------------------------------------
# Asian / Pacific Islander
#----------------------------------------

res.API.y1 = fit_vxcov(1415, API)
res.API.y2 = fit_vxcov(1516, API)
res.API.y3 = fit_vxcov(1617, API)
res.API.y4 = fit_vxcov(1718, API.y4)

api_res = as.data.frame(rbind(res.API.y1, res.API.y2, res.API.y3, res.API.y4)) %>%
  mutate(year = c(1, 2, 3, 4),
         race = "Asian / Pacific Islander") %>%
  dplyr::select(year, race, everything())

#----------------------------------------
# Asian / Pacific Islander, adjusted for education
#----------------------------------------

res.API.adjedu.y1 = fit_vxcov(1415, API, adj_edu = TRUE)
res.API.adjedu.y2 = fit_vxcov(1516, API, adj_edu = TRUE)
res.API.adjedu.y3 = fit_vxcov(1617, API, adj_edu = TRUE)
res.API.adjedu.y4 = fit_vxcov(1718, API.y4, adj_edu = TRUE)

api_adjedu_res = as.data.frame(rbind(res.API.adjedu.y1, res.API.adjedu.y2, 
                                     res.API.adjedu.y3, res.API.adjedu.y4)) %>%
  mutate(year = c(1, 2, 3, 4),
         race = "Asian / Pacific Islander") %>%
  dplyr::select(year, race, everything())

#----------------------------------------
# Black or African-American
#----------------------------------------
res.black.y1 = fit_vxcov(1415, black)
res.black.y2 = fit_vxcov(1516, black)
res.black.y3 = fit_vxcov(1617, black)
res.black.y4 = fit_vxcov(1718, black.y4)

black_res = as.data.frame(rbind(res.black.y1, res.black.y2, res.black.y3, res.black.y4)) %>%
  mutate(year = c(1, 2, 3, 4),
         race = "Black/African American") %>%
  dplyr::select(year, race, everything())

#----------------------------------------
# Black or African-American, adjusted for education
#----------------------------------------

res.black.adjedu.y1 = fit_vxcov(1415, black, adj_edu = TRUE)
res.black.adjedu.y2 = fit_vxcov(1516, black, adj_edu = TRUE)
res.black.adjedu.y3 = fit_vxcov(1617, black, adj_edu = TRUE)
res.black.adjedu.y4 = fit_vxcov(1718, black.y4, adj_edu = TRUE)

black_adjedu_res = as.data.frame(rbind(res.black.adjedu.y1, res.black.adjedu.y2, 
                                       res.black.adjedu.y3, res.black.adjedu.y4)) %>%
  mutate(year = c(1, 2, 3, 4),
         race = "Black/African American") %>%
  dplyr::select(year, race, everything())

#----------------------------------------
# Latino
#----------------------------------------
res.latino.y1 = fit_vxcov(1415, latino)
res.latino.y2 = fit_vxcov(1516, latino)
res.latino.y3 = fit_vxcov(1617, latino)
res.latino.y4 = fit_vxcov(1718, latino.y4)

latino_res = as.data.frame(rbind(res.latino.y1, res.latino.y2, res.latino.y3, res.latino.y4)) %>%
  mutate(year = c(1, 2, 3, 4),
         race = "Latino") %>%
  dplyr::select(year, race, everything())

#----------------------------------------
# Latino, adjusted for education
#----------------------------------------

res.latino.adjedu.y1 = fit_vxcov(1415, latino, adj_edu = TRUE)
res.latino.adjedu.y2 = fit_vxcov(1516, latino, adj_edu = TRUE)
res.latino.adjedu.y3 = fit_vxcov(1617, latino, adj_edu = TRUE)
res.latino.adjedu.y4 = fit_vxcov(1718, latino.y4, adj_edu = TRUE)

latino_adjedu_res = as.data.frame(rbind(res.latino.adjedu.y1, res.latino.adjedu.y2, 
                                        res.latino.adjedu.y3, res.latino.adjedu.y4)) %>%
  mutate(year = c(1, 2, 3, 4),
         race = "Latino") %>%
  dplyr::select(year, race, everything())

#----------------------------------------
# Multiple Races
#----------------------------------------
res.multi.y1 = fit_vxcov(1415, multi)
res.multi.y2 = fit_vxcov(1516, multi)
res.multi.y3 = fit_vxcov(1617, multi)
res.multi.y4 = fit_vxcov(1718, multi.y4)

multi_res = as.data.frame(rbind(res.multi.y1, res.multi.y2, res.multi.y3, res.multi.y4)) %>%
  mutate(year = c(1, 2, 3, 4),
         race = "Multiple Races") %>%
  dplyr::select(year, race, everything())

#----------------------------------------
# Multiple Races, adjusted for education
#----------------------------------------
res.multi.adjedu.y1 = fit_vxcov(1415, multi, adj_edu = TRUE)
res.multi.adjedu.y2 = fit_vxcov(1516, multi, adj_edu = TRUE)
res.multi.adjedu.y3 = fit_vxcov(1617, multi, adj_edu = TRUE)
res.multi.adjedu.y4 = fit_vxcov(1718, multi.y4, adj_edu = TRUE)

multi_adjedu_res = as.data.frame(rbind(res.multi.adjedu.y1, res.multi.adjedu.y2, 
                                       res.multi.adjedu.y3, res.multi.adjedu.y4)) %>%
  mutate(year = c(1, 2, 3, 4),
         race = "Multiple Races") %>%
  dplyr::select(year, race, everything())

#----------------------------------------
# Combine all races 
#----------------------------------------
all = bind_rows(
  white_res, black_res, api_res, latino_res, multi_res
)

all = all %>% mutate(pval = round(pval, 3), 
                     sig = ifelse(pval <0.05, TRUE, FALSE), 
                     Season = case_when(year == 1 ~ "2014-15", 
                                        year == 2 ~ "2015-16", 
                                        year == 3 ~ "2016-17", 
                                        year == 4 ~ "2017-18")) %>% 
  rename("Race" = "race") %>% 
  mutate(Race = case_when(Race == "Multi" ~ "Multiple Races",
                          Race == "Black" ~ "Black/African American",
                          Race == "Latino" ~ "Latino", 
                          Race == "Asian / Pacific Islander" ~ "Asian/Pacific Islander",
                          TRUE ~ Race))

saveRDS(all, paste0(local_res_path, "/mean_diff_race.csv"))

#----------------------------------------
# Combine all races, adjusted for education
#----------------------------------------
all_adjedu = bind_rows(
  white_adjedu_res, black_adjedu_res, api_adjedu_res, latino_adjedu_res, multi_adjedu_res
)

all_adjedu = all_adjedu %>% mutate(pval = round(pval, 3), 
                                   sig = ifelse(pval < 0.05, TRUE, FALSE), 
                                   Season = case_when(year == 1 ~ "2014-15", 
                                                      year == 2 ~ "2015-16", 
                                                      year == 3 ~ "2016-17", 
                                                      year == 4 ~ "2017-18")) %>% 
  rename("Race" = "race") %>% 
  mutate(Race = case_when(Race == "Multi" ~ "Multiple Races",
                          Race == "Black" ~ "Black/African American",
                          Race == "Latino" ~ "Latino", 
                          Race == "Asian / Pacific Islander" ~ "Asian/Pacific Islander",
                          TRUE ~ Race))

saveRDS(all_adjedu, paste0(local_res_path, "/adj_mean_diff_race.csv"))

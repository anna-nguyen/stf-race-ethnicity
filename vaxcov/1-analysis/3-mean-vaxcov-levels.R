##########################################
# Shoo the Flu evaluation
# Vaccination coverage analysis

# Calculate vaccine coverage by race/
# ethnicity in both districts
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

data.y4$race = as.character(data.y4$race)
data.y4 = data.y4 %>% mutate(race = ifelse(race == "Asian" | race == "Pacific islander",
                                           "API", 
                                           race))

#----------------------------------------------------------------------
# Calculate vaccine coverage and CIs for each year/race/district combo
#----------------------------------------------------------------------
vax_vars = rep(c("vx1415yn", "vx1516yn", "vx1617yn", "vx1718yn"), 5)
season_vars = rep(c("2014-15", "2015-16", "2016-17", "2017-18"), 5)
race_vars = rep(c("White", "API", "Multi", "Black", "Latino"), 4)

vaxcov_tbl = data.frame(race = race_vars) %>% 
  mutate(season = season_vars, vax_col = vax_vars) %>% 
  arrange(race, season)

find_vaxcov_ci = function(vaxcov_tbl_row){
  if (vaxcov_tbl_row["season"] == "2017-18"){
    vaxcov_data = data.y4
  } else {
    vaxcov_data = data
  }
  
  filtered_race_tbl = vaxcov_data %>% filter(race == as.character(vaxcov_tbl_row["race"]))

  glm_equation = paste0(vaxcov_tbl_row[["vax_col"]], " ~ district + edu")
  vaxcov_glm = glm(glm_equation, data = filtered_race_tbl)
  vaxcov_cov = sandwichSE(fm = vaxcov_glm, cluster = filtered_race_tbl$matchid)
  vaxcov_rfit = coeftest(vaxcov_glm, vaxcov_cov)
  
  n_ousd = nrow(filtered_race_tbl %>% filter(district == "OUSD"))
  prop_ousd = vaxcov_rfit[1, 1]
  se_ousd = vaxcov_rfit[1, 2]
  ci_ousd = calculate_wilson_ci(prop_ousd, se_ousd)

  n_wccusd = nrow(filtered_race_tbl %>% filter(district == "WCCUSD"))
  prop_wccusd = vaxcov_rfit[1, 1] + vaxcov_rfit[2, 1]
  se_wccusd = sqrt(vaxcov_cov[1,1] + vaxcov_cov[2,2] + 2 * vaxcov_cov[1,2])
  ci_wccusd = calculate_wilson_ci(prop_wccusd, se_wccusd)

  output_tbl = data.frame("district" = c("OUSD", "WCCUSD"), 
                          "race" = vaxcov_tbl_row[["race"]],
                          "season" = vaxcov_tbl_row[["season"]],
                          "n" = c(n_ousd, n_wccusd),
                          "est" = c(prop_ousd, prop_wccusd), 
                          "lb" = c(ci_ousd[1], ci_wccusd[1]),
                          "ub" = c(ci_ousd[2], ci_wccusd[2]))
  
  return(output_tbl)
}


vax_cov_estimates = apply(vaxcov_tbl, 1, find_vaxcov_ci) %>% bind_rows()
vax_cov_estimates = vax_cov_estimates %>% rename("District" = "district")
saveRDS(vax_cov_estimates, paste0(local_res_path, "/race_dist_vaxcov.RDS"))
            
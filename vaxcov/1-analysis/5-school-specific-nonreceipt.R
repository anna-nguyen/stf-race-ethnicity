##########################################
# Shoo the Flu evaluation
# Vaccination coverage analysis

# Merge percentage of reported reason for
# non-receipt with school-level characteristics
##########################################

rm(list = ls())

source(here::here("0-config.R"))
library(tidyr)
library(lmtest)
library(data.table)

school_covars = read.csv(school_data_path)
local_res_path = here::here("results")
nonreceipt_tbl = readRDS(paste0(local_res_path, "/reasons_for_nonreceipt.RDS"))

# In a given school, what percentage said they had a logistic concern, what had a nonbelief concern
school_nonreceipt = nonreceipt_tbl %>% 
  group_by(schoolname, reason_cat) %>% 
  summarise(n = n()) %>%
  mutate(freq = n / sum(n) * 100) %>% 
  dplyr::select(-n)  %>% 
  ungroup() %>% 
  spread(key = "reason_cat", value = "freq") 

colnames(school_nonreceipt) = c("school", "logistics", "nonbelief","sliv_specific")

school_data = school_covars %>% right_join(school_nonreceipt, by = "school") %>% 
  mutate(per.API = per.Asian + per.PI + per.Filipino) %>% 
  filter(district %in% c("Oakland Unified", "West Contra Costa Unified")) %>% 
  mutate(tr = ifelse(district == "Oakland Unified", "intervention", "comparison"))

intervention_schools = school_data %>% 
  filter(district == "Oakland Unified") %>% 
  mutate(logistics_above_median = ifelse(logistics >= median(logistics), TRUE, FALSE),
         nonbelief_above_median = ifelse(nonbelief >= median(nonbelief), TRUE, FALSE))

comparison_schools = school_data %>% 
  filter(district == "West Contra Costa Unified") %>% 
  mutate(logistics_above_median = ifelse(logistics > median(logistics), TRUE, FALSE),
         nonbelief_above_median = ifelse(nonbelief > median(nonbelief), TRUE, FALSE))

school_data = bind_rows(intervention_schools, comparison_schools)

table_output = function(school_char, nonreceipt_var, district){
  fit = glm(paste0(school_char, " ~ ", nonreceipt_var), 
            data = school_data %>% filter(tr == !!district), 
            family = "poisson")
  return(data.frame(district = district,
                    characteristic = school_char,
                    below_med_avg = round(exp(coefficients(fit)[[1]]), 2), 
                    above_med_avg = round(exp(sum(coefficients(fit))), 2), 
                    pval = round(summary(fit)$coefficients[2,4], 2)))
}

school_vars = c("per.White", "per.API", "per.Hispanic", "per.AfAmer", "per.Twoplus",
                "per.freelunch", "per.englearn")

school_data %>% filter(tr == "intervention") %>% group_by(logistics_above_median) %>% summarise(mean(per.freelunch))

intervention_logistics_tbl = 
  lapply(school_vars, function(x) table_output(x, "logistics_above_median", "intervention")) %>% 
  bind_rows() 

intervention_nonbelief_tbl = 
  lapply(school_vars, function(x) table_output(x, "nonbelief_above_median", "intervention")) %>% 
  bind_rows() 

intervention_tbl = bind_cols(intervention_nonbelief_tbl %>% dplyr::select(-district), 
                             intervention_logistics_tbl %>% dplyr::select(-district, -characteristic))

write.csv(intervention_tbl, paste0(local_tab_path, "/intervention_schools_reasons_for_nonreceipt.csv"))

comparison_logistics_tbl = 
  lapply(school_vars, function(x) table_output(x, "logistics_above_median", "comparison")) %>% 
  bind_rows()

comparison_nonbelief_tbl = 
  lapply(school_vars, function(x) table_output(x, "nonbelief_above_median", "comparison")) %>% 
  bind_rows()

comparison_tbl = bind_cols(comparison_nonbelief_tbl %>% dplyr::select(-district), 
                           comparison_logistics_tbl %>% dplyr::select(-district, -characteristic))

write.csv(comparison_tbl, paste0(local_tab_path, "/comparison_schools_reasons_for_nonreceipt.csv"))

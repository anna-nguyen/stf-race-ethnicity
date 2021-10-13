##########################################
# Shoo the Flu evaluation
# Vaccination coverage analysis

# Generate table and figure of 
# distributions of reasons for non-receipt
##########################################

rm(list=ls())

source(here::here("0-config.R"))
library(tidyr)
library(lmtest)
library(data.table)

nonreceipt_tbl = readRDS(paste0(local_res_path, "/reasons_for_nonreceipt.RDS")) %>% 
  mutate(race = ifelse(race == "Latino", "Hispanic/Latino", race))

## Find estimates and CIs
race_vars = rep(c("White", "Asian/Pacific Islander", "Multiple Races", "Black/African American", "Hispanic/Latino"), 3)
reason_vars = rep(c("Logistics", "Nonbelief", "SLIVreason"), 5)

nonreceipt = data.frame(race = race_vars,
                        reason = reason_vars) %>% 
  arrange(race)


find_nonreceipt_ci = function(nonreceipt_row){
  filtered_race_tbl = nonreceipt_tbl %>% filter(race == nonreceipt_row[["race"]])
  
  glm_equation = paste0(nonreceipt_row[["reason"]], " ~ district")
  nr_glm = glm(glm_equation, data = filtered_race_tbl)
  nr_cov = sandwichSE(fm = nr_glm, cluster = filtered_race_tbl$matchid)
  nr_rfit = coeftest(nr_glm, nr_cov)
  
  n_ousd = nrow(filtered_race_tbl %>% filter(district == "OUSD"))
  prop_ousd = nr_rfit[1, 1]
  se_ousd = nr_rfit[1, 2]
  ci_ousd = calculate_wilson_ci(prop_ousd, se_ousd)
  
  n_wccusd = nrow(filtered_race_tbl %>% filter(district == "WCCUSD"))
  prop_wccusd = nr_rfit[1, 1] + nr_rfit[2, 1]
  se_wccusd = sqrt(nr_cov[1,1] + nr_cov[2,2] + 2 * nr_cov[1,2])
  ci_wccusd = calculate_wilson_ci(prop_wccusd, se_wccusd)
  
  output_tbl = data.frame("district" = c("OUSD", "WCCUSD"), 
                          "race" = nonreceipt_row[["race"]],
                          "reason" = nonreceipt_row[["reason"]],
                          "n" = c(n_ousd, n_wccusd),
                          "est" = c(prop_ousd, prop_wccusd), 
                          "lb" = c(ci_ousd[1], ci_wccusd[1]),
                          "ub" = c(ci_ousd[2], ci_wccusd[2]))
  
  return(output_tbl)
}

nonreceipt_estimates = apply(nonreceipt, 1, find_nonreceipt_ci) %>% bind_rows() %>% 
  filter(!(district == "WCCUSD" & reason == "SLIVreason")) %>% 
  mutate(District = ifelse(district == "OUSD", "Intervention District", "Comparison District"),
         reason = ifelse(reason == "Nonbelief", "Non-belief", reason), 
         reason = ifelse(reason == "SLIVreason", "SLIV-specific concerns", reason)) %>% 
  rename("Race" = "race")

nonreceipt_estimates$District = factor(nonreceipt_estimates$District, levels = c("Intervention District", "Comparison District"))
nonreceipt_estimates$reason = factor(nonreceipt_estimates$reason, 
                                     levels = c("Logistics", "Non-belief", "Distrust", "SLIV-specific concerns"))

write.csv(nonreceipt_estimates, paste0(local_tab_path, "/nonreceipt_estimates.csv"))

nonreceipt_barplot = ggplot(nonreceipt_estimates, aes(x = reason, y = est * 100, fill = Race)) +
  facet_grid( ~ District, scale = "free") +
  geom_bar(stat = "identity", width = 0.7, position = "dodge") + 
  geom_errorbar(aes(ymin = lb * 100, ymax = ub * 100), position = position_dodge(0.7),width = 0.2) + 
  xlab("Reasons for Non-Receipt") + 
  ylab("Percent of Responses") +
  theme_complete_bw() + 
  theme(legend.position = "bottom")

ggsave(filename = paste0(local_plot_path, "/nonreceipt_barplot.png"), plot = nonreceipt_barplot, width = 8, height = 4)

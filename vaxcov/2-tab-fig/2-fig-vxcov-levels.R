##########################################
# Shoo the Flu evaluation
# Vaccination coverage analysis

# Plot vaccine coverage levels for each 
# race/ethnicity in both districts for
# each season
##########################################

rm(list=ls())

# define directories, load libraries
source(here::here("0-config.R"))
library(gridExtra)

vaxcov_tbl = readRDS(paste0(local_res_path, "/race_dist_vaxcov.RDS"))
              
vaxcov_plt_tbl = vaxcov_tbl %>% 
  mutate(race = case_when(race == "API" ~ "Asian/Pacific Islander", 
                          race == "Latino" ~ "Hispanic/Latino",
                          race == "Black" ~ "Black/African American", 
                          race == "Multi" ~ "Multiple Races", 
                          TRUE ~ race), 
         est = 100 * est, lb = 100 * lb, ub = 100 * ub, 
         District = ifelse(District == "OUSD", "Intervention", "Comparison"))

vaxcov_plt_tbl$race = factor(vaxcov_plt_tbl$race, 
                             levels = sort(unique(vaxcov_plt_tbl$race), decreasing = T))

write.csv(vaxcov_plt_tbl, paste0(local_tab_path, "/vaxcov_levels_tbl.csv"))

vaxcov_plt = ggplot(vaxcov_plt_tbl, aes(x = race, y = est, fill = District)) + 
  facet_wrap(~season, ncol = 2) + 
  geom_bar(stat = "identity", width = 0.5, position=position_dodge(0.6))+
  geom_errorbar(aes(x = race, ymin = lb, ymax = ub),
                width = 0.4, position=position_dodge(0.6)) +
  theme_complete_bw() + 
  theme(panel.grid.major.y = element_blank())+
  xlab("")+
  ylab("Vaccine Coverage (95% CI)") +
  theme(legend.position = "bottom") +
  coord_flip()

ggsave(
  filename = paste0(local_plot_path, "/vaxcov_plt_adjedu.png"),
  plot = vaxcov_plt,
  width = 10,
  height = 6
)
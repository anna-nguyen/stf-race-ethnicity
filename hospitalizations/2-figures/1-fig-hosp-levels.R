rm(list=ls())
source(here::here("0-config.R"))

seas_list = matrix(c(1415, 1516, 1617, 1718))
race_list = c("API", "Black", "Hispanic", "White", "white_api")
blank <- grid.rect(gp=gpar(col="white"))


##################################################
# Plot incidence of hospitalizations with 95% CIs
##################################################

data_zip_race = readRDS(paste0(data_dir, "ceip-flu-data-age-sex-race-complete-zip.RDS"))

process_data_zip_race = function(data){
  grouped = data %>% filter(seas %in% seas_list, race %in% race_list) %>% 
                     mutate(race = case_when(race == "API" ~ "White/API", 
                            race == "Black" ~ "Black/African American", 
                            race == "Hispanic" ~ "Hispanic", 
                            race == "White" ~ "White/API")) %>% 
                     group_by(race, dist, seas) %>% 
                     summarize(pyears = sum(N), cases = sum(flucases)) %>%
                     mutate(seas = case_when(seas == 1415 ~ "2014-15", 
                                             seas == 1516 ~ "2015-16", 
                                             seas == 1617 ~ "2016-17",
                                             seas == 1718 ~ "2017-18", 
                                             T ~ "")) %>% 
                    rename("District" = "dist")
  return(grouped %>% ungroup())
  }

all_zip_race = process_data_zip_race(data_zip_race$fluseasCDPH_2_5$all)
eld_zip_race = process_data_zip_race(data_zip_race$fluseasCDPH_2_5$eld)
nonelem_zip_race = process_data_zip_race(data_zip_race$fluseasCDPH_2_5$nonelem)

calculate_incidence = function(cphd_data_row){
  poisson_test_output = poisson.test(as.numeric(cphd_data_row["cases"]), 
                                     T = as.numeric(cphd_data_row["pyears"]))
  poisson_test_ci = poisson_test_output$conf.int
  return(data.frame(est = as.numeric(poisson_test_output$estimate) * 100000, 
                    lb = poisson_test_ci[1] * 100000, 
                    ub = poisson_test_ci[2] * 100000))
}


generate_abs_hosp_plot = function(zip_race_data, age, title){
  incidence_est = zip_race_data %>% select(race, District, seas) %>% 
    bind_cols(apply(zip_race_data, 1, calculate_incidence) %>% bind_rows()) %>% 
    mutate(agecat = age, 
           District = ifelse(District == "OUSD", "Intervention", "Comparison"))
  
  incidence_est$race = factor(incidence_est$race, 
                              levels = sort(unique(incidence_est$race), decreasing = T))
  
  write.csv(incidence_est, paste0(tab_dir, "hosp_levels_", age, ".csv"))
  
  hosp_inc_plt = ggplot(incidence_est, aes(x = race, y = est, fill = District)) + 
    facet_wrap(~seas, ncol = 1) + 
    geom_bar(stat = "identity", width = 0.5, position = position_dodge(0.6))+
    geom_errorbar(aes(x = race, ymin = lb, ymax = ub),
                  width = 0.4, position = position_dodge(0.6)) +
    theme_complete_bw() + 
    theme(panel.grid.major.y = element_blank()) +
    xlab("") +
    ylab("") +
    coord_flip() +
    ggtitle(title) +
    theme(legend.position = "none")
  
  if (age == "nonelem") {
    hosp_inc_plt = hosp_inc_plt +
      theme(legend.position = "bottom")
  } 
  
  return(hosp_inc_plt)
  
}

all_hosp_inc_plt = generate_abs_hosp_plot(all_zip_race, "all", "All Ages")
eld_hosp_inc_plt = generate_abs_hosp_plot(eld_zip_race, "eld", "Older Adults")
nonelem_hosp_inc_plt = generate_abs_hosp_plot(nonelem_zip_race, "nonelem", "Non-Elementary Aged")

legend <- cowplot::get_legend(nonelem_hosp_inc_plt)
yaxis_txt = grid.text("Cummulative Incidence per 100,000 (95% CI)")

hosp_inc_plt = grid.arrange(all_hosp_inc_plt, eld_hosp_inc_plt, ncol = 2)
hosp_inc_plt = grid.arrange(hosp_inc_plt, yaxis_txt, legend, ncol = 1, heights = c(10, 0.5, 0.5))
ggsave(filename = here::here("figures", "hosp_inc_plt.png"), 
       plot = hosp_inc_plt, width = 9, height = 8)
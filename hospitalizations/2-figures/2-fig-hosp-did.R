##########################################
# Shoo the Flu evaluation
# Analysis of lab-confirmed flu data

# Plot difference-in-differences
# Primary analysis stratified by race
##########################################
rm(list=ls())
source(here::here("0-config.R"))

res = readRDS(paste0(res_dir, "flu_surv_differences_race.RDS"))

res_data = res %>%
  filter(parameter =="Difference-in-difference" &
           race %in% c("Black", "Hispanic", "white_api")) %>%
  mutate(Season = factor(
    case_when(
      seas == 1415 ~ "2014-15",
      seas == 1516 ~ "2015-16",
      seas == 1617 ~ "2016-17",
      seas == 1718 ~ "2017-18"
    )
  ),
  Age = factor(
    case_when(
      agecat == "all" ~ "All Ages",
      agecat == "eld" ~ "Older Adults (65+)",
      agecat == "nonelem" ~ "Non-Elementary Aged"
    ), levels = c("All Ages", "Non-Elementary Aged", "Older Adults (65+)")
  ),
  effect = factor(
    case_when(
      agecat == "all" ~ "Total effect",
      agecat == "eld" ~ "Herd effect",
      agecat == "nonelem" ~ "Herd effect"
    ), levels = c("Herd effect", "Total effect")
    )
  )  %>%
  # rescale
  mutate(
    estimate = estimate * 100000,
    lb = lb * 100000,
    ub = ub * 100000
  ) %>%
  rename(Race = race) %>%
  mutate(Race = case_when(
    Race == "Black" ~ "Black/African American",
    Race == "white_api" ~ "White/API",
    TRUE ~ Race
  ))

# drop other in 2017-18 since so few
# drops = which(res_data$Race=="Other" & res_data$Season=="2017-18")
# res_data = res_data[-drops,]

res_data %>% filter(parameter == "Difference-in-difference", 
                    seas == "1718",
                    agecat == "eld")

yellow = "#BB9D00"
green = "#00B81F"
pink = "#E76BF3"
cols = c(yellow, green, pink)
shapes = c(17, 16, 15)


res_data = res_data %>% 
  mutate(Race = case_when(as.character(Race) == "Asian / Pacific Islander" ~ "Asian/Pacific Islander",
                          as.character(Race) == "Two or more races" ~ "Multiple", 
                          TRUE ~ as.character(Race))) %>% 
  filter(Race != "Other")

plt_did_race = ggplot(res_data %>% filter(Age != "Non-Elementary Aged"), 
                      aes(x = Season, y = estimate)) + 
  geom_point(aes(shape = Race), 
             position = position_dodge(width=0.5), size = 2) +
  geom_linerange(aes(ymin = lb, ymax = ub, shape = Race), 
                position = position_dodge(width=0.5)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Difference-in-difference in\ncumulative incidence per 100,000") +
  scale_shape_manual("Type of effect", values = shapes) +
  facet_wrap(~Age, scales = "free") + 
  theme_complete_bw()+
  theme(legend.position = "bottom")

ggsave(here::here("figures", "fluhosp_did_race.png"), 
       plt_did_race,
       width=10, height=4)

plt_did_race_nonelem = ggplot(res_data %>% filter(Age == "Non-Elementary Aged"), 
                      aes(x = Season, y = estimate)) + 
  geom_point(aes(shape = Race), 
             position = position_dodge(width=0.5), size = 2) +
  geom_linerange(aes(ymin = lb, ymax = ub, shape = Race), 
                 position = position_dodge(width=0.5)) + 
  geom_hline(yintercept = 0, linetype = "dashed") +
  ylab("Difference-in-difference in\ncumulative incidence per 100,000") +
  scale_shape_manual("Type of effect", values = shapes) +
  facet_wrap(~Age, scales = "free") + 
  theme_complete_bw()+
  theme(legend.position = "bottom")

ggsave(here::here("figures", "fluhosp_did_race_nonelem.png"), 
       plt_did_race_nonelem,
       width=7.5, height=4)

res_data_tbl = res_data %>% 
  mutate(est = paste0(round(estimate, 2), " (",
                      round(lb, 2), ", ",
                      round(ub, 2), ")")) %>% 
  select(Race, Season, agecat, est) %>% 
  spread(Season, est) %>% 
  mutate(agecat = case_when(agecat == "all" ~ "All Ages", 
                            agecat == "eld" ~ "Older Adults", 
                            agecat == "nonelem" ~ "Non-Elementary School Aged")) %>% 
  arrange(agecat)

write.csv(res_data_tbl, here::here("tables", "res_data_tbl.csv"))

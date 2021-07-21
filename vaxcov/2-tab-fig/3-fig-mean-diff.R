##########################################
# Shoo the Flu evaluation
# Vaccination coverage analysis

# Generate table and figure of mean 
# difference in vax stratified by race
##########################################

rm(list = ls())

# define directories, load libraries
source(here::here("0-config.R"))

all = readRDS(paste0(local_res_path, "/mean_diff_race.csv"))
all_adjedu = readRDS(paste0(local_res_path, "/adj_mean_diff_race.csv"))

# Plot differences
vaxcov_diff_race = ggplot(all, aes(y = pt.est, x = Season)) +
  geom_point(aes(col = Race), position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = lb, ymax = ub, col = Race), position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_complete_bw() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2)) +
  ylab("Difference in percent of students \nvaccinated for influenza")


ggsave(
  filename = paste0(local_plot_path, "/vaxcov_diff_race.png"),
  plot = vaxcov_diff_race,
  width = 5.5,
  height = 4
)


race_tbl = all %>% mutate(diff = paste0(
  format(round(pt.est * 100, 2), nsmall = 2),
  " (",
  format(round(lb * 100, 2), nsmall = 2),
  ", ",
  format(round(ub * 100, 2), nsmall = 2),
  ")"
)) %>%
  dplyr::select(Race, Season, diff) %>%
  arrange(Race) %>%
  spread(Season, diff)

write.csv(race_tbl, paste0(local_tab_path, "/mean_diff_race.csv"))

# Plot differences, adjusted for parental education
vaxcov_diff_race_adjedu =
  ggplot(
    all_adjedu %>% mutate(
      pt.est = pt.est * 100,
      lb = lb * 100,
      ub = ub * 100
    ),
    aes(y = pt.est, x = Season)
  ) +
  geom_point(aes(col = Race), position = position_dodge(width = 0.5)) +
  geom_linerange(aes(ymin = lb, ymax = ub, col = Race), position = position_dodge(width = 0.5)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_complete_bw() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2)) +
  ylab("Difference in percent of students \nvaccinated for influenza (%)")

ggsave(
  filename = paste0(local_plot_path, "/vaxcov_diff_race_adjedu.png"),
  plot = vaxcov_diff_race_adjedu,
  width = 5.5,
  height = 4
)

race_adjedu = all_adjedu %>% mutate(diff = paste0(
  format(round(pt.est * 100, 2), nsmall = 2),
  " (",
  format(round(lb * 100, 2), nsmall = 2),
  ", ",
  format(round(ub * 100, 2), nsmall = 2),
  ")"
)) %>%
  dplyr::select(Race, Season, diff) %>%
  arrange(Race) %>%
  spread(Season, diff)

write.csv(race_adjedu, paste0(local_tab_path, "/mean_diff_race_adjedu.csv"))


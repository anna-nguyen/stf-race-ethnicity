########################################
# Vaccine coverage survey conducted in March 2017
# Create figures for presentation
# Distribution of student race
########################################

rm(list = ls())
# define directories, load libraries
source(here::here("0-config.R"))

load(paste0(clean_data_path_2017, ".RData"))
data <- data.import
load(district_demographics_path)

#relabel race categories
data$race = as.character(data$race)
data$race[data$race == "Black"] = "Black/African American"
data$race[data$race == "Multi"] = "Multiple Races"
data$race[data$race == "Missing"] = "Not reported"
data$race[data$race == "Asian"] = "Asian/Pacific Islander"
data$race[data$race == "Pacific islander"] = "Asian/Pacific Islander"

data$race = as.factor(data$race)
svy.race = as.data.frame(prop.table(table(data$race, data$dist), 2) * 100)
colnames(svy.race) = c("race", "dist", "per")
svy.race$type = "Survey"
svy.race$dist = as.character(svy.race$dist)
svy.race$dist[svy.race$dist == "OUSD"] = "Intervention district"
svy.race$dist[svy.race$dist == "WCCUSD"] = "Comparison district"

dist.race.api = data.frame(lab = "Asian/Pacific Islander",
                           dist.race[dist.race$lab == "Asian", 2:3] +
                             dist.race[dist.race$lab == "Pacific Islander", 2:3]) 
dist.race = dist.race %>% filter(!(lab %in% c("Asian", "Pacific Islander"))) %>% rbind(dist.race.api)
dist.race.l = melt(dist.race)
colnames(dist.race.l) = c("race", "dist", "per")


dist.race.l$dist = as.character(dist.race.l$dist)
dist.race.l$dist[dist.race.l$dist == "oak"] = "Intervention district"
dist.race.l$dist[dist.race.l$dist == "wcc"] = "Comparison district"
dist.race.l$type = "Entire district"
dist.race.l$race[dist.race.l$race == "African American"] = "Black/African American"
dist.race.l$race[dist.race.l$race == "Multiple"] = "Multiple Races"


dist.race.s.api = data.frame(lab = "Asian/Pacific Islander",
                           dist.race.s[dist.race.s$lab == "Asian", 2:3] +
                             dist.race.s[dist.race.s$lab == "Pacific Islander", 2:3]) 
dist.race.s = dist.race.s %>% filter(!(lab %in% c("Asian", "Pacific Islander"))) %>% rbind(dist.race.s.api)
dist.race.s.l = melt(dist.race.s)
colnames(dist.race.s.l) = c("race", "dist", "per")

dist.race.s.l$dist = as.character(dist.race.s.l$dist)
dist.race.s.l$dist[dist.race.s.l$dist == "oak"] = "Intervention district"
dist.race.s.l$dist[dist.race.s.l$dist == "wcc"] = "Comparison district"
dist.race.s.l$type = "Schools\nin sample"
dist.race.s.l$race[dist.race.s.l$race == "African American"] = "Black/African American"
dist.race.s.l$race[dist.race.s.l$race == "Multiple"] = "Multiple Races"

race = rbind(svy.race, dist.race.l, dist.race.s.l)
race$race.f = factor(
  race$race,
  levels = c(
    "Latino",
    "Black/African American",
    "Asian/Pacific Islander",
    "White",
    "Multiple Races",
    "Native American",
    "Not reported"
  )
)

race$per.f = sprintf("%0.0f", race$per)

race = race[order(race$type, race$dist, race$race.f), ]

race$per.f[race$race == "Native American"] = ""
race$per.f[race$race == "Pacific Islander"] = ""
race$per.f[race$race == "Not reported"] = ""
race$per.f[race$race == "Multiple Races" &
             race$type == "Entire district"] = ""
race$per.f[race$race == "Multiple Races" &
             race$type == "Schools\nin sample"] = ""

race$printper=c(
  # Comparison, entire district
  75,38,22,8,0,0,0,
  # Intervention, entire district
  78,44,25,12,0,0,0,
  # Comparison, schools in sample
  74,37,18,6,0,0,0,
  # Intervention, schools in sample
  78,44,18,8,0,0,0,
  # Comparison, surveys
  75,44,30,19,9,0,0,
  # Intervention, surveys
  78,50,31,17,8,0,0)

plt_race_dist =
  ggplot(race, aes(y = per, x = type, fill = race.f)) +
  geom_bar(stat = "identity", width = .7) +
  theme_complete_bw() +
  ylab("Percent") +
  xlab("") +
  geom_text(
    mapping = aes(label = per.f, y = printper),
    size = 3,
    show.legend = FALSE
  ) +
  facet_grid(~ dist) + 
  theme(legend.title = element_blank()) +
  theme(strip.text.x = element_text(size = 14))

ggsave(
  paste0(local_plot_path, "/fig-race-dist.png"),
  plt_race_dist,
  width = 8,
  height = 4
)

##########################################
# Shoo the Flu evaluation
# Vaccination coverage analysis

# Generate figure of distributions 
# of location of vaccination
##########################################

rm(list=ls())

# define directories, load libraries
source(here::here("0-config.R"))
library(purrr)

vxloc_race_tbl = readRDS(paste0(local_res_path, "/vax_location.RDS"))

vxloc_race_plt = ggplot(vxloc_race_tbl, 
                        aes(fill = vxloc, y = Percent, x = season)) + 
  facet_grid(district~ Race) + 
  geom_bar(position = "stack", stat = "identity") + 
  xlab("Season") + 
  labs(fill = "Vaccination Location") +
  theme_complete_bw() +
  theme(legend.position = "bottom")       


ggsave(filename = paste0(local_plot_path, "/vaxlocation_barplot.png"), 
       plot = vxloc_race_plt, 
       width = 13, height = 6)





##############################################
# Shoo the Flu evaluation
# Analysis of student influenza vaccination coverage

# configure directories and load required libraries
##############################################
#load libraries
renv::restore()

library(here)
library(plyr) #need this for count() command
library(assertthat)
library(dplyr)
library(tidyr)
library(reshape2)
library(sl3)
library(tlverse)
library(SuperLearner)
library(dplyr)
library(ggplot2)
library(gdata)
library(foreign)
library(plyr)
library(foreign)
library(Matching)
library(rdrop2)

#---------------------------------------------
# Source base functions
#---------------------------------------------
source(here::here("0-base-functions", "0-base-functions.R"))
source(here::here("0-base-functions", "0-base-functions-tabfig.R"))
source(here::here("0-base-functions", "theme_complete_bw.R"))

#---------------------------------------------
# Paths to clean / processed datasets
#---------------------------------------------

data_path = here::here("data")

data_path_2017 = paste0(data_path, "/vxcov_2017.csv")
data_path_2018 = paste0(data_path, "/vxcov_2018.csv")

school_data_path = paste0(data_path, "/school_data.csv")
district_demographics_path = paste0(data_path, "/dist_demog.RData")

#---------------------------------------------
# Paths for results, plots, and figures
#---------------------------------------------
local_plot_path = here::here("figures")
local_tab_path = here::here("tables")
local_res_path = here::here("results")

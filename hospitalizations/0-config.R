##########################################
# Shoo the Flu evaluation
# Surveillance analysis

# Configuration file for all scripts

# Load required libraries, source scripts
# Define data and results directories
##########################################
renv::restore()

library(dplyr)
library(zoo)
library(binom)
library(reshape2)
library(ggplot2)
library(grid)
library(gridExtra)
library(assertthat)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(binom)
library(grid)
library(dplyr)
library(scales)
library(here)

source(here::here("0-base-functions/1-base-functions.R"))
source(here::here("0-base-functions/2-analysis-functions.R"))

raw_data_dir = paste0(here::here(),"/data/Untouched/Flu/")
raw_census_dir = here::here("data-census/")
clean_census_dir = here::here("data-census/clean/")

data_dir = paste0(here::here(), "/data/Temp/")
res_dir = paste0(here::here(), "/results/results-data/")
fig_dir = paste0(here::here(), "/figures/")
tab_dir = paste0(here::here(), "/tables/")

cdph_pre17 = paste0(here::here(), "/data-cdph/ILIData_CA_201101_201739.csv")
cdph_1718 = paste0(here::here(), "/data-cdph/ShooTheFluRequest_2017_2018Season.csv")

source(paste0(here::here(), "/2-figures/theme_complete_bw.R"))

#--------------------------------------------
# define zip codes for various analyses
#--------------------------------------------

# primary analysis
ousd.zip=c(94601, 94602, 94603, 94605, 94606, 94607, 94608, 94609, 
           94610, 94611, 94612, 94618, 94619, 94621, 94705, 94613, 
           94704, 94604, 94614, 94615, 94617, 94622, 94623, 94624, 
           94649, 94659, 94660, 94661, 94666)

wcc.zip=c(94530, 94547, 94564, 94707, 94708, 94801, 94803, 94804, 
          94805, 94806, 94850)

# subset to zip codes that do not overlap with school
# district boundaries
ousd.zip.sens=ousd.zip[!ousd.zip %in% c(94705, 94611, 94610, 
                                        94608, 94619)]

wcc.zip.sens=wcc.zip[!wcc.zip %in% c(94708, 94707, 94563)]








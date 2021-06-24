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

my_dropbox_token <- drop_auth()

#---------------------------------------------
# Source base functions
#---------------------------------------------
source(paste0(here::here(), "/0-base-functions/0-base-functions.R"))
source(paste0(here::here(), "/0-base-functions/0-base-functions-tabfig.R"))
source(paste0(here::here(), "/0-base-functions/theme_complete_bw.R"))

#---------------------------------------------
# Paths to raw datasets
#---------------------------------------------
matching_path = "~/Dropbox/Flu/StFData/Matching/"

raw_data_path_2017 = "~/Dropbox/Flu/StFData/2016-2017/Data/Raw/Flu vaccine survey data_4.14.17.csv"
raw_ousd_data_path_2017 = "~/Dropbox/Flu/StFData/2017-2018/Data/Raw/Flu Survey 2018_Dataset_OAK_03.23.18.csv"
raw_wcc_data_path_2017 = "~/Dropbox/Flu/StFData/2017-2018/Data/Raw/Flu Survey 2018_Dataset_WCC_03.23.18.csv"
raw_sdi_data_path = "~/Dropbox/Flu/StFData/2016-2017/Data/Raw/2016 school SDI and add'l measures.csv"
raw_cov_data_path = "~/Dropbox/Flu/StFData/Absentee/Combined-data/BAFS Enrolled Schools Multi-Year - STF 2015 Participation_Rev 2.25.2016.csv"
participation_pre17_data_path = "~/Dropbox/Flu/StFData/2016-2017/Data/Raw/5a-STF-Master-Database-2017.csv"
participation_1718_data_path = "~/Dropbox/Flu/StFData/2017-2018/Data/Raw/5a-STF-Master-Database-2018.csv"
school_data_path = "~/Dropbox/Flu/StFData/Matching/Data/Final/2a-school_covariates.csv"
school_names_data_path = "~/Dropbox/Flu/StFData/2016-2017/Data/Raw/ShootheFlu_SchoolandClassCodes.csv"
school_match_data_path = "~/Dropbox/Flu/StFData/Matching/Data/Temp/Stf_schooldata_1516.RData"

#---------------------------------------------
# Paths to clean / processed datasets
#---------------------------------------------
results_path = "~/Dropbox/Flu/StFData/2016-2017/Data/Temp/"

clean_data_path_2017 = paste0(results_path, "vxcov-import")
clean_data_path_2018 = "~/Dropbox/Flu/StFData/2017-2018/Data/Temp/vxcov-import"

clean_public_data_path_2017 = "~/Dropbox/Flu/StFData/Vax cov/public-data/vxcov_2017"
clean_public_data_path_2018 = "~/Dropbox/Flu/StFData/Vax cov/public-data/vxcov_2018"

complete_data_path_2017 = paste0(results_path, "vxcov")
coverage_participation_data_path = "~/Dropbox/Flu/StFData/2017-2018/Data/Temp/coverage_participation"

#---------------------------------------------
# Paths to results
#---------------------------------------------
matching_clean_schooldata_path = paste0(results_path, "clean_school_data.RData")
all_matches_path = paste0(results_path, "match_output.RData")

district_demographics_path = paste0(results_path, "dist_demog.RData")
vax_results_2017_path = paste0(results_path, "vxcov_results.RData")
vax_results_2018_path = paste0(results_path, "vxcov_results_1718.RData")
vax_standardized_results_path = paste0(results_path, "vxcov_std_results.RData")
vax_school_results_path = paste0(results_path, "vxcov_school_results.RData")
vax_distress_performance_results_path = paste0(results_path, "vxcov_perf_sdi.RData")
vax_coverage_range_results_path = "~/Dropbox/Flu/StFData/2016-2017/Data/Final/vxcov_range_y1-3.csv"

#---------------------------------------------
# Paths to saved figures and tables
#---------------------------------------------
plot_path = "~/Dropbox/Flu/StFData/Vax cov/Figures/"
tab_path = "~/Dropbox/Flu/StFData/Vax cov/Tables/"

#---------------------------------------------
# Paths for Github Release
#---------------------------------------------
local_plot_path = here::here("figures")
local_tab_path = here::here("tables")
local_res_path = here::here("results")

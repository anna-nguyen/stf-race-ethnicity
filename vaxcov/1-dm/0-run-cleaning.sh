#!/bin/bash

# Remove previous results
cd ~/Dropbox/Flu/StFData/2016-2017/Data/Temp/
rm -f "vxcov-import.RData" "vxcov-import.csv" "vxcov.RData" "vxcov.csv"

cd ~/Dropbox/Flu/StFData/2017-2018/Data/Temp/
rm -f "vxcov-import.RData" "vxcov-import.csv" "coverage_participation.RDS"

# Run folder scripts and produce output
cd ~/Documents/crg/stf/stf-race/coverage/1-dm/

R CMD BATCH 1-import-2017.R
R CMD BATCH 2-import-2018.R
R CMD BATCH 3-merge.R
R CMD BATCH 4-merge-vaxcov-participation.R
R CMD BATCH 6-make-public-data.R

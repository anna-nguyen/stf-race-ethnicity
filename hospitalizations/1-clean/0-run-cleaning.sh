#!/bin/bash

# Remove previous results
cd ~/Documents/CRG/stf/stf-race/hospitalizations/data/Temp/
rm -f \
"cdph_season.RDS" \
"ceip-flu-clean.RDS" "ceip-flu-clean-sens.RDS" \
"ceip-flu-los-clean.RDS" \
"Censpop-ageyrs-sex-dist.RDS" \
"Censpop-ageyrs-sex-zip.RDS" "Censpop-ageyrs-sex-zip-sens.RDS" \
"Censpop-agecat-sex-race-dist.RDS" \
"Censpop-agecat-sex-race-zip.RDS" "Censpop-agecat-sex-race-zip-sens.RDS" "Censpop-agecat-sex-race-zip-race-complete.RDS" \
"~/Dropbox/Flu/StFData/Census/Censpop-agecat-sex-race-zip2.RData" \
"ACSyearpop.RDS" \
"ceip-flu-data-age-sex-zip.RDS" "ceip-flu-data-age-sex-race-zip.RDS" "ceip-flu-data-age-sex-race-complete-zip.RDS" "ceip-flu-data-noICU-age-sex-race-zip.RDS" "ceip-flu-data-ICU-age-sex-race-zip.RDS""ceip-flu-data-fpsens-age-sex-race-zip.RDS" "ceip-flu-data-age-sex-race-dist.RDS" "ceip-flu-data-age-sex-race-zipsens.RDS" "ceip-flu-data-age-acs.RDS" \
"ceip-icu_death-data-age-sex-zip.RDS"

cd ~/Documents/CRG/stf/stf-race/hospitalizations/results/figures/
rm -f "cdph_fluseas_cutoff_all.pdf" "cdph_fluseas_cutoff_2.pdf"

# Run folder scripts and produce output
cd ~/Documents/CRG/stf/stf-race/hospitalizations/1-clean/

R CMD BATCH 0-prep-cdph-fluseas.R
R CMD BATCH 1a-eip-flu-clean.R
R CMD BATCH 1a-eip-flu-LOS-clean.R
R CMD BATCH 2a-census-age-sex-dist-clean.R
R CMD BATCH 2a-census-age-sex-zip-clean.R
R CMD BATCH 2b-census-age-sex-race-dist-clean.R
R CMD BATCH 2b-census-age-sex-race-zip-clean.R
R CMD BATCH 2c-census-age-sex-race-zipSAVEzip-clean.R
R CMD BATCH 3-acs-clean.R
R CMD BATCH 4-merge-flu-pop.R
R CMD BATCH 5-merge-flu-pop-icu-death.R

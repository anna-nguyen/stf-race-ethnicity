#!/bin/bash
# Run folder scripts and produce output
cd ~/Documents/crg/stf/stf-race/hospitalizations/2-analyze/

R CMD BATCH 1-est-DID-race.R
R CMD BATCH 2-table1-acs-chars.R

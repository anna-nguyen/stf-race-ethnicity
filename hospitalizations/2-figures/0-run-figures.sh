#!/bin/bash

# Run folder scripts and produce output
cd ~/Documents/crg/stf/stf-race/hospitalizations/3-figures/

R CMD BATCH 1-fig-hosp-levels.R
R CMD BATCH 2-fig-hosp-did.R

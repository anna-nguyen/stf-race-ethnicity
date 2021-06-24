#!/bin/bash

R CMD BATCH 0-demographics.R
R CMD BATCH 1-survey-demographics.R
R CMD BATCH 2-mean-diff.R
R CMD BATCH 3-mean-vaxcov-levels.R
R CMD BATCH 4-vax-nonreceipt.R
R CMD BATCH 5-school-specific-nonreceipt.R

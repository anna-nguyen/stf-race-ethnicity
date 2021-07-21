#!/bin/bash

R CMD BATCH 1-fig-race-dist.R
R CMD BATCH 2-fig-vxcov-levels.R
R CMD BATCH 3-fig-mean-diff.R
R CMD BATCH 4-fig-vax-nonreceipt.R

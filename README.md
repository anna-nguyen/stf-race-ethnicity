# Evaluation of a city-wide school-located influenza vaccination program in Oakland, California with respect to race and ethnicity: a matched cohort study

This repository contains data and replication scripts for an evaluation of the Shoo the Flu program, a city-wide school-located influenza vaccination program, with respect to race and ethnicity.

In this evaluation, we assessed the program's impact on vaccination coverage among school aged children (`vaxcov`) and community-wide hospitalizations (`hospitalizations`). The directory structure for each subanalysis is described in the respective subdirectories. The `0-config.R` files in each subfolder loads all dependencies and sets the appropriate directory paths for the data, results, figures, and tables. These configuration files also include a `renv::restore()` call that ensures that loaded packages are the same version that was used in the initial analysis. 

The entire project, including both the vaccination coverage and hospitalization analyses, can be reproduced by running the `0-run-project.sh` bash script. Vaccination coverage data is publicly available (found at `vaxcov/data`), but hospitalization surveillance data is not.


![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)

# Effects of thermal inversion induced air pollution on COVID-19
This repository (in progress) includes the code and data to replicate the findings summarized in our paper. 

Currently it contains only the final data panels for the regression analysis and for creating the figures. 

Code for scraping and processing the raw data and for building the panels  will be added.

Scripts need to be executed from the root directory of the repository and require the softwares R and Stata. 



## Data

All data files are stored in the folder `data/`.

Panels for regressions at the weekly level: `cases_weekly.RData` and `deaths_weekly.RData ` 

Panels for regressions at the daily level: `data_daily.RData`

Panels for IV estimation: `2sls_bootstrap_deaths.dta` , `2sls_bootstrap_cases.dta`

Data for plotting the specification chart in Figure 3 are in the folder `data/coefficients_specification_chart/`.



## Required packages

To run the scripts the following packages need to be installed and loaded.

In R:

```
neededPackages = c("dplyr","purrr", "tidyr", "fixest", "ggplot2", "viridis", "ggthemes") 

allPackages    = c(neededPackages %in% installed.packages()[ , "Package"])

if(!all(allPackages)) {
  missingIDX = which(allPackages == FALSE)
  needed     = neededPackages[missingIDX]
  lapply(needed, install.packages)
}

lapply(neededPackages, library, character.only = TRUE)
```

In Stata: 

```
ssc install reghdfe,  replace
ssc install ppmlhdfe, replace
```



## Regression Analysis

All analysis scripts are stored in the folder `code/regression_analysis/`. They can be executed in any order. 

First stage regressions presented in Table 1 as well as reduced form regressions presented in Table 6  are estimated in `regression_analysis_weekly.R`.

The regressions at the daily level presented in Table 3 and 4 are estimated in `regression_analysis_daily.R`.

IV regression presented in Table 2 are estimated in Stata  with `twostage_bootstrap_batches.do`.

Robustness checks are conducted in the script `regression_analysis_robustness.R`.  This script produces the four RData-files stored in `data/coefficients_specification_chart/` which are required for creating Figure 3. 



## Figures

The scripts to create the figures are stored in the folder `code/figure/`. 

The script `figure1.R` creates the heatmap in Figure 1.

The script `figure2.R` creates the spaghetti-plot in Figure 2.

The script `figure3.R` creates specification chart in Figure 3.



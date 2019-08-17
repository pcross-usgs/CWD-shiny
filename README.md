# README #

This is a repository with R code for an interactive Shiny application of CWD disease models. Currently there are deterministic and stochastic models that are intended to project out scenarios for a 5 to 10 year window over which we assume that the management and vital rates are kept constant. The models are sex and age structured with direct and indirect transmission. 

### Webpage for the shiny apps###  

https://paulchafeecr.shinyapps.io/comb_app_v2_CWD/

### How do I get set up? ###

Dependencies: shiny, popbio, tidyverse, cowplot, magrittr, reshape2, knitr, ggridges, shinydashboard, markdown.  

### Files ###

/code/  

- create_params_det.r: script that creates a list of parameters that can be passed to the model function.  
- create_params_det.r: same as above but for the stochastic model  
- run_det_mod.r: script to do a test run of the deterministic model and construct the plots.  
- run_stoch_mod.4: same for the stochastic model  

/app/   

- allocate_deaths.r function that randomly chooses which infected individuals die from hunting across the 10 subcategories  
- app.r script that runs the shiny app.   
- cwd_det_model.r function for the deterministic model  
- cwd_stoch_model.r function for the stochastic model  
- description_combo.Rmd R markdown for the first page of the shiny app  
- est_beta_params.r function to convert the mean and variance to shape and scale parameters of the Beta distribution  
- plot_compare_scenarios.r functions to compare the different scenarios
- plot_fxns.r plotting of the deterministic model   
- plot_params.r functions to help plot the parameters  
- plot_stoch_fxns.r function to plot results of the stochastic model.  

- The text files are general text descriptions that are inserted into the shiny app  
- UI files are user interface files for the app  
- server are server files for the app  
- compare files are used in the comparison of different scenarios  
- compare files that have the suffix 2 help construct the plots that directly compare the two scenarios in the same plot.  

### Who do I talk to? ###

Paul C Cross  
US Geological Survey  
Northern Rocky Mountain Science Center  
pcross@usgs.gov

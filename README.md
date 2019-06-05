# README #

This is a code repository to test out interactive R Shiny applications of CWD disease models. Currently there are deterministic and stochastic models that are intended to project out scenarios for a 5 to 10 year window over which we assume that the management and vital rates are kept constant. 

### Webpages for the shiny apps###  

Combined version 2 application:  
https://paulchafeecr.shinyapps.io/comb_app_v2_CWD/

### How do I get set up? ###

Dependencies: shiny, popbio, tidyverse, cowplot, magrittr, reshape2, knitr, ggridges, shinycssloaders, shinydashboard, markdown.  

### Folders ###
code: holds scripts and function files to run the models outside of the shiny environment. This is mostly for testing purposes prior to porting over to the shiny folders. Many of the code files are duplicated in the shiny folders. Not sure of a better system at the moment for referencing common files across shiny apps except by duplicating them and locating them in the local folder. 

comb_app_v2: version 2 of the deterministic and stochastic models joined in a single shiny app.
old: now contains the following sub-folders  
- det_app: deterministic shiny model with a constant disease transmission rate  
- det_app2: deterministic shiny model with dynamic disease transmission   
- stoch_app: stochastic shiny model with a constant disease transmission rate  
- stoch_app2: stochastic shiny model with a dynamic disease transmission rate  

output: stores some of the created parameter files. Only used for testing purposes. 


### Issues ###
1. doe:buck ratios seem high compared to what are often observed in the field.  
2. No density dependence on vital rates  
3. No change in the environmental reservoir  
4. Not sure how to implement hot spot targeted hunting. Currently allowing for positive individuals to be more likely to be hunted compared to the background prevalence via the "relative risk" parameter.

### Who do I talk to? ###

Paul C Cross  
US Geological Survey  
Northern Rocky Mountain Science Center  
pcross@usgs.gov

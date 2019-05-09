#About the chronic wasting disease model

This application is a decision-support tool for natural resource managers interested in running different scenarios associated with chronic wasting disease in deer and elk of North America. The model is sex and age structured in order to allow for management strategies that may target different sex and age groups. This discrete time model runs on a monthly timestep. We assume the model starts in May and individuals move to the next age category at the start of May. All births occur in June and all hunting mortality is assumed to be in November. 

We assume that hunting, disease and natural mortality are all additive and there is no density dependence in any of the vital rates such that the population size will increase or decrease exponentially. The order of operations in the model is as follows: aging, reproduction, natural mortality, hunting mortality, disease mortality, disease transmission.

## Initial conditions  

To initiate the model we contruct a Leslie matrix population model and calculate the stable stage distribution. We currently do not account for disease impacts on the population structure in the calcuation of the initial age and sex distribution. All age classes begin at equal disease prevalence. 

## Sex and Age Structure  

The model has 2 sexes and 12 age categories. We assume that those individuals older than 2 years have the same vital rates. The number of age categories is coded in a way that this would be easy to change (or make as a user input). 

## Transmission  

In this model we assume that there is a constant environmental reservoir that creates a constant probability of being infected from the environment. Direct transmission from infected individuals, however, is a modeled as $\frac{\beta SI}{N^{\theta}}$ for females. To account for the sometimes higher prevalence in males we assume their infection rate is a  $\frac{\beta\gamma SI}{N^{\theta}}$ where $\gamma$ is a scaling function. If males are equally likely to become infecten then $\gamma = 1$. 

## Disease induced mortality  

We use a box-car approach to model disease induced mortality so that time until death is gamma distribution (roughly bell-shaped). In this approach we split the infectious category into 10 sub-categories and individuals move from sub-category 1 to 10 at a constant rate and then die when they move out of the 10th sub-category. This makes the time until death about 2 years. 

## Hunting  

We allow hunting to preferentially kill positive individuals. The intent is to simulate the possibility of increased hunting in hot-spot areas. The user can input the relative risk of hunting positives, which equals 1 in the case of no preference and positives are hunted in proportion to the prevalence. This calculation is based on the 2x2 contingency table of hunted vs not hunted and positive vs negative for disease. If the table looks as follows: 

Table    | hunted | not hunted |totals  |
---------| ------- |---------- |-------| 
positive  |A        |B          |G |
negative  |C        |D          |H| 
total     |E        |F 


The relative risk of hunting a positive is RR = A/(A+B) / [C / (C+D)]. In this case we know RR, the prevalence G/(G+H), and the % hunted E/ (E+F). Using this we can re-arrange to calculate A, B, C, and D. 

We assume that infected individuals are hunted equally across all sub-categories. 

## Stochasticity  

We currently allow for both environmental and demographic stochasticity in the model.  Vital rates and the percent hunted in each age category are pulled annually from a Beta distribution to be constrained between 0 and 1. In every timestep, binomial draws are done to determine reproduction, survival, transmission, mortality, and the movement of infected individuals among subcategories. 

## Plots  
When calculating the total # of individuals we ignore within year variation and only plot the total population size in February. The buck:doe ratio is sampled in December of every year. The fawn:doe ratio is assumed to be taken at the end of the biological year in April.   
  
## Citation
This CWD model was developed by the U.S. Geological Survey, Northern Rocky Mountain Science Center, in collaboration with Montana Fish, Wildlife and Parks. 

The tool may be cited as:
xxx

R functions underlying the model are available through the following data release:

xxx

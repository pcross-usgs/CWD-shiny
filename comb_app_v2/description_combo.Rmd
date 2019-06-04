  
## About the chronic wasting disease model  

This application is a decision-support tool for natural resource managers interested in investigating different scenarios associated with chronic wasting disease (CWD). This tool allows the user to enter parameters for deer and elk vital rates, hunting mortality, and disease transmission and the model will plot the total number of individuals, prevalence, age and sex distribution, and how many deaths were natural, hunting or CWD-related. In addition to this description page there are three other pages that can be accessed via the tabs above. The determinisitic model, as the name implies, includes no stochasticity. Parameters can entered and changed using the sliders on the left hand side of the screen and any time a parameter is changed the model will automatically re-run and plot the new results. The stochastic model has the same structure as the deterministic model, but includes both environmental and demographic stochasticity. Vital rates and hunting mortality varies annually while disease transmission rates are fixed. The distribution of these parameters is shown in the plots section under the Parameters tab. In order to run or re-run the stochastic model the user must first press the "Run Simulation" button. The last page, "Scenario comparison", allows the user to enter two different scenarios and view the distribution of modeled outcomes. This uses the stochastic model and the "Run Simulation" button must be clicked to view the output for each scenario.  
  
###User inputs  

A few of the user inputs require some additional explanation. All survival rates are entered as annual probabilities, while reproductive rates are entered as the number of total offspring per female. Indirect transmission is the annual probability of being infected from the environment, which we assumed to be constant for the duration of the simulation. The direct transmission rate is proportional to $\frac{\beta SI}{N^{\theta}}$, where $I$ and $N$ are the number of infectious individuals and the total number of individuals. $\theta$ determines whether disease transmission depends on the number of infecteds $\theta = 0$ or the prevalence $\theta = 1$ (i.e. density or frequency dependent transmission). Due to the strong effect of $\theta$ on the transmission rate, $\beta$ is calculated from the direct $R_0$ per year, which we define as the expected number of individuals to be infected by an infected individual in a completely susceptible population in one year. By defining this on an annual basis, it does not depend on natural and hunting mortality rates, which are other user inputs. We assume males may be more likely to become infected than females and this is regulated by $\gamma$, which equals one if females and males are equally likely to become infected. The direct $R_0$ entered by the user is assumed to be for females. The direct $R_0$, which accounts for how long infected individuals are likely to live is provided as an output for adult males and females below the plots. 
  
Disease induced mortality is controlled by the rate that infected individuals move through 10 sub-categories. This makes the time to disease-induced deaths bell-shaped and when the index 0.43 the time to disease-induced death is about two years. Finally, the relative hunting risk is the ratio of the probability of an infected vs. uninfected individuals being hunted. When the relative risk is greater than one, infected individuals are morelikely likely to be hunted relative to the background prevalence, which may be due to the behavior of infected individuals or management strategies that target hot-spots of infection. The parameters in the stochastic model are the same with the exception of the number of simulations to be run. The variance in vital rates is currently fixed at 0.005. 

###Model Structure

The underlying model is a discrete time model with a monthly timestep. Each simulation begins in May with all births occuring in June and all hunting occuring in November. The model tracks sex and ages of individuals in susceptible ($S$) and infectious ($I$) categories to allow for hunting strategies that may target different sex and age groups, and may be preferential to infectious individuals. 

We assume that hunting, disease and natural mortality are all additive and there is no density dependence in any of the survival or reproduction rates. As a result, host population size will increase or decrease exponentially. However, the intent of the model is to be useful over timespans of five to ten years. The order of operations in the model is as follows: aging, reproduction, natural mortality, hunting mortality, disease mortality, and disease transmission. To initiate the model we contruct an annual timestep Leslie matrix population model using the reprodution, natural survival and hunting rates. We then calculate the stable stage distribution and assume that all age classes have equal disease prevalence at the start of the simulation.  

*Table 1. Parameters included in the model*  

|Table 1.          | Parameter         | Symbol    | Comments|  
|----------------- | ----------------------- |----------   | ----------------------------------------------|  
|State variables   | Susceptible       | $S_{i,a}$   | $i$ = sex, $a$ = age  |      
|                  | Infectious        | $I_{i,a, j}$ | $j$ = infectious sub-category from 1 to 10 |  
| |  |   | |  
|Vital rates       | natural mortality | $\delta_{i,a}$ |  assumed to be the same for >2 years old within each sex category |
|                | reproduction      | $\alpha_{a>1}$| Fawns are assumed not to reproduce |  
|                | hunting mortality | $\kappa_{i,a}$ |  |
|                | disease mortality | $\rho$ | dictates the rate of movement among infectious sub-categories |
| |  |   | |  
|Transmission    | envi. transmisison| $\epsilon$ | probability from 0 to 1 |
|                | transmission parameter | $\beta$ | baseline female transmission |
|                | male scaling      | $\gamma$ | male transmission = $\beta \gamma$|
|                | mixing parameter  | $\theta$ | $\theta = 1$ is frequency dependent transmission, $\theta = 0$ is density dependent transmission |
| |  |   | |  
|Stochasticity   | vital rate variation  | $\sigma^2$ | $\sigma^2 = 0.005$ for all vital rates
  
  
### Disease induced mortality  

We use a box-car approach to model disease induced mortality so that time until death is $\Gamma$ distribution and roughly bell-shaped. To accomplish this we split the infectious category into 10 sub-categories and individuals move from sub-category 1 to 10 at a constant rate $\rho$ and then die when they move out of the 10th sub-category. One can observe the relationship between $\rho$ and the time until disease-induced death by looked at the parameter plot tab.  

### Hunting  

We allow hunting to preferentially kill positive individuals. The intent is to simulate the possibility of increased hunting in hot-spot areas. The user can input the relative risk of hunting positives, which equals 1 in the case of no preference and positives are hunted in proportion to the prevalence. This calculation is based on the 2x2 contingency table of hunted vs not hunted and positive vs negative for disease shown below. 

*Table 2. Contingency table of hunting versus disease prevalence.*  

Table 2    | hunted     | not hunted  |totals    |
-----------| --------- |------------ |--------- | 
positive   |A        |B          |G       |
negative   |C        |D          |H       | 
total      |E        |F 


In Table 1 the marginal frequencies (*i.e.* the total number of positives, negatives, hunted and not hunted) are known via user inputs. The relative risk of hunting a positive is $RR = \frac{A / (A+B)}{C/(C+D)}$. In this case $RR$, the prevalence $G/(G+H)$, and the % hunted $E/(E+F)$ are known such that we can solve for A, B, C, and D. We assume that infected individuals are hunted equally across all sub-categories. 

### Stochasticity  

We currently allow for both environmental and demographic stochasticity in the model.  Vital rates and the percent hunted in each age category are pulled annually from a Beta distribution so that the probabilities are constrained between 0 and 1. The $\alpha$ and $\beta$ parameters of the Beta distribution relate to the mean $\mu$ and variance $\sigma^2$ as follows: $\alpha=\left(\frac{1-\mu}{\sigma^2}-\frac{1}{\mu}\right)\mu^2$ and 
$\beta=\alpha\left(\frac{1}{\mu}-1\right)$. In every monthly timestep, binomial draws are done to determine reproduction, survival, transmission, mortality, and the movement of infected individuals among subcategories.   

### Plots  

For plotting purposes we subsample the entire monthly timeseries to include only one month per year to avoid displaying the within-year variation.  Total population size is plotted for February. The male:female ratio is sampled in December, and the fawn:doe ratio taken at the end of the biological year in April. Prevalence information is sampled in November. Note that the error bars shown in the stochastic plots are the 5% and 95% quantiles, which may not be estimated well if only a few simulations are run. 
  
### Code and Implementation
  
Code was written using R version 3.4.4 (R Foundation for Statistical Computing, Vienna, AT; www.R-project.org) and the Shiny package version 1.0.5 (Chang et al. 2017). The other packages used were:  
* reshape2: version 1.4.3  
* popbio: version 2.4.4  
* magrittr: version 1.5  
* tidyverse: version 1.2.1  
* cowplot: version 0.9.2  
* ggridges: version 0.5.1  
* knitr: version 1.20  
* shinydashboard: version 0.7.0  
* shinycssloaders: version 0.2.0


All of the source code is available at https://github.com/pcross-usgs/CWD-shiny.  
  
### Citation
This CWD model was developed by the U.S. Geological Survey, Northern Rocky Mountain Science Center, in collaboration with Montana Fish, Wildlife, and Parks. 

The tool may be cited as:

Cross, PC and EA Almberg. 2019. Interactive chronic wasting disease model for scenario planning. URL

*Contact:* For technical assistance or to report outages, please contact pcross@usgs.gov

*Disclaimers*: Although these data have been processed successfully on a computer system at the U.S. Geological Survey (USGS), no warranty expressed or implied is made regarding the display or utility of the data for other purposes, nor on all computer systems, nor shall the act of distribution constitute any such warranty. The USGS or the U.S. Government shall not be held liable for improper or incorrect use of the data described and/or contained herein.  

Any use of trade, firm, or product names is for descriptive purposes only and does not imply endorsement by the U.S. Government.


### References
Chang, W, J Cheng, JJ Allaire, Y Xie, and J McPherson (2017). shiny: Web
  Application Framework for R. R package version 1.0.5. https://CRAN.R-project.org/package=shiny
  
Chang, W, and BB Ribeiro (2018). shinydashboard: Create Dashboards with 'Shiny'.
  R package version 0.7.0. https://CRAN.R-project.org/package=shinydashboard
  
R Core Team (2018). R: A language and environment for statistical computing. R Foundation for
  Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

Sali, A. 2017. shinycssloaders: Add CSS Loading Animations to 'shiny'
  Outputs. R package version 0.2.0.
  https://CRAN.R-project.org/package=shinycssloaders

Stubben, C.J. and Milligan, B.G. 2007. Estimating and Analyzing Demographic Models Using the popbio Package in R. Journal of Statistical Software 22:11.  

Wickham, H (2017). tidyverse: Easily Install and Load the 'Tidyverse'. R package version
  1.2.1. https://CRAN.R-project.org/package=tidyverse

Wickham, H (2007). Reshaping Data with the reshape Package. Journal of Statistical Software, 21(12), 1-20.
URL http://www.jstatsoft.org/v21/i12/.

Wilke, CO (2017). cowplot: Streamlined Plot Theme and Plot Annotations for 'ggplot2'. R
  package version 0.9.2. https://CRAN.R-project.org/package=cowplot

Wilke, CO (2018). ggridges: Ridgeline Plots in 'ggplot2'. R package version 0.5.1.
  https://CRAN.R-project.org/package=ggridges
  
Xie, Y (2018). knitr: A General-Purpose Package for Dynamic Report Generation in R. R package
  version 1.20.

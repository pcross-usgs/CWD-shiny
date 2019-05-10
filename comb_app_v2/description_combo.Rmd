  
## About the chronic wasting disease model  

This application is a decision-support tool for natural resource managers interested in running different scenarios associated with chronic wasting disease (CWD) in deer and elk. The underlying model is a discrete time model with a monthly timestep, that begins in May with all births occuring in June and all hunting occuring in November. The model tracks sex and ages of individuals in susceptible ($S$) and infectious ($I$) categories to allow for hunting strategies that may target different sex and age groups, and may be preferential to infectious individuals. 

We assume that hunting, disease and natural mortality are all additive and there is no density dependence in any of the vital rates. As a result, host population size will increase or decrease exponentially. The order of operations in the model is as follows: aging, reproduction, natural mortality, hunting mortality, disease mortality, and disease transmission. To initiate the model we contruct an annual timestep Leslie matrix population model using the reprodution, natural survival and hunting rates. We then calculate the stable stage distribution and assume that all age classes have equal disease prevalence at the start of the simulation.  

*Table 1. Parameters included in the model*  

|Table 1.        | Parameter         | Symbol    | Comments|  
|--------------- | ----------------------- |----------   | ----------------------------------------------|  
|State variables | Susceptible       | $S_{i,a}$   | $i$ = sex, $a$ = age  |      
|                | Infectious        | $I_{i,a, j}$ | $j$ = infectious sub-category 1 to 10 |  
|Vital rates     | natural mortality | $\delta_{i,a}$ |  assumed to be the same for >2 years old within each sex category |  
|                | reproduction      | $\alpha_{a>1}$| Fawns are assumed not to reproduce |  
|                | hunting mortality | $\kappa+{i,a}$ |  |
|                | disease mortality | $\rho$ | dictates the rate of movement among infectious sub-categories |
|Transmission    | envi. transmisison| $\epsilon$ | probability from 0 to 1 |
|                | transmission parameter | $\beta$ | baseline female transmission |
|                | male scaling      | $\gamma$ | male transmission = $\beta \gamma$|
|                | mixing parameter  | $\theta$ | $\theta = 1$ is frequency dependent transmission, $\theta = 0$ is density dependent transmission |
|Stochasticity   | vitals variation  | $\sigma^2$ | $\sigma^2 = 0.005$ for all vital rates

### Transmission  

We assume that there is an environmental reservoir that creates a probability of being infected that does not change over time. The per capita rate of direct transmission to susceptible females is a modeled as $\frac{\beta \sum I}{N^{\theta}}$ for females, where $N$ is the total number of individuals and $\theta=1$ for freqency dependent transmission or $\theta = 0$ for density dependent transmission. To convert this rate to a monthly probability that a susceptible individuals will be infected we take the exponential: $1-\exp^{-\beta * \frac{\sum I}{\sum N ^ \theta}}$. To account for the sometimes higher prevalence in males we assume their infection rate is multiplied by a scaler, $\gamma$. If males are equally likely to become infecten then $\gamma = 1$. 

### Disease induced mortality  

We use a box-car approach to model disease induced mortality so that time until death is $\Gamma$ distribution and roughly bell-shaped. To accomplish this we split the infectious category into 10 sub-categories and individuals move from sub-category 1 to 10 at a constant rate $\rho$ and then die when they move out of the 10th sub-category. For $\rho = 0.43$ the time until death about 2 years. 

### Hunting  

We allow hunting to preferentially kill positive individuals. The intent is to simulate the possibility of increased hunting in hot-spot areas. The user can input the relative risk of hunting positives, which equals 1 in the case of no preference and positives are hunted in proportion to the prevalence. This calculation is based on the 2x2 contingency table of hunted vs not hunted and positive vs negative for disease shown below. 

*Table 2. Contingency table of hunting versus disease prevalence.*  

Table 2  | hunted  | not hunted |totals  |
----------| ------- |---------- |------- | 
positive  |A        |B          |G       |
negative  |C        |D          |H       | 
total     |E        |F 


In Table 1 the marginal frequencies (*i.e.* the total number of positives, negatives, hunted and not hunted) are known via user inputs. The relative risk of hunting a positive is $RR = \frac{A / (A+B)}{C/(C+D)}$. In this case $RR$, the prevalence $G/(G+H)$, and the % hunted $E/(E+F)$ are known such that we can solve for A, B, C, and D. 

We assume that infected individuals are hunted equally across all sub-categories. 

### Stochasticity  

We currently allow for both environmental and demographic stochasticity in the model.  Vital rates and the percent hunted in each age category are pulled annually from a Beta distribution so that the probabilities are constrained between 0 and 1. The $\alpha$ and $\beta$ parameters of the Beta distribution relate to the mean $\mu$ and variance $\sigma^2$ as follows: 
$$\alpha=\left(\frac{1-\mu}{\sigma^2}-\frac{1}{\mu}\right)\mu^2$$

$$\beta=\alpha\left(\frac{1}{\mu}-1\right)$$. 

In every monthly timestep, binomial draws are done to determine reproduction, survival, transmission, mortality, and the movement of infected individuals among subcategories. 

### Plots  

For plotting purposes we subsample the entire monthly timeseries to include only one month per year to avoid displaying the within-year variation.  Total population size is plotted for February. The buck:doe ratio is sampled in December, and the fawn:doe ratio taken at the end of the biological year in April. Prevalence information is sampled in November. 
  
### Citation
This CWD model was developed by the U.S. Geological Survey, Northern Rocky Mountain Science Center, in collaboration with Montana Fish, Wildlife, and Parks. 

The tool may be cited as:

XXXX

Source code is currently available here: 

https://github.com/pcross-usgs/CWD-shiny


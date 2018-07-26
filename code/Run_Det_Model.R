# Deterministic Discrete Annual age and sex structured model
rm(list = ls())
library(popbio)
library(tidyverse)
source("./code/det_pop_model_fxn.r")
source("./code/plot_fxns.r")
load("./output/params.RData")

##RUN THE model
output <- det.pop.model(params)

#PLOT the results

plot.tots(output, type = "l", ylab = "Total population", xlab = "Year",
          ylim = c(0, 2000), lwd = 3,
          cex = 1.25, cex.lab = 1.25, cex.axis = 1.25)
# all months, ages, sex, disease cat
# if years.only == TRUE then only plot one point per year, otherwise plot every month
plot.all(output, years.only = T)

# prevalence plot over time.
plot.prev(output, type = "l", col = "red", xlab = "year", ylab = "prevalence")

# prevalence plot by age over time
plot.prev.age(output, by.sex = T)

#plot the fawn to adult ratio
plot.fawn.adult(output, type = "l", xlab = "year", ylab = "fawn:adult")
plot.buck.doe(output, type = "l", xlab = "year", ylab = "buck:doe")


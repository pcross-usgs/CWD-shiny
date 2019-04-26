# Deterministic Discrete Annual age and sex structured model
rm(list = ls())
library(popbio)
library(cowplot)
library(tidyverse)
library(reshape2)
source("./code/det_pop_model_fxn_ver2.r")
source("./code/plot_fxns.r")
source("./code/plot_params.r")

load("./output/params_det_v2.RData")

##RUN THE model
out <- det.pop.model.v2(params)

#PLOT the results
par(mfrow = c(1,1))
plot.tots(out$counts)

# prevalence plots
plot.prev.2(out$counts)

# plot the fawn:doe and buck:doe ratios
plot.fawn.buck(out$counts)
# plot the deaths
plot.deaths(out$deaths)
plot.perc.deaths(out$deaths)

# plot time to death
plot.ttd(params$p)


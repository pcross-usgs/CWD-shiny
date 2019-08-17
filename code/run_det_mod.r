# Deterministic Discrete Annual age and sex structured model
rm(list = ls())
library(popbio)
library(cowplot)
library(tidyverse)
library(reshape2)
library(ggridges)
source("./app/cwd_det_model_fxn.r")
source("./app/plot_fxns.r")
source("./app/plot_params.r")
source("./code/create_params_det.r")

##RUN THE model
out <- cwd_det_model(params)

#PLOT the results
par(mfrow = c(1,1))
plot_tots(out$counts)

# prevalence plots
plot_prev_age_end(out$counts)#, ylim = c(0,1))

# plot the fawn:doe and buck:doe ratios
plot_fawn_buck(out$counts)#, ylim = c(0.2, .8))

# plot the deaths
plot_deaths(out$deaths)
plot_perc_deaths(out$deaths)

# plot time to death
plot_ttd(params$p)

##Run a second set of parameters.
params.b <- params
params.b$hunt.mort.ad.m <- .5

outb <- cwd_det_model(params.b)

# comparison plot all
plot_compare_all_det(out, outb)


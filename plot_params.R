# Script to plot the parameters of the stochastic model
#NOT WORKING
rm(list = ls())
library(reshape2)
library(tidyverse)
library(magrittr)
load("./output/params_stoch.RData")
source("./code/estBetaParams.r")

# First draw some values from the distribution
sur.fawn <- params %$%
  estBetaParams(fawn.an.sur, fawn.an.sur.var) %$%
  rbeta(1000, alpha, beta)
sur.juv <- params %$%
  estBetaParams(juv.an.sur, an.sur.var) %$%
  rbeta(1000, alpha, beta)
sur.ad.f <- params %$%
  estBetaParams(ad.an.f.sur, an.sur.var) %$%
  rbeta(1000, alpha, beta)
sur.ad.m <- params %$%
  estBetaParams(ad.an.m.sur, an.sur.var) %$%
  rbeta(1000, alpha, beta)

hunt.fawn <- params %$%
  estBetaParams(hunt.mort.fawn, hunt.mort.var) %$%
  rbeta(1000, alpha, beta)
hunt.juv <- params %$%
  estBetaParams(hunt.mort.juv, hunt.mort.var) %$%
  rbeta(1000, alpha, beta)
hunt.ad.f <- params %$%
  estBetaParams(hunt.mort.ad.f, hunt.mort.var) %$%
  rbeta(1000, alpha, beta)
hunt.ad.m <- params %$%
  estBetaParams(hunt.mort.ad.m, hunt.mort.var) %$%
  rbeta(1000, alpha, beta)


sur.tot.fawn <- sur.fawn*(1 - hunt.fawn)
sur.tot.juv <- sur.juv*(1 - hunt.juv)
sur.tot.ad.f <- sur.ad.f*(1 - hunt.ad.f)
sur.tot.ad.m <- sur.ad.m*(1 - hunt.ad.m)

repro.juv <-  params %$%
  estBetaParams(juv.repro/2, juv.repro.var) %$%
  rbeta(1000, alpha, beta) * 2
repro.ad <-  params %$%
  estBetaParams(ad.repro/2, ad.repro.var) %$%
  rbeta(1000, alpha, beta) * 2


#create a wide data.frame
params.stoch <- data.frame(#sur.fawn = sur.fawn, sur.juv = sur.juv,
                           #sur.ad.f = sur.ad.f, sur.ad.m = sur.ad.m,
                           #hunt.fawn = hunt.fawn, hunt.juv = hunt.juv,
                           #hunt.ad.f = hunt.ad.f, hunt.ad.m = hunt.ad.m,
                           sur.tot.fawn = sur.tot.fawn, sur.tot.juv = sur.tot.juv,
                           sur.tot.ad.f = sur.tot.ad.f, sur.tot.ad.m = sur.tot.ad.m,
                           repro.juv = repro.juv, repro.ad = repro.ad)

params.stoch.2 <-  params.stoch %>%
  gather(#'sur.fawn', 'sur.juv', 'sur.ad.f', 'sur.ad.m',
         #'hunt.fawn', 'hunt.juv', 'hunt.ad.f', 'hunt.ad.m',
         'sur.tot.fawn', 'sur.tot.juv', 'sur.tot.ad.f',
         'sur.tot.ad.m', 'repro.juv', 'repro.ad',
         key = "parameter", value = "value")

# plot the params
theme_set(theme_bw())
library(ggridges)
# plot
g <- ggplot(params.stoch.2, aes(x = value, y = parameter)) +
  geom_density_ridges() + theme_ridges() +
  scale_y_discrete(labels = c("reproduction adult",
                              "reproduction juvenile",
                              "survival male",
                              "survival female",
                              "survival fawn",
                              "survival juvenile"))
g

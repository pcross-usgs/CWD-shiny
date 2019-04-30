# script to create some initial parameters.
rm(list =ls())
library(tidyverse)

##########CONSTANTS#########
n.age.cats <- 12 # age categories
n0 <- 3000 # initial population size
n.years <- 10 # number of years for the simulation

#Annual variance on reproduction; input by user?
fawn.an.repro.var <- 0
juv.an.repro.var <- 0.005
ad.an.repro.var <- 0.005

#Annual variance on survival; input by user?
fawn.an.sur.var <- 0.005
an.sur.var <- 0.005

#Variance of additive hunt mortality; user input?
#Right now, fixed across all age/sex classes
hunt.mort.var <- 0.005

#######USER INPUTS#############
#Initial prevalences; user input
ini.fawn.prev <- 0
ini.juv.prev <- 0.1
ini.ad.f.prev <- 0.2
ini.ad.m.prev <- 0.2

# Annual reproductive rates of fawns/doe; input by user
fawn.repro <- 0
juv.repro <- 0.8
ad.repro  <- 1.7

#Natural Annual survival rates - means; input by user
fawn.an.sur <- 0.4
juv.an.sur <- 0.7
ad.an.f.sur <- 0.9
ad.an.m.sur <- 0.85

#Mean additive hunt mortality; user input
hunt.mort.fawn <- 0.02
hunt.mort.juv <- 0.07
hunt.mort.ad.f <- 0.1
hunt.mort.ad.m <- 0.2

###########
#Transmission
env.foi <- 1 - (0.99^(1/12)) # monthly probability of becoming infected
beta <- 0.007 # direct transmission
beta.m <- 1.1 # transmission rate to males increased by 10%

theta <- .5  # 0 = Density dependent transmission, 1 = Freq. dep. trans.

#dis.mort is addressed by using 10 infectious box cars with a defined probability of transitioning, p.
p <- 0.43 #probability of transitioning between infectious box cars; determines disease-induced mortality rate

#relative risk of hunting a positive case
rel.risk <- 1
###########

#rescaling variance since can't just divide by 2....need to check this step
fawn.repro.var <- (1/2)^2 * fawn.an.repro.var
juv.repro.var <- (1/2)^2 * juv.an.repro.var
ad.repro.var <- (1/2)^2 * ad.an.repro.var

#bundle them into a list
params <- list(fawn.an.sur = fawn.an.sur,
               juv.an.sur = juv.an.sur,
               ad.an.f.sur = ad.an.f.sur,
               ad.an.m.sur = ad.an.m.sur,

               fawn.an.sur.var = fawn.an.sur.var,
               an.sur.var = an.sur.var,

               hunt.mort.fawn = hunt.mort.fawn,
               hunt.mort.juv = hunt.mort.juv,
               hunt.mort.ad.f =  hunt.mort.ad.f,
               hunt.mort.ad.m = hunt.mort.ad.m,
               hunt.mort.var = hunt.mort.var,

               rel.risk = rel.risk,

               fawn.repro = fawn.repro,
               juv.repro = juv.repro,
               ad.repro = ad.repro,

               fawn.repro.var = fawn.repro.var,
               juv.repro.var = juv.repro.var,
               ad.repro.var = ad.repro.var,

               ini.fawn.prev = ini.fawn.prev,
               ini.juv.prev = ini.juv.prev,
               ini.ad.f.prev = ini.ad.f.prev,
               ini.ad.m.prev = ini.ad.m.prev,

               env.foi = env.foi,
               beta = beta,
               beta.m = beta.m,
               theta = theta,
               n0 = n0,
               n.years = n.years,
               n.age.cats = n.age.cats,
               p = p)

save(params, file = "./output/params_stoch_ver2.RData")



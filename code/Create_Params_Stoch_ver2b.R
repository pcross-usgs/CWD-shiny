# script to create some initial parameters.
rm(list =ls())
library(tidyverse)

##########CONSTANTS#########
n.age.cats <- 12 # age categories
n0 <- 2000 # initial population size
n.years <- 30 # number of years for the simulation

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
ini.juv.prev <- 0.05
ini.ad.f.prev <- 0.05
ini.ad.m.prev <- 0.05

# Annual reproductive rates of fawns/doe; input by user
fawn.repro <- 0
juv.repro <- 0.6
ad.repro  <- 1

#Natural Annual survival rates - means; input by user
fawn.an.sur <- 0.6
juv.an.sur <- 0.8
ad.an.f.sur <- 0.9
ad.an.m.sur <- 0.85

#Mean additive hunt mortality; user input
hunt.mort.fawn <- 0.02
hunt.mort.juv.f <- 0.1
hunt.mort.juv.m <- 0.5

hunt.mort.ad.f <- 0.1
hunt.mort.ad.m <- 0.5

###########
#dis.mort is addressed by using 10 infectious box cars with a defined probability of transitioning, p.
p <- 0.43 #probability of transitioning between infectious box cars; determines disease-induced mortality rate


#Transmission
env.foi <- 1 - (0.99^(1/12)) # monthly probability of becoming infected
r0 <- 1.3
theta <- 1  # 0 = Density dependent transmission, 1 = Freq. dep. trans.

beta.f = r0 * (n0^(theta-1)) / (mean(apply(cbind(rnbinom(1000, 1, (1 - ad.an.f.sur^(1/12))),
                                               rnbinom(1000, 1, (1 - (1 - hunt.mort.ad.f)^(1/12))),
                                               rgamma(1000, 10, p)), 1, FUN = min)))
beta.m <- 2 # transmission rate to males increased by twofold

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
               hunt.mort.juv.f = hunt.mort.juv.f,
               hunt.mort.juv.m = hunt.mort.juv.m,
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
               beta.f = beta.f,
               beta.m = beta.m,
               theta = theta,
               n0 = n0,
               n.years = n.years,
               n.age.cats = n.age.cats,
               p = p)

save(params, file = "./output/params_stoch_ver2b.RData")



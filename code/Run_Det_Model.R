# Deterministic Discrete Annual age and sex structured model
rm(list = ls())
library(popbio)
library(tidyverse)
source("./code/det_pop_model_fxn.r")
source("./code/plot_fxns.r")

######PARAMETER VALUES######
#Natural Annual survival rates
fawn.an.sur <- 0.4
juv.an.sur <- 0.7
ad.an.f.sur <- 0.9
ad.an.m.sur <- 0.8

# annual reproductive rates
fawn.rep <- 0
juv.rep <- 0.8
ad.rep  <- 1.7

# disease parameters
dis.mort <- 1-((1-0.3)^(1/12)) # additional disease induced mortality rates per month.
ini.prev <- 0.03 # initial prevalence
foi <- 1 - (0.95^(1/12)) # monthly probability of becoming infected

n0 <- 2000 # initial population size
n.years <- 10 # number of years for the simulation

#hunting
hunt.mort.f <- rep(0.1,12) # added annual hunting mortality over the entire season for females
hunt.mort.m <- rep(0.2,12) # added annual hunting mortality over the entire season for males
hunt.mort.i.f <- rep(0.1,12) #hunting mortality associated with infected females - hot-spot removal
hunt.mort.i.m <- rep(0.2,12) #hunting mortality associated with infected males - hot-spot removal

# other model parameters
n.age.cats <- 12 # age categories
p <- 0.43 #probability of transitioning between infectious box cars

# Calculated parameters
months <- seq(1, n.years*12)
hunt.mo <- rep(0, n.years*12) # months in where the hunt occurs
hunt.mo[months %% 12 == 7] <- 1 # hunt.mo==1 on Nov

#Natural monthly survival rates
fawn.sur <- fawn.an.sur^(1/12)
juv.sur <- juv.an.sur^(1/12)
ad.f.sur <- ad.an.f.sur^(1/12)
ad.m.sur <- ad.an.m.sur^(1/12)

#bundle them into a list
params <- list(fawn.sur = fawn.sur, juv.sur = juv.sur,
               ad.f.sur = ad.f.sur, ad.m.sur = ad.m.sur,
               fawn.rep = fawn.rep, juv.rep = juv.rep,
               ad.rep = ad.rep,
               dis.mort = dis.mort, ini.prev = ini.prev,
               foi = foi, n0= n0, n.years = n.years,
               hunt.mort.f = hunt.mort.f,
               hunt.mort.m = hunt.mort.m,
               hunt.mort.i.f = hunt.mort.i.f,
               hunt.mort.i.m = hunt.mort.i.m,
               n.age.cats = n.age.cats, p = p)

##RUN THE model
output <- det.pop.model(params)

###TEST OUT MULTIPLE RUNS (prep for stoch version)
output.sims <- array(data = NA, dim = length(data), dimnames = NULL)

sims <- 2
out.sims <- vector("list",sims)

for(i in 1:sims){
  out.sims[[i]] <- det.pop.model(params)
}

out.sims.long <- melt(out.sims) %>%
  rename(age = Var1, month = Var2, population = value,
              category = L2, sim = L1)
summary(out.sims.long)




plot.all(out.sims[[2]], years.only = F)

x <- array(unlist(out.sims), dim = c(nrow(out.sims[[1]][[1]]),
                                     ncol(out.sims[[1]][[1]]),
                                     length(out.sims[[1]]),
                                     sims))
#Need to figure out how to melt down a list of lists
dat <- melt(output.sims[[1]])
head(dat)

# Deterministic Discrete Annual age and sex structured model
rm(list = ls())
library(popbio)
library(tidyverse)

######PARAMETER VALUES######
#Natural Annual survival rates
fawn.an.sur <- 0.4
juv.an.sur <- 0.7
ad.an.f.sur <- 0.9
ad.an.m.sur <- 0.8

#Natural monthly survival rates
fawn.sur <- fawn.an.sur^(1/12)
juv.sur <- juv.an.sur^(1/12)
ad.f.sur <- ad.an.f.sur^(1/12)
ad.m.sur <- ad.an.m.sur^(1/12)

# annual reproductive rates
fawn.rep <- 0
juv.rep <- 0.8
ad.rep  <- 1.7

n.age.cats <- 12 # age categories
n0 <- 2000 # initial population size
ini.prev <- 0.03 # initial prevalence
foi <- 1 - (0.95^(1/12)) # monthly probability of becoming infected

dis.mort <- 1-((1-0.3)^(1/12)) # additional disease induced mortality rates per month.
hunt.mort.f <- rep(0.1,12) # added annual hunting mortality over the entire season for females
hunt.mort.m <- rep(0.2,12) # added annual hunting mortality over the entire season for males
hunt.mort.i.f <- rep(0.1,12) #hunting mortality associated with infected females - hot-spot removal
hunt.mort.i.m <- rep(0.2,12) #hunting mortality associated with infected males - hot-spot removal


n.years <- 10 # number of years for the simulation

months <- seq(1, n.years*12)
hunt.mo <- rep(0, n.years*12) # months in where the hunt occurs
hunt.mo[months %% 12 == 7] <- 1 # hunt.mo==1 on Nov



#########CREATE INITIAL CONDITIONS##########
# Create the survival and birth vectors
Sur.an.f <- c(fawn.an.sur, juv.an.sur, rep(ad.an.f.sur, n.age.cats - 2)) # vector of survival rates for 12 age classes
Sur.an.m <- c(fawn.an.sur, juv.an.sur, rep(ad.an.m.sur, n.age.cats - 2)) # vector of survival rates for 12 age classes
Bir <- c(fawn.rep, juv.rep, rep(ad.rep, n.age.cats - 2)) # vector of birth rates
Sur.f <- c(fawn.sur, juv.sur, rep(ad.f.sur, n.age.cats - 2)) # vector of survival rates for 12 age classes
Sur.m <- c(fawn.sur, juv.sur, rep(ad.m.sur, n.age.cats - 2)) # vector of survival rates for 12 age classes

# Construct the sex-age projection matrix
M <- matrix(rep(0, n.age.cats*2 * n.age.cats*2), nrow = n.age.cats*2)
# replace the -1 off-diagonal with the survival rates
M[row(M) == (col(M) + 1)] <- c(Sur.an.f[1:(n.age.cats-1)], 0, Sur.an.m[1:(n.age.cats-1)])
# insert the fecundity vector
M[1, 1:n.age.cats] <- Bir * 0.5
M[n.age.cats +1, 1:n.age.cats] <- Bir * 0.5
lambda(M)

# pre-allocate the output matrices
St.f <- matrix(0, nrow = n.age.cats, ncol = n.years*12)
It.f <- matrix(0, nrow = n.age.cats, ncol = n.years*12)
St.m <- matrix(0, nrow = n.age.cats, ncol = n.years*12)
It.m <- matrix(0, nrow = n.age.cats, ncol = n.years*12)

# Intializing with the stabe age distribution
St.f[,1] <- round(stable.stage(M)[1:n.age.cats] * n0 * 0.5 * (1-ini.prev))
St.m[,1] <- round(stable.stage(M)[(n.age.cats+1):(n.age.cats*2)] * n0 * 0.5 *
                    (1-ini.prev))

# equally allocating prevalence across ages.
It.m[,1] <- round(stable.stage(M)[1:n.age.cats] * n0 * 0.5 * ini.prev)
It.f[,1] <- round(stable.stage(M)[(n.age.cats+1):(n.age.cats*2)] * n0 * 0.5 *
                    ini.prev)


#######POPULATION MODEL############
for(t in 2:(n.years*12)){

  # on birthdays add in recruits and age everyone by one year
  if(t %% 12 == 2){  # births happen in June, model starts in May


    # aging
    St.f[2:n.age.cats, t] <- St.f[1:n.age.cats-1, t-1]
    It.f[2:n.age.cats, t] <- It.f[1:n.age.cats-1, t-1]
    St.m[2:n.age.cats, t] <- St.m[1:n.age.cats-1, t-1]
    It.m[2:n.age.cats, t] <- It.m[1:n.age.cats-1, t-1]

    # reproduction
    St.f[1, t] <- (St.f[1, t-1] * fawn.rep + St.f[2, t-1] * juv.rep +
                     sum(St.f[3:n.age.cats, t-1]) * ad.rep) * 0.5

    St.m[1, t] <- (St.f[1, t-1] * fawn.rep + St.f[2, t-1] * juv.rep +
                     sum(St.f[3:n.age.cats, t-1]) * ad.rep) * 0.5
  }
  if(t %% 12 != 0){
    #updating the next month
    St.f[, t] <- St.f[, t-1]
    It.f[, t] <- It.f[, t-1]
    St.m[, t] <- St.m[, t-1]
    It.m[, t] <- It.m[, t-1]
  }

  ##HUNT MORT then NATURAL MORT, THEN TRANSMISSION
  # need to double check the ordering
  St.f[, t] <- (1 - foi) * ((St.f[,t] * (1 - hunt.mort.f * hunt.mo[t])) * Sur.f)

  It.f[, t] <- foi * ((St.f[,t] * (1 - hunt.mort.f * hunt.mo[t])) * Sur.f) +
    ((It.f[,t] * (1 - hunt.mort.i.f * hunt.mo[t])) * Sur.f) * (1 - dis.mort)

  St.m[, t] <- (1 - foi) * ((St.m[,t] * (1 - hunt.mort.m * hunt.mo[t])) * Sur.m)

  It.m[, t] <- foi * ((St.m[,t] * (1 - hunt.mort.m * hunt.mo[t])) * Sur.m) +
    ((It.m[,t] * (1 - hunt.mort.i.m * hunt.mo[t])) * Sur.m) * (1 - dis.mort)

  # break if the population becomes negative.
  if (sum(St.f[,t] + St.m[,t] + It.f[,t] + It.m[,t]) <= 0) break
}

output <- list(St.f = St.f, St.m = St.m, It.f = It.f, It.m = It.m)

#load functions
source("./code/plot_fxns.r")

#plots
plot.tots(output, type = "l", ylab = "Total population", xlab = "Year", lwd = 3,
          cex = 1.25, cex.lab = 1.25, cex.axis = 1.25)

# all months, ages, sex, disease cat
plot.all(output)

# only years, ages, sex, disease cat
plot.all.yr(output)

# prevalence plot over time.
plot.prev(output, type = "l", col = "red", xlab = "year", ylab = "prevalence")

#plot the fawn to doe ratio
plot.fawn.doe(output, type = "l", xlab = "year", ylab = "fawn:doe")

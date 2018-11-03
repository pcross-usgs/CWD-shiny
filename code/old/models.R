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
I1t.f <- matrix(0, nrow = n.age.cats, ncol = n.years*12)
I2t.f <- matrix(0, nrow = n.age.cats, ncol = n.years*12)
I3t.f <- matrix(0, nrow = n.age.cats, ncol = n.years*12)
I4t.f <- matrix(0, nrow = n.age.cats, ncol = n.years*12)
I5t.f <- matrix(0, nrow = n.age.cats, ncol = n.years*12)
I6t.f <- matrix(0, nrow = n.age.cats, ncol = n.years*12)
I7t.f <- matrix(0, nrow = n.age.cats, ncol = n.years*12)
I8t.f <- matrix(0, nrow = n.age.cats, ncol = n.years*12)
I9t.f <- matrix(0, nrow = n.age.cats, ncol = n.years*12)
I10t.f <- matrix(0, nrow = n.age.cats, ncol = n.years*12)

St.m <- matrix(0, nrow = n.age.cats, ncol = n.years*12)
I1t.m <- matrix(0, nrow = n.age.cats, ncol = n.years*12)
I2t.m <- matrix(0, nrow = n.age.cats, ncol = n.years*12)
I3t.m <- matrix(0, nrow = n.age.cats, ncol = n.years*12)
I4t.m <- matrix(0, nrow = n.age.cats, ncol = n.years*12)
I5t.m <- matrix(0, nrow = n.age.cats, ncol = n.years*12)
I6t.m <- matrix(0, nrow = n.age.cats, ncol = n.years*12)
I7t.m <- matrix(0, nrow = n.age.cats, ncol = n.years*12)
I8t.m <- matrix(0, nrow = n.age.cats, ncol = n.years*12)
I9t.m <- matrix(0, nrow = n.age.cats, ncol = n.years*12)
I10t.m <- matrix(0, nrow = n.age.cats, ncol = n.years*12)

# Intializing with the stable age distribution
St.f[,1] <- round(stable.stage(M)[1:n.age.cats] * n0 * (1-ini.prev))
St.m[,1] <- round(stable.stage(M)[(n.age.cats+1):(n.age.cats*2)] * n0 *
                    (1-ini.prev))

# equally allocating prevalence across ages.
I1t.m[,1] <- round(stable.stage(M)[1:n.age.cats] * n0 * ini.prev)
I1t.f[,1] <- round(stable.stage(M)[(n.age.cats+1):(n.age.cats*2)] * n0 *
                    ini.prev)

#######POPULATION MODEL############
for(t in 2:(n.years*12)){

  # on birthdays add in recruits and age everyone by one year
  if(t %% 12 == 2){  # births happen in June, model starts in May

    # aging
    St.f[2:n.age.cats, t] <- St.f[1:n.age.cats-1, t-1]
    I1t.f[2:n.age.cats, t] <- I1t.f[1:n.age.cats-1, t-1]
    I2t.f[2:n.age.cats, t] <- I2t.f[1:n.age.cats-1, t-1]
    I3t.f[2:n.age.cats, t] <- I3t.f[1:n.age.cats-1, t-1]
    I4t.f[2:n.age.cats, t] <- I4t.f[1:n.age.cats-1, t-1]
    I5t.f[2:n.age.cats, t] <- I5t.f[1:n.age.cats-1, t-1]
    I6t.f[2:n.age.cats, t] <- I6t.f[1:n.age.cats-1, t-1]
    I7t.f[2:n.age.cats, t] <- I7t.f[1:n.age.cats-1, t-1]
    I8t.f[2:n.age.cats, t] <- I8t.f[1:n.age.cats-1, t-1]
    I9t.f[2:n.age.cats, t] <- I9t.f[1:n.age.cats-1, t-1]
    I10t.f[2:n.age.cats, t] <- I10t.f[1:n.age.cats-1, t-1]

    St.m[2:n.age.cats, t] <- St.m[1:n.age.cats-1, t-1]
    I1t.m[2:n.age.cats, t] <- I1t.m[1:n.age.cats-1, t-1]
    I2t.m[2:n.age.cats, t] <- I2t.m[1:n.age.cats-1, t-1]
    I3t.m[2:n.age.cats, t] <- I3t.m[1:n.age.cats-1, t-1]
    I4t.m[2:n.age.cats, t] <- I4t.m[1:n.age.cats-1, t-1]
    I5t.m[2:n.age.cats, t] <- I5t.m[1:n.age.cats-1, t-1]
    I6t.m[2:n.age.cats, t] <- I6t.m[1:n.age.cats-1, t-1]
    I7t.m[2:n.age.cats, t] <- I7t.m[1:n.age.cats-1, t-1]
    I8t.m[2:n.age.cats, t] <- I8t.m[1:n.age.cats-1, t-1]
    I9t.m[2:n.age.cats, t] <- I9t.m[1:n.age.cats-1, t-1]
    I10t.m[2:n.age.cats, t] <- I10t.m[1:n.age.cats-1, t-1]

    # reproduction
    I_fawn <- (I1t.f[1, t-1] + I2t.f[1, t-1] + I3t.f[1, t-1] + I4t.f[1, t-1] +
                 I5t.f[1, t-1] + I6t.f[1, t-1] + I7t.f[1, t-1] + I8t.f[1, t-1] +
                 I9t.f[1, t-1] + I10t.f[1, t-1])

    I_juv <- (I1t.f[2, t-1] + I2t.f[2, t-1] + I3t.f[2, t-1] + I4t.f[2, t-1] +
                I5t.f[2, t-1] + I6t.f[2, t-1] + I7t.f[2, t-1] + I8t.f[2, t-1] +
                I9t.f[2, t-1] + I10t.f[2, t-1])

    I_adults <- (I1t.f[3:n.age.cats, t-1] + I2t.f[3:n.age.cats, t-1] +
                   I3t.f[3:n.age.cats, t-1] + I4t.f[3:n.age.cats, t-1] +
                   I5t.f[3:n.age.cats, t-1] + I6t.f[3:n.age.cats, t-1] +
                   I7t.f[3:n.age.cats, t-1] + I8t.f[3:n.age.cats, t-1] +
                   I9t.f[3:n.age.cats, t-1] + I10t.f[3:n.age.cats, t-1])


    St.f[1, t] <- ((St.f[1, t-1] + I_fawn)* fawn.rep +
                     (St.f[2, t-1] + I_juv) * juv.rep +
                     sum((St.f[3:n.age.cats, t-1] + I_adults)) * ad.rep) * 0.5


    St.m[1, t] <- ((St.f[1, t-1] + I_fawn)* fawn.rep +
                     (St.f[2, t-1] + I_juv) * juv.rep +
                     sum((St.f[3:n.age.cats, t-1] + I_adults)) * ad.rep) * 0.5
  }

  if(t %% 12 != 2){
    #updating the next month
    St.f[, t] <- St.f[, t-1]
    I1t.f[, t] <- I1t.f[, t-1]
    I2t.f[, t] <- I2t.f[, t-1]
    I3t.f[, t] <- I3t.f[, t-1]
    I4t.f[, t] <- I4t.f[, t-1]
    I5t.f[, t] <- I5t.f[, t-1]
    I6t.f[, t] <- I6t.f[, t-1]
    I7t.f[, t] <- I7t.f[, t-1]
    I8t.f[, t] <- I8t.f[, t-1]
    I9t.f[, t] <- I9t.f[, t-1]
    I10t.f[, t] <- I10t.f[, t-1]

    St.m[, t] <- St.m[, t-1]
    I1t.m[, t] <- I1t.m[, t-1]
    I2t.m[, t] <- I2t.m[, t-1]
    I3t.m[, t] <- I3t.m[, t-1]
    I4t.m[, t] <- I4t.m[, t-1]
    I5t.m[, t] <- I5t.m[, t-1]
    I6t.m[, t] <- I6t.m[, t-1]
    I7t.m[, t] <- I7t.m[, t-1]
    I8t.m[, t] <- I8t.m[, t-1]
    I9t.m[, t] <- I9t.m[, t-1]
    I10t.m[, t] <- I10t.m[, t-1]
  }

  ##HUNT MORT then NATURAL MORT, THEN TRANSMISSION
  St.f[, t] <- (1 - foi) * ((St.f[,t] * (1 - hunt.mort.f * hunt.mo[t])) * Sur.f)

  # I1.f.move<-rbinom(n.age.cats, I1t.f[,t], p) #stochastic movement of individuals from I1 to I2
  # I2.f.move<-rbinom(n.age.cats, I2t.f[,t], p)
  # I3.f.move<-rbinom(n.age.cats, I3t.f[,t], p)
  # I4.f.move<-rbinom(n.age.cats, I4t.f[,t], p)
  # I5.f.move<-rbinom(n.age.cats, I5t.f[,t], p)
  # I6.f.move<-rbinom(n.age.cats, I6t.f[,t], p)
  # I7.f.move<-rbinom(n.age.cats, I7t.f[,t], p)
  # I8.f.move<-rbinom(n.age.cats, I8t.f[,t], p)
  # I9.f.move<-rbinom(n.age.cats, I9t.f[,t], p)
  # I10.f.move<-rbinom(n.age.cats, I10t.f[,t], p)


  I1.f.move<-(I1t.f[,t] * p) #deterministic movement of individuals from I1 to I2
  I2.f.move<-(I2t.f[,t] * p)
  I3.f.move<-(I3t.f[,t] * p)
  I4.f.move<-(I4t.f[,t] * p)
  I5.f.move<-(I5t.f[,t] * p)
  I6.f.move<-(I6t.f[,t] * p)
  I7.f.move<-(I7t.f[,t] * p)
  I8.f.move<-(I8t.f[,t] * p)
  I9.f.move<-(I9t.f[,t] * p)
  I10.f.move<-(I10t.f[,t]* p)



  I1t.f[, t] <- foi * ((St.f[,t] * (1 - hunt.mort.f * hunt.mo[t])) * Sur.f) +
    (((I1t.f[,t]-I1.f.move) * (1 - hunt.mort.i.f * hunt.mo[t])) * Sur.f) #suceptibles that survive hunt and natural mortality and then become infected, plus I1 individuals that stay and survive hunt and natural mortality

  I2t.f[, t] <- (((I2t.f[,t] - I2.f.move + I1.f.move) * (1 - hunt.mort.i.f * hunt.mo[t])) * Sur.f)  #

  I3t.f[, t] <- (((I3t.f[,t] - I3.f.move + I2.f.move) * (1 - hunt.mort.i.f * hunt.mo[t])) * Sur.f)  #

  I4t.f[, t] <- (((I4t.f[,t] - I4.f.move + I3.f.move) * (1 - hunt.mort.i.f * hunt.mo[t])) * Sur.f)  #

  I5t.f[, t] <- (((I5t.f[,t] - I5.f.move + I4.f.move) * (1 - hunt.mort.i.f * hunt.mo[t])) * Sur.f)  #

  I6t.f[, t] <- (((I6t.f[,t] - I6.f.move + I5.f.move) * (1 - hunt.mort.i.f * hunt.mo[t])) * Sur.f)  #

  I7t.f[, t] <- (((I7t.f[,t] - I7.f.move + I6.f.move) * (1 - hunt.mort.i.f * hunt.mo[t])) * Sur.f)  #

  I8t.f[, t] <- (((I8t.f[,t] - I8.f.move + I7.f.move) * (1 - hunt.mort.i.f * hunt.mo[t])) * Sur.f)  #

  I9t.f[, t] <- (((I9t.f[,t] - I9.f.move + I8.f.move) * (1 - hunt.mort.i.f * hunt.mo[t])) * Sur.f)  #

  I10t.f[, t] <- (((I10t.f[,t] - I10.f.move + I9.f.move) * (1 - hunt.mort.i.f * hunt.mo[t])) * Sur.f) #


  St.m[, t] <- (1 - foi) * ((St.m[,t] * (1 - hunt.mort.m * hunt.mo[t])) * Sur.m)


  I1.m.move<-round(I1t.m[,t] * p) #deterministic movement of individuals from I1 to I2
  I2.m.move<-round(I2t.m[,t] * p)
  I3.m.move<-round(I3t.m[,t] * p)
  I4.m.move<-round(I4t.m[,t] * p)
  I5.m.move<-round(I5t.m[,t] * p)
  I6.m.move<-round(I6t.m[,t] * p)
  I7.m.move<-round(I7t.m[,t] * p)
  I8.m.move<-round(I8t.m[,t] * p)
  I9.m.move<-round(I9t.m[,t] * p)
  I10.m.move<-round(I10t.m[,t]* p)

  I1t.m[, t] <- foi * ((St.m[,t] * (1 - hunt.mort.m * hunt.mo[t])) * Sur.m) +
    (((I1t.m[,t]-I1.m.move) * (1 - hunt.mort.i.m * hunt.mo[t])) * Sur.m) #suceptibles that survive hunt and natural mortality and then become infected, plus I1 individuals that stay and survive hunt and natural mortality

  I2t.m[, t] <- (((I2t.m[,t] - I2.m.move + I1.m.move) * (1 - hunt.mort.i.m * hunt.mo[t])) * Sur.m)  #

  I3t.m[, t] <- (((I3t.m[,t] - I3.m.move + I2.m.move) * (1 - hunt.mort.i.m * hunt.mo[t])) * Sur.m)  #

  I4t.m[, t] <- (((I4t.m[,t] - I4.m.move + I3.m.move) * (1 - hunt.mort.i.m * hunt.mo[t])) * Sur.m)  #

  I5t.m[, t] <- (((I5t.m[,t] - I5.m.move + I4.m.move) * (1 - hunt.mort.i.m * hunt.mo[t])) * Sur.m)  #

  I6t.m[, t] <- (((I6t.m[,t] - I6.m.move + I5.m.move) * (1 - hunt.mort.i.m * hunt.mo[t])) * Sur.m)  #

  I7t.m[, t] <- (((I7t.m[,t] - I7.m.move + I6.m.move) * (1 - hunt.mort.i.m * hunt.mo[t])) * Sur.m)  #

  I8t.m[, t] <- (((I8t.m[,t] - I8.m.move + I7.m.move) * (1 - hunt.mort.i.m * hunt.mo[t])) * Sur.m)  #

  I9t.m[, t] <- (((I9t.m[,t] - I9.m.move + I8.m.move) * (1 - hunt.mort.i.m * hunt.mo[t])) * Sur.m)  #

  I10t.m[, t] <- (((I10t.m[,t] - I10.m.move + I9.m.move) * (1 - hunt.mort.i.m * hunt.mo[t])) * Sur.m)

    # break if the population becomes negative.
  if (sum(St.f[,t] + St.m[,t] + I1t.f[,t] +  I2t.f[,t] + I3t.f[,t] +
          I4t.f[,t] + I5t.f[,t] + I6t.f[,t] + I7t.f[,t] + I8t.f[,t] +
          I9t.f[,t] + I10t.f[,t] + I1t.m[,t] +  I2t.m[,t] + I3t.m[,t] +
          I4t.m[,t] + I5t.m[,t] + I6t.m[,t] + I7t.m[,t] + I8t.m[,t] +
          I9t.m[,t] + I10t.m[,t]) <= 0) break
}

output <- list(St.f = St.f, St.m = St.m,
               I1t.f = I1t.f, I2t.f = I2t.f, I3t.f = I3t.f, I4t.f = I4t.f,
               I5t.f = I5t.f, I6t.f = I6t.f, I7t.f = I7t.f, I8t.f = I8t.f,
               I9t.f = I9t.f, I10t.f = I10t.f, I1t.m = I1t.m, I2t.m = I2t.m,
               I3tm = I3t.m, I4t.m = I4t.m, I5t.m = I5t.m, I6t.m = I6t.m,
               I7t.m = I7t.m, I8t.m = I8t.m, I9t.m = I9t.m, I10t.m = I10t.m)

#load functions
source("./code/plot_fxns.r")

#PLOTS
plot.tots(output, type = "l", ylab = "Total population", xlab = "Year",
          ylim = c(0, 2000), lwd = 3,
          cex = 1.25, cex.lab = 1.25, cex.axis = 1.25)

# all months, ages, sex, disease cat
plot.all(output, years.only = F)

# prevalence plot over time.
plot.prev(output, type = "l", col = "red", xlab = "year", ylab = "prevalence")

#plot the fawn to adult ratio
plot.fawn.adult(output, type = "l", xlab = "year", ylab = "fawn:adult")

#plot the buck to doe ratio
plot.buck.doe(output, type = "l", xlab = "year", ylab = "buck:doe")

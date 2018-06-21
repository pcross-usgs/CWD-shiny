#Stochastic.2 monthly age and sex structured model that incorporates random draws from distibutions of natural survival, reproduction, and hunt mortality. Currently does not include a distribution on FOI.

rm(list = ls())
#setwd("C:/Users/CFA021/Documents/CWD/CWD management plan/CWD management model")
library(popbio)
library(tidyverse)

######PARAMETER VALUES######
#Natural Annual survival rates - means; input by user
fawn.an.sur <- 0.4
juv.an.sur <- 0.7
ad.an.f.sur <- 0.9
ad.an.m.sur <- 0.8

#Annual variance on survival; input by user?
fawn.an.sur.var <- 0.005
juv.an.sur.var <- 0.005
ad.an.f.sur.var <- 0.005
ad.an.m.sur.var <- 0.005


#Function to estimate alpha and beta for a beta distribution
  estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}


#Estimate alpha & beta values for survival
fawn.sur.alpha <- estBetaParams(fawn.an.sur, fawn.an.sur.var)$alpha
fawn.sur.beta <- estBetaParams(fawn.an.sur, fawn.an.sur.var)$beta

juv.sur.alpha <- estBetaParams(juv.an.sur, juv.an.sur.var)$alpha
juv.sur.beta <- estBetaParams(juv.an.sur, juv.an.sur.var)$beta

ad.f.sur.alpha <- estBetaParams(ad.an.f.sur, ad.an.f.sur.var)$alpha
ad.f.sur.beta <- estBetaParams(ad.an.f.sur, ad.an.f.sur.var)$beta

ad.m.sur.alpha <- estBetaParams(ad.an.m.sur, ad.an.m.sur.var)$alpha
ad.m.sur.beta <- estBetaParams(ad.an.m.sur, ad.an.m.sur.var)$beta


#check distributions in plots
pl.beta <- function(a,b, asp = if(isLim) 1, ylim = if(isLim) c(0,1.1)) {
  if(isLim <- a == 0 || b == 0 || a == Inf || b == Inf) {
    eps <- 1e-10
    x <- c(0, eps, (1:7)/16, 1/2+c(-eps,0,eps), (9:15)/16, 1-eps, 1)
  } else {
    x <- seq(0, 1, length = 1025)
  }
  fx <- cbind(dbeta(x, a,b))
  f <- fx; f[fx == Inf] <- 1e100
  matplot(x, f, ylab="", type="l", ylim=ylim, asp=asp,
          main = sprintf("[dpq]beta(x, a=%g, b=%g)", a,b))
  abline(0,1,     col="gray", lty=3)
  abline(h = 0:1, col="gray", lty=3)
  legend("top", paste0(c("d","p","q"), "beta(x, a,b)"),
         col=1:3, lty=1:3, bty = "n")
  invisible(cbind(x, fx))
}

par(mfrow=c(2,2))
pl.beta(fawn.sur.alpha, fawn.sur.beta)
pl.beta(juv.sur.alpha, juv.sur.beta)
pl.beta(ad.f.sur.alpha, ad.f.sur.beta)
pl.beta(ad.m.sur.alpha, ad.m.sur.beta)


#monthly stochastic survival rates that will be used to initalize the Leslie matrix
fawn.sur<- (rbeta(1, fawn.sur.alpha, fawn.sur.beta, ncp = 0))^(1/12)
juv.sur <- (rbeta(1, juv.sur.alpha, juv.sur.beta, ncp = 0))^(1/12)
ad.f.sur <- (rbeta(1, ad.f.sur.alpha, ad.f.sur.beta, ncp = 0))^(1/12)
ad.m.sur <- (rbeta(1, ad.m.sur.alpha, ad.m.sur.beta, ncp = 0))^(1/12)

# annual reproductive rates of fawns/doe; input by user
fawn.rep <- 0
juv.rep <- 0.8
ad.rep  <- 1.7

#Annual variance on reproduction; input by user?
fawn.an.repro.var <- 0
juv.an.repro.var <- 0.005
ad.an.repro.var <- 0.005

#assume adults always twin and calculate the proportion of females reproducing; fixed
fawns.fawn <- 2
fawns.juv <- 2
fawns.ad <- 2

preg.fawn <- fawn.rep/2
preg.juv <- juv.rep/2
preg.ad <- ad.rep/2

#rescaling variance since can't just divide by 2....need to check this step
fawn.repro.var <- (1/2)^2 * fawn.an.repro.var
juv.repro.var <- (1/2)^2 * juv.an.repro.var
ad.repro.var <- (1/2)^2 * ad.an.repro.var

#Estimate alpha & beta values for the beta distribution of probability of reproducing
fawn.repro.alpha <- estBetaParams(preg.fawn, fawn.repro.var)$alpha
fawn.repro.beta <- estBetaParams(preg.fawn, fawn.repro.var)$beta

juv.repro.alpha <- estBetaParams(preg.juv, juv.repro.var)$alpha
juv.repro.beta <- estBetaParams(preg.juv, juv.repro.var)$beta

ad.repro.alpha <- estBetaParams(preg.ad, ad.repro.var)$alpha
ad.repro.beta <- estBetaParams(preg.ad, ad.repro.var)$beta

#check distributions
par(mfrow=c(2,2))
pl.beta(fawn.repro.alpha, fawn.repro.beta)
pl.beta(juv.repro.alpha, juv.repro.beta)
pl.beta(ad.repro.alpha, ad.repro.beta)

#monthly stochastic reproductive rates that will be used to initalize the Leslie matrix - need to multiply by 2 in the Leslie matrix
fawn.preg.draw <- ifelse(fawn.rep==0, 0, (rbeta(1, fawn.repro.alpha, fawn.repro.beta, ncp = 0))) #when the mean is 0, the beta distribution doesn't work...
juv.preg.draw <- (rbeta(1, juv.repro.alpha, juv.repro.beta, ncp = 0))
ad.preg.draw <- (rbeta(1, ad.repro.alpha, ad.repro.beta, ncp = 0))

n.age.cats <- 12 # age categories
n0 <- 2000 # initial population size

#Initial prevalences; user input
ini.fawn.prev <- 0
ini.juv.prev <- 0.01
ini.ad.f.prev <- 0.02
ini.ad.m.prev <- 0.08

ini.f.prev <- c(ini.fawn.prev, ini.juv.prev, rep(ini.ad.f.prev, (n.age.cats-2))) # initial female prevalence
ini.m.prev <- c(ini.fawn.prev, ini.juv.prev, rep(ini.ad.m.prev, (n.age.cats-2))) # initial male prevalence

#not sure if we need to draw this from a distribution?
foi <- 1 - (0.98^(1/12)) # monthly probability of becoming infected

#dis.mort is addressed by using 10 infectious box cars with a defined probability of transitioning, p.
p <- 0.43 #probability of transitioning between infectious box cars; determines disease-induced mortality rate

#Assuming hunting mortality is drawn from a distribution:
#Mean additive hunt mortality; user input
hunt.mort.fawn <- 0.03
hunt.mort.juv <- 0.07
hunt.mort.ad.f <- 0.1
hunt.mort.ad.m <- 0.2

hunt.mort.f.mean <- c(hunt.mort.fawn, hunt.mort.juv, rep(hunt.mort.ad.f, (n.age.cats-2)))
hunt.mort.m.mean <- c(hunt.mort.fawn, hunt.mort.juv, rep(hunt.mort.ad.m, (n.age.cats-2)))
hunt.mort.i.f.mean <- c(hunt.mort.fawn, hunt.mort.juv, rep(hunt.mort.ad.f, (n.age.cats-2)))#right now, infectious are harvested at same rate as uninfected
hunt.mort.i.m.mean <- c(hunt.mort.fawn, hunt.mort.juv, rep(hunt.mort.ad.m, (n.age.cats-2)))#right now, infectious are harvested at same rate as uninfected

#Variance of additive hunt mortality; user input?  Right now, fixed across all age/sex classes
hunt.mort.f.var <- 0.005
hunt.mort.m.var <- 0.005
hunt.mort.i.f.var <- 0.005
hunt.mort.i.m.var <- 0.005

#Estimate alpha and beta of beta distribution
hunt.mort.f.alpha <- estBetaParams(hunt.mort.f.mean, hunt.mort.f.var)$alpha
hunt.mort.f.beta <- estBetaParams(hunt.mort.f.mean, hunt.mort.f.var)$beta

hunt.mort.m.alpha <- estBetaParams(hunt.mort.m.mean, hunt.mort.m.var)$alpha
hunt.mort.m.beta <- estBetaParams(hunt.mort.m.mean, hunt.mort.m.var)$beta

hunt.mort.i.f.alpha <- estBetaParams(hunt.mort.i.f.mean, hunt.mort.i.f.var)$alpha
hunt.mort.i.f.beta <- estBetaParams(hunt.mort.i.f.mean, hunt.mort.i.f.var)$beta

hunt.mort.i.m.alpha <- estBetaParams(hunt.mort.i.m.mean, hunt.mort.i.m.var)$alpha
hunt.mort.i.m.beta <- estBetaParams(hunt.mort.i.m.mean, hunt.mort.i.m.var)$beta

#stochastic hunting survival rates; right now, it's drawing the hunting mort for each age class
hunt.mort.f<- (rbeta(n.age.cats, hunt.mort.f.alpha, hunt.mort.f.beta, ncp = 0)) # added annual hunting mortality over the entire season for females
hunt.mort.m<- (rbeta(n.age.cats, hunt.mort.m.alpha, hunt.mort.m.beta, ncp = 0))# added annual hunting mortality over the entire season for males
hunt.mort.i.f<- (rbeta(n.age.cats, hunt.mort.i.f.alpha, hunt.mort.i.f.beta, ncp = 0))#hunting mortality associated with infected females - hot-spot removal
hunt.mort.i.m<- (rbeta(n.age.cats, hunt.mort.i.m.alpha, hunt.mort.i.m.beta, ncp = 0))#hunting mortality associated with infected males - hot-spot removal

n.years <- 10 # number of years for the simulation

months <- seq(1, n.years*12)
hunt.mo <- rep(0, n.years*12) # months in where the hunt occurs
hunt.mo[months %% 12 == 7] <- 1 # hunt.mo==1 on Nov

#########CREATE INITIAL CONDITIONS##########
# Create the survival and birth vectors
Sur.an.f <- c(fawn.sur^12, juv.sur^12, rep(ad.f.sur^12, n.age.cats - 2)) # vector of survival rates for 12 age classes
Sur.an.m <- c(fawn.sur^12, juv.sur^12, rep(ad.m.sur^12, n.age.cats - 2)) # vector of survival rates for 12 age classes
Bir <- c(fawn.preg.draw*fawns.fawn, juv.preg.draw*fawns.juv, rep(ad.preg.draw*fawns.ad, n.age.cats - 2)) # vector of birth rates
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

# we could instead make this a list of arrays
for(i in 1:10){
  assign(paste("I", i, "t.f", sep = ""),
         matrix(0, nrow = n.age.cats, ncol = n.years*12))
  assign(paste("I", i, "t.m", sep = ""),
         matrix(0, nrow = n.age.cats, ncol = n.years*12))
}

#No stochasticity built into starting prevalence...

# Intializing with the stabe age distribution
St.f[,1] <- round(stable.stage(M)[1:n.age.cats] * n0 * (1-ini.f.prev))
St.m[,1] <- round(stable.stage(M)[(n.age.cats+1):(n.age.cats*2)] * n0 *
                    (1-ini.m.prev))

# equally allocating prevalence across ages.
I1t.m[,1] <- round(stable.stage(M)[1:n.age.cats] * n0 *  ini.m.prev)
I1t.f[,1] <- round(stable.stage(M)[(n.age.cats+1):(n.age.cats*2)] * n0 *
                    ini.f.prev)

#######POPULATION MODEL############
for(t in 2:(n.years*12)){

  #pull annual variables from a beta distribution:
  if(t %% 12 == 0){
   fawn.sur<- (rbeta(1, fawn.sur.alpha, fawn.sur.beta, ncp = 0))^(1/12)
   juv.sur <- (rbeta(1, juv.sur.alpha, juv.sur.beta, ncp = 0))^(1/12)
   ad.f.sur <- (rbeta(1, ad.f.sur.alpha, ad.f.sur.beta, ncp = 0))^(1/12)
   ad.m.sur <- (rbeta(1, ad.m.sur.alpha, ad.m.sur.beta, ncp = 0))^(1/12)

   #pull annual reproductive rates from a beta distribution:
   fawn.preg.draw <- ifelse(fawn.rep==0, 0, (rbeta(1, fawn.repro.alpha, fawn.repro.beta, ncp = 0))) #when the mean is 0, the beta distribution doesn't work...
   juv.preg.draw <- (rbeta(1, juv.repro.alpha, juv.repro.beta, ncp = 0))
   ad.preg.draw <- (rbeta(1, ad.repro.alpha, ad.repro.beta, ncp = 0))

  }


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

    fawns_born <- sum(rbinom(1, (St.f[1, t-1] + I_fawn), fawn.preg.draw) *  fawns.fawn) +
      sum(rbinom(1, (St.f[2, t-1] + I_juv), juv.preg.draw)*fawns.juv) +
      rbinom(1, (sum(St.f[3:n.age.cats, t-1]) + sum(I_adults)), ad.preg.draw)*fawns.ad

    St.f[1, t] <- rbinom(1, fawns_born, 0.5)
    St.m[1, t] <- fawns_born - St.f[1, t]
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
  # need to double check the ordering

   I1.f.move<-rbinom(n.age.cats, I1t.f[,t], p) #stochastic movement of individuals from I1 to I2
   I2.f.move<-rbinom(n.age.cats, I2t.f[,t], p)
   I3.f.move<-rbinom(n.age.cats, I3t.f[,t], p)
   I4.f.move<-rbinom(n.age.cats, I4t.f[,t], p)
   I5.f.move<-rbinom(n.age.cats, I5t.f[,t], p)
   I6.f.move<-rbinom(n.age.cats, I6t.f[,t], p)
   I7.f.move<-rbinom(n.age.cats, I7t.f[,t], p)
   I8.f.move<-rbinom(n.age.cats, I8t.f[,t], p)
   I9.f.move<-rbinom(n.age.cats, I9t.f[,t], p)
   I10.f.move<-rbinom(n.age.cats, I10t.f[,t], p)


  survive.s.f = rbinom(n.age.cats, (rbinom(n.age.cats, St.f[,t],
                                           (1 - hunt.mort.f * hunt.mo[t]))), Sur.f)
  survive.s.m = rbinom(n.age.cats, (rbinom(n.age.cats, St.m[,t],
                                           (1 - hunt.mort.m * hunt.mo[t]))), Sur.m)

  transmission.f = rbinom(n.age.cats, survive.s.f, foi) #number of individuals becoming infected
  transmission.m = rbinom(n.age.cats, survive.s.m, foi) #number of individuals becoming infected

  St.f[, t] <- survive.s.f - transmission.f
  I1t.f[, t] <- transmission.f + rbinom(n.age.cats,
                                        (rbinom(n.age.cats, (I1t.f[,t]-I1.f.move),
                                                (1 - hunt.mort.i.f * hunt.mo[t]))),
                                        Sur.f) #suceptibles that survive hunt and natural mortality and then become infected, plus I1 individuals that stay and survive hunt and natural mortality

  I2t.f[, t] <- rbinom(n.age.cats, (rbinom(n.age.cats,
                                           (I2t.f[,t] - I2.f.move + I1.f.move),
                                           (1 - hunt.mort.i.f * hunt.mo[t]))), Sur.f)

  I3t.f[, t] <- rbinom(n.age.cats, (rbinom(n.age.cats,
                                           (I3t.f[,t] - I3.f.move + I2.f.move),
                                           (1 - hunt.mort.i.f * hunt.mo[t]))), Sur.f)

  I4t.f[, t] <- rbinom(n.age.cats, (rbinom(n.age.cats,
                                           (I4t.f[,t] - I4.f.move + I3.f.move),
                                           (1 - hunt.mort.i.f * hunt.mo[t]))), Sur.f)

  I5t.f[, t] <- rbinom(n.age.cats, (rbinom(n.age.cats,
                                           (I5t.f[,t] - I5.f.move + I4.f.move),
                                           (1 - hunt.mort.i.f * hunt.mo[t]))), Sur.f)

  I6t.f[, t] <- rbinom(n.age.cats, (rbinom(n.age.cats,
                                           (I6t.f[,t] - I6.f.move + I5.f.move),
                                           (1 - hunt.mort.i.f * hunt.mo[t]))), Sur.f)

  I7t.f[, t] <- rbinom(n.age.cats, (rbinom(n.age.cats,
                                           (I7t.f[,t] - I7.f.move + I6.f.move),
                                           (1 - hunt.mort.i.f * hunt.mo[t]))), Sur.f)

  I8t.f[, t] <- rbinom(n.age.cats, (rbinom(n.age.cats,
                                           (I8t.f[,t] - I8.f.move + I7.f.move),
                                           (1 - hunt.mort.i.f * hunt.mo[t]))), Sur.f)

  I9t.f[, t] <- rbinom(n.age.cats, (rbinom(n.age.cats,
                                           (I9t.f[,t] - I9.f.move + I8.f.move),
                                           (1 - hunt.mort.i.f * hunt.mo[t]))), Sur.f)

  I10t.f[, t] <- rbinom(n.age.cats, (rbinom(n.age.cats,
                                            (I10t.f[,t] - I10.f.move + I9.f.move),
                                            (1 - hunt.mort.i.f * hunt.mo[t]))), Sur.f)


  St.m[, t] <- survive.s.m - transmission.m

   I1.m.move<-rbinom(n.age.cats, I1t.m[,t], p) #stochastic movement of individuals from I1 to I2
   I2.m.move<-rbinom(n.age.cats, I2t.m[,t], p)
   I3.m.move<-rbinom(n.age.cats, I3t.m[,t], p)
   I4.m.move<-rbinom(n.age.cats, I4t.m[,t], p)
   I5.m.move<-rbinom(n.age.cats, I5t.m[,t], p)
   I6.m.move<-rbinom(n.age.cats, I6t.m[,t], p)
   I7.m.move<-rbinom(n.age.cats, I7t.m[,t], p)
   I8.m.move<-rbinom(n.age.cats, I8t.m[,t], p)
   I9.m.move<-rbinom(n.age.cats, I9t.m[,t], p)
   I10.m.move<-rbinom(n.age.cats, I10t.m[,t], p)


  I1t.m[, t] <- transmission.m + rbinom(n.age.cats, (rbinom(n.age.cats, (I1t.m[,t]-I1.m.move), (1 - hunt.mort.i.m * hunt.mo[t]))), Sur.m) #suceptibles that survive hunt and natural mortality and then become infected, plus I1 individuals that stay and survive hunt and natural mortality
  I2t.m[, t] <- rbinom(n.age.cats, (rbinom(n.age.cats, (I2t.m[,t] - I2.m.move + I1.m.move), (1 - hunt.mort.i.m * hunt.mo[t]))), Sur.m)
  I3t.m[, t] <- rbinom(n.age.cats, (rbinom(n.age.cats, (I3t.m[,t] - I3.m.move + I2.m.move), (1 - hunt.mort.i.m * hunt.mo[t]))), Sur.m)
  I4t.m[, t] <- rbinom(n.age.cats, (rbinom(n.age.cats, (I4t.m[,t] - I4.m.move + I3.m.move), (1 - hunt.mort.i.m * hunt.mo[t]))), Sur.m)
  I5t.m[, t] <- rbinom(n.age.cats, (rbinom(n.age.cats, (I5t.m[,t] - I5.m.move + I4.m.move), (1 - hunt.mort.i.m * hunt.mo[t]))), Sur.m)
  I6t.m[, t] <- rbinom(n.age.cats, (rbinom(n.age.cats, (I6t.m[,t] - I6.m.move + I5.m.move), (1 - hunt.mort.i.m * hunt.mo[t]))), Sur.m)
  I7t.m[, t] <- rbinom(n.age.cats, (rbinom(n.age.cats, (I7t.m[,t] - I7.m.move + I6.m.move), (1 - hunt.mort.i.m * hunt.mo[t]))), Sur.m)
  I8t.m[, t] <- rbinom(n.age.cats, (rbinom(n.age.cats, (I8t.m[,t] - I8.m.move + I7.m.move), (1 - hunt.mort.i.m * hunt.mo[t]))), Sur.m)
  I9t.m[, t] <- rbinom(n.age.cats, (rbinom(n.age.cats, (I9t.m[,t] - I9.m.move + I8.m.move), (1 - hunt.mort.i.m * hunt.mo[t]))), Sur.m)
  I10t.m[, t]<- rbinom(n.age.cats, (rbinom(n.age.cats, (I10t.m[,t] - I10.m.move + I9.m.move), (1 - hunt.mort.i.m * hunt.mo[t]))), Sur.m)

if (sum(St.f[,t] + St.m[,t] + I1t.f[,t] +  I2t.f[,t] + I3t.f[,t] + I4t.f[,t] + I5t.f[,t] + I6t.f[,t] + I7t.f[,t] + I8t.f[,t] + I9t.f[,t] + I10t.f[,t] + I1t.m[,t] +  I2t.m[,t] + I3t.m[,t] + I4t.m[,t] + I5t.m[,t] + I6t.m[,t] + I7t.m[,t] + I8t.m[,t] + I9t.m[,t] + I10t.m[,t]) <= 0) break
}

output <- list(St.f = St.f, St.m = St.m,
               I1t.f = I1t.f, I2t.f = I2t.f, I3t.f = I3t.f, I4t.f = I4t.f,
               I5t.f = I5t.f, I6t.f = I6t.f, I7t.f = I7t.f, I8t.f = I8t.f,
               I9t.f = I9t.f, I10t.f = I10t.f, I1t.m = I1t.m, I2t.m = I2t.m,
               I3tm = I3t.m, I4t.m = I4t.m, I5t.m = I5t.m, I6t.m = I6t.m,
               I7t.m = I7t.m, I8t.m = I8t.m, I9t.m = I9t.m, I10t.m = I10t.m)

#load functions
source("./code/plot_fxns.r")
#source("./plot_fxns.r")

#PLOTS
plot.tots(output, type = "l", ylab = "Total population", xlab = "Year",
          ylim = c(0, 1000), lwd = 3,
          cex = 1.25, cex.lab = 1.25, cex.axis = 1.25)
# all months, ages, sex, disease cat
# if years.only == TRUE then only plot one point per year, otherwise plot every month
plot.all(output, years.only = F)

# prevalence plot over time.
plot.prev(output, type = "l", col = "red", xlab = "year", ylab = "prevalence")

#plot the fawn to adult ratio
plot.fawn.adult(output, type = "l", xlab = "year", ylab = "fawn:adult")
plot.buck.doe(output, type = "l", xlab = "year", ylab = "buck:doe")

#matplot(months, t(St.f))
It.f<-(I1t.f[,] +  I2t.f[,] + I3t.f[,] + I4t.f[,] + I5t.f[,] + I6t.f[,] + I7t.f[,] + I8t.f[,] + I9t.f[,] + I10t.f[,] + I1t.m[,] +  I2t.m[,] + I3t.m[,] + I4t.m[,] + I5t.m[,] + I6t.m[,] + I7t.m[,] + I8t.m[,] + I9t.m[,] + I10t.m[,])

N.m<-(I1t.f[,] +  I2t.f[,] + I3t.f[,] + I4t.f[,] + I5t.f[,] + I6t.f[,] + I7t.f[,] + I8t.f[,] + I9t.f[,] + I10t.f[,] + I1t.m[,] +  I2t.m[,] + I3t.m[,] + I4t.m[,] + I5t.m[,] + I6t.m[,] + I7t.m[,] + I8t.m[,] + I9t.m[,] + I10t.m[,] + St.f[,] + St.m[,])

#matplot(months, t(It.f))
#matplot(months, t(N.m))
round(St.f[,1:14])

# matplot(months, t(St.f))
# matplot(months, t(It.f))
# matplot(months, t(It.m))
# matplot(months, t(St.m))
#
#
# plot(months, (apply(It,2,sum)/apply(N.m,2,sum)), type="line")
#
# for(i in seq(1, n.age.cats)){
#   if(i==1){
#     plot(months, (It[i,]/N.m[i,]), type="line", col=as.factor(n.age.cats), ylim=c(0,0.2))
#   }
#   lines(months, (It[i,]/N.m[i,]), type="line", col=as.factor(n.age.cats))
# }
#
#
#
# plot(months, (It[1,]/N.m[1,]), type="line")
# plot(months, (apply(It[1,],2,sum)/apply(N.m[1,],2,sum)), type="line")
# round(St.f[,1:14])
# #  list(St.f = St.f, St.m = St.m, It.f = It.f, It.m = It.m)
# #})

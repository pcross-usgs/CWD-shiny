#Stochastic.2b monthly age and sex structured model that incorporates random draws from distibutions of natural survival, reproduction, and hunt mortality. Currently does not include a distribution on FOI.
stoch.pop.model.2 <- function(params){

  # write the list objects to the local environment
  for (v in 1:length(params)) assign(names(params)[v], params[[v]])

  #########CREATE INITIAL CONDITIONS##########
  months <- seq(1, n.years*12)
  hunt.mo <- rep(0, n.years*12) # months in where the hunt occurs
  hunt.mo[months %% 12 == 7] <- 1 # hunt.mo==1 on Nov

  #assume adults always twin and calculate the proportion of females reproducing; fixed
  fawns.fawn <- 2
  fawns.juv <- 2
  fawns.ad <- 2

  fawn.rep <- 0

  ini.f.prev <- c(ini.fawn.prev, ini.juv.prev, rep(ini.ad.f.prev, (n.age.cats-2))) # initial female prevalence
  ini.m.prev <- c(ini.fawn.prev, ini.juv.prev, rep(ini.ad.m.prev, (n.age.cats-2))) # initial male prevalence

  #monthly stochastic survival rates that will be used to initalize the Leslie matrix
  fawn.sur <- (rbeta(1, fawn.sur.alpha, fawn.sur.beta, ncp = 0))^(1/12)
  juv.sur <- (rbeta(1, juv.sur.alpha, juv.sur.beta, ncp = 0))^(1/12)
  ad.f.sur <- (rbeta(1, ad.f.sur.alpha, ad.f.sur.beta, ncp = 0))^(1/12)
  ad.m.sur <- (rbeta(1, ad.m.sur.alpha, ad.m.sur.beta, ncp = 0))^(1/12)

  #monthly stochastic reproductive rates that will be used to initalize the Leslie matrix - need to multiply by 2 in the Leslie matrix
  fawn.preg.draw <- ifelse(fawn.rep == 0, 0,
                           (rbeta(1, fawn.repro.alpha, fawn.repro.beta, ncp = 0))) #when the mean is 0, the beta distribution doesn't work...
  juv.preg.draw <- (rbeta(1, juv.repro.alpha, juv.repro.beta, ncp = 0))
  ad.preg.draw <- (rbeta(1, ad.repro.alpha, ad.repro.beta, ncp = 0))

  # Create the survival and birth vectors
  Sur.an.f <- c(juv.sur^12, rep(ad.f.sur^12, n.age.cats - 2))
  Sur.an.m <- c(juv.sur^12, rep(ad.m.sur^12, n.age.cats - 2))
  Bir <- c(fawn.preg.draw * fawns.fawn, juv.preg.draw * fawns.juv,
           rep(ad.preg.draw * fawns.ad, n.age.cats - 2)) # vector of birth rates

  Sur.f <- c(juv.sur, rep(ad.f.sur, n.age.cats - 2)) # vector of survival rates for 12 age classes
  Sur.m <- c(juv.sur, rep(ad.m.sur, n.age.cats - 2)) # vector of survival rates for 12 age classes

  # Construct the sex-age projection matrix
  M <- matrix(rep(0, n.age.cats*2 * n.age.cats*2), nrow = n.age.cats*2)
  # replace the -1 off-diagonal with the survival rates
  M[row(M) == (col(M) + 1)] <- c(Sur.an.f[1:(n.age.cats-1)], 0, Sur.an.m[1:(n.age.cats-1)])
  # insert the fecundity vector
  M[1, 1:n.age.cats] <- Bir * 0.5 * fawn.sur^11 # prebirth census
  M[n.age.cats +1, 1:n.age.cats] <- Bir * 0.5 * fawn.sur^11 # prebirth census
  M[n.age.cats, n.age.cats] <- ad.f.sur^12 # adult female survival in top age cat
  M[n.age.cats*2, n.age.cats*2] <- ad.m.sur^12 # adult female survival in top age cat
  lambda(M)

  # pre-allocate the output matrices (and put I into a list)
  tmp <- matrix(0, nrow = n.age.cats, ncol = n.years*12)
  St.f <- tmp
  St.m <- tmp
  It.m <- list()
  It.f <- list()

  for(i in 1:10){
    It.m[[i]] <- tmp
    It.f[[i]] <- tmp
  }

  #No stochasticity built into starting prevalence...
  # Intializing with the stabe age distribution
  St.f[,1] <- round(stable.stage(M)[1:n.age.cats] * n0 * (1-ini.f.prev))
  St.m[,1] <- round(stable.stage(M)[(n.age.cats+1):(n.age.cats*2)] * n0 *
                      (1-ini.m.prev))

  # equally allocating prevalence across ages.
  It.m[[1]][,1] <- round(stable.stage(M)[1:n.age.cats] * n0 *  ini.m.prev)
  It.f[[1]][,1] <- round(stable.stage(M)[(n.age.cats+1):(n.age.cats*2)] * n0 *
                      ini.f.prev)

  #######POPULATION MODEL############
  for(t in 2:(n.years*12)){

    # on birthdays add in recruits and age everyone by one year
    # also on birthdays do the random parameter draws
    if(t %% 12 == 2){  # births happen in June, model starts in May

      #Parameter Draws
      fawn.sur<- (rbeta(1, fawn.sur.alpha, fawn.sur.beta, ncp = 0))^(1/12)
      juv.sur <- (rbeta(1, juv.sur.alpha, juv.sur.beta, ncp = 0))^(1/12)
      ad.f.sur <- (rbeta(1, ad.f.sur.alpha, ad.f.sur.beta, ncp = 0))^(1/12)
      ad.m.sur <- (rbeta(1, ad.m.sur.alpha, ad.m.sur.beta, ncp = 0))^(1/12)

      #pull annual reproductive rates from a beta distribution:
      fawn.preg.draw <- ifelse(fawn.rep == 0, 0,
                               (rbeta(1, fawn.repro.alpha, fawn.repro.beta, ncp = 0))) #when the mean is 0, the beta distribution doesn't work...
      juv.preg.draw <- (rbeta(1, juv.repro.alpha, juv.repro.beta, ncp = 0))
      ad.preg.draw <- (rbeta(1, ad.repro.alpha, ad.repro.beta, ncp = 0))

      # Create the survival and birth vectors
      Sur.f <- c(fawn.sur, juv.sur, rep(ad.f.sur, n.age.cats - 2))
      Sur.m <- c(fawn.sur, juv.sur, rep(ad.m.sur, n.age.cats - 2))

      #stochastic hunting survival rates; right now, it's drawing the hunting mort for each age class
      hunt.mort.f <- (rbeta(n.age.cats, hunt.mort.f.alpha, hunt.mort.f.beta, ncp = 0)) # added annual hunting mortality over the entire season for females
      hunt.mort.m <- (rbeta(n.age.cats, hunt.mort.m.alpha, hunt.mort.m.beta, ncp = 0))# added annual hunting mortality over the entire season for males
      hunt.mort.i.f <- (rbeta(n.age.cats, hunt.mort.i.f.alpha, hunt.mort.i.f.beta, ncp = 0))#hunting mortality associated with infected females - hot-spot removal
      hunt.mort.i.m <- (rbeta(n.age.cats, hunt.mort.i.m.alpha, hunt.mort.i.m.beta, ncp = 0))#hunting mortality associated with infected males - hot-spot removal

      # aging
      St.f[2:n.age.cats, t] <- St.f[1:n.age.cats-1, t-1]
      St.m[2:n.age.cats, t] <- St.m[1:n.age.cats-1, t-1]

      for(j in 1:10){
        It.f[[j]][2:n.age.cats, t] <- It.f[[j]][1:n.age.cats-1, t-1]
        It.m[[j]][2:n.age.cats, t] <- It.m[[j]][1:n.age.cats-1, t-1]
      }

      # reproduction
      I_fawn <- sapply(It.f, colSums)
      I_fawn <- Reduce("+", It.f)[1, t-1] # sums across all objects in the list, age = 1
      I_juv <- Reduce("+", It.f)[2, t-1]
      I_adults <- Reduce("+", It.f)[3:n.age.cats, t-1]

      fawns_born <- sum(rbinom(1, (St.f[1, t-1] + I_fawn), fawn.preg.draw)*fawns.fawn) +
                    sum(rbinom(1, (St.f[2, t-1] + I_juv), juv.preg.draw) * fawns.juv) +
                    rbinom(1, (sum(St.f[3:n.age.cats, t-1]) + sum(I_adults)), ad.preg.draw)*fawns.ad

      St.f[1, t] <- rbinom(1, fawns_born, 0.5)
      St.m[1, t] <- fawns_born - St.f[1, t]
    }

    if(t %% 12 != 2){
      #updating the next month
      St.f[, t] <- St.f[, t-1]
      St.m[, t] <- St.m[, t-1]

      for(j in 1:10){
        It.f[[j]][, t] <- It.f[[j]][, t-1]
        It.m[[j]][, t] <- It.m[[j]][, t-1]
      }
    }

    ##HUNT MORT then NATURAL MORT, THEN TRANSMISSION

    #stochastic movement of individuals from I1 to I2
    I.f.move <- list()
    I.m.move <- list()

    for(j in 1:10){
      I.f.move[[j]] <- rbinom(n.age.cats, It.f[[j]][,t], p)
      I.m.move[[j]] <- rbinom(n.age.cats, It.m[[j]][,t], p)
    }

    survive.s.f = rbinom(n.age.cats, (rbinom(n.age.cats, St.f[,t],
                                             (1 - hunt.mort.f * hunt.mo[t]))), Sur.f)
    survive.s.m = rbinom(n.age.cats, (rbinom(n.age.cats, St.m[,t],
                                             (1 - hunt.mort.m * hunt.mo[t]))), Sur.m)

    transmission.f = rbinom(n.age.cats, survive.s.f, foi) #number of individuals becoming infected
    transmission.m = rbinom(n.age.cats, survive.s.m, foi) #number of individuals becoming infected

    St.f[, t] <- survive.s.f - transmission.f
    St.m[, t] <- survive.s.m - transmission.m

    It.f[[1]][,t] <- transmission.f + rbinom(n.age.cats,
                                             (rbinom(n.age.cats, (It.f[[1]][,t] - I.f.move[[1]]),
                                                     (1 - hunt.mort.i.f * hunt.mo[t]))),
                                             Sur.f) #suceptibles that survive hunt and natural mortality and then become infected, plus I1 individuals that stay and survive hunt and natural mortality

    It.m[[1]][,t] <- transmission.m + rbinom(n.age.cats,
                                             (rbinom(n.age.cats, (It.m[[1]][,t] - I.m.move[[1]]),
                                                     (1 - hunt.mort.i.m * hunt.mo[t]))),
                                             Sur.f) #suceptibles that survive hunt and natural mortality and then become infected, plus I1 individuals that stay and survive hunt and natural mortality

    for(k in 2:10){
      It.f[[k]][, t] <- rbinom(n.age.cats, (rbinom(n.age.cats,
                                             (It.f[[k]][,t] - I.f.move[[k]] + I.f.move[[k-1]]),
                                             (1 - hunt.mort.i.f * hunt.mo[t]))), Sur.f)
      It.m[[k]][, t] <- rbinom(n.age.cats, (rbinom(n.age.cats,
                                                   (It.m[[k]][,t] - I.m.move[[k]] + I.m.move[[k-1]]),
                                                   (1 - hunt.mort.i.m * hunt.mo[t]))), Sur.m)
      }
  }

  # Reorganize the output so that it is not lists of lists, but a list of arrays
  for(i in 1:10){
    assign(paste0("I", i, "t.f"), It.f[[i]])
    assign(paste0("I", i, "t.m"), It.m[[i]])
    }

  output <- list(St.f = St.f, St.m = St.m,
                 I1t.f = I1t.f, I1t.m = I1t.m,
                 I2t.f = I1t.f, I2t.m = I1t.m,
                 I3t.f = I1t.f, I3t.m = I1t.m,
                 I4t.f = I1t.f, I4t.m = I1t.m,
                 I5t.f = I1t.f, I5t.m = I1t.m,
                 I6t.f = I1t.f, I6t.m = I1t.m,
                 I7t.f = I1t.f, I7t.m = I1t.m,
                 I8t.f = I1t.f, I8.m = I1t.m,
                 I9t.f = I1t.f, I9t.m = I1t.m,
                 I10t.f = I1t.f, I10t.m = I1t.m)
}

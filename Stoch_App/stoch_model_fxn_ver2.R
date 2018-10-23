#Stochastic.2b monthly age and sex structured model that incorporates
#random draws from distibutions of natural survival, reproduction, and hunt mortality.
#Currently does not include a distribution on FOI.

stoch.pop.model.2 <- function(params){
  require(popbio)
  source("./estBetaParams.r")
  #browser()
  # write the list objects to the local environment
  for (v in 1:length(params)) assign(names(params)[v], params[[v]])

  #########CREATE INITIAL CONDITIONS##########
  months <- seq(1, n.years*12)
  hunt.mo <- rep(0, n.years*12) # months in where the hunt occurs
  hunt.mo[months %% 12 == 7] <- 1 # hunt.mo==1 on Nov

  #Estimate alpha & beta values for survival
  fawn.s.b <- estBetaParams(fawn.an.sur, fawn.an.sur.var)
  juv.s.b <- estBetaParams(juv.an.sur, an.sur.var)
  ad.f.s.b <- estBetaParams(ad.an.f.sur, an.sur.var)
  ad.m.s.b <- estBetaParams(ad.an.m.sur, an.sur.var)

  #Estimate alpha & beta values for the beta distribution of probability of reproducing
  fawn.r.b <-estBetaParams(fawn.repro/2, fawn.repro.var)
  juv.r.b <- estBetaParams(juv.repro/2, juv.repro.var)
  ad.r.b <- estBetaParams(ad.repro/2, ad.repro.var)

  #Estimate alpha and beta of beta distribution
  hunt.fawn.b <- estBetaParams(hunt.mort.fawn, hunt.mort.var)
  hunt.juv.b <- estBetaParams(hunt.mort.juv, hunt.mort.var)
  hunt.f.b <- estBetaParams(hunt.mort.ad.f, hunt.mort.var)
  hunt.m.b <- estBetaParams(hunt.mort.ad.m, hunt.mort.var)

  fawn.rep <- 0
  # group into a vector
  ini.f.prev <- c(ini.fawn.prev, ini.juv.prev, rep(ini.ad.f.prev, (n.age.cats-2))) # initial female prevalence
  ini.m.prev <- c(ini.fawn.prev, ini.juv.prev, rep(ini.ad.m.prev, (n.age.cats-2))) # initial male prevalence

  # Create the Leslie Matrix to start the population at stable age dist
  M <- matrix(rep(0, n.age.cats*2 * n.age.cats*2), nrow = n.age.cats*2)
  # replace the -1 off-diagonal with the survival rates
  M[row(M) == (col(M) + 1)] <- c(juv.an.sur*(1-hunt.mort.juv),
                                 rep(ad.an.f.sur*(1-hunt.mort.ad.f), n.age.cats - 2),
                                 0, #spacer
                               c(juv.an.sur*(1-hunt.mort.juv),
                                 rep(ad.an.m.sur*(1-hunt.mort.ad.m), n.age.cats - 2)))
  # if you want the top age category to continue to survive
  #  M[n.age.cats, n.age.cats] <- ad.an.f.sur # adult female survival in top age cat
  #  M[n.age.cats*2, n.age.cats*2] <- ad.an.m.sur # adult female survival in top age cat

  # insert the fecundity vector
  # prebirth census
  M[1, 1:n.age.cats] <- c(fawn.repro, juv.repro,
                          rep(ad.repro, n.age.cats -2)) * 0.5 * fawn.an.sur * (1-hunt.mort.fawn)
  M[n.age.cats +1, 1:n.age.cats] <- M[1, 1:n.age.cats]
  #  lambda(M)

  # pre-allocate the output matrices (and put I into a list)
  tmp <- matrix(0, nrow = n.age.cats, ncol = n.years*12)
  St.f <- tmp
  St.m <- tmp
  It.m <- rep(list(tmp),10)
  It.f <- rep(list(tmp),10)

  # Intializing with the stable age distribution.
  St.f[,1] <- round(stable.stage(M)[1:n.age.cats] * n0 * (1-ini.f.prev))
  St.m[,1] <- round(stable.stage(M)[(n.age.cats+1):(n.age.cats*2)] * n0 *
                      (1-ini.m.prev))

  # equally allocating prevalence across ages.
  It.m[[1]][,1] <- round(stable.stage(M)[1:n.age.cats] * n0 * ini.m.prev)
  It.f[[1]][,1] <- round(stable.stage(M)[(n.age.cats+1):(n.age.cats*2)] * n0 *
                      ini.f.prev)

  #######POPULATION MODEL############
  for(t in 2:(n.years*12)){

    #Annual Parameter Draws
    #monthly stochastic survival rates
    fawn.sur.draw <- rbeta(1, fawn.s.b$alpha, fawn.s.b$beta, ncp = 0)^(1/12)
    juv.sur.draw <- rbeta(1, juv.s.b$alpha, juv.s.b$beta, ncp = 0)^(1/12)
    ad.f.sur.draw <- rbeta(1, ad.f.s.b$alpha, ad.f.s.b$beta, ncp = 0)^(1/12)
    ad.m.sur.draw <- rbeta(1, ad.m.s.b$alpha, ad.m.s.b$beta, ncp = 0)^(1/12)

    #monthly stochastic reproductive rates
    fawn.preg.draw <- ifelse(fawn.repro == 0, 0,
                             (rbeta(1, fawn.r.b$alpha, fawn.r.b$beta, ncp = 0))) #when the mean is 0, the beta distribution doesn't work...
    juv.preg.draw <- rbeta(1, juv.r.b$alpha, juv.r.b$beta, ncp = 0)
    ad.preg.draw <- rbeta(1, ad.r.b$alpha, ad.r.b$beta, ncp = 0)

    # group into a vector
    Sur.f <- c(juv.sur.draw, rep(ad.f.sur.draw, n.age.cats - 2))
    Sur.m <- c(juv.sur.draw, rep(ad.m.sur.draw, n.age.cats - 2))

    #stochastic hunting survival rates; right now, it's drawing the hunting mort for each age class
    hunt.fawn.draw <- rbeta(n.age.cats, hunt.fawn.b$alpha, hunt.fawn.b$beta, ncp = 0)
    hunt.juv.draw <- rbeta(n.age.cats, hunt.juv.b$alpha, hunt.juv.b$beta, ncp = 0)
    hunt.f.draw <- rbeta(n.age.cats, hunt.f.b$alpha, hunt.f.b$beta, ncp = 0)
    hunt.m.draw <- rbeta(n.age.cats, hunt.m.b$alpha, hunt.m.b$beta, ncp = 0)

    # on birthdays add in recruits and age everyone by one year
    # also on birthdays do the random parameter draws
    if(t %% 12 == 2){  # births happen in June, model starts in May

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

      fawns_born <- sum(rbinom(1, (St.f[1, t-1] + I_fawn), fawn.preg.draw) * 2) +
                    sum(rbinom(1, (St.f[2, t-1] + I_juv), juv.preg.draw) * 2) +
                    rbinom(1, (sum(St.f[3:n.age.cats, t-1]) + sum(I_adults)), ad.preg.draw)*2

      St.f[1, t] <- rbinom(1, fawns_born, 0.5)
      St.m[1, t] <- rbinom(1, fawns_born, 0.5)
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

    # disease induced mortality here
    for(j in 1:10){
      I.f.move[[j]] <- rbinom(n.age.cats, It.f[[j]][,t], p)
      I.m.move[[j]] <- rbinom(n.age.cats, It.m[[j]][,t], p)
    }

    survive.s.f = rbinom(n.age.cats, rbinom(n.age.cats, St.f[,t],
                                             (1 - c(hunt.fawn.draw,
                                                    hunt.juv.draw,
                                                    rep(hunt.f.draw, n.age.cats - 2))
                                             * hunt.mo[t])), Sur.f)

    survive.s.m = rbinom(n.age.cats, rbinom(n.age.cats, St.m[,t],
                                            (1 - c(hunt.fawn.draw,
                                                   hunt.juv.draw,
                                                   rep(hunt.m.draw, n.age.cats - 2))
                                            * hunt.mo[t])), Sur.m)

    transmission.f = rbinom(n.age.cats, survive.s.f, foi) #number of individuals becoming infected
    transmission.m = rbinom(n.age.cats, survive.s.m, foi) #number of individuals becoming infected

    St.f[, t] <- survive.s.f - transmission.f
    St.m[, t] <- survive.s.m - transmission.m

    # suceptibles that survive hunt and natural mortality and then become infected, plus I1 individuals that stay and survive hunt and natural mortality
    # not currently including the additional hunting mortality on infected individuals

    It.f[[1]][,t] <- transmission.f +
                      rbinom(n.age.cats,
                        rbinom(n.age.cats,
                          (It.f[[1]][,t] - I.f.move[[1]]),
                          (1 - c(hunt.fawn.draw, hunt.juv.draw,
                                 rep(hunt.f.draw, n.age.cats - 2)) * hunt.mo[t])),
                        Sur.f)

    It.m[[1]][,t] <- transmission.m +
                       rbinom(n.age.cats,
                              rbinom(n.age.cats,
                                     (It.m[[1]][,t] - I.m.move[[1]]),
                                     (1 - c(hunt.fawn.draw, hunt.juv.draw,
                                            rep(hunt.m.draw, n.age.cats - 2)) * hunt.mo[t])),
                              Sur.m)

    for(k in 2:10){
      It.f[[k]][, t] <- rbinom(n.age.cats,
                               rbinom(n.age.cats,
                                      (It.f[[k]][,t] - I.f.move[[k]] + I.f.move[[k-1]]),
                                      (1 - c(hunt.fawn.draw, hunt.juv.draw,
                                             rep(hunt.f.draw, n.age.cats - 2))) * hunt.mo[t]),
                               Sur.f)

      It.m[[k]][, t] <- rbinom(n.age.cats,
                               rbinom(n.age.cats,
                                      (It.m[[k]][,t] - I.m.move[[k]] + I.m.move[[k-1]]),
                                      (1 - c(hunt.fawn.draw, hunt.juv.draw,
                                             rep(hunt.m.draw, n.age.cats - 2))) * hunt.mo[t]),
                               Sur.m)

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

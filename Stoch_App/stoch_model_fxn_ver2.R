#Stochastic.2b monthly age and sex structured model that incorporates random draws from distibutions of natural survival, reproduction, and hunt mortality.
#Currently does not include a distribution on FOI.
stoch.pop.model.2 <- function(params){
  require(popbio)
  source("./estBetaParams.r", local = T)
  source("./allocateDeaths.r", local = T)
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
  #fawn.r.b <- estBetaParams(fawn.repro/2, fawn.repro.var)
  juv.r.b <- estBetaParams(juv.repro/2, juv.repro.var)
  ad.r.b <- estBetaParams(ad.repro/2, ad.repro.var)

  #Estimate alpha and beta of beta distribution
  hunt.fawn.b <- estBetaParams(hunt.mort.fawn, hunt.mort.var)
  hunt.juv.b <- estBetaParams(hunt.mort.juv, hunt.mort.var)
  hunt.f.b <- estBetaParams(hunt.mort.ad.f, hunt.mort.var)
  hunt.m.b <- estBetaParams(hunt.mort.ad.m, hunt.mort.var)

  #fawn.rep <- 0
  # group into a vector
  ini.f.prev <- c(ini.fawn.prev, ini.juv.prev, rep(ini.ad.f.prev, (n.age.cats-2))) # initial female prevalence
  ini.m.prev <- c(ini.fawn.prev, ini.juv.prev, rep(ini.ad.m.prev, (n.age.cats-2))) # initial male prevalence

  # Create the Leslie Matrix to start the population at stable age dist
  M <- matrix(rep(0, n.age.cats*2 * n.age.cats*2), nrow = n.age.cats*2)

  # replace the -1 off-diagonal with the survival rates
  M[row(M) == (col(M) + 1)] <- c(juv.an.sur*(1- hunt.mort.juv),
                                 rep(ad.an.f.sur*(1-hunt.mort.ad.f), n.age.cats - 2),
                                 0, #spacer
                               c(juv.an.sur*(1- hunt.mort.juv),
                                 rep(ad.an.m.sur*(1-hunt.mort.ad.m), n.age.cats - 2)))
  # if you want the top age category to continue to survive
    M[n.age.cats, n.age.cats] <- ad.an.f.sur*(1 - hunt.mort.ad.f)# adult female survival in top age cat
    M[n.age.cats*2, n.age.cats*2] <- ad.an.m.sur*(1- hunt.mort.ad.m) # adult male survival in top age cat

  # insert the fecundity vector
  # prebirth census
  M[1, 1:n.age.cats] <- c(0, juv.repro, rep(ad.repro, n.age.cats -2)) *
                          0.5 * fawn.an.sur * (1 - hunt.mort.fawn)
  M[n.age.cats +1, 1:n.age.cats] <- M[1, 1:n.age.cats]
  #  lambda(M)

  # pre-allocate the output matrices (and put I into a list)
  tmp <- matrix(0, nrow = n.age.cats, ncol = n.years*12)
  St.f <- tmp
  St.m <- tmp
  It.m <- array(rep(tmp), dim = c(n.age.cats, n.years*12, 10))
  It.f <- array(rep(tmp), dim = c(n.age.cats, n.years*12, 10))

  # Intializing with the stable age distribution.
  St.f[,1] <- round(stable.stage(M)[1:n.age.cats] * n0 * (1-ini.f.prev))
  St.m[,1] <- round(stable.stage(M)[(n.age.cats+1):(n.age.cats*2)] * n0 *
                      (1-ini.m.prev))

  # randomly allocating infecteds across ages and categories.
  It.m[ , 1, 1:10] <- rbinom(n.age.cats*10, round(stable.stage(M)[1:n.age.cats] * n0/10),  ini.m.prev)
  It.f[ , 1, 1:10] <- rbinom(n.age.cats*10, round(stable.stage(M)[1:n.age.cats] * n0/10),  ini.f.prev)

  #######POPULATION MODEL############
  for(t in 2:(n.years*12)){

    #Annual Parameter Draws
    #monthly stochastic survival rates
    fawn.sur.draw <- rbeta(1, fawn.s.b$alpha, fawn.s.b$beta, ncp = 0)^(1/12)
    juv.sur.draw <- rbeta(1, juv.s.b$alpha, juv.s.b$beta, ncp = 0)^(1/12)
    ad.f.sur.draw <- rbeta(1, ad.f.s.b$alpha, ad.f.s.b$beta, ncp = 0)^(1/12)
    ad.m.sur.draw <- rbeta(1, ad.m.s.b$alpha, ad.m.s.b$beta, ncp = 0)^(1/12)

    #monthly stochastic reproductive rates
    #fawn.preg.draw <- ifelse(fawn.repro == 0, 0,
    #                         (rbeta(1, fawn.r.b$alpha, fawn.r.b$beta, ncp = 0))) #when the mean is 0, the beta distribution doesn't work...
    juv.preg.draw <- rbeta(1, juv.r.b$alpha, juv.r.b$beta, ncp = 0)
    ad.preg.draw <- rbeta(1, ad.r.b$alpha, ad.r.b$beta, ncp = 0)

    # group into a vector
    Sur.f <- c(fawn.sur.draw, juv.sur.draw, rep(ad.f.sur.draw, n.age.cats - 2))
    Sur.m <- c(fawn.sur.draw, juv.sur.draw, rep(ad.m.sur.draw, n.age.cats - 2))

    #stochastic hunting survival rates
    hunt.fawn.draw <- rbeta(1, hunt.fawn.b$alpha, hunt.fawn.b$beta, ncp = 0)
    hunt.juv.draw <- rbeta(1, hunt.juv.b$alpha, hunt.juv.b$beta, ncp = 0)
    hunt.f.draw <- rbeta(n.age.cats-2, hunt.f.b$alpha, hunt.f.b$beta, ncp = 0)
    hunt.m.draw <- rbeta(n.age.cats-2, hunt.m.b$alpha, hunt.m.b$beta, ncp = 0)

    # on birthdays add in recruits and age everyone by one year
    # also on birthdays do the random parameter draws

     if(t %% 12 == 2){  # births happen in June, model starts in May

      # aging
      # the last age category remains in place and doesn't die
      St.f[2:(n.age.cats-1), t] <- St.f[1:(n.age.cats-2), t-1]
      St.f[n.age.cats, t] <- St.f[n.age.cats, t-1] + St.f[(n.age.cats-1), t-1]


      St.m[2:(n.age.cats-1), t] <- St.m[1:(n.age.cats-2), t-1]
      St.m[n.age.cats, t] <- St.m[n.age.cats, t-1] + St.m[(n.age.cats-1), t-1]

      It.f[2:(n.age.cats-1), t, ] = It.f[1:(n.age.cats-2), t-1, ]
      It.f[n.age.cats, t, ] <- It.f[n.age.cats, t-1, ] + It.f[(n.age.cats-1), t-1, ]

      It.m[2:(n.age.cats-1), t, ] = It.m[1:(n.age.cats-2), t-1, ]
      It.m[n.age.cats, t, ] <- It.m[n.age.cats, t-1, ] + It.m[(n.age.cats-1), t-1, ]

      # reproduction
      I_juv    <- sum(It.f[2, t, ])
      I_adults <-  sum(It.f[3:n.age.cats, t, ])

      fawns_born <- rbinom(1, (St.f[2, t] + I_juv), juv.preg.draw) * 2 +
                    rbinom(1, (sum(St.f[3:n.age.cats, t]) + sum(I_adults)), ad.preg.draw) * 2

      St.f[1, t] <- rbinom(1, fawns_born, 0.5)
      St.m[1, t] <- fawns_born - St.f[1, t]
    }

    if(t %% 12 != 2){
      #updating the next month
      St.f[, t] <- St.f[, t-1]
      St.m[, t] <- St.m[, t-1]
      It.f[, t, ] <- It.f[, t-1, ]
      It.m[, t, ] <- It.m[, t-1, ]
    }

    if(length(which(It.f[,t,] < 0)) > 0)browser()
    if(length(which(It.m[,t,] < 0)) > 0)browser()

    ##Disease MORT then HUNT MORT then NATURAL MORT, THEN TRANSMISSION
    #stochastic movement of individuals from I1 to I2
    # disease induced mortality here by advancing all I's
    # and only a proportion of the 10th category remains
    I.f.move <- matrix(rep(0, n.age.cats*10), nrow = 12, ncol = 10)
    I.m.move <- matrix(rep(0, n.age.cats*10), nrow = 12, ncol = 10)

    for(i in 1:10){
      I.f.move[ ,i] <- rbinom(n.age.cats, It.f[ , t, i], p)
      I.m.move[ ,i] <- rbinom(n.age.cats, It.m[ , t, i], p)
    }

    It.f[ , t, 1]    <- It.f[ ,t, 1] - I.f.move[ ,1]
    It.f[ , t, 2:10] <- It.f[ , t, 2:10] - I.f.move[ , 2:10] + I.f.move[, 1:9]

    It.m[ , t, 1]   <- It.m[ ,t, 1] - I.m.move[ ,1]
    It.m[ , t, 2:10] <- It.m[ , t, 2:10] - I.m.move[ , 2:10] + I.m.move[, 1:9]

    # hunting mortality
    Iall.f <- rowSums(It.f[ ,t,])
    Iall.m <- rowSums(It.m[ ,t,])
    Nt.f <- St.f[, t] + Iall.f
    Nt.m <- St.m[, t] + Iall.m

    # binomial draw on the total hunted
    hunted.f <- rbinom(n.age.cats, Nt.f, c(hunt.fawn.draw, hunt.juv.draw,
                                         hunt.f.draw) * hunt.mo[t])

    hunted.m <- rbinom(n.age.cats, Nt.m, c(hunt.fawn.draw, hunt.juv.draw,
                                           hunt.m.draw) * hunt.mo[t])

    # those hunted in the I class overall
    # can result in a divide by 0 and NA.
    # this can also result in more hunting of a category than are available.
    hunted.i.f <- round((rel.risk * Iall.f * hunted.f) / (St.f[,t] + rel.risk * Iall.f))
    hunted.i.m <- round((rel.risk * Iall.m * hunted.m) / (St.m[,t] + rel.risk * Iall.m))
    hunted.i.f[which(is.na(hunted.i.f))] <- 0
    hunted.i.m[which(is.na(hunted.i.m))] <- 0

    # those hunted in the S class
    hunted.s.f <- hunted.f - hunted.i.f
    hunted.s.m <- hunted.m - hunted.i.m

    # natural mort
    survive.s.f <- rbinom(n.age.cats, St.f[,t] - hunted.s.f, Sur.f)
    survive.s.m <- rbinom(n.age.cats, St.m[,t] - hunted.s.m, Sur.m)

    hunted.i.f[Iall.f < hunted.i.f] <- Iall.f[Iall.f < hunted.i.f]
    hunted.i.m[Iall.m < hunted.i.m] <- Iall.m[Iall.m < hunted.i.m]

    survive.i.f <- rbinom(n.age.cats, Iall.f - hunted.i.f, Sur.f)
    survive.i.m <- rbinom(n.age.cats, Iall.m - hunted.i.m, Sur.m)

    deaths.i.f <- Iall.f - survive.i.f
    deaths.i.m <- Iall.m - survive.i.m

    if(length(which(is.na(deaths.i.f))) > 0) browser()
    if(length(which(is.na(deaths.i.m))) > 0) browser()

    # allocate those deaths across the Icategories
    It.f[ , t, ] <- allocate.deaths(deaths.i.f, It.f[ , t, ])
    It.m[ , t, ] <- allocate.deaths(deaths.i.m, It.m[ , t, ])

    #infection
    transmission.f <- rbinom(n.age.cats, survive.s.f, foi)
    transmission.m <- rbinom(n.age.cats, survive.s.m, foi)

    St.f[, t] <- survive.s.f - transmission.f
    St.m[, t] <- survive.s.m - transmission.m

    # update with the new infections
    It.f[, t, 1] <- transmission.f + It.f[ ,t, 1]
    It.m[, t, 1] <- transmission.m + It.m[ ,t, 1]

    if(length(which(It.f[,t,] < 0)) > 0)browser()
    if(length(which(It.m[,t,] < 0)) > 0)browser()

  }

  output <- list(St.f = St.f, St.m = St.m,
                 I1t.f = It.f[,,1], I1t.m = It.m[,,1],
                 I2t.f = It.f[,,2], I2t.m = It.m[,,2],
                 I3t.f = It.f[,,3], I3t.m = It.m[,,3],
                 I4t.f = It.f[,,4], I4t.m = It.m[,,4],
                 I5t.f = It.f[,,5], I5t.m = It.m[,,5],
                 I6t.f = It.f[,,6], I6t.m = It.m[,,6],
                 I7t.f = It.f[,,7], I7t.m = It.m[,,7],
                 I8t.f = It.f[,,8], I8t.m = It.m[,,8],
                 I9t.f = It.f[,,9], I9t.m = It.m[,,9],
                 I10t.f = It.f[,,10], I10t.m = It.m[,,10])
}

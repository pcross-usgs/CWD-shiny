#Stochastic monthly age and sex structured model that incorporates random draws 
# from distibutions of natural survival, reproduction, and hunt mortality.
#Currently does not include a distribution on transmission rate.
stoch.pop.model.2 <- function(params){
  require(popbio)
  source("./estBetaParams.r", local = T)
  source("./allocateDeaths.r", local = T)
   
  # write the list objects to the local environment
  for (v in 1:length(params)) assign(names(params)[v], params[[v]])

  #########CREATE INITIAL CONDITIONS##########
  months <- seq(1, n.years*12)  # monthly timestep
  hunt.mo <- rep(0, n.years*12) # months in where the hunt occurs
  hunt.mo[months %% 12 == 7] <- 1 # hunt.mo==1 on Nov

  #Estimate shape and scale parameters for the Beta distribution given the 
  # user input of mean and variance. 
  # natural survival
  fawn.s.b <- estBetaParams(fawn.an.sur, fawn.an.sur.var)
  juv.s.b <- estBetaParams(juv.an.sur, an.sur.var)
  ad.f.s.b <- estBetaParams(ad.an.f.sur, an.sur.var)
  ad.m.s.b <- estBetaParams(ad.an.m.sur, an.sur.var)

  #reproduction
  juv.r.b <- estBetaParams(juv.repro/2, juv.repro.var)
  ad.r.b <- estBetaParams(ad.repro/2, ad.repro.var)

  #hunting
  hunt.fawn.b <- estBetaParams(hunt.mort.fawn, hunt.mort.var)
  hunt.juv.f.b <- estBetaParams(hunt.mort.juv.f, hunt.mort.var)
  hunt.juv.m.b <- estBetaParams(hunt.mort.juv.m, hunt.mort.var)
  hunt.f.b <- estBetaParams(hunt.mort.ad.f, hunt.mort.var)
  hunt.m.b <- estBetaParams(hunt.mort.ad.m, hunt.mort.var)

 # group into a vector
  ini.f.prev <- c(ini.fawn.prev, ini.juv.prev, 
                  rep(ini.ad.f.prev, (n.age.cats-2))) # initial female prevalence
  ini.m.prev <- c(ini.fawn.prev, ini.juv.prev, 
                  rep(ini.ad.m.prev, (n.age.cats-2))) # initial male prevalence

  # Create the Leslie Matrix to start the population at stable age dist
  M <- matrix(rep(0, n.age.cats*2 * n.age.cats*2), nrow = n.age.cats*2)

  # replace the -1 off-diagonal with the survival rates
  M[row(M) == (col(M) + 1)] <- c(juv.an.sur*(1- hunt.mort.juv.f),
                                 rep(ad.an.f.sur*(1-hunt.mort.ad.f), n.age.cats - 2),
                                 0, #spacer
                                 c(juv.an.sur*(1- hunt.mort.juv.m),
                                   rep(ad.an.m.sur*(1-hunt.mort.ad.m), n.age.cats - 2)))
  
  # if you want the top age category to continue to survive
  M[n.age.cats, n.age.cats] <- ad.an.f.sur*(1 - hunt.mort.ad.f)# adult female survival in top age cat
  M[n.age.cats*2, n.age.cats*2] <- ad.an.m.sur*(1- hunt.mort.ad.m) # adult male survival in top age cat

  # insert the fecundity vector
  # prebirth census
  M[1, 1:n.age.cats] <- c(0, juv.repro, rep(ad.repro, n.age.cats -2)) *
    0.5 * fawn.an.sur * (1 - hunt.mort.fawn)
  M[n.age.cats +1, 1:n.age.cats] <- M[1, 1:n.age.cats]

  # pre-allocate the output matrices
  tmp <- matrix(0, nrow = n.age.cats, ncol = n.years*12)
  St.f <- tmp # susceptible female vector
  St.m <- tmp # suceptible male vector
  It.m <- array(rep(tmp), dim = c(n.age.cats, n.years*12, 10)) # infectious females
  It.f <- array(rep(tmp), dim = c(n.age.cats, n.years*12, 10)) # infectious males

  # tracking the # hunted
  Ht.f <- tmp
  Ht.m <- tmp
  # natural deaths
  Dt.f <- tmp
  Dt.m <- tmp
  # disease deaths
  CWDt.f <- tmp
  CWDt.m <- tmp

  # Intializing with the stable age distribution.
  St.f[,1] <- round(stable.stage(M)[1:n.age.cats] * n0 * (1-ini.f.prev))
  St.m[,1] <- round(stable.stage(M)[(n.age.cats+1):(n.age.cats*2)] * n0 *
                      (1-ini.m.prev))

  # randomly allocating infecteds across ages and categories.
  It.m[ , 1, 1:10] <- rbinom(n.age.cats*10, 
                             round(stable.stage(M)[1:n.age.cats] * n0/10),  
                             ini.m.prev)
  It.f[ , 1, 1:10] <- rbinom(n.age.cats*10, 
                             round(stable.stage(M)[1:n.age.cats] * n0/10),  
                             ini.f.prev)
  rm(M)
  
  #######POPULATION MODEL############
  for(t in 2:(n.years*12)){

    #Annual Parameter Draws
    #monthly stochastic survival rates
    fawn.sur.draw <- rbeta(1, fawn.s.b$alpha, fawn.s.b$beta, ncp = 0)^(1/12)
    juv.sur.draw <- rbeta(1, juv.s.b$alpha, juv.s.b$beta, ncp = 0)^(1/12)
    ad.f.sur.draw <- rbeta(1, ad.f.s.b$alpha, ad.f.s.b$beta, ncp = 0)^(1/12)
    ad.m.sur.draw <- rbeta(1, ad.m.s.b$alpha, ad.m.s.b$beta, ncp = 0)^(1/12)

    #monthly stochastic reproductive rates
    juv.preg.draw <- rbeta(1, juv.r.b$alpha, juv.r.b$beta, ncp = 0)
    ad.preg.draw <- rbeta(1, ad.r.b$alpha, ad.r.b$beta, ncp = 0)

    # group into a vector
    Sur.f <- c(fawn.sur.draw, juv.sur.draw, rep(ad.f.sur.draw, n.age.cats - 2))
    Sur.m <- c(fawn.sur.draw, juv.sur.draw, rep(ad.m.sur.draw, n.age.cats - 2))

    #stochastic hunting survival rates
    hunt.fawn.draw <- rbeta(1, hunt.fawn.b$alpha, hunt.fawn.b$beta, ncp = 0)
    hunt.juv.f.draw <- rbeta(1, hunt.juv.f.b$alpha, hunt.juv.f.b$beta, ncp = 0)
    hunt.juv.m.draw <- rbeta(1, hunt.juv.m.b$alpha, hunt.juv.m.b$beta, ncp = 0)
    hunt.f.draw <- rbeta(n.age.cats-2, hunt.f.b$alpha, hunt.f.b$beta, ncp = 0)
    hunt.m.draw <- rbeta(n.age.cats-2, hunt.m.b$alpha, hunt.m.b$beta, ncp = 0)

    # on birthdays add in recruits and age everyone by one year
    # also on birthdays do the random parameter draws
    if(t %% 12 == 2){  # births happen in June, model starts in May

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
      I_juv    <- sum(It.f[2, t-1, ])
      I_adults <-  sum(It.f[3:n.age.cats, t-1, ])

      fawns_born <- rbinom(1, (St.f[2, t-1] + I_juv), juv.preg.draw) * 2 +
        rbinom(1, (sum(St.f[3:n.age.cats, t-1]) + I_adults), ad.preg.draw) * 2

      St.f[1, t] <- rbinom(1, fawns_born, 0.5)
      St.m[1, t] <- fawns_born - St.f[1, t]
    }
    
    # if not June populate the current month with last months values
    if(t %% 12 != 2){
      #updating the next month
      St.f[, t] <- St.f[, t-1]
      St.m[, t] <- St.m[, t-1]
      It.f[, t, ] <- It.f[, t-1, ]
      It.m[, t, ] <- It.m[, t-1, ]
    }

    ##Natural Mort then hunt then disease mort Then transmission
    #Natural Mortality
    #susceptibles
    nat.s.f <- rbinom(n.age.cats, St.f[ ,t], (1-Sur.f))
    nat.s.m <- rbinom(n.age.cats, St.m[ ,t], (1-Sur.m))

    St.f[ ,t] <- St.f[,t] - nat.s.f
    St.m[ ,t] <- St.m[,t] - nat.s.m
    # infecteds
    nat.i.f <- matrix(rbinom(length(It.f[ ,t, ]), size = It.f[ ,t, ], 
                                prob = (1-Sur.f)), nrow = 12)
    nat.i.m <- matrix(rbinom(length(It.m[ ,t, ]), size = It.m[ ,t, ], 
                                prob = (1-Sur.m)), nrow = 12)
    
    It.f[ , t, ] <- It.f[ ,t, ] - nat.i.f
    It.m[ , t, ] <- It.m[ ,t, ] - nat.i.m

    Dt.f[, t] <- nat.s.f + rowSums(nat.i.f)
    Dt.m[, t] <- nat.s.m + rowSums(nat.i.m)

    # Hunt mortality
    if(hunt.mo[t]==1){
      Iall.f <- rowSums(It.f[ ,t,]) # total # infected females
      Iall.m <- rowSums(It.m[ ,t,]) # total # infected males
      Nt.f <- St.f[, t] + Iall.f # total population of females
      Nt.m <- St.m[, t] + Iall.m # total population of males

      # binomial draw on the total hunted
      Ht.f[,t] <- rbinom(n.age.cats, Nt.f, c(hunt.fawn.draw, hunt.juv.f.draw,
                                             hunt.f.draw))

      Ht.m[,t] <- rbinom(n.age.cats, Nt.m, c(hunt.fawn.draw, hunt.juv.m.draw,
                                             hunt.m.draw))

      # those hunted in the I class overall based on the total hunted, the total
      # that are susceptible/infected and the relative hunting risk of S v. I
      # can result in a divide by 0 and NA.
      # this can also result in more hunting of a category than are available.
      hunted.i.f <- round((rel.risk * Iall.f * Ht.f[,t]) /
                            (St.f[,t] + rel.risk * Iall.f))
      hunted.i.m <- round((rel.risk * Iall.m * Ht.m[,t]) /
                            (St.m[,t] + rel.risk * Iall.m))
      
      hunted.i.f[which(is.na(hunted.i.f))] <- 0
      hunted.i.m[which(is.na(hunted.i.m))] <- 0

      hunted.i.f[Iall.f < hunted.i.f] <- Iall.f[Iall.f < hunted.i.f]
      hunted.i.m[Iall.m < hunted.i.m] <- Iall.m[Iall.m < hunted.i.m]
      
      # subtracting out those hunted in the S class
      St.f[,t] <- St.f[,t] - (Ht.f[,t] - hunted.i.f)
      St.m[,t] <- St.m[,t] - (Ht.m[,t] - hunted.i.m)
      
      # allocate those deaths across the 10 I categories
      It.f[ , t, ] <- allocate.deaths(hunted.i.f, It.f[ , t, ])
      It.m[ , t, ] <- allocate.deaths(hunted.i.m, It.m[ , t, ])
    }

    #Disease mortality
    #stochastic movement of individuals from I1 to I2
    # disease induced mortality here by advancing all I's
    # and only a proportion of the 10th category remains
    I.f.move <- matrix(rbinom(n.age.cats*10, size = It.f[ , t, ], 
                             prob = p), nrow = 12)
    I.m.move <- matrix(rbinom(n.age.cats*10, size = It.m[ , t, ], 
                                     prob = p), nrow = 12)
    
    #store info on those that die directly from disease
    CWDt.f[ ,t] <- I.f.move[ ,10]
    CWDt.m[ ,t] <- I.m.move[ ,10]
    
    # move the I individuals forward in their categories
    It.f[ , t, 1]    <- It.f[ ,t, 1] - I.f.move[ ,1]
    It.f[ , t, 2:10] <- It.f[ , t, 2:10] - I.f.move[ , 2:10] + I.f.move[, 1:9]

    It.m[ , t, 1]   <- It.m[ ,t, 1] - I.m.move[ ,1]
    It.m[ , t, 2:10] <- It.m[ , t, 2:10] - I.m.move[ , 2:10] + I.m.move[, 1:9]

    # Direct transmission considering all I's are equal
    Iall <- sum(It.f[ ,t, ] + It.m[ ,t,])
    Nall <- sum(St.f[,t] + St.m[,t]) + Iall

    foi <- 1 - exp(-beta.f * Iall/Nall^theta)
    foi.m <- 1 - exp(-beta.f * beta.m * Iall/Nall^theta)

    transmission.f <- rbinom(n.age.cats, St.f[,t], foi)
    transmission.m <- rbinom(n.age.cats, St.m[,t], foi.m)

    St.f[, t] <- St.f[ ,t] - transmission.f
    St.m[, t] <- St.m[ ,t] - transmission.m

    # update with the new infections
    It.f[, t, 1] <- transmission.f + It.f[ ,t, 1]
    It.m[, t, 1] <- transmission.m + It.m[ ,t, 1]

    #Environmental transmission happens last
    envcases.f <- rbinom(n.age.cats, St.f[ ,t], env.foi)
    envcases.m <- rbinom(n.age.cats, St.m[ ,t], env.foi)

    St.f[ ,t] <- St.f[ ,t] - envcases.f
    St.m[ ,t] <- St.m[ ,t] - envcases.m

    It.f[ ,t, 1] <-  It.f[ ,t, 1] + envcases.f
    It.m[ ,t, 1] <-  It.m[ ,t, 1] + envcases.m
  }
  # group the output
  counts <- list(St.f = St.f, St.m = St.m,
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
  deaths <- list(Ht.f = Ht.f, Ht.m = Ht.m, Dt.f = Dt.f, Dt.m = Dt.m,
                 CWDt.f = CWDt.f, CWDt.m = CWDt.m)
  
  # convert the output to long form
  counts.long <- melt(counts) %>%
    rename(age = Var1, month = Var2, population = value,
           category = L1) %>%
    mutate(year = (month - 1) / 12, sex = as.factor(str_sub(category, -1)),
           disease = "no")
  counts.long$disease[str_sub(counts.long$category, 1,1) == "I"] = "yes"
  counts.long$disease <- as.factor(counts.long$disease)

  deaths.long <- melt(deaths) %>%
    rename(age = Var1, month = Var2, population = value, category = L1) %>%
    mutate(year = (month - 1) / 12,
           sex = as.factor(str_sub(category, -1)))

  output <- list(counts = counts.long, deaths = deaths.long)

}

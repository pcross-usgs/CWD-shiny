det.pop.model <- function(params){
  require(popbio)
  # write the list objects to the local environment
  for (v in 1:length(params)) assign(names(params)[v], params[[v]])
  #browser()

  #monthly index
  months <- seq(1, n.years*12)
  hunt.mo <- rep(0, n.years*12) # months in where the hunt occurs
  hunt.mo[months %% 12 == 7] <- 1 # hunt.mo==1 on Nov

  #Natural monthly survival rates
  fawn.sur <- fawn.an.sur^(1/12)
  juv.sur <- juv.an.sur^(1/12)
  ad.f.sur <- ad.an.f.sur^(1/12)
  ad.m.sur <- ad.an.m.sur^(1/12)

  #########CREATE INITIAL CONDITIONS##########
  # group into a vector
  ini.f.prev <- c(ini.fawn.prev, ini.juv.prev, rep(ini.ad.f.prev, (n.age.cats-2))) # initial female prevalence
  ini.m.prev <- c(ini.fawn.prev, ini.juv.prev, rep(ini.ad.m.prev, (n.age.cats-2))) # initial male prevalence

  # Create the survival and birth vectors
  Sur.f <- c(fawn.sur, juv.sur, rep(ad.f.sur, n.age.cats - 2)) # vector of survival rates for 12 age classes
  Sur.m <- c(fawn.sur, juv.sur, rep(ad.m.sur, n.age.cats - 2)) # vector of survival rates for 12 age classes

  # Create the Leslie Matrix to start the population at stable age dist
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

  # equally allocating prevalence across ages.
  It.m[[1]][,1] <- round(stable.stage(M)[1:n.age.cats] * n0 * ini.m.prev)
  It.f[[1]][,1] <- round(stable.stage(M)[(n.age.cats+1):(n.age.cats*2)] * n0 *
                           ini.f.prev)

  #######POPULATION MODEL############
  for(t in 2:(n.years*12)){

    # on birthdays add in recruits and age everyone by one year
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

      St.f[1, t] <- ((St.f[2, t-1] + I_juv) * juv.repro +
                       sum(St.f[3:n.age.cats, t-1] + I_adults) * ad.repro) * 0.5


      St.m[1, t] <- ((St.f[2, t-1] + I_juv) * juv.repro +
                       sum(St.f[3:n.age.cats, t-1] + I_adults) * ad.repro) * 0.5
    }

    if(t %% 12 != 2){
      #updating the next month
      St.f[, t] <- St.f[, t-1]
      St.m[, t] <- St.m[, t-1]

      It.f[, t, ] <- It.f[, t-1, ]
      It.m[, t, ] <- It.m[, t-1, ]
    }

    I.f.move <- list()
    I.m.move <- list()

    # disease induced mortality here
    for(i in 1:10){
      I.f.move[ ,i] <- It.f[ , t, i]* p
      I.m.move[ ,i] <- It.m[ , t, i]* p
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
    hunted.f <- Nt.f * c(hunt.mort.fawn, hunt.mort.juv,
                         rep(hunt.mort.ad.f, n.age.cats - 2)) * hunt.mo[t]
    hunted.m <- Nt.m * c(hunt.mort.fawn, hunt.mort.juv,
                     rep(hunt.mort.ad.m, n.age.cats - 2)) * hunt.mo[t]

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
    survive.s.f <- (St.f[,t] - hunted.s.f) * Sur.f
    survive.s.m <- (St.m[,t] - hunted.s.m) * Sur.m

    hunted.i.f[Iall.f < hunted.i.f] <- Iall.f[Iall.f < hunted.i.f]
    hunted.i.m[Iall.m < hunted.i.m] <- Iall.m[Iall.m < hunted.i.m]

    survive.i.f <- (Iall.f - hunted.i.f) * Sur.f
    survive.i.m <- (Iall.m - hunted.i.m) * Sur.m

    deaths.i.f <- Iall.f - survive.i.f
    deaths.i.m <- Iall.m - survive.i.m

    browser()
    # converting I from list to array. Also need to figure out how to allocate
    # deaths across the I array in the deterministic model.

    transmission.f = survive.s.f * foi #number of individuals becoming infected
    transmission.m = survive.s.m * foi #number of individuals becoming infected

    St.f[, t] <- survive.s.f - transmission.f
    St.m[, t] <- survive.s.m - transmission.m

    #suceptibles that survive hunt and natural mortality and then become infected,
    #plus I1 individuals that stay and survive hunt and natural mortality
    It.f[[1]][,t] <- (transmission.f + It.f[[1]][,t] - I.f.move[[1]]) *
                    (1 - c(hunt.mort.fawn, hunt.mort.juv,
                           rep(hunt.mort.ad.f, n.age.cats - 2)) * hunt.mo[t]) * Sur.f
    It.m[[1]][,t] <- (transmission.m + It.m[[1]][,t] - I.m.move[[1]]) *
      (1 - c(hunt.mort.fawn, hunt.mort.juv,
             rep(hunt.mort.ad.m, n.age.cats - 2)) * hunt.mo[t]) * Sur.m

    for(k in 2:10){
      It.f[[k]][,t] <- (It.f[[k]][,t] - I.f.move[[k]] + I.f.move[[k-1]]) *
                        (1 - c(hunt.mort.fawn, hunt.mort.juv,
                               rep(hunt.mort.ad.f, n.age.cats - 2)) * hunt.mo[t]) * Sur.f

      It.m[[k]][,t] <- (It.m[[k]][,t] - I.m.move[[k]] + I.m.move[[k-1]]) *
        (1 - c(hunt.mort.fawn, hunt.mort.juv,
                rep(hunt.mort.ad.m, n.age.cats - 2)) * hunt.mo[t]) * Sur.m
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

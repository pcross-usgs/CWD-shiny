det.pop.model <- function(params){
  require(popbio)
  # write the list objects to the local environment
  for (v in 1:length(params)) {
    assign(names(params)[v], params[[v]])
  }
  browser()

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
#    Sur.an.f <- c(fawn.an.sur, juv.an.sur, rep(ad.an.f.sur, n.age.cats - 2)) # vector of survival rates for 12 age classes
#    Sur.an.m <- c(fawn.an.sur, juv.an.sur, rep(ad.an.m.sur, n.age.cats - 2)) # vector of survival rates for 12 age classes    Bir <- c(fawn.repro, juv.repro, rep(ad.repro, n.age.cats - 2)) # vector of birth rates
  Sur.f <- c(fawn.sur, juv.sur, rep(ad.f.sur, n.age.cats - 2)) # vector of survival rates for 12 age classes
  Sur.m <- c(fawn.sur, juv.sur, rep(ad.m.sur, n.age.cats - 2)) # vector of survival rates for 12 age classes

  # Create the Leslie Matrix to start the population at stable age dist
  M <- matrix(rep(0, n.age.cats*2 * n.age.cats*2), nrow = n.age.cats*2)
  # replace the -1 off-diagonal with the survival rates
  M[row(M) == (col(M) + 1)] <- c(juv.an.sur-hunt.mort.juv,
                                 rep(ad.an.f.sur-hunt.mort.ad.f, n.age.cats - 2),
                                 0, #spacer
                                 c(juv.an.sur,
                                   rep(ad.an.m.sur-hunt.mort.ad.m, n.age.cats - 2)))
  # if you want the top age category to continue to survive
  #  M[n.age.cats, n.age.cats] <- ad.an.f.sur # adult female survival in top age cat
  #  M[n.age.cats*2, n.age.cats*2] <- ad.an.m.sur # adult female survival in top age cat

  # insert the fecundity vector
  # prebirth census
  M[1, 1:n.age.cats] <- c(fawn.repro, juv.repro,
                          rep(ad.repro, n.age.cats -2) * 0.5 * fawn.an.sur)
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

    # on birthdays add in recruits and age everyone by one year
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

      St.f[1, t] <- ((St.f[1, t-1] + I_fawn)* fawn.repro +
                       (St.f[2, t-1] + I_juv) * juv.repro +
                       sum((St.f[3:n.age.cats, t-1] + I_adults)) * ad.repro) * 0.5


      St.m[1, t] <- ((St.f[1, t-1] + I_fawn)* fawn.repro +
                       (St.f[2, t-1] + I_juv) * juv.repro +
                       sum((St.f[3:n.age.cats, t-1] + I_adults)) * ad.repro) * 0.5
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

    I.f.move <- list()
    I.m.move <- list()

    # disease induced mortality here
    for(j in 1:10){
      I.f.move[[j]] <- It.f[[j]][,t] * p
      I.m.move[[j]] <- It.m[[j]][,t] * p
    }

    ##HUNT MORT then NATURAL MORT, THEN TRANSMISSION
    survive.s.f = St.f[,t] * (1 - c(hunt.mort.fawn, hunt.mort.juv,
                                    rep(hunt.mort.ad.f, n.age.cats - 2)) * hunt.mo[t]) * Sur.f

    survive.s.m = St.m[,t] * (1 - c(hunt.mort.fawn, hunt.mort.juv,
                                    rep(hunt.mort.ad.m, n.age.cats - 2)) * hunt.mo[t]) * Sur.m

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

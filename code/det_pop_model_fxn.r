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
#browser()

  # pre-allocate the output matrices
  tmp <- matrix(0, nrow = n.age.cats, ncol = n.years*12)
  St.f <- tmp
  St.m <- tmp
  It.m <- array(rep(tmp), dim = c(n.age.cats, n.years*12, 10))
  It.f <- array(rep(tmp), dim = c(n.age.cats, n.years*12, 10))

  # Intializing with the stable age distribution.
  St.f[,1] <- stable.stage(M)[1:n.age.cats] * n0 * (1-ini.f.prev)
  St.m[,1] <- stable.stage(M)[(n.age.cats+1):(n.age.cats*2)] * n0 *
                      (1-ini.m.prev)

  # equally allocating prevalence across ages.
  It.m[ , 1, 1:10] <- stable.stage(M)[1:n.age.cats] * n0/10 * ini.m.prev
  It.f[ , 1, 1:10] <- stable.stage(M)[(n.age.cats+1):(n.age.cats*2)] * n0/10 *
                           ini.f.prev

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

   # Natural mortality
   St.f[, t] <- St.f[, t] * Sur.f
   St.m[, t] <- St.m[, t] * Sur.m

   It.f[ , t, ] <- It.f[ , t, ] * Sur.f
   It.m[ , t, ] <- It.m[ , t, ] * Sur.m

   # Hunt mortality
   if(hunt.mo[t]==1){
    # browser()

     Iall.f <- rowSums(It.f[ ,t,])
     Iall.m <- rowSums(It.m[ ,t,])
     Nt.f <- St.f[, t] + Iall.f
     Nt.m <- St.m[, t] + Iall.m

     # total hunted
     hunted.f <- Nt.f * c(hunt.mort.fawn, hunt.mort.juv,
                          rep(hunt.mort.ad.f, n.age.cats - 2))
     hunted.m <- Nt.m * c(hunt.mort.fawn, hunt.mort.juv,
                          rep(hunt.mort.ad.m, n.age.cats - 2))

     # those hunted in the I class overall
     # can result in a divide by 0 and NA.
     hunted.i.f <- (rel.risk * Iall.f * hunted.f) / (St.f[,t] + rel.risk * Iall.f)
     hunted.i.m <- (rel.risk * Iall.m * hunted.m) / (St.m[,t] + rel.risk * Iall.m)
     hunted.i.f[which(is.na(hunted.i.f))] <- 0
     hunted.i.m[which(is.na(hunted.i.m))] <- 0

       # those hunted in the S class
     St.f[ ,t] <- St.f[,t] - hunted.f - hunted.i.f
     St.m[ ,t] <- St.m[,t] - hunted.m - hunted.i.m

     It.f[ , t, ] <- It.f[ , t, ] * (1- hunted.i.f/Iall.f)
     It.m[ , t, ] <- It.m[ , t, ] * (1- hunted.i.m/Iall.m)
    }


   # disease induced mortality here
    It.f[ , t, 1]    <- It.f[ ,t, 1] * (1-p)
    It.f[ , t, 2:10] <- It.f[ , t, 2:10] *(1-p) + It.f[ , t, 1:9] * p

    It.m[ , t, 1]    <- It.m[ ,t, 1] * (1-p)
    It.m[ , t, 2:10] <- It.m[ , t, 2:10] *(1-p) + It.m[ , t, 1:9] * p

    #browser()
    #Disease transmission here
    St.f[,t] <- St.f[ ,t]*(1-foi)
    St.m[,t] <- St.m[ ,t]*(1-foi)

    It.f[ ,t, 1] <-  It.f[ ,t, 1] + St.f[ ,t]*foi
    It.m[ ,t, 1] <-  It.m[ ,t, 1] + St.m[ ,t]*foi
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

stoch.pop.model <- function(params){

  # write the list objects to the local environment
  for (v in 1:length(params)) assign(names(params)[v], params[[v]])

  #monthly index
  months <- seq(1, n.years*12)
  hunt.mo <- rep(0, n.years*12) # months in where the hunt occurs
  hunt.mo[months %% 12 == 7] <- 1 # hunt.mo==1 on Nov

  #Natural monthly survival rates
  fawn.sur <- fawn.an.sur^(1/12)
  juv.sur <- juv.an.sur^(1/12)
  ad.f.sur <- ad.an.f.sur^(1/12)
  ad.m.sur <- ad.an.m.sur^(1/12)

  # pregnancy parameters
  fawns.fawn <- ifelse(fawn.rep < 1, 1, 2)
  fawns.juv <- ifelse(juv.rep < 1, 1, 2)
  fawns.ad <- ifelse(ad.rep < 1, 1, 2)

  preg.fawn <- ifelse(fawns.fawn==1, fawn.rep, fawn.rep/2)
  preg.juv <- ifelse(fawns.juv==1, juv.rep, juv.rep/2)
  preg.ad <- ifelse(fawns.ad==1, ad.rep, ad.rep/2)


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



  # Intializing with the stabe age distribution
  St.f[,1] <- round(stable.stage(M)[1:n.age.cats] * n0 * (1-ini.prev))
  St.m[,1] <- round(stable.stage(M)[(n.age.cats+1):(n.age.cats*2)] * n0 *
                      (1-ini.prev))

  # equally allocating prevalence across ages.
  I1t.m[,1] <- round(stable.stage(M)[1:n.age.cats] * n0 *  ini.prev)
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
      I_fawn <- (I1t.f[1, t-1] + I2t.f[1, t-1] + I3t.f[1, t-1] + I4t.f[1, t-1] + I5t.f[1, t-1] + I6t.f[1, t-1] + I7t.f[1, t-1] + I8t.f[1, t-1] + I9t.f[1, t-1] + I10t.f[1, t-1])

      I_juv <- (I1t.f[2, t-1] + I2t.f[2, t-1] + I3t.f[2, t-1] + I4t.f[2, t-1] + I5t.f[2, t-1] + I6t.f[2, t-1] + I7t.f[2, t-1] + I8t.f[2, t-1] + I9t.f[2, t-1] + I10t.f[2, t-1])

      I_adults <- (I1t.f[3:n.age.cats, t-1] + I2t.f[3:n.age.cats, t-1] + I3t.f[3:n.age.cats, t-1] + I4t.f[3:n.age.cats, t-1] + I5t.f[3:n.age.cats, t-1] + I6t.f[3:n.age.cats, t-1] + I7t.f[3:n.age.cats, t-1] + I8t.f[3:n.age.cats, t-1] + I9t.f[3:n.age.cats, t-1] + I10t.f[3:n.age.cats, t-1])


      fawns_born <- sum(rbinom(n.age.cats, (St.f[1, t-1] + I_fawn), preg.fawn)*fawns.fawn) + sum(rbinom(n.age.cats, (St.f[2, t-1] + I_juv), preg.juv)*fawns.juv) + rbinom(1, (sum(St.f[3:n.age.cats, t-1]) + sum(I_adults)), preg.ad)*fawns.ad

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

    survive.s.f = rbinom(n.age.cats, (rbinom(n.age.cats, St.f[,t], (1 - hunt.mort.f * hunt.mo[t]))), Sur.f)
    survive.s.m = rbinom(n.age.cats, (rbinom(n.age.cats, St.m[,t], (1 - hunt.mort.m * hunt.mo[t]))), Sur.m)

    transmission.f = rbinom(n.age.cats, survive.s.f, foi) #number of individuals becoming infected
    transmission.m = rbinom(n.age.cats, survive.s.m, foi) #number of individuals becoming infected

    St.f[, t] <- survive.s.f - transmission.f


    I1t.f[, t] <- transmission.f + rbinom(n.age.cats, (rbinom(n.age.cats, (I1t.f[,t]-I1.f.move), (1 - hunt.mort.i.f * hunt.mo[t]))), Sur.f) #suceptibles that survive hunt and natural mortality and then become infected, plus I1 individuals that stay and survive hunt and natural mortality

    I2t.f[, t] <- rbinom(n.age.cats, (rbinom(n.age.cats, (I2t.f[,t] - I2.f.move + I1.f.move), (1 - hunt.mort.i.f * hunt.mo[t]))), Sur.f)

    I3t.f[, t] <- rbinom(n.age.cats, (rbinom(n.age.cats, (I3t.f[,t] - I3.f.move + I2.f.move), (1 - hunt.mort.i.f * hunt.mo[t]))), Sur.f)

    I4t.f[, t] <- rbinom(n.age.cats, (rbinom(n.age.cats, (I4t.f[,t] - I4.f.move + I3.f.move), (1 - hunt.mort.i.f * hunt.mo[t]))), Sur.f)

    I5t.f[, t] <- rbinom(n.age.cats, (rbinom(n.age.cats, (I5t.f[,t] - I5.f.move + I4.f.move), (1 - hunt.mort.i.f * hunt.mo[t]))), Sur.f)

    I6t.f[, t] <- rbinom(n.age.cats, (rbinom(n.age.cats, (I6t.f[,t] - I6.f.move + I5.f.move), (1 - hunt.mort.i.f * hunt.mo[t]))), Sur.f)

    I7t.f[, t] <- rbinom(n.age.cats, (rbinom(n.age.cats, (I7t.f[,t] - I7.f.move + I6.f.move), (1 - hunt.mort.i.f * hunt.mo[t]))), Sur.f)

    I8t.f[, t] <- rbinom(n.age.cats, (rbinom(n.age.cats, (I8t.f[,t] - I8.f.move + I7.f.move), (1 - hunt.mort.i.f * hunt.mo[t]))), Sur.f)

    I9t.f[, t] <- rbinom(n.age.cats, (rbinom(n.age.cats, (I9t.f[,t] - I9.f.move + I8.f.move), (1 - hunt.mort.i.f * hunt.mo[t]))), Sur.f)

    I10t.f[, t] <- rbinom(n.age.cats, (rbinom(n.age.cats, (I10t.f[,t] - I10.f.move + I9.f.move), (1 - hunt.mort.i.f * hunt.mo[t]))), Sur.f)

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

    I10t.m[, t] <- rbinom(n.age.cats, (rbinom(n.age.cats, (I10t.m[,t] - I10.m.move + I9.m.move), (1 - hunt.mort.i.m * hunt.mo[t]))), Sur.m)

    if (sum(St.f[,t] + St.m[,t] + I1t.f[,t] +  I2t.f[,t] + I3t.f[,t] + I4t.f[,t] + I5t.f[,t] + I6t.f[,t] + I7t.f[,t] + I8t.f[,t] + I9t.f[,t] + I10t.f[,t] + I1t.m[,t] +  I2t.m[,t] + I3t.m[,t] + I4t.m[,t] + I5t.m[,t] + I6t.m[,t] + I7t.m[,t] + I8t.m[,t] + I9t.m[,t] + I10t.m[,t]) <= 0) break
  }

  output <- list(St.f = St.f, St.m = St.m,
                 I1t.f = I1t.f, I2t.f = I2t.f, I3t.f = I3t.f, I4t.f = I4t.f,
                 I5t.f = I5t.f, I6t.f = I6t.f, I7t.f = I7t.f, I8t.f = I8t.f,
                 I9t.f = I9t.f, I10t.f = I10t.f, I1t.m = I1t.m, I2t.m = I2t.m,
                 I3tm = I3t.m, I4t.m = I4t.m, I5t.m = I5t.m, I6t.m = I6t.m,
                 I7t.m = I7t.m, I8t.m = I8t.m, I9t.m = I9t.m, I10t.m = I10t.m)


}
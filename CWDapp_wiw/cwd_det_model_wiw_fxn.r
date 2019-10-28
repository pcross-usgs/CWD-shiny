#' CWD who-infects-who deterministic model 
#' 
#' Deterministic monthly age and sex structured model with constant
#'  environmental transmission and dynamic direct transmission. 2x2 matrix of 
#'  transmission rates between males and females. 
#'  
#' @param params A list with the following parameters included: 
#' 
#' fawn.an.sur = annual fawn survival (scaler value between 0 and 1),  
#' 
#' juv.an.sur = annual juvenile survival (scaler value between 0 and 1),  
#' 
#' ad.an.f.sur = annual adult female survival (scaler value between 0 and 1),  
#' 
#' ad.an.m.sur = annual adult male survival (scaler value between 0 and 1),   
#' 
#' fawn.repro = fawn reproduction (scaler value >= 0),  
#' 
#' juv.repro = juvenile reproduction (scaler value >= 0),  
#' 
#' ad.repro = adult reproduction (scaler value >= 0),  
#' 
#' hunt.mort.fawn = percentage of fawns hunted (scaler value between 0 and 1),  
#' 
#' hunt.mort.juv.f = percentage of juvenile females hunted (scaler value between 0 and 1),  
#' 
#' hunt.mort.juv.m = percentage of juvenile males hunted (scaler value between 0 and 1),  
#' 
#' hunt.mort.ad.f = percentage of adult females hunted (scaler value between 0 and 1),  
#' 
#' hunt.mort.ad.m = percentage of adult males hunted (scaler value between 0 and 1),  
#' 
#' ini.fawn.prev = percentage of fawns infected at the start (scaler value between 0 and 1),  
#' 
#' ini.juv.prev = percentage of juveniles infected at the start (scaler value between 0 and 1),  
#' 
#' ini.ad.f.prev = percentage of adult females infected at the start (scaler value between 0 and 1),  
#' 
#' ini.ad.m.prev = percentage of adult males infected at the start (scaler value between 0 and 1),   
#' 
#' n.age.cats = number of age categories to monitor. (scaler value greater than 3).
#' The final age category includes all those of that age or greater. 
#' 
#' p = rate of movement in the infectious categories (scaler values between 0 and 1). 
#' See model documentation vignette for how this relates to disease induced mortality.   
#' 
#' env.foi = % of the population that is infected by the environment per year (scaler value between 0 and 1) #' 
#' beta.ff = female to female transmission coefficient (scaler value greater than 0),  
#' 
#' gamma.mm = relative increase in the male-male transmission coefficient compared 
#' to female-female transmission (scaler value greater than 0. A value of 1 indicates equal transmission)  
#' 
#' gamma.mf = relative increase in the male-female transmission coefficient compared 
#' to female-female transmission (scaler value greater than 0. A value of 1 indicates equal transmission)  
#' 
#' gamma.fm = relative increase in the female-male transmission coefficient compared 
#' to female-female transmission (scaler value greater than 0. A value of 1 indicates equal transmission) 
#' 
#' theta = effect of population size on transmission (1 = frequency dependence, 0 = density dependent).  
#' 
#' n0 = initial population size (scaler value greater than 0)
#' 
#' n.years = number of years to run the model (scaler value greater than 2),  
#' 
#' rel.risk = relative risk of infected individuals being hunted. A value of 1 
#' indicates no hunter preference for infected individuals  
#' 
#' 
#' @return A list with 2 dataframes is returned as output: 
#' 
#' 1. counts of the # of individuals in the susceptible and infectious 
#' categories by over time. 
#' 
#'  Columns include: 
#' 
#'  age (in years)
#' 
#'  month of simulation,
#' 
#'  population = number of individuals
#' 
#'  category: St.f = susceptible females, St.m = susceptible males, Ixt.f = 
#'  infectious females in the x category (1-10), Ixt.m = infectious males in the 
#'  x infectious category (1-10) 
#' 
#'  sex = female or male
#' 
#' disease = yes or no for susceptible or infectious
#' 
#'   
#' 2. deaths--how individuals died over time (hunting, natural or disease).
#' 
#'  Columns include: 
#'  
#'  age in years, 
#'  
#'  month of the simulation, 
#'  
#'  population = # of individuals, 
#'  
#'  category: Ht.f = hunted females, Ht.m = hunted males, Dt.f = natural 
#'  mortality females, Dt.m = natural mortality males, CWDt.f = disease mortality 
#'  females, CWDt.m = disease mortality males.  
#'    
#'  year = year of the simulation
#'  
#'  sex
#'
#' @importFrom popbio stable.stage
#' @importFrom dplyr rename mutate
#' @importFrom reshape2 melt
#' @examples 
#' params <- list(fawn.an.sur = 0.6, juv.an.sur = 0.8, ad.an.f.sur = 0.95, 
#' ad.an.m.sur = 0.9, fawn.repro = 0, juv.repro = 0.6, ad.repro = 1, 
#' hunt.mort.fawn = 0.01, hunt.mort.juv.f = 0.1, hunt.mort.juv.m = 0.1,
#' hunt.mort.ad.f = 0.1, hunt.mort.ad.m = 0.2, ini.fawn.prev = 0.02,
#' ini.juv.prev = 0.03, ini.ad.f.prev = 0.04,  ini.ad.m.prev = 0.04,
#' n.age.cats = 12,  p = 0.43, env.foi = 0,  beta.ff = 0.06, 
#' gamma.mm = 2, gamma.mf = 2, gamma.fm = 1,
#' theta = 1, n0 = 2000, n.years = 10, rel.risk = 1.0)
#' 
#' out <- cwd_det_model_wiw(params)
#' 
#' plot_tots(out$counts)
#' 
#' @export

cwd_det_model_wiw <- function(params) {
  # write the list objects to the local environment
  for (v in 1:length(params)) assign(names(params)[v], params[[v]])

  # check if parameters exist.    
  if(exists("fawn.an.sur")==FALSE){
    message("fawn survival is missing, using default value")
    fawn.an.sur <- 0.6
  }
  
  if(exists("juv.an.sur")==FALSE){
    message("juvenile survival is missing, using default value")
    juv.an.sur <- 0.8
  }
  
  if(exists("ad.an.f.sur")==FALSE){
    message("adult female survival is missing, using default value")
    ad.an.f.sur <- 0.95
  }
  
  if(exists("ad.an.m.sur")==FALSE){
    message("adult male survival is missing, using default value")
    ad.an.m.sur <- 0.9
  }
  
  if(exists("fawn.repro")==FALSE){
    message("fawn repro is missing, using default value")
    fawn.repro <- 0
  }
  
  if(exists("juv.repro")==FALSE){
    message("juvenile repro is missing, using default value")
    juv.repro <- 0.6
  }
  
  if(exists("ad.repro")==FALSE){
    message("adult repro is missing, using default value")
    ad.repro <- 1
  }
  
  if(exists("hunt.mort.fawn")==FALSE){
    message("fawn hunting mortality is missing, using default value")
    hunt.mort.fawn <- 0.01
  }
  
  if(exists("hunt.mort.juv.f")==FALSE){
    message("juv. female  hunting mortality is missing, using default value")
    hunt.mort.juv.f <- 0.1
  }
  if(exists("hunt.mort.juv.m")==FALSE){
    message("juv. male hunting mortality is missing, using default value")
    hunt.mort.juv.m <- 0.1
  }
  
  if(exists("hunt.mort.ad.f")==FALSE){
    message("adult female hunting mortality is missing, using default value")
    hunt.mort.ad.f <- 0.2
  }
  if(exists("hunt.mort.ad.m")==FALSE){
    message("adult male hunting mortality is missing, using default value")
    hunt.mort.ad.m <- 0.2
  }
  if(exists("ini.fawn.prev")==FALSE){
    message("initial fawn prevalence is missing, using default value")
    ini.fawn.prev <- 0.01
  }
  if(exists("ini.juv.prev")==FALSE){
    message("initial juvenile prevalence is missing, using default value")
    ini.juv.prev <- 0.03
  }
  if(exists("ini.ad.f.prev")==FALSE){
    message("initial adult female prevalence is missing, using default value")
    ini.ad.f.prev <- 0.04
  }
  if(exists("ini.ad.m.prev")==FALSE){
    message("initial adult male prevalence is missing, using default value")
    ini.ad.m.prev <- 0.04
  }
  if(exists("n.age.cats")==FALSE){
    message("# of age categories is missing, using default value")
    n.age.cats <- 12
  }
  
  if(exists("p")==FALSE){
    message("disease mortality index p is missing, using default value")
    p <- 0.43
  }
  if(exists("env.foi")==FALSE){
    message("indirect transmission env.foi is missing, using default value")
    env.foi <- 0
  }
  
  if(exists("beta.ff")==FALSE){
    message("female transmission beta.f is missing, using default value")
    beta.f <- 0.05
  }
  
  if(exists("theta")==FALSE){
    message("theta is missing, using default value")
    theta <- 1
  }
  
  if(exists("n0")==FALSE){
    message("initial population size n0 is missing, using default value")
    n0 <- 1000
  }
  
  if(exists("n.years")==FALSE){
    message("n.years is missing, using default value")
    n.years <- 10
  }
  
  if(exists("rel.risk")==FALSE){
    message("rel.risk is missing, using default value")
    rel.risk <- 1
  }
  
  if(exists("gamma.mm")==FALSE){
    message("gamma.mm is missing, using default value")
    gamma.mm <- 2
  }
  if(exists("gamma.fm")==FALSE){
    message("gamma.fm is missing, using default value")
    gamma.fm <- 1
  }
  if(exists("gamma.mf")==FALSE){
    message("gamma.mf is missing, using default value")
    gamma.mf <- 1
  }
  
  ###### check parameter values ###
  if(fawn.an.sur <= 0) warning("fawn survival must be positive")
  if(fawn.an.sur > 1) warning("fawn survival must be <= 1")
  if(juv.an.sur <= 0) warning("juvenile survival must be positive")
  if(juv.an.sur > 1) warning("juvenile survival must be <= 1")
  if(ad.an.f.sur <= 0) warning("adult female survival must be positive")
  if(ad.an.f.sur > 1) warning("adult female survival must be <= 1")
  
  if(fawn.repro < 0) warning("fawn.repro must be positive")
  if(juv.repro <= 0) warning("juv.repro must be >= 0 ")
  if(ad.repro  <= 0) warning("ad.repro must be >= 0 ")
  
  if(hunt.mort.fawn < 0) warning("hunt.mort.fawn must be >=0")
  if(hunt.mort.fawn > 1) warning("hunt.mort.fawn must be < 1")
  if(hunt.mort.juv.f < 0) warning("hunt.mort.juv.f must be >=0")
  if(hunt.mort.juv.f > 1) warning("hunt.mort.juv.f must be < 1")
  if(hunt.mort.juv.m < 0) warning("hunt.mort.juv.m must be >=0")
  if(hunt.mort.juv.m > 1) warning("hunt.mort.juv.m must be < 1")
  if(hunt.mort.ad.f < 0) warning("hunt.mort.ad.f must be >=0")
  if(hunt.mort.ad.f > 1) warning("hunt.mort.ad.f must be < 1")
  if(hunt.mort.ad.m < 0) warning("hunt.mort.ad.m must be >=0")
  if(hunt.mort.ad.m > 1) warning("hunt.mort.ad.m must be < 1")
  
  if(ini.fawn.prev < 0) warning("ini.fawn.prev must >=0")
  if(ini.fawn.prev > 1) warning("ini.fawn.prev must be <= 1")
  if(ini.juv.prev < 0) warning("ini.juv.prev must >=0")
  if(ini.juv.prev > 1) warning("ini.juv.prev must be <= 1")
  if(ini.ad.f.prev < 0) warning("ini.ad.f.prev must >=0")
  if(ini.ad.f.prev > 1) warning("ini.ad.f.prev must be <= 1")
  if(ini.ad.m.prev < 0) warning("ini.ad.m.prev must >=0")
  if(ini.ad.m.prev > 1) warning("ini.ad.m.prev must be <= 1")
  
  if(n.age.cats < 3) warning("n.age.cats must be 3 or more")
  if(p < 0) warning("p must be between 0 and 1")
  if(p > 1) warning("p must be between 0 and 1")
  if(env.foi < 0) warning("env.foi must be between 0 and 1")
  if(env.foi > 1) warning("env.foi must be between 0 and 1")
  if(beta.ff < 0) warning("beta.ff cannot be negative")
  if(n0 <= 0) warning("n0 must be positive")
  if(n.years <= 0) warning("n.years must be positive")
  if(rel.risk <= 0) warning("n.years must be positive")
  
  if(gamma.mm <= 0) warning("repro.var must be positive")
  if(gamma.fm <= 0) warning("fawn.sur.var must be positive")
  if(gamma.mf <= 0) warning("sur.var must be positive")
  
  # the gender specific transmission terms are offsets from the female to female term
  beta.mm <- beta.ff*gamma.mm
  beta.mf <- beta.ff*gamma.mf
  beta.fm <- beta.ff*gamma.fm
  
  ######### CREATE INITIAL CONDITIONS########## monthly index
  months <- seq(1, n.years * 12)  # monthly timestep
  hunt.mo <- rep(0, n.years * 12)  # months in where the hunt occurs
  hunt.mo[months%%12 == 7] <- 1  # hunt.mo==1 on Nov

  # Natural monthly survival rates
  fawn.sur <- fawn.an.sur^(1/12)
  juv.sur <- juv.an.sur^(1/12)
  ad.f.sur <- ad.an.f.sur^(1/12)
  ad.m.sur <- ad.an.m.sur^(1/12)

  # group into a vector initial female prevalence
  ini.f.prev <- c(ini.fawn.prev, ini.juv.prev, rep(ini.ad.f.prev, (n.age.cats -
                                                                     2)))
  # initial male prevalence
  ini.m.prev <- c(ini.fawn.prev, ini.juv.prev, rep(ini.ad.m.prev, (n.age.cats -
                                                                     2)))

  # Create the survival and birth vectors vector of survival rates for 12
  # age classes
  Sur.f <- c(fawn.sur, juv.sur, rep(ad.f.sur, n.age.cats - 2))
  Sur.m <- c(fawn.sur, juv.sur, rep(ad.m.sur, n.age.cats - 2))

  # Create the Leslie Matrix to start the population at stable age dist
  M <- matrix(rep(0, n.age.cats * 2 * n.age.cats * 2), nrow = n.age.cats *
                2)
  # replace the -1 off-diagonal with the survival rates
  M[row(M) == (col(M) + 1)] <- c(juv.an.sur * (1 - hunt.mort.juv.f),
                                 rep(ad.an.f.sur * (1 - hunt.mort.ad.f),
                                     n.age.cats - 2), 0,
                                 c(juv.an.sur *
                                     (1 - hunt.mort.juv.m),
                                   rep(ad.an.m.sur * (1 - hunt.mort.ad.m),
                                       n.age.cats - 2)))
  # if you want the top age category to continue to survive adult female
  # survival in top age cat
  M[n.age.cats, n.age.cats] <- ad.an.f.sur * (1 - hunt.mort.ad.f)
  # adult male survival in top age cat
  M[n.age.cats * 2, n.age.cats * 2] <- ad.an.m.sur * (1 - hunt.mort.ad.m)

  # insert the fecundity vector for prebirth census
  M[1, 1:n.age.cats] <- c(0, juv.repro, rep(ad.repro, n.age.cats - 2)) *
    0.5 * fawn.an.sur * (1 - hunt.mort.fawn)
  M[n.age.cats + 1, 1:n.age.cats] <- M[1, 1:n.age.cats]

  # pre-allocate the output matrices
  tmp <- matrix(0, nrow = n.age.cats, ncol = n.years * 12)
  St.f <- tmp  # susceptible female vector
  St.m <- tmp  # suceptible male vector
  It.m <- array(rep(tmp), dim = c(n.age.cats, n.years * 12, 10))  # infectious females
  It.f <- array(rep(tmp), dim = c(n.age.cats, n.years * 12, 10))  # infectious males

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
  St.f[, 1] <- popbio::stable.stage(M)[1:n.age.cats] * n0 * (1 - ini.f.prev)
  St.m[, 1] <- popbio::stable.stage(M)[(n.age.cats + 1):(n.age.cats *
                                                           2)] * n0 * (1 - ini.m.prev)

  # equally allocating prevalence across ages.
  It.m[, 1, 1:10] <- popbio::stable.stage(M)[1:n.age.cats] * n0/10 *
    ini.m.prev
  It.f[, 1, 1:10] <- popbio::stable.stage(M)[(n.age.cats + 1):(n.age.cats *
                                                                 2)] * n0/10 * ini.f.prev

  ####### POPULATION MODEL############
  for (t in 2:(n.years * 12)) {

    # on birthdays add in recruits and age everyone by one year births
    # happen in June, model starts in May
    if (t%%12 == 2) {

      # aging the last age category remains in place and doesn't die
      St.f[2:(n.age.cats - 1), t] <- St.f[1:(n.age.cats - 2), t - 1]
      St.f[n.age.cats, t] <- St.f[n.age.cats, t - 1] + St.f[(n.age.cats - 1), t - 1]

      St.m[2:(n.age.cats - 1), t] <- St.m[1:(n.age.cats - 2), t - 1]
      St.m[n.age.cats, t] <- St.m[n.age.cats, t - 1] + St.m[(n.age.cats - 1), t - 1]

      It.f[2:(n.age.cats - 1), t, ] <- It.f[1:(n.age.cats - 2), t - 1, ]
      It.f[n.age.cats, t, ] <- It.f[n.age.cats, t - 1, ] +
                                  It.f[(n.age.cats - 1), t - 1, ]

      It.m[2:(n.age.cats - 1), t, ] <- It.m[1:(n.age.cats - 2), t - 1, ]
      It.m[n.age.cats, t, ] <- It.m[n.age.cats, t - 1, ] +
                                  It.m[(n.age.cats - 1), t - 1, ]

      # reproduction
      I_juv <- sum(It.f[2, t - 1, ])
      I_adults <- sum(It.f[3:n.age.cats, t - 1, ])

      St.f[1, t] <- ((St.f[2, t - 1] + I_juv) * juv.repro +
                       (sum(St.f[3:n.age.cats, t - 1]) + I_adults) * ad.repro) * 0.5


      St.m[1, t] <- ((St.f[2, t - 1] + I_juv) * juv.repro +
                       (sum(St.f[3:n.age.cats, t - 1]) + I_adults) * ad.repro) * 0.5
    }

    if (t%%12 != 2) {
      # updating the next month
      St.f[, t] <- St.f[, t - 1]
      St.m[, t] <- St.m[, t - 1]

      It.f[, t, ] <- It.f[, t - 1, ]
      It.m[, t, ] <- It.m[, t - 1, ]
    }

    # Natural mortality
    St.f[, t] <- St.f[, t] * Sur.f
    St.m[, t] <- St.m[, t] * Sur.m

    It.f[, t, ] <- It.f[, t, ] * Sur.f
    It.m[, t, ] <- It.m[, t, ] * Sur.m

    Dt.f[, t] <- (St.f[, t] + rowSums(It.f[, t, ])) * (1 - Sur.f)
    Dt.m[, t] <- (St.m[, t] + rowSums(It.m[, t, ])) * (1 - Sur.m)

    # Hunt mortality
    if (hunt.mo[t] == 1) {
      Iall.f <- rowSums(It.f[, t, ])
      Iall.m <- rowSums(It.m[, t, ])
      Nt.f <- St.f[, t] + Iall.f
      Nt.m <- St.m[, t] + Iall.m

      # total hunted
      hunted.f <- Nt.f * c(hunt.mort.fawn, hunt.mort.juv.f, rep(hunt.mort.ad.f,
                                                                n.age.cats - 2))
      hunted.m <- Nt.m * c(hunt.mort.fawn, hunt.mort.juv.m, rep(hunt.mort.ad.m,
                                                                n.age.cats - 2))

      # tracking the # hunted
      Ht.f[, t] <- hunted.f
      Ht.m[, t] <- hunted.m

      # those hunted in the I class overall based on the total hunted, the
      # total that are susceptible/infected and the relative hunting risk of
      # S v. I can result in a divide by 0 and NA.  this can also result in
      # more hunting of a category than are available.
      hunted.i.f <- (rel.risk * Iall.f * hunted.f)/(St.f[, t] + rel.risk *
                                                      Iall.f)
      hunted.i.m <- (rel.risk * Iall.m * hunted.m)/(St.m[, t] + rel.risk *
                                                      Iall.m)
      hunted.i.f[which(is.na(hunted.i.f))] <- 0
      hunted.i.m[which(is.na(hunted.i.m))] <- 0

      # those hunted in the S class
      St.f[, t] <- St.f[, t] - (hunted.f - hunted.i.f)
      St.m[, t] <- St.m[, t] - (hunted.m - hunted.i.m)

      It.f[, t, ] <- It.f[, t, ] * (1 - hunted.i.f/Iall.f)
      It.m[, t, ] <- It.m[, t, ] * (1 - hunted.i.m/Iall.m)
    }

    # Disease mortality stochastic movement of individuals from I1 to I2
    # disease induced mortality here by advancing all I's and only a
    # proportion of the 10th category remains
    f.move <- It.f[, t, ] * p
    m.move <- It.m[, t, ] * p

    It.f[, t, 1] <- It.f[, t, 1] - f.move[, 1]
    It.f[, t, 2:10] <- It.f[, t, 2:10] - f.move[, 2:10] + f.move[, 1:9]
    It.m[, t, 1] <- It.m[, t, 1] - m.move[, 1]
    It.m[, t, 2:10] <- It.m[, t, 2:10] - m.move[, 2:10] + m.move[, 1:9]

    # store info on those that die directly from disease
    CWDt.f[, t] <- f.move[, 10]
    CWDt.m[, t] <- m.move[, 10]

    # Direct transmission
    Iall <- sum(It.f[ ,t, ] + It.m[ ,t,])
    Nall <- sum(St.f[,t] + St.m[,t]) + Iall
    
    cases.f <- St.f[ ,t] * (1 - exp( - (beta.ff * sum(It.f[ ,t, ]) / Nall^theta + 
                                          beta.mf * sum(It.m[ ,t, ]) / Nall^theta)))
    
    cases.m <- St.m[ ,t] * (1 - exp( - (beta.mm * sum(It.m[ ,t, ]) / Nall^theta + 
                                          beta.fm * sum(It.f[ ,t, ]) / Nall^theta)))
    
    St.f[, t] <- St.f[, t] - cases.f
    St.m[, t] <- St.m[, t] - cases.m

    It.f[, t, 1] <- It.f[, t, 1] + cases.f
    It.m[, t, 1] <- It.m[, t, 1] + cases.m

    # Environmental transmission happens last
    envcases.f <- St.f[, t] * env.foi
    envcases.m <- St.m[, t] * env.foi

    St.f[, t] <- St.f[, t] - envcases.f
    St.m[, t] <- St.m[, t] - envcases.m

    It.f[, t, 1] <- It.f[, t, 1] + envcases.f
    It.m[, t, 1] <- It.m[, t, 1] + envcases.m
  }
  # group the output

  counts <- list(St.f = St.f, St.m = St.m, I1t.f = It.f[, , 1],
                 I1t.m = It.m[, , 1], I2t.f = It.f[, , 2],
                 I2t.m = It.m[, , 2], I3t.f = It.f[, , 3],
                 I3t.m = It.m[, , 3], I4t.f = It.f[, , 4],
                 I4t.m = It.m[, , 4], I5t.f = It.f[, , 5],
                 I5t.m = It.m[, , 5], I6t.f = It.f[, , 6],
                 I6t.m = It.m[, , 6], I7t.f = It.f[, , 7],
                 I7t.m = It.m[, , 7], I8t.f = It.f[, , 8],
                 I8t.m = It.m[, , 8], I9t.f = It.f[, , 9],
                 I9t.m = It.m[, , 9], I10t.f = It.f[, , 10],
                 I10t.m = It.m[, , 10])

  deaths <- list(Ht.f = Ht.f, Ht.m = Ht.m, Dt.f = Dt.f, Dt.m = Dt.m,
                 CWDt.f = CWDt.f, CWDt.m = CWDt.m)

  # convert the output to long form
  counts.long <- melt(counts) %>%
    rename(age = Var1, month = Var2, population = value, category = L1) %>%
    mutate(year = (month - 1)/12, sex = as.factor(str_sub(category, -1)),
           disease = "no")
  counts.long$disease[str_sub(counts.long$category, 1, 1) == "I"] <- "yes"
  counts.long$disease <- as.factor(counts.long$disease)

  deaths.long <- melt(deaths) %>%
    rename(age = Var1, month = Var2, population = value, category = L1) %>%
    mutate(year = (month - 1)/12, sex = as.factor(str_sub(category, -1)))

  output <- list(counts = counts.long, deaths = deaths.long)
}

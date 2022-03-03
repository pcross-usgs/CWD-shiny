#' CWD stochastic model function
#'
#' Stochastic monthly age and sex structured model with constant
#'  environmental transmission and dynamic direct transmission. The function 
#'  conducts one run of the model. 
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
#' env.foi = % of the population that is infected by the environment per month (scaler value between 0 and 1)
#' 
#' beta.f = female transmission coefficient (scaler value greater than 0),  
#' 
#' beta.m = male transmission coefficient (scaler value greater than 0),  
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
#' repro.var = variance in the annual reproduction rates from year to year
#' 
#' fawn.sur.var = variance in the annual fawn survival rate from year to year
#' 
#' sur.var = variance in the juvenile and adult survival rates from year to year. 
#' 
#' hunt.var = variance in the proportion of individuals hunter per category per year. 
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
#' 3. m.R0 = basic disease reproductive number for an initially infected adult 
#' male for direct transmission only.
#'   
#' 4. f.R0 = basic disease reproductive number for an initially infected adult 
#' female for direct transmission only.
#' 
#' @importFrom popbio stable.stage
#' @importFrom stats rbeta rbinom rnbinom rgamma
#' @importFrom dplyr rename mutate
#' @importFrom reshape2 melt
#' @importFrom magrittr %>%
#' @examples 
#' 
#' params <- list(fawn.an.sur = 0.7, juv.an.sur = 0.9, ad.an.f.sur = 0.95, 
#' ad.an.m.sur = 0.8, fawn.repro = 0, juv.repro = 0.4, ad.repro = .9, 
#' hunt.mort.fawn = 0.01, hunt.mort.juv.f = 0.1, hunt.mort.juv.m = 0.2,
#' hunt.mort.ad.f = 0.15, hunt.mort.ad.m = 0.35, ini.fawn.prev = 0.01,
#' ini.juv.prev = 0.03, ini.ad.f.prev = 0.04,  ini.ad.m.prev = 0.04,
#' n.age.cats = 12,  p = 0.27, env.foi = 0,  beta.f = 0.08,  beta.m = 0.08,
#' theta = 1, n0 = 1000, n.years = 10, rel.risk = 1.0, 
#' repro.var = 0.005, fawn.sur.var = 0.005, sur.var = 0.005, hunt.var = 0.005)
#' 
#' out <- cwd_stoch_model(params)
#' 
#' plot_tots(out$counts)
#' 
#' @export


# Stochastic monthly age and sex structured model that incorporates random draws
# from distibutions of natural survival, reproduction, and hunt mortality.
# Currently does not include a distribution on transmission rate.
cwd_stoch_model <- function(params) {

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
    hunt.mort.ad.f <- 0.1
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
  
  if(exists("beta.f")==FALSE){
    message("female transmission beta.f is missing, using default value")
    beta.f <- 0.08
  }
  
  if(exists("beta.m")==FALSE){
    message("male transmission beta.m is missing, using default value")
    beta.m <- 0.08
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
  
  if(exists("repro.var")==FALSE){
    message("repro.var is missing, using default value")
    repro.var <- 0.005
  }
  if(exists("fawn.sur.var")==FALSE){
    message("fawn.sur.var is missing, using default value")
    fawn.sur.var <- 0.005
  }
  if(exists("sur.var")==FALSE){
    message("sur.var is missing, using default value")
    sur.var <- 0.005
  }
  if(exists("hunt.var")==FALSE){
    message("hunt.var is missing, using default value")
    hunt.var <- 0.005
  }
  ###### check parameter values ###
  if(fawn.an.sur < 0) warning("fawn survival must be positive")
  if(fawn.an.sur > 1) warning("fawn survival must be < 1")
  if(juv.an.sur < 0) warning("juvenile survival must be positive")
  if(juv.an.sur > 1) warning("juvenile survival must be < 1")
  if(ad.an.f.sur < 0) warning("adult female survival must be positive")
  if(ad.an.f.sur > 1) warning("adult female survival must be < 1")
  
  if(fawn.repro < 0) warning("fawn.repro must be >= 0")
  if(juv.repro < 0) warning("juv.repro must be >= 0 ")
  if(ad.repro  < 0) warning("ad.repro must be >= 0 ")
  
  if(hunt.mort.fawn <= 0) warning("hunt.mort.fawn must be =0")
  if(hunt.mort.fawn >= 1) warning("hunt.mort.fawn must be < 1")
  if(hunt.mort.juv.f <= 0) warning("hunt.mort.juv.f must be >0")
  if(hunt.mort.juv.f >= 1) warning("hunt.mort.juv.f must be < 1")
  if(hunt.mort.juv.m <= 0) warning("hunt.mort.juv.m must be >0")
  if(hunt.mort.juv.m >= 1) warning("hunt.mort.juv.m must be < 1")
  if(hunt.mort.ad.f <= 0) warning("hunt.mort.ad.f must be >0")
  if(hunt.mort.ad.f >= 1) warning("hunt.mort.ad.f must be < 1")
  if(hunt.mort.ad.m <= 0) warning("hunt.mort.ad.m must be >0")
  if(hunt.mort.ad.m >= 1) warning("hunt.mort.ad.m must be < 1")
  
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
  if(beta.f < 0) warning("beta.f cannot be negative")
  if(beta.m < 0) warning("beta.m cannot be negative")
  if(n0 <= 0) warning("n0 must be positive")
  if(n.years <= 0) warning("n.years must be positive")
  if(rel.risk <= 0) warning("n.years must be positive")
  
  if(repro.var <= 0) warning("repro.var must be positive")
  if(fawn.sur.var <= 0) warning("fawn.sur.var must be positive")
  if(sur.var <= 0) warning("sur.var must be positive")
  if(hunt.var <= 0) warning("hunt.var must be positive")
  
  ######### CREATE INITIAL CONDITIONS##########
  months <- seq(1, n.years * 12)  # monthly timestep
  hunt.mo <- rep(0, n.years * 12)  # months in where the hunt occurs
  hunt.mo[months%%12 == 7] <- 1  # hunt.mo==1 on Nov

  # Estimate shape and scale parameters for the Beta distribution given the user
  # input of mean and variance.  natural survival
  fawn.s.b <- est_beta_params(fawn.an.sur, fawn.sur.var)
  juv.s.b <- est_beta_params(juv.an.sur, sur.var)
  ad.f.s.b <- est_beta_params(ad.an.f.sur, sur.var)
  ad.m.s.b <- est_beta_params(ad.an.m.sur, sur.var)

  # reproduction
  juv.r.b <- est_beta_params(juv.repro/2, repro.var)
  ad.r.b <- est_beta_params(ad.repro/2, repro.var)

  # hunting
  hunt.fawn.b <- est_beta_params(hunt.mort.fawn, hunt.var)
  hunt.juv.f.b <- est_beta_params(hunt.mort.juv.f, hunt.var)
  hunt.juv.m.b <- est_beta_params(hunt.mort.juv.m, hunt.var)
  hunt.f.b <- est_beta_params(hunt.mort.ad.f, hunt.var)
  hunt.m.b <- est_beta_params(hunt.mort.ad.m, hunt.var)

  # group into a vector
  # initial female prevalence
  ini.f.prev <- c(ini.fawn.prev, ini.juv.prev,
                  rep(ini.ad.f.prev, (n.age.cats - 2)))
  # initial male prevalence
  ini.m.prev <- c(ini.fawn.prev, ini.juv.prev,
                  rep(ini.ad.m.prev, (n.age.cats - 2)))

  # Create the Leslie Matrix to start the population at stable age dist
  M <- matrix(rep(0, n.age.cats * 2 * n.age.cats * 2), nrow = n.age.cats * 2)

  # replace the -1 off-diagonal with the survival rates
  M[row(M) == (col(M) + 1)] <- c(juv.an.sur * (1 - hunt.mort.juv.f),
                                 rep(ad.an.f.sur *  (1 - hunt.mort.ad.f),
                                     n.age.cats - 2), 0,
                                 c(juv.an.sur * (1 - hunt.mort.juv.m),
                                   rep(ad.an.m.sur * (1 - hunt.mort.ad.m),
                                       n.age.cats - 2)))

  # if you want the top age category to continue to survive
  M[n.age.cats, n.age.cats] <- ad.an.f.sur * (1 - hunt.mort.ad.f)
  M[n.age.cats * 2, n.age.cats * 2] <- ad.an.m.sur * (1 - hunt.mort.ad.m)

  # insert the fecundity vector prebirth census
  M[1, 1:n.age.cats] <- c(0, juv.repro, rep(ad.repro, n.age.cats - 2)) * 0.5 *
    fawn.an.sur * (1 - hunt.mort.fawn)
  M[n.age.cats + 1, 1:n.age.cats] <- M[1, 1:n.age.cats]

  # pre-allocate the output matrices
  tmp <- matrix(0, nrow = n.age.cats, ncol = n.years * 12)
  St.f <- tmp  # susceptible female vector
  St.m <- tmp  # suceptible male vector
  # infectious categories
  It.m <- array(rep(tmp), dim = c(n.age.cats, n.years * 12, 10))  # females
  It.f <- array(rep(tmp), dim = c(n.age.cats, n.years * 12, 10))  # males

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
  St.f[, 1] <- round(popbio::stable.stage(M)[1:n.age.cats] * n0 * (1 - ini.f.prev))
  St.m[, 1] <- round(popbio::stable.stage(M)[(n.age.cats + 1):(n.age.cats * 2)] *
                       n0 * (1 - ini.m.prev))

  if(sum(St.f[,1]) <= 0) {
    warning("These parameters result in a stable age structure with no surviving 
            females.")
  } 
  
  # randomly allocating infecteds across ages and categories.
  It.f[, 1, 1:10] <- rbinom(n.age.cats * 10, round(popbio::stable.stage(M)[1:n.age.cats] *
                                                     n0/10), ini.f.prev)
  It.m[, 1, 1:10] <- rbinom(n.age.cats * 10, round(popbio::stable.stage(M)
                                                   [(n.age.cats + 1):(n.age.cats * 2)] *
                                                     n0/10), ini.m.prev)

  # calculate R0s for adult females and males
  # in the denominator find the average minimum survival for the 3 mortality types
  f.R0 <-  (beta.f * n0) / (n0 ^ theta) * 
    mean(apply(cbind(rnbinom(1000, 1, (1 - ad.an.f.sur^(1/12))), 
                     rnbinom(1000, 1, (1 - (1 - hunt.mort.ad.f)^(1/12))),
                     rgamma(1000, 10, p)), 1, FUN = min, na.rm = T))
  
  m.R0 <-  (beta.m * n0)  / (n0 ^ theta) *
    mean(apply(cbind(rnbinom(1000, 1, (1 - ad.an.m.sur^(1/12))), 
                     rnbinom(1000, 1, (1 - (1 - hunt.mort.ad.m)^(1/12))),
                     rgamma(1000, 10, p)), 1, FUN = min, na.rm = T))

  rm(M)
  
  
  ####### POPULATION MODEL############
  for (t in 2:(n.years * 12)) {

    # Annual Parameter Draws monthly stochastic survival rates
    fawn.sur.draw <- rbeta(1, fawn.s.b$alpha, fawn.s.b$beta, ncp = 0)^(1/12)
    juv.sur.draw <- rbeta(1, juv.s.b$alpha, juv.s.b$beta, ncp = 0)^(1/12)
    ad.f.sur.draw <- rbeta(1, ad.f.s.b$alpha, ad.f.s.b$beta, ncp = 0)^(1/12)
    ad.m.sur.draw <- rbeta(1, ad.m.s.b$alpha, ad.m.s.b$beta, ncp = 0)^(1/12)

    # monthly stochastic reproductive rates
    juv.preg.draw <- rbeta(1, juv.r.b$alpha, juv.r.b$beta, ncp = 0)
    ad.preg.draw <- rbeta(1, ad.r.b$alpha, ad.r.b$beta, ncp = 0)

    # group into a vector
    Sur.f <- c(fawn.sur.draw, juv.sur.draw, rep(ad.f.sur.draw, n.age.cats - 2))
    Sur.m <- c(fawn.sur.draw, juv.sur.draw, rep(ad.m.sur.draw, n.age.cats - 2))

    # stochastic hunting survival rates
    hunt.fawn.draw <- rbeta(1, hunt.fawn.b$alpha, hunt.fawn.b$beta, ncp = 0)
    hunt.juv.f.draw <- rbeta(1, hunt.juv.f.b$alpha, hunt.juv.f.b$beta, ncp = 0)
    hunt.juv.m.draw <- rbeta(1, hunt.juv.m.b$alpha, hunt.juv.m.b$beta, ncp = 0)
    hunt.f.draw <- rbeta(n.age.cats - 2, hunt.f.b$alpha, hunt.f.b$beta, ncp = 0)
    hunt.m.draw <- rbeta(n.age.cats - 2, hunt.m.b$alpha, hunt.m.b$beta, ncp = 0)

    # on birthdays add in recruits and age everyone by one year also on birthdays do
    # the random parameter draws births happen in June, model starts in May
    if (t%%12 == 2) {

      # the last age category remains in place and doesn't die
      St.f[2:(n.age.cats - 1), t] <- St.f[1:(n.age.cats - 2), t - 1]
      St.f[n.age.cats, t] <- St.f[n.age.cats, t - 1] +
                                St.f[(n.age.cats - 1), t - 1]
      St.m[2:(n.age.cats - 1), t] <- St.m[1:(n.age.cats - 2), t - 1]
      St.m[n.age.cats, t] <- St.m[n.age.cats, t - 1] +
                                St.m[(n.age.cats - 1), t - 1]
      It.f[2:(n.age.cats - 1), t, ] <- It.f[1:(n.age.cats - 2), t - 1, ]
      It.f[n.age.cats, t, ] <- It.f[n.age.cats, t - 1, ] +
                                  It.f[(n.age.cats - 1), t - 1, ]
      It.m[2:(n.age.cats - 1), t, ] <- It.m[1:(n.age.cats - 2), t - 1, ]
      It.m[n.age.cats, t, ] <- It.m[n.age.cats, t - 1, ] +
                                It.m[(n.age.cats - 1), t - 1, ]

      # reproduction
      I_juv <- sum(It.f[2, t - 1, ])
      I_adults <- sum(It.f[3:n.age.cats, t - 1, ])

      fawns_born <- rbinom(1, (St.f[2, t - 1] + I_juv), juv.preg.draw) * 2 +
        rbinom(1, (sum(St.f[3:n.age.cats, t - 1]) + I_adults), ad.preg.draw) *
        2

      St.f[1, t] <- rbinom(1, fawns_born, 0.5)
      St.m[1, t] <- fawns_born - St.f[1, t]
    }

    # if not June populate the current month with last months values
    if (t%%12 != 2) {
      # updating the next month
      St.f[, t] <- St.f[, t - 1]
      St.m[, t] <- St.m[, t - 1]
      It.f[, t, ] <- It.f[, t - 1, ]
      It.m[, t, ] <- It.m[, t - 1, ]
    }

    ## Natural Mort then hunt then disease mort Then transmission Natural Mortality
    ## susceptibles
    nat.s.f <- rbinom(n.age.cats, St.f[, t], (1 - Sur.f))
    nat.s.m <- rbinom(n.age.cats, St.m[, t], (1 - Sur.m))

    St.f[, t] <- St.f[, t] - nat.s.f
    St.m[, t] <- St.m[, t] - nat.s.m
    # infecteds
    nat.i.f <- matrix(rbinom(length(It.f[, t, ]), size = It.f[, t, ],
                             prob = (1 - Sur.f)), nrow = 12)
    nat.i.m <- matrix(rbinom(length(It.m[, t, ]), size = It.m[, t, ],
                             prob = (1 - Sur.m)), nrow = 12)

    It.f[, t, ] <- It.f[, t, ] - nat.i.f
    It.m[, t, ] <- It.m[, t, ] - nat.i.m

    Dt.f[, t] <- nat.s.f + rowSums(nat.i.f)
    Dt.m[, t] <- nat.s.m + rowSums(nat.i.m)

    # Hunt mortality
    if (hunt.mo[t] == 1) {
      Iall.f <- rowSums(It.f[, t, ])  # total # infected females
      Iall.m <- rowSums(It.m[, t, ])  # total # infected males
      Nt.f <- St.f[, t] + Iall.f  # total population of females
      Nt.m <- St.m[, t] + Iall.m  # total population of males

      # binomial draw on the total hunted
      Ht.f[, t] <- rbinom(n.age.cats, Nt.f, c(hunt.fawn.draw, hunt.juv.f.draw,
                                              hunt.f.draw))

      Ht.m[, t] <- rbinom(n.age.cats, Nt.m, c(hunt.fawn.draw, hunt.juv.m.draw,
                                              hunt.m.draw))

      # those hunted in the I class overall based on the total hunted, the total that
      # are susceptible/infected and the relative hunting risk of S v. I can result in
      # a divide by 0 and NA.  this can also result in more hunting of a category than
      # are available.

      hunted.i.f <- round((rel.risk * Iall.f * Ht.f[, t]) /
                            (St.f[, t] + rel.risk * Iall.f))
      hunted.i.m <- round((rel.risk * Iall.m * Ht.m[, t]) /
                            (St.m[, t] + rel.risk * Iall.m))

      hunted.i.f[which(is.na(hunted.i.f))] <- 0
      hunted.i.m[which(is.na(hunted.i.m))] <- 0

      hunted.i.f[Iall.f < hunted.i.f] <- Iall.f[Iall.f < hunted.i.f]
      hunted.i.m[Iall.m < hunted.i.m] <- Iall.m[Iall.m < hunted.i.m]

      # subtracting out those hunted in the S class
      St.f[, t] <- St.f[, t] - (Ht.f[, t] - hunted.i.f)
      St.m[, t] <- St.m[, t] - (Ht.m[, t] - hunted.i.m)

      # allocate those deaths across the 10 I categories
      It.f[, t, ] <- allocate_deaths(hunted.i.f, It.f[, t, ])
      It.m[, t, ] <- allocate_deaths(hunted.i.m, It.m[, t, ])
    }

    # Disease mortality stochastic movement of individuals from I1 to I2 disease
    # induced mortality here by advancing all I's and only a proportion of the 10th
    # category remains
    I.f.move <- matrix(rbinom(n.age.cats * 10, size = It.f[, t, ], prob = p),
                       nrow = 12)
    I.m.move <- matrix(rbinom(n.age.cats * 10, size = It.m[, t, ], prob = p),
                       nrow = 12)

    # store info on those that die directly from disease
    CWDt.f[, t] <- I.f.move[, 10]
    CWDt.m[, t] <- I.m.move[, 10]

    # move the I individuals forward in their categories
    It.f[, t, 1] <- It.f[, t, 1] - I.f.move[, 1]
    It.f[, t, 2:10] <- It.f[, t, 2:10] - I.f.move[, 2:10] + I.f.move[, 1:9]

    It.m[, t, 1] <- It.m[, t, 1] - I.m.move[, 1]
    It.m[, t, 2:10] <- It.m[, t, 2:10] - I.m.move[, 2:10] + I.m.move[, 1:9]

    # Direct transmission considering all I's are equal
    Iall <- sum(It.f[, t, ] + It.m[, t, ])
    Nall <- sum(St.f[, t] + St.m[, t]) + Iall

    foi.f <- 1 - exp(-beta.f * Iall/Nall^theta)
    foi.m <- 1 - exp(-beta.m * Iall/Nall^theta)

    transmission.f <- rbinom(n.age.cats, St.f[, t], foi.f)
    transmission.m <- rbinom(n.age.cats, St.m[, t], foi.m)

    St.f[, t] <- St.f[, t] - transmission.f
    St.m[, t] <- St.m[, t] - transmission.m

    # update with the new infections
    It.f[, t, 1] <- transmission.f + It.f[, t, 1]
    It.m[, t, 1] <- transmission.m + It.m[, t, 1]

    # Environmental transmission happens last
    envcases.f <- rbinom(n.age.cats, St.f[, t], env.foi)
    envcases.m <- rbinom(n.age.cats, St.m[, t], env.foi)

    St.f[, t] <- St.f[, t] - envcases.f
    St.m[, t] <- St.m[, t] - envcases.m

    It.f[, t, 1] <- It.f[, t, 1] + envcases.f
    It.m[, t, 1] <- It.m[, t, 1] + envcases.m
  }
  # group the output
  counts <- list(St.f = St.f, St.m = St.m, I1t.f = It.f[, , 1],
                 I1t.m = It.m[, , 1], I2t.f = It.f[, , 2], I2t.m = It.m[, , 2],
                 I3t.f = It.f[, , 3], I3t.m = It.m[, , 3], I4t.f = It.f[, , 4],
                 I4t.m = It.m[, , 4], I5t.f = It.f[, , 5], I5t.m = It.m[ , , 5],
                 I6t.f = It.f[, , 6], I6t.m = It.m[, , 6], I7t.f = It.f[, , 7],
                 I7t.m = It.m[, , 7], I8t.f = It.f[, , 8], I8t.m = It.m[, , 8],
                 I9t.f = It.f[, , 9], I9t.m = It.m[, , 9], I10t.f = It.f[, , 10],
                 I10t.m = It.m[, , 10])

  deaths <- list(Ht.f = Ht.f, Ht.m = Ht.m, Dt.f = Dt.f, Dt.m = Dt.m, CWDt.f = CWDt.f,
                 CWDt.m = CWDt.m)

  # convert the output to long form
  counts.long <- melt(counts) %>%
    dplyr::rename(age = Var1, month = Var2, population = value, category = L1) %>%
    mutate(year = (month - 1)/12, sex = as.factor(str_sub(category, - 1)), disease = "no")
  counts.long$disease[str_sub(counts.long$category, 1, 1) == "I"] <- "yes"
  counts.long$disease <- as.factor(counts.long$disease)

  deaths.long <- melt(deaths) %>%
    dplyr::rename(age = Var1, month = Var2, population = value, category = L1) %>%
    mutate(year = (month - 1)/12, sex = as.factor(str_sub(category, - 1)))

  output <- list(counts = counts.long, deaths = deaths.long, f.R0 = f.R0, 
                 m.R0 = m.R0)
}

#' CWD deterministic model with predation
#'
#' Deterministic monthly age and sex structured model with constant
#'  environmental transmission and dynamic direct transmission. The function 
#'  conducts one run of the model. Healthy and infected individuals are 
#'  removed from the population 
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
#' env.foi = % of the population that is infected by the environment per month (scaler value between 0 and 1) #' 
#' beta.ff = female to female transmission coefficient (scaler value greater than 0),  
#' 
#' theta = effect of population size on transmission (1 = frequency dependence, 0 = density dependent).  
#' 
#' n0 = initial population size (scaler value >= 0)
#' 
#' n.years = number of years to run the model (scaler value greater than 2),  
#' 
#' rel.risk = relative risk of infected individuals being hunted. A value of 1 
#' indicates no hunter preference for infected individuals  
#' 
#' n.predators = initial predator abundance (scaler value >= 0) 
#' 
#' min.predators = minimum predator abundance allowed (scaler value >= 0 and less than n.predators) 
#' 
#' max.predators = maximum predator abundance allowed (scaler value >= 0 and greater than n.predators) 
#' 
#' k_max = maximum per capita kill rate (scaler value >= 0) 
#' 
#' k_inflect = inflection point for functional response (scaler value >= 0) 
#' 
#' n_inflect = inflection point for numerical response (scaler value >= 0) 
#' 
#' numeric.form = detemines "Type2" or "Type3" numeric form (character value equal to "Type2" or "Type3") 
#' 
#' functional.form = detemines "Type2" or "Type3" functional form (character value equal to "Type2" or "Type3") 
#' 
#' r = rate of increase in prey selection as CWD progresses (scaler value >= 0)
#' 
#' selection.form = form of increase in prey selection as CWD progresses (character value equal to "exponential", "linear", or "equal")
#' 
#' beta.f.low = transmission rate for CWD stages 1-7, which can differ by sex f=female (scaler value >= 0) 
#' 
#' beta.m.low = transmission rate for CWD stages 1-7, which can differ by sex m=male (scaler value >= 0) 
#' 
#' S = increase in CWD transmission rate for CWD stages 8-10 (scaler value >= 0) 
#' 
#' base.juv=  baseline relative selection for juveniles by predators (scaler value >= 0)
#' 
#' base.adult = baseline relative selection for young adults by predators (scaler value >= 0)
#' 
#' base.old = baseline relative selection for senescent adults by predators (scaler value >= 0)
#' 
#' stages = total number of CWD stages to include, where 0 is uninfected (vector from 0 to 0 through 10)
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
#' 3. predation -- predator population size and predation (number of 
#'  individuals and proportiion of diet) of sex- and age-specific prey
#'  
#'  Columns include: 
#'  
#'  age in years (NA if recording predator population size), 
#'  
#'  month of the simulation , 
#'  
#'  population: number of individuals (if category is
#'  "Predated_" or "Predator") or proportion of predator diet (if category is 
#'  "Proportion_Diet_"), 
#'  
#'  category: "Predator" to describe predator population. "Predated_" or 
#'  "Proportion_Diet_" for healthy females "Hf" and males "Hm" or "I1" 
#'  through "I10" for infected females and males in disease categories 1:10 
#'  to describe prey consumed andd proportion of prey consumed.
#'    
#'  year = year of the simulation
#'  
#'  sex
#'  
#'  4. params -- list of elements entered into the function, for plotting purposes
#' 
#' @importFrom popbio stable.stage
#' @importFrom stats rbeta rbinom rnbinom rgamma
#' @importFrom dplyr rename mutate
#' @importFrom reshape2 melt
#' @examples 
#' 
#' #WOLF-ELK Parameters
#' params <- list(n.predators=70, k_max=1, min.predators=20,
#'                max.predators=150, r=0.25, k_inflect=3000, n_inflect=2000,
#'                numeric.form="Type2", functional.form="Type3", 
#'                beta.f.low=0.026, beta.m.low=0.026, S=5, env.foi=0.0, 
#'                base.juv=3, base.adult=1, base.old=2, p=0.28, 
#'                selection.form='exponential', stages=c(0:10),
#'                juvs=c(1:2), adults=c(3:13), old=c(14:18), n.age.cats=18, 
#'                fawn.an.sur=0.65, juv.an.sur=0.85, ad.an.f.sur=0.9,
#'                ad.an.m.sur=0.85, fawn.repro=0, juv.repro=0.4, ad.repro=0.85,
#'                hunt.mort.fawn=0.01, hunt.mort.juv.f=0.03, hunt.mort.juv.m=0.03,
#'                hunt.mort.ad.f=0.05, hunt.mort.ad.m=0.1, 
#'                ini.fawn.prev=0.01, ini.juv.prev=0.03, ini.ad.f.prev=0.04, ini.ad.m.prev=0.04,
#'                n0=10000, n.years=20, theta=1, rel.risk=1.0)
#' 
#' out <- cwd_detmod_predation_late(params)
#' 
#' # COUGAR-DEER Parameters
#' params <- list(n.predators=40, k_max=3, min.predators=10,
#'                max.predators=80, r=0.25, k_inflect=3000, n_inflect=2000, 
#'                numeric.form="Type2", functional.form="Type3",
#'                beta.f.low=0.028, beta.m.low=0.028, S=7, env.foi=0.0, 
#'                base.juv=3, base.adult=1, base.old=1.5, p=0.43, 
#'                selection.form='exponential', stages=c(0:10),
#'                juvs=c(1:2), adults=c(3:8), old=c(9:10), n.age.cats=10, 
#'                fawn.an.sur=0.5, juv.an.sur=0.8, ad.an.f.sur=0.9,
#'                ad.an.m.sur=0.85, fawn.repro=0, juv.repro=0.4, ad.repro=1.2,
#'                hunt.mort.fawn=0.01, hunt.mort.juv.f=0.02, hunt.mort.juv.m=0.02,
#'                hunt.mort.ad.f=0.04, hunt.mort.ad.m=0.1,
#'                ini.fawn.prev=0.01, ini.juv.prev=0.03, ini.ad.f.prev=0.04, ini.ad.m.prev=0.04,
#'                n0=10000, n.years=20, theta=1, rel.risk=1.0)
#'                
#' out <- cwd_detmod_predation_late(params) 
#' plot_tots(out$counts)
#' 
#' 
#' @export


cwd_detmod_predation_late <- function(params) {
  # write the list objects to the local environment
  for (v in 1:length(params)) assign(names(params)[v], params[[v]])
  
  #### set prey selection values based on functional form
  if(selection.form=="equal"){
    s.juv <- rep(base.juv, length(stages))
    s.adult <- rep(base.adult, length(stages))
    s.old <- rep(base.old, length(stages))
  }
  if(selection.form=="linear"){
    s.juv <- base.juv + stages*r
    s.adult <- base.adult + stages*r
    s.old <- base.old + stages*r
  }
  
  if(selection.form=="exponential"){
    s.juv <- base.juv*(1 + r)^stages
    s.adult <- base.adult*(1 + r)^stages
    s.old <- base.old*(1 + r)^stages
  }
  selection.coeffs <- data.frame(cbind(s.juv,s.adult,s.old))
  
  ## SET YOUR TRANSMISSION VALUES where S is the scaled transmission for late-stage CWD
  beta.f.high <- beta.f.low*S; beta.m.high <- beta.m.low*S
  
  ######### CREATE INITIAL CONDITIONS (monthly index) ##########
  months <- seq(1, n.years * 12)   # monthly timestep
  hunt.mo <- rep(0, n.years * 12)  # months in where the hunt occurs
  hunt.mo[months%%12 == 7] <- 1    # hunt.mo==1 on Nov
  
  # Natural monthly survival rates
  fawn.sur <- fawn.an.sur^(1/12)
  juv.sur <- juv.an.sur^(1/12)
  ad.f.sur <- ad.an.f.sur^(1/12)
  ad.m.sur <- ad.an.m.sur^(1/12)
  
  # group into a vector initial female prevalence
  ini.f.prev <- c(ini.fawn.prev, ini.juv.prev, rep(ini.ad.f.prev, (n.age.cats - 2)))
  # initial male prevalence
  ini.m.prev <- c(ini.fawn.prev, ini.juv.prev, rep(ini.ad.m.prev, (n.age.cats - 2)))
  
  # Create the survival and birth vectors vector of survival rates for all age classes
  Sur.f <- c(fawn.sur, juv.sur, rep(ad.f.sur, n.age.cats - 2))
  Sur.m <- c(fawn.sur, juv.sur, rep(ad.m.sur, n.age.cats - 2))
  
  # Create the Leslie Matrix to start the population at stable age dist (half is F, half is M)
  M <- matrix(rep(0, n.age.cats * 2 * n.age.cats * 2), nrow = n.age.cats * 2)
  # replace the -1 off-diagonal with the survival rates
  M[row(M) == (col(M) + 1)] <- c(juv.an.sur * (1 - hunt.mort.juv.f),
                                 rep(ad.an.f.sur * (1 - hunt.mort.ad.f),
                                     n.age.cats - 2), 0,
                                 c(juv.an.sur *
                                     (1 - hunt.mort.juv.m),
                                   rep(ad.an.m.sur * (1 - hunt.mort.ad.m),
                                       n.age.cats - 2)))
  # if you want the top age category to continue to survive (yes), put adult female survival in top age cat
  M[n.age.cats, n.age.cats] <- ad.an.f.sur * (1 - hunt.mort.ad.f)
  # adult male survival in top age cat
  M[n.age.cats * 2, n.age.cats * 2] <- ad.an.m.sur * (1 - hunt.mort.ad.m)
  
  # insert the fecundity vector for prebirth census - assuming 50/50 sex ratio
  M[1, 1:n.age.cats] <- c(0, juv.repro, rep(ad.repro, n.age.cats - 2)) *
    0.5 * fawn.an.sur * (1 - hunt.mort.fawn)
  M[n.age.cats + 1, 1:n.age.cats] <- M[1, 1:n.age.cats]
  
  ## look at the matrix
  # View(M)
  
  # pre-allocate the output matrices
  tmp <- matrix(0, nrow = n.age.cats, ncol = n.years * 12)
  St.f <- tmp  # susceptible female vector
  St.m <- tmp  # susceptible male vector
  It.f <- array(rep(tmp), dim = c(n.age.cats, n.years * 12, 10))  # infectious females
  It.m <- array(rep(tmp), dim = c(n.age.cats, n.years * 12, 10))  # infectious males
  
  # keep track of number of predators (monthly, but same for all months in year)
  Predators <- c(rep(n.predators,12), rep(0,(n.years-1)*12))
  # keep track of the proportion of age*infection class in predators' diet
  Proportion_Diet_Hf <- tmp   # healthy females
  Proportion_Diet_Hm <- tmp   # healthy males
  Proportion_Diet_If <- It.f  # infected females
  Proportion_Diet_Im <- It.m  # infected males
  # keep track of number of prey predated
  Predated_Hf <- tmp   # healthy females
  Predated_Hm <- tmp   # healthy males
  Predated_If <- It.f  # infected females
  Predated_Im <- It.m  # infected males
  
  # tracking the # hunted by sex
  Ht.f <- tmp
  Ht.m <- tmp
  # natural deaths by sex
  Dt.f <- tmp
  Dt.m <- tmp
  # disease deaths by sex
  CWDt.f <- tmp
  CWDt.m <- tmp
  
  # Initializing with the stable age distribution
  St.f[, 1] <- popbio::stable.stage(M)[1:n.age.cats] * n0 * (1 - ini.f.prev)
  St.m[, 1] <- popbio::stable.stage(M)[(n.age.cats + 1):(n.age.cats * 2)] * n0 * (1 - ini.m.prev)
  
  if(sum(St.f[,1]) <= 0) {
    warning("These parameters result in a stable age structure with no surviving 
            females.")
  } 
  
  # equally allocating prevalence across ages
  It.f[, 1, 1:10] <- popbio::stable.stage(M)[1:n.age.cats] * n0/10 * ini.f.prev
  It.m[, 1, 1:10] <- popbio::stable.stage(M)[(n.age.cats + 1):(n.age.cats * 2)] * n0/10 * ini.m.prev
  # check it
  # It.f[, 1, 1:10]
  # It.f[1:10, 1, 1]
  
  ########## POPULATION MODEL ############
  for (t in 2:(n.years * 12)) {
    
    # on birthdays, add in recruits and age everyone by one year births
    # reproduction is in June, model starts in May
    if (t%%12 == 2) {
      
      # aging the last age category remains in place and doesn't die
      St.f[2:(n.age.cats - 1), t] <- St.f[1:(n.age.cats - 2), t - 1]
      St.f[n.age.cats, t] <- St.f[n.age.cats, t - 1] + St.f[(n.age.cats - 1), t - 1]
      
      St.m[2:(n.age.cats - 1), t] <- St.m[1:(n.age.cats - 2), t - 1]
      St.m[n.age.cats, t] <- St.m[n.age.cats, t - 1] + St.m[(n.age.cats - 1), t - 1]
      
      It.f[2:(n.age.cats - 1), t, ] <- It.f[1:(n.age.cats - 2), t - 1, ]
      It.f[n.age.cats, t, ] <- It.f[n.age.cats, t - 1, ] + It.f[(n.age.cats - 1), t - 1, ]
      
      It.m[2:(n.age.cats - 1), t, ] <- It.m[1:(n.age.cats - 2), t - 1, ]
      It.m[n.age.cats, t, ] <- It.m[n.age.cats, t - 1, ] + It.m[(n.age.cats - 1), t - 1, ]
      
      # reproduction (female only)
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
    
    # Disease mortality: stochastic movement of individuals from one stage to next
    # disease induced mortality occurs by advancing all I's and only a proportion of the final (10) category remains
    f.move <- It.f[, t, ] * p
    m.move <- It.m[, t, ] * p
    
    It.f[, t, 1] <- It.f[, t, 1] - f.move[, 1]
    It.f[, t, 2:10] <- It.f[, t, 2:10] - f.move[, 2:10] + f.move[, 1:9]
    It.m[, t, 1] <- It.m[, t, 1] - m.move[, 1]
    It.m[, t, 2:10] <- It.m[, t, 2:10] - m.move[, 2:10] + m.move[, 1:9]
    
    # store info on those that die directly from disease
    CWDt.f[, t] <- f.move[, 10]
    CWDt.m[, t] <- m.move[, 10]
    
    ## TWO TRANSMISSION VALUES: early (1-7) and late (8-10)
    Ilow <- sum(It.f[, t, 1:7] + It.m[, t, 1:7])
    Ihigh <- sum(It.f[, t, 8:10] + It.m[, t, 8:10])
    Nall <- sum(St.f[, t] + St.m[, t]) + Ilow + Ihigh
    
    ## CALCULATE NEW CASES
    cases.f <- St.f[, t] * (1 - exp(- (beta.f.low*(Ilow/Nall^theta) + beta.f.high*(Ihigh/Nall^theta)) ))
    cases.m <- St.m[, t] * (1 - exp(- (beta.m.low*(Ilow/Nall^theta) + beta.m.high*(Ihigh/Nall^theta)) ))
    
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
    
    ########################### PREDATION ############################
    ## SELECTION
    # selection.coeffs     # check these
    
    # make new variables to work with temporarily so we are not working in the main data frame
    total.prey <- sum(St.f[,t], St.m[,t], It.f[,t,], It.m[,t,])
    A.f <- St.f[,t]
    A.m <- St.m[,t]
    AI.f <- It.f[,t,]
    AI.m <- It.m[,t,]
    # use .s to denote 'selected'
    A.f.s <- St.f[,t]
    A.m.s <- St.m[,t]
    AI.f.s <- It.f[,t,]
    AI.m.s <- It.m[,t,]
    
    #### PART ONE
    ## CALCULATE abundance*selection for all HEALTHY individuals (j=0)
    for(i in 1:length(A.f)){  
      if(i %in% juvs){
        temp.s <- selection.coeffs$s.juv[1]
      } 
      if(i %in% adults){
        temp.s <- selection.coeffs$s.adult[1]
      } 
      if(i %in% old){
        temp.s <- selection.coeffs$s.old[1]
      }
      # temp.s
      
      A.f.s[i] <-  (A.f[i]*temp.s)
      A.m.s[i] <-  (A.m[i]*temp.s)
    }
    # A.f; A.f.s
    # A.m; A.m.s
    
    ## CALCULATE abundance*selection for all INFECTED individuals (j=1-10)
    for(i in 1:nrow(AI.f)){
      if(i %in% juvs){ 
        temp.s <- selection.coeffs[2:nrow(selection.coeffs), 1]
      }
      if(i %in% adults){
        temp.s <- selection.coeffs[2:nrow(selection.coeffs), 2]
      }
      if(i %in% old){
        temp.s <- selection.coeffs[2:nrow(selection.coeffs), 3]
      }
      # temp.s
      
      AI.f.s[i,] <-  (AI.f[i,]*temp.s)
      AI.m.s[i,] <-  (AI.m[i,]*temp.s)
    }
    # AI.f; AI.f.s
    # AI.m; AI.m.s
    
    ## CALCULATE proportion of each age*infection class in the predator's diet
    # first we need the total sum
    total.sum <- ( sum(A.f.s) + sum(A.m.s) + sum(AI.f.s) + sum(AI.m.s) )
    # equation = sA[i] / (sA[i]+sum(all other sA[i]'s)
    A.f.p <- (A.f.s / total.sum)
    A.m.p <- (A.m.s / total.sum)
    AI.f.p <- (AI.f.s / total.sum)
    AI.m.p <- (AI.m.s / total.sum)
    # if(round(sum(A.f.p) + sum(A.m.p) + sum(AI.f.p) + sum(AI.m.p),3)>1.10){
    #   print("PREDATOR DIET EXCEEDS 100%")
    # }
    # compare predation by age class / infection class
    # A.f.p ; colSums(AI.f.p)
    # A.m.p ; colSums(AI.m.p)
    
    ## PUT INTO STORAGE ARRAYS
    # reminder that HEALTHY storage matrix is Proportion_Diet_H
    # reminder that INFECTED storage matrix is Proportion_Diet_I
    Proportion_Diet_Hf[,t] <- A.f.p
    Proportion_Diet_Hm[,t] <- A.m.p
    Proportion_Diet_If[,t,1:10] <- AI.f.p
    Proportion_Diet_Im[,t,1:10] <- AI.m.p
    
    #### PART TWO - calculate prey removal
    n.predators <- Predators[t]

    if(functional.form=="Type2"){
      k <- (n.predators*k_max*total.prey) / (k_inflect + total.prey)
    }
    if(functional.form=="Type3"){
      k <- (n.predators*k_max*(total.prey^2)) / ((k_inflect^2)+(total.prey^2))
    }
    k
    
    ## calculate how many individuals to REMOVE in their age*infection class + PUT INTO STORAGE ARRAYS
    # reminder that HEALTHY storage matrix is Predated_H
    # reminder that INFECTED storage matrix is Predated_I
    Predated_Hf[,t] <- (A.f.p * k)
    Predated_Hm[,t] <- (A.m.p * k)
    Predated_If[,t,1:10] <- (AI.f.p * k)
    Predated_Im[,t,1:10] <- (AI.m.p * k)
    
    ## check that we are removing all that we should be
    # floor(k) == floor(sum(A.f.p * k) + sum(A.m.p * k) + sum(AI.f.p * k) + sum(AI.m.p * k))
    
    # make sure we are not removing more than is in each age*infection class
    # if(any(Predated_Hf[,t] > St.f[, t])==TRUE){
    #   print("MORE HEALTHY FEMALES REMOVED BY PREDATION THAN AVAILABLE")
    #   # print(which(Predated_Hf[,t] > St.f[, t]))
    # }
    # if(any(Predated_Hm[,t] > St.m[, t])==TRUE){
    #   print("MORE HEALTHY MALES REMOVED BY PREDATION THAN AVAILABLE")
    #   # print(which(Predated_Hm[,t] > St.m[, t]))
    # }
    # if(any(Predated_If[,t,1:10] > It.f[, t, ])==TRUE){
    #   print("MORE INFECTED FEMALES REMOVED BY PREDATION THAN AVAILABLE")
    #   print(which(Predated_If[,t,1:10] > It.f[, t, ]))
    # }
    # if(any(Predated_Im[,t,1:10] > It.m[, t, ])==TRUE){
    #   print("MORE INFECTED MALES REMOVED BY PREDATION THAN AVAILABLE")
    #   # print(which(Predated_Im[,t,1:10] > It.m[, t, ]))
    # }
    
    ## ACTUALLY REMOVE THOSE INDIVIDUALS
    St.f[, t] <- (St.f[, t] - Predated_Hf[, t])
    St.m[, t] <- (St.m[, t] - Predated_Hm[, t])
    It.f[, t, ] <- (It.f[, t, ] - Predated_If[, t, ])
    It.m[, t, ] <- (It.m[, t, ] - Predated_Im[, t, ])
    
    ################################################################# 
    
    # Hunt mortality
    if (hunt.mo[t] == 1) {
      Iall.f <- rowSums(It.f[, t, ])
      Iall.m <- rowSums(It.m[, t, ])
      Nt.f <- St.f[, t] + Iall.f
      Nt.m <- St.m[, t] + Iall.m
      
      # total hunted
      hunted.f <- Nt.f * c(hunt.mort.fawn, hunt.mort.juv.f, rep(hunt.mort.ad.f, n.age.cats - 2))
      hunted.m <- Nt.m * c(hunt.mort.fawn, hunt.mort.juv.m, rep(hunt.mort.ad.m, n.age.cats - 2))
      
      # tracking the # hunted
      Ht.f[, t] <- hunted.f
      Ht.m[, t] <- hunted.m
      
      # those hunted in the I class overall are based on: the total hunted, the total that are susceptible/infected, and the relative hunting risk of S v. I - warning: this can result in a dividing by 0 and NA. 
      # This can also result in more hunting of a category than are available.
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
    
    # Natural mortality
    St.f[, t] <- St.f[, t] * Sur.f
    St.m[, t] <- St.m[, t] * Sur.m
    
    It.f[, t, ] <- It.f[, t, ] * Sur.f
    It.m[, t, ] <- It.m[, t, ] * Sur.m
    
    Dt.f[, t] <- (St.f[, t] + rowSums(It.f[, t, ])) * (1 - Sur.f)
    Dt.m[, t] <- (St.m[, t] + rowSums(It.m[, t, ])) * (1 - Sur.m)
    
    ######################## PREDATOR NUMERICAL RESPONSE ########################
    if (t%%12 == 0){  ## perform on the last month of the year (April)
      total.prey <- sum(St.f[, t], St.m[, t], It.f[, t, ], It.m[, t, ])
      
      if(numeric.form=="Type2"){
        predators_t1 <- (max.predators*total.prey) / (n_inflect + total.prey)
      }
      if(numeric.form=="Type3"){
        predators_t1 <- (max.predators*(total.prey^2)) / ((n_inflect^2)+(total.prey^2))
      }
      
      predators_t1 <- ifelse(predators_t1<min.predators, min.predators, predators_t1)
      Predators[(t+1) : (t+12)] <- round(predators_t1, 2)
    }
    #################################################################
  }
  
  ## GROUP THE OUTPUTS
  Predators <- Predators[1:(length(Predators)-12)]

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
  
  predation <- list(Proportion_Diet_Hf = Proportion_Diet_Hf,
                    Proportion_Diet_Hm = Proportion_Diet_Hm,
                    Proportion_Diet_I1f = Proportion_Diet_If[, , 1],
                    Proportion_Diet_I2f = Proportion_Diet_If[, , 2],
                    Proportion_Diet_I3f = Proportion_Diet_If[, , 3],
                    Proportion_Diet_I4f = Proportion_Diet_If[, , 4],
                    Proportion_Diet_I5f = Proportion_Diet_If[, , 5],
                    Proportion_Diet_I6f = Proportion_Diet_If[, , 6],
                    Proportion_Diet_I7f = Proportion_Diet_If[, , 7],
                    Proportion_Diet_I8f = Proportion_Diet_If[, , 8],
                    Proportion_Diet_I9f = Proportion_Diet_If[, , 9],
                    Proportion_Diet_I10f = Proportion_Diet_If[, , 10],
                    Proportion_Diet_I1m = Proportion_Diet_Im[, , 1],
                    Proportion_Diet_I2m = Proportion_Diet_Im[, , 2],
                    Proportion_Diet_I3m = Proportion_Diet_Im[, , 3],
                    Proportion_Diet_I4m = Proportion_Diet_Im[, , 4],
                    Proportion_Diet_I5m = Proportion_Diet_Im[, , 5],
                    Proportion_Diet_I6m = Proportion_Diet_Im[, , 6],
                    Proportion_Diet_I7m = Proportion_Diet_Im[, , 7],
                    Proportion_Diet_I8m = Proportion_Diet_Im[, , 8],
                    Proportion_Diet_I9m = Proportion_Diet_Im[, , 9],
                    Proportion_Diet_I10m = Proportion_Diet_Im[, , 10],
                    Predated_Hf = Predated_Hf, 
                    Predated_Hm = Predated_Hm,
                    Predated_I1f = Predated_If[, , 1], 
                    Predated_I2f = Predated_If[, , 2], 
                    Predated_I3f = Predated_If[, , 3], 
                    Predated_I4f = Predated_If[, , 4], 
                    Predated_I5f = Predated_If[, , 5], 
                    Predated_I6f = Predated_If[, , 6], 
                    Predated_I7f = Predated_If[, , 7], 
                    Predated_I8f = Predated_If[, , 8], 
                    Predated_I9f = Predated_If[, , 9], 
                    Predated_I10f = Predated_If[, , 10], 
                    Predated_I1m = Predated_Im[, , 1], 
                    Predated_I2m = Predated_Im[, , 2], 
                    Predated_I3m = Predated_Im[, , 3], 
                    Predated_I4m = Predated_Im[, , 4], 
                    Predated_I5m = Predated_Im[, , 5], 
                    Predated_I6m = Predated_Im[, , 6], 
                    Predated_I7m = Predated_Im[, , 7], 
                    Predated_I8m = Predated_Im[, , 8], 
                    Predated_I9m = Predated_Im[, , 9], 
                    Predated_I10m = Predated_Im[, , 10])
  # convert the output to long form
  counts.long <- melt(counts) %>%
    rename(age = Var1, month = Var2, population = value, category = L1) %>%
    mutate(year = (month - 1)/12, 
           sex = as.factor(stringr::str_sub(category, -1)),
           disease = "no")
  counts.long$disease[stringr::str_sub(counts.long$category, 1, 1) == "I"] <- "yes"
  counts.long$disease <- as.factor(counts.long$disease)
  
  deaths.long <- melt(deaths) %>%
    rename(age = Var1, month = Var2, population = value, category = L1) %>%
    mutate(year = (month - 1)/12, sex = as.factor(stringr::str_sub(category, -1)))
  
  predation.long <- melt(predation) %>%
    rename(age = Var1, month = Var2, population = value, category = L1) %>%
    mutate(year = (month - 1)/12, sex = as.factor(stringr::str_sub(category, -1)))

  # add in predator counts
  predators <- data.frame(age = NA, month=months, population=Predators, 
                          category=rep('Predator',length(months)), 
                          year=(months-1)/12,  sex = NA) 
  
  predation.long <- rbind(predators, predation.long)
  
  output <- list(counts = counts.long, deaths = deaths.long, predation = predation.long, params = params)
  return(output)
}


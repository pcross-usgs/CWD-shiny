#' CWD R0 (R-naught) next-generation matrix calculation
#' 
#' Next-generation maxtrix method to calculate R0 based on transfer 
#' of infected individuals between infectious categories and new infections
#' under disease-free equilibrium. Currently does not take into account 
#' environmental transmission and assumes relative risk of harvest is equal. 
#' This formulation is designed to work with the basic transmission formulation.
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
#' n.age.cats = number of age categories to monitor. (scaler value greater than 3).
#' The final age category includes all those of that age or greater. 
#' 
#' p = rate of movement in the infectious categories (scaler values between 0 and 1). 
#' See model documentation vignette for how this relates to disease induced mortality.   
#' 
#' beta.f = female transmission coefficient (scaler value greater than 0),  
#' 
#' beta.m = male transmission coefficient (scaler value greater than 0),  
#' 
#' theta = effect of population size on transmission (1 = frequency dependence, 
#'   0 = density dependent).  
#' 
#' n0 = initial population size (scaler value greater than 0)
#' 
#' stable.stage.pop = 
#' 
#' @param stable.stage.pop whether to evaluate at a stable age and sex distribution 
#'   (logical TRUE or FALSE). FALSE dictates evaluation at an equal sex and age distribution.
#'   
#' @return A single value is returned describing the calculated R0 following next-generation matrix. 
#'
#' @importFrom popbio stable.stage
#' @importFrom stringr str_detect
#' 
#' @examples 
#' params <- list(fawn.an.sur = 0.6, juv.an.sur = 0.8, ad.an.f.sur = 0.95,
#'                ad.an.m.sur = 0.9, fawn.repro = 0, juv.repro = 0.6, ad.repro = 1,
#'                hunt.mort.fawn = 0.01, hunt.mort.juv.f = 0.1, hunt.mort.juv.m = 0.1,
#'                hunt.mort.ad.f = .1, hunt.mort.ad.m = .1, ini.fawn.prev = 0.02,
#'                ini.juv.prev = 0.03, ini.ad.f.prev = 0.04, ini.ad.m.prev = 0.04,
#'                n.age.cats = 12, p = 0.43, beta.f = 0.035, beta.m = 0.05, theta = 1, n0 = 2000)
#' 
#' R0_NGM(params, stable.stage.pop = TRUE)
#' 
#' @export


R0_NGM <- function(params, stable.stage.pop = TRUE){
  for (v in 1:length(params)) assign(names(params)[v], params[[v]])
  
  fawn.sur <- fawn.an.sur^(1/12) #monthly fawn survival
  juv.sur <- juv.an.sur^(1/12) #"" juvenile
  ad.f.sur <- ad.an.f.sur^(1/12) #"" female
  ad.m.sur <- ad.an.m.sur^(1/12) #"" male
  N <- n0 # population
  
  # create population matrix
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
  # survival in top age cat
  M[n.age.cats, n.age.cats] <- ad.an.f.sur * (1 - hunt.mort.ad.f)
  
  # adult male survival in top age cat
  M[n.age.cats * 2, n.age.cats * 2] <- ad.an.m.sur * (1 - hunt.mort.ad.m)
  
  # insert the fecundity vector for prebirth census
  M[1, 1:n.age.cats] <- c(0, juv.repro, rep(ad.repro, n.age.cats - 2)) *
    0.5 * fawn.an.sur * (1 - hunt.mort.fawn)
  
  # replace off-diagonal
  M[n.age.cats + 1, 1:n.age.cats] <- M[1, 1:n.age.cats]
  
  if (stable.stage.pop) S <- popbio::stable.stage(M)*N #stable age and sex distribution at disease free equilibrium
  if (!stable.stage.pop) S <- rep(N/(n.age.cats*2), n.age.cats*2) #equal age distribution at disease free equilibrium

  age <- rep(c(1:n.age.cats), 2) #age naming vector
  sex <- rep(c("f","m"), each = n.age.cats) # sex naming vector
  S. <- paste0("S", age, sex) # making a vector of variable names
  for (i in 1:length(age)){
    val <- S[i] #calling the value from stable DFE
    var <- S.[i] #calling the name from above
    assign(var, val) #creating a variable in the local environment
  }
  
  age <- rep(c(rep(c(1:n.age.cats), each = 10)),2) # for infectious classes
  sex <- rep(c("f","m"), each = n.age.cats*10) # for infectious classes
  cat <- rep(c(rep(c(1:10), n.age.cats)),2) # for infectious classes
  I <- paste0("I", age, sex, cat) # for infectious classes
  for (i in 1:length(age)){
    var <- I[i] # for infectious classes
    assign(var, 0) # zero because no infectious animals at dfe
  }

  A <- str2lang(paste(I[which(stringr::str_detect(I, "f") == T)], collapse = " + ")) # this creates a sum of all infectious female classes
  B <- str2lang(paste(I[which(stringr::str_detect(I, "m") == T)], collapse = " + ")) # this creates a sum of all infectious male classes
  
  F.mat <- rep(list(NA), length(age)) #creating a place to store a ton of expressions
  names(F.mat) <- paste0("F", age, sex, cat) #naming each list for my own ability to check
  
  for (i in 1:(2*10*n.age.cats)){ #for each infectious age class
    if (i == 1){
      F.mat[[i]] <- substitute(C * (1 - exp(-(beta.f * (A+B)
                                              /(N^theta)))), # this equation comes from the model vignette
                               list(A = A, B = B, C = str2lang(S.[1]))) #if fawn
    }
    if (i == 11){
      F.mat[[i]] <- substitute(C * (1 - exp(-(beta.f * (A+B)
                                              /(N^theta)))),
                               list(A = A, B = B, C = str2lang(S.[2]))) #if juvenile f
    }
    if (i %in% seq(21, 10*n.age.cats, by = 10)){
      for (j in 3:n.age.cats){
        F.mat[[i]] <- substitute(C * (1 - exp(-(beta.f * (A+B)
                                                /(N^theta)))),
                                 list(A = A, B = B, C = str2lang(S.[j]))) # if adult female in 3:n.age.cat
      }
    }
    if (i == 10*n.age.cats+1){
      F.mat[[i]] <- substitute(C * (1 - exp(-(beta.m * (A+B)
                                              /(N^theta)))),
                               list(A = A, B = B, C = str2lang(S.[1+n.age.cats]))) # for males
    }
    if (i == 10*n.age.cats+11){
      F.mat[[i]] <- substitute(C * (1 - exp(-(beta.m * (A+B)
                                              /(N^theta)))),
                               list(A = A, B = B, C = str2lang(S.[2+n.age.cats]))) # for males
    }
    if (i %in% seq(10*n.age.cats + 21, 2*10*n.age.cats, by = 10)){
      for (j in (n.age.cats+3):(2*n.age.cats)) {
        F.mat[[i]] <- substitute(C * (1 - exp(-(beta.m * (A+B)
                                                /(N^theta)))),
                                 list(A = A, B = B, C = str2lang(S.[j]))) # for males
      }
    }
    if (!i %in% seq(1, 2*10*n.age.cats, by = 10)) {F.mat[i] <- 0} # if it is not the first infectious class, there are no gains of infection, only transfers
  }
  
  Vm.mat <- rep(list(NA), length(age)) #another expression holder
  names(Vm.mat) <- paste0("Vm", age, sex, cat) #named
  
  hunt <- rep(0, length(I)) #creating vector of appropriate hunting variables
  hunt[1:10] <- "hunt.mort.fawn" #assigning harvest mortality to proper bins
  hunt[11:20] <- "hunt.mort.juv.f" #assigning harvest mortality to proper bins
  hunt[21:(n.age.cats*10)] <- "hunt.mort.ad.f" #assigning harvest mortality to proper bins
  hunt[(n.age.cats*10+1):(n.age.cats*10+10)] <- "hunt.mort.fawn" #assigning harvest mortality to proper bins
  hunt[(n.age.cats*10+11):(n.age.cats*10+20)] <- "hunt.mort.juv.m" #assigning harvest mortality to proper bins
  hunt[(n.age.cats*10+21):(n.age.cats*10*2)] <- "hunt.mort.ad.m" #assigning harvest mortality to proper bins
  
  mort <- rep(0, length(I)) #creating vector of appropriate natural mortality variables
  mort[1:10] <- "fawn.sur" #assigning natural mortality to proper bins
  mort[11:20] <- "juv.sur" #assigning natural mortality to proper bins
  mort[21:(n.age.cats*10)] <- "ad.f.sur" #assigning natural mortality to proper bins
  mort[(n.age.cats*10+1):(n.age.cats*10+10)] <- "fawn.sur" #assigning natural mortality to proper bins
  mort[(n.age.cats*10+11):(n.age.cats*10+20)] <- "juv.sur" #assigning natural mortality to proper bins
  mort[(n.age.cats*10+21):(n.age.cats*10*2)] <- "ad.m.sur"  #assigning natural mortality to proper bins
  
  for (i in 1:(2*10*n.age.cats)){
    # this says that infectious age classes lose infection because of 
    # hunting (spread over a year), natural mortality, and infection transfers
    Vm.mat[[i]] <- substitute(A*((B/12)+(1-C) + p),
                              list(A = as.symbol(I[i]), 
                                   B = as.symbol(hunt[i]), 
                                   C = as.symbol(mort[i]))) 
  }
  
  Vp.mat <- rep(list(NA), length(age)) # another expression holder
  names(Vp.mat) <- paste0("Vp", age, sex, cat)  # names
  
  for (i in 1:(2*10*n.age.cats)){
    #there are no transfers into first disease class
    if (i %in% seq(1, (2*10*n.age.cats), by = 10)){
      Vp.mat[[i]] <- 0 # force no transfers
    }
    #there are for disease classes 2:10
    if (!i %in% seq(1, (2*10*n.age.cats), by = 10)){
      Vp.mat[[i]] <- substitute(A*p, list(A = as.symbol(I[i-1]))) # transfer at rate p
    }
  }
  
  V.mat <- rep(list(NA), length(age)) # another expression holder
  names(V.mat) <- paste0("V", age, sex, cat) # named
  
  for (i in 1:(2*10*n.age.cats)){
    V.mat[[i]] <- substitute(a - b, list(a = Vm.mat[[i]], b = Vp.mat[[i]])) # subtracting appropriate expression to create loss of infection
  }
  
  f.matrix <- matrix(0, nrow = (2*10*n.age.cats), ncol = (2*10*n.age.cats)) # a blank matrix to store evaluations of gains
  v.matrix <- matrix(0, nrow = (2*10*n.age.cats), ncol = (2*10*n.age.cats)) # a blank matrix to store evaluations of losses
  
  for (i in 1:(2*10*n.age.cats)){
    for (j in 1:(2*10*n.age.cats)){
      eq <- F.mat[[i]] # calling gain equations
      disease.class <- I[j] # calling infectious classes
      partial.d <- D(eq, disease.class) # creating a partial derivivative of the "q-th" eq with respect to the "r-th" infectious class
      f.matrix[i,j] <- eval(partial.d) # Evaluating the partial derivative at disease free equilibrium, specified above
    }
  }
  
  for (i in 1:(2*10*n.age.cats)){
    for (j in 1:(2*10*n.age.cats)){
      eq <- V.mat[[i]] # calling loss equations
      disease.class <- I[j] # calling infectious classes
      partial.d <- D(eq, disease.class) # creating a partial derivivative of the "q-th" eq with respect to the "r-th" infectious class
      v.matrix[i,j] <- eval(partial.d) # Evaluating the partial derivative at disease free equilibrium, specified above
    }
  }
  R0 <- max(Re(eigen(f.matrix %*% solve(v.matrix))$values)) # this finds the largest eigen value of the matrix product of gains times inverse losses
  #NOTE: sometimes, this evaluates with complex numbers (e.g. +0i).
  return(R0)
}

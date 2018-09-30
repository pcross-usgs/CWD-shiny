# script to create some initial parameters.
rm(list =)
##########CONSTANTS#########
n.age.cats <- 12 # age categories
n0 <- 3000 # initial population size
n.years <- 10 # number of years for the simulation

#dis.mort is addressed by using 10 infectious box cars with a defined probability of transitioning, p.
p <- 0.43 #probability of transitioning between infectious box cars; determines disease-induced mortality rate

#Annual variance on reproduction; input by user?
fawn.an.repro.var <- 0
juv.an.repro.var <- 0.005
ad.an.repro.var <- 0.005

#Variance of additive hunt mortality; user input?  Right now, fixed across all age/sex classes
hunt.mort.f.var <- 0.005
hunt.mort.m.var <- 0.005
hunt.mort.i.f.var <- 0.005
hunt.mort.i.m.var <- 0.005

#######USER INPUTS#############
#Initial prevalences; user input
ini.fawn.prev <- 0
ini.juv.prev <- 0.01
ini.ad.f.prev <- 0.02
ini.ad.m.prev <- 0.02

#Natural Annual survival rates - means; input by user
fawn.an.sur <- 0.4
juv.an.sur <- 0.7
ad.an.f.sur <- 0.9
ad.an.m.sur <- 0.8

#Annual variance on survival; input by user?
fawn.an.sur.var <- 0.005
juv.an.sur.var <- 0.005
ad.an.f.sur.var <- 0.005
ad.an.m.sur.var <- 0.005

# annual reproductive rates of fawns/doe; input by user
fawn.rep <- 0
juv.rep <- 0.8
ad.rep  <- 1.7

#not sure if we need to draw this from a distribution?
foi <- 1 - (0.98^(1/12)) # monthly probability of becoming infected

#Assuming hunting mortality is drawn from a distribution:
#Mean additive hunt mortality; user input
hunt.mort.fawn <- 0.03
hunt.mort.juv <- 0.07
hunt.mort.ad.f <- 0.1
hunt.mort.ad.m <- 0.2

###########Calculated parameters####################
preg.fawn <- fawn.rep/2
preg.juv <- juv.rep/2
preg.ad <- ad.rep/2

#Function to estimate alpha and beta for a beta distribution
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

#Estimate alpha & beta values for survival
fawn.sur.alpha <- estBetaParams(fawn.an.sur, fawn.an.sur.var)$alpha
fawn.sur.beta <- estBetaParams(fawn.an.sur, fawn.an.sur.var)$beta
juv.sur.alpha <- estBetaParams(juv.an.sur, juv.an.sur.var)$alpha
juv.sur.beta <- estBetaParams(juv.an.sur, juv.an.sur.var)$beta
ad.f.sur.alpha <- estBetaParams(ad.an.f.sur, ad.an.f.sur.var)$alpha
ad.f.sur.beta <- estBetaParams(ad.an.f.sur, ad.an.f.sur.var)$beta
ad.m.sur.alpha <- estBetaParams(ad.an.m.sur, ad.an.m.sur.var)$alpha
ad.m.sur.beta <- estBetaParams(ad.an.m.sur, ad.an.m.sur.var)$beta

#rescaling variance since can't just divide by 2....need to check this step
fawn.repro.var <- (1/2)^2 * fawn.an.repro.var
juv.repro.var <- (1/2)^2 * juv.an.repro.var
ad.repro.var <- (1/2)^2 * ad.an.repro.var

#Estimate alpha & beta values for the beta distribution of probability of reproducing
fawn.repro.alpha <- estBetaParams(preg.fawn, fawn.repro.var)$alpha
fawn.repro.beta <- estBetaParams(preg.fawn, fawn.repro.var)$beta
juv.repro.alpha <- estBetaParams(preg.juv, juv.repro.var)$alpha
juv.repro.beta <- estBetaParams(preg.juv, juv.repro.var)$beta
ad.repro.alpha <- estBetaParams(preg.ad, ad.repro.var)$alpha
ad.repro.beta <- estBetaParams(preg.ad, ad.repro.var)$beta

hunt.mort.f.mean <- c(hunt.mort.fawn, hunt.mort.juv, rep(hunt.mort.ad.f, (n.age.cats-2)))
hunt.mort.m.mean <- c(hunt.mort.fawn, hunt.mort.juv, rep(hunt.mort.ad.m, (n.age.cats-2)))
hunt.mort.i.f.mean <- c(hunt.mort.fawn, hunt.mort.juv, rep(hunt.mort.ad.f, (n.age.cats-2)))#right now, infectious are harvested at same rate as uninfected
hunt.mort.i.m.mean <- c(hunt.mort.fawn, hunt.mort.juv, rep(hunt.mort.ad.m, (n.age.cats-2)))#right now, infectious are harvested at same rate as uninfected

#Estimate alpha and beta of beta distribution
hunt.mort.f.alpha <- estBetaParams(hunt.mort.f.mean, hunt.mort.f.var)$alpha
hunt.mort.f.beta <- estBetaParams(hunt.mort.f.mean, hunt.mort.f.var)$beta
hunt.mort.m.alpha <- estBetaParams(hunt.mort.m.mean, hunt.mort.m.var)$alpha
hunt.mort.m.beta <- estBetaParams(hunt.mort.m.mean, hunt.mort.m.var)$beta
hunt.mort.i.f.alpha <- estBetaParams(hunt.mort.i.f.mean, hunt.mort.i.f.var)$alpha
hunt.mort.i.f.beta <- estBetaParams(hunt.mort.i.f.mean, hunt.mort.i.f.var)$beta
hunt.mort.i.m.alpha <- estBetaParams(hunt.mort.i.m.mean, hunt.mort.i.m.var)$alpha
hunt.mort.i.m.beta <- estBetaParams(hunt.mort.i.m.mean, hunt.mort.i.m.var)$beta

#stochastic hunting survival rates; right now, it's drawing the hunting mort for each age class
hunt.mort.f <- (rbeta(n.age.cats, hunt.mort.f.alpha, hunt.mort.f.beta, ncp = 0)) # added annual hunting mortality over the entire season for females
hunt.mort.m <- (rbeta(n.age.cats, hunt.mort.m.alpha, hunt.mort.m.beta, ncp = 0))# added annual hunting mortality over the entire season for males
hunt.mort.i.f <- (rbeta(n.age.cats, hunt.mort.i.f.alpha, hunt.mort.i.f.beta, ncp = 0))#hunting mortality associated with infected females - hot-spot removal
hunt.mort.i.m <- (rbeta(n.age.cats, hunt.mort.i.m.alpha, hunt.mort.i.m.beta, ncp = 0))#hunting mortality associated with infected males - hot-spot removal

#bundle them into a list
params <- list(fawn.sur.alpha = fawn.sur.alpha,
               fawn.sur.beta = fawn.sur.beta,
               juv.sur.alpha = juv.sur.alpha,
               juv.sur.beta = juv.sur.beta,
               ad.f.sur.alpha = ad.f.sur.alpha,
               ad.f.sur.beta = ad.f.sur.beta,
               ad.m.sur.alpha = ad.m.sur.alpha,
               ad.m.sur.beta = ad.m.sur.beta,

               fawn.repro.alpha = fawn.repro.alpha,
               fawn.repro.beta = fawn.repro.beta,
               juv.repro.alpha = juv.repro.alpha,
               juv.repro.beta = juv.repro.beta,
               ad.repro.alpha = ad.repro.alpha,
               ad.repro.beta = ad.repro.beta,

               hunt.mort.f.alpha = hunt.mort.f.alpha,
               hunt.mort.m.alpha = hunt.mort.m.alpha,
               hunt.mort.i.f.alpha = hunt.mort.i.f.alpha,
               hunt.mort.i.m.alpha = hunt.mort.i.m.alpha,
               hunt.mort.f.beta = hunt.mort.f.beta,
               hunt.mort.m.beta = hunt.mort.m.beta,
               hunt.mort.i.f.beta = hunt.mort.i.f.beta,
               hunt.mort.i.m.beta = hunt.mort.i.m.beta,

               ini.fawn.prev = ini.fawn.prev,
               ini.juv.prev = ini.juv.prev,
               ini.ad.f.prev = ini.ad.f.prev,
               ini.ad.m.prev = ini.ad.m.prev,

               foi = foi,
               n0 = n0,
               n.years = n.years,
               n.age.cats = n.age.cats,
               p = p)

save(params, file = "./output/params_stoch.RData")


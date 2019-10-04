# script to create some initial parameters.

######PARAMETER VALUES######
n.years <- 20 # number of years for the simulation

#relative risk of hunting a positive case
rel.risk <- 1

# disease mortality parameter
p <- .43 #0.43 #probability of transitioning between infectious box cars

#Natural Annual survival rates
fawn.an.sur <- 0.6
juv.an.sur <- 0.8
ad.an.f.sur <- 0.9
ad.an.m.sur <- 0.85

# annual reproductive rates
fawn.repro <- 0
juv.repro <- 0.6
ad.repro  <- 1.1

fawns.fawn <- ifelse(fawn.repro < 1, 1, 2)
fawns.juv <- ifelse(juv.repro < 1, 1, 2)
fawns.ad <- ifelse(ad.repro < 1, 1, 2)

preg.fawn <- ifelse(fawns.fawn==1, fawn.repro, fawn.repro/2)
preg.juv <- ifelse(fawns.juv==1, juv.repro, juv.repro/2)
preg.ad <- ifelse(fawns.ad==1, ad.repro, ad.repro/2)

n.age.cats <- 12 # age categories
n0 <- 2000 # initial population size

#Initial prevalences; user input
ini.fawn.prev <- 0.05
ini.juv.prev <- 0.05
ini.ad.f.prev <- 0.05
ini.ad.m.prev <- 0.05

#Mean additive hunt mortality; user input
hunt.mort.fawn <- 0.02
hunt.mort.juv.f <- 0.1
hunt.mort.juv.m <- 0.2

hunt.mort.ad.f <- 0.1
hunt.mort.ad.m <- 0.2

#Transmission
env.foi <- 1 - (0.99^(1/12)) # monthly probability of becoming infected
r0 <- 1.1
theta <- 1  # 0 = Density dependent transmission, 1 = Freq. dep. trans.

beta.f = r0 * (n0^(theta-1)) / (mean(apply(cbind(rnbinom(1000, 1, (1 - ad.an.f.sur^(1/12))),
                                    rnbinom(1000, 1, (1 - (1 - hunt.mort.ad.f)^(1/12))),
                                    rgamma(1000, 10, p)), 1, FUN = min)))

beta.m <- 1.5 # transmission rate to males increased 10%

#bundle them into a list
params <- list(fawn.an.sur = fawn.an.sur,
               juv.an.sur = juv.an.sur,
               ad.an.f.sur = ad.an.f.sur,
               ad.an.m.sur = ad.an.m.sur,

               fawn.repro = fawn.repro,
               juv.repro = juv.repro,
               ad.repro = ad.repro,

               hunt.mort.fawn = hunt.mort.fawn,
               hunt.mort.juv.f = hunt.mort.juv.f,
               hunt.mort.juv.m = hunt.mort.juv.m,
               hunt.mort.ad.f = hunt.mort.ad.f,
               hunt.mort.ad.m = hunt.mort.ad.m,

               ini.fawn.prev = ini.fawn.prev,
               ini.juv.prev = ini.juv.prev,
               ini.ad.f.prev = ini.ad.f.prev,
               ini.ad.m.prev = ini.ad.m.prev,

               n.age.cats = n.age.cats,
               p = p,
               env.foi = env.foi,
               beta.f = beta.f,
               beta.m = beta.m,
               theta = theta,
               n0 = n0,
               n.years = n.years,
               rel.risk = rel.risk)

save(params, file = "./output/params_det_v2.RData")

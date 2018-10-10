# script to create some initial parameters.

######PARAMETER VALUES######
#Natural Annual survival rates
fawn.an.sur <- 0.4
juv.an.sur <- 0.6
ad.an.f.sur <- 0.8
ad.an.m.sur <- 0.7

# annual reproductive rates
fawn.repro <- 0
juv.repro <- 0.8
ad.repro  <- 1.7

fawns.fawn <- ifelse(fawn.repro < 1, 1, 2)
fawns.juv <- ifelse(juv.repro < 1, 1, 2)
fawns.ad <- ifelse(ad.repro < 1, 1, 2)

preg.fawn <- ifelse(fawns.fawn==1, fawn.repro, fawn.repro/2)
preg.juv <- ifelse(fawns.juv==1, juv.repro, juv.repro/2)
preg.ad <- ifelse(fawns.ad==1, ad.repro, ad.repro/2)

n.age.cats <- 12 # age categories
n0 <- 2000 # initial population size

#Initial prevalences; user input
ini.fawn.prev <- 0.3
ini.juv.prev <- 0.1
ini.ad.f.prev <- 0.2
ini.ad.m.prev <- 0.2

foi <- 1 - (0.97^(1/12)) # monthly probability of becoming infected

#dis.mort <- 1-((1-0.3)^(1/12)) # additional disease induced mortality rates per month.
p <- 0.43 #probability of transitioning between infectious box cars

#Mean additive hunt mortality; user input
hunt.mort.fawn <- 0.03
hunt.mort.juv <- 0.07
hunt.mort.ad.f <- 0.1
hunt.mort.ad.m <- 0.2

n.years <- 10 # number of years for the simulation

#bundle them into a list
params <- list(fawn.an.sur = fawn.an.sur,
               juv.an.sur = juv.an.sur,
               ad.an.f.sur = ad.an.f.sur,
               ad.an.m.sur = ad.an.m.sur,

               fawn.repro = fawn.repro,
               juv.repro = juv.repro,
               ad.repro = ad.repro,

               hunt.mort.fawn = hunt.mort.fawn,
               hunt.mort.juv = hunt.mort.juv,
               hunt.mort.ad.f = hunt.mort.ad.f,
               hunt.mort.ad.m = hunt.mort.ad.m,

               ini.fawn.prev = ini.fawn.prev,
               ini.juv.prev = ini.juv.prev,
               ini.ad.f.prev = ini.ad.f.prev,
               ini.ad.m.prev = ini.ad.m.prev,

               n.age.cats = n.age.cats,
               p = p,
               foi = foi,
               n0 = n0,
               n.years = n.years)

save(params, file = "./output/params_deterministic.RData")
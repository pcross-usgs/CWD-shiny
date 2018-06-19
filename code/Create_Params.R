# script to create some initial parameters.

######PARAMETER VALUES######
#Natural Annual survival rates
fawn.an.sur <- 0.4
juv.an.sur <- 0.7
ad.an.f.sur <- 0.9
ad.an.m.sur <- 0.8

# annual reproductive rates
fawn.rep <- 0
juv.rep <- 0.8
ad.rep  <- 1.7

fawns.fawn <- ifelse(fawn.rep < 1, 1, 2)
fawns.juv <- ifelse(juv.rep < 1, 1, 2)
fawns.ad <- ifelse(ad.rep < 1, 1, 2)

preg.fawn <- ifelse(fawns.fawn==1, fawn.rep, fawn.rep/2)
preg.juv <- ifelse(fawns.juv==1, juv.rep, juv.rep/2)
preg.ad <- ifelse(fawns.ad==1, ad.rep, ad.rep/2)

n.age.cats <- 12 # age categories
n0 <- 2000 # initial population size
ini.prev <- 0.03 # initial prevalence
foi <- 1 - (0.95^(1/12)) # monthly probability of becoming infected

dis.mort <- 1-((1-0.3)^(1/12)) # additional disease induced mortality rates per month.
p <- 0.43 #probability of transitioning between infectious box cars

hunt.mort.f <- rep(0.1,12) # added annual hunting mortality over the entire season for females
hunt.mort.m <- rep(0.2,12) # added annual hunting mortality over the entire season for males
hunt.mort.i.f <- rep(0.1,12) #hunting mortality associated with infected females - hot-spot removal
hunt.mort.i.m <- rep(0.2,12) #hunting mortality associated with infected males - hot-spot removal

n.years <- 10 # number of years for the simulation

#bundle them into a list
params <- list(fawn.an.sur = fawn.an.sur, juv.an.sur = juv.an.sur,
               ad.an.f.sur = ad.an.f.sur, ad.an.m.sur = ad.an.m.sur,
               fawn.rep = fawn.rep, juv.rep = juv.rep,
               ad.rep = ad.rep,
               dis.mort = dis.mort, ini.prev = ini.prev,
               foi = foi, n0= n0, n.years = n.years,
               hunt.mort.f = hunt.mort.f,
               hunt.mort.m = hunt.mort.m,
               hunt.mort.i.f = hunt.mort.i.f,
               hunt.mort.i.m = hunt.mort.i.m,
               n.age.cats = n.age.cats, p = p)

save(params, file = "./output/params.RData")
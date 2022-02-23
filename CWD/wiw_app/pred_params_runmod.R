## Hypothetical parameters for wolf-elk and cougar-deer systems using the late-stage transmission model, exponential prey selection, a Type II numerical response, and a Type III functional response.

## List of model parameters / objects ((double parentheses indicate symbology in text/Appendix 1)):
# n.predators: initial predator abundance ((P))
# min.predators: minimum predator abundance allowed
# max.predators: maximum predator abundance allowed
# k_max: maximum per capita kill rate ((K))
# k_inflect: inflection point for functional response ((phi))
# n_inflect: inflection point for numerical response ((delta))
# numeric.form: "Type2" or "Type3"
# functional.form: "Type2" or "Type3"
# r/c: rate of increase in prey selection as CWD progresses ((r=exponential, c=linear))
# selection.form: form of increase in prey selection as CWD progresses ((s_ij, options: "exponential","linear", or "equal"))
# beta.f.low / beta.m.low: transmission rate for CWD stages 1-7, which can differ by sex f=female m=male ((beta))
# S: increase in CWD transmission rate for CWD stages 8-10 ((M in Table S1))
# env.foi: force of infection from the environment
# base.juv: baseline relative selection for juveniles by predators ((b_ij))
# base.adult: baseline relative selection for young adults by predators ((b_ij))
# base.old: baseline relative selection for senescent adults by predators ((b_ij))
# p: probability of progression to next CWD stage ((rho))
# stages: total number of CWD stages to include, where 0 is uninfected
# juvs / adults / old: mutually-exclusive age classes defined by prey ages, juv=juvenile, ad=young adults, old=senescent adults
# n.age.cats: total number of age categories ((L)) - note that the model allows individuals to remain in the final age class based on survival rates, so lifespan can slightly exceed the last age class
# "an.sur": annual survival stratified by fawn (or calf, age 0), juvenile (juv, age 1), adult (ad, age 2+), sex (m/f) ((mu))
# "repro": reproductive rate stratified by fawn (or calf, age 0), juvenile (juv, age 1), adult (ad, age 2+) *female only* ((pi))
# "hunt.mort": mortality rate from hunting stratified by age (fawn/calf, juvenile, adult) and sex (m/f) ((sigma))
# "ini.prev": initial CWD prevalence stratified by age (fawn/calf, juvenile, adult) and sex (m/f)
# n0: initial prey abundance ((N))
# n.years: how many years the simulation runs
# theta: frequency / density dependent transmission (1=entirely frequency-dependent, 0=entirely density-dependent) ((theta))
# rel.risk: preference for hunters to kill infected individuals over healthy individuals (1=no difference in risk)


################################## WOLF-ELK ##################################
params <- list(n.predators=70, k_max=1, min.predators=20,
               max.predators=150, r=0.25, k_inflect=3000, n_inflect=2000,
               numeric.form="Type2", functional.form="Type3",
               beta.f.low=0.026, beta.m.low=0.026, S=5, env.foi=0.0, 
               base.juv=3, base.adult=1, base.old=2, p=0.28, 
               selection.form='exponential', stages=c(0:10),
               juvs=c(1:2), adults=c(3:13), old=c(14:18), n.age.cats=18, 
               fawn.an.sur=0.65, juv.an.sur=0.85, ad.an.f.sur=0.9,
               ad.an.m.sur=0.85, fawn.repro=0, juv.repro=0.4, ad.repro=0.85,
               hunt.mort.fawn=0.01, hunt.mort.juv.f=0.03, hunt.mort.juv.m=0.03,
               hunt.mort.ad.f=0.05, hunt.mort.ad.m=0.1, 
               ini.fawn.prev=0.01, ini.juv.prev=0.03, ini.ad.f.prev=0.04, ini.ad.m.prev=0.04,
               n0=10000, n.years=20, theta=1, rel.risk=1.0)

  out <- cwd_detmod_predation_late(params)


################################## COUGAR-DEER ##################################
params <- list(n.predators=40, k_max=3, min.predators=10,
               max.predators=80, r=0.25, k_inflect=3000, n_inflect=2000, 
               numeric.form="Type2", functional.form="Type3",
               beta.f.low=0.028, beta.m.low=0.028, S=7, env.foi=0.0, 
               base.juv=3, base.adult=1, base.old=1.5, p=0.43, 
               selection.form='exponential', stages=c(0:10),
               juvs=c(1:2), adults=c(3:8), old=c(9:10), n.age.cats=10, 
               fawn.an.sur=0.5, juv.an.sur=0.8, ad.an.f.sur=0.9,
               ad.an.m.sur=0.85, fawn.repro=0, juv.repro=0.4, ad.repro=1.2,
               hunt.mort.fawn=0.01, hunt.mort.juv.f=0.02, hunt.mort.juv.m=0.02,
               hunt.mort.ad.f=0.04, hunt.mort.ad.m=0.1,
               ini.fawn.prev=0.01, ini.juv.prev=0.03, ini.ad.f.prev=0.04, ini.ad.m.prev=0.04,
               n0=10000, n.years=20, theta=1, rel.risk=1.0)
out <- cwd_detmod_predation_late(params)
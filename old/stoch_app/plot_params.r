plot.vitals <- function(params){
  require(reshape2)
  require(tidyverse)
  require(ggridges)
#source("estBetaParams.r", local = T)
  # INPUT
  # params = list of the parameters
  # OUTPUT
  # plot of the prevalence
  # First draw some values from the distribution
  sur.fawn <- params %$%
    estBetaParams(fawn.an.sur, fawn.an.sur.var) %$%
    rbeta(1000, alpha, beta)
  sur.juv <- params %$%
    estBetaParams(juv.an.sur, an.sur.var) %$%
    rbeta(1000, alpha, beta)
  sur.ad.f <- params %$%
    estBetaParams(ad.an.f.sur, an.sur.var) %$%
    rbeta(1000, alpha, beta)
  sur.ad.m <- params %$%
    estBetaParams(ad.an.m.sur, an.sur.var) %$%
    rbeta(1000, alpha, beta)
  
  hunt.fawn <- params %$%
    estBetaParams(hunt.mort.fawn, hunt.mort.var) %$%
    rbeta(1000, alpha, beta)
  hunt.juv <- params %$%
    estBetaParams(hunt.mort.juv, hunt.mort.var) %$%
    rbeta(1000, alpha, beta)
  hunt.ad.f <- params %$%
    estBetaParams(hunt.mort.ad.f, hunt.mort.var) %$%
    rbeta(1000, alpha, beta)
  hunt.ad.m <- params %$%
    estBetaParams(hunt.mort.ad.m, hunt.mort.var) %$%
    rbeta(1000, alpha, beta)
  
  sur.tot.fawn <- sur.fawn*(1 - hunt.fawn)
  sur.tot.juv <- sur.juv*(1 - hunt.juv)
  sur.tot.ad.f <- sur.ad.f*(1 - hunt.ad.f)
  sur.tot.ad.m <- sur.ad.m*(1 - hunt.ad.m)
  
  repro.juv <-  params %$%
    estBetaParams(juv.repro/2, juv.repro.var) %$%
    rbeta(1000, alpha, beta) * 2
  repro.ad <-  params %$%
    estBetaParams(ad.repro/2, ad.repro.var) %$%
    rbeta(1000, alpha, beta) * 2
  
  #create a wide data.frame
  params.stoch <- data.frame(sur.tot.fawn = sur.tot.fawn, sur.tot.juv = sur.tot.juv,
    sur.tot.ad.f = sur.tot.ad.f, sur.tot.ad.m = sur.tot.ad.m,
    repro.juv = repro.juv, repro.ad = repro.ad)
  
  params.stoch.2 <-  params.stoch %>%
    gather('sur.tot.fawn', 'sur.tot.juv', 'sur.tot.ad.f',
      'sur.tot.ad.m', 'repro.juv', 'repro.ad',
      key = "parameter", value = "value")
  
  # plot the params
  theme_set(theme_bw())
  # plot
  ggplot(params.stoch.2, aes(x = value, y = parameter)) +
    geom_density_ridges() + theme_ridges() + ylab("") +
    scale_y_discrete(labels = c("reproduction adult",
                                "reproduction juvenile",
                                "survival female",
                                "survival male",
                                "survival fawn",
                                "survival juvenile"))
  }



plot.ttd <- function(p){
  #time to death: ttd
  tmp <- data.frame(years.to.death = rgamma(1000, 10, p)/12)
  
  theme_set(theme_bw())
  ggplot(tmp, aes(x = years.to.death)) +
    geom_density(fill = "grey") + theme_ridges() +
    labs(x = "years", y = "density",
         title = "Time until disease induced mortality")
}

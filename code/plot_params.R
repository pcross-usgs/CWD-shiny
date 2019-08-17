plot_vitals <- function(params){
  require(reshape2)
  require(tidyverse)
  require(ggridges)
  # INPUT
  # params = list of the parameters
  # OUTPUT
  # plot of the prevalence
  # First draw some values from the distribution
  sur.fawn <- params %$%
    est_beta_params(fawn.an.sur, fawn.an.sur.var) %$%
    rbeta(1000, alpha, beta)
  sur.juv <- params %$%
    est_beta_params(juv.an.sur, an.sur.var) %$%
    rbeta(1000, alpha, beta)
  sur.ad.f <- params %$%
    est_beta_params(ad.an.f.sur, an.sur.var) %$%
    rbeta(1000, alpha, beta)
  sur.ad.m <- params %$%
    est_beta_params(ad.an.m.sur, an.sur.var) %$%
    rbeta(1000, alpha, beta)

  hunt.fawn <- params %$%
    est_beta_params(hunt.mort.fawn, hunt.mort.var) %$%
    rbeta(1000, alpha, beta)
  hunt.juv.m <- params %$%
    est_beta_params(hunt.mort.juv.f, hunt.mort.var) %$%
    rbeta(1000, alpha, beta)
  hunt.juv.f <- params %$%
    est_beta_params(hunt.mort.juv.m, hunt.mort.var) %$%
    rbeta(1000, alpha, beta)
  hunt.ad.f <- params %$%
    est_beta_params(hunt.mort.ad.f, hunt.mort.var) %$%
    rbeta(1000, alpha, beta)
  hunt.ad.m <- params %$%
    est_beta_params(hunt.mort.ad.m, hunt.mort.var) %$%
    rbeta(1000, alpha, beta)

  sur.tot.fawn <- sur.fawn*(1 - hunt.fawn)
  sur.tot.juv.f <- sur.juv*(1 - hunt.juv.f)
  sur.tot.juv.m <- sur.juv*(1 - hunt.juv.m)
  sur.tot.ad.f <- sur.ad.f*(1 - hunt.ad.f)
  sur.tot.ad.m <- sur.ad.m*(1 - hunt.ad.m)

  repro.juv <-  params %$%
    est_beta_params(juv.repro/2, juv.repro.var) %$%
    rbeta(1000, alpha, beta) * 2
  repro.ad <-  params %$%
    est_beta_params(ad.repro/2, ad.repro.var) %$%
    rbeta(1000, alpha, beta) * 2

  #create a wide data.frame
  params.stoch <- data.frame(sur.tot.fawn = sur.tot.fawn,
                             sur.tot.juv.f = sur.tot.juv.f,
                             sur.tot.juv.m = sur.tot.juv.m,
                             sur.tot.ad.f = sur.tot.ad.f,
                             sur.tot.ad.m = sur.tot.ad.m,
                             repro.juv = repro.juv,
                             repro.ad = repro.ad)

  params.stoch.2 <-  params.stoch %>%
    gather('sur.tot.fawn', 'sur.tot.juv.f', 'sur.tot.juv.m','sur.tot.ad.f',
      'sur.tot.ad.m', 'repro.juv', 'repro.ad',
      key = "parameter", value = "value")

  # plot the params
  theme_set(theme_bw(base_size = 18))
  # plot
  ggplot(params.stoch.2, aes(x = value, y = parameter)) +
    geom_density_ridges() + theme_ridges() + ylab("") +
    scale_y_discrete(labels = c("reproduction adult",
                                "reproduction juvenile",
                                "survival female",
                                "survival male",
                                "survival fawn",
                                "survival juvenile male",
                                "survival juvenile female"))
  }



plot_ttd <- function(p){
  #time to death: ttd
  tmp <- data.frame(years.to.death = rgamma(1000, 10, p)/12)

  theme_set(theme_bw(base_size = 18))
  ggplot(tmp, aes(x = years.to.death)) +
    geom_density(fill = "grey") +
    theme_ridges() +
    labs(x = "Years", y = "Density",
         title = "Time until disease induced mortality")
}


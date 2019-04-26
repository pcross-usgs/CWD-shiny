# Script to run the stochastic model
rm(list = ls())
library(popbio)
library(reshape2)
library(tidyverse)
source("./code/stoch_model_fxn.r")
source("./code/plot_stoch_fxns.r")
source("./code/plot_params.r")
source("./code/estBetaParams.r")
load("./output/params_stoch.RData")

#Run the model
sims <- 10
counts.sims <- vector("list", sims)
deaths.sims <- vector("list", sims)

for(i in 1:sims){
  out <- stoch.pop.model(params)
  counts.sims[[i]] <- out$counts
  deaths.sims[[i]] <- out$deaths
}

counts <- melt(counts.sims,
                         id = c("age", "month", "population", "category",
                                            "year", "sex", "disease")) %>%
                          rename(sim = L1)

deaths <- melt(deaths.sims,
                         id = c("age", "month", "population", "category",
                                             "year", "sex")) %>% rename(sim = L1)

out <- list(counts = counts,deaths = deaths)
#plot the totals
plot.stoch.tots(out$counts, all.lines = T, error.bars = c(0.25, 0.75),
                by.sexage = T)

#plot the prevalence
plot.stoch.prev(out$counts, all.lines = T, error.bars = TRUE, cis = c(0.25, 0.75))

# prev by age
plot.stoch.prev.age(out$counts, by.sex = T)

#plot fawn.adult and buck:doe
library(cowplot)
p1 <- plot.stoch.fawn.adult(out$counts, all.lines = T, error.bars = c(0.05, 0.95))
p2 <- plot.stoch.buck.doe(out$counts, all.lines = T, error.bars = c(0.05, 0.95))
plot_grid(p1, p2, labels = c("A", "B"))

# plot deaths by type
plot.stoch.deaths(out$deaths, error.bars = c(0.05, 0.95))
plot.stoch.perc.deaths(out$deaths, error.bars = c(0.05, 0.95))

# plot the params
plot.vitals(params)
# plot time to death
plot.ttd(params$p)

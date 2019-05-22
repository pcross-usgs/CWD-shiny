# Script to run the stochastic model
rm(list = ls())
library(popbio)
library(reshape2)
library(magrittr)
library(tidyverse)
source("./code/stoch_model_fxn_ver2.r")
source("./code/plot_stoch_fxns.r")
source("./code/plot_params.r")
source("./code/estBetaParams.r")
source("./code/plot_compare_scenarios.r")
load("./output/params_stoch_ver2.RData")

#Run the model
sims <- 20
counts.sims <- vector("list", sims)
deaths.sims <- vector("list", sims)

for(i in 1:sims){
  outa <- stoch.pop.model.2(params)
  counts.sims[[i]] <- outa$counts
  deaths.sims[[i]] <- outa$deaths
}

counts <- melt(counts.sims,
                         id = c("age", "month", "population", "category",
                                            "year", "sex", "disease")) %>%
                          rename(sim = L1)

deaths <- melt(deaths.sims,
                         id = c("age", "month", "population", "category",
                                             "year", "sex")) %>% rename(sim = L1)

outa <- list(counts = counts,deaths = deaths)
#plot the totals
plot.stoch.tots(outa$counts, all.lines = T, error.bars = c(0.05, 0.95),
                by.sexage = T)

plot.stoch.tots.2(outa$counts, error.bars = c(0.05, 0.95))
#plot the prevalence
plot.stoch.prev(outa$counts, all.lines = T, error.bars = TRUE, cis = c(0.05, 0.95))

# prev by age
plot.stoch.prev.age(outa$counts, by.sex = T)
plot.stoch.prev.age.2(outa$counts, error.bars = c(0.05, 0.95))

#plot fawn.adult and buck:doe
library(cowplot)
p1 <- plot.stoch.fawn.adult(outa$counts, all.lines = T, error.bars = c(0.05, 0.95))
p2 <- plot.stoch.buck.doe(outa$counts, all.lines = T, error.bars = c(0.05, 0.95))
plot_grid(p1, p2, labels = c("A", "B"))

# plot deaths by type
plot.stoch.deaths(outa$deaths, error.bars = c(0.05, 0.95))
plot.stoch.perc.deaths(outa$deaths, error.bars = c(0.05, 0.95))

# plot the params
plot.vitals(params)
# plot time to death
plot.ttd(params$p)
# plot the age distribution
plot.stoch.age.dist(outa$counts)

#####################
# run it again for comparison plot testing
sims <- 20
counts.sims <- vector("list", sims)
deaths.sims <- vector("list", sims)

for(i in 1:sims){
  outb <- stoch.pop.model.2(params)
  counts.sims[[i]] <- outb$counts
  deaths.sims[[i]] <- outb$deaths
}

counts <- melt(counts.sims,
               id = c("age", "month", "population", "category",
                      "year", "sex", "disease")) %>%
  rename(sim = L1)

deaths <- melt(deaths.sims,
               id = c("age", "month", "population", "category",
                      "year", "sex")) %>% rename(sim = L1)

outb <- list(counts = counts, deaths = deaths)
##############


dat <- list(outa$counts, outb$counts)

dat <- melt(dat, id = c("age", "month", "population", "category",
                        "year", "sex", "disease", "sim")) %>%
  filter(month %% 12 == 10, round(year, 0) == max(round(year, 0))) %>%
  rename(scenario = L1) %>%
  mutate(scenario = fct_recode(as.factor(scenario), A = "1", B = "2")) %>%
  group_by(sim, scenario) %>%
  summarize(n = sum(population)) %>%
  spread(key = scenario, value = n)
dat$comp <- dat$A - dat$B
length(which(dat$comp > 0 ))

# plot the comparisons
plot.compare.tots(outa$counts, outb$counts)
plot.compare.prev(outa$counts, outb$counts)

plot.compare.hunted(outa$deaths, outb$deaths)
plot.compare.hunted.end(outa$deaths, outb$deaths)

plot.compare.buckshunted(outa$deaths, outb$deaths)
plot.compare.buckshunted.end(outa$deaths, outb$deaths)

plot.compare.oldbuckshunted(outa$deaths, outb$deaths)
plot.compare.oldbuckshunted.end(outa$deaths, outb$deaths)

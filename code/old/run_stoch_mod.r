# Script to run the stochastic model
rm(list = ls())
setwd("D:/Current Projects/CWD_shiny")
library(profvis)
#profvis({

library(popbio)
library(reshape2)
library(magrittr)
library(tidyverse)
library(cowplot)

source("./app/cwd_stoch_model_fxn.r")
source("./app/plot_stoch_fxns.r")
source("./app/plot_params.r")
source("./app/est_beta_params.r")
source("./app/plot_compare_scenarios.r")
source("./code/create_stoch_params.r")

setwd("./app/")

#Run the model
sims <- 20
counts.sims <- vector("list", sims)
deaths.sims <- vector("list", sims)

for(i in 1:sims){
  outa <- cwd_stoch_model(stoch.params)
  counts.sims[[i]] <- outa$counts
  deaths.sims[[i]] <- outa$deaths
}

counts <- melt(counts.sims, id = c("age", "month", "population", "category",
                                   "year", "sex", "disease")) %>%
              rename(sim = L1)

deaths <- melt(deaths.sims, id = c("age", "month", "population", "category",
                                   "year", "sex")) %>% rename(sim = L1)

outa <- list(counts = counts,deaths = deaths)
#plot the totals
plot_stoch_tots(outa$counts, all.lines = T, error.bars = c(0.05, 0.95),
                by.sexage = T)

plot_stoch_disease(outa$counts, error.bars = c(0.05, 0.95))

#plot the prevalence
plot_stoch_prev(outa$counts, all.lines = T, error.bars = TRUE, cis = c(0.05, 0.95))

# prev by age
plot_stoch_prev_age(outa$counts, by.sex = T)
plot_stoch_prev_age_end(outa$counts, error.bars = c(0.05, 0.95))

#plot fawn.adult and buck:doe
p1 <- plot_stoch_fawn_doe(outa$counts, all.lines = T, error.bars = c(0.05, 0.95))
p2 <- plot_stoch_buck_doe(outa$counts, all.lines = T, error.bars = c(0.05, 0.95))
plot_grid(p1, p2, labels = c("A", "B"))

# plot deaths by type
plot_stoch_deaths(outa$deaths, error.bars = c(0.05, 0.95))
plot_stoch_perc_deaths(outa$deaths, error.bars = c(0.05, 0.95))

# plot the params
plot_vitals(stoch.params)
# plot time to death
plot_ttd(stoch.params$p)
# plot the age distribution
plot_stoch_age_dist(outa$counts)

#####################
# run it again for comparison plot testing
stoch.params.b <- stoch.params
stoch.params.b$hunt.mort.ad.m <- 0.5

counts.sims <- vector("list", sims)
deaths.sims <- vector("list", sims)

for(i in 1:sims){
  outb <- cwd_stoch_model(stoch.params.b)
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

# plot the comparisons
plot_compare_all(outa, outb)

#})


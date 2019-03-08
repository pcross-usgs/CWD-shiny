# Script to plot the parameters of the stochastic model
#NOT WORKING
rm(list = ls())
library(reshape2)
library(tidyverse)

load("./output/params_stoch.RData")
source("./code/estBetaParams.r")
# First draw some distribution and create a data.frame from the params list

params.stoch.df <- data.frame(parameter = NA, value = NA)
x <- estBetaParams(params$fawn.an.sur, params$fawn.an.sur.var) %>% rbeta(1000, alpha, beta)


fawn.sur <- rbeta(1000, alpha, beta)
juv.sur <- rbeta(1000, x$alpha, x$beta)

params.stoch.df <- params.stoch.df %>%
                    mutate(paramete)
plot(seq(0,1,0.01), dbeta(seq(0,1,.01), x$alpha, x$beta))

# plot the params
theme_set(theme_bw())

# plot
g <- ggplot(mpg, aes(class, cty))
g + geom_violin() +
  labs(title="Violin plot",
       subtitle="City Mileage vs Class of vehicle",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")


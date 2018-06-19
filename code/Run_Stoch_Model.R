# Script to run the stochastic model
rm(list = ls())
library(popbio)
library(reshape2)
library(tidyverse)
source("./code/stoch_pop_model_fxn.r")
load("./output/params.RData")

#Run the model
sims <- 2
out.sims <- vector("list",sims)

for(i in 1:sims){
  out.sims[[i]] <- stoch.pop.model(params)
}

out.sims.long <- melt(out.sims) %>%
  rename(age = Var1, month = Var2, population = value,
         category = L2, sim = L1) %>%
  mutate(year = (month - 1) / 12, sex = as.factor(str_sub(category, -1)),
         disease = "no")
out.sims.long$disease[str_sub(out.sims.long$category, 1,1) == "I"] = "yes"
out.sims.long$disease <- as.factor(out.sims.long$disease)
summary(out.sims.long)

# Script to run the stochastic model
rm(list = ls())
library(popbio)
library(reshape2)
library(tidyverse)
source("./code/stoch_pop_model_fxn.r")
source("./code/plot_stoch_fxns.r")

load("./output/params.RData")

#Run the model
sims <- 30
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

#save(out.sims.long, file = "./output/out.RData")

#plot the totals
plot.stoch.tots(out.sims.long, all.lines = T, error.bars = c(0.25, 0.75),
                by.sexage = T)

# prev by age
plot.stoch.prev.age(out.sims.long, by.sex = T)

#plot fawn.adult and buck:doe
library(cowplot)
p1 <- plot.stoch.fawn.adult(out.sims.long, all.lines = T, error.bars = c(0.05, 0.95))
p2 <- plot.stoch.buck.doe(out.sims.long, all.lines = T, error.bars = c(0.05, 0.95))
plot_grid(p1, p2, labels = c("A", "B"))

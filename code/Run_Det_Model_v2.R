# Deterministic Discrete Annual age and sex structured model
rm(list = ls())
library(popbio)
library(tidyverse)
library(reshape2)
source("./code/det_pop_model_fxn_ver2.r")
source("./code/plot_fxns.r")
load("./output/params_det_v2.RData")

##RUN THE model
out <- det.pop.model.v2(params)

out.long <- melt(out) %>%
  rename(age = Var1, month = Var2, population = value,
         category = L1) %>%
  mutate(year = (month - 1) / 12, sex = as.factor(str_sub(category, -1)),
         disease = "no")
out.long$disease[str_sub(out.long$category, 1,1) == "I"] = "yes"
out.long$disease <- as.factor(out.long$disease)

#PLOT the results
plot.tots(out.long, type = "l", ylab = "Total population", xlab = "Year",
          ylim = c(0,10000), lwd = 3,
          cex = 1.25, cex.lab = 1.25, cex.axis = 1.25)

#plot.all(out.long)

# prevalence plot over time.
plot.prev(out.long, type = "l", col = "red", xlab = "year", ylab = "prevalence")

# prevalence plot by age
#not sure if this is working
plot.prev.age(out.long, by.sex = T)

#plot the fawn to adult ratio
plot.fawn.adult(out.long, type = "l", xlab = "year", ylab = "fawn:adult")
plot.buck.doe(out.long, type = "l", xlab = "year", ylab = "buck:doe")

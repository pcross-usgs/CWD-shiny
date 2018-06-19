# Functions to plot the output from a multiple stochastic simulations

plot.stoch.tots <- function(dat, ...){
  # INPUT
  # dat = data.frame with columns of
  # age = numeric
  # month = numeric index
  # population = numeric
  # category = text
  # sim = number of simulation
  # year = year of the simulation (numeric)
  # sex = f/m (factor)
  # disease = yes/no (factor)
  # OUTPUT
  # plot of the population totals split by age.
  require(tidyverse)

  # summarize by year and sex
  dat.sum <- dat %>%
    filter(month %% 12 == 1) %>%
    group_by(year, disease) %>%
    summarize(n = sum(population)) %>%
    spread(key = disease, value = n) %>%
    mutate(tots = no + yes)

  #ggplot
  #STOPPED HERE

}

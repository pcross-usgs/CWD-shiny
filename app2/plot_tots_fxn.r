# Functions to plot the output from a single simulation
plot.tots <- function(dat, ...){
  # INPUT
  # dat = list of the output matrices
  # OUTPUT
  # plot of the population totals split by age.
  attach(dat)

  # Calculate the sums from the output
  #MAY NEED TO CHANGE THIS SO THAT IT HANDLES VARIABLE #s and names of categories
  N <- colSums(St.f + St.m + It.f + It.m)
  N.f <- colSums(St.f + It.f)
  N.m <- colSums(St.m + It.m)

  # get the indices for the start of each year
  yr.indices <- which(seq(1,length(N)) %% 12 == 1)

  #plot
  par(mar = c(4,5, 1,1))
  plot((yr.indices-1)/12, N[yr.indices], ...)
  lines((yr.indices-1)/12, N.f[yr.indices], col = "red", lwd = 2)
  lines((yr.indices-1)/12, N.m[yr.indices], col = "blue", lwd = 2)

  detach(dat)
}

# plot all ages all months
plot.all <- function(dat, ...){
  # INPUT
  # dat = list of the output matrices
  # OUTPUT
  # plot of the population totals split by age, sex, prevalence
  require(reshape2)
  require(ggplot2)

  dat.long <- melt(dat)
  names(dat.long) <- c("age", "month", "population", "category")
  dat.long$category <- as.factor(dat.long$category)

  plot1 <- ggplot(dat.long, aes(month, population, color = age)) +
    geom_point() + facet_wrap(~category, ncol = 4)
  plot1
}

# plot all ages only the start of the years
plot.all.yr <- function(dat){

  # INPUT
  # dat = list of the output matrices
  # OUTPUT
  # plot of the population totals split by age, sex, prevalence
  require(reshape2)
  require(tidyverse)

  # create the indices for the start of each year
  yr.indices <- which(seq(1,dim(dat[[1]])[2]) %% 12 == 1)

  # organize the data into long form
  dat2 <- melt(dat) %>%
   dplyr::rename(age = Var1, month = Var2, population = value,
                        category = L1) %>%
    filter(month %in% yr.indices) %>%
    arrange(category, age, month)
  dat2$category <- as.factor(dat2$category)
  dat2$year <- (dat2$month-1)/12
  #plot it
  plot1 <- ggplot(dat2, aes(year, population, color = age)) +
    geom_point() + facet_wrap(~category, ncol = 4)
  plot1
}
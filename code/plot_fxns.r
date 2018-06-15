# Functions to plot the output from a single simulation
plot.tots <- function(dat, ...){
  # INPUT
  # dat = list of the output matrices
  # OUTPUT
  # plot of the population totals split by age.
  require(reshape2)
  require(tidyverse)

  # create the indices for the start of each year
  indices <- which(seq(1,dim(dat[[1]])[2]) %% 12 == 1)

  # organize the data into long form
  dat.lon <- melt(dat) %>%
    dplyr::rename(age = Var1, month = Var2, population = value,
                  category = L1) %>%
    filter(month %in% indices) %>%
    mutate(year = (month-1)/12, sex = as.factor(str_sub(category, -1)))

  # summarize by year and sex
  dat.sum <- dat.lon %>%
    group_by(year, sex) %>%
    summarize(n = sum(population)) %>%
    spread(key = sex, value = n) %>%
    mutate(tots = f + m)

  #plot
  par(mar = c(4,5, 1,1))
  plot(dat.sum$year, dat.sum$tots, ...)
  lines(dat.sum$year, dat.sum$f, col = "red", lwd = 2)
  lines(dat.sum$year, dat.sum$m, col = "blue", lwd = 2)

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
  # plot of the population totals split by age, sex, disease category
  require(reshape2)
  require(tidyverse)

  # create the indices for the start of each year
  yr.indices <- which(seq(1,dim(dat[[1]])[2]) %% 12 == 1)

  # organize the data into long form
  dat.lon <- melt(dat) %>%
   dplyr::rename(age = Var1, month = Var2, population = value,
                        category = L1) %>%
    filter(month %in% yr.indices) %>%
    arrange(category, age, month)
  dat.lon$category <- as.factor(dat.lon$category)
  dat.lon$year <- (dat.lon$month-1)/12

  #plot it
  plot1 <- ggplot(dat.lon, aes(year, population, color = age)) +
    geom_point() + facet_wrap(~category, ncol = 4)
  plot1
}

# plot the prevalence
plot.prev <- function(dat, ...){
  # INPUT
  # dat = list of the output matrices
  # OUTPUT
  # plot of the prevalence
  require(reshape2)
  require(tidyverse)

  # create the indices for the start of each year
  indices <- which(seq(1,dim(dat[[1]])[2]) %% 12 == 1)

  # organize the data into long form
  dat.lon <- melt(dat) %>%
    dplyr::rename(age = Var1, month = Var2, population = value,
                  category = L1) %>%
    filter(month %in% indices) %>%
    arrange(category, age, month) %>%
    mutate(year = (month-1)/12, sex = as.factor(str_sub(category, -1)),
           disease = "no")
  dat.lon$disease[str_sub(dat.lon$category, 1,1) == "I"] = "yes"

  # summarize by year and disease status, calculate the prevalence
  dat.sum <- dat.lon %>%
    group_by(year, disease) %>%
    summarize(n = sum(population)) %>%
    spread(key = disease, value = n) %>%
    mutate(prev = yes/ (no + yes))

  plot(dat.sum$year, dat.sum$prev, ...)
}

# plot the doe:fawn
plot.fawn.doe <- function(dat, ...){
  # INPUT
  # dat = list of the output matrices
  # OUTPUT
  # plot of the prevalence
  require(reshape2)
  require(tidyverse)

  # create the indices for the start of each year
  indices <- which(seq(1, dim(dat[[1]])[2]) %% 12 == 1) # may of every year

  # organize the data into long form
  dat.lon <- melt(dat) %>%
    dplyr::rename(age = Var1, month = Var2, population = value,
                  category = L1) %>%
    filter(month %in% indices) %>%
    mutate(year = (month-1)/12, sex = as.factor(str_sub(category, -1)))

  dat.lon$age.cat <- "adult"
  dat.lon$age.cat[dat.lon$age == 1] <- "fawn"


  # summarize by year and disease status, calculate the prevalence
  dat.sum <- dat.lon %>%
    group_by(year, age.cat) %>%
    summarize(n = sum(population)) %>%
    spread(key = age.cat, value = n) %>%
    mutate(fawn.doe = fawn / adult)

  plot(dat.sum$year, dat.sum$fawn.doe, ...)

}


# plot buck:doe
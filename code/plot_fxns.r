# Functions to plot the output from a single simulation

#parameter beta plot
pl.beta <- function(a,b, asp = if(isLim) 1, ylim = if(isLim) c(0,1.1)) {
  if(isLim <- a == 0 || b == 0 || a == Inf || b == Inf) {
    eps <- 1e-10
    x <- c(0, eps, (1:7)/16, 1/2+c(-eps,0,eps), (9:15)/16, 1-eps, 1)
  } else {
    x <- seq(0, 1, length = 1025)
  }
  fx <- cbind(dbeta(x, a,b))
  f <- fx; f[fx == Inf] <- 1e100
  matplot(x, f, ylab="", type="l", ylim=ylim, asp=asp,
          main = sprintf("[dpq]beta(x, a=%g, b=%g)", a,b))
  abline(0,1,     col="gray", lty=3)
  abline(h = 0:1, col="gray", lty=3)
  legend("top", paste0(c("d","p","q"), "beta(x, a,b)"),
         col=1:3, lty=1:3, bty = "n")
  invisible(cbind(x, fx))
}

plot.tots <- function(dat, ...){
  # INPUT
  # dat = list of the output matrices
  # OUTPUT
  # plot of the population totals split by age.
  require(reshape2)
  require(tidyverse)

    # summarize by year and sex
  dat.sum <- dat %>%
    filter(month %% 12 == 10) %>%
    group_by(year, disease) %>%
    summarize(n = sum(population)) %>%
    spread(key = disease, value = n) %>%
    mutate(tots = no + yes)

  #plot
  par(mar = c(4,5, 1,1))
  plot(dat.sum$year, dat.sum$tots, ...)
  lines(dat.sum$year, dat.sum$yes, col = "red", lwd = 2)
  lines(dat.sum$year, dat.sum$no, col = "blue", lwd = 2)
  legend("topright", c("total", "infected", "healthy"),
         col = c("black", "red", "blue"), lwd = 2)
}

# plot all ages all months
plot.all <- function(dat, ...){
  # INPUT
  # dat = one matrix in long form
  # OUTPUT
  # plot of the population totals split by age, sex, prevalence
  require(reshape2)
  require(ggplot2)

  dat.lon <- dat %>% filter(month %% 12 == 10)

  plot1 <- ggplot(dat.lon, aes(year, population, color = age)) +
    geom_point() + facet_wrap(~disease + sex, ncol = 2, scales = "free")
  plot1
}

# plot the prevalence
plot.prev <- function(dat, ...){
  # INPUT
  # dat = longform data matrix
  # OUTPUT
  # plot of the prevalence
  require(reshape2)
  require(tidyverse)

  # summarize by year and disease status, calculate the prevalence
  dat.sum <- dat %>%
    filter(month %% 12 == 10) %>%
    group_by(year, disease) %>%
    summarize(n = sum(population)) %>%
    spread(key = disease, value = n) %>%
    mutate(prev = yes/ (no + yes))

  plot(dat.sum$year, dat.sum$prev, ...)
}

# plot the prevalence
plot.prev.age <- function(dat, by.sex, ...){
  # INPUT
  # dat = list of the output matrices
  # by.sex = if True then facet by sex
  #
  # OUTPUT
  # plot of the prevalence
  require(reshape2)
  require(tidyverse)

  if(missing(by.sex)){by.sex <- F}

  # summarize by year and disease status, calculate the prevalence
  if(by.sex == F){
    dat.sum <- dat %>%
      filter(month %% 12 == 10) %>%
      group_by(year, age, disease) %>%
      summarize(n = sum(population)) %>%
      spread(key = disease, value = n) %>%
      mutate(prev = yes/ (no + yes))
  }

  if(by.sex == T){
    dat.sum <- dat %>%
      filter(month %% 12 == 10) %>%
      group_by(year, age, sex, disease)%>%
      summarize(n = sum(population)) %>%
      spread(key = disease, value = n) %>%
      mutate(prev = yes/ (no + yes))
  }

  #create the plot
  if(by.sex == T){
    p <- ggplot(dat.sum, aes(year, prev, group = age, color = age)) +
    geom_line() + facet_wrap(~sex)
  }
  if(by.sex == F){
    p <- ggplot(dat.sum, aes(year, prev, group = age, color = age)) +
      geom_line()
  }
  p <- p + theme_light() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
  p
}

# plot the fawn:adult
plot.fawn.adult <- function(dat, ...){
  # INPUT
  # dat = list of the output matrices
  # OUTPUT
  # plot of the prevalence
  require(reshape2)
  require(tidyverse)

  dat$age.cat <- "adult"
  dat$age.cat[dat$age == 1] <- "fawn"

  # summarize by year and disease status, calculate the prevalence
  dat.sum <- dat %>%
    filter(month %% 12 == 11) %>%
    group_by(year, age.cat) %>%
    summarize(n = sum(population)) %>%
    spread(key = age.cat, value = n) %>%
    mutate(fawn.adult = fawn / adult)

  plot(dat.sum$year, dat.sum$fawn.adult, ...)

}

# plot buck:doe
plot.buck.doe <- function(dat, ...){
  # INPUT
  # dat = list of the output matrices
  # OUTPUT
  # plot of the prevalence
  require(reshape2)
  require(tidyverse)

  dat$age.cat <- "adult"
  dat$age.cat[dat$age == 1] <- "fawn"

  # summarize by year and disease status, calculate the prevalence
  dat.sum <- dat %>%
    filter(month %% 12 == 8) %>% # december of every year
    group_by(year, sex, age.cat) %>%
    summarize(n = sum(population)) %>%
    unite(sex.age, sex, age.cat) %>%
    spread(key = sex.age, value = n) %>%
    mutate(buck.doe = m_adult / f_adult)

  plot(dat.sum$year, dat.sum$buck.doe, ...)

}

# Functions to plot the output from a multiple stochastic simulations

plot.stoch.tots <- function(dat, all.lines, error.bars, by.sexage, ...){
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
  #
  # all_lines = TRUE (include all simulations)
  # error bars = vector of high and low percentiles
  # by.sexage = facet by sex and age?
  #
  # OUTPUT
  # plot of the population totals split by age.
  require(tidyverse)

  if(missing(all.lines)){all.lines = T}

  if(missing(by.sexage)){by.sex = F}

  # summarize the data

  if(by.sexage == T){
    dat$age.cat <- "adult"
    dat$age.cat[dat$age == 1] <- "fawn"

    dat.sum <- dat %>%
      filter(month %% 12 == 10) %>%
      group_by(year, age.cat, sex, sim) %>%
      summarize(n = sum(population)) %>%
      unite(sex.age, sex, age.cat) %>%
      arrange(sim, year)

    # calculate the mean
    dat.mean <- dat.sum %>%
      group_by(year, sex.age) %>%
      summarize(avg = mean(n))

      if(missing(error.bars) == F){# calculate the error bars
        dat.errors <- dat.sum %>%
          group_by(year, sex.age) %>%
          summarize(lo = quantile(n, error.bars[1]),
                    hi = quantile(n, error.bars[2]))
      }
  }

  if(by.sexage == F){
    dat.sum <- dat %>%
      filter(month %% 12 == 10) %>%
      group_by(year, sim) %>%
      summarize(n = sum(population)) %>%
      arrange(sim, year)

    # calculate the mean
    dat.mean <- dat.sum %>%
      group_by(year) %>%
      summarize(avg = mean(n))

    if(missing(error.bars) == F){# calculate the error bars
      dat.errors <- dat.sum %>%
        group_by(year) %>%
        summarize(lo = quantile(n, error.bars[1]),
                  hi = quantile(n, error.bars[2]))
    }
  }

  if(all.lines == TRUE){
    p <- ggplot(data = dat.sum, aes(x = year, y = n, group = sim)) +
      geom_line(color = "grey") +
      geom_line(data = dat.mean, aes(x = year, y = avg, group = NULL), size = 1.5)
  }

  if(all.lines == FALSE){
    p <- ggplot(data = dat.mean, aes(x = year, y = avg, group = NULL)) +
      geom_line(size = 1.5)
    }

  if(missing(error.bars) == FALSE){

    p <- p + geom_line(data = dat.errors, aes(x = year, y = lo, group = NULL),
              linetype = "dashed", color = "red") +
              geom_line(data = dat.errors, aes(x = year, y = hi, group = NULL),
              linetype = "dashed", color = "red")
  }


  if(by.sexage == T){
    p <- p + facet_wrap(~ sex.age)
  }

  # adjust the theme
  p <- p + xlab("Year") + ylab("Population") + theme_light() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())

  p
}

# plot the prevalence by age over time
plot.stoch.prev.age <- function(dat, by.sex, ...){
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
  #
  # by.sex = TRUE....facet by sex
  # OUTPUT
  # plot of the prevalence
  require(reshape2)
  require(tidyverse)

  if(missing(by.sex)){by.sex = F}

  if(by.sex == F){
    # summarize by year and sex
    dat.sum <- dat %>%
      filter(month %% 12 == 7) %>%
      group_by(year, age, disease, sim) %>%
      summarize(n = sum(population)) %>%
      arrange(sim, year) %>%
      spread(key = disease, value = n) %>%
      mutate(prev = yes/ (no + yes))

    # calculate the mean
    dat.mean <- dat.sum %>%
      group_by(age, year) %>%
      summarize(avg = mean(prev))

    p <- ggplot(dat.mean, aes(year, avg, group = age, color = age)) +
      geom_line()
  }

  if(by.sex == T){
    # summarize by year and sex
    dat.sum <- dat %>%
      filter(month %% 12 == 7) %>%
      group_by(year, age, sex, disease, sim) %>%
      summarize(n = sum(population)) %>%
      spread(key = disease, value = n) %>%
      mutate(prev = yes/ (no + yes))

    # calculate the mean
    dat.mean <- dat.sum %>%
      group_by(age, sex, year) %>%
      summarize(avg = mean(prev))

    p <- ggplot(dat.mean, aes(year, avg, group = age, color = age)) +
      geom_line() + facet_wrap(~sex)

  }

  p <- p + xlab("Year") + ylab("Prevalence") +
    theme_light() + theme(panel.grid.minor = element_blank(),
                        panel.grid.major.x = element_blank())
  p
}

# plot the prevalence over time
plot.stoch.prev <- function(dat, all.lines, error.bars, cis, ...){
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
  #
  # all.lines = plot all simulations
  # error.bars = plot the error bars?
  # OUTPUT
  # plot of the prevalence over time

  require(reshape2)
  require(tidyverse)

  if(missing(all.lines)){all.lines = TRUE}
  if(missing(error.bars)){
    error.bars <- FALSE
    cis <- c(0.1, 0.9)
  }
  if(missing(cis)){
    warning('no arguments for the cis were provided')
    cis <- c(0.1, 0.9)
  }


  # summarize by year and sex
  dat.sum <- dat %>%
    filter(month %% 12 == 7) %>%
    group_by(year, sim, disease) %>%
    summarize(n = sum(population)) %>%
    spread(key = disease, value = n) %>%
    mutate(prev = yes/ (no + yes)) %>%
    arrange(sim, year)

  # calculate mean, lo and hi percentiles.
  dat.errors <- dat.sum %>%
      group_by(year) %>%
      summarize(avg.prev = mean(prev),
                lo = quantile(prev, cis[1]),
                hi = quantile(prev, cis[2]))

  # Start constructing the plot
  if(all.lines == TRUE){
    p <- ggplot(data = dat.sum, aes(x = year, y = prev, group = sim)) +
      geom_line(color = "grey") +
      geom_line(data = dat.errors, aes(x = year, y = avg.prev, group = NULL),
                size = 1.5)
  }

  if(all.lines == FALSE){
    p <- ggplot(data = dat.errors, aes(x = year, y = avg.prev, group = NULL)) +
      geom_line(size = 1.5)
  }

  if(error.bars == TRUE){
    # plot the error bars
    p <- p + geom_line(data = dat.errors, aes(x = year, y = lo, group = NULL),
                       linetype = "dashed", color = "red") +
            geom_line(data = dat.errors, aes(x = year, y = hi, group = NULL),
                      linetype = "dashed", color = "red")
  }

  p <- p + xlab("Year") + ylab("Prevalence") + theme_light() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
  p
}

# plot the fawn:adult
plot.stoch.fawn.adult <- function(dat, all.lines, error.bars, ...){
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
  #
  # by.sex = TRUE....facet by sex
  # OUTPUT
  # plot of the fawn.adult ratio

  require(reshape2)
  require(tidyverse)

  dat$age.cat <- "adult"
  dat$age.cat[dat$age == 1] <- "fawn"

  # summarize by year and sex
  dat.sum <- dat %>%
    filter(month %% 12 == 11) %>%
    group_by(year, age.cat, sim) %>%
    summarize(n = sum(population)) %>%
    spread(key = age.cat, value = n) %>%
    mutate(fawn.adult = fawn / adult)%>%
    arrange(sim, year)

   # calculate the mean
  dat.mean <- dat.sum %>%
    group_by(year) %>%
    summarize(avg = mean(fawn.adult))

  if(missing(all.lines)){all.lines = TRUE}
  if(all.lines == TRUE){
    p <- ggplot(data = dat.sum, aes(x = year, y = fawn.adult, group = sim)) +
      geom_line(color = "grey") +
      geom_line(data = dat.mean, aes(x = year, y = avg, group = NULL), size = 1.5)
  }

  if(all.lines == FALSE){
    p <- ggplot(data = dat.mean, aes(x = year, y = avg, group = NULL)) +
      geom_line(size = 1.5)
  }

  if(missing(error.bars) == FALSE){
    # calculate the mean, and the error bars
    dat.mean <- dat.sum %>%
      group_by(year) %>%
      summarize(avg = mean(fawn.adult), lo = quantile(fawn.adult,error.bars[1]),
                hi = quantile(fawn.adult,error.bars[2]))

    p <- p + geom_line(data = dat.mean, aes(x = year, y = lo, group = NULL),
                       linetype = "dashed", color = "red") +
      geom_line(data = dat.mean, aes(x = year, y = hi, group = NULL),
                linetype = "dashed", color = "red")
  }

  p <- p + xlab("Year") + ylab("Fawn:Adult") + theme_light() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())

  p


}

# plot the buck:doe
plot.stoch.buck.doe <- function(dat, all.lines, error.bars, ...){
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
  #
  # OUTPUT
  # plot of the buck:doe ratio

  require(reshape2)
  require(tidyverse)

  dat$age.cat <- "adult"
  dat$age.cat[dat$age == 1] <- "fawn"

  # summarize by year and sex
  dat.sum <- dat %>%
    filter(month %% 12 == 8) %>% # december of every year
    group_by(year, age.cat, sex, sim) %>%
    summarize(n = sum(population)) %>%
    unite(sex.age, sex, age.cat) %>%
    spread(key = sex.age, value = n) %>%
    mutate(buck.doe = m_adult / f_adult)

  # calculate the mean
  dat.mean <- dat.sum %>%
    group_by(year) %>%
    summarize(avg = mean(buck.doe))

  if(missing(all.lines)){all.lines = TRUE}
  if(all.lines == TRUE){
    p <- ggplot(data = dat.sum, aes(x = year, y = buck.doe, group = sim)) +
      geom_line(color = "grey") +
      geom_line(data = dat.mean, aes(x = year, y = avg, group = NULL), size = 1.5)
  }

  if(all.lines == FALSE){
    p <- ggplot(data = dat.mean, aes(x = year, y = avg, group = NULL)) +
      geom_line(size = 1.5)
  }

  if(missing(error.bars) == FALSE){
    # calculate the mean, and the error bars
    dat.mean <- dat.sum %>%
      group_by(year) %>%
      summarize(avg = mean(buck.doe), lo = quantile(buck.doe, error.bars[1]),
                hi = quantile(buck.doe, error.bars[2]))

    p <- p + geom_line(data = dat.mean, aes(x = year, y = lo, group = NULL),
                       linetype = "dashed", color = "red") +
      geom_line(data = dat.mean, aes(x = year, y = hi, group = NULL),
                linetype = "dashed", color = "red")
  }

  p <- p + xlab("Year") + ylab("Buck:Doe") + theme_light() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())

  p


}

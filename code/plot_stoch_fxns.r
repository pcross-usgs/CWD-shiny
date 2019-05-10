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
  p <- p + xlab("Year") + ylab("Population") + theme_light(base_size = 18) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())


  p
}

# plot totals, pos, and negatives by sex
plot.stoch.tots.2 <- function(dat, error.bars){
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
  # error bars = vector of high and low percentiles
  #
  # OUTPUT
  # plot of the population totals split by sex and disease

  require(reshape2)
  require(tidyverse)

  dat.sum <- dat %>%
    filter(month %% 12 == 10) %>%
    group_by(year, sim, disease) %>%
    summarize(n = sum(population)) %>%
    spread(key = disease, value = n) %>%
    mutate(total = no + yes) %>%
    gather ("no", "yes", "total", key = "disease", value = "n" ) %>%
    mutate(disease = fct_recode(disease,
                                "negative" = "no",
                                "positive" = "yes",
                                "total" = "total")) %>%
    mutate(disease = fct_reorder(disease,n)) %>%
    arrange(sim, year)

  # calculate the mean
  dat.mean <- dat.sum %>%
    group_by(year, disease) %>%
    summarize(lo = quantile(n, error.bars[1]),
              hi = quantile(n, error.bars[2]),
              avg = mean(n)) %>%
    mutate(disease = fct_reorder(disease, avg))

  p <-   ggplot(data = dat.mean, aes(x = year, y = avg, color = disease)) +
    geom_line(size = 1.5) +
    xlab("Year") + ylab("Population")

  if(missing(error.bars) == F){
    p <- p + geom_line(data = dat.mean, aes(x = year, y = lo, color = disease),
                       linetype = "dashed") +
         geom_line(data = dat.mean, aes(x = year, y = hi, color = disease),
                linetype = "dashed")
  }

  p <- p + theme_light(base_size = 18) +
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
    theme_light(base_size = 18) + theme(panel.grid.minor = element_blank(),
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
    cis <- c(0.05, 0.95)
  }
  if(missing(cis)){
    warning('no arguments for the cis were provided')
    cis <- c(0.05, 0.95)
  }

  # summarize by year and disease
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

  p <- p + xlab("Year") + ylab("Prevalence") + theme_light(base_size = 18) +
    ylim(0, 1) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
  
  p
}

# plot the prevalence by age at the end point
plot.stoch.prev.age.2 <- function(dat, error.bars, ...){
  # INPUT
  # dat = list of the output matrices
  # OUTPUT
  # plot of the prevalence
  require(reshape2)
  require(tidyverse)

  # summarize disease status on the last year, calculate the prevalence
  dat.sum <- dat %>%
    filter(month %% 12 == 10, round(year, 0) == max(round(year, 0))) %>%
    group_by(age, sex, sim, disease)%>%
    summarize(n = sum(population)) %>%
    spread(key = disease, value = n) %>%
    mutate(prev = yes / (no + yes)) %>%
    select(age, sex, prev, sim)

  # calculate the mean
  dat.mean <- dat.sum %>%
    group_by(age, sex) %>%
    summarize(lo = quantile(prev, error.bars[1], na.rm = T),
              hi = quantile(prev, error.bars[2], na.rm = T),
              avg = mean(prev, na.rm = T)) %>%
    arrange(sex, age)

  p <-   ggplot(data = dat.mean, aes(x = age, y = avg, color = sex)) +
    geom_line(size = 1.5) +
    xlab("Age") + ylab("Prevalence")

    p <- p + geom_line(data = dat.mean, aes(x = age, y = lo, color = sex),
                       linetype = "dashed") +
      geom_line(data = dat.mean, aes(x = age, y = hi, color = sex),
                linetype = "dashed") +  theme_light(base_size = 18) +
      theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
    
    p
}

# plot the age distribution at the end point
plot.stoch.age.dist <- function(dat, ...){
  # INPUT
  # dat = list of the output matrices
  # OUTPUT
  # plot of the prevalence
  require(reshape2)
  require(tidyverse)

  # summarize disease status on the last year, calculate the prevalence
  dat.sum <- dat %>%
    filter(month %% 12 == 10, round(year, 0) == max(round(year, 0))) %>%
    group_by(age, sex, sim) %>%
    summarize(n = sum(population)) %>%
    select(age, sex, n, sim) %>%
    group_by(age, sex) %>%
    summarize(avg = mean(n, na.rm = T)) %>%
    arrange(sex, age)

  #create the plot
  p <-   ggplot(data = dat.sum, aes(x = age, y = avg, color = sex)) +
    geom_line(size = 1.5) +
    xlab("Age") + ylab("Population") + theme_light(base_size = 18) +
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

  p <- p + xlab("Year") + ylab("Fawn:Adult") + theme_light(base_size = 18) +
    ylim(0.2, 1) +
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

  p <- p + xlab("Year") + ylab("Buck:Doe") + theme_light(base_size = 18) +
    ylim(0.1,1) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
  

  p


}

# plot total deaths by type each year
plot.stoch.deaths <- function(dat, error.bars){
  # INPUT
  # dat = list of the output matrices of deaths
  # OUTPUT
  # plot of the total deaths over time
  require(reshape2)
  require(tidyverse)

  dat.sum <- dat %>%
    filter(age >= 2) %>%
    mutate(category = as.factor(str_sub(category, 1, 1))) %>%
    mutate(category = fct_recode(category,
                                 "CWD" = "C",
                                 "Natural" = "D",
                                 "Hunted" = "H"),
           year = floor(year)) %>%
    group_by(year, sex, category, sim) %>%
    summarize(n = sum(population))


  # calculate the mean
  dat.mean <- dat.sum %>%
    group_by(year, sex, category) %>%
    summarize(avg = mean(n)) %>%
    mutate(category = fct_reorder(category, avg))

  if(missing(error.bars) == F){# calculate the error bars
    dat.mean <- dat.sum %>%
      group_by(year, sex, category) %>%
      summarize(lo = quantile(n, error.bars[1]),
                hi = quantile(n, error.bars[2]),
                avg = mean(n)) %>%
      mutate(category = fct_reorder(category, avg))
  }

  p <-   ggplot(data = dat.mean, aes(x = year, y = avg, color = category)) +
    geom_line(size = 1.5) +
    xlab("Year") + ylab("# of Adult Deaths")

  if(missing(error.bars) == F){
    p <- p + geom_line(data = dat.mean, aes(x = year, y = lo, color = category),
                         linetype = "dashed") +
        geom_line(data = dat.mean, aes(x = year, y = hi, color = category),
                  linetype = "dashed")

  }
  p <- p + theme_light(base_size = 18) + facet_wrap(~sex) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
  
  p

}

# plot total deaths by type each year
plot.stoch.perc.deaths <- function(dat, error.bars){
  # INPUT
  # dat = list of the output matrices of deaths
  # OUTPUT
  # plot of the percentage of deaths by type over time
  require(reshape2)
  require(tidyverse)

  dat.sum <- dat %>%
    filter(age >= 2) %>%
    mutate(category = as.factor(str_sub(category, 1, 1))) %>%
    mutate(category = fct_recode(category,
                                 "CWD" = "C",
                                 "Natural" = "D",
                                 "Hunted" = "H"),
           year = floor(year)) %>%
    group_by(year, sex, category, sim) %>%
    summarize(n = sum(population)) %>%
    spread(key = category, value = n) %>%
    mutate(total = CWD + Natural + Hunted) %>%
    mutate(cwd.p = CWD/total, nat.p = Natural/total, hunt.p = Hunted/total) %>%
    select(year, sex, cwd.p, nat.p, hunt.p) %>%
    gather("cwd.p", "nat.p", "hunt.p", key ="category", value = "percent" ) %>%
    mutate(category = fct_recode(category,
                               "CWD" = "cwd.p",
                               "Natural" = "nat.p",
                               "Hunted" = "hunt.p"))

  # calculate the mean
  dat.mean <- dat.sum %>%
    group_by(year, sex, category) %>%
    summarize(avg.percent = mean(percent))%>%
    mutate(category = fct_reorder(category, avg.percent))

  if(missing(error.bars) == F){# calculate the error bars
    dat.mean <- dat.sum %>%
      group_by(year, sex, category) %>%
      summarize(lo = quantile(percent, error.bars[1]),
                hi = quantile(percent, error.bars[2]),
                avg.percent = mean(percent))%>%
      mutate(category = fct_reorder(category, avg.percent))
  }

  p <- ggplot(data = dat.mean, aes(x = year, y = avg.percent, color = category)) +
    geom_line(size = 1.5)  + xlab("Year") + ylab("% of Adult Deaths")

  if(missing(error.bars) == F){
    p <- p + geom_line(data = dat.mean, aes(x = year, y = lo, color = category),
                       linetype = "dashed") +
      geom_line(data = dat.mean, aes(x = year, y = hi, color = category),
                linetype = "dashed")
  }

 p <- p + facet_wrap(~sex) + theme_light(base_size = 18) +
          theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
 
  p
}

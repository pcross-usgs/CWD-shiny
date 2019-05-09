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
    mutate(total = no + yes) %>%
    gather ("no", "yes", "total", key = "disease", value = "n" ) %>%
    mutate(disease = fct_recode(disease,
                                "negative" = "no",
                                "positive" = "yes",
                                "total" = "total")) %>%
    mutate(disease = fct_reorder(disease,n))



  #plot
  p <- ggplot(dat.sum, aes(year, n, color = disease)) +
    geom_line(size = 1.5) +
    xlab("Year") + ylab("Population") + theme_light(base_size = 18) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
  p
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

  plot(dat.sum$year, dat.sum$prev, xlab = "Year", ylab = "Prevalence",
       bty = "l", type = "l", lwd = 2, ylim = ylims, ...)
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

# plot the prevalence by age at the end point
plot.prev.2 <- function(dat, ...){
  # INPUT
  # dat = list of the output matrices
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

   # summarize disease status on the last year, calculate the prevalence
  dat.sum2 <- dat %>%
    filter(month %% 12 == 10, round(year, 0) == max(round(year, 0))) %>%
    group_by(age, sex, disease)%>%
    summarize(n = sum(population)) %>%
    spread(key = disease, value = n) %>%
    mutate(prev = yes/ (no + yes)) %>%
    select(age, sex, prev) %>%
    spread(key = sex, value = prev)


  #create the plot
 par(mfrow = c(1,2))
 plot(dat.sum$year, dat.sum$prev, xlab = "Year", ylab = "Prevalence",
       bty = "l", type = "l", lwd = 2,
      cex = 1.5, cex.lab = 1.5, cex.axis = 1.5,  ...)
 plot(dat.sum2$age, dat.sum2$f, type = "l", col = "red", xlab = "Age",
      ylab = "Prevalence", bty = "l", lwd = 2,
      cex = 1.5, cex.lab = 1.5, cex.axis = 1.5, ...)
 lines(dat.sum2$age + 0.1, dat.sum2$m, col = "blue", lwd = 2)
 legend("bottomright", c("females", "males"),
        col = c("red", "blue"), cex = 1.5, lwd = 2, box.lty = 0)
}

# plot the age distribution at the end point
plot.age.dist <- function(dat, ...){
  # INPUT
  # dat = list of the output matrices
  # OUTPUT
  # plot of the prevalence
  require(reshape2)
  require(tidyverse)

  # summarize disease status on the last year, calculate the prevalence
  dat.sum <- dat %>%
    filter(month %% 12 == 10, round(year, 0) == max(round(year, 0))) %>%
    group_by(age, sex, age) %>%
    summarize(n = sum(population)) %>%
    select(age, sex, n) %>%
    arrange(sex, age)

  #create the plot
  p <- ggplot(dat.sum, aes(x = age, y = n, color = sex)) +
    geom_line(size = 1.5) +
    ylab("Population") + xlab("Age") +
    theme_light()  + theme(text = element_text(size = 18),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())
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

# plot both fawn:doe and buck:doe
plot.fawn.buck <- function(dat, ...){
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


  # summarize by year and disease status, calculate the prevalence
  dat.sum.2 <- dat %>%
    filter(month %% 12 == 11) %>%
    group_by(year, age.cat) %>%
    summarize(n = sum(population)) %>%
    spread(key = age.cat, value = n) %>%
    mutate(fawn.adult = fawn / adult)

  par(mfrow = c(1,2))
  par(mar = c(6, 6, 1, 2),
      cex = 1.5, cex.lab = 1.5, cex.axis = 1.5)
  plot(dat.sum$year, dat.sum$buck.doe, type = "l",
       col = "blue", bty = "l",
       ylab = "buck:doe", xlab = "Year", ...)
  plot(dat.sum.2$year, dat.sum.2$fawn.adult, type = "l",
       col = "red", bty = "l", ylab = "fawn:doe",
       xlab = "Year", ...)
}


# plot total deaths by type each year
plot.deaths <- function(dat){
  # INPUT
  # dat = list of the output matrices of deaths
  # OUTPUT
  # plot of the total deaths over time
  require(reshape2)
  require(tidyverse)

  deaths <- dat %>%
    filter(age >= 2) %>%
    mutate(category = as.factor(str_sub(category, 1, 1))) %>%
    mutate(category = fct_recode(category,
                                 "CWD" = "C",
                                 "Natural" = "D",
                                 "Hunted" = "H"),
           year = floor(year)) %>%
    group_by(year, sex, category) %>%
    summarize(n = sum(population)) %>%
    mutate(category = fct_reorder(category, n))

p <-   ggplot(data = deaths, aes(x = year, y = n, color = category)) +
    geom_line(size = 1.5) + facet_wrap(~sex) +
    xlab("Year") + ylab("# of Adult Deaths") + theme_light(base_size = 18) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())
p

}

# plot total deaths by type each year
plot.perc.deaths <- function(dat){
  # INPUT
  # dat = list of the output matrices of deaths
  # OUTPUT
  # plot of the percentage of deaths by type over time
  require(reshape2)
  require(tidyverse)

  deaths <- dat %>%
    filter(age >= 2) %>%
    mutate(category = as.factor(str_sub(category, 1, 1))) %>%
    mutate(category = fct_recode(category,
                                 "CWD" = "C",
                                 "Natural" = "D",
                                 "Hunted" = "H"),
           year = floor(year)) %>%
    group_by(year, sex, category) %>%
    summarize(n = sum(population)) %>%
    spread(key = category, value = n) %>%
    mutate(total = CWD + Natural + Hunted) %>%
    mutate(cwd.p = CWD/total, nat.p = Natural/total, hunt.p = Hunted/total) %>%
    select(year, sex, cwd.p, nat.p, hunt.p) %>%
    gather("cwd.p", "hunt.p", "nat.p", key ="category", value = "percent" ) %>%
    mutate(category = fct_recode(category,
                               "CWD" = "cwd.p",
                               "Natural" = "nat.p",
                               "Hunted" = "hunt.p")) %>%
    mutate(category = fct_reorder(category, percent))


  p <- ggplot(data = deaths, aes(x = year, y = percent, color = category)) +
          geom_line(size = 1.5) + facet_wrap(~sex) +
          xlab("Year") + ylab("% of Adult Deaths") + theme_light(base_size = 18) +
          theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
p
}


# Functions to plot the output from a single simulation
plot_tots <- function(dat, ...){
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
    mutate(disease = fct_reorder(disease, n))

  #plot
  p <- ggplot(dat.sum, aes(year, n, color = disease)) +
    geom_line(size = 1.5) +
    xlab("Year") + ylab("Population") + theme_light(base_size = 18) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())
  p
}

# plot the prevalence
plot_prev_time <- function(dat, ...){
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

  #plot(dat.sum$year, dat.sum$prev, xlab = "Year", ylab = "Prevalence",
  #     bty = "l", type = "l", lwd = 2, ...)
  par(cex = 1.25, cex.lab = 1.25, cex.axis = 1.25)
  ggplot(dat.sum, aes(x = year, y = prev)) +
    geom_line(size = 1.5) + ylim(0,1) +
    ylab("Prevalence") + xlab("Year") +
    theme_light()  + theme(text = element_text(size = 18),
                           panel.grid.minor = element_blank(),
                           panel.grid.major = element_blank())

}

# plot the prevalence
plot_prev_age <- function(dat, by.sex, ...){
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
plot_prev_age_end <- function(dat, ...){
  # INPUT
  # dat = list of the output matrices
  # OUTPUT
  # plot of the prevalence
  require(reshape2)
  require(tidyverse)
  require(cowplot)

   # summarize disease status on the last year, calculate the prevalence
  dat.sum <- dat %>%
    filter(month %% 12 == 10, round(year, 0) == max(round(year, 0))) %>%
    group_by(age, sex, disease)%>%
    summarize(n = sum(population)) %>%
    spread(key = disease, value = n) %>%
    mutate(prev = yes/ (no + yes)) %>%
    select(age, sex, prev)

 #prevalence by age
 ggplot(dat.sum, aes(x = age, y = prev, color = sex)) +
   geom_line(size = 1.5) + ylim(0,1) +
   ylab("") + xlab("Age") +
   theme_light()  + theme(text = element_text(size = 18),
                          panel.grid.minor = element_blank(),
                          panel.grid.major = element_blank(),
                          legend.position = c(.25,.85))
 }

# plot the age distribution at the end point
plot_age_dist <- function(dat, ...){
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
    select(age, sex, n)

  #create the plot
  p <- ggplot(dat.sum, aes(x = age, y = n, color = sex)) +
    geom_line(size = 1.5) +
    ylab("Population") + xlab("Age") +
    theme_light()  + theme(text = element_text(size = 18),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(), legend.position = c(.15,.8))

  p
}

# plot the fawn:doe
plot_fawn_doe <- function(dat, ...){
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
    group_by(year, sex, age.cat) %>%
    summarize(n = sum(population)) %>%
    unite(sex.age, sex, age.cat) %>%
    spread(key = sex.age, value = n) %>%
    mutate(fawn.doe = (m_fawn + f_fawn) / f_adult)

  ggplot(dat.sum, aes(x = year, y = fawn.doe)) +
    geom_line(size = 1.5) + ylim(0, 1.2) +
    ylab("Fawn:Doe ratio") + xlab("Year") +
    theme_light()  + theme(text = element_text(size = 18),
                           panel.grid.minor = element_blank(),
                           panel.grid.major = element_blank())

}

# plot buck:doe
plot_buck_doe <- function(dat, ...){
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

  ggplot(dat.sum, aes(x = year, y = buck.doe)) +
    geom_line(size = 1.5) + ylim(0, 1.2) +
    ylab("Buck:Doe ratio") + xlab("Year") +
    theme_light()  + theme(text = element_text(size = 18),
                           panel.grid.minor = element_blank(),
                           panel.grid.major = element_blank())

}

# plot total deaths by type each year
plot_deaths <- function(dat, percent){
  # INPUT
  # dat = list of the output matrices of deaths
  # percent = TRUE to plot the percentages rather that the totals
  # OUTPUT
  # plot of the total deaths over time
  require(reshape2)
  require(tidyverse)

  if(missing(percent)){
    percent <- F
  }
  if(percent == F){
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

    p <- ggplot(data = deaths, aes(x = year, y = n, color = category)) +
      geom_line(size = 1.5) + facet_wrap(~sex) +
      xlab("Year") + ylab("# of Adult Deaths")
  }

  if(percent == T){
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
      geom_line(size = 1.5) + facet_wrap(~sex)+
      xlab("Year") + ylab("% of Adult Deaths")


  }

  p <-  p + theme_light(base_size = 18) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())

  p

}

# plot total deaths by type each year
plot_perc_deaths <- function(dat){
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
          panel.grid.major = element_blank())

p
}

#comparison plots for deterministic scenarios
plot_compare_all_det <- function(outa, outb, ...){
  # INPUT
  # outa = all output from scenario a
  # outb = all  output from scenario b

  # OUTPUT
  # comparison plots

  require(reshape2)
  require(tidyverse)
  require(cowplot)

  # organize the data
  counts <- list(outa$counts, outb$counts)

  counts <- melt(counts, id = c("age", "month", "population", "category",
                                "year", "sex", "disease")) %>%
    filter(month %% 12 == 10, round(year, 0) == max(round(year, 0))) %>%
    rename(scenario = L1) %>%
    mutate(scenario = fct_recode(as.factor(scenario), A = "1", B = "2"))

  totals <- counts %>%
    group_by(scenario) %>%
    summarize(n = sum(population))

  prev <- counts %>%
    group_by(disease, scenario) %>%
    summarize(n = sum(population)) %>%
    spread(key = disease, value = n) %>%
    mutate(prevalence = yes/ (no + yes))

  deaths <- list(outa$deaths, outb$deaths)

  hunted <- melt(deaths, id = c("age", "month", "population", "category",
                                "year", "sex")) %>%
    filter(age >= 2, str_sub(category, 1, 1) == "H") %>%
    rename(scenario = L1) %>%
    mutate(scenario = fct_recode(as.factor(scenario), A = "1", B = "2"))

  tot.hunted <- hunted %>%
    group_by(scenario) %>%
    summarize(n = sum(population))

  males.hunted <- hunted %>%
    filter(sex == "m") %>%
    group_by(scenario) %>%
    summarize(n = sum(population))

  last.hunted <- hunted %>%
    filter(round(year, 0) == max(round(year, 0))) %>%
    group_by(scenario) %>%
    summarize(n = sum(population))

  males.last.hunted <- hunted %>%
    filter(round(year, 0) == max(round(year, 0))) %>%
    filter(sex == "m") %>%
    group_by(scenario) %>%
    summarize(n = sum(population))

  # define theme
  theme_set(theme_bw(base_size = 18))

  # totals
  p1 <- ggplot(totals, aes(x = scenario, y = n)) +
    geom_bar(stat = "identity", fill = "dark grey", color = "black") +
    ylab("Total population") + xlab("") +
    coord_flip() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), legend.position = "none")

  # prevalence
  p2 <- ggplot(prev, aes(x = scenario, y = prevalence)) +
    geom_bar(stat = "identity", fill = "dark grey", color = "black") +
    ylab("Prevalence") +xlab("") +
    coord_flip() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), legend.position = "none")

  # total hunted
  p3 <- ggplot(tot.hunted, aes(x = scenario, y = n)) +
    geom_bar(stat = "identity", fill = "dark grey", color = "black") +
    ylab("Total hunted") + xlab("") +
    coord_flip() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), legend.position = "none")

  # total last hunted
  p4 <- ggplot(last.hunted, aes(x = scenario, y = n)) +
    geom_bar(stat = "identity", fill = "dark grey", color = "black") +
    ylab("Total hunted last year") + xlab("") +
    coord_flip() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), legend.position = "none")

  # males hunted
  p5 <- ggplot(males.hunted, aes(x = scenario, y = n)) +
    geom_bar(stat = "identity", fill = "dark grey", color = "black") +
    ylab("Males hunted") + xlab("") +
    coord_flip() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), legend.position = "none")

  # males hunted
  p6 <- ggplot(males.last.hunted, aes(x = scenario, y = n)) +
    geom_bar(stat = "identity", fill = "dark grey", color = "black") +
    ylab("Males hunted last year") + xlab("") +
    coord_flip() +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), legend.position = "none")

  #browser()
  p7 <- plot_grid(p1,p2,p3,p4,p5,p6, nrow = 3)
  p7
}


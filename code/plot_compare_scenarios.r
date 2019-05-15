plot.compare.tots <- function(outa, outb, ...){
# INPUT
# outa = counts output from scenario a
# outb = counts output from scenario b

# OUTPUT
# comparison plot of total population size

require(reshape2)
require(tidyverse)
require(ggridges)
# combine the two outputs
dat <- list(outa, outb)

dat <- melt(dat, id = c("age", "month", "population", "category",
                        "year", "sex", "disease", "sim")) %>%
  filter(month %% 12 == 10, round(year, 0) == max(round(year, 0))) %>%
  rename(scenario = L1) %>%
  mutate(scenario = fct_recode(as.factor(scenario), A = "1", B = "2")) %>%
  group_by(sim, scenario) %>%
  summarize(n = sum(population))

theme_set(theme_bw(base_size = 18))

# define some color options
cols <- c('#ffff00','#0000ff')

# plot
ggplot(dat, aes(x = n, y = scenario, fill = scenario)) +
  geom_density_ridges(alpha= 0.6) + theme_bw(base_size = 18) + theme_ridges() +
  xlab("Total population") + ylab("") +
  scale_y_discrete() + scale_fill_manual(values = cols)

}

plot.compare.prev <- function(outa, outb, ...){
  # INPUT
  # outa = counts output from scenario a
  # outb = counts output from scenario b

  # OUTPUT
  # comparison plot of prevalence
  require(reshape2)
  require(tidyverse)
  require(ggridges)

  # combine the two outputs
  dat <- list(outa, outb)

  dat <- melt(dat, id = c("age", "month", "population", "category",
                          "year", "sex", "disease", "sim")) %>%
    filter(month %% 12 == 10, round(year, 0) == max(round(year, 0))) %>%
    rename(scenario = L1) %>%
    mutate(scenario = fct_recode(as.factor(scenario), A = "1", B = "2")) %>%
    group_by(disease, sim, scenario) %>%
    summarize(n = sum(population))%>%
    spread(key = disease, value = n) %>%
    mutate(prev = yes / (no + yes))

  theme_set(theme_bw(base_size = 18))

  # define some color options
  cols <- c('#ffff00','#0000ff')

  # plot
  ggplot(dat, aes(x = prev, y = scenario, fill = scenario)) +
    geom_density_ridges(alpha= 0.6) + theme_ridges() +
    xlab("Disease prevalence") + ylab("") +
    scale_y_discrete() + scale_fill_manual(values = cols)
}

plot.compare.hunted <- function(outa, outb, ...){
  # INPUT
  # outa = deaths output from scenario a
  # outb = deaths output from scenario b

  # OUTPUT
  # comparison plot of total population size

  require(reshape2)
  require(tidyverse)
  require(ggridges)
  # combine the two outputs
  dat <- list(outa, outb)
  dat <- melt(dat, id = c("age", "month", "population", "category",
                          "year", "sex", "sim")) %>%
    filter(age >= 2, str_sub(category, 1, 1) == "H") %>%
    rename(scenario = L1) %>%
    mutate(scenario = fct_recode(as.factor(scenario), A = "1", B = "2"),
           year = floor(year)) %>%
    group_by(sim, scenario) %>%
    summarize(n = sum(population))

  theme_set(theme_bw(base_size = 18))

  # define some color options
  cols <- c('#ffff00','#0000ff')

  # plot
  ggplot(dat, aes(x = n, y = scenario, fill = scenario)) +
    geom_density_ridges(alpha= 0.6) + theme_ridges() +
    xlab("Total hunted > 1yr old") + ylab("") +
    scale_y_discrete() + scale_fill_manual(values = cols)

}

plot.compare.hunted.end <- function(outa, outb, ...){
  # INPUT
  # outa = deaths output from scenario a
  # outb = deaths output from scenario b

  # OUTPUT
  # comparison plot of total population size

  require(reshape2)
  require(tidyverse)
  require(ggridges)
  # combine the two outputs
  dat <- list(outa, outb)
  dat <- melt(dat, id = c("age", "month", "population", "category",
                          "year", "sex", "sim")) %>%
    filter(age >= 2, str_sub(category, 1, 1) == "H",
           round(year, 0) == max(round(year, 0))) %>%
    rename(scenario = L1) %>%
    mutate(scenario = fct_recode(as.factor(scenario), A = "1", B = "2"),
           year = floor(year)) %>%
    group_by(sim, scenario) %>%
    summarize(n = sum(population))

  theme_set(theme_bw(base_size = 18))

  # define some color options
  cols <- c('#ffff00','#0000ff')

  # plot
  ggplot(dat, aes(x = n, y = scenario, fill = scenario)) +
    geom_density_ridges(alpha= 0.6) + theme_ridges() +
    xlab("Total hunted > 1yr old in last year") + ylab("") +
    scale_y_discrete() + scale_fill_manual(values = cols)

}


plot.compare.buckshunted <- function(outa, outb, ...){
  # INPUT
  # outa = deaths output from scenario a
  # outb = deaths output from scenario b

  # OUTPUT
  # comparison plot of total population size

  require(reshape2)
  require(tidyverse)
  require(ggridges)
  # combine the two outputs
  dat <- list(outa, outb)
  dat <- melt(dat, id = c("age", "month", "population", "category",
                          "year", "sex", "sim")) %>%
    filter(age >= 2, str_sub(category, 1, 1) == "H", sex == "m") %>%
    rename(scenario = L1) %>%
    mutate(scenario = fct_recode(as.factor(scenario), A = "1", B = "2"),
           year = floor(year)) %>%
    group_by(sim, scenario) %>%
    summarize(n = sum(population))

  theme_set(theme_bw(base_size = 18))

  # define some color options
  cols <- c('#ffff00','#0000ff')

  # plot
  ggplot(dat, aes(x = n, y = scenario, fill = scenario)) +
    geom_density_ridges(alpha= 0.6) + theme_ridges() +
    xlab("Males hunted > 1yr old") + ylab("") +
    scale_y_discrete() + scale_fill_manual(values = cols)

}

plot.compare.buckshunted.end <- function(outa, outb, ...){
  # INPUT
  # outa = deaths output from scenario a
  # outb = deaths output from scenario b

  # OUTPUT
  # comparison plot of total population size

  require(reshape2)
  require(tidyverse)
  require(ggridges)
  # combine the two outputs
  dat <- list(outa, outb)
  dat <- melt(dat, id = c("age", "month", "population", "category",
                          "year", "sex", "sim")) %>%
    filter(age >= 2, str_sub(category, 1, 1) == "H", sex == "m",
           round(year, 0) == max(round(year, 0))) %>%
    rename(scenario = L1) %>%
    mutate(scenario = fct_recode(as.factor(scenario), A = "1", B = "2"),
           year = floor(year)) %>%
    group_by(sim, scenario) %>%
    summarize(n = sum(population))

  theme_set(theme_bw(base_size = 18))

  # define some color options
  cols <- c('#ffff00','#0000ff')

  # plot
  ggplot(dat, aes(x = n, y = scenario, fill = scenario)) +
    geom_density_ridges(alpha= 0.6) + theme_ridges() +
    xlab("Males hunted > 1yr old in last year") + ylab("") +
    scale_y_discrete() + scale_fill_manual(values = cols)

}

plot.compare.oldbuckshunted <- function(outa, outb, ...){
  # INPUT
  # outa = deaths output from scenario a
  # outb = deaths output from scenario b

  # OUTPUT
  # comparison plot of total population size

  require(reshape2)
  require(tidyverse)
  require(ggridges)
  # combine the two outputs
  dat <- list(outa, outb)
  dat <- melt(dat, id = c("age", "month", "population", "category",
                          "year", "sex", "sim")) %>%
    filter(age >= 4, str_sub(category, 1, 1) == "H", sex == "m") %>%
    rename(scenario = L1) %>%
    mutate(scenario = fct_recode(as.factor(scenario), A = "1", B = "2"),
           year = floor(year)) %>%
    group_by(sim, scenario) %>%
    summarize(n = sum(population))

  theme_set(theme_bw(base_size = 18))

  # define some color options
  cols <- c('#ffff00','#0000ff')

  # plot
  ggplot(dat, aes(x = n, y = scenario, fill = scenario)) +
    geom_density_ridges(alpha= 0.6) + theme_ridges() +
    xlab("Males hunted > 3yr old") + ylab("") +
    scale_y_discrete() + scale_fill_manual(values = cols)

}

plot.compare.oldbuckshunted.end <- function(outa, outb, ...){
  # INPUT
  # outa = deaths output from scenario a
  # outb = deaths output from scenario b

  # OUTPUT
  # comparison plot of total population size

  require(reshape2)
  require(tidyverse)
  require(ggridges)
  # combine the two outputs
  dat <- list(outa, outb)
  dat <- melt(dat, id = c("age", "month", "population", "category",
                          "year", "sex", "sim")) %>%
    filter(age >= 4, str_sub(category, 1, 1) == "H", sex == "m",
           round(year, 0) == max(round(year, 0))) %>%
    rename(scenario = L1) %>%
    mutate(scenario = fct_recode(as.factor(scenario), A = "1", B = "2"),
           year = floor(year)) %>%
    group_by(sim, scenario) %>%
    summarize(n = sum(population))

  theme_set(theme_bw(base_size = 18))

  # define some color options
  cols <- c('#ffff00','#0000ff')

  # plot
  ggplot(dat, aes(x = n, y = scenario, fill = scenario)) +
    geom_density_ridges(alpha= 0.6) + theme_ridges() +
    xlab("Males hunted > 3yr old in last year") + ylab("") +
    scale_y_discrete() + scale_fill_manual(values = cols)

}


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

plot.compare.all <- function(outa, outb, ...){
  # INPUT
  # outa = all output from scenario a
  # outb = all  output from scenario b

  # OUTPUT
  # comparison plot of total population size
  #browser()
  require(reshape2)
  require(tidyverse)
  require(ggridges)
  require(cowplot)

  outcount <- list(outa$counts, outb$counts)
  outcount <- melt(outcount, id = c("age", "month", "population", "category",
                              "year", "sex", "disease", "sim")) %>%
    filter(month %% 12 == 10, round(year, 0) == max(round(year, 0))) %>%
    rename(scenario = L1) %>%
    mutate(scenario = fct_recode(as.factor(scenario), A = "1", B = "2"))

  totals <- outcount %>%
    group_by(sim, scenario) %>%
    summarize(n = sum(population))

  prev <- outcount %>%
    group_by(sim, disease, scenario) %>%
    summarize(n = sum(population)) %>%
    spread(key = disease, value = n) %>%
    mutate(prevalence = yes/ (no + yes))

  death <- list(outa$deaths, outb$deaths)

  hunted <- melt(death, id = c("age", "month", "population", "category",
                               "year", "sex", "sim")) %>%
    filter(age >= 2, str_sub(category, 1, 1) == "H") %>%
    rename(scenario = L1) %>%
    mutate(scenario = fct_recode(as.factor(scenario), A = "1", B = "2"))

  tot.hunted <- hunted %>%
    group_by(sim, scenario) %>%
    summarize(n = sum(population))

  males.hunted <- hunted %>%
    filter(sex == "m") %>%
    group_by(sim, scenario) %>%
    summarize(n = sum(population))

  last.hunted <- hunted %>%
    filter(round(year, 0) == max(round(year, 0))) %>%
    group_by(sim, scenario) %>%
    summarize(n = sum(population))

  males.last.hunted <- hunted %>%
    filter(round(year, 0) == max(round(year, 0))) %>%
    filter(sex == "m") %>%
    group_by(sim, scenario) %>%
    summarize(n = sum(population))

  theme_set(theme_bw(base_size = 18))

  # define colors
  cols <- c('#ffff00','#0000ff')

  # totals

  p1 <- ggplot(totals, aes(x = n, y = scenario, fill = scenario)) +
    geom_density_ridges(alpha= 0.6, scale = 4) + theme_ridges() +
    theme(legend.position = "none") +
    xlab("Total population") + ylab("") +
    scale_y_discrete() + scale_fill_manual(values = cols)

  # prev
  p2 <- ggplot(prev, aes(x = prevalence, y = scenario, fill = scenario)) +
    geom_density_ridges(alpha= 0.6, scale = 4) +
    theme_ridges() +theme(legend.position = "none") +
    xlab("Disease prevalence") + ylab("") +
    scale_y_discrete() + scale_fill_manual(values = cols)

  # plot
  p3 <- ggplot(tot.hunted, aes(x = n, y = scenario, fill = scenario)) +
    geom_density_ridges(alpha= 0.6, scale = 4) +
    theme_ridges() +theme(legend.position = "none") +
    xlab("Total hunted > 1yr old") + ylab("") +
    scale_y_discrete() + scale_fill_manual(values = cols)

  p4 <- ggplot(last.hunted, aes(x = n, y = scenario, fill = scenario)) +
    geom_density_ridges(alpha= 0.6, scale = 4) + theme_ridges() +
    theme(legend.position = "none") +
    xlab("Total hunted > 1yr old in last year") + ylab("") +
    scale_y_discrete() + scale_fill_manual(values = cols)

  # plot
  p5 <- ggplot(males.hunted, aes(x = n, y = scenario, fill = scenario)) +
    geom_density_ridges(alpha= 0.6, scale = 4) + theme_ridges() +
    theme(legend.position = "none") +
    xlab("Males hunted > 1yr old") + ylab("") +
    scale_y_discrete() + scale_fill_manual(values = cols)

  # plot
  p6 <- ggplot(males.last.hunted, aes(x = n, y = scenario, fill = scenario)) +
    geom_density_ridges(alpha= 0.6, scale = 4) + theme_ridges() +
    theme(legend.position = "none") +
    xlab("Males hunted > 1yr old in last year") + ylab("") +
    scale_y_discrete() + scale_fill_manual(values = cols)

  #browser()
  p7 <- plot_grid(p1,p2,p3,p4,p5,p6, nrow = 3)
  p7
}

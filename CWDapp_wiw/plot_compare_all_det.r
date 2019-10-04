#' Deterministic Comparison plot
#'
#' @param outa counts as provided as output from the CWD model functions for the
#'  first simulation
#' @param outb counts as provided as output from the CWD model functions for the
#'  second simulation
#'
#' @return a bar plot comparison
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom reshape2 melt
#' @importFrom tidyr spread
#' @importFrom cowplot plot_grid
#' @importFrom stringr str_sub
#' @importFrom forcats fct_recode
#' 
#' @examples
#' params.a <- list(fawn.an.sur = 0.6, juv.an.sur = 0.8, ad.an.f.sur = 0.95, 
#' ad.an.m.sur = 0.9, fawn.repro = 0, juv.repro = 0.6, ad.repro = 1, 
#' hunt.mort.fawn = 0.01, hunt.mort.juv.f = 0.1, hunt.mort.juv.m = 0.1,
#' hunt.mort.ad.f = 0.2, hunt.mort.ad.m = 0.2, ini.fawn.prev = 0.02,
#' ini.juv.prev = 0.03, ini.ad.f.prev = 0.04,  ini.ad.m.prev = 0.04,
#' n.age.cats = 12,  p = 0.43, env.foi = 0,  beta.f = 0.15,  beta.m = 0.15,
#' theta = 1, n0 = 2000, n.years = 10, rel.risk = 1.0)
#' 
#' params.b <- list(fawn.an.sur = 0.6, juv.an.sur = 0.8, ad.an.f.sur = 0.95, 
#' ad.an.m.sur = 0.9, fawn.repro = 0, juv.repro = 0.6, ad.repro = 1, 
#' hunt.mort.fawn = 0.01, hunt.mort.juv.f = 0.1, hunt.mort.juv.m = 0.1,
#' hunt.mort.ad.f = 0.1, hunt.mort.ad.m = 0.5, ini.fawn.prev = 0.02,
#' ini.juv.prev = 0.03, ini.ad.f.prev = 0.04, ini.ad.m.prev = 0.04,
#' n.age.cats = 12,  p = 0.43, env.foi = 0,  beta.f = 0.15,  beta.m = 0.15,
#' theta = 1, n0 = 2000, n.years = 10, rel.risk = 1.0)
#' 
#' out.a <- cwd_det_model(params.a)
#' out.b <- cwd_det_model(params.b)
#' 
#' plot_compare_all_det(out.a, out.b)
#'
#' @export

plot_compare_all_det <- function(outa, outb){

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


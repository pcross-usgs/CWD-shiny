#' Plot the predation deaths compared to deaths from hunting, natural mortality,
#' and disease.
#'
#'
#' @param deaths the deaths output from predator simulations
#' @param predation the predation output from predator simulations
#' @param percents TRUE or FALSE on whether to plot the percentatges or raw numbers
#'
#' @return a plot death due to predation, hunting, disease, and natural.
#' 
#' @import ggplot2
#' @import dplyr
#' @import ggpubr
#' @importFrom magrittr %>%
#' @importFrom forcats fct_recode fct_reorder
#' @examples
#' 
#' params <- list(n.predators=70, k_max=1, min.predators=20,
#' max.predators=150, r=0.25, k_inflect=3000, n_inflect=2000,
#' numeric.form="Type2", functional.form="Type3",
#' beta.f.low=0.026, beta.m.low=0.026, S=5, env.foi=0.0,
#' base.juv=3, base.adult=1, base.old=2, p=0.28,
#' selection.form='exponential', stages=c(0:10),
#' juvs=c(1:2), adults=c(3:13), old=c(14:18), n.age.cats=18,
#' fawn.an.sur=0.65, juv.an.sur=0.85, ad.an.f.sur=0.9,
#' ad.an.m.sur=0.85, fawn.repro=0, juv.repro=0.4, ad.repro=0.85,
#' hunt.mort.fawn=0.01, hunt.mort.juv.f=0.03, hunt.mort.juv.m=0.03,
#' hunt.mort.ad.f=0.05, hunt.mort.ad.m=0.1,
#' ini.fawn.prev=0.01, ini.juv.prev=0.03, ini.ad.f.prev=0.04, ini.ad.m.prev=0.04,
#' n0=10000, n.years=20, theta=1, rel.risk=1.0)
#' 
#' out <- cwd_detmod_predation_late(params)
#' 
#' plot_predation_deaths(out$deaths, out$predation, percents = FALSE)
#' 
#' @export

plot_predation_deaths <- function(deaths, predation, percents) 
{
  if (missing(deaths) | missing(predation) == TRUE) 
    warning("missing data to plot")
  if (missing(percents)) {
    percents <- F
  }
  
  predation <- predation %>% 
    filter(!grepl('Proportion', category), !grepl('Predator', category))
  predation$category <- "P"
  
  dat.combo <- rbind(deaths, predation)
  if (percents == F) {
    deaths <- dat.combo %>%
      filter(age >= 2) %>%
      mutate(category = as.factor(stringr::str_sub(category, 1, 1))) %>%
      mutate(category = fct_recode(category,
                                   "CWD" = "C",
                                   "Natural" = "D",
                                   "Hunted" = "H",
                                   "Predated" = "P"),
             year = floor(year)) %>%
      group_by(year, category) %>%
      summarize(n = sum(population)) %>%
      mutate(category = fct_reorder(category, n))
    
    p <- ggplot(data = deaths, aes(x = year, y = n, color = category)) +
      geom_line(size = 1.5) +
      xlab("Year") + ylab("Adult Deaths") + theme_light(base_size = 18) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
  }
  if (percents == T) {
    deaths.p <- dat.combo %>%
      filter(age >= 2) %>%
      mutate(category = as.factor(stringr::str_sub(category, 1, 1))) %>%
      mutate(category = fct_recode(category,
                                   "CWD" = "C",
                                   "Natural" = "D",
                                   "Hunted" = "H",
                                   "Predated" = "P"),
             year = floor(year)) %>%
      group_by(year, category) %>%
      summarize(n = sum(population)) %>%
      spread(key = category, value = n) %>%
      mutate(total = CWD + Natural + Hunted + Predated) %>%
      mutate(cwd.p = CWD/total, nat.p = Natural/total, hunt.p = Hunted/total, predated.p = Predated/total) %>%
      select(year, cwd.p, nat.p, hunt.p, predated.p) %>%
      gather("cwd.p", "hunt.p", "nat.p", "predated.p", key ="category", value = "percent" ) %>%
      mutate(category = fct_recode(category,
                                   "CWD" = "cwd.p",
                                   "Natural" = "nat.p",
                                   "Hunted" = "hunt.p",
                                   "Predated" = "predated.p")) %>%
      mutate(category = fct_reorder(category, percent))
    
    deaths.p$category <- factor(deaths.p$category, levels=c("CWD","Predated","Hunted","Natural"))
    p <- ggplot(data = deaths.p, aes(x = year, y = percent*100, color = category)) +
      geom_line(size = 1.5) +
      xlab("Year") + ylab("% of Adult Deaths") + theme_light(base_size = 18) +
      theme(panel.grid.minor = element_blank(),
            panel.grid.major = element_blank())
  }
  
  p <- p + theme_light(base_size = 18) + theme(panel.grid.minor = element_blank(), 
                                               panel.grid.major = element_blank())
  p
}


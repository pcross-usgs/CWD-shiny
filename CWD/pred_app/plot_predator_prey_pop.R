#' Plot the predator-prey counts over time.
#'
#'
#' @param dat.prey the prey output from predator simulations
#' @param dat.pred the predation output from predator simulations
#'
#' @return a plot of four components:
#' 
#' 1. Transmission values by sex and disease category
#' 
#' 2. Vulnerability by age and ddisease category
#' 
#' 3. Monthly kill rate by prey population size
#' 
#' 4. Numerical response of predators to prey population
#'
#' @import ggplot2
#' @import dplyr
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
#' plot_predator_prey_pop(out$counts, out$predation)
#' 
#' @export

plot_predator_prey_pop <- function(dat.prey, dat.pred){
  if(missing(dat.prey) | missing(dat.pred) ==TRUE) warning("missing data to plot")
  
  prey.pop <- dat.prey %>% 
    filter(year%%1 == 0) %>% 
    group_by(year) %>% 
    summarise(n = sum(population))
  
  pred.pop <- dat.pred %>% 
    filter(category == "Predator") %>% 
    filter(month%%12 == 1)
  
  p <- ggplot() +
    geom_line(data = prey.pop, mapping = aes(x = year, y = n, color = "Prey")) +
    geom_line(data = pred.pop, mapping = aes(x = year, y = population/max(pred.pop$population)*max(prey.pop$n)*0.8, color = "Predator")) +
    scale_y_continuous(sec.axis = sec_axis(trans = ~.*max(pred.pop$population)/max(prey.pop$n)/0.8, name = "Predator Population")) +
    theme_light(base_size = 18) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()) +
    labs(x = "Year",
         y = "Prey Population") +
    scale_color_discrete("Population Type") +
    theme_light(base_size = 18) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()) 
  
  p
}

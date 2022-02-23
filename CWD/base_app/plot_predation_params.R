#' Plots to show some of the predation parameters.
#'
#'
#' @param dat parameters entered into predation model
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
#' @importFrom ggpubr ggarrange
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @importFrom forcats fct_recode fct_reorder
#' @examples
#' 
#' params <- list(n.predators=70, k_max=1, min.predators=20,
#' max.predators=150, r=0.25, k_inflect=3000, n_inflect=2000,
#' numeric.form="Type2", functional.form="Type3",
#' beta.f.low=0.02, beta.m.low=0.03, S=5, env.foi=0.0,
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
#' plot_predation_params(out$params)
#' 
#' @export

plot_predation_params <- function(dat){
  if(missing(dat)==TRUE) warning("missing data to plot")

  beta.low <- c(dat$beta.f.low, dat$beta.m.low) # no sex differences
  S <- dat$S # scalar for increase in transmision in late stages
  early.stages <- 1:7
  late.stages <- 8:10
  trans.stages <- rep(c(early.stages, late.stages),2)
  transmission <- c(rep(beta.low[1], length(early.stages)), rep(beta.low[1]*S, length(late.stages)),
                    rep(beta.low[2], length(early.stages)), rep(beta.low[2]*S, length(late.stages)))
  max.d <- beta.low*S
  
  a <- tibble(Sex = rep(c("Female", "Male"), each = 10),
         Transmission = transmission, 
         Trans.stages = trans.stages) %>% 
    ggplot(aes(x = Trans.stages, y = Transmission, color = Sex)) +
    geom_rect(data = tibble(x=1), ymin = -Inf, ymax = Inf, xmin = 7.5, xmax = 10, alpha = 0.25, color = NA, fill = "black", inherit.aes = F) +
    geom_point() + 
    geom_line() +
    theme_light(base_size = 18) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()) +
    labs(x = "CWD Stages")
  
  stages = 1:10
  base.juv <- dat$base.juv # baseline juvenile selection relative to adults
  base.adult <- 1
  base.old <- dat$base.old # baseline senescent selection relative to adults
  r <- dat$r # rate of vulnerability increase (exponential form BUT THIS CAN CHANGE - use the options and equations from the full predator model: exponential, linear, equal)
  
  if(dat$selection.form=="equal"){
    s.juv <- rep(base.juv, length(stages))
    s.adult <- rep(base.adult, length(stages))
    s.old <- rep(base.old, length(stages))
    max.c <- max(base.juv, base.adult, base.old)
  }
  
  if(dat$selection.form == "linear") {
    s.juv <- base.juv + r*stages
    s.adult <- base.adult + r*stages
    s.old <- base.old + r*stages
    max.c <- max(base.juv, base.adult, base.old) + r*stages
  }

  if(dat$selection.form == "exponential") {
    s.juv <- base.juv*(1 + r)^stages
    s.adult <- base.adult*(1 + r)^stages
    s.old <- base.old*(1 + r)^stages
    max.c <- max(base.juv, base.adult, base.old)*(1 + r)^10
  }
  
  b <- tibble(selection = c(s.juv, s.adult, s.old),
         stages = rep(1:10, 3),
         Age = rep(c("Juvenile", "Adult", "Senescent"), each = 10)) %>% 
    ggplot(aes(x = stages, y = selection, color = Age)) +
    geom_point()+
    geom_line() +
    geom_hline(linetype = "dashed", color = "grey50", yintercept = max.c) +
    theme_light(base_size = 18) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()) +
    labs(x = "CWD Stages",
         y = "Vulnerability")
  
  n.total.prey <- seq(0,dat$n0*1.2,by=10) # might need to impose a max x-axis (prey density) for plotting
  k_max <- dat$k_max # max per capita kill rate
  k_inflect <- dat$k_inflect # inflection point
  
  min.predators <- dat$min.predators # minimum, set a priori
  max.predators <- dat$max.predators # maximum, set a priori
  n_inflect <- dat$n_inflect # inflection point
  
  if(dat$functional.form == "Type2"){
    kr <- (max.predators * n.total.prey) / (n_inflect + n.total.prey)
  }
  
  if(dat$functional.form == "Type3"){
    kr <- (k_max * n.total.prey^2) / (k_inflect^2 + n.total.prey^2)
  }
  
  max.a <- max(kr)
  
  c <- tibble(kr = kr,
              prey = n.total.prey) %>% 
    ggplot(aes(x = prey, y = kr)) +
    geom_point()+
    geom_line() +
    geom_hline(linetype = "dashed", color = "grey50", yintercept = max.a) +
    theme_light(base_size = 18) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()) +
    labs(x = "Prey Population",
         y = "Monthly Kill Rate")
  
  if(dat$numeric.form == "Type2"){
    n.pred <-  (max.predators*n.total.prey) / (n_inflect + n.total.prey)
    n.pred <- ifelse(n.pred<min.predators, min.predators, n.pred)
  }
  
  if(dat$numeric.form == "Type3"){
    n.pred <-  (k_max*n.total.prey^2) / (n_inflect + n.total.prey^2)
    n.pred <- ifelse(n.pred<min.predators, min.predators, n.pred)
    n.pred <- ifelse(n.pred>max.predators, max.predators, n.pred)
  }
  
  max.b <- max(n.pred)
  
  d <- tibble(kr = n.pred,
              prey = n.total.prey) %>% 
    ggplot(aes(x = prey, y = kr)) +
    geom_point()+
    geom_line() +
    geom_hline(linetype = "dashed", color = "grey50", yintercept = max.b) +
    theme_light(base_size = 15) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()) +
    labs(x = "Prey Population",
         y = "Numerical Response")
  
  p <- ggpubr::ggarrange(a,b,c,d, labels = c("A.", "B.","C.","D."), hjust = -0.1,
                         nrow = 2, ncol = 2)
  
  p
}

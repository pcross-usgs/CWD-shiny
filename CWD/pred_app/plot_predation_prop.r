#' Plot the proportion of juveniles, adults, and old
#' that are consumed by predators based on infection status.
#'
#' @param dat.prey the counts output from predator simulations
#' @param dat.pred the predation output from predator simulations
#' @param dat.params the params output from predator simulations
#'
#' @return a plot death due to predation, hunting, disease, and natural.
#' 
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom stringr str_detect
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
#' plot_predation_prop(out$counts, out$predation, out$params)
#' 
#' @export

plot_predation_prop <- function(dat.prey, dat.pred, dat.params){
  if(missing(dat.pred) |
     missing(dat.params) |
     missing(dat.prey)) warning("missing data to plot")
  
  prey.pop <- dat.prey %>% 
    mutate(class = case_when(age %in% dat.params$juvs ~ "Juveniles",
                             age %in% dat.params$adults ~ "Adult",
                             age %in% dat.params$old ~ "Old"),
           infected = ifelse(disease == "no", "Healthy", "Infected")) %>% 
    group_by(year, class, infected) %>% 
    dplyr::summarize(prop = sum(population))
  
  pred.prop <- dat.pred %>% 
    filter(str_detect(category,"Predated")) %>% 
    mutate(class = case_when(age %in% dat.params$juvs ~ "Juveniles",
                             age %in% dat.params$adults ~ "Adult",
                             age %in% dat.params$old ~ "Old"),
           infected = ifelse(str_detect(category, "\\_H"), "Healthy", "Infected")) %>% 
    group_by(year, class, infected) %>% 
    dplyr::summarize(prop = sum(population))
  
  pred.prop$prop = pred.prop$prop/prey.pop$prop
  
  p <- ggplot() +
    geom_line(data = pred.prop, mapping = aes(x = year, y = prop, color = class, linetype = infected)) +
    theme_light(base_size = 18) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()) +
    labs(x = "Year",
         y = "Deaths / Population size") +
    scale_color_discrete("Population Type") +
    scale_linetype_discrete("Disease Status") +
    theme_light(base_size = 18) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank()) 
  
  p
}

#' Age distribution plot
#'
#' @param dat counts as provided as output from the CWD model
#'
#' @return a plot of the percent of the population in each age class
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr %>%
#' @examples 
#' params <- list(fawn.an.sur = 0.6, juv.an.sur = 0.8, ad.an.f.sur = 0.95, 
#' ad.an.m.sur = 0.9, fawn.repro = 0, juv.repro = 0.6, ad.repro = 1, 
#' hunt.mort.fawn = 0.01, hunt.mort.juv.f = 0.1, hunt.mort.juv.m = 0.1,
#' hunt.mort.ad.f = 0.2, hunt.mort.ad.m = 0.2, ini.fawn.prev = 0.02,
#' ini.juv.prev = 0.03, ini.ad.f.prev = 0.04,  ini.ad.m.prev = 0.04,
#' n.age.cats = 12,  p = 0.43, env.foi = 0,  beta.f = 0.15,  beta.m = 0.15,
#' theta = 1, n0 = 2000, n.years = 10, rel.risk = 1.0, 
#' repro.var = 0.005, fawn.sur.var = 0.005, sur.var = 0.005, hunt.var = 0.005)
#' 
#' out <- cwd_stoch_wrapper(params, nsims = 10)
#' plot_stoch_age_dist(out$counts)
#' 
#' @export

plot_stoch_age_dist <- function(dat){
  if(missing(dat)==TRUE) warning("missing data to plot")
  
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

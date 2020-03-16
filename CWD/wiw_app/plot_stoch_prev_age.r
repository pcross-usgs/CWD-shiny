#' Plot stochastic prevalence by age over time
#'
#' @param dat counts as provided as output from the CWD model
#' @param by.sex TRUE/FALSE on whether to facet by sex. Default = FALSE
#'
#' @return a multiple line plot of the simulation over time
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom tidyr spread
#' @examples 
#' params <- list(fawn.an.sur = 0.6, juv.an.sur = 0.8, ad.an.f.sur = 0.95, 
#' ad.an.m.sur = 0.9, fawn.repro = 0, juv.repro = 0.6, ad.repro = 1, 
#' hunt.mort.fawn = 0.01, hunt.mort.juv.f = 0.1, hunt.mort.juv.m = 0.1,
#' hunt.mort.ad.f = 0.1, hunt.mort.ad.m = 0.4, ini.fawn.prev = 0.02,
#' ini.juv.prev = 0.03, ini.ad.f.prev = 0.04,  ini.ad.m.prev = 0.04,
#' n.age.cats = 12,  p = 0.43, env.foi = 0,  beta.f = 0.15,  beta.m = 0.15,
#' theta = 1, n0 = 2000, n.years = 10, rel.risk = 1.0, 
#' repro.var = 0.005, fawn.sur.var = 0.005, sur.var = 0.005, hunt.var = 0.005)
#' 
#' out <- cwd_stoch_wrapper(params, nsims = 20)
#' plot_stoch_prev_age(out$counts, by.sex = TRUE)
#' @export

plot_stoch_prev_age <- function(dat, by.sex){
  if(missing(dat)==TRUE) warning("missing data to plot")
  
  if(missing(by.sex)){by.sex = F}

  if(by.sex == F){
    # summarize by year and sex
    dat.sum <- dat %>%
      filter(month %% 12 == 7) %>%
      group_by(year, age, disease, sim) %>%
      summarize(n = sum(population)) %>%
      arrange(sim, year) %>%
      spread(key = disease, value = n) %>%
      mutate(prev = yes/ (no + yes))

    # calculate the mean
    dat.mean <- dat.sum %>%
      group_by(age, year) %>%
      summarize(avg = mean(prev))

    p <- ggplot(dat.mean, aes(year, avg, group = age, color = age)) +
      geom_line()
  }

  if(by.sex == T){
    # summarize by year and sex
    dat.sum <- dat %>%
      filter(month %% 12 == 7) %>%
      group_by(year, age, sex, disease, sim) %>%
      summarize(n = sum(population)) %>%
      spread(key = disease, value = n) %>%
      mutate(prev = yes/ (no + yes))

    # calculate the mean
    dat.mean <- dat.sum %>%
      group_by(age, sex, year) %>%
      summarize(avg = mean(prev))

    p <- ggplot(dat.mean, aes(year, avg, group = age, color = age)) +
      geom_line() + facet_wrap(~sex)

  }

  p <- p + xlab("Year") + ylab("Prevalence") +
    theme_light(base_size = 18) + theme(panel.grid.minor = element_blank(),
                                        panel.grid.major.x = element_blank())

  p
}

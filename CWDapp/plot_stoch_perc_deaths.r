#' Percentage death types stochastic plot
#'
#' @param dat counts as provided as output from the CWD model
#' @param error.bars 2 value vector for the hi and lo percentiles on the error
#' bars. If missing, no error bars are shown.
#'
#' @return a plot of % death types over time.
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom stringr str_sub
#' @importFrom forcats fct_recode fct_reorder
#' @importFrom stats quantile
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
#' plot_stoch_perc_deaths(out$deaths, error.bars = c(0.05, 0.95))
#' @export

plot_stoch_perc_deaths <- function(dat, error.bars){
  if(missing(dat)==TRUE) warning("missing data to plot")
  
  dat.sum <- dat %>%
    filter(age >= 2) %>%
    mutate(category = as.factor(str_sub(category, 1, 1))) %>%
    mutate(category = fct_recode(category,
                                 "CWD" = "C",
                                 "Natural" = "D",
                                 "Hunted" = "H"),
           year = floor(year)) %>%
    group_by(year, sex, category, sim) %>%
    summarize(n = sum(population)) %>%
    spread(key = category, value = n) %>%
    mutate(total = CWD + Natural + Hunted) %>%
    mutate(cwd.p = CWD/total, nat.p = Natural/total, hunt.p = Hunted/total) %>%
    select(year, sex, cwd.p, nat.p, hunt.p) %>%
    gather("cwd.p", "nat.p", "hunt.p", key ="category", value = "percent" ) %>%
    mutate(category = fct_recode(category,
                               "CWD" = "cwd.p",
                               "Natural" = "nat.p",
                               "Hunted" = "hunt.p"))

  # calculate the mean
  dat.mean <- dat.sum %>%
    group_by(year, sex, category) %>%
    summarize(avg.percent = mean(percent, na.rm = T))%>%
    mutate(category = fct_reorder(category, avg.percent))

   p <- ggplot(data = dat.mean, aes(x = year, y = avg.percent, color = category)) +
    geom_line(size = 1.5)  + xlab("Year") + ylab("% of Adult Deaths")

  if(missing(error.bars) == FALSE){
    # calculate the error bars
    dat.mean <- dat.sum %>%
      group_by(year, sex, category) %>%
      summarize(lo = quantile(percent, error.bars[1]),
                hi = quantile(percent, error.bars[2]),
                avg.percent = mean(percent))%>%
      mutate(category = fct_reorder(category, avg.percent))

  p <- p + geom_line(data = dat.mean, aes(x = year, y = lo, color = category),
                       linetype = "dashed") +
      geom_line(data = dat.mean, aes(x = year, y = hi, color = category),
                linetype = "dashed")

  }

  p <- p + facet_wrap(~sex) + theme_light(base_size = 18) +
          theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())

  p
}

#' Fawn:doe stochastic plot
#'
#' @param dat counts as provided as output from the CWD model
#' @param all.lines TRUE/FALSE for whether to plot a line for every simulation
#' @param error.bars 2 value vector for the hi and lo percentiles on the error
#' bars. If missing, no error bars will be shown.
#'
#' @return a plot of the percent of the population in each age class
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom stats quantile
#' @importFrom tidyr unite spread
#' @examples 
#' params <- list(fawn.an.sur = 0.7, juv.an.sur = 0.9, ad.an.f.sur = 0.95, 
#' ad.an.m.sur = 0.8, fawn.repro = 0, juv.repro = 0.4, ad.repro = .9, 
#' hunt.mort.fawn = 0.01, hunt.mort.juv.f = 0.1, hunt.mort.juv.m = 0.2,
#' hunt.mort.ad.f = 0.15, hunt.mort.ad.m = 0.35, ini.fawn.prev = 0.01,
#' ini.juv.prev = 0.03, ini.ad.f.prev = 0.04,  ini.ad.m.prev = 0.04,
#' n.age.cats = 12,  p = 0.27, env.foi = 0,  beta.f = 0.15,  beta.m = 0.15,
#' theta = 1, n0 = 2000, n.years = 10, rel.risk = 1.0, 
#' repro.var = 0.005, fawn.sur.var = 0.005, sur.var = 0.005, hunt.var = 0.005)
#' 
#' out <- cwd_stoch_wrapper(params, nsims = 20)
#' plot_stoch_fawn_doe(out$counts, error.bars = c(0.05, 0.95))
#' @export

plot_stoch_fawn_doe <- function(dat, all.lines, error.bars){
  if(missing(dat)==TRUE) warning("missing data to plot")
  
  if(missing(all.lines)){all.lines = TRUE}

  dat$age.cat <- "adult"
  dat$age.cat[dat$age == 1] <- "fawn"

  # summarize by year and sex
  dat.sum <- dat %>%
    filter(month %% 12 == 11) %>%
    group_by(year, sex, age.cat, sim) %>%
    dplyr::summarize(n = sum(population)) %>%
    unite(sex.age, sex, age.cat) %>%
    spread(key = sex.age, value = n) %>%
    mutate(fawn.doe = (m_fawn + f_fawn) / f_adult)

  # calculate the mean
  dat.mean <- dat.sum %>%
    group_by(year) %>%
    dplyr::summarize(avg = mean(fawn.doe))

  if(all.lines == TRUE){
    p <- ggplot(data = dat.sum, aes(x = year, y = fawn.doe, group = sim)) +
      geom_line(color = "grey") +
      geom_line(data = dat.mean, aes(x = year, y = avg, group = NULL), size = 1.5)
  }

  if(all.lines == FALSE){
    p <- ggplot(data = dat.mean, aes(x = year, y = avg, group = NULL)) +
      geom_line(size = 1.5)
  }

  if(missing(error.bars) == FALSE){
    # calculate the mean, and the error bars
    dat.mean <- dat.sum %>%
      group_by(year) %>%
      dplyr::summarize(avg = mean(fawn.doe), lo = quantile(fawn.doe,error.bars[1]),
                hi = quantile(fawn.doe,error.bars[2]))

    p <- p + geom_line(data = dat.mean, aes(x = year, y = lo, group = NULL),
                       linetype = "dashed", color = "red") +
      geom_line(data = dat.mean, aes(x = year, y = hi, group = NULL),
                linetype = "dashed", color = "red")
  }

  p <- p + xlab("Year") + ylab("Fawn:Doe") + theme_light(base_size = 18) +
    ylim(0.1, 1) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())


  p


}

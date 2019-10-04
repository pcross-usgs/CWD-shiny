#' Plot stochastic totals over time
#'
#' @param dat counts as provided as output from the CWD model
#' @param all.lines TRUE/FALSE for whether to plot a line for every simulation
#' @param error.bars error bars = vector of high and low percentiles (2 values only).
#' If missing, no error bars will be shown.
#' @param by.sexage TRUE/FALSE for whether to facet by sex and age.
#'
#' @return a multiple line plot of the simulation over time
#' 
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom reshape2 melt
#' @importFrom tidyr spread
#' @importFrom stats quantile
#' @importFrom forcats fct_reorder
#'
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
#' plot_stoch_tots(out$counts, all.lines = TRUE, by.sexage = TRUE, 
#' error.bars = c(0.05, 0.95))
#' 
#' @export

plot_stoch_tots <- function(dat, all.lines, error.bars, by.sexage){
  if(missing(dat)==TRUE) warning("missing data to plot")
  
  if(missing(all.lines)){all.lines = T}

  if(missing(by.sexage)){by.sex = F}

  # summarize the data

  if(by.sexage == T){
    dat$age.cat <- "adult"
    dat$age.cat[dat$age == 1] <- "fawn"

    dat.sum <- dat %>%
      filter(month %% 12 == 10) %>%
      group_by(year, age.cat, sex, sim) %>%
      summarize(n = sum(population)) %>%
      unite(sex.age, sex, age.cat)

    # calculate the mean
    dat.mean <- dat.sum %>%
      group_by(year, sex.age) %>%
      summarize(avg = mean(n, na.rm = T))

    if(missing(error.bars) == F){# calculate the error bars
      dat.errors <- dat.sum %>%
        group_by(year, sex.age) %>%
        summarize(lo = quantile(n, error.bars[1]),
                  hi = quantile(n, error.bars[2]))
    }
  }

  if(by.sexage == F){
    dat.sum <- dat %>%
      filter(month %% 12 == 10) %>%
      group_by(year, sim) %>%
      summarize(n = sum(population)) %>%
      arrange(sim, year)

    # calculate the mean
    dat.mean <- dat.sum %>%
      group_by(year) %>%
      summarize(avg = mean(n, na.rm = T))

    if(missing(error.bars) == F){# calculate the error bars
      dat.errors <- dat.sum %>%
        group_by(year) %>%
        summarize(lo = quantile(n, error.bars[1]),
                  hi = quantile(n, error.bars[2]))
    }
  }

  if(all.lines == TRUE){
    p <- ggplot(data = dat.sum, aes(x = year, y = n, group = sim)) +
      geom_line(color = "grey") +
      geom_line(data = dat.mean, aes(x = year, y = avg, group = NULL), size = 1.5)
  }

  if(all.lines == FALSE){
    p <- ggplot(data = dat.mean, aes(x = year, y = avg, group = NULL)) +
      geom_line(size = 1.5)
  }

  if(missing(error.bars) == FALSE){

    p <- p + geom_line(data = dat.errors, aes(x = year, y = lo, group = NULL),
                       linetype = "dashed", color = "red") +
      geom_line(data = dat.errors, aes(x = year, y = hi, group = NULL),
                linetype = "dashed", color = "red")
  }

  if(by.sexage == T){
    p <- p + facet_wrap(~ sex.age)
  }

  # adjust the theme
  p <- p + xlab("Year") + ylab("Population") + theme_light(base_size = 18) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank())


  p
}

#' Prevalence versus age plot
#'
#' @param dat counts as provided as output from the CWD model
#' @param error.bars vector with 2 values for the low and high percentiles. If
#' missing, then no error bars will be shown.
#'
#' @return a plot of prevalence versus age at the last timepoint of the
#' simulation
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom tidyr spread
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
#' plot_stoch_prev_age_end(out$counts, error.bars = c(0.05, 0.95))
#' 
#' @export

plot_stoch_prev_age_end <- function(dat, error.bars){
  if(missing(dat)==TRUE) warning("missing data to plot")
  
  # summarize disease status on the last year, calculate the prevalence
  dat.sum <- dat %>%
    filter(month %% 12 == 10, round(year, 0) == max(round(year, 0))) %>%
    group_by(age, sex, sim, disease)%>%
    summarize(n = sum(population)) %>%
    spread(key = disease, value = n) %>%
    mutate(prev = yes / (no + yes)) %>%
    select(age, sex, prev, sim)

  dat.mean <- dat.sum %>%
    group_by(age, sex) %>%
    summarize(avg = mean(prev, na.rm = T)) %>%
    arrange(sex, age)

  p <-   ggplot(data = dat.mean, aes(x = age, y = avg, color = sex)) +
    geom_line(size = 1.5) + ylim(0,1) +
    xlab("Age") + ylab("Prevalence")

  if(missing(error.bars) == FALSE){
    # calculate the mean
    dat.mean <- dat.sum %>%
    group_by(age, sex) %>%
    summarize(lo = quantile(prev, error.bars[1], na.rm = T),
              hi = quantile(prev, error.bars[2], na.rm = T),
              avg = mean(prev, na.rm = T)) %>%
    arrange(sex, age)

    p <- p + geom_line(data = dat.mean, aes(x = age, y = lo, color = sex),
                     linetype = "dashed") +
    geom_line(data = dat.mean, aes(x = age, y = hi, color = sex),
              linetype = "dashed")

  }

  p <- p + theme_light(base_size = 18) + theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          legend.position = c(.15,.85))

  p
}

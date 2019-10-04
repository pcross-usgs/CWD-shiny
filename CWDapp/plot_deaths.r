#' Plot the deaths by category.
#'
#'
#' @param dat deaths as provided as output from the CWD model functions
#' @param percents TRUE/FALSE on whether to plot the totals or the percentage
#' absolute totals are the default
#'
#' @return a plot deaths by category
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom forcats fct_recode fct_reorder
#' @examples
#' params <- list(fawn.an.sur = 0.6, juv.an.sur = 0.8, ad.an.f.sur = 0.95, 
#' ad.an.m.sur = 0.9, fawn.repro = 0, juv.repro = 0.6, ad.repro = 1, 
#' hunt.mort.fawn = 0.01, hunt.mort.juv.f = 0.1, hunt.mort.juv.m = 0.1,
#' hunt.mort.ad.f = 0.2, hunt.mort.ad.m = 0.2, ini.fawn.prev = 0.02,
#' ini.juv.prev = 0.03, ini.ad.f.prev = 0.04,  ini.ad.m.prev = 0.04,
#' n.age.cats = 12,  p = 0.43, env.foi = 0,  beta.f = 0.15,  beta.m = 0.15,
#' theta = 1, n0 = 2000, n.years = 10, rel.risk = 1.0)
#' 
#' out <- cwd_det_model(params)
#' plot_deaths(out$deaths, percents = TRUE)
#' plot_deaths(out$deaths, percents = FALSE)
#' 
#' @export

plot_deaths <- function(dat, percents){
  if(missing(dat)==TRUE) warning("missing data to plot")
  
  if(missing(percents)){percents <- F}

  if(percents == F){
    deaths <- dat %>%
      filter(age >= 2) %>%
      mutate(category = as.factor(str_sub(category, 1, 1))) %>%
      mutate(category = fct_recode(category,
                                   "CWD" = "C",
                                   "Natural" = "D",
                                   "Hunted" = "H"),
             year = floor(year)) %>%
      group_by(year, sex, category) %>%
      summarize(n = sum(population)) %>%
      mutate(category = fct_reorder(category, n))

    p <- ggplot(data = deaths, aes(x = year, y = n, color = category)) +
      geom_line(size = 1.5) + facet_wrap(~sex) +
      xlab("Year") + ylab("# of Adult Deaths")
  }

  if(percents == T){
    deaths <- dat %>%
      filter(age >= 2) %>%
      mutate(category = as.factor(str_sub(category, 1, 1))) %>%
      mutate(category = fct_recode(category,
                                   "CWD" = "C",
                                   "Natural" = "D",
                                   "Hunted" = "H"),
             year = floor(year)) %>%
      group_by(year, sex, category) %>%
      summarize(n = sum(population)) %>%
      spread(key = category, value = n) %>%
      mutate(total = CWD + Natural + Hunted) %>%
      mutate(cwd.p = CWD/total, nat.p = Natural/total, hunt.p = Hunted/total) %>%
      select(year, sex, cwd.p, nat.p, hunt.p) %>%
      gather("cwd.p", "hunt.p", "nat.p", key ="category", value = "percent" ) %>%
      mutate(category = fct_recode(category,
                                   "CWD" = "cwd.p",
                                   "Natural" = "nat.p",
                                   "Hunted" = "hunt.p")) %>%
      mutate(category = fct_reorder(category, percent))
    p <- ggplot(data = deaths, aes(x = year, y = percent, color = category)) +
      geom_line(size = 1.5) + facet_wrap(~sex)+
      xlab("Year") + ylab("% of Adult Deaths")


  }

  p <-  p + theme_light(base_size = 18) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank())

  p
}

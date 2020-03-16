#' Stochastic Comparison plot of all hunted
#'
#' @param outa deaths data as provided as output from the CWD model functions for the
#'  first simulation
#' @param outb deaths data as provided as output from the CWD model functions for the
#'  second simulation
#' @param end TRUE/FALSE for whether to show just the last timepoint
#'  (end = TRUE), or the cumulative number over the whole simulation.
#'  Default = FALSE.
#' @param males.only TRUE/FALSE for whether to show only males
#'  Default = FALSE.
#' @param old.only TRUE/FALSE for whether to show just those hunted over 3yrs.
#'  Default = FALSE.
#'
#' @return a density plot comparison
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom reshape2 melt
#' @importFrom tidyr spread
#' @examples 
#' params.a <- list(fawn.an.sur = 0.6, juv.an.sur = 0.8, ad.an.f.sur = 0.95, 
#' ad.an.m.sur = 0.9, fawn.repro = 0, juv.repro = 0.6, ad.repro = 1, 
#' hunt.mort.fawn = 0.01, hunt.mort.juv.f = 0.1, hunt.mort.juv.m = 0.1,
#' hunt.mort.ad.f = 0.2, hunt.mort.ad.m = 0.2, ini.fawn.prev = 0.02,
#' ini.juv.prev = 0.03, ini.ad.f.prev = 0.04,  ini.ad.m.prev = 0.04,
#' n.age.cats = 12,  p = 0.43, env.foi = 0,  beta.f = 0.15,  beta.m = 0.15,
#' theta = 1, n0 = 2000, n.years = 10, rel.risk = 1.0, 
#' repro.var = 0.005, fawn.sur.var = 0.005, sur.var = 0.005, hunt.var = 0.005)
#' 
#' params.b <- list(fawn.an.sur = 0.6, juv.an.sur = 0.8, ad.an.f.sur = 0.95, 
#' ad.an.m.sur = 0.9, fawn.repro = 0, juv.repro = 0.6, ad.repro = 1, 
#' hunt.mort.fawn = 0.01, hunt.mort.juv.f = 0.1, hunt.mort.juv.m = 0.1,
#' hunt.mort.ad.f = 0.05, hunt.mort.ad.m = 0.6, ini.fawn.prev = 0.02,
#' ini.juv.prev = 0.03, ini.ad.f.prev = 0.04,  ini.ad.m.prev = 0.04,
#' n.age.cats = 12,  p = 0.43, env.foi = 0,  beta.f = 0.15,  beta.m = 0.15,
#' theta = 1, n0 = 2000, n.years = 10, rel.risk = 1.0, 
#' repro.var = 0.005, fawn.sur.var = 0.005, sur.var = 0.005, hunt.var = 0.005)
#
#' out.a <- cwd_stoch_wrapper(params.a, nsims = 20)
#' out.b <- cwd_stoch_wrapper(params.b, nsims = 20)
#' 
#' plot_compare_hunted(out.a$deaths, out.b$deaths)
#'
#' @export

plot_compare_hunted <- function(outa, outb, end, males.only, old.only){
  if(missing(outa)==TRUE) warning("missing scenario a data to plot")
  if(missing(outb)==TRUE) warning("missing scenario b data to plot")  
  
  if(missing(end) == TRUE){end <- FALSE}
  if(missing(males.only) == TRUE){males.only <- FALSE}
  if(missing(old.only) == TRUE){old.only <- FALSE}

  # combine the two outputs
  dat <- list(outa, outb)
  dat <- melt(dat, id = c("age", "month", "population", "category",
                          "year", "sex", "sim")) %>%
    filter(age >= 2, str_sub(category, 1, 1) == "H") %>%
    rename(scenario = L1) %>%
    mutate(scenario = fct_recode(as.factor(scenario), A = "1", B = "2"),
           year = floor(year))

 if(males.only == TRUE){
    dat <- dat %>% filter(sex == "m")
  }

 if(old.only == TRUE){
    dat <- dat %>% filter(age >= 4)
  }

 if(end == TRUE){
   dat <- dat %>% filter(round(year, 0) == max(round(year, 0)))
 }

  dat <- dat %>%
    group_by(sim, scenario) %>%
    summarize(n = sum(population))

  # define some color options
  cols <- c('#ffff00','#0000ff')

  # plot
  p <- ggplot(dat, aes(x = n, y = scenario, fill = scenario)) +
    geom_density_ridges(alpha= 0.6) + theme_ridges() + ylab("") +
    scale_y_discrete() + scale_fill_manual(values = cols) +
    theme_set(theme_bw(base_size = 18))

  if(end == FALSE){
    if(males.only == FALSE){
      if(old.only == FALSE){
        p <- p + xlab("Total hunted > 1yr old")
      }
    }
  }

  if(end == TRUE){
    if(males.only == FALSE){
      if(old.only == FALSE){
        p <- p + xlab("Total hunted > 1yr old in last year")
      }
    }
  }

  if(end == FALSE){
    if(males.only == TRUE){
      if(old.only == FALSE){
        p <- p + xlab("Total males hunted > 1yr old")
      }
    }
  }

  if(end == TRUE){
    if(males.only == TRUE){
      if(old.only == FALSE){
        p <- p + xlab("Total males hunted > 1yr old in last year")
      }
    }
  }

  if(end == FALSE){
    if(males.only == FALSE){
      if(old.only == T){
        p <- p + xlab("Total hunted > 3yr old")
      }
    }
  }

  if(end == TRUE){
    if(males.only == FALSE){
      if(old.only == T){
        p <- p + xlab("Total hunted > 3yr old in last year")
      }
    }
  }

  if(end == FALSE){
    if(males.only == TRUE){
      if(old.only == T){
        p <- p + xlab("Total males hunted > 3yr old")
      }
    }
  }

  if(end == TRUE){
    if(males.only == TRUE){
      if(old.only == T){
        p <- p + xlab("Total males hunted > 3yr old in last year")
      }
    }
  }


}

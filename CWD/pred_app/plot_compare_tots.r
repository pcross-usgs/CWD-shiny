#' Stochastic Comparison plot of totals over time
#'
#' @param outa counts as provided as output from the CWD model functions for the
#'  first simulation
#' @param outb counts as provided as output from the CWD model functions for the
#'  second simulation
#'
#' @return a density plot comparison
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom reshape2 melt
#' @importFrom forcats fct_recode
#' @examples 
#' params.a <- list(fawn.an.sur = 0.7, juv.an.sur = 0.9, ad.an.f.sur = 0.95, 
#' ad.an.m.sur = 0.8, fawn.repro = 0, juv.repro = 0.4, ad.repro = .9, 
#' hunt.mort.fawn = 0.01, hunt.mort.juv.f = 0.1, hunt.mort.juv.m = 0.1,
#' hunt.mort.ad.f = 0.2, hunt.mort.ad.m = 0.2, ini.fawn.prev = 0.02,
#' ini.juv.prev = 0.03, ini.ad.f.prev = 0.04,  ini.ad.m.prev = 0.04,
#' n.age.cats = 12,  p = 0.27, env.foi = 0,  beta.f = 0.15,  beta.m = 0.15,
#' theta = 1, n0 = 2000, n.years = 10, rel.risk = 1.0, 
#' repro.var = 0.005, fawn.sur.var = 0.005, sur.var = 0.005, hunt.var = 0.005)
#' 
#' params.b <- list(fawn.an.sur = 0.7, juv.an.sur = 0.9, ad.an.f.sur = 0.95, 
#' ad.an.m.sur = 0.9, fawn.repro = 0, juv.repro = 0.4, ad.repro = .9, 
#' hunt.mort.fawn = 0.01, hunt.mort.juv.f = 0.1, hunt.mort.juv.m = 0.1,
#' hunt.mort.ad.f = 0.05, hunt.mort.ad.m = 0.6, ini.fawn.prev = 0.02,
#' ini.juv.prev = 0.03, ini.ad.f.prev = 0.04,  ini.ad.m.prev = 0.04,
#' n.age.cats = 12,  p = 0.27, env.foi = 0,  beta.f = 0.15,  beta.m = 0.15,
#' theta = 1, n0 = 2000, n.years = 10, rel.risk = 1.0, 
#' repro.var = 0.005, fawn.sur.var = 0.005, sur.var = 0.005, hunt.var = 0.005)
#
#' out.a <- cwd_stoch_wrapper(params.a, nsims = 20)
#' out.b <- cwd_stoch_wrapper(params.b, nsims = 20)
#' 
#' plot_compare_tots(out.a$counts, out.b$counts)
#'
#' @export

plot_compare_tots <- function(outa, outb){
  if(missing(outa)==TRUE) warning("missing scenario a data to plot")
  if(missing(outb)==TRUE) warning("missing scenario b data to plot")  
  
dat <- list(outa, outb)

dat <- melt(dat, id = c("age", "month", "population", "category",
                        "year", "sex", "disease", "sim")) %>%
  filter(month %% 12 == 10, round(year, 0) == max(round(year, 0))) %>%
  dplyr::rename(scenario = L1) %>%
  mutate(scenario = fct_recode(as.factor(scenario), A = "1", B = "2")) %>%
  group_by(sim, scenario) %>%
  dplyr::summarize(n = sum(population))

theme_set(theme_bw(base_size = 18))

# define some color options
cols <- c('#ffff00','#0000ff')

# plot
ggplot(dat, aes(x = n, y = scenario, fill = scenario)) +
  geom_density_ridges(alpha= 0.6) + theme_bw(base_size = 18) + theme_ridges() +
  xlab("Total population") + ylab("") +
  scale_y_discrete() + scale_fill_manual(values = cols)

}


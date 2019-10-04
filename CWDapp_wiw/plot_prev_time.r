#' Plot the prevalence over time
#'
#' @param dat counts provided as output from the CWD model functions
#' @return a plot of the prevalence over time
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom tidyr spread
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
#' plot_prev_time(out$counts)

#' @export

plot_prev_time <- function(dat){
  if(missing(dat)==TRUE) warning("missing data to plot")
  
  # summarize by year and disease status, calculate the prevalence
  dat.sum <- dat %>%
    filter(month %% 12 == 10) %>%
    group_by(year, disease) %>%
    summarize(n = sum(population)) %>%
    spread(key = disease, value = n) %>%
    mutate(prev = yes/ (no + yes))

  ggplot(dat.sum, aes(x = year, y = prev)) +
    geom_line(size = 1.5) + ylim(0,1) +
    ylab("Prevalence") + xlab("Year") +
    theme_light()  + theme(text = element_text(size = 18),
                           panel.grid.minor = element_blank(),
                           panel.grid.major = element_blank())

}

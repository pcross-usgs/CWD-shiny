
#' Plot the prevalence by age at the end of the simulation
#'
#' @param dat counts provided as output from the CWD model functions
#'
#' @return a plot of the prevalence by age
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#' @examples
#' params <- list(fawn.an.sur = 0.7, juv.an.sur = 0.9, ad.an.f.sur = 0.95, 
#' ad.an.m.sur = 0.8, fawn.repro = 0, juv.repro = 0.4, ad.repro = .9, 
#' hunt.mort.fawn = 0.01, hunt.mort.juv.f = 0.1, hunt.mort.juv.m = 0.2,
#' hunt.mort.ad.f = 0.15, hunt.mort.ad.m = 0.35, ini.fawn.prev = 0.01,
#' ini.juv.prev = 0.03, ini.ad.f.prev = 0.04,  ini.ad.m.prev = 0.04,
#' n.age.cats = 12,  p = 0.27, env.foi = 0,  beta.f = 0.08,  beta.m = 0.08,
#' theta = 1, n0 = 2000, n.years = 10, rel.risk = 1.5)
#' 
#' out <- cwd_det_model(params)
#' plot_prev_age_end(out$counts)
#' 
#' @export

plot_prev_age_end <- function(dat){
  if(missing(dat)==TRUE) warning("missing data to plot")
  
  # summarize disease status on the last year, calculate the prevalence
  dat.sum <- dat %>%
    filter(month %% 12 == 10, round(year, 0) == max(round(year, 0))) %>%
    group_by(age, sex, disease)%>%
    dplyr::summarize(n = sum(population)) %>%
    spread(key = disease, value = n) %>%
    mutate(prev = yes/ (no + yes)) %>%
    select(age, sex, prev)

  #prevalence by age
  ggplot(dat.sum, aes(x = age, y = prev, color = sex)) +
    geom_line(size = 1.5) + ylim(0,1) +
    ylab("") + xlab("Age") +
    theme_light()  + theme(text = element_text(size = 18),
                           panel.grid.minor = element_blank(),
                           panel.grid.major = element_blank(),
                           legend.position = c(.25,.85))
}

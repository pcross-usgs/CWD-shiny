#' Plot the fawn to doe ratio over time
#'
#'
#' @param dat counts provided as output from the CWD model functions
#'
#' @return a plot the fawn:doe ratio
#'
#' @import ggplot2
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom tidyr unite spread
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
#' plot_fawn_doe(out$counts)
#'
#' @export

plot_fawn_doe <- function(dat){
  if(missing(dat)==TRUE) warning("missing data to plot")
  
  dat$age.cat <- "adult"
  dat$age.cat[dat$age == 1] <- "fawn"

  # summarize by year and disease status, calculate the prevalence
  dat.sum <- dat %>%
    filter(month %% 12 == 11) %>%
    group_by(year, sex, age.cat) %>%
    dplyr::summarize(n = sum(population)) %>%
    unite(sex.age, sex, age.cat) %>%
    spread(key = sex.age, value = n) %>%
    mutate(fawn.doe = (m_fawn + f_fawn) / f_adult)

  ggplot(dat.sum, aes(x = year, y = fawn.doe)) +
    geom_line(size = 1.5) + ylim(0, 1.2) +
    ylab("Fawn:Doe ratio") + xlab("Year") +
    theme_light()  + theme(text = element_text(size = 18),
                           panel.grid.minor = element_blank(),
                           panel.grid.major = element_blank())

}


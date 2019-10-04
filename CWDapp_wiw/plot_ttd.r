#' Plot Time to disease-induced death
#'
#' Creates a plot of the time (in years) to disease-induced death based on the
#' proportion or probability of movement through the 10 infectious
#' sub-categories. Uses 1000 draws from a Gamma distribution.
#'
#' @param p proportion or probability of progressing through the infections
#' categories. Must be between 0 and 1.
#' @return a density plot of time to death.
#'
#' @import ggplot2
#' @importFrom ggridges theme_ridges
#' @importFrom stats rgamma
#' @examples
#' plot_ttd(p = 0.043)
#' 
#' @export

plot_ttd <- function(p){
  if(missing(p)==TRUE) warning("missing p parameter")
  
  #time to death: ttd
  tmp <- data.frame(years.to.death = rgamma(1000, 10, p)/12)

  theme_set(theme_bw(base_size = 18))
  p <- ggplot(tmp, aes(x = years.to.death)) +
    geom_density(fill = "grey") +
    theme_ridges() +
    labs(x = "Years", y = "Density",
         title = "Time until disease induced mortality")
  p
}


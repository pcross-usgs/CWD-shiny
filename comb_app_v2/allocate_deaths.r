allocate_deaths <- function(deaths, pop) {
  # pop is a matrix of n.age.categories = rows and number of I categories = columns
  # deaths is a vector of how many die in each age category
  condition1 <- which(deaths > 0)
  cats <- seq(1, 10, 1)

  for (i in condition1) {
    # vector of column locations of length equal to all individuals possible
    c2 <- rep(cats, pop[i, ])
    if (length(c2) == 1) {
      pop[i, c2] <- pop[i, c2] - deaths[i]
    } else {
      # sample these
      c3 <- plyr::count(sample(c2, deaths[i], replace = F))
      # remove those that died
      pop[i, c3$x] <- pop[i, c3$x] - c3$freq
    }
  }
  return(pop)
}
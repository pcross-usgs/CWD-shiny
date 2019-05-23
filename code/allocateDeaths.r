allocate.deaths <- function(deaths, pop) {
  #browser()
  # Population is a matrix of n.age.categories = rows
  # and number of I categories = columns
  # deaths is a vector of how many die in each age category
  for(i in 1:length(deaths)){
    if(deaths[i] > 0){
      # locations where individuals exist
      cells <- which(pop[i,] > 0)
      # vector of cell locations of length equal to all individuals possible
      c2 <- rep(cells, pop[i, cells])

      if(length(c2) == 1){
        pop[i, c2] <- pop[i, c2] - deaths[i]
      }

      if(length(c2) > 1){
        # randomly sample that vector and sort it
        c3 <- sort(sample(c2, deaths[i], replace = F))
        # aggregate it by cell location
        c4 <- as.data.frame(table(c3)) %>%
          mutate(c3 = as.numeric(levels(c3)))
        # remove those that died
        pop[i, c4$c3] <- pop[i, c4$c3] - c4$Freq
      }
    }
  }
  return(pop)
}

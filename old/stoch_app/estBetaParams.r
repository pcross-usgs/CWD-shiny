estBetaParams <- function(mu, var) {
  #  if(mu > 0){
  #    if(mu < 1){
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  #    }
  #  }
  #  if(mu == 0){
  #    alpha <- 1
  #    beta <- 1000
  #  }
  #  if(mu == 1){
  #    alpha <- 1000
  #    beta <- 1
  #  }
  return(params = list(alpha = alpha, beta = beta))
}

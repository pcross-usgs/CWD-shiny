#' Beta distribution conversion function
#'
#' Converts mean and variance parameters to the shape and scale parameters of a
#' Beta distribution
#'
#' @param mu mean
#' @param var variance
#'
#' @return A list of alpha and beta values
#' @examples 
#' est_beta_params(mu = 0.9, var = 0.005)
#' 
#' @export

est_beta_params <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

#' CWD stochastic model wrapper
#'
#' Wrapper function to run the stochastic CWD model many times. 
#'
#' @param params A list with the parameters needed for the stochastic model:  
#' fawn.an.sur, juv.an.sur, ad.an.f.sur, ad.an.m.sur, fawn.repro, 
#' juv.repro, ad.repro, hunt.mort.fawn, hunt.mort.juv.f, hunt.mort.juv.m,
#' hunt.mort.ad.f, hunt.mort.ad.m, ini.fawn.prev,
#' ini.juv.prev, ini.ad.f.prev,  ini.ad.m.prev,
#' n.age.cats, p, env.foi, beta.f, beta.m, theta, n0, n.years, 
#' rel.risk, repro.var, fawn.sur.var, sur.var, and hunt.var
#' @param nsims The number of simulations to run. 
#'
#' @return A list with 2 dataframes: 1. counts of the # of individuals in the 
#' susceptible and infectious categories by over time. 2. deaths--how 
#' individuals died over time (hunting, natural or disease). 
#' 
#' @importFrom popbio stable.stage
#' @importFrom stats rbeta rbinom
#' @importFrom dplyr rename mutate
#' @importFrom reshape2 melt
#' @examples 
#' 
#' params <- list(fawn.an.sur = 0.6, juv.an.sur = 0.8, ad.an.f.sur = 0.95, 
#' ad.an.m.sur = 0.9, fawn.repro = 0, juv.repro = 0.6, ad.repro = 1, 
#' hunt.mort.fawn = 0.01, hunt.mort.juv.f = 0.1, hunt.mort.juv.m = 0.1,
#' hunt.mort.ad.f = 0.2, hunt.mort.ad.m = 0.2, ini.fawn.prev = 0.02,
#' ini.juv.prev = 0.03, ini.ad.f.prev = 0.04,  ini.ad.m.prev = 0.04,
#' n.age.cats = 12,  p = 0.43, env.foi = 0,  beta.f = 0.15,  beta.m = 0.15,
#' theta = 1, n0 = 2000, n.years = 10, rel.risk = 1.0, 
#' repro.var = 0.005, fawn.sur.var = 0.005, sur.var = 0.005, hunt.var = 0.005)
#' 
#' out <- cwd_stoch_wrapper(params, nsims = 10)
#' 
#' plot_stoch_tots(out$counts, all.lines = TRUE, error.bars = c(0.05, 0.95), 
#' by.sexage = TRUE)
#' 
#' 
#' @export

cwd_stoch_wrapper <- function(params, nsims) {

  if(missing(nsims) == T) warning('nsims not provided')
  if(missing(params) == T) warning('params not provided')
  
  #pre-allocate the output vectors
  counts.sims <- vector("list", nsims)
  deaths.sims <- vector("list", nsims)

  for(i in 1:nsims){
    outa <- cwd_stoch_model(params)
    counts.sims[[i]] <- outa$counts
    deaths.sims[[i]] <- outa$deaths
  }
  
  # organize the output into a long data.frame
  counts <- melt(counts.sims, id = c("age", "month", "population", "category",
                                     "year", "sex", "disease")) %>% 
    rename(sim = L1)
  
  deaths <- melt(deaths.sims, id = c("age", "month", "population", "category",
                                     "year", "sex")) %>% rename(sim = L1)
  
  out <- list(counts = counts, deaths = deaths)
}


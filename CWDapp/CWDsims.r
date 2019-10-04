#' CWDsims: An R package for chronic wasting disease simulations
#' 
#' Collections of functions to run an interactive Shiny application
#' of CWD disease models. Currently there are deterministic and stochastic
#' models that are intended to project out scenarios for a 5 to 10 year window.
#' The models are sex and age structured with direct and indirect transmission. 
#' 
#' @section Model functions:
#' cwd_det_model runs the deterministic CWD model 
#'  
#' cwd_stoch_model runs the stochastic CWD model  
#' 
#' cwd_det_model_wiw similar to cwd_det_model, but allows for different male-male
#' male-female, and female-male transmission rates.  
#' 
#' cwd_stoch_model_wiw similar to cwd_stoch_model, but allows for different male-male
#' male-female, and female-male transmission rates.  
#' 
#' cwd_stoch_wrapper runs the stochastic CWD model multiple times for the same
#'   parameter set
#' 
#' launchCWDapp launches the Shiny applications.     
#' 
#' @section Helper functions: 
#' est_beta_params converts from a mean and variance to the shape and scale 
#' parameters of the Beta distribution  
#' 
#' allocate_deaths randomly allocates deaths among the 10 infectious 
#' subcategories
#' 
#' @section Plotting functions: 
#' plot_age_dist plots the age distribution at the last timepoint
#'   
#' plot_buck_doe plots the adult male to female ratio over time
#' 
#' plot_fawn_doe plots the fawn to adult female ratio over time
#' 
#' plot_deaths plots how types of deaths change over time.
#' 
#' plot_prev_age_end plots the prevalence versus age curve at the last 
#'   timepoint. 
#'   
#' plot_prev_time plots the prevalence over time. 
#' 
#' plot_tots plots the total population size over time
#' 
#' plot_ttd plots the distribution of years until disease-induced death given a 
#'   rate of movement among the infectious subcategories
#' 
#' plot_vitals plots the distribution of survival and reproductive rates
#' 
#' @section Stochastic plotting functions: 
#' plot_stoch_age_dist plots the stochastic age distribution plot for the last
#'  time point
#' 
#' plot_stoch_buck_doe plots the stochastic adult male to adult female ratio
#' 
#' plot_stoch_deaths plots how types of deaths change in the stochastic model 
#'  over time. 
#' 
#' plot_stoch_disease plots the number of positive and negative individuals over 
#'  time 
#' 
#' plot_stoch_fawn_doe fawns to adult females ratio over time
#' 
#' plot_stoch_perc_deaths plots how the % of each type of deaths change in the 
#'   stochastic model over time. 
#' 
#' plot_stoch_prev plot the prevalence over time for the stochastic model
#' 
#' plot_stoch_prev_age plot stochastic prevalence by age over time
#' 
#' plot_stoch_prev_age_end Prevalence versus age plot at the last time point
#' 
#' plot_stoch_tots plot the total number of individuals over time
#' 
#' @section Comparison plotting functions:    
#' plot_compare_all_det barplot to compare the output of two different 
#'   deterministic scenarios
#' 
#' plot_compare_all_stoch density plot to compare the output of two different scenarios for
#'   the stochastic model
#' 
#' plot_compare_hunted density plot to look at how the number of hunted 
#'   individuals compares for the stochasitic model. 
#' 
#' plot_compare_prev density plot comparing prevalence for the stochasitic 
#'   model. 
#' 
#' plot_compare_tots density plot comparing the total population size for the 
#'  stochasitic model. 
#' 
#' @docType package
#' @name CWDsims
NULL
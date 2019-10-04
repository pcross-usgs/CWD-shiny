#' Shiny app ui function
#'
#' determines plots for the stochastic comparison
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @import shiny


compare_stoch_plotsUI <- function(id){
  ns <- NS(id)
  wellPanel(
    tagList(
      h4("Plots"),
      plotOutput(ns('PrevPlot')), hr(),
      plotOutput(ns('TotalPlot')), hr(),
      plotOutput(ns('DeathPlot2')), hr(),
      plotOutput(ns('DeathPlot'))
      )
  )
}

#' Shiny app server function
#'
#' determines plots for the 2nd portion of the stochastic comparison
#' @param input provided by shiny
#' @param output provided by shiny
#' @import shiny

compare_stoch_plotsUI2 <- function(id){
  ns <- NS(id)
  tagList(
    h3("Comparison Plots"),
    h6("Distributions of simulated outcomes for scenarios A and B."),
    h6("Both simulations need to run to display a comparison and the number of simulations needs to be the same."),
    plotOutput(ns('ComparePlot')), hr(),
    h4(htmlOutput(ns('CompareText'))), hr(),
    includeMarkdown("disclaimer.md")
    )
}

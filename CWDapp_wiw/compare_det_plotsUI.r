#' Shiny app UI function
#'
#' User interface function for the comparison pane of the
#' deterministic model.
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @import shiny

compare_det_plotsUI <- function(id){
  ns <- NS(id)
  wellPanel(
    tagList(
      h4("Plots"),
      plotOutput(ns('PrevPlot_det')), hr(),
      plotOutput(ns('TotalPlot_det')), hr(),
      plotOutput(ns('DeathPlot_det'))
      )
  )

}

#' Shiny app UI function
#'
#' User interface function for the bottom portion of the comparison pane of the
#' deterministic model.
#'
#' @param input provided by shiny
#' @param output provided by shiny
#' @import shiny

compare_det_plotsUI2 <- function(id){
  ns <- NS(id)
  tagList(
    h3("Comparison Plots"),
    plotOutput(ns('ComparePlotDet')), hr(),
    includeMarkdown("disclaimer.md")
    )
}
